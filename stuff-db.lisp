;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010, 2011 Bert Burgemeister
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


(in-package :phoros)

(defun collect-pictures-file-data (path)
  "Return vector of image-data structures containing data from the
picture-headers of the .pictures file in path."
  (let ((estimated-header-length
         (ignore-errors
           (- (find-keyword path "PICTUREHEADER_END")
              (find-keyword path "PICTUREHEADER_BEGIN")
              *picture-header-length-tolerance*)))) ; allow for variation in dataSize and a few other parameters
    (if estimated-header-length ;otherwise we don't have a decent header
        (with-open-file (stream path :element-type 'unsigned-byte)
          (cl-log:log-message :db-dat "Digesting ~A." path)
          (loop
             with pictures-data = (make-array '(600) :fill-pointer 0)
             for picture-start =
             (find-keyword-in-stream stream "PICTUREHEADER_BEGIN" 0) then
             (find-keyword-in-stream stream "PICTUREHEADER_BEGIN"
                                     (+ picture-start picture-length
                                        estimated-header-length))
             for picture-length = (find-keyword-value
                                   path "dataSize=" picture-start
                                   estimated-header-length)
             and time-trigger = (utc-from-unix
                                 (or
                                  (find-keyword-value
                                   path "timeTrigger=" picture-start
                                   estimated-header-length)
                                  -1))
             and timestamp = (find-keyword-value
                              path "cameraTimestamp=" picture-start
                              estimated-header-length)
             and recorded-device-id = (format
                                       nil "~S"
                                       (find-keyword-value
                                        path "cam=" picture-start
                                        estimated-header-length))
             and gain = (find-keyword-value
                         path "gain=" picture-start estimated-header-length)
             and shutter = (find-keyword-value
                            path "shutter=" picture-start
                            estimated-header-length)
             while picture-start
             do (vector-push-extend
                 (make-instance
                  'image-data
                  :trigger-time time-trigger
                  :camera-timestamp timestamp
                  :recorded-device-id recorded-device-id
                  :filename (file-namestring path)
                  :gain gain
                  :shutter shutter
                  :byte-position picture-start)
                 pictures-data)
             finally
             (repair-missing-trigger-times pictures-data)
             (return  pictures-data)))
        (cl-log:log-message
         :db-dat "Skipping ~A because it looks disgusting." path))))

(defun repair-missing-trigger-times (images)
  "Use slot camera-timestamp to fake missing trigger-times."
  (labels ((slope (offending-index good-index)
             (/ (- (trigger-time (aref images (+ offending-index
                                                 (- good-index 2))))
                   (trigger-time (aref images (+ offending-index
                                                 (+ good-index 2)))))
                (- (camera-timestamp (aref images (+ offending-index
                                                     (- good-index 2))))
                   (camera-timestamp (aref images (+ offending-index
                                                     (+ good-index 2)))))))
           (intercept (offending-index good-index slope)
             (- (trigger-time (aref images (+ offending-index good-index)))
                (* slope (camera-timestamp (aref images (+ offending-index
                                                           good-index))))))
           (fake-trigger-time (offending-index good-index)
             (let* ((m (slope offending-index good-index))
                    (t-0 (intercept offending-index good-index m)))
               (+ (* m (camera-timestamp (aref images offending-index)))
                  t-0))))
    (dolist (offending-index
              (loop
                 with previous-trigger-time = 0
                 for h across images
                 for i from 0
                 for trigger-time = (trigger-time h)
                 if (> (+ trigger-time 1d-4) previous-trigger-time)
                 do (setf previous-trigger-time trigger-time)
                 else collect i))
      (ignore-errors
        (let ((good-index-offset -3))
          (handler-bind ((error #'(lambda (x) (declare (ignore x))
                                          (invoke-restart 'next-try))))
            (setf (trigger-time (aref images offending-index))
                  (restart-case
                      (fake-trigger-time offending-index good-index-offset)
                    (next-try ()
                      (incf good-index-offset 6)
                      (fake-trigger-time offending-index good-index-offset)))
                  (fake-trigger-time-p (aref images offending-index)) t)))))))

(defun collect-pictures-directory-data (dir-path)
  "Return vector of instances of class image-data with data from the
.pictures files in dir-path."
  (let ((pictures-files
         (directory (make-pathname
                     :directory (append (pathname-directory dir-path)
                                        '(:wild-inferiors))
                     :name :wild :type "pictures"))))
    (assert pictures-files ()
            "Sorry, but I couldn't find a single .pictures file below ~A."
            dir-path)
    (reduce #'(lambda (x1 x2) (merge 'vector x1 x2 #'< :key #'trigger-time))
            (mapcar #'collect-pictures-file-data
                    pictures-files))))

(defun collect-gps-data (dir-path estimated-utc)
  "Put content of files in dir-path/**/applanix/*eventN.txt into
vectors.  Return a list of elements (N vector) where N is the event
number."
  (let* ((event-dir
          (make-pathname
           :directory (append (pathname-directory dir-path)
                              '(:wild-inferiors) '("applanix" "points"))
           :name :wild
           :type :wild))
         (gps-files (directory event-dir))
         (gps-event-files
          (loop
             for gps-file in gps-files
             for gps-basename = (pathname-name gps-file)
             for event-number = (ignore-errors
                                  (subseq gps-basename
                                          (mismatch
                                           gps-basename "event"
                                           :start1
                                           (search "event" gps-basename
                                                   :from-end t))))
             when event-number collect (list event-number gps-file))))
    (assert gps-event-files ()
            "Sorry, but I couldn't find a single GPS event file in ~A."
            (directory-namestring event-dir))
    (cl-log:log-message
     :db-dat "Digesting GPS data from ~{~A~#^, ~}."
     (mapcar #'cadr gps-event-files))
    (loop
       for gps-event-file-entry in gps-event-files
       for gps-event-number = (first gps-event-file-entry)
       for gps-event-file = (second gps-event-file-entry)
       collect
       (cons
        gps-event-number
        (with-open-file (stream gps-event-file) 
          (loop
             for line = (read-line stream)
             for i from 0 to 35
             until (string-equal
                    (string-trim " " line)
                    "(time in Sec, distance in Meters, position in Meters, lat, long in Degrees, orientation angles and SD in Degrees, velocity in Meter/Sec, position SD in Meters)")
             finally 
               (read-line stream)
             (assert (< i 35) ()
                     "Unfamiliar header.  Check Applanix file format." nil))
          (loop
             with gps-points = (make-array '(1000) :fill-pointer 0)
             for line = (read-line stream nil)
             while line
             do (vector-push-extend
                 (let ((point (make-instance 'point-data)))
                   (with-slots
                         (gps-time
                          event-number
                          longitude latitude ellipsoid-height
                          roll pitch heading
                          east-velocity north-velocity up-velocity
                          east-sd north-sd height-sd
                          roll-sd pitch-sd heading-sd
                          easting northing cartesian-height)
                       point
                     (with-input-from-string (line-content line)
                       (setf event-number gps-event-number
                             gps-time
                             (utc-from-gps estimated-utc ; From GPS week time.
                                           (read line-content nil)))
                       (read line-content nil) ; Discard distance.
                       (setf easting (read line-content nil)
                             northing (read line-content nil)
                             cartesian-height (read line-content nil)
                             latitude (read line-content nil)
                             longitude (read line-content nil)
                             ellipsoid-height (read line-content nil)
                             roll (read line-content nil)
                             pitch (read line-content nil)
                             heading (read line-content nil)
                             east-velocity (read line-content nil)
                             north-velocity (read line-content nil)
                             up-velocity (read line-content nil)
                             east-sd (read line-content nil)
                             north-sd (read line-content nil)
                             height-sd (read line-content nil)
                             roll-sd (read line-content nil)
                             pitch-sd (read line-content nil)
                             heading-sd (read line-content nil))))
                   point)
                 gps-points)
             finally (return gps-points)))))))

(defun aggregate-gps-events (gps-points)
  "Turn an alist of ((event1 . points1) (event2 . points2)...) into
((t . all-points))."
  (cl-log:log-message
 :db-sys
 "I was asked to aggregate-events so I won't distinguish any event numbers.")
  (list
   (cons t (reduce #'(lambda (x y) (merge 'vector x y #'< :key #'gps-time))
                   (mapcar #'cdr gps-points)))))

(defparameter *leap-seconds* nil
  "An alist of (time . leap-seconds) elements.  leap-seconds are to be
  added to GPS time to get UTC.")

(defparameter *time-steps-history-url*
  "http://hpiers.obspm.fr/eoppc/bul/bulc/TimeSteps.history"
  "URL of the leap second table which should contain lines like this:
 1980  Jan.   1              - 1s
 1981  Jul.   1              - 1s
 ...")

(defparameter *time-steps-history-file*
  (make-pathname :directory '(:relative) :name "TimeSteps" :type "history")
  "Fallback in case *time-steps-history-url* is unavailable.")

(let ((leap-second-months
       (pairlis '("Jan." "Feb." "March" "Apr." "May" "Jun."
                  "Jul." "Aug." "Sept." "Oct." "Nov." "Dec.")
                '(1 2 3 4 5 6 7 8 9 10 11 12))))
  ;; Month names as used in
  ;; http://hpiers.obspm.fr/eoppc/bul/bulc/TimeSteps.history."
  (defun initialize-leap-seconds () ; TODO: Security hole: read-from-string
    (handler-case
        (multiple-value-bind
              (body status-code headers uri stream must-close reason-phrase)
            (drakma:http-request *time-steps-history-url*)
          (declare (ignore headers stream must-close reason-phrase))
          (assert (= status-code 200))
          (with-open-file (stream *time-steps-history-file*
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string body stream)
            (cl-log:log-message
             :debug "Downloaded leap second information from ~A." uri)))
      (error (e)
        (cl-log:log-message
         :warning
         "Couldn't get the latest leap seconds information from ~A. (~A)  Falling back to cached data in ~A."
         *time-steps-history-url* e *time-steps-history-file*)))
    (with-open-file (stream *time-steps-history-file*
                            :direction :input :if-does-not-exist :error)
      (loop for time-record = (string-trim " 	" (read-line stream))
         until (equal 1980 (read-from-string time-record nil)))
      (setf
       *leap-seconds*
       (loop for time-record = (string-trim " s	" (read-line stream))
          with leap = nil and leap-date = nil
          until (string-equal (subseq time-record 1 20)
                              (make-string 19 :initial-element #\-))
          do (with-input-from-string (line time-record)
               (let ((year (read line))
                     (month (cdr (assoc
                                  (read line)
                                  leap-second-months
                                  :test #'string-equal)))
                     (date (read line))
                     (sign (read line))
                     (seconds (read line)))
                 (setf leap-date (encode-universal-time 0 0 0 date month year 0)
                       leap (if (eq sign '-) (- seconds) seconds))))
          sum leap into leap-sum
          collect leap-date into leap-dates
          collect leap-sum into leap-sums
          finally (return (sort (pairlis leap-dates leap-sums) #'<
                                :key #'car)))))))

(defparameter *gps-epoch* (encode-universal-time 0 0 0 6 1 1980 0))
(defparameter *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))

(defun gps-start-of-week (time)
  "Begin of a GPS week (approximately Sunday 00:00)"
  (let ((week-length (* 7 24 3600))
        (leap-seconds (cdr (find time *leap-seconds*
                                 :key #'car :test #'> :from-end t))))
    (assert leap-seconds ()
            "Couldn't determine leap seconds for ~A" (timestring (round time)))
    (+ (* (floor (- time *gps-epoch*) week-length)
          week-length)
       *gps-epoch*
       leap-seconds)))

(defun utc-from-gps (utc-approximately gps-week-time)
  "Convert GPS week time into UTC.  gps-week-time may be of type
float; in this case a non-integer is returned which can't be fed into
decode-universal-time."
  (+ (gps-start-of-week utc-approximately) gps-week-time))

(defun utc-from-unix (unix-time)
  "Convert UNIX UTC to Lisp time."
  (when unix-time (+ unix-time *unix-epoch*)))

;;(defun event-number (recorded-device-id)
;;  "Return the GPS event number corresponding to recorded-device-id of camera (etc.)"
;;  (let ((event-table (pairlis '(21 22 11 12 1 2)
;;                              '("1" "1" "2" "2" "1" "1"))))
;;    (cdr (assoc recorded-device-id event-table)))) ; TODO: make a saner version


(let (event-number-storage)
  (defun device-event-number (recorded-device-id utc)
    "Return the GPS event number (a string) corresponding to
recorded-device-id (a string) of camera (etc.)"
    (let ((device-event-number
           (cdr (assoc recorded-device-id event-number-storage
                       :test #'string-equal))))
      (if device-event-number
          device-event-number
          (let* ((date (simple-date:universal-time-to-timestamp (round utc)))
                 (device-stage-of-life
                  (car
                   (select-dao
                    'sys-device-stage-of-life
                    (:and (:overlaps
                           (:set 'mounting-date
                                 (:least :current-date 'unmounting-date))
                           (:set (:date date) (:date date)))
                          (:= 'recorded-device-id recorded-device-id))))))
            (assert device-stage-of-life
                    ()
                    "Can't figure out what event-number belongs to recorded-device-id ~S of (approx.) ~A.  There should be some entry in table sys-device-stage-of-life to this end."
                    recorded-device-id (timestring (round utc)))
            (push (cons recorded-device-id (event-number device-stage-of-life))
                  event-number-storage)
            (event-number device-stage-of-life))))))

(defun almost= (x y epsilon)
  (< (abs (- x y)) epsilon))

(defun geographic-to-utm (utm-zone longitude latitude &optional (height 0d0))
  "Return UTM utm-zone representation of geographic coordinates."
  (let ((utm-coordinate-system
         (format nil "+proj=utm +ellps=WGS84 +zone=~D" utm-zone)))
    (proj:cs2cs (list (proj:degrees-to-radians longitude) (proj:degrees-to-radians latitude) height)
                :destination-cs utm-coordinate-system)))

(defun utm-zone (longitude)
  "Return UTM zone number belonging to longitude."
  (1+ (floor (+ longitude 180) 6)))

(defun assert-utm-zone (longitude-median longitude-leeway longitude latitude
                        geographic-height easting northing cartesian-height)
  "Check if, given longitude and latitude, easting and northing are
calculated in the UTM zone belonging to longitude-median."
  (let ((epsilon 1d-1))
    (unless
        (or (every #'(lambda (x y) (almost= x y epsilon))
                   (geographic-to-utm (utm-zone (- longitude-median
                                                   longitude-leeway))
                                      longitude latitude geographic-height)
                   (list easting northing cartesian-height))
            (every #'(lambda (x y) (almost= x y epsilon))
                   (geographic-to-utm (utm-zone (+ longitude-median
                                                   longitude-leeway))
                                      longitude latitude geographic-height)
                   (list easting northing cartesian-height)))
      (error "The longitude median ~A should be in or near UTM zone ~D.  ~
              This is inconsistent with the easting values I was given.  ~
              Offending coordinates: (~A ~A ~A) (~A ~A ~A)."
             longitude-median (utm-zone longitude-median) longitude latitude
             geographic-height easting northing cartesian-height))))

(defun assert-gps-points-sanity (gps-points)
  "Check if gps-points (as returned by collect-gps-data) are ok.
Return the Proj.4 string describing the cartesian coordinate system
used."
  (loop
     for gps-event in gps-points
     for gps-event-vector = (cdr gps-event)
     for first-longitude = (longitude (aref gps-event-vector 0))
     for first-latitude = (latitude (aref gps-event-vector 0))
     for first-geographic-height = (ellipsoid-height (aref gps-event-vector 0))
     for first-easting = (easting (aref gps-event-vector 0))
     for first-northing = (northing (aref gps-event-vector 0))
     for first-cartesian-height = (cartesian-height (aref gps-event-vector 0))
     for longitude-median =
       (loop
          for point across gps-event-vector
          for i from 1
          sum (longitude point) into longitude-sum
          finally (return (/ longitude-sum i)))
     do (assert-utm-zone longitude-median 1
                         first-longitude first-latitude
                         first-geographic-height
                         first-easting first-northing
                         first-cartesian-height)
     finally (return (format nil "+proj=utm +ellps=WGS84 +zone=~D"
                             (utm-zone longitude-median)))))

(defun get-measurement-id (common-table-name dir-path cartesian-system)
  "Get measurement-id associated with dir-path and
acquisition-project-id.  Create a fresh matching record if necessary."
  (let ((acquisition-project
         (car (select-dao 'sys-acquisition-project
                          (:= 'common-table-name common-table-name)))))
    (assert acquisition-project)
    (let* ((acquisition-project-id (acquisition-project-id acquisition-project))
           (measurement
            (or (car (select-dao
                      'sys-measurement
                      (:and (:= 'acquisition-project-id acquisition-project-id)
                            (:= 'directory dir-path))))
                (insert-dao
                 (make-instance 'sys-measurement
                                :acquisition-project-id acquisition-project-id
                                :directory dir-path
                                :cartesian-system cartesian-system
                                :fetch-defaults t)))))
      (measurement-id measurement))))

(defun store-images-and-points (common-table-name dir-path
                                &key (epsilon 1d-3)
                                (root-dir (user-homedir-pathname))
                                aggregate-events)
  "Link images to GPS points; store both into their respective DB
tables.  Images become linked to GPS points when their respective
times differ by less than epsilon seconds, and when the respective
events match.  dir-path is a (probably absolute) path to a directory
that contains one set of measuring data.  root-dir must be equal for
all pojects."
  ;; TODO: epsilon could be a range.  We would do a raw mapping by (a bigger) time epsilon and then take speed into account.
  (assert-phoros-db-major-version)
  (assert     ;not strictly necessary, but may save the user some time
   (select-dao 'sys-acquisition-project
               (:= 'common-table-name common-table-name))
   () "There is no acquisition project named ~A." common-table-name)
  (create-data-table-definitions common-table-name)
  (initialize-leap-seconds)
  (let* ((images
          (collect-pictures-directory-data dir-path))
         (estimated-time
          (loop
             for i across images
             unless (or (fake-trigger-time-p i)
                        (< (trigger-time i) *gps-epoch*))
             do (return (trigger-time i))))
         (gps-points
          (if aggregate-events
              (aggregate-gps-events (collect-gps-data dir-path estimated-time))
              (collect-gps-data dir-path estimated-time)))
         (gps-start-pointers (loop
                                for i in gps-points
                                collect (cons (car i) 0)))
         (mapped-image-counter (length images))
         (cartesian-system (assert-gps-points-sanity gps-points))
         (dir-below-root-dir
          (enough-namestring (string-right-trim "/\\ " dir-path) root-dir)))
    (cl-log:log-message
     :db-dat "I assume this measure was taken approximately ~A."
     (timestring (round estimated-time)))
    (loop
       for i across images
       for image-event-number = (or aggregate-events
                                    (device-event-number (recorded-device-id i)
                                                         estimated-time))
       for image-time = (trigger-time i)
       for matching-point =
         (when image-time               ; otherwise this image is junk
           (let ((gps-start-pointer
                  (cdr (assoc image-event-number gps-start-pointers
                              :test #'equal))))
             (assert gps-start-pointer ()
                     "Can't find an event number of ~S ~
                      (as suggested by the sys tables relevant to the ~
                      current image) among ~{~S~#^, ~} ~
                      (as derived from the names of the GPS event files).  ~
                      Consider using --aggregate-events if you can't ~
                      recitfy your data."
                     image-event-number (mapcar #'car gps-start-pointers))
             (loop
                for gps-pointer from gps-start-pointer
                for gps-point across (subseq (cdr (assoc image-event-number
                                                         gps-points
                                                         :test #'equal))
                                             gps-start-pointer)
                when (almost= (gps-time gps-point) image-time epsilon)
                do (setf (cdr (assoc image-event-number
                                     gps-start-pointers :test #'equal))
                         gps-pointer) ; remember index of last matching point
                and return gps-point)))
       if matching-point
       do (let ((point-id               ; TODO: consider using transaction
                 (or (point-id matching-point) ; We've hit a point twice.
                     (sequence-next (point-id-sequence-name matching-point))))
                (measurement-id (get-measurement-id common-table-name
                                                    dir-below-root-dir
                                                    cartesian-system)))
            (setf (point-id i) point-id
                  (point-id matching-point) point-id
                  (measurement-id matching-point) measurement-id
                  (measurement-id i) measurement-id
                  (trigger-time matching-point) image-time)
            (save-dao matching-point)
            (execute (:update (dao-table-name (class-of matching-point))
                              :set 'coordinates
                              (:st_geomfromewkt
                               (format nil "SRID=4326; POINT(~S ~S ~S)"
                                       (longitude matching-point)
                                       (latitude matching-point)
                                       (ellipsoid-height matching-point)))
                              :where (:= 'point-id (point-id matching-point))))
            (save-dao i))
       else do
         (decf mapped-image-counter)
         (cl-log:log-message
          :orphan
          "Couldn't map to any point: ~A~A, byte ~S. ~
           ~:[~; It didn't have a decent trigger time anyway.~]"
          dir-path (filename i) (image-byte-position i)
          (fake-trigger-time-p i)))
    (cl-log:log-message
     :db-dat
     "Tried to map ~D images to GPS points.  ~
      The attempt has been successful in ~:[~D~;all~] cases.~
      ~1@*~:[  See file orphans.log for details on the failures.~;~]"
     (length images) (= (length images) mapped-image-counter)
     mapped-image-counter)))

(defun assert-user-points-version (user-points-version)
  "Check if user-points-version is compatible with the current
user-point table definition."
  (cond                ;insert more interesting clauses when necessary
    ((null user-points-version)
     (warn "Storing user-points which don't have a version number."))
    (t)))

(defun store-user-points (common-table-name json-file-path)
  "Store in DB user points given in file at json-file-path, which
supposedly was created by Phoros.  Return number of points stored,
number of points that were already in DB, and number of points found
in JSON file."
  (assert-phoros-db-major-version)
  (let* ((user-point-table-name (user-point-table-name common-table-name))
         (raw-input (with-open-file (stream json-file-path)
                      (json:decode-json stream)))
         (raw-input-version (cdr (assoc :phoros-version raw-input)))
         (raw-features (cdr (assoc :features raw-input))))
    (assert-user-points-version raw-input-version)
    (loop
       for i in raw-features
       for coordinates = (cdr (assoc :coordinates (cdr (assoc :geometry i))))
       for point-form = (format nil "SRID=4326; POINT(~{~S ~})" coordinates)
       for properties = (cdr (assoc :properties i))
       for user-name = (cdr (assoc :user-name properties))
       for attribute = (cdr (assoc :attribute properties))
       for description = (cdr (assoc :description properties))
       for numeric-description = (cdr (assoc :numeric-description properties))
       for creation-date = (cdr (assoc :creation-date properties))
       for stdx-global = (cdr (assoc :stdx-global properties))
       for stdy-global = (cdr (assoc :stdy-global properties))
       for stdz-global = (cdr (assoc :stdz-global properties))
       for input-size = (cdr (assoc :input-size properties))
       for aux-numeric = (cdr (assoc :aux-numeric properties))
       for aux-text = (cdr (assoc :aux-text properties))
       for aux-numeric-comparison =
         (if aux-numeric
             (format nil "(~A = (CAST (ARRAY[~{~S~#^,~}] AS NUMERIC[])))"
                     (s-sql:to-sql-name 'aux-numeric) aux-numeric)
             (sql (:is-null 'aux-numeric)))
       for aux-text-comparison =
         (if aux-text
             (sql (:= 'aux-text (apply #'vector aux-text)))
             (sql (:is-null 'aux-text)))
       with points-stored = 0
       with points-already-in-db = 0
       sum 1 into points-tried
       do
         (if
          (query
           (:select
            t
            :from user-point-table-name :natural :left-join 'sys-user
            :where (:and (:st_equals 'coordinates
                                     (:st_geomfromewkt point-form))
                         (:= 'user-name user-name)
                         (:= 'attribute attribute)
                         (:= 'description description)
                         (:= 'numeric-description numeric-description)
                         (:= (:to-char 'creation-date
                                       *user-point-creation-date-format*)
                             creation-date)
                         (:= 'stdx-global stdx-global)
                         (:= 'stdy-global stdy-global)
                         (:= 'stdz-global stdz-global)
                         (:= 'input-size input-size)
                         (:raw aux-numeric-comparison)
                         (:raw aux-text-comparison))))
          (incf points-already-in-db)
          (progn
            (assert
             (= 1
                (execute
                 (:insert-into user-point-table-name :set
                               'coordinates (:st_geomfromewkt point-form)
                               'user-id (:select 'user-id
                                                 :from 'sys-user
                                                 :where (:= 'user-name
                                                            user-name))
                               'attribute attribute
                               'description description
                               'numeric-description numeric-description
                               'creation-date creation-date
                               'stdx-global stdx-global
                               'stdy-global stdy-global
                               'stdz-global stdz-global
                               'input-size input-size
                               'aux-numeric (if aux-numeric
                                                (apply #'vector aux-numeric)
                                                :null)
                               'aux-text (if aux-text
                                             (apply #'vector aux-text)
                                             :null))))
             () "Point not stored.  This should not happen.")
            (incf points-stored)))
       finally (return (values points-stored
                               points-already-in-db
                               points-tried)))))

(defun* store-camera-hardware (&key
                               (try-overwrite t)
                               &mandatory-key
                               sensor-width-pix
                               sensor-height-pix
                               pix-size
                               channels
                               pix-depth
                               color-raiser
                               bayer-pattern
                               serial-number
                               description)
  "Store a new record in table sys-camera-hardware, or try updating an
existing one.  Return camera-hardware-id of the altered record."
  (assert-phoros-db-major-version)
  (let ((record
         (or (when try-overwrite
               (car (select-dao 'sys-camera-hardware
                                (:and (:= 'sensor-width-pix sensor-width-pix)
                                      (:= 'sensor-height-pix sensor-height-pix)
                                      (:= 'pix-size pix-size)
                                      (:= 'channels channels)
                                      (:= 'serial-number serial-number)
                                      (:= 'pix-depth pix-depth)))))
             (make-instance 'sys-camera-hardware :fetch-defaults t))))
    (with-slots ((sensor-width-pix-slot sensor-width-pix)
                 (sensor-height-pix-slot sensor-height-pix)
                 (pix-size-slot pix-size)
                 (channels-slot channels)
                 (pix-depth-slot pix-depth)
                 (color-raiser-slot color-raiser)
                 (bayer-pattern-slot bayer-pattern)
                 (serial-number-slot serial-number)
                 (description-slot description))
        record
      (setf sensor-width-pix-slot sensor-width-pix
            sensor-height-pix-slot sensor-height-pix
            pix-size-slot pix-size
            channels-slot channels
            pix-depth-slot pix-depth
            color-raiser-slot color-raiser
            bayer-pattern-slot bayer-pattern
            serial-number-slot serial-number
            description-slot description))
    (let ((new-row-p (save-dao record)))
      (cl-log:log-message
       :db-sys
       "sys-camera-hardware: ~:[Updated~;Stored new~] camera-hardware-id ~A"
       new-row-p (camera-hardware-id record)))
    (camera-hardware-id record)))

(defun* store-lens (&key (try-overwrite t)
                         &mandatory-key
                         c
                         serial-number
                         description)
  "Store a new record in table sys-lens, or try updating an existing
one.  Return lens-id of the altered record."
  (assert-phoros-db-major-version)
  (let ((record
         (or (when try-overwrite
               (car (select-dao 'sys-lens
                                (:and (:= 'c c)
                                      (:= 'serial-number serial-number)))))
             (make-instance 'sys-lens :fetch-defaults t))))
    (with-slots ((c-slot c)
                 (serial-number-slot serial-number)
                 (description-slot description))
        record
      (setf c-slot c
            serial-number-slot serial-number
            description-slot description))
    (let ((new-row-p (save-dao record)))
      (cl-log:log-message
       :db-sys "sys-lens: ~:[Updated~;Stored new~] lens-id ~A"
       new-row-p (lens-id record)))
    (lens-id record)))

(defun store-generic-device
    (&key (camera-hardware-id :null) (lens-id :null) (scanner-id :null))
  "Store a new record in table sys-generic-device.  Return
generic-device-id of the new record."
  (assert-phoros-db-major-version)
  (assert (notevery
           #'(lambda (x) (eq :null x))
           (list camera-hardware-id lens-id scanner-id))
          () "Generic device: not enough components.")
  (let ((record (make-instance 'sys-generic-device
                               :camera-hardware-id camera-hardware-id
                               :lens-id lens-id
                               :scanner-id scanner-id
                               :fetch-defaults t)))
    (let ((new-row-p (save-dao record)))
      (cl-log:log-message
       :db-sys
       "sys-generic-device: ~:[Updated~;Stored new~] generic-device-id ~A"
       new-row-p (generic-device-id record)))
    (generic-device-id record)))

(defun* store-device-stage-of-life (&key (unmounting-date :null)
                                   (try-overwrite t)
                                   &mandatory-key
                                   recorded-device-id
                                   event-number
                                   generic-device-id
                                   vehicle-name
                                   casing-name
                                   computer-name
                                   computer-interface-name
                                   mounting-date)
  "Store a new record in table sys-device-stage-of-life, or try
updating an existing one.  Return device-stage-of-life-id of the
altered record."
  (assert-phoros-db-major-version)
  (let ((record
         (or (when try-overwrite
               (car (select-dao
                     'sys-device-stage-of-life
                     (:and (:= 'recorded-device-id recorded-device-id)
                           (:= 'event-number event-number)
                           (:= 'generic-device-id generic-device-id)
                           (:= 'vehicle-name vehicle-name)
                           (:= 'mounting-date mounting-date)))))
             (make-instance 'sys-device-stage-of-life :fetch-defaults t))))
    (with-slots ((recorded-device-id-slot recorded-device-id)
                 (event-number-slot event-number)
                 (generic-device-id-slot generic-device-id)
                 (vehicle-name-slot vehicle-name)
                 (casing-name-slot casing-name)
                 (computer-name-slot computer-name)
                 (computer-interface-name-slot computer-interface-name)
                 (mounting-date-slot mounting-date)
                 (unmounting-date-slot unmounting-date))
        record
      (setf recorded-device-id-slot recorded-device-id
            event-number-slot event-number
            generic-device-id-slot generic-device-id
            vehicle-name-slot vehicle-name
            casing-name-slot casing-name
            computer-name-slot computer-name
            computer-interface-name-slot computer-interface-name
            mounting-date-slot mounting-date
            unmounting-date-slot unmounting-date))
    (let ((new-row-p (save-dao record)))
      (cl-log:log-message
       :db-sys
       "sys-device-stage-of-life: ~:[Updated~;Stored new~] device-stage-of-life-id ~A"
       new-row-p (device-stage-of-life-id record)))
    (device-stage-of-life-id record)))

(defun* store-device-stage-of-life-end (&mandatory-key device-stage-of-life-id
                                                       unmounting-date)
  "Update record in table sys-device-stage-of-life with an unmounting
date.  Return device-stage-of-life-id of the altered record."
  (assert-phoros-db-major-version)
  (let ((record
         (get-dao 'sys-device-stage-of-life device-stage-of-life-id)))
    (with-slots ((unmounting-date-slot unmounting-date))
        record
      (setf unmounting-date-slot unmounting-date))
    (update-dao record)
    (device-stage-of-life-id record)))

(defun* store-camera-calibration (&mandatory-key
                                  device-stage-of-life-id
                                  date
                                  person
                                  main-description
                                  debug
                                  photogrammetry-version
                                  mounting-angle
                                  inner-orientation-description
                                  c
                                  xh
                                  yh
                                  a1
                                  a2
                                  a3
                                  b1
                                  b2
                                  c1
                                  c2
                                  r0
                                  outer-orientation-description
                                  dx
                                  dy
                                  dz
                                  omega
                                  phi
                                  kappa
                                  boresight-description
                                  b-dx
                                  b-dy
                                  b-dz
                                  b-ddx
                                  b-ddy
                                  b-ddz
                                  b-rotx
                                  b-roty
                                  b-rotz
                                  b-drotx
                                  b-droty
                                  b-drotz
                                  nx
                                  ny
                                  nz
                                  d)
  "Store a new record of camera-calibration in table
sys-device-stage-of-life, or update an existing one.  Return
device-stage-of-life-id and date of the altered record."
  (assert-phoros-db-major-version)
  (let ((record
         (or (car (select-dao
                   'sys-camera-calibration
                   (:and (:= 'device-stage-of-life-id device-stage-of-life-id)
                         (:= 'date date))))
             (make-instance 'sys-camera-calibration :fetch-defaults t))))
    (with-slots
          ((device-stage-of-life-id-slot device-stage-of-life-id)
           (date-slot date)
           (person-slot person)
           (main-description-slot main-description)
           (debug-slot debug)
           (photogrammetry-version-slot photogrammetry-version)
           (mounting-angle-slot mounting-angle)
           (inner-orientation-description-slot inner-orientation-description)
           (c-slot c)
           (xh-slot xh)
           (yh-slot yh)
           (a1-slot a1)
           (a2-slot a2)
           (a3-slot a3)
           (b1-slot b1)
           (b2-slot b2)
           (c1-slot c1)
           (c2-slot c2)
           (r0-slot r0)
           (outer-orientation-description-slot outer-orientation-description)
           (dx-slot dx)
           (dy-slot dy)
           (dz-slot dz)
           (omega-slot omega)
           (phi-slot phi)
           (kappa-slot kappa)
           (boresight-description-slot boresight-description)
           (b-dx-slot b-dx)
           (b-dy-slot b-dy)
           (b-dz-slot b-dz)
           (b-ddx-slot b-ddx)
           (b-ddy-slot b-ddy)
           (b-ddz-slot b-ddz)
           (b-rotx-slot b-rotx)
           (b-roty-slot b-roty)
           (b-rotz-slot b-rotz)
           (b-drotx-slot b-drotx)
           (b-droty-slot b-droty)
           (b-drotz-slot b-drotz)
           (nx-slot nx)
           (ny-slot ny)
           (nz-slot nz)
           (d-slot d))
        record
      (setf device-stage-of-life-id-slot device-stage-of-life-id
            date-slot date
            person-slot person
            main-description-slot main-description
            debug-slot debug
            photogrammetry-version-slot photogrammetry-version
            mounting-angle-slot mounting-angle
            inner-orientation-description-slot inner-orientation-description
            c-slot c
            xh-slot xh
            yh-slot yh
            a1-slot a1
            a2-slot a2
            a3-slot a3
            b1-slot b1
            b2-slot b2
            c1-slot c1
            c2-slot c2
            r0-slot r0
            outer-orientation-description-slot outer-orientation-description
            dx-slot dx
            dy-slot dy
            dz-slot dz
            omega-slot omega
            phi-slot phi
            kappa-slot kappa
            boresight-description-slot boresight-description
            b-dx-slot b-dx
            b-dy-slot b-dy
            b-dz-slot b-dz
            b-ddx-slot b-ddx
            b-ddy-slot b-ddy
            b-ddz-slot b-ddz
            b-rotx-slot b-rotx
            b-roty-slot b-roty
            b-rotz-slot b-rotz
            b-drotx-slot b-drotx
            b-droty-slot b-droty
            b-drotz-slot b-drotz
            nx-slot nx
            ny-slot ny
            nz-slot nz
            d-slot d))
    (let ((new-row-p (save-dao record)))
      (cl-log:log-message
       :db-sys
       "sys-camera-calibration: ~:[Updated~;Stored new~] record ~
        for ~A, device-stage-of-life-id ~A"
       new-row-p (date record) (device-stage-of-life-id record)))
    (values (device-stage-of-life-id record)
            (date record))))

#|
(with-connection '("phoros-dev" "postgres" "passwd" "host")
  (nuke-all-tables)
  (create-acquisition-project "yyyy")
  (store-camera-hardware :sensor-width-pix 7000 :sensor-height-pix 800 :pix-size .003 :channels 3 :pix-depth 17 :color-raiser #(1 2 3) :bayer-pattern #(4 5 6) :serial-number "18" :description "yyy" :try-overwrite t)
  (store-lens :c 10.5 :serial-number "17.8.8" :description "blahBlah3" :try-overwrite nil)
  (store-generic-device :camera-hardware-id 1 :lens-id 1)
  (store-device-stage-of-life :recorded-device-id "1" :event-number "777" :generic-device-id 1 :vehicle-name "Auto" :casing-name "Vorn links" :computer-name "ccdheck" :computer-interface-name "eth0" :mounting-date "2010-01-30T07:00-1")
  (store-images-and-points "yyyy" "/home/bertb/phoros-testdata/mitsa-small/"))
|#