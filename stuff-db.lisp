(in-package :phoros)

(defun collect-pictures-file-data (path)
  "Return vector of image-data structures containing data from the picture-headers of the .pictures file in path."
  (let ((estimated-header-length
         (- (find-keyword path "PICTUREHEADER_END")
            (find-keyword path "PICTUREHEADER_BEGIN") *picture-header-length-tolerance*))) ; allow for variation in dataSize and a few other parameters
    (with-open-file (stream (print path) :element-type 'unsigned-byte)
      (loop
         with pictures-data = (make-array '(600) :fill-pointer 0)
         for picture-start =
           (find-keyword-in-stream stream "PICTUREHEADER_BEGIN" 0) then
           (find-keyword-in-stream stream "PICTUREHEADER_BEGIN" (+ picture-start picture-length estimated-header-length))
         for picture-length = (find-keyword-value path "dataSize=" picture-start)
         and time-trigger = (utc-from-unix (find-keyword-value path "timeTrigger=" picture-start))
         and timestamp = (find-keyword-value path "cameraTimestamp=" picture-start)
         and recorded-device-id = (find-keyword-value path "cam=" picture-start)
         and gain = (find-keyword-value path "gain=" picture-start)
         and shutter = (find-keyword-value path "shutter=" picture-start)
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
         (return  pictures-data)))))

(defun repair-missing-trigger-times (images)
  "Use slot camera-timestamp to fake missing trigger-times."
  (labels ((slope (offending-index good-index)
             (/ (- (trigger-time (aref images (+ offending-index (- good-index 2))))
                   (trigger-time (aref images (+ offending-index (+ good-index 2)))))
                (- (camera-timestamp (aref images (+ offending-index (- good-index 2))))
                   (camera-timestamp (aref images (+ offending-index (+ good-index 2)))))))
           (intercept (offending-index good-index slope)
             (- (trigger-time (aref images (+ offending-index good-index)))
                (* slope (camera-timestamp (aref images (+ offending-index good-index))))))
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
      (let ((good-index-offset -3))
        (handler-bind ((error #'(lambda (x) (invoke-restart 'next-try))))
          (setf (trigger-time (aref images offending-index))
                (restart-case (fake-trigger-time offending-index good-index-offset)
                  (next-try ()
                    (incf good-index-offset 6)
                    (fake-trigger-time offending-index good-index-offset)))
                (fake-trigger-time-p (aref images offending-index)) t))))))

(defun collect-pictures-directory-data (dir-path)
  "Return vector of instances of class image-data with data from the .pictures files in dir-path."
  (reduce #'(lambda (x1 x2) (merge 'vector x1 x2 #'< :key #'trigger-time))
          (mapcar #'collect-pictures-file-data
                  (directory (make-pathname
                              :directory (append (pathname-directory dir-path) '(:wild-inferiors))
                              :name :wild :type "pictures")))))

(defun collect-gps-data (dir-path estimated-utc)
  "Put content of files in dir-path/**/applanix/*eventN.txt into vectors.  Return a list of elements (N vector) where N is the event number."
  (let* ((gps-files
          (directory
           (make-pathname
            :directory (append (pathname-directory dir-path) '(:wild-inferiors) '("applanix" "points"))
            :name :wild
            :type "txt")))
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
             until (string-equal (string-trim " " line) "(time in Sec, distance in Meters, position in Meters, lat, long in Degrees, orientation angles and SD in Degrees, velocity in Meter/Sec, position SD in Meters)")
             finally 
             (read-line stream)
             (assert (< i 35) () "Unfamiliar header.  Check Applanix file format." nil))
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

(defparameter *leap-seconds* nil
  "An alist of (time . leap-seconds) elements.  leap-seconds are to be added to GPS time to get UTC.")

(defparameter *time-steps-history-url* "http://hpiers.obspm.fr/eoppc/bul/bulc/TimeSteps.history"
  "URL of the leap second table which should contain lines like this:
 1980  Jan.   1              - 1s
 1981  Jul.   1              - 1s
 ...")

(defparameter *time-steps-history-file*
  (merge-pathnames (make-pathname :name "TimeSteps" :type "history"))
  "Month names as used in http://hpiers.obspm.fr/eoppc/bul/bulc/TimeSteps.history.")

(defparameter *leap-second-months* (pairlis '(jan. feb. march apr. may jun. jul. aug. sept. oct. nov. dec.)
                                            '(1 2 3 4 5 6 7 8 9 10 11 12)))

(defun initialize-leap-seconds ()
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request *time-steps-history-url*)
    (if (= status-code 200)
        (with-open-file (stream *time-steps-history-file*
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string body stream))
        (warn "Coudn't get the latest leap seconds information from ~A.~%Falling back to cached data in ~A."
              uri *time-steps-history-file*)))
  (with-open-file (stream *time-steps-history-file*
                          :direction :input :if-does-not-exist :error)
    (loop for time-record = (string-trim " 	" (read-line stream))
       until (equal 1980 (read-from-string time-record nil)))
    (setf
     *leap-seconds*
     (loop for time-record = (string-trim " s	" (read-line stream))
        with leap = nil and leap-date = nil
        until (string-equal (subseq time-record 1 20) (make-string 19 :initial-element #\-))
        do (with-input-from-string (line time-record)
             (let ((year (read line))
                   (month (cdr (assoc (read line) *leap-second-months*)))
                   (date (read line))
                   (sign (read line))
                   (seconds (read line)))
               (setf leap-date (encode-universal-time 0 0 0 date month year 0)
                     leap (if (eq sign '-) (- seconds) seconds))))
        sum leap into leap-sum
        collect leap-date into leap-dates
        collect leap-sum into leap-sums
        finally (return (sort (pairlis leap-dates leap-sums) #'< :key #'car))))))

(defparameter *gps-epoch* (encode-universal-time 0 0 0 6 1 1980 0))
(defparameter *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))

(defun gps-start-of-week (time)
  "Begin of a GPS week (approximately Sunday 00:00)"
  (let ((week-length (* 7 24 3600))
        (leap-seconds (cdr (find time *leap-seconds* :key #'car :test #'> :from-end t))))
    (+ (* (floor (- time *gps-epoch*) week-length)
          week-length)
       *gps-epoch*
       leap-seconds)))

(defun utc-from-gps (utc-approximately gps-week-time)
  "Convert GPS week time into UTC.  gps-week-time may be of type float; in this case a non-integer is returned which can't be fed into decode-universal-time."
  (+ (gps-start-of-week utc-approximately) gps-week-time))

(defun utc-from-unix (unix-time)
  "Convert UNIX UTC to Lisp time."
  (+ unix-time *unix-epoch*))

(defun event-number (recorded-device-id)
  "Return the GPS event number corresponding to recorded-device-id of camera (etc.)"
  (let ((event-table (pairlis '(21 22 11 12)
                              '("1" "1" "2" "2"))))
    (cdr (assoc recorded-device-id event-table)))) ; TODO: make a saner version

(defun almost= (x y epsilon)
  (< (abs (- x y)) epsilon))

(defun geographic-to-utm (utm-zone longitude latitude &optional (height 0))
  "Return UTM utm-zone representation of geographic coordinates."
  (let ((command
         (format nil "cs2cs +proj=latlong +datum=WGS84 +to +proj=utm +ellps=WGS84 +zone=~D +units=m +no_defs -f %.9f" utm-zone)))
    (multiple-value-bind (standard-output error-output exit-status) 
        (trivial-shell:shell-command command
                                     :input (format nil "~F ~F ~F" longitude latitude height))
      (unless (zerop exit-status) (error "Attempt to call `~A´ returned ~D: ~A"
                                         command exit-status error-output))
      (with-input-from-string (stream standard-output)
        (loop
           for number = (read stream nil)
           repeat 3
           while number
           do (assert (numberp number))
           collect number)))))

(defun utm-zone (longitude)
  "Return UTM zone number belonging to longitude."
  (1+ (floor (+ longitude 180) 6)))

(defun assert-utm-zone (longitude-median longitude-leeway longitude latitude geographic-height easting northing cartesian-height)
  "Check if, given latitude and longitude, easting and northing are calculated in the UTM zone belonging to longitude-median."
  (let ((epsilon 1d-1))
    (unless
        (or (every #'(lambda (x y) (almost= x y epsilon))
                   (geographic-to-utm (utm-zone (- longitude-median longitude-leeway))
                                      latitude longitude geographic-height)
                   (list easting northing cartesian-height))
            (every #'(lambda (x y) (almost= x y epsilon))
                   (geographic-to-utm (utm-zone (+ longitude-median longitude-leeway))
                                      latitude longitude geographic-height)
                   (list easting northing cartesian-height)))
      (error "The longitude median ~A should be in or near UTM zone ~D.  This is inconsistent with the easting values I was given.  Offending coordinates: (~A ~A ~A) (~A ~A ~A)."
             longitude-median (utm-zone longitude-median) longitude latitude geographic-height easting northing cartesian-height))))

(defun assert-gps-points-sanity (gps-points)
  "Check if gps-points (as returned by collect-gps-data) are ok."
  (loop
     for gps-event in gps-points
     for gps-event-vector = (cdr gps-event)
     for first-latitude = (latitude (aref gps-event-vector 0))
     for first-longitude = (longitude (aref gps-event-vector 0))
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
                         first-latitude first-longitude
                         first-geographic-height
                         first-easting first-northing
                         first-cartesian-height)))

(defun get-measurement-id (common-table-name dir-path)
  "Get measurement-id associated with dir-path and acquisition-project-id.  Create a fresh matching record if necessary."
  (let ((acquisition-project
         (car (select-dao 'sys-acquisition-project
                          (:= 'common-table-name common-table-name)))))
    (assert acquisition-project)
    (let* ((acquisition-project-id (acquisition-project-id acquisition-project))
           (measurement
            (or (car (select-dao 'sys-measurement
                                 (:and (:= 'acquisition-project-id acquisition-project-id)
                                       (:= 'directory dir-path))))
                (insert-dao (make-instance 'sys-measurement
                                           :acquisition-project-id acquisition-project-id
                                           :directory dir-path
                                           :fetch-defaults t)))))
      (measurement-id measurement))))

(defun store-images-and-points (common-table-name dir-path &key (epsilon 1d-4) (root-dir (user-homedir-pathname)))
  "Link images to GPS points; store both into their respective DB tables.  Images become linked to GPS points when their respective times differ by less than epsilon seconds, and when the respective events match.  dir-path is a (probably absolute) path to a directory that contains one set of measuring data.  root-dir must be equal for all pojects."
  ;; TODO: epsilon could be a range.  We would do a raw mapping by (a bigger) time epsilon and then take speed into account.
  (initialize-leap-seconds)
  (let* ((images
          (collect-pictures-directory-data dir-path))
         (estimated-time
          (loop
             for i across images
             unless (fake-trigger-time-p i)
             do (return (trigger-time i))))
         (gps-points
          (collect-gps-data dir-path estimated-time))
         (gps-start-pointers (loop
                                for i in gps-points
                                collect (cons (car i) 0)))
         (dir-below-root-dir (print(enough-namestring (string-right-trim "/\\ " dir-path) root-dir))))
    (assert-gps-points-sanity gps-points)
    (loop
       for i across images
       for image-event-number = (event-number (recorded-device-id i))
       for image-time = (trigger-time i)
       for matching-point =
       (let ((gps-start-pointer
              (cdr (assoc image-event-number gps-start-pointers :test #'equal))))
         (loop
            for gps-pointer from gps-start-pointer
            for gps-point across (subseq (cdr (assoc image-event-number gps-points :test #'equal))
                                         gps-start-pointer)
            when (almost= (gps-time gps-point) image-time epsilon)
            do (setf (cdr (assoc image-event-number gps-start-pointers :test #'equal))
                     gps-pointer) ; remember index of last matching point
            and return gps-point))
       if matching-point
       do (let ((point-id               ; TODO: consider using transaction
                 (or (point-id matching-point) ; We've hit a point twice.
                     (sequence-next (point-id-sequence-name matching-point))))
                (measurement-id (get-measurement-id common-table-name dir-below-root-dir)))
            (setf (point-id i) point-id
                  (point-id matching-point) point-id
                  (measurement-id matching-point) measurement-id
                  (measurement-id i) measurement-id
                  (trigger-time matching-point) image-time)
            (save-dao matching-point)
            (save-dao i))
       else do (print i))               ; TODO: log orphaned images
    ))
