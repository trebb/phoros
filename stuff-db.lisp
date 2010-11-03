(in-package :phoros)

(defstruct gps-point
  "Information about one gps point from applanix/**/*event*.txt."
  event-number
  time
  longitude latitude ellipsoid-height
  roll pitch heading
  east-velocity north-velocity up-velocity
  east-sd north-sd height-sd
  roll-sd pitch-sd heading-sd
  easting northing cartesian-height)

(defstruct (positioned-image (:include gps-point))
  "Information for one image from a .pictures file, and matching information from applanix/**/*event*.txt."
  trigger-time fake-trigger-time-p camera-timestamp recorded-device-id path file-position orphanp)

(defun positioned-image-slots-setf (positioned-image gps-point)
  "Set slots of positioned-image to the respective values from gps-point."
  (setf (positioned-image-event-number positioned-image) (gps-point-event-number gps-point))
  (setf (positioned-image-time positioned-image) (gps-point-time gps-point))
  (setf (positioned-image-longitude positioned-image) (gps-point-longitude gps-point))
  (setf (positioned-image-latitude positioned-image) (gps-point-latitude gps-point))
  (setf (positioned-image-ellipsoid-height positioned-image) (gps-point-ellipsoid-height gps-point))
  (setf (positioned-image-roll positioned-image) (gps-point-roll gps-point))
  (setf (positioned-image-pitch positioned-image) (gps-point-pitch gps-point))
  (setf (positioned-image-heading positioned-image) (gps-point-heading gps-point))
  (setf (positioned-image-east-velocity positioned-image) (gps-point-east-velocity gps-point))
  (setf (positioned-image-north-velocity positioned-image) (gps-point-north-velocity gps-point))
  (setf (positioned-image-up-velocity positioned-image) (gps-point-up-velocity gps-point))
  (setf (positioned-image-east-sd positioned-image) (gps-point-east-sd gps-point))
  (setf (positioned-image-north-sd positioned-image) (gps-point-north-sd gps-point))
  (setf (positioned-image-height-sd positioned-image) (gps-point-height-sd gps-point))
  (setf (positioned-image-roll-sd positioned-image) (gps-point-roll-sd gps-point))
  (setf (positioned-image-pitch-sd positioned-image) (gps-point-pitch-sd gps-point))
  (setf (positioned-image-heading-sd positioned-image) (gps-point-heading-sd gps-point))
  (setf (positioned-image-easting positioned-image) (gps-point-easting gps-point))
  (setf (positioned-image-northing positioned-image) (gps-point-northing gps-point))
  (setf (positioned-image-cartesian-height positioned-image) (gps-point-cartesian-height gps-point)))                  

(defun collect-pictures-file-data (path root-dir-path)
  "Return vector of positioned-image structures containing data from the picture-headers of the .pictures file in path; path is stored relative to root-dir-path."
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
         and trigger-time = (utc-from-unix (find-keyword-value path "timeTrigger=" picture-start))
         and camera-timestamp = (find-keyword-value path "cameraTimestamp=" picture-start)
         and recorded-device-id = (find-keyword-value path "cam=" picture-start)
         while picture-start
         do (vector-push-extend (make-positioned-image :trigger-time trigger-time
                                                       :camera-timestamp camera-timestamp
                                                       :recorded-device-id recorded-device-id
                                                       :path (enough-namestring path root-dir-path)
                                                       :file-position picture-start)
                                pictures-data)
         finally
         (repair-missing-trigger-times pictures-data)
         (return  pictures-data)))))

(defun repair-missing-trigger-times (positioned-images)
  "Use slot camera-timestamp to fake missing trigger-times."
  (labels ((slope (offending-index good-index)
             (/ (- (positioned-image-trigger-time (aref positioned-images (+ offending-index (- good-index 2))))
                   (positioned-image-trigger-time (aref positioned-images (+ offending-index (+ good-index 2)))))
                (- (positioned-image-camera-timestamp (aref positioned-images (+ offending-index (- good-index 2))))
                   (positioned-image-camera-timestamp (aref positioned-images (+ offending-index (+ good-index 2)))))))
           (intercept (offending-index good-index slope)
             (- (positioned-image-trigger-time (aref positioned-images (+ offending-index good-index)))
                (* slope (positioned-image-camera-timestamp (aref positioned-images (+ offending-index good-index))))))
           (fake-trigger-time (offending-index good-index)
             (let* ((m (slope offending-index good-index))
                    (t-0 (intercept offending-index good-index m)))
               (+ (* m (positioned-image-camera-timestamp (aref positioned-images offending-index)))
                  t-0))))
    (dolist (offending-index
              (print(loop
                 with previous-trigger-time = 0
                 for h across positioned-images
                 for i from 0
                 for trigger-time = (positioned-image-trigger-time h)
                 if (> (+ trigger-time 1d-4) previous-trigger-time)
                 do (setf previous-trigger-time trigger-time)
                 else collect i)))
      (let ((good-index-offset -3))
        (handler-bind ((error #'(lambda (x) (invoke-restart 'next-try))))
          (setf (positioned-image-trigger-time (aref positioned-images offending-index))
                (restart-case (fake-trigger-time offending-index good-index-offset)
                  (next-try ()
                    (incf good-index-offset 6)
                    (fake-trigger-time offending-index good-index-offset)))
                (positioned-image-fake-trigger-time-p (aref positioned-images offending-index))
                t))))))

(defun collect-pictures-directory-data (dir-path root-dir-path)
  "Return vector of positioned-image structures of the .pictures files in dir-path; path is stored relative to root-dir-path."
  (reduce #'(lambda (x1 x2) (merge 'vector x1 x2 #'< :key #'positioned-image-trigger-time))
          (mapcar #'(lambda (x) (collect-pictures-file-data x root-dir-path))
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
                 (with-input-from-string (line-content line)
                   (let ((gps-week-time (read line-content nil))
                         (distance (read line-content nil))
                         (easting (read line-content nil))
                         (northing (read line-content nil))
                         (ellipsoid-height (read line-content nil))
                         (latitude (read line-content nil))
                         (longitude (read line-content nil))
                         (cartesian-height (read line-content nil))
                         (roll (read line-content nil))
                         (pitch (read line-content nil))
                         (heading (read line-content nil))
                         (east-velocity (read line-content nil))
                         (north-velocity (read line-content nil))
                         (up-velocity (read line-content nil))
                         (east-sd (read line-content nil))
                         (north-sd (read line-content nil))
                         (height-sd (read line-content nil))
                         (roll-sd (read line-content nil))
                         (pitch-sd (read line-content nil))
                         (heading-sd (read line-content nil)))
                     (make-gps-point
                      :time (utc-from-gps estimated-utc gps-week-time)
                      :event-number gps-event-number
                      :longitude longitude :latitude latitude
                      :ellipsoid-height ellipsoid-height
                      :roll roll :pitch pitch :heading heading
                      :east-velocity east-velocity
                      :north-velocity north-velocity
                      :up-velocity up-velocity
                      :east-sd east-sd :north-sd north-sd :height-sd height-sd
                      :roll-sd roll-sd :pitch-sd pitch-sd :heading-sd heading-sd
                      :easting easting :northing northing
                      :cartesian-height cartesian-height)))
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
     for first-latitude = (gps-point-latitude (aref gps-event-vector 0))
     for first-longitude = (gps-point-longitude (aref gps-event-vector 0))
     for first-geographic-height = (gps-point-ellipsoid-height (aref gps-event-vector 0))
     for first-easting = (gps-point-easting (aref gps-event-vector 0))
     for first-northing = (gps-point-northing (aref gps-event-vector 0))
     for first-cartesian-height = (gps-point-cartesian-height (aref gps-event-vector 0))
     for longitude-median =
       (loop
          for point across gps-event-vector
          for i from 1
          sum (gps-point-longitude point) into longitude-sum
          finally (return (/ longitude-sum i)))
     do (assert-utm-zone longitude-median 1
                         first-latitude first-longitude
                         first-geographic-height
                         first-easting first-northing
                         first-cartesian-height)))

(defun collect-positioned-images (dir-path root-path &optional (epsilon 1d-4))
  "Return a vector of positioned images.  Images get positioned by merging with gps-points when their respective times differ by less than epsilon seconds."
  ;; TODO: epsilon could be a range.  We would do a raw mapping by (a bigger) time epsilon and then take speed into account.
  (initialize-leap-seconds)
  (let* ((images
          (collect-pictures-directory-data dir-path root-path))
         (estimated-time
          (loop
             for i across images
             unless (positioned-image-fake-trigger-time-p i)
             do (return (positioned-image-trigger-time i))))
         (gps-points
          (collect-gps-data dir-path estimated-time))
         (gps-start-pointers (loop
                                for i in gps-points
                                collect (cons (car i) 0))))
    (assert-gps-points-sanity gps-points)
    (time(loop
       for i across images
       for image-event-number = (event-number (positioned-image-recorded-device-id i))
       for image-time = (positioned-image-trigger-time i)
       for matching-point =
              (let ((gps-start-pointer
                     (cdr (assoc image-event-number gps-start-pointers :test #'equal))))
                (loop
                   for gps-pointer from gps-start-pointer
                   for gps-point across (subseq (cdr (assoc image-event-number gps-points :test #'equal))
                                                gps-start-pointer)
                   when (almost= (gps-point-time gps-point) image-time epsilon)
                   do (setf (cdr (assoc image-event-number gps-start-pointers :test #'equal))
                            gps-pointer)
                   and return gps-point))
       if matching-point
       do (positioned-image-slots-setf i matching-point)
       else do (setf (positioned-image-orphanp i) t)))
    images))
