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
    (declare (ignore headers stream must-close reason-phrase))
    (if (= status-code 200)
        (with-open-file (stream *time-steps-history-file*
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string body stream)
          (cl-log:log-message :debug "Downloaded leap second information from ~A" uri))
        (cl-log:log-message :warning "Coudn't get the latest leap seconds information from ~A.~%Falling back to cached data in ~A."
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
  (let ((command                        ; TODO:  perhaps reduce dependencies by asking PostGIS; we need a PostgreSQL connection anyway (or do we?)
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
            (execute (:update (dao-table-name (class-of matching-point))
                              :set 'coordinates
                              (:st_geomfromewkt (format nil "SRID=4326; POINT(~E ~E ~E)"
                                                        (longitude matching-point)
                                                        (latitude matching-point)
                                                        (ellipsoid-height matching-point)))
                              :where (:= 'point-id (point-id matching-point))))
            (save-dao i))
       else do (cl-log:log-message :orphan "Couldn't map to any point: ~A, byte ~A. ~:[~; It didn't have a decent trigger time anyway.~]"
                                   (filename i) (image-byte-position i) (fake-trigger-time-p i)))))

(defun store-camera-hardware
    (&key (sensor-width-pix (error "sensor-width-pix needed."))
     (sensor-height-pix (error "sensor-height-pix needed."))
     (pix-size (error "pix-size needed."))
     (channels (error "channels needed."))
     (pix-depth (error "pix-depth needed."))
     (color-raiser (error "color-raiser needed."))
     (pix-colors (error "pix-colors needed."))
     (serial-number (error "serial-number needed."))
     (description (error "description needed."))
     (try-overwrite t))
  "Store a new record in table sys-camera-hardware, or try updating an existing one.  Return camera-hardware-id of the altered record."
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
                 (pix-colors-slot pix-colors)
                 (serial-number-slot serial-number)
                 (description-slot description))
        record
      (setf sensor-width-pix-slot sensor-width-pix
            sensor-height-pix-slot sensor-height-pix
            pix-size-slot pix-size
            channels-slot channels
            pix-depth-slot pix-depth
            color-raiser-slot color-raiser
            pix-colors-slot pix-colors
            serial-number-slot serial-number
            description-slot description))
    (save-dao record)
    (camera-hardware-id record)))

(defun store-lens
    (&key (c (error "c needed."))
     (serial-number (error "serial-number needed."))
     (description (error "description needed."))
     (try-overwrite t))
  "Store a new record in table sys-lens, or try updating an existing one.  Return lens-id of the altered record."
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
    (save-dao record)
    (lens-id record)))

(defun store-generic-device
    (&key (camera-hardware-id :null) (lens-id :null) (scanner-id :null))
  "Store a new record in table sys-generic-device.  Return generic-device-id of the new record."
  (assert (notevery #'(lambda (x) (eq :null x)) (list camera-hardware-id lens-id scanner-id))
          () "Generic device: not enough components.")
  (let ((record (make-instance 'sys-generic-device
                               :camera-hardware-id camera-hardware-id
                               :lens-id lens-id
                               :scanner-id scanner-id
                               :fetch-defaults t)))
    (save-dao record)
    (generic-device-id record)))

(defun store-device-stage-of-life
    (&key (recorded-device-id (error "recorded-device-id needed."))
     (event-number (error "event-number needed."))
     (generic-device-id (error "generic-device-id needed."))
     (vehicle-name (error "vehicle-name needed."))
     (casing-name (error "casing-name needed."))
     (computer-name (error "computer-name needed."))
     (computer-interface-name (error "computer-interface-name needed."))
     (mounting-date (error "mounting-date needed."))
     (unmounting-date :null)
     (try-overwrite t))
  "Store a new record in table sys-device-stage-of-life, or try updating an existing one.  Return device-stage-of-life-id of the altered record."
  (let ((record
         (or (when try-overwrite
               (car (select-dao 'sys-device-stage-of-life
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
    (save-dao record)
    (device-stage-of-life-id record)))

(defun store-camera-calibration
    (&key
     (device-stage-of-life-id (error "device-stage-of-life-id needed."))
     (date (error "date needed."))
     (person (error "person needed."))
     (main-description (error "main-description needed."))
     (debug (error "debug needed."))
     (photogrammetry-version (error "photogrammetry-version needed."))
     (mounting-angle (error "mounting-angle needed."))
     (inner-orientation-description (error "inner-orientation-description needed."))
     (c (error "c needed."))
     (xh (error "xh needed."))
     (yh (error "yh needed."))
     (a1 (error "a1 needed."))
     (a2 (error "a2 needed."))
     (a3 (error "a3 needed."))
     (b1 (error "b1 needed."))
     (b2 (error "b2 needed."))
     (c1 (error "c1 needed."))
     (c2 (error "c2 needed."))
     (r0 (error "r0 needed."))
     (outer-orientation-description (error "outer-orientation-description needed."))
     (dx (error "dx needed."))
     (dy (error "dy needed."))
     (dz (error "dz needed."))
     (omega (error "omega needed."))
     (phi (error "phi needed."))
     (kappa (error "kappa needed."))
     (boresight-description (error "boresight-description needed."))
     (b-dx (error "b-dx needed."))
     (b-dy (error "b-dy needed."))
     (b-dz (error "b-dz needed."))
     (b-ddx (error "b-ddx needed."))
     (b-ddy (error "b-ddy needed."))
     (b-ddz (error "b-ddz needed."))
     (b-rotx (error "b-rotx needed."))
     (b-roty (error "b-roty needed."))
     (b-rotz (error "b-rotz needed."))
     (b-drotx (error "b-drotx needed."))
     (b-droty (error "b-droty needed."))
     (b-drotz (error "b-drotz needed.")))
  "Store a new record of camera-calibration in table sys-device-stage-of-life, or update an existing one.  Return device-stage-of-life-id and date of the altered record."
  (let ((record
         (or (car (select-dao 'sys-camera-calibration
                              (:and (:= 'device-stage-of-life-id device-stage-of-life-id)
                                    (:= 'dat date))))
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
           (b-drotz-slot b-drotz))
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
            b-drotz-slot b-drotz))
    (save-dao record)
    (values (device-stage-of-life-id record)
            (date record))))

#|
(with-connection '("phoros-dev" "postgres" "passwd" "host")
  (nuke-all-tables)
  (create-data-tables "yyyy")
  (store-camera-hardware :sensor-width-pix 7000 :sensor-height-pix 800 :pix-size .003 :channels 3 :pix-depth 17 :color-raiser #(1 2 3) :pix-colors #(4 5 6) :serial-number "18" :description "yyy" :try-overwrite t)
  (store-lens :c 10.5 :serial-number "17.8.8" :description "blahBlah3" :try-overwrite nil)
  (store-generic-device :camera-hardware-id 1 :lens-id 1)
  (store-device-stage-of-life :recorded-device-id "1" :event-number "777" :generic-device-id 1 :vehicle-name "Auto" :casing-name "Vorn links" :computer-name "ccdheck" :computer-interface-name "eth0" :mounting-date "2010-01-30T07:00-1")
  (store-images-and-points "yyyy" "/home/bertb/phoros-testdata/mitsa-small/"))
|#