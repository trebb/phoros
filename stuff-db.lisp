(in-package :phoros)

;(directory (make-pathname :directory '(:absolute "home" "bertb" "phoros-testdata" "mnt" "data0" "Rohdaten" "mittelsachsen_0002" :wild-inferiors) :name :wild :type "pictures"))


(with-open-file (s "/home/bertb/phoros-testdata/mnt/data0/Rohdaten/mittelsachsen_0002/applanix/points/mitsa716_event1.txt") 
  (loop for line = (read-line s)
     for i from 0 to 35
     until (string-equal (string-trim " " line) "(time in Sec, distance in Meters, position in Meters, lat, long in Degrees, orientation angles and SD in Degrees, velocity in Meter/Sec, position SD in Meters)")
     finally 
     (read-line s)
     (assert (< i 35) () "Unfamiliar header.  Check Applanix file format." nil))
  (let ((time (read s))
        (distance (read s))
        (easting (read s))
        (northing (read s))
        (ellipsoid-height (read s))
        (latitude (read s))
        (longitude (read s))
        (ellipsoid-height-0 (read s))
        (roll (read s))
        (pitch (read s))
        (heading (read s))
        (east-velocity (read s))
        (north-velocity (read s))
        (up-velocity (read s))
        (east-sd (read s))
        (north-sd (read s))
        (height-sd (read s))
        (roll-sd (read s))
        (pitch-sd (read s))
        (heading-sd (read s)))
    ))

(defparameter *leap-seconds* nil
  "An alist of (time . leap-seconds) elements.  leap-seconds are to be added to GPS time to get UTC.")

(defparameter *time-steps-history-url* "http://hpiers.obspm.fr/eoppc/bul/bulc/TimeSteps.history")

(defparameter *time-steps-history-file*
  (merge-pathnames (make-pathname :name "TimeSteps" :type "history")))

(with-input-from-string (s *t*) (loop for time-record = (string-trim " 	" (read-line s))
                                   until (equal 1980 (print(read-from-string time-record nil))))
                        (loop for time-record = (print  (string-trim " s	" (read-line s)))
                           until (string-equal (subseq time-record 1 20) (make-string 19 :initial-element #\-))
                           do (with-input-from-string (line time-record)
                                (let((year (read line))
                                     (month (cdr (assoc (read line) *leap-second-months*)))
                                     (date (read line))
                                     (sign (read line))
                                     (seconds (read line)))
                                  (print(encode-universal-time 0 0 0 date month year 0))
                                  (print (* seconds (if (eq sign '-) -1 1)))
                                  )
                                )))

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
        (warn "Coudn't get the latest leap seconds information from ~A.~%Falling back to cached datain ~A."
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



        
