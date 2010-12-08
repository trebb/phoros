;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010 Bert Burgemeister
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


;;;; The UNIX command line interface

(in-package :phoros)

(defparameter *cli-main-options*
  '((("help" #\h) :action #'cli-help-action  :documentation "Print this help and exit.")
    ("version" :action #'cli-version-action :documentation "Output version information and exit.")
    ("verbose" :type integer :initial-value 0 :documentation "Emit increasing amounts of debugging output.")
    ("log-dir" :type string :initial-value "" :documentation "Where to put the log files.  Created if necessary; should end with a slash.")
    ("check-db" :action #'check-db-action :documentation "Check database connection and exit.")
    ("check-dependencies" :action #'check-dependencies-action :documentation "Check presence of dependencies on local system and exit.")
    ("nuke-all-tables" :action #'nuke-all-tables-action :documentation "Ask for confirmation, then delete anything in database and exit.")
    ("create-sys-tables" :action #'create-sys-tables-action :documentation "Ask for confirmation, then create in database a set of sys-* tables (tables shared between all projects).  The database should probably be empty before you try this.")
    ("create-acquisition-project" :type string :action #'create-acquisition-project-action :documentation "Create a fresh set of canonically named data tables.  The string argument is the acquisition project name.  It will be stored in table sys-acquisition-project, field common-table-name, and used as a common part of the data table names.")))

(defparameter *cli-db-connection-options*
  '((("host" #\H) :type string :initial-value "localhost" :documentation "Database server.")
    (("port" #\P) :type integer :initial-value 5432 :documentation "Port on database server.")
    (("database" #\D) :type string :initial-value "phoros" :documentation "Name of database.")
    (("user" #\U) :type string :documentation "Database user.")
    (("password" #\W) :type string :documentation "Database user's password.")
    ("use-ssl" :type string :initial-value "no" :documentation "Use SSL in database connection. [yes|no|try]")))

(defparameter *cli-get-image-options*
  '(("get-image" :action #'get-image-action :documentation "Get a single image from a .pictures file, print its trigger-time to stdout, and exit.")
    ("count" :type integer :initial-value 0 :documentation "Image number in .pictures file.")
    ("byte-position" :type integer :documentation "Byte position of image in .pictures file.")
    ("in" :type string :documentation "Path to .pictures file.")
    ("out" :type string :initial-value "phoros-get-image.png" :documentation "Path to to output .png file.")
    ;; The way it should be had we two-dimensional arrays in postmodern:
    ;;("bayer-pattern" :type string :list t :optional t :action :raw-bayer-pattern :documentation "The first pixels of the first row.  Repeat this option to describe following row(s).  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
    ("bayer-pattern" :type string :optional t :action :raw-bayer-pattern :documentation "The first pixels of the first row.  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")))

(defparameter *cli-camera-hardware-options*
  '(("store-camera-hardware" :action #'store-camera-hardware-action :documentation "Put new camera-hardware data into the database; print camera-hardware-id to stdout.")
    ("sensor-width-pix" :type integer :documentation "Width of camera sensor.")
    ("sensor-height-pix" :type integer :documentation "Height of camera sensor.")
    ("pix-size" :type string :documentation "Camera pixel size in millimetres (float).")
    ("channels" :type integer :documentation "Number of color channels")
    ("pix-depth" :type integer :initial-value 255 :documentation "Greatest possible pixel value.")
    ("color-raiser" :type string :initial-value "1,1,1" :action :raw-color-raiser :documentation "Multipliers for the individual color components.  Example: 1.2,1,.8 multiplies red by 1.2 and blue by 0.8.")
    ;; The way it should be had we two-dimensional arrays in postmodern:
    ;;("bayer-pattern" :type string :list t :optional t :action :raw-bayer-pattern :documentation "The first pixels of the first row.  Repeat this option to describe following row(s).  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
    ("bayer-pattern" :type string :optional t :action :raw-bayer-pattern :documentation "The first pixels of the first row.  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
    ("serial-number" :type string :documentation "Serial number.")
    ("description" :type string :documentation "Description of camera.")
    ("try-overwrite" :type boolean :initial-value "yes" :documentation "Overwrite matching camera-hardware record if any.")))

(defparameter *cli-lens-options*
  '(("store-lens" :action #'store-lens-action :documentation "Put new lens data into the database; print lens-id to stdout.")
    ("c" :type string :documentation "Nominal focal length in millimetres.")
    ("serial-number" :type string :documentation "Serial number.")
    ("description" :type string :documentation "Lens desription.")
    ("try-overwrite" :type boolean :initial-value "yes" :documentation "Overwrite matching lens record if any.")))

(defparameter *cli-generic-device-options*
  '(("store-generic-device" :action #'store-generic-device-action :documentation "Put a newly defined generic-device into the database; print generic-device-id to stdout.")
    ("camera-hardware-id" :type integer :documentation "Numeric camera hardware id in database.")
    ("lens-id" :type integer :documentation "Numeric lens id in database.")))

(defparameter *cli-device-stage-of-life-options*
  '(("store-device-stage-of-life" :action #'store-device-stage-of-life-action :documentation "Put a newly defined device-stage-of-life into the database; print device-stage-of-life-id to stdout.")
    ("recorded-device-id" :type string :documentation "Device id stored next to the measuring data.")
    ("event-number" :type string :documentation "GPS event that triggers this generic device.")
    ("generic-device-id" :type integer :documentation "Numeric generic-device id in database.")
    ("vehicle-name" :type string :documentation "Descriptive name of vehicle.")
    ("casing-name" :type string :documentation "Descriptive name of device casing.")
    ("computer-name" :type string :documentation "Name of the recording device.")
    ("computer-interface-name" :type string :documentation "Interface at device.")
    ("mounting-date" :type string :documentation "Time this device constellation became effective.  Format: `2010-11-19T13:49+01´.")))

(defparameter *cli-device-stage-of-life-end-options*
  '(("store-device-stage-of-life-end" :action #'store-device-stage-of-life-end-action :documentation "Put an end date to a device-stage-of-life in the database; print device-stage-of-life-id to stdout.")
    ("device-stage-of-life-id" :type string :documentation "Id of the device-stage-of-life to put to an end.")
    ("unmounting-date" :type string :documentation "Time this device constellation ceased to be effective.  Format: `2010-11-19T17:02+01´.")))

(defparameter *cli-camera-calibration-options*
  '(("store-camera-calibration" :action #'store-camera-calibration-action :documentation "Put new camera-calibration into the database; print generic-device-id and calibration date to stdout.")
    ("device-stage-of-life-id" :type string :documentation "This tells us what hardware this calibration is for.")
    ("date" :type string :documentation "Date of calibration.  Format: `2010-11-19T13:49+01´.")
    ("person" :type string :documentation "Person who did the calibration.")
    ("main-description" :type string :documentation "Regarding this entire set of calibration data")
    ("debug" :type string :documentation "If true: not for production use; may be altered or deleted at any time.")
    ("photogrammetry-version" :type string :documentation "Software version used to create this data.")
    ("mounting-angle" :type integer :documentation "Head up = 0; right ear up = 90; left ear up = -90; head down = 180.")
    ("inner-orientation-description" :type string :documentation "Comments regarding inner orientation calibration.")
    ("c" :type string :documentation "Inner orientation: focal length.")
    ("xh" :type string :documentation "Inner orientation: principal point displacement.")
    ("yh" :type string :documentation "Inner orientation: principal point displacement.")
    ("a1" :type string :documentation "Inner orientation: radial distortion.")
    ("a2" :type string :documentation "Inner orientation: radial distortion.")
    ("a3" :type string :documentation "Inner orientation: radial distortion.")
    ("b1" :type string :documentation "Inner orientation: asymmetric and tangential distortion.")
    ("b2" :type string :documentation "Inner orientation: asymmetric and tangential distortion.")
    ("c1" :type string :documentation "Inner orientation: affinity and shear distortion.")
    ("c2" :type string :documentation "Inner orientation: affinity and shear distortion.")
    ("r0" :type string :documentation "Inner orientation.")
    ("outer-orientation-description" :type string :documentation "Comments regarding outer orientation calibration.")
    ("dx" :type string :documentation "Outer orientation; in metres.")
    ("dy" :type string :documentation "Outer orientation; in metres.")
    ("dz" :type string :documentation "Outer orientation; in metres.")
    ("omega" :type string :documentation "Outer orientation.")
    ("phi" :type string :documentation "Outer orientation.")
    ("kappa" :type string :documentation "Outer orientation.")
    ("boresight-description" :type string :documentation "Comments regarding boresight alignment calibration.")
    ("b-dx" :type string :documentation "Boresight alignment.")
    ("b-dy" :type string :documentation "Boresight alignment.")
    ("b-dz" :type string :documentation "Boresight alignment.")
    ("b-ddx" :type string :documentation "Boresight alignment.")
    ("b-ddy" :type string :Documentation "Boresight alignment.")
    ("b-ddz" :type string :documentation "Boresight alignment.")
    ("b-rotx" :type string :documentation "Boresight alignment.")
    ("b-roty" :type string :documentation "Boresight alignment.")
    ("b-rotz" :type string :documentation "Boresight alignment.")
    ("b-drotx" :type string :documentation "Boresight alignment.")
    ("b-droty" :type string :documentation "Boresight alignment.")
    ("b-drotz" :type string :documentation "Boresight alignment.")
    ("nx" :type string :documentation "X component of unit vector of vehicle ground plane.")
    ("ny" :type string :documentation "Y component of unit vector of vehicle ground plane.")
    ("nz" :type string :documentation "Z component of unit vector of vehicle ground plane.")
    ("d" :type string :documentation "Distance of vehicle ground plane.")))

(defparameter *cli-store-images-and-points-options*
  '((("store-images-and-points" #\s) :type string :action #'store-images-and-points-action :documentation "Link images to GPS points; store both into their respective DB tables.  Images become linked to GPS points when their respective times differ by less than epsilon seconds, and when the respective events match.  The string argument is the acquisition project name.")
    (("directory" #\d) :type string :documentation "Directory containing one set of measuring data.")
    (("common-root" #\r) :type string :documentation "The root part of directory that is equal for all pojects.  TODO: come up with some sensible default.")
    ("epsilon" :type string :initial-value ".001" :documentation "Difference in seconds below which two timestamps are considered equal.")
    ("aggregate-events" :type nil :documentation "Put all GPS points in one bucket, disregarding any event numbers.  Use this if you have morons setting up your generic-device.  Hundreds of orphaned images may indicate this is the case.")))

(defparameter *cli-options* (append *cli-main-options* *cli-db-connection-options* *cli-get-image-options* *cli-camera-hardware-options* *cli-lens-options* *cli-generic-device-options* *cli-device-stage-of-life-options* *cli-device-stage-of-life-end-options* *cli-camera-calibration-options* *cli-store-images-and-points-options*))

(defun main ()
  "The UNIX command line entry point."
;;  (handler-bind ((serious-condition (lambda (c)
;;                                      (declare (ignore c))
;;                                      (sb-debug:backtrace))))
  #+sbcl (sb-ext:disable-debugger)
  (handler-case
      (progn
        (cffi:use-foreign-library photogrammetrie)
        (command-line-arguments:compute-and-process-command-line-options *cli-options*))
    (serious-condition (c)
      (cl-log:log-message :warning "Cancelled: ~A" c)
      (format *error-output* "~A~&" c))))

(defun cli-help-action (&rest rest)
  "Print --help message."
  (declare (ignore rest))
  (let ((format-headline (formatter "~&~95,,,'#@<~A ~>")))
    (format *standard-output*
            "~&Usage: phoros [options] ...~&~A"
            (asdf:system-long-description (asdf:find-system :phoros)))
    (format *standard-output* format-headline "Main Options")
    (command-line-arguments:show-option-help *cli-main-options*)
    (format *standard-output* format-headline "Database Connection")
    (command-line-arguments:show-option-help *cli-db-connection-options*)
    (format *standard-output* format-headline "Examine .pictures File")
    (command-line-arguments:show-option-help *cli-get-image-options*)
    (format *standard-output* format-headline "Camera Hardware Parameters")
    (command-line-arguments:show-option-help *cli-camera-hardware-options*)
    (format *standard-output* format-headline "Lens Parameters")
    (command-line-arguments:show-option-help *cli-lens-options*)
    (format *standard-output* format-headline "Generic Device Definition")
    (command-line-arguments:show-option-help *cli-generic-device-options*)
    (format *standard-output* format-headline "Device Stage-Of-Life Definition")
    (command-line-arguments:show-option-help *cli-device-stage-of-life-options*)
    (format *standard-output* format-headline "Put An End To A Device's Stage-Of-Life")
    (command-line-arguments:show-option-help *cli-device-stage-of-life-end-options*)
    (format *standard-output* format-headline "Camera Calibration Parameters")
    (command-line-arguments:show-option-help *cli-camera-calibration-options*)
    (format *standard-output* format-headline "Store Measure Data")
    (command-line-arguments:show-option-help *cli-store-images-and-points-options*)))

(defun cli-version-action (&rest rest)
  "Print --version message."
  (declare (ignore rest))
  (format *standard-output* "~&~A ~A~%"
          (asdf:system-description (asdf:find-system :phoros))
          (asdf:component-version (asdf:find-system :phoros))))

(defun check-db-action (&rest rest)
  "Say `OK´ if database is accessible."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (let (connection)
      (handler-case
          (setf
           connection
           (connect database user password host :port port
                    :use-ssl (s-sql:from-sql-name use-ssl))) ; string to keyword
        (error (e) (format *error-output* "~A~&" e)))
      (when connection
        (disconnect connection)
        (format *error-output* "~&OK~%")))))

(defun check-dependencies-action (&rest rest)
  "Say `OK´ if the necessary external dependencies are available."
  (declare (ignore rest))
  (handler-case
      (progn
        (geographic-to-utm 33 13 52) ;check cs2cs
        (del-all)                    ;check photogrammetry
        (initialize-leap-seconds)    ;check source of leap second info
        (format *error-output* "~&OK~%"))
    (error (e) (format *error-output* "~A~&" e))))

(defun nuke-all-tables-action (&rest rest)
  "Drop the bomb.  Ask for confirmation first."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (launch-logger log-dir)
    (when (yes-or-no-p "You asked me to delete anything in database ~A at ~A:~D.  Proceed?"
                       database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl)) ; string to keyword
        (nuke-all-tables))
      (cl-log:log-message :db "Nuked database ~A at ~A:~D.  Back to square one!" database host port))))

(defun create-sys-tables-action (&rest rest)
  "Make a set of sys-* tables.  Ask for confirmation first."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (launch-logger log-dir)
    (when (yes-or-no-p "You asked me to create a set of sys-* tables in database ~A at ~A:~D.  Make sure you know what you are doing.  Proceed?"
                       database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl)) ; string to keyword
        (create-sys-tables))
      (cl-log:log-message :db-sys "Created a fresh set of system tables in database ~A at ~A:~D." database host port))))

(defun create-acquisition-project-action (common-table-name)
  "Make a set of data tables."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (create-acquisition-project common-table-name))
    (cl-log:log-message :db-dat "Created a fresh acquisition project by the name of ~A in database ~A at ~A:~D." common-table-name database host port)))

(defun store-images-and-points-action (common-table-name)
  "Put data into the data tables."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            directory epsilon common-root aggregate-events
                            &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (cl-log:log-message :db-dat "Start: storing data from ~A into acquisition project ~A in database ~A at ~A:~D." directory common-table-name database host port)
      (store-images-and-points common-table-name directory
                               :epsilon (read-from-string epsilon nil)
                               :root-dir common-root
                               :aggregate-events aggregate-events))
    (cl-log:log-message :db-dat "Finish: storing data from ~A into acquisition project ~A in database ~A at ~A:~D." directory common-table-name database host port)))

;;; We don't seem to have two-dimensional arrays in postmodern
;;(defun canonicalize-bayer-pattern (raw &optional sql-string-p)
;;  "Convert list of strings of comma-separated hex color strings (ex: #ff0000 for red) into an array of integers.  If sql-string-p is t, convert it into a string in SQL syntax."
;;  (when raw
;;    (let* ((array
;;            (loop
;;               for row in raw
;;               collect
;;               (loop
;;                  for hex-color in (cl-utilities:split-sequence #\, row)
;;                  collect
;;                  (let ((*read-base* 16))
;;                    (assert (eql (elt hex-color 0) #\#) () "~A is not a valid color" hex-color)
;;                    (read-from-string
;;                     (concatenate 'string
;;                                  (subseq hex-color 5 7)
;;                                  (subseq hex-color 3 5)
;;                                  (subseq hex-color 1 3))
;;                     nil)))))
;;           (rows (length array))
;;           (columns (length (elt array 0))))
;;      (if sql-string-p
;;          (format nil "{~{{~{~A~#^,~}}~}}" array)
;;          (make-array (list rows columns) :initial-contents array)))))

(defun canonicalize-bayer-pattern (raw &optional sql-string-p)
  "Convert a string of comma-separated hex color strings (ex: #ff0000 for red) into a vector integers.  If sql-string-p is t, convert it into a string in SQL syntax."
  (when raw
    (let* ((vector
            (loop
               for hex-color in (cl-utilities:split-sequence #\, raw)
               collect
                 (let ((*read-base* 16))
                   (assert (eql (elt hex-color 0) #\#) () "~A is not a valid color" hex-color)
                   (read-from-string
                    (concatenate 'string
                                 (subseq hex-color 5 7)
                                 (subseq hex-color 3 5)
                                 (subseq hex-color 1 3))
                    nil))))
           (columns (length vector)))
      (if sql-string-p
          (format nil "{~{~A~#^,~}}" vector)
          (make-array (list columns) :initial-contents vector)))))

(defun canonicalize-color-raiser (raw &optional sql-string-p)
  "Convert string of comma-separated numbers into a vector.  If sql-string-p is t, convert it into a string in SQL syntax."
  (when raw
    (let* ((vector
            (loop
               for multiplier in (cl-utilities:split-sequence #\, raw :count 3)
               collect
               (read-from-string multiplier nil))))
      (if sql-string-p
          (format nil "{~{~A~#^,~}}" vector)
          (make-array '(3) :initial-contents vector)))))

(defun store-stuff (store-function)
  "Open database connection and call store-function on command line options.  Print return values to *standard-output*.  store-function should only take keyargs."
  (let ((command-line-options
         (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)))
    (setf (getf command-line-options :bayer-pattern)
          (canonicalize-bayer-pattern (getf command-line-options :raw-bayer-pattern) t)
          (getf command-line-options :color-raiser)
          (canonicalize-color-raiser (getf command-line-options :raw-color-raiser) t))
    (destructuring-bind (&key host port database (user "") (password "") use-ssl
                              log-dir &allow-other-keys)
        command-line-options
      (launch-logger log-dir)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (format *standard-output* "~&~{~D~#^ ~}~%"
                (multiple-value-list (apply store-function :allow-other-keys t command-line-options)))))))

(defun store-camera-hardware-action (&rest rest)
  (declare (ignore rest))
  (store-stuff #'store-camera-hardware))
          
(defun store-lens-action (&rest rest)
  (declare (ignore rest))
  (store-stuff #'store-lens))

(defun store-generic-device-action (&rest rest)
  (declare (ignore rest))
  (store-stuff #'store-generic-device))

(defun store-device-stage-of-life-action (&rest rest)
  (declare (ignore rest))
  (store-stuff #'store-device-stage-of-life))

(defun store-device-stage-of-life-end-action (&rest rest)
  (declare (ignore rest))
  (store-stuff #'store-device-stage-of-life-end))

(defun store-camera-calibration-action (&rest rest)
  (declare (ignore rest))
  (store-stuff #'store-camera-calibration))

(defun get-image-action (&rest rest)
  "Output a PNG file extracted from a .pictures file; print its trigger-time to stdout."
  (declare (ignore rest))
  (destructuring-bind (&key count byte-position in out raw-bayer-pattern raw-color-raiser &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (with-open-file (out-stream out :direction :output :element-type 'unsigned-byte
                                :if-exists :supersede)
      (let ((trigger-time
             (if byte-position
                 (send-png out-stream in byte-position :bayer-pattern (canonicalize-bayer-pattern raw-bayer-pattern) :color-raiser (canonicalize-color-raiser raw-color-raiser))
                 (send-nth-png count out-stream in :bayer-pattern (canonicalize-bayer-pattern raw-bayer-pattern) :color-raiser (canonicalize-color-raiser raw-color-raiser)))))
        (format *standard-output* "~&~A~%" (timestring (utc-from-unix trigger-time)))))))
