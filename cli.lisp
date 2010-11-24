;;;; UNIX command line interface

(in-package :phoros)

(defparameter *cli-main-options*
  '((("help" #\h) :action #'cli-help-action  :documentation "Print this help and exit.")
    ("version" :action #'cli-version-action :documentation "Output version information and exit.")
    ("verbose" :type integer :initial-value 0 :documentation "Emit increasing amounts of debugging output.")
    ("check-db" :action #'check-db-action :documentation "Check database connection and exit.")
    ("get-image" :action #'get-image-action :documentation "Get a single image from a .pictures file and exit.")
    ("nuke-all-tables" :action #'nuke-all-tables-action :documentation "Ask for confirmation, then delete anything in database and exit.")
    ("create-sys-tables" :action #'create-sys-tables-action :documentation "Ask for confirmation, then create in database a set of sys-* tables (tables shared between all projects).  The database should probably be empty before you try this.")
    ("store-camera-hardware" :action #'store-camera-hardware-action :documentation "Put new camera-hardware data into the database; print camera-hardware-id to stdout.")
    ("store-lens" :action #'store-lens-action :documentation "Put new lens data into the database; print lens-id to stdout.")
    ("store-generic-device" :action #'store-generic-device-action :documentation "Put a newly defined generic-device into the database; print generic-device-id to stdout.")
    ("store-device-stage-of-life" :action #'store-device-stage-of-life-action :documentation "Put a newly defined device-stage-of-life into the database; print device-stage-of-life-id to stdout.")
    ("store-device-stage-of-life-end" :action #'store-device-stage-of-life-end-action :documentation "Put an end date to a device-stage-of-life in the database; print device-stage-of-life-id to stdout.")
    ("store-camera-calibration" :action #'store-camera-calibration-action :documentation "Put new camera-calibration into the database; print generic-device-id and calibration date to stdout.")))

(defparameter *cli-db-connection-options*
  '((("host" #\H) :type string :initial-value "localhost" :documentation "Database server.")
    (("port" #\P) :type integer :initial-value 5432 :documentation "Port on database server.")
    (("database" #\D) :type string :initial-value "phoros" :documentation "Name of database.")
    (("user" #\U) :type string :documentation "Database user.")
    (("password" #\W) :type string :documentation "Database user's password.")
    ("use-ssl" :type string :initial-value "no" :documentation "Use SSL in database connection. [yes|no|try]")))

(defparameter *cli-get-image-options*
  '(("count" :type integer :initial-value 0 :documentation "Image number in .pictures file.")
    ("byte-position" :type integer :documentation "Byte position of image in .pictures file.")
    ("in" :type string :documentation "Path to .pictures file.")
    ("out" :type string :initial-value "phoros-get-image.png" :documentation "Path to to output .png file.")))

(defparameter *cli-camera-hardware-options*
  '(("sensor-width-pix" :type integer :documentation "Width of camera sensor.")
    ("sensor-height-pix" :type integer :documentation "Height of camera sensor.")
    ("pix-size" :type string :documentation "Camera pixel size in millimetres (float).")
    ("channels" :type integer :documentation "Number of color channels")
    ("pix-depth" :type integer :initial-value 255 :documentation "Greatest possible pixel value.")
    ("color-raiser" :type string :documentation "TODO: Yet to be defined.  Try '{r,g,b}'")
    ("bayer-pattern" :type string :documentation "TODO: Yet to be defined.")
    ("serial-number" :type string :documentation "Serial number.")
    ("description" :type string :documentation "Description of camera.")
    ("try-overwrite" :type boolean :initial-value "yes" :documentation "Overwrite matching camera-hardware record if any.")))

(defparameter *cli-lens-options*
  '(("c" :type string :documentation "Nominal focal length in millimetres.")
    ("serial-number" :type string :documentation "Serial number.")
    ("description" :type string :documentation "Lens desription.")
    ("try-overwrite" :type boolean :initial-value "yes" :documentation "Overwrite matching lens record if any.")))

(defparameter *cli-generic-device-options*
  '(("camera-hardware-id" :type integer :documentation "Numeric camera hardware id in database.")
    ("lens-id" :type integer :documentation "Numeric lens id in database.")))

(defparameter *cli-device-stage-of-life-options*
  '(("recorded-device-id" :type string :documentation "Device id stored next to the measuring data.")
    ("event-number" :type string :documentation "GPS event that triggers this generic device.")
    ("generic-device-id" :type integer :documentation "Numeric generic-device id in database.")
    ("vehicle-name" :type string :documentation "Descriptive name of vehicle.")
    ("casing-name" :type string :documentation "Descriptive name of device casing.")
    ("computer-name" :type string :documentation "Name of the recording device.")
    ("computer-interface-name" :type string :documentation "Interface at device.")
    ("mounting-date" :type string :documentation "Time this device constellation became effective.  Format: `2010-11-19T13:49+01´.")))

(defparameter *cli-device-stage-of-life-end-options*
  '(("device-stage-of-life-id" :type string :documentation "Id of the device-stage-of-life to put to an end.")
    ("unmounting-date" :type string :documentation "Time this device constellation ceased to be effective.  Format: `2010-11-19T17:02+01´.")))

(defparameter *cli-camera-calibration-options*
  '(("device-stage-of-life-id" :type string :documentation "This tells us what hardware this calibration is for.")
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
    ("b-drotz" :type string :documentation "Boresight alignment.")))    

(defparameter *cli-options* (append *cli-main-options* *cli-db-connection-options* *cli-get-image-options* *cli-camera-hardware-options* *cli-lens-options* *cli-generic-device-options* *cli-device-stage-of-life-options* *cli-device-stage-of-life-end-options* *cli-camera-calibration-options*))

(defun main ()
  "The UNIX command line entry point."
  (handler-case
      (command-line-arguments:compute-and-process-command-line-options *cli-options*)
    (error (e) (format *error-output* "~A~&" e))))

(defun cli-help-action (&rest rest)
  "Print --help message."
  (declare (ignore rest))
  (format *standard-output*
          "~&Usage: phoros command [options] ...~&~A~&### Commands:"
          (asdf:system-long-description (asdf:find-system :phoros)))
  (command-line-arguments:show-option-help *cli-main-options*)
  (format *standard-output* "~&### Database connection:")
  (command-line-arguments:show-option-help *cli-db-connection-options*)
  (format *standard-output* "~&### Examine .pictures file:")
  (command-line-arguments:show-option-help *cli-get-image-options*)
  (format *standard-output* "~&### Camera hardware parameters:")
  (command-line-arguments:show-option-help *cli-camera-hardware-options*)
  (format *standard-output* "~&### Lens parameters:")
  (command-line-arguments:show-option-help *cli-lens-options*)
  (format *standard-output* "~&### Generic device definition:")
  (command-line-arguments:show-option-help *cli-generic-device-options*)
  (format *standard-output* "~&### Device stage-of-life definition:")
  (command-line-arguments:show-option-help *cli-device-stage-of-life-options*)
  (format *standard-output* "~&### Put an end to a device's stage-of-life:")
  (command-line-arguments:show-option-help *cli-device-stage-of-life-end-options*)
  (format *standard-output* "~&### Camera calibration parameters:")
  (command-line-arguments:show-option-help *cli-camera-calibration-options*))

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

(defun nuke-all-tables-action (&rest rest)
  "Drop the bomb.  Ask for confirmation first."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (when (yes-or-no-p "You asked me to delete anything in database ~A at ~A:~D.  Proceed?"
                       database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl)) ; string to keyword
        (nuke-all-tables)))))

(defun create-sys-tables-action (&rest rest)
  "Make a set of sys-* tables.  Ask for confirmation first."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (when (yes-or-no-p "You asked me to create a set of sys-* tables in database ~A at ~A:~D.  Make sure you know what you are doing.  Proceed?"
                       database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl)) ; string to keyword
        (create-sys-tables)))))

(defun store-stuff (store-function)
  "Open database connection and call store-function on command line options.  Print return values to *standard-output*.  store-function should only take keyargs."
  (let ((command-line-options
         (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)))
    (destructuring-bind (&key host port database (user "") (password "") use-ssl &allow-other-keys)
        command-line-options
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
  "Output a PNG file extracted from a .pictures file."
  (declare (ignore rest))
  (destructuring-bind (&key count byte-position in out &allow-other-keys)
      (command-line-arguments:process-command-line-options *cli-options* command-line-arguments:*command-line-arguments*)
    (with-open-file (out-stream out :direction :output :element-type 'unsigned-byte
                                :if-exists :supersede)
      (if byte-position
          (send-png out-stream in byte-position)
          (send-nth-png count out-stream in)))))
