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


;;;; The UNIX command line interface

;; TODO: options that have a function as their :action seem to mask earlier options.  Fix or document.

(in-package :phoros)

(defparameter *cli-main-options*
  '((("help" #\h) :action #'cli-help-action
     :documentation "Print this help and exit.")
    (("licence" "license") :action #'cli-licence-action
     :documentation "Print licence boilerplate and exit.")
    ("version" :action #'cli-version-action
     :documentation "Print version information and exit.  Use --verbose=1 to see more.  In a version string A.B.C, changes in A denote incompatible changes in data; changes in B mean user-visible changes in feature set.")
    ("verbose" :type integer :initial-value 0 :action *verbose*
     :documentation "Dependent on bits set in this integer, emit various kinds of debugging output. ")
    ("log-dir" :type string :initial-value ""
     :documentation "Where to put the log files.  Created if necessary; should end with a slash.")
    ("check-db" :action #'check-db-action
     :documentation "Check database connection and exit.")
    ("check-dependencies" :action #'check-dependencies-action
     :documentation "Check presence of dependencies on local system and exit.")
    ("nuke-all-tables" :action #'nuke-all-tables-action
     :documentation "Ask for confirmation, then delete anything in database and exit.")
    ("create-sys-tables" :action #'create-sys-tables-action
     :documentation "Ask for confirmation, then create in database a set of sys-* tables (tables shared between all projects).  The database should probably be empty before you try this.")))

(defparameter *cli-db-connection-options*
  '((("host" #\H) :type string :initial-value "localhost"
     :documentation "Database server.")
    (("port" #\P) :type integer :initial-value 5432
     :documentation "Port on database server.")
    (("database" #\D) :type string :initial-value "phoros"
     :documentation "Name of database.")
    (("user" #\U) :type string
     :documentation "Database user.")
    (("password" #\W) :type string
     :documentation "Database user's password.")
    ("use-ssl" :type string :initial-value "no"
     :documentation "Use SSL in database connection. [yes|no|try]")))

(defparameter *cli-get-image-options*
  '(("get-image" :action #'get-image-action
     :documentation "Get a single image from a .pictures file, print its trigger-time to stdout, and exit.")
    ("count" :type integer :initial-value 0
     :documentation "Image number in .pictures file.")
    ("byte-position" :type integer
     :documentation "Byte position of image in .pictures file.")
    ("in" :type string
     :documentation "Path to .pictures file.")
    ("out" :type string :initial-value "phoros-get-image.png"
     :documentation "Path to to output .png file.")
    ;; The way it should be had we two-dimensional arrays in postmodern:
    ;;("bayer-pattern" :type string :list t :optional t :action :raw-bayer-pattern :documentation "The first pixels of the first row.  Repeat this option to describe following row(s).  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
    ("bayer-pattern" :type string :initial-value "#ff0000,#00ff00" :action :raw-bayer-pattern
     :documentation "The first pixels of the first row.  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")))

(defparameter *cli-camera-hardware-options*
  '(("store-camera-hardware" :action #'store-camera-hardware-action
     :documentation "Put new camera-hardware data into the database; print camera-hardware-id to stdout.")
    ("sensor-width-pix" :type integer
     :documentation "Width of camera sensor.")
    ("sensor-height-pix" :type integer
     :documentation "Height of camera sensor.")
    ("pix-size" :type string
     :documentation "Camera pixel size in millimetres (float).")
    ("channels" :type integer
     :documentation "Number of color channels")
    ("pix-depth" :type integer :initial-value 255
     :documentation "Greatest possible pixel value.")
    ("color-raiser" :type string :initial-value "1,1,1"
     :action :raw-color-raiser
     :documentation "Multipliers for the individual color components.  Example: 1.2,1,.8 multiplies red by 1.2 and blue by 0.8.")
    ;; The way it should be had we two-dimensional arrays in postmodern:
    ;;("bayer-pattern" :type string :list t :optional t :action :raw-bayer-pattern :documentation "The first pixels of the first row.  Repeat this option to describe following row(s).  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
    ("bayer-pattern" :type string :optional t
     :action :raw-bayer-pattern
     :documentation "The first pixels of the first row.  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
    ("serial-number" :type string
     :documentation "Serial number.")
    ("description" :type string
     :documentation "Description of camera.")
    ("try-overwrite" :type boolean :initial-value "yes"
     :documentation "Overwrite matching camera-hardware record if any.")))

(defparameter *cli-lens-options*
  '(("store-lens" :action #'store-lens-action
     :documentation "Put new lens data into the database; print lens-id to stdout.")
    ("c" :type string
     :documentation "Nominal focal length in millimetres.")
    ("serial-number" :type string
     :documentation "Serial number.")
    ("description" :type string
     :documentation "Lens desription.")
    ("try-overwrite" :type boolean :initial-value "yes"
     :documentation "Overwrite matching lens record if any.")))

(defparameter *cli-generic-device-options*
  '(("store-generic-device" :action #'store-generic-device-action
     :documentation "Put a newly defined generic-device into the database; print generic-device-id to stdout.")
    ("camera-hardware-id" :type integer
     :documentation "Numeric camera hardware id in database.")
    ("lens-id" :type integer
     :documentation "Numeric lens id in database.")))

(defparameter *cli-device-stage-of-life-options*
  '(("store-device-stage-of-life" :action #'store-device-stage-of-life-action
     :documentation "Put a newly defined device-stage-of-life into the database; print device-stage-of-life-id to stdout.")
    ("recorded-device-id" :type string
     :documentation "Device id stored next to the measuring data.")
    ("event-number" :type string
     :documentation "GPS event that triggers this generic device.")
    ("generic-device-id" :type integer
     :documentation "Numeric generic-device id in database.")
    ("vehicle-name" :type string
     :documentation "Descriptive name of vehicle.")
    ("casing-name" :type string
     :documentation "Descriptive name of device casing.")
    ("computer-name" :type string
     :documentation "Name of the recording device.")
    ("computer-interface-name" :type string
     :documentation "Interface at device.")
    ("mounting-date" :type string
     :documentation "Time this device constellation became effective.  Format: \"2010-11-19T13:49+01\".")))

(defparameter *cli-device-stage-of-life-end-options*
  '(("store-device-stage-of-life-end" :action #'store-device-stage-of-life-end-action
     :documentation "Put an end date to a device-stage-of-life in the database; print device-stage-of-life-id to stdout.")
    ("device-stage-of-life-id" :type string
     :documentation "Id of the device-stage-of-life to put to an end.")
    ("unmounting-date" :type string
     :documentation "Time this device constellation ceased to be effective.  Format: \"2010-11-19T17:02+01\".")))

(defparameter *cli-camera-calibration-options*
  '(("store-camera-calibration" :action #'store-camera-calibration-action
     :documentation "Put new camera-calibration into the database; print generic-device-id and calibration date to stdout.")
    ("device-stage-of-life-id" :type string
     :documentation "This tells us what hardware this calibration is for.")
    ("date" :type string
     :documentation "Date of calibration.  Format: \"2010-11-19T13:49+01\".")
    ("person" :type string
     :documentation "Person who did the calibration.")
    ("main-description" :type string
     :documentation "Regarding this entire set of calibration data")
    ("debug" :type string
     :documentation "If true: not for production use; may be altered or deleted at any time.")
    ("photogrammetry-version" :type string
     :documentation "Software version used to create this data.")
    ("mounting-angle" :type integer
     :documentation "Head up = 0; right ear up = 90; left ear up = -90; head down = 180.")
    ("inner-orientation-description" :type string
     :documentation "Comments regarding inner orientation calibration.")
    ("c" :type string :documentation "Inner orientation: focal length.")
    ("xh" :type string
     :documentation "Inner orientation: principal point displacement.")
    ("yh" :type string
     :documentation "Inner orientation: principal point displacement.")
    ("a1" :type string :documentation "Inner orientation: radial distortion.")
    ("a2" :type string :documentation "Inner orientation: radial distortion.")
    ("a3" :type string :documentation "Inner orientation: radial distortion.")
    ("b1" :type string
     :documentation "Inner orientation: asymmetric and tangential distortion.")
    ("b2" :type string
     :documentation "Inner orientation: asymmetric and tangential distortion.")
    ("c1" :type string
     :documentation "Inner orientation: affinity and shear distortion.")
    ("c2" :type string
     :documentation "Inner orientation: affinity and shear distortion.")
    ("r0" :type string :documentation "Inner orientation.")
    ("outer-orientation-description" :type string
     :documentation "Comments regarding outer orientation calibration.")
    ("dx" :type string :documentation "Outer orientation; in metres.")
    ("dy" :type string :documentation "Outer orientation; in metres.")
    ("dz" :type string :documentation "Outer orientation; in metres.")
    ("omega" :type string :documentation "Outer orientation.")
    ("phi" :type string :documentation "Outer orientation.")
    ("kappa" :type string :documentation "Outer orientation.")
    ("boresight-description" :type string
     :documentation "Comments regarding boresight alignment calibration.")
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
    ("nx" :type string
     :documentation "X component of unit vector of vehicle ground plane.")
    ("ny" :type string
     :documentation "Y component of unit vector of vehicle ground plane.")
    ("nz" :type string
     :documentation "Z component of unit vector of vehicle ground plane.")
    ("d" :type string :documentation "Distance of vehicle ground plane.")))

(defparameter *cli-acquisition-project-options*
  '(("create-acquisition-project"
     :type string :action #'create-acquisition-project-action
     :documentation "Create a fresh set of canonically named data tables.  The string argument is the acquisition project name.  It will be stored in table sys-acquisition-project, field common-table-name, and used as a common part of the data table names.")
    ("delete-acquisition-project"
     :type string :action #'delete-acquisition-project-action
     :documentation "Ask for confirmation, then delete acquisition project and all its measurements.")
    ("delete-measurement"
     :type integer :action #'delete-measurement-action
     :documentation "Delete a measurement by its ID.")
    ("list-acquisition-project"
     :type string :optional t :action #'list-acquisition-project-action
     :documentation "List measurements of one acquisition project if its name is specified, or of all acquisition projects otherwise.")))

(defparameter *cli-store-images-and-points-options*
  '((("store-images-and-points" #\s) :type string :action #'store-images-and-points-action
     :documentation "Link images to GPS points; store both into their respective DB tables.  Images become linked to GPS points when their respective times differ by less than epsilon seconds, and when the respective events match.  The string argument is the acquisition project name.")
    (("directory" #\d) :type string
     :documentation "Directory containing one set of measuring data.")
    (("common-root" #\r) :type string
     :documentation "The root part of directory that is equal for all pojects.  TODO: come up with some sensible default.")
    ("epsilon" :type string :initial-value ".001"
     :documentation "Difference in seconds below which two timestamps are considered equal.")
    ("aggregate-events" :type nil
     :documentation "Put all GPS points in one bucket, disregarding any event numbers.  Use this if you have morons setting up your generic-device.  Hundreds of orphaned images may indicate this is the case.")))

(defparameter *cli-start-server-options*
  '(("server" :action #'server-action
     :documentation "Start HTTP presentation server.  Entry URI is http://<host>:<port>/phoros/<presentation-project>")
    ("address" :type string
     :documentation "Address (of local machine) server is to listen to.  Default is listening to all available addresses.")
    ("server-port" :type integer :initial-value 8080
     :documentation "Port the presentation server listens on.")
    (("common-root" #\r) :type string :initial-value "/"
     :documentation "The root part of directory that is equal for all pojects.  TODO: come up with some sensible default.")
    ("images" :type integer :initial-value 4 :action *number-of-images*
     :documentation "Number of photos shown to the HTTP client.")))

(defparameter *cli-presentation-project-options*
  '(("create-presentation-project"
     :type string :action #'create-presentation-project-action
     :documentation "Create a fresh presentation project which is to expose a set of measurements to certain users.")
    ("delete-presentation-project"
     :type string :action #'delete-presentation-project-action
     :documentation "Ask for confirmation, then delete the presentation project including its table of user-generated points.")
    ("list-presentation-project"
     :type string :optional t :action #'list-presentation-project-action
     :documentation "List one presentation project if specified, or all presentation projects if not.")
    ("add-to-presentation-project"
     :type string :action #'add-to-presentation-project-action
     :documentation "Add to the presentation project given either certain measurements or all measurements currently in a certain acquisition project.")
    ("remove-from-presentation-project"
     :type string :action #'remove-from-presentation-project-action
     :documentation "Remove from the presentation project given either certain measurements or all measurements currently in a certain acquisition project.")
    ("measurement-id" :type integer :list t :optional t
     :documentation "One measurement-id to add or remove.  Repeat if necessary.")
    ("acquisition-project" :type string
     :documentation "The acquisition project whose measurements are to add or remove.")))

(defparameter *cli-user-options*
  '(("create-user"
     :type string :action #'create-user-action
     :documentation "Create or update user (specified by their ID) of certain presentation projects.")
    ("user-password" :type string :documentation "User's password.")
    ("user-full-name" :type string :documentation "User's real name.")
    ("user-role"
     :type string :initial-value "read"
     :documentation "User's permission on their projects.  One of \"read\", \"write\", or \"admin\" where \"write\" is the same as \"read\" plus permission to add user points and delete them if written by same user; and \"admin\" is the same as \"write\" plus permission to delete points written by other users.")
    ("presentation-project" :type string :list t :optional t
     :documentation "Presentation project the user is allowed to see.  Repeat if necessary.")
    ("delete-user"
     :type string :action #'delete-user-action
     :documentation "Delete user.")
    ("list-user"
     :type string :optional t :action #'list-user-action
     :documentation "List the specified user with their presentation projects, or all users if no user is given.")))

(defparameter *cli-options*
  (append *cli-main-options* *cli-db-connection-options*
          *cli-get-image-options*
          *cli-camera-hardware-options* *cli-lens-options*
          *cli-generic-device-options* *cli-device-stage-of-life-options*
          *cli-device-stage-of-life-end-options*
          *cli-camera-calibration-options*
          *cli-acquisition-project-options*
          *cli-store-images-and-points-options*
          *cli-start-server-options*
          *cli-presentation-project-options* *cli-user-options*))

(defun main ()
  "The UNIX command line entry point."
;;  (handler-bind ((serious-condition (lambda (c)
;;                                      (declare (ignore c))
;;                                      (sb-debug:backtrace))))
  #+sbcl (sb-ext:disable-debugger)
  (handler-case
      (progn
        (cffi:use-foreign-library phoml)
        (compute-and-process-command-line-options *cli-options*))
    (serious-condition (c)
      (cl-log:log-message :warning "Fatal: ~A" c)
      (format *error-output* "~A~&" c))))

(defun ignore-warnings (c) (declare (ignore c)) (muffle-warning))

(defun cli-help-action (&rest rest)
  "Print --help message."
  (declare (ignore rest))
  (flet ((show-help-headline (content)
           (format *standard-output* "~2&#### ~105,,,'#@<~A ~>" content)))
    (format
     *standard-output*
     "~&Usage: phoros [options] ...~&~A"
     (handler-bind ((warning #'ignore-warnings))
       (asdf:system-long-description (asdf:find-system :phoros))))
    (show-help-headline "Main Options")
    (show-option-help *cli-main-options*)
    (show-help-headline "Database Connection (necessary for most operations)")
    (show-option-help *cli-db-connection-options*)
    (show-help-headline "Examine .pictures File")
    (show-option-help *cli-get-image-options*)
    (show-help-headline
     "Camera Hardware Parameters (not including information on lens or mounting)")
    (show-option-help *cli-camera-hardware-options*)
    (show-help-headline
     "Lens Parameters (for human consumption; not used for photogrammetric calculation)")
    (show-option-help *cli-lens-options*)
    (show-help-headline
     "Generic Device Definition (basically a particular camera with a particular lens)")
    (show-option-help *cli-generic-device-options*)
    (show-help-headline
     "Device Stage-Of-Life Definition (generic device in a particular mounting constellation)")
    (show-option-help *cli-device-stage-of-life-options*)
    (show-help-headline
     "Put An End To A Device's Stage-Of-Life (e.g. after accidental change of mounting constellation)")
    (show-option-help *cli-device-stage-of-life-end-options*)
    (show-help-headline "Camera Calibration Parameters")
    (show-option-help *cli-camera-calibration-options*)
    (show-help-headline "Manage Acquisition Projects")
    (show-option-help *cli-acquisition-project-options*)
    (show-help-headline "Store Measure Data")
    (show-option-help *cli-store-images-and-points-options*)
    (show-help-headline "Become A HTTP Presentation Server")
    (show-option-help *cli-start-server-options*)
    (show-help-headline
     "Manage Presentation Projects (comprising data visible via web interface)")
    (show-option-help *cli-presentation-project-options*)
    (show-help-headline "Manage Presentation Project Users")
    (show-option-help *cli-user-options*)))

(defun phoros-version (&key major minor revision)
  "Return version of this program, either one integer part as denoted by
the key argument, or the whole dotted string."
  (let* ((version-string
          (handler-bind ((warning #'ignore-warnings))
            (asdf:component-version (asdf:find-system :phoros))))
         (version-components
          (mapcar #'parse-integer
                  (cl-utilities:split-sequence #\. version-string))))
    (cond (major (first version-components))
          (minor (second version-components))
          (revision (third version-components))
          (t version-string))))

(defun cli-version-action (&rest rest)
  "Print --version message. TODO: OpenLayers, Proj4js version."
  (declare (ignore rest))
  (process-command-line-options *cli-options* *command-line-arguments*)
  (case *verbose*
    (0
     (format
      *standard-output*
      "~&~A~&" (phoros-version)))
    (otherwise
     (format
      *standard-output*
      "~&~A version ~A~&  ~A version ~A~&  Proj4 library: ~A~&  Photogrammetry version ~A~&"
      (handler-bind ((warning #'ignore-warnings))
        (asdf:system-description (asdf:find-system :phoros)))
      (handler-bind ((warning #'ignore-warnings))
        (asdf:component-version (asdf:find-system :phoros)))
      (lisp-implementation-type) (lisp-implementation-version)
      (proj:version)
      (phoml:get-version-number)))))

(defun cli-licence-action (&rest rest)
  "Print --licence boilerplate."
  (declare (ignore rest))
  (format
   *standard-output* "~&~A~&"
   (handler-bind ((warning #'ignore-warnings))
     (asdf:system-licence (asdf:find-system :phoros)))))

(defun check-db-action (&rest rest)
  "Say `OK´ if database is accessible."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (when (check-db (list database user password host :port port
                          :use-ssl (s-sql:from-sql-name use-ssl)))
      (format *error-output* "~&OK~%"))))

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
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (when (yes-or-no-p
           "You asked me to delete anything in database ~A at ~A:~D.  Proceed?"
           database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl)) ; string to keyword
        (nuke-all-tables))
      (cl-log:log-message
       :db "Nuked database ~A at ~A:~D.  Back to square one!"
       database host port))))

(defun create-sys-tables-action (&rest rest)
  "Make a set of sys-* tables.  Ask for confirmation first."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (when (yes-or-no-p
           "You asked me to create a set of sys-* tables in database ~A at ~A:~D.  Make sure you know what you are doing.  Proceed?"
           database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (create-sys-tables))
      (cl-log:log-message
       :db-sys "Created a fresh set of system tables in database ~A at ~A:~D."
       database host port))))

(defun create-acquisition-project-action (common-table-name)
  "Make a set of data tables."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (create-acquisition-project common-table-name))
    (cl-log:log-message
     :db-dat
     "Created a fresh acquisition project by the name of ~A in database ~A at ~A:~D."
     common-table-name database host port)))

(defun delete-acquisition-project-action (common-table-name)
  "Delete an acquisition project."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (when (yes-or-no-p
           "You asked me to delete acquisition-project ~A (including all its measurements) from database ~A at ~A:~D.  Proceed?"
           common-table-name database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (let ((project-did-exist-p
               (delete-acquisition-project common-table-name)))
          (cl-log:log-message
           :db-dat
           "~:[Tried to delete nonexistent~;Deleted~] acquisition project ~A from database ~A at ~A:~D."
           project-did-exist-p common-table-name database host port))))))

(defun delete-measurement-action (measurement-id)
  "Delete a measurement by its measurement-id."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (let ((measurement-did-exist-p
             (delete-measurement measurement-id)))
        (cl-log:log-message
         :db-dat
         "~:[Tried to delete nonexistent~;Deleted~] measurement with ID ~A from database ~A at ~A:~D."
         measurement-did-exist-p measurement-id database host port)))))

(defun list-acquisition-project-action (&optional common-table-name)
  "List content of acquisition projects."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (let ((content
             (if (stringp common-table-name)
                 (query
                  (:order-by
                   (:select
                    'common-table-name
                    'sys-acquisition-project.acquisition-project-id
                    'measurement-id
                    'directory
                    'cartesian-system
                    :from
                    'sys-acquisition-project :natural :left-join 'sys-measurement
                    :where (:= 'common-table-name common-table-name))
                   'measurement-id))
                 (query
                  (:order-by
                   (:select
                    'common-table-name
                    'sys-acquisition-project.acquisition-project-id
                    'measurement-id
                    'directory
                    'cartesian-system
                    :from
                    'sys-acquisition-project :natural :left-join 'sys-measurement)
                   'common-table-name 'measurement-id)))))
        (format-table *standard-output* " | " content
                      "Acquisition Project" "ID" "Meas. ID" "Directory" "Cartesian CS")))))

(defun store-images-and-points-action (common-table-name)
  "Put data into the data tables."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            directory epsilon common-root aggregate-events
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (cl-log:log-message
       :db-dat
       "Start: storing data from ~A into acquisition project ~A in database ~A at ~A:~D."
       directory common-table-name database host port)
      (store-images-and-points common-table-name directory
                               :epsilon (read-from-string epsilon nil)
                               :root-dir common-root
                               :aggregate-events aggregate-events))
    (cl-log:log-message
     :db-dat
     "Finish: storing data from ~A into acquisition project ~A in database ~A at ~A:~D."
     directory common-table-name database host port)))

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
  "Convert a string of comma-separated hex color strings (ex: #ff0000
for red) into a vector integers.  If sql-string-p is t, convert it
into a string in SQL syntax."
  (when raw
    (let* ((vector
            (loop
               for hex-color in (cl-utilities:split-sequence #\, raw)
               collect
                 (let ((*read-base* 16))
                   (assert (eql (elt hex-color 0) #\#)
                           () "~A is not a valid color" hex-color)
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
  "Convert string of comma-separated numbers into a vector.  If
sql-string-p is t, convert it into a string in SQL syntax."
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
  "Open database connection and call store-function on command line
options.  Print return values to *standard-output*.  store-function
should only take keyargs."
  (let ((command-line-options
         (process-command-line-options *cli-options* *command-line-arguments*)))
    (setf (getf command-line-options :bayer-pattern)
          (canonicalize-bayer-pattern
           (getf command-line-options :raw-bayer-pattern) t)
          (getf command-line-options :color-raiser)
          (canonicalize-color-raiser
           (getf command-line-options :raw-color-raiser) t))
    (destructuring-bind (&key host port database (user "") (password "") use-ssl
                              log-dir &allow-other-keys)
        command-line-options
      (launch-logger log-dir)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (format *standard-output* "~&~{~D~#^ ~}~%"
                (multiple-value-list
                 (apply store-function :allow-other-keys t
                        command-line-options)))))))

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
  "Output a PNG file extracted from a .pictures file; print its
trigger-time to stdout."
  (declare (ignore rest))
  (destructuring-bind (&key count byte-position in out
                            raw-bayer-pattern raw-color-raiser
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (with-open-file (out-stream out :direction :output
                                :element-type 'unsigned-byte
                                :if-exists :supersede)
      (let ((trigger-time
             (if byte-position
                 (send-png out-stream in byte-position
                           :bayer-pattern
                           (canonicalize-bayer-pattern raw-bayer-pattern)
                           :color-raiser
                           (canonicalize-color-raiser raw-color-raiser))
                 (send-nth-png count out-stream in
                               :bayer-pattern
                               (canonicalize-bayer-pattern raw-bayer-pattern)
                               :color-raiser
                               (canonicalize-color-raiser raw-color-raiser)))))
        (format *standard-output*
                "~&~A~%" (timestring (utc-from-unix trigger-time)))))))

(defun create-presentation-project-action (presentation-project-name)
  "Make a presentation project."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (let ((fresh-project-p
             (create-presentation-project presentation-project-name)))
        (cl-log:log-message
         :db-dat
         "~:[Tried to recreate an existing~;Created a fresh~] presentation project by the name of ~A in database ~A at ~A:~D."
         fresh-project-p presentation-project-name database host port)))))


(defun delete-presentation-project-action (presentation-project-name)
  "Delete a presentation project."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (when (yes-or-no-p
           "You asked me to delete presentation-project ~A (including its table of user-defined points usr-~:*~A-point) from database ~A at ~A:~D.  Proceed?"
           presentation-project-name database host port)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (let ((project-did-exist-p
               (delete-presentation-project presentation-project-name)))
          (cl-log:log-message
           :db-dat
           "~:[Tried to delete nonexistent~;Deleted~] presentation project ~A from database ~A at ~A:~D."
           project-did-exist-p presentation-project-name database host port))))))

(defun add-to-presentation-project-action (presentation-project-name)
  "Add measurements to a presentation project."
    (destructuring-bind (&key host port database (user "") (password "") use-ssl
                              log-dir
                              measurement-id acquisition-project
                              &allow-other-keys)
        (process-command-line-options *cli-options* *command-line-arguments*)
      (launch-logger log-dir)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (add-to-presentation-project presentation-project-name
                                     :measurement-ids measurement-id
                                     :acquisition-project acquisition-project))
      (cl-log:log-message
       :db-dat
       "Added ~@[measurement-ids ~{~D~#^, ~}~]~@[all measurements from acquisition project ~A~] to presentation project ~A in database ~A at ~A:~D."
       measurement-id acquisition-project
       presentation-project-name database host port)))

(defun remove-from-presentation-project-action (presentation-project-name)
  "Add measurements to a presentation project."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            measurement-id acquisition-project
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (remove-from-presentation-project presentation-project-name
                                   :measurement-ids measurement-id
                                   :acquisition-project acquisition-project))
    (cl-log:log-message
     :db-dat
     "Removed ~@[measurement-ids ~{~D~#^, ~}~]~@[all measurements that belong to acquisition project ~A~] from presentation project ~A in database ~A at ~A:~D."
     measurement-id acquisition-project
     presentation-project-name database host port)))

(defun create-user-action (presentation-project-user)
  "Define a new user."
  (let (fresh-user-p)
    (destructuring-bind (&key host port database (user "") (password "") use-ssl
                              log-dir
                              user-password user-full-name user-role presentation-project
                              &allow-other-keys)
        (process-command-line-options *cli-options* *command-line-arguments*)
      (launch-logger log-dir)
      (with-connection (list database user password host :port port
                             :use-ssl (s-sql:from-sql-name use-ssl))
        (setf fresh-user-p
              (create-user presentation-project-user
                           :password user-password
                           :full-name user-full-name
                           :user-role user-role
                           :presentation-projects presentation-project)))
      (cl-log:log-message
       :db-dat ;TODO: We're listing nonexistent p-projects here as well.
       "~:[Updated~;Created~] user ~A (~A) who has ~A access to ~:[no ~;~]presentation project(s)~:*~{ ~A~#^,~} in database ~A at ~A:~D."
       fresh-user-p presentation-project-user
       user-full-name user-role
       presentation-project database host port))))

(defun delete-user-action (presentation-project-user)
  "Delete a presentation project user."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (let ((user-did-exist-p
             (delete-user presentation-project-user)))
        (cl-log:log-message
         :db-dat
         "~:[Tried to delete nonexistent~;Deleted~] presentation project user ~A from database ~A at ~A:~D."
         user-did-exist-p presentation-project-user database host port)))))

(defun list-user-action (&optional presentation-project-user)
  "List presentation project users together with their presentation
projects."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (let ((content
             (if (stringp presentation-project-user)
                 (query
                  (:order-by
                   (:select
                    'user-name 'sys-user.user-id 'user-password
                    'user-full-name 'presentation-project-name
                    'sys-user-role.presentation-project-id 'user-role
                    :from 'sys-user 'sys-user-role 'sys-presentation-project
                    :where (:and (:= 'sys-user-role.presentation-project-id
                                     'sys-presentation-project.presentation-project-id)
                                 (:= 'sys-user.user-id 'sys-user-role.user-id)
                                 (:= 'user-name presentation-project-user)))
                   'user-name))
                 (query
                  (:order-by
                   (:select
                    'user-name 'sys-user.user-id 'user-password
                    'user-full-name 'presentation-project-name
                    'sys-user-role.presentation-project-id 'user-role
                    :from 'sys-user 'sys-user-role 'sys-presentation-project
                    :where (:and (:= 'sys-user-role.presentation-project-id
                                     'sys-presentation-project.presentation-project-id)
                                 (:= 'sys-user.user-id 'sys-user-role.user-id)))
                   'user-name)))))
        (format-table *standard-output* " | " content
                      "User" "ID" "Password" "Full Name" "Presentation Project" "ID" "Role")))))

(defun list-presentation-project-action (&optional presentation-project)
  "List content of presentation projects."
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (with-connection (list database user password host :port port
                           :use-ssl (s-sql:from-sql-name use-ssl))
      (let ((content
             (if (stringp presentation-project)
                 (query
                  (:order-by
                   (:select
                    'presentation-project-name
                    'sys-presentation-project.presentation-project-id
                    'sys-presentation.measurement-id
                    'common-table-name
                    'sys-measurement.acquisition-project-id
                    :from
                    'sys-presentation-project 'sys-presentation
                    'sys-measurement 'sys-acquisition-project
                    :where (:and (:= 'sys-presentation-project.presentation-project-id
                                     'sys-presentation.presentation-project-id)
                                 (:= 'sys-presentation.measurement-id
                                     'sys-measurement.measurement-id)
                                 (:= 'sys-measurement.acquisition-project-id
                                     'sys-acquisition-project.acquisition-project-id)
                                 (:= 'presentation-project-name presentation-project)))
                   'presentation-project-name 'sys-presentation.measurement-id))
                 (query
                  (:order-by
                   (:select
                    'presentation-project-name
                    'sys-presentation-project.presentation-project-id
                    'sys-presentation.measurement-id
                    'common-table-name
                    'sys-measurement.acquisition-project-id
                    :from
                    'sys-presentation-project 'sys-presentation
                    'sys-measurement 'sys-acquisition-project
                    :where (:and (:= 'sys-presentation-project.presentation-project-id
                                     'sys-presentation.presentation-project-id)
                                 (:= 'sys-presentation.measurement-id
                                     'sys-measurement.measurement-id)
                                 (:= 'sys-measurement.acquisition-project-id
                                     'sys-acquisition-project.acquisition-project-id)))
                   'presentation-project-name 'sys-presentation.measurement-id)))))
        (format-table *standard-output* " | " content
                      "Presentation Project" "ID" "Meas. ID" "Acquisition Project" "ID")))))
         
(defun format-table (destination column-separator content &rest column-headers)
  "Print content (a list of lists) to destination."
  (let* ((rows
          (append (list column-headers) (list ()) content))
         (number-of-rows (length column-headers))
         (widths
          (loop
             for column from 0 below number-of-rows collect
               (loop
                  for row in rows
                  maximize (length (format nil "~A" (nth column row)))))))
    (setf (second rows)
          (loop
             for width in widths collect
               (make-string width :initial-element #\-)))
    (loop 
       for row in rows do
         (format destination "~&~{~VA~1,#^~A~}~%"
                 (loop
                    for width in widths and field in row
                    collect width collect field collect column-separator)))))

(defun server-action (&rest rest)
  "Start the HTTP server."
  (declare (ignore rest))
  (destructuring-bind (&key host port database (user "") (password "") use-ssl
                            log-dir
                            server-port address common-root
                            &allow-other-keys)
      (process-command-line-options *cli-options* *command-line-arguments*)
    (launch-logger log-dir)
    (setf *postgresql-credentials*
          (list database user password host :port port
                :use-ssl (s-sql:from-sql-name use-ssl)))
    (start-server :server-port server-port :address address
                  :common-root common-root)
    (cl-log:log-message
     :server
     "HTTP server listens on port ~D of ~:[all available addresses~;address ~:*~A~].  Database is ~A on ~A:~D.  Files are searched for in ~A."
     server-port address database host port common-root)
    (loop (sleep 10))))
