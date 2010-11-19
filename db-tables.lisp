(in-package :phoros)

(defun nuke-all-tables ()
  "Drop all tables (except PostGIS helper tables) and sequences in current database."
  (let ((user-tables (list-tables))
        (sequences (list-sequences))
        (system-tables '(:spatial-ref-sys :geometry-columns)))
    (dolist (system-table system-tables)
      (setf user-tables (remove system-table user-tables))) ; TODO: do we need to spare those?
    (dolist (user-table user-tables)
      (execute (format nil "DROP TABLE IF EXISTS ~A CASCADE" (s-sql:to-sql-name user-table))))
    (dolist (sequence sequences)
      (execute (format nil "DROP SEQUENCE IF EXISTS ~A CASCADE" (s-sql:to-sql-name sequence))))))
 
;;TODO: make a spatial-ref-sys table
;;
;;CREATE TABLE spatial_ref_sys ( 
;;  srid       INTEGER NOT NULL PRIMARY KEY, 
;;  auth_name  VARCHAR(256), 
;;  auth_srid  INTEGER, 
;;  srtext     VARCHAR(2048), 
;;  proj4text  VARCHAR(2048) 
;;)

;;TODO: make a geometry-columns table
;;
;;CREATE TABLE geometry_columns ( 
;;  f_table_catalog    VARRCHAR(256) NOT NULL, 
;;  f_table_schema     VARCHAR(256) NOT NULL,
;;  f_table_nam        VARCHAR(256) NOT NULL, 
;;  f_geometry_column  VARCHAR(256) NOT NULL, 
;;  coord_dimension    INTEGER NOT NULL, 
;;  srid               INTEGER NOT NULL, 
;;  type               VARCHAR(30) NOT NULL 
;;)

(defclass sys-user ()
  ((user-id
    :col-type integer
    :col-default (:nextval 'sys-user-id-seq))
   (user-name
    :col-type text
    :documentation "This one is used for authentication.")
   (user-password
    :col-type text)
   (user-full-name
    :col-type text))
  (:metaclass dao-class)
  (:keys user-id)
  (:documentation "List of users of the presentation front end.  This is certainly not a full-fledged authentication system."))

(deftable sys-user
  (:create-sequence 'sys-user-id-seq)
  (!dao-def))

(defclass sys-acquisition-project ()
  ((acquisition-project-id
    :reader acquisition-project-id
    :col-type integer
    :col-default (:nextval 'sys-acquisition-project-id-seq))
   (common-table-name
    :col-type text
    :initarg :common-table-name
    :documentation "Name of this project's data tables sans their canonical prefixes and suffixes.  Serves as a human-readable acquisition procect identifier.  Should be one table for all projects but this seems to come with a speed penalty."))
  (:metaclass dao-class)
  (:keys acquisition-project-id)
  (:documentation "An acquisition project is basically a set of measurements that is stored in a common table."))

(deftable sys-acquisition-project
  (:create-sequence 'sys-acquisition-project-id-seq)
  (!dao-def)
  (sql-compile `(:alter-table ,*table-name* :add :constraint "common-table-name-unique" :unique 'common-table-name)))

(defclass sys-presentation-project ()
  ((presentation-project-id
    :col-type integer
    :col-default (:nextval 'sys-presentation-project-id-seq))
   (presentation-project-name
    :col-type text
    :initarg :project-name))
  (:metaclass dao-class)
  (:keys presentation-project-id))

(deftable sys-presentation-project
  (:create-sequence 'sys-presentation-project-id-seq)
  (!dao-def))

(defclass sys-user-role ()
  ((user-id
    :col-type integer)
   (presentation-project-id
    :col-type integer)
   (user-role
    :col-type text
    :documentation "Some well-defined string, e.g. read-only, r/w, etc.  TODO: define some."))
  (:metaclass dao-class)
  (:keys user-id presentation-project-id))

(deftable sys-user-role
  (!dao-def)
  (!foreign 'sys-user 'user-id :on-delete :cascade :on-update :cascade)
  (!foreign 'sys-presentation-project 'presentation-project-id :on-delete :cascade :on-update :cascade))

(defclass sys-measurement ()
  ((measurement-id
    :reader measurement-id
    :col-type integer
    :col-default (:nextval 'sys-measurement-id-seq))
   (acquisition-project-id
    :initarg :acquisition-project-id
    :col-type integer)
   (directory
    :initarg :directory
    :col-type text
    :documentation "Below some universal root common to all measurements; excluding `applanix/´ `images/´ etc.

The entire directory structure looks like this:

Points
======
/some/path/in/our/system/this/measurement/blah/applanix/points/xyz-event1.txt
/some/path/in/our/system/this/measurement/blah/applanix/points/uvw-event2.txt
                                          ---- +++++++++++++++ ----+++++ ++++
^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^                               ^
    universal root       stored here in                                event
                         slot directory                                number

Images
======
/some/path/in/our/system/this/measurement/blah/images/front77.pictures
/some/path/in/our/system/this/measurement/blah/images/front78.pictures
                                          ---- ++++++         ++++++++
^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^             ^^^^^^^^^^^^^^^^
    universal root       stored here in               stored file name 
                         slot directory

++++ means constant
---- means unimportant"))
  (:metaclass dao-class)
  (:keys measurement-id)
  (:documentation "A measurement comprises .pictures files and one set of GPS event log files in a dedicated directory."))

(deftable sys-measurement
  (:create-sequence 'sys-measurement-id-seq)
  (!dao-def)
  (!foreign 'sys-acquisition-project 'acquisition-project-id :on-delete :cascade :on-update :cascade))

(defclass sys-presentation ()
  ((presentation-project-id
    :col-type integer)
   (measurement-id
    :col-type integer))
  (:metaclass dao-class)
  (:keys presentation-project-id measurement-id)
  (:documentation "Tell us which measurements belong to which presentation project(s)."))

(deftable sys-presentation
  (!dao-def)
  (!foreign 'sys-presentation-project 'presentation-project-id :on-delete :cascade :on-update :cascade)
  (!foreign 'sys-measurement 'measurement-id :on-delete :cascade :on-update :cascade))

(defclass sys-camera-hardware ()
  ((camera-hardware-id
    :reader camera-hardware-id
    :col-type integer
    :col-default (:nextval 'sys-camera-hardware-id-seq))
   (sensor-width-pix
    :col-type integer)
   (sensor-height-pix
    :col-type integer)
   (pix-size
    :col-type double-float)
   (channels
    :col-type integer)
   (pix-depth
    :col-type integer)
   (color-raiser
    :col-type integer[]
    :documentation "Array of multipliers for red, green, blue.")
   (pix-colors
    :col-type integer[]
    :documentation "Array containing the colors the first pixels of the first two (or three) rows.  Each pixel is to be interpreted as a three-byte RGB value.")
   (serial-number
    :col-type text)
   (description
    :col-type text
    :description "Camera type, manufacturer, etc."))
  (:metaclass dao-class)
  (:keys camera-hardware-id))

(deftable sys-camera-hardware
  (:create-sequence 'sys-camera-hardware-id-seq)
  (!dao-def))

(defclass sys-lens ()
  ((lens-id
    :reader lens-id
    :col-type integer
    :col-default (:nextval 'sys-lens-id-seq))
   (c
    :col-type double-float
    :documentation "Focal length.  Only for human consumption.")
   (serial-number
    :col-type text)
   (description
    :col-type text
    :documentation "Lens type, manufacturer, etc."))
  (:metaclass dao-class)
  (:keys lens-id))

(deftable sys-lens
  (:create-sequence 'sys-lens-id-seq)
  (!dao-def))

(defclass sys-generic-device ()
  ((generic-device-id
    :reader generic-device-id
    :col-type integer
    :col-default (:nextval 'sys-generic-device-id-seq))
   (camera-hardware-id
    :initarg :camera-hardware-id
    :col-type (or db-null integer))
   (lens-id
    :initarg :lens-id
    :col-type (or db-null integer))
   (scanner-id
    :initarg :scanner-id
    :col-type (or db-null integer)
    :documentation "Scanners yet to be defined."))
  (:metaclass dao-class)
  (:keys generic-device-id)
  (:documentation "A row should describe either a camera with a lens, or a laser scanner."))

(deftable sys-generic-device
  (:create-sequence 'sys-generic-device-id-seq)
  (!dao-def)
  (!foreign 'sys-camera-hardware 'camera-hardware-id :on-delete :restrict :on-update :restrict)
  (!foreign 'sys-lens 'lens-id :on-delete :restrict :on-update :restrict)
  ;;;; Once we have a sys-scanner table:
  ;;(!foreign 'sys-scanner 'scanner-id :on-delete :restrict :on-update :restrict)
  )

(defclass sys-device-stage-of-life ()
  ((device-stage-of-life-id
    :reader device-stage-of-life-id
    :col-type integer
    :col-default (:nextval 'sys-device-stage-of-life-seq))
   (recorded-device-id
    :col-type text
    :documentation "Must be stored next to each data record.  Example: in a .pictures file, this is the value of `cam=´.")
   (event-number
    :col-type text
    :documentation "Identifier for the GPS event that triggers this device.  Must correspond to the N the GPS file name: ...eventN.txt.")
   (generic-device-id
    :col-type integer)
   (vehicle-name
    :col-type text)
   (casing-name
    :col-type text
    :documentation "Something like `upper rear left´ or maybe `1.2.1´")
   (computer-name
    :col-type text
    :documentation "Computer (or or other recording device) this device is connected to.")
   (computer-interface-name
    :col-type text
    :documentation "Things like `eth0´, `COM1´ etc.")
   (mounting-date
    :col-type :timestamp-with-time-zone)
   (unmounting-date
    :col-type (or db-null :timestamp-with-time-zone)
    :documentation "Date and time when this device was unmounted or altered in other ways that may render the current calibration invalid."))
  (:metaclass dao-class)
  (:keys device-stage-of-life-id)
  (:documentation "This data is to be collected on the measuring vehicle.  There must be a new record for every relevant change (including accidental ones) in device configuration."))

(deftable sys-device-stage-of-life
  (:create-sequence 'sys-device-stage-of-life-seq)
  (!dao-def)
  (!index 'recorded-device-id)
  (!index 'mounting-date)
  (!index 'unmounting-date)
  (!foreign 'sys-generic-device 'generic-device-id :on-delete :restrict :on-update :restrict))

(defclass sys-camera-calibration ()
  ((device-stage-of-life-id
    :col-type integer
    :documentation "This tells us what hardware this calibration is for.")
   (date
    :reader date
    :col-type timestamp)
   (person
    :col-type text)
   (main-description
    :col-type text
    :documentation "Regarding this entire set of calibration data.  Note the special-purpose description fields inner-orientation-description, outer-orientation-description, boresight-description.")
   (debug
    :col-type boolean
    :documentation "If true: not for production use; may be altered or deleted at any time.")
   (photogrammetry-version
    :col-type text
    :documentation "Software version used to create this data.")
   (mounting-angle
    :col-type integer
    :documentation "Head up = 0; right ear up = 90; left ear up = -90; head down = 180.")
   (inner-orientation-description
    :col-type text
    :documentation "Comments regarding inner orientation calibration.")
   (c
    :col-type double-float
    :documentation "Inner orientation: focal length.")
   (xh
    :col-type double-float
    :documentation "Inner orientation: principal point displacement.")
   (yh
    :col-type double-float
    :documentation "Inner orientation: principal point displacement.")
   (a1
    :col-type double-float
    :documentation "Inner orientation: radial distortion.")
   (a2
    :col-type double-float
    :documentation "Inner orientation: radial distortion.")
   (a3
    :col-type double-float
    :documentation "Inner orientation: radial distortion.")
   (b1
    :col-type double-float
    :documentation "Inner orientation: asymmetric and tangential distortion.")
   (b2
    :col-type double-float
    :documentation "Inner orientation: asymmetric and tangential distortion.")
   (c1
    :col-type double-float
    :documentation "Inner orientation: affinity and shear distortion.")
   (c2
    :col-type double-float
    :documentation "Inner orientation: affinity and shear distortion.")
   (r0
    :col-type double-float
    :documentation "Inner orientation.")
   (outer-orientation-description
    :col-type text
    :documentation "Comments regarding outer orientation calibration.")
   (dx
    :col-type double-float
    :documentation "Outer orientation; in metres.")
   (dy
    :col-type double-float
    :documentation "Outer orientation; in metres.")
   (dz
    :col-type double-float
    :documentation "Outer orientation; in metres.")
   (omega
    :col-type double-float
    :documentation "Outer orientation.")
   (phi
    :col-type double-float
    :documentation "Outer orientation.")
   (kappa
    :col-type double-float
    :documentation "Outer orientation.")
   (boresight-description
    :col-type text
    :documentation "Comments regarding boresight alignment calibration.")
   (b-dx
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-dy
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-dz
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-ddx
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-ddy
    :col-type double-float
    :Documentation "Boresight alignment.")
   (b-ddz
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-rotx
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-roty
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-rotz
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-drotx
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-droty
    :col-type double-float
    :documentation "Boresight alignment.")
   (b-drotz
    :col-type double-float
    :documentation "Boresight alignment."))
  (:metaclass dao-class)
  (:keys device-stage-of-life-id date)
  (:documentation "TODO: description of vehicle ground plane (for mono photogrammetry)"))

(deftable sys-camera-calibration
  (!dao-def)
  (!foreign 'sys-device-stage-of-life 'device-stage-of-life-id :on-delete :restrict :on-update :restrict))

(defun create-all-sys-tables ()
  "Create in current database a set of sys-* tables, i.e. tables that are used by all projects.  The database should probably be empty."
  (create-table 'sys-user)
  (create-table 'sys-acquisition-project)
  (create-table 'sys-presentation-project)
  (create-table 'sys-user-role)
  (create-table 'sys-measurement)
  (create-table 'sys-presentation)
  (create-table 'sys-camera-hardware)
  (create-table 'sys-lens)
  (create-table 'sys-generic-device)
  (create-table 'sys-device-stage-of-life)
  (create-table 'sys-camera-calibration))

(defun !!index (table field &key (index-type :btree)) 
  (format nil "CREATE INDEX ~0@*~A_~1@*~A_index ON ~0@*~A USING ~2@*~A (~1@*~A)"
          (s-sql:to-sql-name table)
          (s-sql:to-sql-name field)
          (s-sql:to-sql-name index-type)))

(defclass point-template ()
  (;; We need a slot point-id which is defined in our subclasses.
   (measurement-id
    :writer (setf measurement-id)
    :col-type integer)
   (event-number
    :documentation "Event that triggered this record.  Taken from the GPS file name: ...eventN.txt gives an event number N.  May be a string of any length.")
   (gps-time
    :reader gps-time
    :documentation "UTC calculated from GPS week time.")
   (trigger-time
    :writer (setf trigger-time)
    :col-type double-precision
    :documentation "UNIX time, i.e. seconds from 1970.")
   (roll
    :col-type double-precision)
   (pitch
    :col-type double-precision)
   (heading
    :col-type double-precision)
   (east-velocity
    :col-type double-precision)
   (north-velocity
    :col-type double-precision)
   (up-velocity
    :col-type double-precision)
   (east-sd
    :col-type double-precision)
   (north-sd
    :col-type double-precision)
   (height-sd
    :col-type double-precision)
   (roll-sd
    :col-type double-precision)
   (pitch-sd
    :col-type double-precision)
   (heading-sd
    :col-type double-precision)
   (longitude
    :reader longitude
    :documentation "Same content as in slot coordinates.  TODO: should probably be made redundant in favour of the latter.")
   (latitude
    :reader latitude
    :documentation "Same content as in slot coordinates.  TODO: should probably be made redundant in favour of the latter.")
   (ellipsoid-height
    :reader ellipsoid-height
    :documentation "Same content as in slot coordinates.  TODO: should probably be made redundant in favour of the latter.")
   (coordinates
    :col-type (or db-null geometry)
    :documentation "Geographic coordinates.")
   (easting
    :reader easting
    :documentation "In the same coordinate system as the standard deviations.")
   (northing
    :reader northing
    :documentation "In the same coordinate system as the standard deviations.")
   (cartesian-height
    :reader cartesian-height
    :documentation "In the same coordinate system as the standard deviations."))
  (:metaclass dao-class)
  (:keys point-id)
  (:documentation "Information about one GPS point, originally from applanix/**/*event*.txt.  There shouldn't be any point-id without a matching one in the *-image table.  This can't be enforced on database level.  Perhaps we should create some cleaning operation to maintain referential integrity. (TODO)"))

(defclass image-template ()
  ((measurement-id
    :writer (setf measurement-id)
    :col-type integer
    :documentation "A primary key.  We need to recognize images should they come in twice, perhaps with slightly changed point data.  In such a case we want the old ones superseded.")
   (filename
    :reader filename
    :initarg :filename
    :col-type text
    :documentation "Name without any directory components.")
   (byte-position
    :reader image-byte-position
    :initarg :byte-position
    :col-type integer
    :documentation "Start of image in .pictures file named by slot filename.")
   (point-id
    :accessor point-id
    :col-type integer)
   (recorded-device-id
    :initarg :recorded-device-id
    :reader recorded-device-id
    :col-type text
    :documentation "As found in .pictures file, header tag `cam=´.")
   (footprint
    :col-type (or db-null geometry)
    :documentation "Polygon on the ground describing the approximate area covered by this image.")
   (gain
    :initarg :gain
    :col-type double-precision)
   (shutter
    :initarg :shutter
    :col-type double-precision)
   (trigger-time
    :initarg :trigger-time
    :accessor trigger-time
    :documentation "UNIX time, i.e. seconds from 1970.")
   (fake-trigger-time-p
    :accessor fake-trigger-time-p
    :initform nil
    :documentation "T if trigger-time has been reconstructed from adjacent data.")
   (camera-timestamp
    :initarg :camera-timestamp
    :reader camera-timestamp
    :documentation "Some camera clocktick count starting at an unknown origin."))
  (:metaclass dao-class)
  (:keys measurement-id filename byte-position)
  (:documentation "One row per image, originating from a .pictures file."))

(defclass point-data (point-template)
  ((point-id
    :accessor point-id
    :initform nil
    :col-type integer
    :col-default nil)                   ; to be redefined
   point-id-sequence-name)              ; to be redefined
  (:metaclass dao-class)
  (:table-name nil))                    ; to be redefined

(defclass image-data (image-template)
  ()
  (:metaclass dao-class)
  (:table-name nil))                    ; to be redefined

(defun create-data-table-definitions (common-table-name)
  "Define or redefine a bunch of dao-classes which can hold measuring data and which are connected to database tables named common-table-name plus type-specific prefix and/or suffix."
  (let* ((table-prefix "dat-")
         (image-data-table-name (format nil "~A~A-image" table-prefix common-table-name))
         (point-data-table-name (format nil "~A~A-point" table-prefix common-table-name))
         (point-id-sequence-name (make-symbol (format nil "~A~A-point-id-seq" table-prefix common-table-name))))
    (eval
     `(defclass point-data (point-template)
        ((point-id
          :accessor point-id
          :initform nil
          :col-type integer
          :col-default (:nextval ,point-id-sequence-name)) ; redefinition
         (point-id-sequence-name
          :initform ,(string point-id-sequence-name) ; redefinition
          :reader point-id-sequence-name
          :allocation :class))
        (:metaclass dao-class)
        (:table-name ,point-data-table-name))) ;redefinition
    (deftable point-data
      (:create-sequence point-id-sequence-name)
      (!dao-def)
      (!!index point-data-table-name 'measurement-id)
      (!!index point-data-table-name 'trigger-time)
      (!!index point-data-table-name 'coordinates :index-type :gist)
      ;; The following let shouldn't be necessary. (Wart In !foreign.)
      (let ((*table-symbol* point-data-table-name)
            (*table-name*  (s-sql:to-sql-name point-data-table-name)))
        (!foreign 'sys-measurement 'measurement-id :on-delete :cascade :on-update :cascade)))
    (eval
     `(defclass image-data (image-template)
        ()
        (:metaclass dao-class)
        (:table-name ,image-data-table-name))) ; redefintion
    (deftable image-data
      (!dao-def)
      (!!index image-data-table-name 'measurement-id)
      (!!index image-data-table-name 'recorded-device-id)
      (!!index image-data-table-name 'gain)
      (!!index image-data-table-name 'shutter)
      (!!index image-data-table-name 'footprint :index-type :gist)
      ;; The following let shouldn't be necessary. (Wart in !foreign.)
      (let ((*table-symbol* image-data-table-name)
            (*table-name*  (s-sql:to-sql-name image-data-table-name)))
        (!foreign point-data-table-name 'point-id :on-delete :cascade :on-update :cascade)
        (!foreign 'sys-measurement 'measurement-id :on-delete :cascade :on-update :cascade))
      )))

(defun create-data-tables (common-table-name)
  "Create in current database a fresh set of canonically named tables.  common-table-name should in most cases resemble the project name and will be stored in table sys-acquisition-project, field common-table-name."
  (create-data-table-definitions common-table-name)
  (handler-case (create-all-sys-tables) ; Create system tables if necessary.
    (cl-postgres-error:syntax-error-or-access-violation () nil))
  (when (select-dao 'sys-acquisition-project (:= 'common-table-name
                                     (s-sql:to-sql-name common-table-name)))
    (error "There is already a row with a common_table_name of ~A in table ~A."
           common-table-name (s-sql:to-sql-name (dao-table-name 'sys-acquisition-project))))
  (create-table 'point-data)
  (create-table 'image-data)
  (insert-dao
   (make-instance 'sys-acquisition-project
                  :common-table-name common-table-name)))
