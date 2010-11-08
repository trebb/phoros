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
  (:documentation "This is certainly not a full-fledged authentication system."))

(deftable sys-user
  (:create-sequence 'sys-user-id-seq)
  (!dao-def))

(defclass sys-project ()
  ((project-id
    :col-type integer
    :col-default (:nextval 'sys-project-id-seq))
   (project-name
    :col-type text
    :initarg :project-name)
   (common-table-name
    :col-type text
    :initarg :common-table-name
    :documentation "Name of this project's data tables sans their canonical prefixes and suffixes.  Should be one table for all projects but this seems to come with a speed penalty."))
  (:metaclass dao-class)
  (:keys project-id))

(deftable sys-project
  (:create-sequence 'sys-project-id-seq)
  (!dao-def))

(defclass sys-user-role ()
  ((user-id
    :col-type integer)
   (project-id
    :col-type integer)
   (user-role
    :col-type text
    :documentation "Some well-defined string, e.g. read-only, r/w, etc."))
  (:metaclass dao-class)
  (:keys user-id project-id))

(deftable sys-user-role
  (!dao-def)
  (!foreign 'sys-user 'user-id :on-delete :cascade :on-update :cascade)
  (!foreign 'sys-project 'project-id :on-delete :cascade :on-update :cascade))

(defclass sys-measurement ()
  ((measurement-id
    :col-type integer
    :col-default (:nextval 'sys-measurement-id-seq))
   (project-id
    :col-type integer)
   (directory
    :col-type text
    :documentation "Below some universal root common to all measurements; excluding `applanix/´ `images/´ etc."))
  (:metaclass dao-class)
  (:keys measurement-id))

(deftable sys-measurement
  (:create-sequence 'sys-measurement-id-seq)
  (!dao-def))

(defclass sys-camera-hardware ()
  ((camera-hardware-id
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
   (pixel-colors
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
    :col-type integer
    :col-default (:nextval 'sys-generic-device-id-seq))
   (camera-hardware-id
    :col-type (or db-null integer))
   (lens-id
    :col-type (or db-null integer))
   (scanner-id
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
    :col-type integer
    :col-default (:nextval 'sys-device-stage-of-life-seq))
   (recorded-device-id
    :col-type text
    :documentation "Must be stored next to each data record.")
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
    :col-type timestamp)
   (unmounting-date
    :col-type (or db-null timestamp)
    :documentation "Date and time when this device was unmounted or altered in other ways that may render the current calibration invalid."))
  (:metaclass dao-class)
  (:keys device-stage-of-life-id))

(deftable sys-device-stage-of-life
  (:create-sequence 'sys-device-stage-of-live-seq)
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
    :documentation "Inner orientation.")
   (yh
    :col-type double-float
    :documentation "Inner orientation.")
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
   (bdx
    :col-type double-float
    :documentation "Boresight alignment.")
   (bdy
    :col-type double-float
    :documentation "Boresight alignment.")
   (bdz
    :col-type double-float
    :documentation "Boresight alignment.")
   (bddx
    :col-type double-float
    :documentation "Boresight alignment.")
   (bddy
    :col-type double-float
    :Documentation "Boresight alignment.")
   (bddz
    :col-type double-float
    :documentation "Boresight alignment.")
   (brotx
    :col-type double-float
    :documentation "Boresight alignment.")
   (broty
    :col-type double-float
    :documentation "Boresight alignment.")
   (brotz
    :col-type double-float
    :documentation "Boresight alignment.")
   (bdrotx
    :col-type double-float
    :documentation "Boresight alignment.")
   (bdroty
    :col-type double-float
    :documentation "Boresight alignment.")
   (bdrotz
    :col-type double-float
    :documentation "Boresight alignment."))
  (:metaclass dao-class)
  (:keys device-stage-of-life-id date))

(deftable sys-camera-calibration
  (!dao-def)
  (!foreign 'sys-device-stage-of-life 'device-stage-of-life-id :on-delete :restrict :on-update :restrict))

(defun create-all-sys-tables ()
  "Create in current database a set of sys-* tables, i.e. tables that are used by all projects.  The database should probably be empty."
  (create-table 'sys-user)
  (create-table 'sys-project)
  (create-table 'sys-user-role)
  (create-table 'sys-measurement)
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

(defun create-data-table-definitions (common-table-name)
  "Define or redefine a bunch of dao-classes which can hold measuring data and which are connected to database tables named common-table-name plus type-specific prefix and/or suffix."
  (let* ((table-prefix "data-")
         (image-data-table-name (format nil "~A~A-image" table-prefix common-table-name))
         (point-data-table-name (format nil "~A~A-point" table-prefix common-table-name))
         (point-id-sequence-name (make-symbol (format nil "~A~A-point-id-seq" table-prefix common-table-name))))
    (eval
     `(defclass point-data ()
        ((point-id
          :col-type integer
          :col-default (:nextval ,point-id-sequence-name))
         (measurement-id
          :col-type integer)
         (trigger-time
          :col-type double-precision
          :documentation "UNIX time, i.e. seconds from 1970.")
         (trigger-time-faked
          :col-type boolean
          :documentation "T if trigger-time has been reconstructed from adjacent data.")
         (roll
          :col-type double-precision)
         (pitch
          :col-type double-precision)
         (Heading
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
         (coordinates
          :col-type geometry
          :documentation "Geographic coordinates."))
        (:metaclass dao-class)
        (:table-name ,point-data-table-name)
        (:keys point-id)
        (:documentation "There shouldn't be any point-id without an equal one in the *-image table.  This can't be enforced on database level.  Perhaps we should create some cleaning operation to maintain referential integrity. (TODO)")))
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
     `(defclass image-data ()
        ((point-id
          :col-type integer)
         (filename
          :col-type text
          :documentation "Name without any directory components.")
         (byte-position
          :col-type integer
          :documentation "Start of image in .pictures file named by slot filename.")
         (recorded-device-id
          :col-type text
          :documentation "As found in .pictures file, header tag `cam=´.")
         (footprint
          :col-type geometry
          :documentation "Polygon on the ground describing the approximate area covered by this image.")
         (gain
          :col-type double-precision)
         (shutter
          :col-type double-precision))
        (:metaclass dao-class)
        (:table-name ,image-data-table-name)
        (:keys point-id filename byte-position)
        (:documentation "One row per image.")))
    (deftable image-data
      (!dao-def)
      (!!index image-data-table-name 'recorded-device-id)
      (!!index image-data-table-name 'gain)
      (!!index image-data-table-name 'shutter)
      (!!index image-data-table-name 'footprint :index-type :gist)
      ;; The following let shouldn't be necessary. (Wart in !foreign.)
      (let ((*table-symbol* image-data-table-name)
            (*table-name*  (s-sql:to-sql-name image-data-table-name)))
        (!foreign point-data-table-name 'point-id :on-delete :cascade :on-update :cascade)))))

(defun create-data-tables (project-name &optional (common-table-name project-name))
  "Create in current database a (previously non-existing) set of canonically named tables.  common-table-name should in most cases resemble the project name and will be stored in table sys-project, field table-name."
  (create-data-table-definitions common-table-name)
  (handler-case (create-all-sys-tables) ; Create system tables if necessary.
    (cl-postgres-error:syntax-error-or-access-violation () nil))
  (when (select-dao 'sys-project (:= 'common-table-name
                                     (s-sql:to-sql-name common-table-name)))
    (error "There is already a row with a common_table_name of ~A in table ~A."
           common-table-name (s-sql:to-sql-name (dao-table-name 'sys-project))))
  (create-table 'point-data)
  (create-table 'image-data)
  (insert-dao
   (make-instance 'sys-project
                  :project-name project-name
                  :common-table-name common-table-name)))
