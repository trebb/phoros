(in-package :phoros)

(defun nuke-all-tables ()
  "Drop all tables (except PostGIS helper tables) in current database."
  (let ((user-tables (list-tables))
        (system-tables '(:spatial-ref-sys :geometry-columns)))
    (dolist (system-table system-tables)
      (setf user-tables (remove system-table user-tables)))
    (dolist (user-table user-tables) (execute (:drop-table user-table)))))

(defclass sys-user ()
  ((user-name
    :col-type text)
   (user-password
    :col-type text)
   (user-full-name
    :col-type text)
   (user-id
    :col-type integer))
  (:metaclass dao-class)
  (:keys user-id))

(defclass sys-project-user ()
  ((user-id
    :col-type integer)
   (project-id
    :col-type integer)
   (user-role
    :col-type text))
  (:metaclass dao-class)
  (:keys user-id project-id))

(defclass sys-project ()
  ((project-id
    :col-type integer)
   (project-name
    :col-type text)
   (table-name
    :col-type text
    :documentation "Should be one table for all projects but this seems to come with a speed penalty."))
  (:metaclass dao-class)
  (:keys project-id))

(defclass sys-measurement ()
  ((measurement-id
    :col-type integer)
   (project-id
    :col-type integer)
   (directory
    :col-type text
    :documentation "Below some universal root common to all measurements; excluding `applanix/´ `images/´ etc."))
  (:metaclass dao-class)
  (:keys measurement-id))

(defclass sys-device-stage-of-life ()
  ((device-stage-of-life-id
    :col-type integer)
   (recorded-device-id
    :col-type text
    :documentation "Must be stored near to each data record.")
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
    :col-type (or db-null timestamp)))
  (:metaclass dao-class)
  (:keys camera-stage-of-life-id))

(defclass sys-generic-device ()
  ((generic-device-id
    :col-type integer)
   (camera-hardware-id
    :col-type integer)
   (lens-id
    :col-type integer)
   (scanner-id
    :col-type integer))
  (:metaclass dao-class)
  (:keys generic-device-id))

(defclass sys-camera-hardware ()
  ((camera-hardware-id
    :col-type integer)
   (sensor-width-pix
    :col-type integer)
   (sensor-height-pix
    :col-type integer)
   (pix-size
    :col-type double-float)
   (channels
    :col-type integer)
   (max-pix-value
    :col-type integer)
   (color-raiser
    :col-type integer[])
   (pixel-colors
    :col-type integer[])
   (serial-number
    :col-type (or db-null text))
   (comments
    :col-type (or db-null text)))
  (:metaclass dao-class)
  (:keys camera-hardware-id))

(defclass sys-lens ()
  ((lens-id
    :col-type integer)
   (c
    :col-type double-float
    :documentation "Focal length.")
   (serial-number
    :col-type (or db-null text))
   (comment
    :col-type (or db-null text)))
  (:metaclass dao-class)
  (:keys lens-id))

(defclass sys-camera-calibration ()
  ((camera-calibration-id
    :col-type integer)
   (date
    :col-type timestamp)
   (person
    :col-type text)
   (comment
    :col-type (or db-null text))
   (photogrammetry-version
    :col-type text
    :documentation "Software version used to create this data.")
   (mounting-angle
    :col-type integer
    :documentation "Head up = 0; right ear up = 90; left ear up = -90; head down = 180.")
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
    :documentation "Boresight alignment.")
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
  (:keys boresight-alignment-id))

(defclass data-template-point ()
  ((point-id
    :col-type integer)
   (measurement-id
    :col-type integer)
   (trigger-time
    :col-type double-precision
    :documentation "UNIX time, i.e. seconds from 1970.")
   (trigger-time-faked
    :col-type boolean
    :documentation "T if trigger-time has been reconstructed from adjacent data.")
   (applanix-time
    :col-type double-precision
    :documentation "UNIX time, i.e. seconds from 1970.  TODO: scrap unless necessary to map point to measurement.")
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
   (the-geom
    :col-type geometry
    :documentation "Geographic coordinates."))
  (:metaclass dao-class)
  (:keys point-id))

(defclass data-template-image ()
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
  (:keys point-id filename byte-position))
   