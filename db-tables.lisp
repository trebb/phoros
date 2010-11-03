(in-package :phoros)



(defclass sys-users ()
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

(defclass project-users ()
  ((user-id
    :col-type integer)
   (project-id
    :col-type integer)
   (role
    :col-type text))
  (:metaclass dao-class)
  (:keys user-id project-id))

(defclass projects ()
  ((project-id
    :col-type integer)
   (project-name
    :col-type text)
   (table-name
    :col-type text))
  (:metaclass dao-class)
  (:keys project-id))

(defclass projects-measurements ()
  ((project-id
    :col-type integer)
   (measurement-id
    :col-type integer)
   (directory
    :col-type text
    :documentation "Below some universal common root; excluding `applanix/´ `einzelbilder´ etc."))
  (:metaclass dao-class)
  (:keys measurement-id))

(defclass camera-stage-of-life ()
  ((camera-stage-of-life-id
    :col-type integer)
   (recorded-device-id
    :col-type text
    :documentation "Stored inside each data record.")
   (photogrammetry-version
    :col-type text
    :documentation "Software version to use on this device's data. TODO: Really to be defined here?")
   (camera-hardware-id
    :col-type integer)
   (camera-lens-id
    :col-type integer)
   (vehicle-name
    :col-type text)
   (casing-name
    :col-type text
    :documentation "Something like `upper rear left´ or maybe `1.2.1´")
   (computer-name
    :col-type text
    :documentation "Computer (or or other recording device) this camera is connected to.")
   (computer-interface-name
    :col-type text
    :documentation "Things like `eth0´, `COM1´ etc.")
   (mounting-date
    :col-type timestamp)
   (unmounting-date
    :col-type (or db-null timestamp)))
  (:metaclass dao-class)
  (:keys camera-stage-of-life-id))

(defclass camera-hardware ()
  ((camera-hardware-id
    :col-type integer)
   (sensor-width-pix
    :col-type integer)
   (sensor-height-pix
    :col-type integer)
   (channels
    :col-type integer)
   (pix-size
    :col-type :double-float)
   (serial-number
    :col-type (or db-null text))
   (comment
    :col-type (or db-null text))
   (head-down
    :col-type boolean))
  (:metaclass dao-class)
  (:keys camera-hardware-id))

(defclass camera-lens ()
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

(defclass camera-outer-orientation ()
  ((outer-orientation-id
    :col-type integer)
   (date
    :col-type timestamp)
   (person
    :col-type text)
   (comment
    :col-type (or db-null text))
   (dx
    :col-type double-float
    :documentation "In metres.")
   (dy
    :col-type double-float
    :documentation "In metres.")
   (dz
    :col-type double-float
    :documentation "In metres.")
   (omega
    :col-type double-float)
   (phi
    :col-type double-float)
   (kappa
    :col-type double-float))
  (:metaclass dao-class)
  (:keys outer-orientation-id))

(defclass camera-inner-orientation ()
  ((inner-orientation-id
    :col-type integer)
   (date
    :col-type timestamp)
   (person
    :col-type text)
   (comment
    :col-type (or db-null text))
   (c
    :col-type double-float)
   (xh
    :col-type double-float)
   (yh
    :col-type double-float)
   (a1
    :col-type double-float)
   (a2
    :col-type double-float)
   (a3
    :col-type double-float)
   (b1
    :col-type double-float)
   (b2
    :col-type double-float)
   (c1
    :col-type double-float)
   (c2
    :col-type double-float)
   (r0
    :col-type double-float)
   (camera-stage-of-life-id
    :col-type integer))
  (:metaclass dao-class)
  (:keys inner-orientation-id))

(defclass camera-boresight-alignment ()
  ((boresight-alignment-id
    :col-type integer)
   (date
    :col-type timestamp)
   (person
    :col-type text)
   (comment
    :col-type (or db-null text))
   (bdx
    :col-type double-float)
   (bdy
    :col-type double-float)
   (bdz
    :col-type double-float)
   (bddx
    :col-type double-float)
   (bddy
    :col-type double-float)
   (bddz
    :col-type double-float)
   (brotx
    :col-type double-float)
   (broty
    :col-type double-float)
   (brotz
    :col-type double-float)
   (bdrotx
    :col-type double-float)
   (bdroty
    :col-type double-float)
   (bdrotz
    :col-type double-float))
  (:metaclass dao-class)
  (:keys boresight-alignment-id))

(defclass data-template-points ()
  ((point-id
    :col-type integer)
   (measurement-id
    :col-type integer)
   (recorded-device-id
    :col-type text
    :documentation "As found in .pictures file, header tag `cam=´.")
   (trigger-time
    :col-type double-precision
    :documentation "UNIX time, i.e. seconds from 1970.")
   (trigger-time-faked
    :col-type boolean
    :documentation "T if trigger-time has been reconstructed from adjacent data.")
   (applanix-time
    :col-type double-precision
    :documentation "UNIX time, i.e. seconds from 1970.  TODO: Why?")
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
   (the-geom
    :col-type geometry
    :documentation "Geographic coordinates."))
  (:metaclass dao-class)
  (:keys point-id))

(defclass data-template-images ()
  ((point-id
    :col-type integer)
   (filename
    :col-type text)
   (byte-position
    :col-type integer
    :documentation "Start of image in .pictures file named by slot filename.")
   (footprint
    :col-type geometry
    :documentation "Polygon on the ground describing the estimated area covered by this image.")
   (gain
    :col-type double-precision)
   (shutter
    :col-type double-precision))
  (:metaclass dao-class)
  (:keys point-id filename byte-position))
   