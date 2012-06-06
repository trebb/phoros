;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010, 2011, 2012 Bert Burgemeister
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

(let (serial-number description try-overwrite device-stage-of-life-id
                    c common-root bayer-pattern unmounting-date)
  (cli:defsynopsis ()
    (text :contents *phoros-long-description*)
    (text
     :contents    
     "Some options have a corresponding environment variable.  Phoros will set environment variables from definitions found in file <phoros-invocation-dir>/.phoros or, if that doesn't exist, in file ~/.phoros.")
    (text
     :contents
     "Options specified on the command line take precedence over any environment variables.  Pre-existing environment variables take precendence over definitions found in any .phoros files.")
    (text
     :contents
     "Config file syntax: one option per line; leading or trailing spaces are ignored; anything not beginning with PHOROS_ is ignored.")
    (enum :long-name "help" :short-name "h"
          :argument-name "FORMAT"
          :enum '(:long :short)
          :argument-type :optional
          :fallback-value :long
          :description "Print help in different formats [long|short] and exit.")
    (flag :long-name "licence"
          :description "Print licence boilerplate and exit.")
    (flag :long-name "license"
          :description "Same as --licence")
    (enum :long-name "version"
          :argument-name "FORMAT"
          :enum '(:all :minimal)
          :argument-type :optional
          :fallback-value :minimal
          :description "Print different amounts [minimal|all] of version information and exit.  In a version string A.B.C, changes in A denote incompatible changes in data; changes in B mean user-visible changes in feature set.")
    (group
     (:header "General Options:")
     (stropt :long-name "verbose"
             :description "Change behaviour, mainly for debugging, as specified in the form of <verbosity-topic>:<verbosity-level>.  Repeat if necessary.
  render-footprints:1 - display image footprints on http client
  suppress-preemptive-caching:1 - don't stuff browser cache with lots of images around map cursor
  log-sql:1 - log SQL activity
  postgresql-warnings:1 - show PostgreSQL warnings
  log-error-backtraces:1 - log http server error backtraces
  use-multi-file-openlayers:1 - use multi-file version of OpenLayers
  pretty-javascript:1 - send nicely formatted JavaScript
  show-server-errors:0 - send http server error messages to client.")
     ;; use-multi-file-openlayers:1 - Use OpenLayers uncompiled from
     ;; openlayers/*, which makes debugging easier and is necessary for
     ;; (ps; ... (debug-info ...)...) to work; doesn't work with
     ;; (OpenLayers 2.10 AND Firefox 4), though.  Otherwise use a
     ;; single-file shrunk ol/Openlayers.js.
     (stropt :long-name "umask"
             :env-var "PHOROS_UMASK"
             :argument-name "OCTAL_NUMBER"
             :default-value "002"
             :description "File permissions mask applied when Phoros creates files and directories.")
     (path :long-name "log-dir"
           :env-var "PHOROS_LOG_DIR"
           :type :directory
           :default-value #P"log.d/"
           :description "Where to put the log files.  Created if necessary; should end with a slash.")
     (flag :long-name "check-db"
           :description "Check connection to databases (including auxiliary if applicable) and exit.")
     (flag :long-name "check-dependencies"
           :description "Check presence of dependencies on local system and exit.")
     (flag :long-name "nuke-all-tables"
           :description "Ask for confirmation, then delete anything in database and exit.")
     (flag :long-name "create-sys-tables"
           :description "Ask for confirmation, then create in database a set of sys-* tables (tables shared between all projects).  The database should probably be empty before you try this."))
    (group
     (:header "Database Connection:")
     (text :contents "Necessary for most operations.")
     (stropt :long-name "host" :short-name "H"
             :env-var "PHOROS_HOST"
             :argument-name "NAME"
             :default-value "localhost"
             :description "Database server.")
     (lispobj :long-name "port" :short-name "P"
              :env-var "PHOROS_PORT"
              :typespec 'integer :argument-name "INT"
              :default-value 5432
              :description "Port on database server.")
     (stropt :long-name "database" :short-name "D"
             :env-var "PHOROS_DATABASE"
             :argument-name "NAME"
             :default-value "phoros"
             :description "Name of database.")
     (stropt :long-name "user" :short-name "U"
             :env-var "PHOROS_USER"
             :argument-name "NAME"
             :description "Database user.")
     (stropt :long-name "password" :short-name "W"
             :env-var "PHOROS_PASSWORD"
             :argument-name "PWD"
             :description "Database user's password.")
     (enum :long-name "use-ssl"
           :env-var "PHOROS_USE_SSL"
           :enum '(:yes :no :try)
           :argument-name "MODE"
           :default-value :no
           :description "Use SSL in database connection. [yes|no|try]"))
    (group
     (:header "Auxiliary Database Connection:")
     (text :contents "Connection parameters to the database containing auxiliary data.  Only needed for definition (--create-aux-view) and use (--server) of auxiliary data.")
     (stropt :long-name "aux-host"
             :env-var "PHOROS_AUX_HOST"
             :argument-name "NAME"
             :default-value "localhost"
             :description "Auxiliary database server.")
     (lispobj :long-name "aux-port"
              :env-var "PHOROS_AUX_PORT"
              :typespec 'integer :argument-name "INT"
              :default-value 5432
              :description "Port on auxiliary database server.")
     (stropt :long-name "aux-database"
             :env-var "PHOROS_AUX_DATABASE"
             :argument-name "NAME"
             :description "Name of auxiliary database.")
     (stropt :long-name "aux-user"
             :env-var "PHOROS_AUX_USER"
             :argument-name "NAME"
             :description "Auxiliary database user.")
     (stropt :long-name "aux-password"
             :env-var "PHOROS_AUX_PASSWORD"
             :argument-name "PWD"
             :description "Auxiliary database user's password.")
     (enum :long-name "aux-use-ssl"
           :env-var "PHOROS_AUX_USE_SSL"
           :argument-name "MODE"
           :enum '(:yes :no :try)
           :default-value :no
           :description "Use SSL in auxiliary database connection. [yes|no|try]"))
    (group
     (:header "Examine .pictures File:")
     (text :contents "Useful primarily for debugging purposes.")
     (flag :long-name "get-image"
           :description "Get a single image from a .pictures file, print its trigger-time to stdout, and exit.")
     (group ()
            (lispobj :long-name "count"
                     :typespec 'integer :argument-name "INT"
                     :default-value 0
                     :description "Image number in .pictures file.")
            (lispobj :long-name "byte-position"
                     :typespec 'integer :argument-name "INT"
                     :description "Byte position of image in .pictures file.")
            (path :long-name "in"
                  :type :file
                  :description "Path to .pictures file.")
            (path :long-name "out"
                  :type :file
                  :default-value #P"phoros-get-image.png"
                  :description "Path to output .png file.")
            ;; The way it should be had we two-dimensional arrays in postmodern:
            ;;("bayer-pattern" :type string :list t :optional t :action :raw-bayer-pattern :description "The first pixels of the first row.  Repeat this option to describe following row(s).  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
            (setf bayer-pattern
                  (cli:make-stropt
                   :long-name "bayer-pattern"
                   :default-value "#ff0000,#00ff00"
                   :description "The first pixels of the first row.  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green."))))
    (group
     (:header "Calibration Data:")
     (group
      (:header "Camera Hardware Parameters:")
      (text :contents "These do not include information on lenses or mounting.")
      (flag :long-name "store-camera-hardware"
            :description "Put new camera-hardware data into the database; print camera-hardware-id to stdout.")
      (group ()
             (lispobj :long-name "sensor-width-pix"
                      :typespec 'integer :argument-name "INT"
                      :description "Width of camera sensor.")
             (lispobj :long-name "sensor-height-pix"
                      :typespec 'integer :argument-name "INT"
                      :description "Height of camera sensor.")
             (lispobj :long-name "pix-size"
                      :typespec 'real :argument-name "NUM"
                      :description "Camera pixel size in millimetres (float).")
             (lispobj :long-name "channels"
                      :typespec 'integer :argument-name "INT"
                      :description "Number of color channels")
             (lispobj :long-name "pix-depth"
                      :typespec 'integer :argument-name "INT"
                      :default-value 255
                      :description "Greatest possible pixel value.")
             (stropt :long-name "color-raiser"
                     :default-value "1,1,1"
                     :description "Multipliers for the individual color components.  Example: 1.2,1,.8 multiplies red by 1.2 and blue by 0.8.")
             ;; The way it should be had we two-dimensional arrays in postmodern:
             ;;("bayer-pattern" :type string :list t :optional t :action :raw-bayer-pattern :description "The first pixels of the first row.  Repeat this option to describe following row(s).  Each pixel is to be interpreted as RGB hex string.  Example: use #ff0000,#00ff00 if the first pixels in topmost row are red, green.")
             bayer-pattern
             (setf serial-number
                   (cli:make-stropt
                    :long-name "serial-number"
                    :default-value " "
                    :description "Serial number."))
             (setf description
                   (cli:make-stropt
                    :long-name "description"
                    :default-value " "
                    :description "Description of camera."))
             (setf try-overwrite
                   (cli:make-switch
                    :long-name "try-overwrite"
                    :default-value t
                    :argument-type :required
                    :description "Overwrite matching record if any."))))
     (group
      (:header "Lens Parameters:")
      (text :contents "Stored primarily for human consumption; not used in photogrammetric calculations.")
      (flag :long-name "store-lens"
            :description "Put new lens data into the database; print lens-id to stdout.")
      (group ()
             (setf c
                   (cli:make-lispobj
                    :long-name "c"
                    :typespec 'real :argument-name "NUM"
                    :description "Focal length."))
             serial-number
             description
             try-overwrite))
     (group
      (:header "Generic Device Definition:")
      (text :contents "Basically, this is a particular camera fitted with a particular lens.")
      (flag :long-name "store-generic-device"
            :description "Put a newly defined generic-device into the database; print generic-device-id to stdout.")
      (group ()
             (lispobj :long-name "camera-hardware-id"
                      :typespec 'integer :argument-name "ID"
                      :description "Numeric camera hardware ID in database.")
             (lispobj :long-name "lens-id"
                      :typespec 'integer :argument-name "ID"
                      :description "Numeric lens ID in database.")
             (lispobj :long-name "scanner-id" ;unimplemented
                      :typespec '(or integer (eql :null)) :argument-name "ID"
                      :default-value :null
                      :description "Numeric scanner ID in database."
                      :hidden t)))
     (group
      (:header "Device Stage-Of-Life Definition:")
      (text :contents "A stage-of-life of a generic device is a possibly unfinished period of time during which the mounting constellation of the generic device remains unchanged.")
      (flag :long-name "store-device-stage-of-life"
            :description "Put a newly defined device-stage-of-life into the database; print device-stage-of-life-id to stdout.")
      (group ()
             (stropt :long-name "recorded-device-id"
                     :description "Device id stored next to the measuring data.")
             (stropt :long-name "event-number"
                     :description "GPS event that triggers this generic device.")
             (lispobj :long-name "generic-device-id"
                      :typespec 'integer :argument-name "ID"
                      :description "Numeric generic-device id in database.")
             (stropt :long-name "vehicle-name"
                     :description "Descriptive name of vehicle.")
             (stropt :long-name "casing-name"
                     :default-value " "
                     ;;KLUDGE:  " " is enforced by clon's help; should be "".
                     ;; We string-trim this away further down the line.
                     :description "Descriptive name of device casing.")
             (stropt :long-name "computer-name"
                     :default-value " "
                     :description "Name of the recording device.")
             (stropt :long-name "computer-interface-name"
                     :default-value " "
                     :description "Interface at device.")
             (stropt :long-name "mounting-date"
                     :description "Time this device constellation became effective.  Format: \"2010-11-19T13:49+01\".")
             (setf unmounting-date
                   (cli:make-stropt
                    :long-name "unmounting-date"
                    :default-value ":null"
                    :description "Time this device constellation ceased to be effective.  Format: \"2010-11-19T17:02+01\"."))))
     (group
      (:header "Put An End To A Device's Stage-Of-Life:")
      (text :contents "This should be done after any event that renders any portion of the calibration data invalid. E.g.: accidental change of mounting constellation.")
      (flag :long-name "store-device-stage-of-life-end"
            :description "Put an end date to a device-stage-of-life in the database; print device-stage-of-life-id to stdout.")
      (group ()
             (setf device-stage-of-life-id
                   (cli:make-lispobj
                    :long-name "device-stage-of-life-id"
                    :typespec 'integer :argument-name "ID"
                    :description "ID of the device-stage-of-life."))
             unmounting-date))
     (group
      (:header "Camera Calibration Parameters:")
      (flag :long-name "store-camera-calibration"
            :description "Put new camera-calibration into the database; print generic-device-id and calibration date to stdout.")
      (group ()
             device-stage-of-life-id
             (stropt :long-name "date"
                     :description "Date of calibration.  Format: \"2010-11-19T13:49+01\".")
             (stropt :long-name "person"
                     :description "Person who did the calibration.")
             (stropt :long-name "main-description"
                     :description "Regarding this entire set of calibration data")
             (switch :long-name "usable"
                     :default-value t
                     :description "Set to no to just display images and inhibit photogrammetric calculations.")
             (switch :long-name "debug"
                     :default-value nil
                     :description "If yes: not for production use; may be altered or deleted at any time.")
             (stropt :long-name "photogrammetry-version"
                     :description "Software version used to create this data.")
             (lispobj :long-name "mounting-angle"
                      :typespec '(member 0 90 -90 180)
                      :description "Head up = 0; right ear up = 90; left ear up = -90; head down = 180.")
             (stropt :long-name "inner-orientation-description"
                     :default-value " "
                     :description "Comments regarding inner orientation calibration.")
             c
             (lispobj :long-name "xh"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: principal point displacement.")
             (lispobj :long-name "yh"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: principal point displacement.")
             (lispobj :long-name "a1"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: radial distortion.")
             (lispobj :long-name "a2"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: radial distortion.")
             (lispobj :long-name "a3"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: radial distortion.")
             (lispobj :long-name "b1"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: asymmetric and tangential distortion.")
             (lispobj :long-name "b2"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: asymmetric and tangential distortion.")
             (lispobj :long-name "c1"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: affinity and shear distortion.")
             (lispobj :long-name "c2"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation: affinity and shear distortion.")
             (lispobj :long-name "r0"
                      :typespec 'real :argument-name "NUM"
                      :description "Inner orientation.")
             (stropt :long-name "outer-orientation-description"
                     :default-value " "
                     :description "Comments regarding outer orientation calibration.")
             (lispobj :long-name "dx"
                      :typespec 'real :argument-name "NUM"
                      :description "Outer orientation; in metres.")
             (lispobj :long-name "dy"
                      :typespec 'real :argument-name "NUM"
                      :description "Outer orientation; in metres.")
             (lispobj :long-name "dz"
                      :typespec 'real :argument-name "NUM"
                      :description "Outer orientation; in metres.")
             (lispobj :long-name "omega"
                      :typespec 'real :argument-name "NUM"
                      :description "Outer orientation.")
             (lispobj :long-name "phi"
                      :typespec 'real :argument-name "NUM"
                      :description "Outer orientation.")
             (lispobj :long-name "kappa"
                      :typespec 'real :argument-name "NUM"
                      :description "Outer orientation.")
             (stropt :long-name "boresight-description"
                     :default-value " "
                     :description "Comments regarding boresight alignment calibration.")
             (lispobj :long-name "b-dx"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-dy"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-dz"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-ddx"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-ddy"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-ddz"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-rotx"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-roty"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-rotz"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-drotx"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-droty"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "b-drotz"
                      :typespec 'real :argument-name "NUM"
                      :description "Boresight alignment.")
             (lispobj :long-name "nx"
                      :typespec 'real :argument-name "NUM"
                      :description "X component of unit vector of vehicle ground plane.")
             (lispobj :long-name "ny"
                      :typespec 'real :argument-name "NUM"
                      :description "Y component of unit vector of vehicle ground plane.")
             (lispobj :long-name "nz"
                      :typespec 'real :argument-name "NUM"
                      :description "Z component of unit vector of vehicle ground plane.")
             (lispobj :long-name "d"
                      :description "Distance of vehicle ground plane."))))
    (group
     (:header "Manage Acquisition Projects:")
     (text :contents "An acquisition project is a set of measurements which share a set of data tables and views named like dat-<acquisition-project-name>-point, dat-<acquisition-project-name>-image, dat-<acquisition-project-name>-aggregate.")
     (stropt :long-name "create-acquisition-project"
             :argument-name "NAME"
             :description "Create a fresh set of canonically named data tables.  NAME is the acquisition project name.  It will be stored in table sys-acquisition-project, field common-table-name, and used as a common part of the data table names.")
     (stropt :long-name "delete-acquisition-project"
             :argument-name "NAME"
             :description "Ask for confirmation, then delete acquisition project NAME and all its measurements.")
     (lispobj :long-name "delete-measurement"
              :typespec 'integer :argument-name "INT"
              :description "Delete a measurement by its ID.")
     (stropt :long-name "list-acquisition-project"
             :argument-name "NAME"
             :argument-type :optional
             :fallback-value "*"
             :description "List measurements of one acquisition project if its name is specified, or of all acquisition projects otherwise."))
    (group
     (:header "Store Measure Data:")
     (stropt :long-name "store-images-and-points" :short-name "s"
             :argument-name "NAME"
             :description "Link images to GPS points; store both into their respective DB tables.  Images become linked to GPS points when their respective times differ by less than epsilon seconds, and when the respective events match.  The string argument is the acquisition project name.")
     (group ()
            (path :long-name "directory" :short-name "d"
                  :type :directory
                  :description "Directory containing one set of measuring data.")
            (setf common-root
                  (cli:make-path
                   :long-name "common-root" :short-name "r"
                   :env-var "PHOROS_COMMON_ROOT"
                   :type :directory
                   :description "The root part of directory that is equal for all pojects.  TODO: come up with some sensible default."))
            (lispobj :long-name "epsilon"
                     :typespec 'real :argument-name "NUM"
                     :default-value .001
                     :description "Difference in seconds below which two timestamps are considered equal.")
            (switch :long-name "aggregate-events"
                    :default-value nil
                    :description "Put all GPS points in one bucket, disregarding any event numbers.  Use this if you have morons setting up your generic-device.  Hundreds of orphaned images may indicate this is the case."))
     (stropt :long-name "insert-footprints"
             :argument-name "NAME"
             :description "Update image footprints (the area on the ground that is most probably covered by the respective image) for acquisition project NAME."))
    (group
     (:header "Become An HTTP Presentation Server:")
     (text :contents "Phoros is a Web server in its own right, but you can also put it behind a proxy server to make it part of a larger Web site.  E.g., for Apache, load module proxy_http and use this configuration:
    ProxyPass /phoros http://127.0.0.1:8080/phoros
    ProxyPassReverse /phoros http://127.0.0.1:8080/phoros")
     (flag :long-name "server"
           :description "Start HTTP presentation server.  Entry URIs are http://<host>:<port>/phoros/<presentation-project>.  Asynchronously update lacking image footprints (which should have been done already using --insert-footprints).")
     (group ()
            (stropt :long-name "proxy-root"
                    :default-value "phoros"
                    :description "First directory element of the server URL.  Must correspond to the proxy configuration if Phoros is hidden behind a proxy.")
            (stropt :long-name "address"
                    :default-value "*"
                    :description "Address (of local machine) server is to listen on.  Default is listening on all available addresses.")
            (lispobj :long-name "http-port"
                     :typespec 'integer :argument-name "INT"
                     :default-value 8080
                     :description "Port the presentation server listens on.")
            common-root
            (lispobj :long-name "images"
                     :typespec 'integer :argument-name "INT"
                     :default-value 4
                     :description "Number of photos displayed on HTTP client.")
            (stropt :long-name "aux-numeric-label"
                    :description "HTML label for an element of auxiliary numeric data.  Repeat if necessary.  The succession of labels should match the auxiliary data (defined by --numeric-column) of all presentation projects served by this server instance.")
            (stropt :long-name "aux-text-label"
                    :description "HTML label for an element of auxiliary text data.  Repeat if necessary.  The succession of labels should match the auxiliary data (defined by --text-column) of all presentation projects served by this server instance.")
            (stropt :long-name "login-intro"
                    :description "Text to be shown below the login form.  Use repeatedly to divide text into paragraphs.  You can use HTML markup as long as it is legal inside <p>...</p>")))
    (group
     (:header "Manage Presentation Projects:")
     (text :contents "A presentation project is a set of measurements that can be visited under a dedicated URL \(http://<host>:<port>/phoros/<presentation-project>).  Its extent may or may not be equal to the extent of an acquisition project.")
     (text :contents "Presentation projects have a table of user points and a table of user lines.  The former is associated with a trigger which may be defined to induce writing into the latter.")
     (stropt :long-name "create-presentation-project"
             :argument-name "NAME"
             :description "Create a fresh presentation project NAME which is to expose a set of measurements to certain users.")
     (stropt :long-name "delete-presentation-project"
             :argument-name "NAME"
             :description "Ask for confirmation, then delete the presentation project including its table of user-generated points.")
     (stropt :long-name "list-presentation-project"
             :argument-name "NAME"
             :argument-type :optional
             :fallback-value "*"
             :description "List one presentation project if specified, or all presentation projects if not.")
     (stropt :long-name "add-to-presentation-project"
             :argument-name "NAME"
             :description "Add to presentation project NAME either certain measurements or all measurements currently in a certain acquisition project.")
     (stropt :long-name "remove-from-presentation-project"
             :argument-name "NAME"
             :description "Remove from presentation project NAME either certain measurements or all measurements currently in a certain acquisition project.")
     (group ()
            (lispobj :long-name "measurement-id"
                     :typespec 'integer :argument-name "ID"
                     :description "One measurement-id to add or remove.  Repeat if necessary.")
            (stropt :long-name "acquisition-project"
                    :argument-name "NAME"
                    :description "The acquisition project whose measurements are to add or remove.")
            (stropt :long-name "redefine-trigger-function"
                    :argument-name "NAME"
                    :description "Change body of the trigger function that is fired on changes to the user point table connected to presentation project NAME.")
            (path :long-name "plpgsql-body"
                  :type :file
                  :description "File containing the body of a PL/pgSQL trigger function.  Any ocurrence of the strings ~0@*~A and ~1@*~A will be replaced by the name of the user point table/of the user line table respectively.  Omit this option to reset that function to just emit a notice.")))
    (group
     (:header "Define Selectable Attributes For Images:")
     (text :contents "HTTP client users can select classes of images defined here.  Attributes are defined as PostgreSQL expressions and may use the following column names:")
     ;; ... which are obtainable like so:
     ;;   SELECT column_name
     ;;   FROM information_schema.columns
     ;;   WHERE table_name = 'dat_<acquisition-project>_aggregate';
     (text :contents "recorded_device_id, device_stage_of_life_id, generic_device_id, random, presentation_project_id, directory, measurement_id, filename, byte_position, point_id, footprint, footprint_device_stage_of_life_id, trigger_time, longitude, latitude, ellipsoid_height, cartesian_system, east_sd, north_sd, height_sd, roll, pitch, heading, roll_sd, pitch_sd, heading_sd, usable, sensor_width_pix, sensor_height_pix, pix_size, bayer_pattern, color_raiser, mounting_angle, dx, dy, dz, omega, phi, kappa, c, xh, yh, a1, a2, a3, b1, b2, c1, c2, r0, b_dx, b_dy, b_dz, b_rotx, b_roty, b_rotz, b_ddx, b_ddy, b_ddz, b_drotx, b_droty, b_drotz, nx, ny, nz, d.")
     (text :contents "Additionally, each of the column names can be prefixed by \"first_\" in order to refer to image data of the first image. (Example: \"measurement_id = first_measurement_id\" only displays images with equal measurement_id.)")
     (stropt :long-name "create-image-attribute"
             :argument-name "NAME"
             :description "Store, for presentation project NAME, a PostgreSQL expression an HTTP client user can use to select some subset of the images available.")
     (stropt :long-name "delete-image-attribute"
             :argument-name "NAME"
             :description "Delete presentation project NAME an image restriction identified by its tag.")
     (stropt :long-name "list-image-attribute"
             :argument-name "NAME"
             :argument-type :optional
             :fallback-value "*"
             :description "List restricting PostgreSQL expressions for presentation project NAME, or for all presentation projects.  If --tag is specified, list only matching expressions.")
     (group ()
            (stropt :long-name "tag"
                    :description "Identifying tag for the restriction.  Should be both short and descriptive as it is shown as a selectable item on HTTP client.")
            (stropt :long-name "sql-clause"
                    :description "Boolean PostgreSQL expression, to be used as an AND clause.  Should yield FALSE for images that are to be excluded.")))
    (group
     (:header "Connect A Presentation Project To A Table Of Auxiliary Data:")
     (text :contents "Arbitrary data from tables not directly belonging to any Phoros project can be connected to a presentation project by means of a view named phoros-<presentation-project-name>-aux-point with columns coordinates (geometry), aux-numeric (null or array of numeric), and aux-text (null or array of text).")
     (text :contents "The array elements of both aux-numeric and aux-text of auxiliary points can then be incorporated into neighbouring user points during user point creation.")
     (text :contents "To match the array elements to the labels shown on HTTP client (defined by --aux-numeric-label, --aux-text-label), NULL array elements can be used act as placeholders where appropriate.")
     (text :contents "Also, a walk mode along auxiliary points becomes available to the HTTP client.  PL/pgSQL function phoros-<presentation-project-name>-thread-aux-points is created to this end.")
     (text :contents "In order to be accessible by Phoros, auxiliary data must be structured rather simple (a single table which has a geometry column and some numeric and/or text columns).  You may want to create a simplifying view if your data looks more complicated.")
     (stropt :long-name "create-aux-view"
             :argument-name "NAME"
             :description "Connect table of auxiliary data with presentation project NAME by creating a view.")
     (group ()
            (stropt :long-name "aux-table"
                    :argument-name "NAME"
                    :description "Name of auxiliary table.  It may reside either in Phoros' native database or in an auxiliary database (which is common to all projects).  It must have a geometry column.")
            (stropt :long-name "coordinates-column"
                    :argument-name "NAME"
                    :default-value "the-geom"
                    :description "Name of the geometry column (which must contain geographic coordinates, SRID=4326; and which should have an index) in the auxiliary data table.")
            (stropt :long-name "numeric-column"
                    :argument-name "NAME"
                    :description "Name of a numeric column in the auxiliary data table.  An empty string defines an empty placeholder column.  Repeat if necessary.")
            (stropt :long-name "text-column"
                    :argument-name "NAME"
                    :description "Name of a text column in the auxiliary data table.  An empty string defines an empty placeholder column.  Repeat if necessary.")))
    (group
     (:header "Manage User Points:")
     (:text :contents "Backup/restore of user points; especially useful for getting them through database upgrades.")
     (stropt :long-name "get-user-points"
             :argument-name "NAME"
             :description "Save user points of presentation project NAME.")
     (stropt :long-name "store-user-points"
             :argument-name "NAME"
             :description "Store user points previously saved (using --get-user-points or download button in Web interface) into presentation project NAME.")
     (group ()
            (path :long-name "json-file"
                  :type :file
                  :description "Path to GeoJSON file.")))
    (group
     (:header "Manage Presentation Project Users:")
     (stropt :long-name "create-user"
             :argument-name "ID"
             :description "Create or update user (specified by their alphanummeric ID) of certain presentation projects, deleting any pre-existing permissions of that user.")
     (group ()
            (stropt :long-name "user-password"
                    :argument-name "PWD"
                    :description "User's password.")
            (stropt :long-name "user-full-name"
                    :description "User's real name.")
            (enum :long-name "user-role"
                  :enum '(:read :write :admin)
                  :default-value :read
                  :description "User's permission on their projects.  One of \"read\", \"write\", or \"admin\" where \"write\" is the same as \"read\" plus permission to add user points and delete them if written by themselves (or by unknown user); and \"admin\" is the same as \"write\" plus permission to delete points written by other users.")
            (stropt :long-name "presentation-project"
                    :argument-name "NAME"
                    :description "Presentation project the user is allowed to see.  Repeat if necessary."))
     (stropt :long-name "delete-user"
             :argument-name "ID"
             :description "Delete user.")
     (stropt :long-name "list-user"
             :argument-name "ID"
             :argument-type :optional
             :fallback-value "*"
             :description "List the specified user with their presentation projects, or all users if no user is given."))))

(defun cli:main ()
  "The UNIX command line entry point."
  (handler-bind
      ((sb-sys:interactive-interrupt
        (lambda (c)
          (declare (ignore c))
          (cl-log:log-message
           :error "Interactive interrupt.")
          (osicat-posix:exit 2)))
       (serious-condition
        (lambda (c)
          (cl-log:log-message
           :error "~A ~:[~;[Backtrace follows]~&~A~]~&"
           c
           (cli:verbosity-level :log-error-backtraces)
           (trivial-backtrace:print-backtrace c :output nil))
          (format *error-output* "~A~&" c)
          (osicat-posix:exit 1)))
       (warning
        (lambda (c) (cl-log:log-message :warning "~A" c))))
    (cffi:use-foreign-library phoml)
    (cli:set-.phoros-options)
    (cli:with-options (:tolerate-missing t)
        ((verbose) umask images (aux-numeric-label) (aux-text-label) (login-intro))
      (setf *verbosity* verbose)
      (setf *umask* umask)
      (setf *number-of-images* images)
      (setf *aux-numeric-labels* aux-numeric-label)
      (setf *aux-text-labels* aux-text-label)
      (setf *login-intro* login-intro))
    (cli:first-action-option help
                             licence
                             license
                             version
                             check-db
                             check-dependencies
                             nuke-all-tables
                             create-sys-tables
                             get-image
                             store-camera-hardware
                             store-lens
                             store-generic-device
                             store-device-stage-of-life
                             store-device-stage-of-life-end
                             store-camera-calibration
                             create-acquisition-project
                             delete-acquisition-project
                             delete-measurement
                             list-acquisition-project
                             store-images-and-points
                             insert-footprints
                             server
                             create-presentation-project
                             delete-presentation-project
                             list-presentation-project
                             add-to-presentation-project
                             remove-from-presentation-project
                             redefine-trigger-function
                             create-image-attribute
                             delete-image-attribute
                             list-image-attribute
                             create-aux-view
                             get-user-points
                             store-user-points
                             create-user
                             delete-user
                             list-user)
    (osicat-posix:exit *unix-exit-code*)))

(defun cli:set-.phoros-options ()
  "Set previously non-existent environment variables, whose names must
start with PHOROS_, according to the most relevant .phoros file."
  (let ((.phoros-path (or (probe-file
                           (make-pathname
                            :name ".phoros"
                            :defaults *default-pathname-defaults*))
                          (probe-file
                           (make-pathname
                            :name ".phoros"
                            :directory (directory-namestring
                                        (user-homedir-pathname))))))
        (unix-environment (osicat:environment)))
    (when .phoros-path
      (with-open-file (s .phoros-path)
        (loop
           for line = (read-line s nil nil)
           while line
           for option = (string-trim " " line)
           for (name value junk) = (cl-utilities:split-sequence #\= option)
           if (and (>= (length name) 7)
                   (string= (subseq name 0 7) "PHOROS_")
                   value
                   (not junk))
           do (setf unix-environment
                    (adjoin (cons name value)
                            unix-environment
                            :test #'(lambda (x y)
                                      (string= (car x) (car y)))))))
      (setf (osicat:environment) unix-environment))))

(defun cli:verbosity-level (topic)
  "Return the number associated with verbose topic, or nil if the
number is 0 or doesn't exist."
  (let* ((digested-verbosity
          (loop
             for entry in *verbosity*
             collect
               (destructuring-bind (topic &optional level)
                   (cl-utilities:split-sequence
                    #\: entry :count 2 :remove-empty-subseqs t)
                 (cons (intern (string-upcase topic) 'keyword)
                       (ignore-errors
                         (parse-integer level :junk-allowed t))))))
         (level (cdr (assoc topic digested-verbosity))))
    (unless (or (null level) (zerop level))
      level)))

(defun cli:set-umask ()
  "Set umask to the value from its octal representation stored in
*umask*"
  (let ((umask (ignore-errors (parse-integer *umask* :radix 8))))
    (assert (typep umask '(integer #o000 #o777)) ()
            "~O is not a valid umask."
            *umask*)
    (osicat-posix:umask umask)))

(defun cli:getopt-mandatory (long-name)
  "Return value of command line option long-name if any. Otherwise
signal error."
  (multiple-value-bind (value supplied-p) (cli:getopt :long-name long-name)
    (assert supplied-p () "Missing option --~A." long-name)
    value))

(defun cli:help-action ()
  (cli:with-options () (help)
    (ecase help
      (:long (cli:help :theme "etc/phoros.cth"))
      (:short (cli:help :theme "etc/short.cth")))))

(defun cli:version-action ()
  "Print --version message. TODO: OpenLayers, Proj4js version."
  (cli:with-options () (version)
    (ecase version
      (:all
       (format
        *standard-output*
        "~&~A version ~A~&  ~A version ~A~&  ~
        Proj4 library: ~A~&  PhoML version ~A~&"
        *phoros-description*
        (phoros-version)
        (lisp-implementation-type) (lisp-implementation-version)
        (proj:version)
        (phoml:get-version-number)))
      (:minimal
       (format *standard-output* "~&~A~&" (phoros-version))))))

(defun cli:licence-action ()
  "Print --licence boilerplate."
  (format *standard-output* "~&~A~&" *phoros-licence*))

(defun cli:license-action ()
  (cli:licence-action))

(defun cli:check-db-action ()
  "Tell us if databases are accessible."
  (cli:with-options ()
      (host port database user use-ssl
            aux-host aux-port
            aux-database aux-user aux-password password aux-use-ssl)
    (format *error-output*
            "Checking database ~A at ~A:~D and ~
            auxiliary database ~A at ~A:~D.~%"
            database host port
            aux-database aux-host aux-port)
    (if (and
         (check-db (list database user password host
                         :port port
                         :use-ssl (s-sql:from-sql-name use-ssl)))
         (check-db (list aux-database aux-user aux-password aux-host
                         :port aux-port
                         :use-ssl (s-sql:from-sql-name aux-use-ssl))))
        (progn
          (format *error-output*
                  "Both are accessible.~%")
          (setf *unix-exit-code* 0))
        (setf *unix-exit-code* 2))))

(defun cli:check-dependencies-action ()
  "Say OK if the necessary external dependencies are available."
  (check-dependencies))

(defun cli:nuke-all-tables-action ()
  "Drop the bomb.  Ask for confirmation first."
  (cli:with-options (:database t :log t) ()
    (when (yes-or-no-p
           "You asked me to delete anything in database ~A at ~A:~D.  ~
           Proceed?"
           database host port)
      (nuke-all-tables))
    (cl-log:log-message
     :db-sys "Nuked database ~A at ~A:~D.  Back to square one!"
     database host port)))

(defun cli:create-sys-tables-action ()
  "Make a set of sys-* tables.  Ask for confirmation first."
  (cli:with-options (:database t :log t) ()
    (when (yes-or-no-p
           "You asked me to create a set of sys-* tables ~
           in database ~A at ~A:~D.  ~
           Make sure you know what you are doing.  Proceed?"
           database host port)
      (create-sys-tables))
    (cl-log:log-message
     :db-sys "Created a fresh set of system tables in database ~A at ~A:~D."
     database host port)))

(defun cli:create-acquisition-project-action ()
  "Make a set of data tables."
  (cli:with-options (:database t :log t) (create-acquisition-project)
    (let ((common-table-name create-acquisition-project))
      (create-acquisition-project common-table-name)
      (cl-log:log-message
       :db-dat
       "Created a fresh acquisition project by the name of ~A ~
       in database ~A at ~A:~D."
       common-table-name database host port))))

(defun cli:delete-acquisition-project-action ()
  "Delete an acquisition project."
  (cli:with-options (:database t :log t) (delete-acquisition-project)
    (let ((common-table-name delete-acquisition-project))
      (assert-acquisition-project common-table-name)
      (when (yes-or-no-p
             "You asked me to delete acquisition-project ~A ~
             (including all its measurements) ~
             from database ~A at ~A:~D.  Proceed?"
             common-table-name database host port)
        (let ((project-did-exist-p
                 (delete-acquisition-project common-table-name)))
            (cl-log:log-message
             :db-dat
             "~:[Tried to delete nonexistent~;Deleted~] ~
             acquisition project ~A from database ~A at ~A:~D."
             project-did-exist-p common-table-name database host port))))))

(defun cli:delete-measurement-action ()
  "Delete a measurement by its measurement-id."
  (cli:with-options (:database t :log t) (delete-measurement)
    (let* ((measurement-id delete-measurement)
           (measurement-did-exist-p (delete-measurement measurement-id)))
      (cl-log:log-message
       :db-dat
       "~:[Tried to delete nonexistent~;Deleted~] ~
       measurement with ID ~A from database ~A at ~A:~D."
       measurement-did-exist-p measurement-id database host port))))

(defun cli:list-acquisition-project-action ()
  "List content of acquisition projects."
  (cli:with-options (:database t) (list-acquisition-project)
    (let* ((common-table-name  (if (string= list-acquisition-project "*")
                                   'common-table-name
                                   list-acquisition-project))
           (content
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
              'measurement-id))))
      (cli:format-table *standard-output* content
                        '("Acquisition Project" "ID" "Meas. ID"
                          "Directory" "Cartesian CS")))))

(defun cli:store-images-and-points-action ()
  "Put data into the data tables."
  (cli:with-options (:database t :log t)
      (directory epsilon common-root aggregate-events store-images-and-points)
    (let ((common-table-name store-images-and-points))
      (assert-acquisition-project common-table-name)
      (cl-log:log-message
       :db-dat
       "Start: storing data from ~A into acquisition project ~A ~
       in database ~A at ~A:~D."
       directory common-table-name database host port)
      (store-images-and-points common-table-name directory
                               :epsilon epsilon
                               :root-dir common-root
                               :aggregate-events aggregate-events)
      (cl-log:log-message
       :db-dat
       "Finish: storing data from ~A into acquisition project ~A ~
       in database ~A at ~A:~D."
       directory common-table-name database host port)
      (let ((points-deleted
             (delete-imageless-points common-table-name)))
        (cl-log:log-message
         :db-dat
         "Checked acquisition project ~A in database ~A at ~A:~D ~
         for imageless points~[; found none.~;. Found and deleted ~:*~D.~]"
         common-table-name database host port
         points-deleted)))))

(defun cli:insert-footprints-action ()
  "Update image footprints."
  (cli:with-options (:database t :log t) (host port database user password use-ssl
                          log-dir
                          insert-footprints)
    (let ((common-table-name insert-footprints))
      (assert-acquisition-project common-table-name)
      (cl-log:log-message
       :db-dat
       "Updating image footprints of acquisition project ~A ~
       in database ~A at ~A:~D."
       common-table-name database host port)
      (let ((number-of-updated-footprints
             (insert-footprints common-table-name)))
        (cl-log:log-message
         :db-dat
         "~:[All image footprints belonging to acquisition project ~*~A ~
         in database ~A at ~A:~D are up to date.~
             ~;Updated ~D image footprint~:P of acquisition project ~A ~
               in database ~A at ~A:~D.~]"
         (plusp number-of-updated-footprints) number-of-updated-footprints
         common-table-name database host port)))))

;;; We don't seem to have two-dimensional arrays in postmodern
;;(defun cli:canonicalize-bayer-pattern (raw &optional sql-string-p)
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

(defun cli:canonicalize-bayer-pattern (raw &optional sql-string-p)
  "Convert a string of comma-separated hex color strings (ex: #ff0000
for red) into a vector of integers.  If sql-string-p is t, convert it
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

(defun cli:canonicalize-color-raiser (raw &optional sql-string-p)
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

(defun cli:store-camera-hardware-action ()
  (cli:with-options (:database t :log t)
      (try-overwrite
       sensor-width-pix
       sensor-height-pix
       pix-size
       channels
       pix-depth
       color-raiser
       bayer-pattern
       serial-number
       description)
    (format *standard-output* "~D~%"
            (store-camera-hardware
             :try-overwrite try-overwrite
             :sensor-width-pix sensor-width-pix
             :sensor-height-pix sensor-height-pix
             :pix-size pix-size
             :channels channels
             :pix-depth pix-depth
             :color-raiser (cli:canonicalize-color-raiser color-raiser)
             :bayer-pattern (cli:canonicalize-bayer-pattern bayer-pattern)
             :serial-number (string-trim " " serial-number)
             :description (string-trim " " description)))))

(defun cli:store-lens-action ()
  (cli:with-options (:database t :log t)
      (try-overwrite
       c
       serial-number
       description)
    (format *standard-output* "~D~%"
            (store-lens
             :try-overwrite try-overwrite
             :c c
             :serial-number (string-trim " " serial-number)
             :description (string-trim " " description)))))

(defun cli:store-generic-device-action ()
  (cli:with-options (:database t :log t)
      (camera-hardware-id
       lens-id
       scanner-id)
    (format *standard-output* "~D~%"
            (store-generic-device
             :camera-hardware-id camera-hardware-id
             :lens-id lens-id
             :scanner-id scanner-id))))
    
(defun cli:string-or-null (string)
  "If string is \":null\", return :null; otherwise return string."
  (if (string-equal string ":null") :null string))

(defun cli:store-device-stage-of-life-action ()
  (cli:with-options (:database t :log t)
      (unmounting-date
       try-overwrite
       recorded-device-id
       event-number
       generic-device-id
       vehicle-name
       casing-name
       computer-name
       computer-interface-name
       mounting-date)
    (format *standard-output* "~D~%"
            (store-device-stage-of-life
             :unmounting-date (cli:string-or-null unmounting-date)
             :try-overwrite try-overwrite
             :recorded-device-id recorded-device-id
             :event-number event-number
             :generic-device-id generic-device-id
             :vehicle-name (string-trim " " vehicle-name)
             :casing-name (string-trim " " casing-name)
             :computer-name (string-trim " " computer-name)
             :computer-interface-name computer-interface-name
             :mounting-date mounting-date))))

(defun cli:store-device-stage-of-life-end-action ()
  (cli:with-options (:database t :log t)
      (device-stage-of-life-id
       unmounting-date)
    (format *standard-output* "~D~%"
            (store-device-stage-of-life-end
             :device-stage-of-life-id device-stage-of-life-id
             :unmounting-date unmounting-date))))

(defun cli:store-camera-calibration-action ()
  (cli:with-options (:database t :log t)
      (usable
       device-stage-of-life-id
       date
       person
       main-description
       debug
       photogrammetry-version
       mounting-angle
       inner-orientation-description
       c
       xh
       yh
       a1
       a2
       a3
       b1
       b2
       c1
       c2
       r0
       outer-orientation-description
       dx
       dy
       dz
       omega
       phi
       kappa
       boresight-description
       b-dx
       b-dy
       b-dz
       b-ddx
       b-ddy
       b-ddz
       b-rotx
       b-roty
       b-rotz
       b-drotx
       b-droty
       b-drotz
       nx
       ny
       nz
       d)
    (format *standard-output* "~D~%"
            (store-camera-calibration
             :usable usable
             :device-stage-of-life-id device-stage-of-life-id
             :date date
             :person person
             :main-description main-description
             :debug debug
             :photogrammetry-version photogrammetry-version
             :mounting-angle mounting-angle
             :inner-orientation-description (string-trim " " inner-orientation-description)
             :c c
             :xh xh
             :yh yh
             :a1 a1
             :a2 a2
             :a3 a3
             :b1 b1
             :b2 b2
             :c1 c1
             :c2 c2
             :r0 r0
             :outer-orientation-description (string-trim " " outer-orientation-description)
             :dx dx
             :dy dy
             :dz dz
             :omega omega
             :phi phi
             :kappa kappa
             :boresight-description (string-trim " " boresight-description)
             :b-dx b-dx
             :b-dy b-dy
             :b-dz b-dz
             :b-ddx b-ddx
             :b-ddy b-ddy
             :b-ddz b-ddz
             :b-rotx b-rotx
             :b-roty b-roty
             :b-rotz b-rotz
             :b-drotx b-drotx
             :b-droty b-droty
             :b-drotz b-drotz
             :nx nx
             :ny ny
             :nz nz
             :d d))))

(defun cli:get-image-action ()
  "Output a PNG file extracted from a .pictures file; print its
trigger-time to stdout."
  (cli:with-options () (count byte-position in out
                           raw-bayer-pattern raw-color-raiser)
    (with-open-file (out-stream out :direction :output
                                :element-type 'unsigned-byte
                                :if-exists :supersede)
      (let ((trigger-time
             (if byte-position
                 (send-png out-stream in byte-position
                           :bayer-pattern
                           (cli:canonicalize-bayer-pattern raw-bayer-pattern)
                           :color-raiser
                           (cli:canonicalize-color-raiser raw-color-raiser))
                 (send-nth-png count out-stream in
                               :bayer-pattern
                               (cli:canonicalize-bayer-pattern raw-bayer-pattern)
                               :color-raiser
                               (cli:canonicalize-color-raiser raw-color-raiser)))))
        (format *standard-output*
                "~&~A~%" (timestring (utc-from-unix trigger-time)))))))

(defun cli:create-presentation-project-action ()
  "Make a presentation project."
  (cli:with-options (:database t :log t) (create-presentation-project)
    (let* ((presentation-project-name create-presentation-project)
           (fresh-project-p
            (create-presentation-project presentation-project-name)))
      (cl-log:log-message
       :db-dat
       "~:[Tried to recreate an existing~;Created a fresh~] ~
       presentation project by the name of ~A in database ~A at ~A:~D."
       fresh-project-p presentation-project-name database host port))))

(defun cli:delete-presentation-project-action ()
  "Delete a presentation project."
  (cli:with-options (:database t :log t) (delete-presentation-project)
    (let ((presentation-project-name delete-presentation-project))
      (assert-presentation-project presentation-project-name)
      (when (yes-or-no-p
             "You asked me to delete presentation-project ~A ~
             (including its tables of user-defined points and lines, ~
             ~A and ~A respectively) from database ~A at ~A:~D.  Proceed?"
             presentation-project-name
             (user-point-table-name presentation-project-name)
             (user-line-table-name presentation-project-name)
             database host port)
        (let ((project-did-exist-p
               (delete-presentation-project presentation-project-name)))
          (cl-log:log-message
           :db-dat
           "~:[Tried to delete nonexistent~;Deleted~] ~
           presentation project ~A from database ~A at ~A:~D."
           project-did-exist-p presentation-project-name
           database host port))))))

(defun cli:add-to-presentation-project-action ()
  "Add measurements to a presentation project."
  (cli:with-options (:database t :log t)
      (add-to-presentation-project)
    (cli:with-options (:tolerate-missing t)
        (measurement-id acquisition-project)
      (let ((presentation-project-name add-to-presentation-project))
        (assert-presentation-project presentation-project-name)
        (add-to-presentation-project presentation-project-name
                                     :measurement-ids measurement-id
                                     :acquisition-project acquisition-project)
        (cl-log:log-message
         :db-dat
         "Added ~@[measurement-ids ~{~D~#^, ~}~]~
       ~@[all measurements from acquisition project ~A~] ~
       to presentation project ~A in database ~A at ~A:~D."
         measurement-id acquisition-project
         presentation-project-name database host port)))))

(defun cli:remove-from-presentation-project-action ()
  "Add measurements to a presentation project."
  (cli:with-options (:database t :log t)
      (measurement-id acquisition-project remove-from-presentation-project)
    (let ((presentation-project-name remove-from-presentation-project))
      (assert-presentation-project presentation-project-name)
      (remove-from-presentation-project
         presentation-project-name
         :measurement-ids measurement-id
         :acquisition-project acquisition-project)
      (cl-log:log-message
       :db-dat
       "Removed ~@[measurement-ids ~{~D~#^, ~}~]~
       ~@[all measurements that belong to acquisition project ~A~] ~
       from presentation project ~A in database ~A at ~A:~D."
       measurement-id acquisition-project
       presentation-project-name database host port))))

(defun cli:create-image-attribute-action ()
  "Store a boolean SQL expression."
  (cli:with-options (:database t :log t)
      (tag sql-clause create-image-attribute)
    (let ((presentation-project-name create-image-attribute))
      (assert-presentation-project presentation-project-name)
      (multiple-value-bind (old-image-attribute
                            number-of-selected-images
                            total-number-of-images)
          (create-image-attribute presentation-project-name
                                  :tag tag :sql-clause sql-clause)
        (cl-log:log-message
         :db-dat
         "~:[Stored a fresh~;Updated an~] ~
         image attribute, tagged ~S, for presentation project ~A ~
         in database ~A at ~A:~D~
         ~0@*~@[, replacing the SQL clause previously stored there of ~S~].  ~
         ~6@*~@[The new SQL clause currently selects ~D out of ~D images.~]"
         old-image-attribute
         tag
         presentation-project-name
         database host port
         number-of-selected-images total-number-of-images)))))

(defun cli:delete-image-attribute-action ()
  "Remove SQL expression specified by presentation-project-name and tag."
  (cli:with-options (:database t :log t)
      (tag delete-image-attribute)
    (let ((presentation-project-name delete-image-attribute))
      (assert-presentation-project presentation-project-name)
      (let ((replaced-sql-clause
             (delete-image-attribute presentation-project-name :tag tag)))
        (cl-log:log-message
         :db-dat
         "~:[Tried to delete a nonexistent~;Deleted~] ~
       image attribute tagged ~S from ~
       presentation project ~A in database ~A at ~A:~D.  ~
       ~0@*~@[Its SQL clause, now deleted, was ~S~]"
         replaced-sql-clause tag presentation-project-name
         database host port)))))

(defun cli:list-image-attribute-action ()
  "List boolean SQL expressions."
  (cli:with-options (:database t) (tag list-image-attribute)
    (let* ((presentation-project-name (if (string= list-image-attribute "*")
                                          'presentation-project-name
                                          list-image-attribute))
           (restriction-id (or tag 'restriction-id))
           (content
            (query
             (:order-by
              (:select 'presentation-project-name
                       'sys-selectable-restriction.presentation-project-id
                       'restriction-id
                       'sql-clause
                       :from 'sys-selectable-restriction
                       :natural :left-join 'sys-presentation-project
                       :where (:and (:= presentation-project-name
                                        'presentation-project-name)
                                    (:= restriction-id
                                        'restriction-id)))
              'presentation-project-name 'restriction-id))))
      (cli:format-table *standard-output* content
                        '("Presentation Project" "ID" "Tag" "SQL-clause")
                        :column-widths '(nil nil nil 60)))))

(defun cli:redefine-trigger-function-action ()
  "Recreate an SQL trigger function that is fired on changes to the
user point table, and fire it once."
  (cli:with-options (:database t :log t)
      (plpgsql-body redefine-trigger-function)
    (let ((presentation-project-name redefine-trigger-function)
          (body-text (make-array '(1) :adjustable t :fill-pointer 0
                                 :element-type 'character)))
      (if plpgsql-body
          (with-open-file (stream plpgsql-body)
            (loop
               for c = (read-char stream nil)
               while c
               do (vector-push-extend c body-text))
            (create-presentation-project-trigger-function
             presentation-project-name
             body-text
             (s-sql:to-sql-name (user-point-table-name
                                 presentation-project-name))
             (s-sql:to-sql-name (user-line-table-name
                                 presentation-project-name))))
          (create-presentation-project-trigger-function
           presentation-project-name))
      (fire-presentation-project-trigger-function presentation-project-name)
      (cl-log:log-message
       :db-dat
       "Defined (and fired once) ~
       a trigger function associatad with user point table of ~
       presentation project ~A in database ~A at ~A:~D to ~
       ~:[perform a minimal default action.~;perform the body given ~
          in file ~:*~A, whose content is is:~&~A~]"
       presentation-project-name database host port
       plpgsql-body body-text))))

(defun cli:create-aux-view-action ()
  "Connect presentation project to an auxiliary data table by means of
a view."
  (cli:with-options (:database t :log t) (create-aux-view)
    (assert-presentation-project create-aux-view))
  (cli:with-options (:aux-database t :log t)
      (host port database user password use-ssl 
            coordinates-column (numeric-column) (text-column) aux-table
            create-aux-view)
    (let* ((presentation-project-name create-aux-view)
           (numeric-columns
            (nsubstitute nil "" numeric-column :test #'string=))
           (text-columns
            (nsubstitute nil "" text-column :test #'string=))
           (aux-view-in-phoros-db-p
            (every #'equal
                   (list host port database user password use-ssl)
                   (list aux-host aux-port aux-database
                         aux-user aux-password aux-use-ssl)))
           (aux-view-exists-p
            (aux-view-exists-p presentation-project-name)))
      (when (or
             aux-view-in-phoros-db-p
             (yes-or-no-p
              "I'm going to ~:[create~;replace~] a view named ~A ~
              in database ~A at ~A:~D.  Proceed?"
              aux-view-exists-p
              (aux-point-view-name presentation-project-name)
              aux-database aux-host aux-port))
        (when aux-view-exists-p
          (delete-aux-view presentation-project-name))
        (create-aux-view presentation-project-name
                         :coordinates-column coordinates-column
                         :numeric-columns numeric-columns
                         :text-columns text-columns
                         :aux-table aux-table)
        (add-spherical-mercator-ref)
        (cl-log:log-message
         :db-dat
         "~:[Created~;Updated~] in database ~A at ~A:~D a view called ~A ~
         into table (of auxiliary data) ~A.  Coordinates column is ~A.  ~
         ~:[No numeric columns.~;Numeric column(s): ~:*~{~A~#^, ~}.~]  ~
         ~:[No text columns.~;Text column(s): ~:*~{~A~#^, ~}.~]  ~
         Also, ~0@*~:[created~;recreated~] in the same database a ~
         function called ~9@*~A."
         aux-view-exists-p
         aux-database aux-host aux-port
         (aux-point-view-name presentation-project-name)
         aux-table coordinates-column
         numeric-columns text-columns
         (thread-aux-points-function-name presentation-project-name))))))

(defun cli:store-user-points-action ()
  "Store user points from a GeoJSON file into database."
  (cli:with-options (:database t :log t) (json-file store-user-points)
    (let ((presentation-project store-user-points))
      (assert-presentation-project presentation-project)
      (multiple-value-bind
            (points-stored points-already-in-db points-tried zombie-users)
          (store-user-points presentation-project :json-file json-file)
        (cl-log:log-message
         :db-dat
         "Tried to store the ~D user point~:P I found in file ~A ~
         into presentation project ~A in database ~A at ~A:~D.  ~
         ~:[~:[~D~;None~*~]~;All~2*~] of them ~:[were~;was~] ~
         already present.  ~
         ~:[~:[~:[~D points have~;1 point has~*~]~;Nothing has~2*~]~
            ~;All points tried have~3*~] ~
         been added to the user point table.  ~
         ~15@*~@[I didn't know ~14@*~[~;a~:;any of the~] user~14@*~P ~
         called ~{~A~#^, ~}; treated them as zombie~14@*~P.~]"
         points-tried
         (truename json-file)
         presentation-project database host port
         (= points-already-in-db points-tried)
         (zerop points-already-in-db)
         points-already-in-db
         (<= points-already-in-db 1)
         (= points-stored points-tried)
         (zerop points-stored)
         (= 1 points-stored)
         points-stored
         (length zombie-users)          ;arg 14
         zombie-users)))))              ;arg 15

(defun cli:get-user-points-action ()
  "Save user points of presentation project into a GeoJSON file."
  (cli:with-options (:database t :log t) (json-file get-user-points)
    (let ((presentation-project get-user-points))
      (assert-presentation-project presentation-project)
      (multiple-value-bind (user-points user-point-count)
          (get-user-points (user-point-table-name presentation-project)
                           :indent t)
        (assert json-file ()
                "Don't know where to store.  Try option --json-file")
        (unless (zerop user-point-count)
          (with-open-file (stream json-file
                                  :direction :output
                                  :if-exists :supersede)
            (princ user-points stream)))
        (cl-log:log-message
         :db-dat
         "~[There are no user points to get from presentation project ~A in ~
         database ~A at ~A:~D.  Didn't touch any file.~
         ~:;~:*Saved ~D user point~:P from presentation project ~A in ~
         database ~A at ~A:~D into file ~A.~]"
         user-point-count
         presentation-project database host port
         (ignore-errors (truename json-file)))))))
    
(defun cli:create-user-action ()
  "Define a new user."
  (cli:with-options (:database t :log t)
      ((presentation-project)
       user-full-name user-role user-password
       create-user)
    (let ((presentation-project-user create-user)
          fresh-user-p)
      (mapcar #'assert-presentation-project presentation-project)
      (setf fresh-user-p
            (create-user presentation-project-user
                         :presentation-projects presentation-project
                         :user-password user-password
                         :user-full-name user-full-name
                         :user-role user-role))
      (cl-log:log-message
       :db-dat ;TODO: We're listing nonexistent p-projects here as well.
       "~:[Updated~;Created~] user ~A (~A) who has ~A access ~
       to ~:[no ~;~]presentation project(s)~:*~{ ~A~#^,~} ~
       in database ~A at ~A:~D."
       fresh-user-p presentation-project-user
       user-full-name user-role
       presentation-project database host port))))

(defun cli:delete-user-action ()
  "Delete a presentation project user."
  (cli:with-options (:database t :log t) (delete-user)
    (let* ((presentation-project-user delete-user)
           (user-did-exist-p (delete-user presentation-project-user)))
      (cl-log:log-message
       :db-dat
       "~:[Tried to delete nonexistent~;Deleted~] ~
       presentation project user ~A from database ~A at ~A:~D."
       user-did-exist-p presentation-project-user database host port))))

(defun cli:list-user-action ()
  "List presentation project users together with their presentation
projects."
  (cli:with-options (:database t) (list-user)
    (let* ((presentation-project-user (if (string= list-user "*")
                                          'user-name
                                          list-user))
           (content
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
              'user-name))))
      (cli:format-table *standard-output* content
                        '("User" "ID" "Password" "Full Name"
                          "Presentation Project" "ID" "Role")))))

(defun cli:list-presentation-project-action ()
  "List content of presentation projects."
  (cli:with-options (:database t) (list-presentation-project)
    (let* ((presentation-project (if (string= list-presentation-project "*")
                                     'presentation-project-name
                                     list-presentation-project))
           (content
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
               :where
               (:and (:= 'sys-presentation-project.presentation-project-id
                         'sys-presentation.presentation-project-id)
                     (:= 'sys-presentation.measurement-id
                         'sys-measurement.measurement-id)
                     (:= 'sys-measurement.acquisition-project-id
                         'sys-acquisition-project.acquisition-project-id)
                     (:= 'presentation-project-name
                         presentation-project)))
              'presentation-project-name
              'sys-presentation.measurement-id))))
      (cli:format-table *standard-output* content
                        '("Presentation Project" "ID" "Meas. ID"
                          "Acquisition Project" "ID")))))

(defun cli:format-table (destination content column-headers &key
                         (column-separator " | ")
                         (header-separator #\-)
                         (column-widths (mapcar (constantly nil)
                                                column-headers)))
  "Print content (a list of lists) to destination."
  (let* ((rows (append (list column-headers)
                       (list (mapcar (constantly "") column-headers))
                       content))
         (number-of-rows (length column-headers))
         (widths
          (loop
             for column from 0 below number-of-rows
             collect (or (nth column column-widths)
                         (loop
                            for row in rows
                            maximize (length (format nil "~A" (nth column row))))))))
    (setf (second rows)
          (loop
             for width in widths collect
             (make-string width :initial-element header-separator)))
    (setf rows
          (loop
             for row in rows
             for i from 0
             nconc (cli:split-last-row (list row) widths)))
    (loop 
       for row in rows do
       (format destination "~&~{~VA~1,#^~A~}~%"
               (loop
                  for width in widths and field in row
                  collect width collect field collect column-separator)))))

(defun cli:split-last-row (rows column-widths)
  "If necessary, split fields of the last element of rows whose width
exceeds the respective column-width over multiple rows."
  (let ((last-row (mapcar #'(lambda (x) (format nil "~A" x))
                          (car (last rows)))))
    (if (notany #'(lambda (field width) (> (length field) width))
                last-row
                column-widths)
        rows
        (loop
           for field in last-row
           for column-width in column-widths
           collect (subseq field 0 (min column-width (length field)))
           into penultimate-row
           collect (subseq field (min column-width (length field)))
           into lowest-row
           finally (return (nconc (butlast rows)
                                  (list penultimate-row)
                                  (cli:split-last-row (list lowest-row)
                                                  column-widths)))))))

(defun cli:server-action ()
  "Start the HTTP server."
  (cli:with-options (:log t)
      (host port database user password use-ssl
            proxy-root http-port address common-root)
    (cli:with-options (:tolerate-missing t)
        (aux-host aux-port aux-database aux-user aux-password aux-use-ssl)
      (setf *postgresql-credentials*
            (list database user password host :port port
                  :use-ssl (s-sql:from-sql-name use-ssl)))
      (setf *postgresql-aux-credentials*
            (if (and aux-user aux-password aux-database)
                (list aux-database aux-user aux-password aux-host
                      :port aux-port
                      :use-ssl (s-sql:from-sql-name aux-use-ssl))
                *postgresql-credentials*))
      (insert-all-footprints *postgresql-credentials*)
      (delete-all-imageless-points *postgresql-credentials*)
      (setf hunchentoot:*log-lisp-backtraces-p*
            (cli:verbosity-level :log-error-backtraces))
      (setf hunchentoot:*show-lisp-errors-p*
            (cli:verbosity-level :show-server-errors))
      (setf *ps-print-pretty*
            (cli:verbosity-level :pretty-javascript))
      (start-server :proxy-root proxy-root
                    :http-port http-port
                    :address (unless (string= address "*") address)
                    :common-root common-root)
      (cl-log:log-message
       :info
       "HTTP server listens on port ~D ~
     of ~:[all available addresses~;address ~:*~A~].  ~
     It expects to be called with a URL path root of /~A/.  ~
     Phoros database is ~A on ~A:~D.  Auxiliary database is ~A on ~A:~D.  ~
     Files are searched for in ~A."
       http-port address
       proxy-root
       database host port
       aux-database aux-host aux-port
       common-root)
      (loop (sleep 10)))))
