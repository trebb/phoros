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

(in-package :phoros)

(setf *js-target-version* 1.8)

;;; Debug helpers.  TODO: remove them.
(defparameter *t* nil)
(defparameter *tt* nil)

(cffi:define-foreign-library phoml
  (:unix (:or "./libphoml.so"
              "./phoml/lib/libphoml.so"))
  (t (:default "libphoml")))

(defparameter *standard-coordinates* 4326
  "EPSG code of the coordinate system that we use for communication.")

(defparameter *postgresql-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl)")

(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))

(setf *read-default-float-format* 'double-float)

(defparameter *phoros-server* nil "Hunchentoot acceptor.")

(defparameter *common-root* nil
  "Root directory; contains directories of measuring data.")

(defparameter *verbose* 0
  "Integer (interpreted as a bit mask) denoting various kinds of
  debugging output.")

(defparameter *use-multi-file-openlayers* nil
  "If t, use OpenLayers uncompiled from openlayers/*, which makes
  debugging easier.  Otherwise use a single-file shrunk
  ol/Openlayers.js.")

(defparameter *number-of-images* 4
  "Number of photos shown to the HTTP client.")

(defparameter *number-of-features-per-layer* 500
  "What we think a browser can swallow.")

(defun check-db (db-credentials)
  "Check postgresql connection.  Return t if successful; show error on
*error-output* otherwise.  db-credentials is a list like so: (database
user password host &key (port 5432) use-ssl)."
  (let (connection)
    (handler-case
        (setf connection (apply #'connect db-credentials))
      (error (e) (format *error-output* "Database connection ~S failed: ~A~&"
                         db-credentials e)))
    (when connection
      (disconnect connection)
      t)))

(defmethod hunchentoot:session-cookie-name (acceptor)
  (declare (ignore acceptor))
  "phoros-session")

(defun start-server (&key (server-port 8080) (common-root "/"))
  (setf *phoros-server* (make-instance 'hunchentoot:acceptor :port server-port))
  (setf *session-max-time* (* 3600 24))
  (setf *common-root* common-root)
  (setf *show-lisp-errors-p* (logbitp 16 *verbose*))
  (setf *ps-print-pretty* (logbitp 15 *verbose*))
  (setf *use-multi-file-openlayers* (logbitp 14 *verbose*))
  ;; Doesn't seem to exist(setf *show-lisp-backtraces-p* t)  ;TODO: tie this to --debug option
  (setf *message-log-pathname* "hunchentoot-messages.log") ;TODO: try using cl-log
  (setf *access-log-pathname* "hunchentoot-access.log") ;TODO: try using cl-log
  (check-db *postgresql-credentials*)
  (with-connection *postgresql-credentials*
    (assert-phoros-db-major-version))
  (hunchentoot:start *phoros-server*))

(defun stop-server () (hunchentoot:stop *phoros-server*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-sql-operators :2+-ary :&& :overlaps))

(define-easy-handler phoros-handler ()
  "First HTTP contact: if necessary, check credentials, establish new
session."
  (with-connection *postgresql-credentials*
    (let* ((presentation-project-name
            (second (cl-utilities:split-sequence #\/ (script-name*) :remove-empty-subseqs t)))
           (presentation-project-id
            (ignore-errors
              (query
               (:select 'presentation-project-id
                        :from 'sys-presentation-project
                        :where (:= 'presentation-project-name presentation-project-name))
               :single))))
      (cond
        ((null presentation-project-id) "No such project.") ;TODO: send appropriate http error code
        ((and (equal (session-value 'presentation-project-name) presentation-project-name)
              (session-value 'authenticated-p))
         (redirect "/phoros-lib/view" :add-session-id t))
        (t
         (progn
           (setf (session-value 'presentation-project-name)
                 presentation-project-name)
           (setf (session-value 'presentation-project-id)
                 presentation-project-id)
           (setf (session-value 'presentation-project-bbox)
                 (presentation-project-bbox presentation-project-id))
           (who:with-html-output-to-string (s nil :prologue t :indent t)
             (:form :method "post" :enctype "multipart/form-data"
                    :action "/phoros-lib/authenticate"
                    "User:" :br
                    (:input :type "text" :name "user-name") :br
                    "Password:" :br
                    (:input :type "password" :name "user-password") :br
                    (:input :type "submit" :value "Submit")))))))))
      
(pushnew (create-prefix-dispatcher "/phoros/" 'phoros-handler)
         *dispatch-table*)

(define-easy-handler
    (authenticate-handler :uri "/phoros-lib/authenticate"
                          :default-request-type :post)
    ()
  "Check user credentials."
  (with-connection *postgresql-credentials*
    (let* ((user-name (post-parameter "user-name"))
           (user-password (post-parameter "user-password"))
           (presentation-project-id (session-value 'presentation-project-id))
           (user-info
            (when presentation-project-id
              (query
               (:select
                'sys-user.user-full-name
                'sys-user.user-id
                'sys-user-role.user-role
                :from 'sys-user-role 'sys-user
                :where (:and
                        (:= 'presentation-project-id presentation-project-id)
                        (:= 'sys-user-role.user-id 'sys-user.user-id)
                        (:= 'user-name user-name)
                        (:= 'user-password user-password)))
               :row)))
           (user-full-name (first user-info))
           (user-id (second user-info))
           (user-role (third user-info)))
      (if user-role
          (progn
            (setf (session-value 'authenticated-p) t
                  (session-value 'user-name) user-name
                  (session-value 'user-full-name) user-full-name
                  (session-value 'user-id) user-id
                  (session-value 'user-role) user-role)            
            (redirect "/phoros-lib/view" :add-session-id t))
          "Rejected."))))

(define-easy-handler logout-handler ()
  (if (session-verify *request*)
      (progn (remove-session *session*)
             "Bye.")
      "Bye (again)."))

(pushnew (create-regex-dispatcher "/logout" 'logout-handler)
         *dispatch-table*)

(define-easy-handler
    (local-data :uri "/phoros-lib/local-data" :default-request-type :post)
    ()
  "Receive coordinates, respond with the count nearest json objects
containing picture url, calibration parameters, and car position,
wrapped in an array."
  (when (session-value 'authenticated-p)
    (let* ((presentation-project-id (session-value 'presentation-project-id))
           (common-table-names (common-table-names presentation-project-id))
           (data (json:decode-json-from-string (raw-post-data)))
           (longitude-input (cdr (assoc :longitude data)))
           (latitude-input (cdr (assoc :latitude data)))
           (count (cdr (assoc :count data)))
           (zoom-input (cdr (assoc :zoom data)))
           ;;(snap-distance (* 10d-5 (expt 2 (- 18 zoom-input)))) ; assuming geographic coordinates
           (snap-distance (* 10d-1 (expt 2 (- 18 zoom-input)))) ; assuming geographic coordinates
           (point-form
            (format nil "POINT(~F ~F)" longitude-input latitude-input))
           (result
            (ignore-errors
              (with-connection *postgresql-credentials*
                (loop
                   for common-table-name in common-table-names
                   nconc
                     (query
                      (:limit
                       (:order-by
                        (:select
                         'date          ;TODO: debug only
                         'measurement-id 'recorded-device-id 'device-stage-of-life-id ;TODO: debug only
                         'directory
                         'filename 'byte-position 'point-id
                         'trigger-time
                         ;'coordinates   ;the search target
                         'longitude 'latitude 'ellipsoid-height
                         'cartesian-system
                         'east-sd 'north-sd 'height-sd
                         'roll 'pitch 'heading 'roll-sd 'pitch-sd 'heading-sd
                         'sensor-width-pix 'sensor-height-pix 'pix-size
                         'mounting-angle
                         'dx 'dy 'dz 'omega 'phi 'kappa
                         'c 'xh 'yh 'a1 'a2 'a3 'b1 'b2 'c1 'c2 'r0
                         'b-dx 'b-dy 'b-dz 'b-rotx 'b-roty 'b-rotz
                         'b-ddx 'b-ddy 'b-ddz 'b-drotx 'b-droty 'b-drotz
                         :from
                         (aggregate-view-name common-table-name)
                         :where
                         (:and (:= 'presentation-project-id presentation-project-id)
                               (:st_dwithin 'coordinates
                                            (:st_geomfromtext point-form *standard-coordinates*)
                                            snap-distance)))
                        (:st_distance 'coordinates
                                      (:st_geomfromtext point-form *standard-coordinates*)))
                       count)
                      :alists))))))
      (json:encode-json-to-string result))))

(define-easy-handler
    (store-point :uri "/phoros-lib/store-point" :default-request-type :post)
    ()
  "Receive point sent by user; store it into database."
  (when (session-value 'authenticated-p)
    (let* ((presentation-project-name (session-value 'presentation-project-name))
           (user-id (session-value 'user-id))
           (user-role (session-value 'user-role))
           (data (json:decode-json-from-string (raw-post-data)))
           (longitude-input (cdr (assoc :longitude data)))
           (latitude-input (cdr (assoc :latitude data)))
           (ellipsoid-height-input (cdr (assoc :ellipsoid-height data)))
           (stdx-global (cdr (assoc :stdx-global data)))
           (stdy-global (cdr (assoc :stdy-global data)))
           (stdz-global (cdr (assoc :stdz-global data)))
           (attribute (cdr (assoc :attribute data)))
           (description (cdr (assoc :description data)))
           (numeric-description (cdr (assoc :numeric-description data)))
           (point-form
            (format nil "SRID=4326; POINT(~S ~S ~S)"
                    longitude-input latitude-input ellipsoid-height-input))
           (user-point-table-name
            (user-point-table-name presentation-project-name)))
      (assert
       (not (string-equal user-role "read")) ;that is, "write" or "admin"
       () "No write permission.")
      (with-connection *postgresql-credentials*
        (assert
         (= 1 (execute (:insert-into user-point-table-name :set
                                     'user-id user-id
                                     'attribute attribute
                                     'description description
                                     'numeric-description numeric-description
                                     'creation-date 'current-timestamp
                                     'coordinates (:st_geomfromewkt point-form)
                                     'stdx-global stdx-global
                                     'stdy-global stdy-global
                                     'stdz-global stdz-global
                                     )))
         () "No point stored.  This should not happen.")))))

(define-easy-handler
    (update-point :uri "/phoros-lib/update-point" :default-request-type :post)
    ()
  "Update point sent by user in database."
  (when (session-value 'authenticated-p)
    (let* ((presentation-project-name (session-value 'presentation-project-name))
           (user-id (session-value 'user-id))
           (user-role (session-value 'user-role))
           (data (json:decode-json-from-string (raw-post-data)))
           (user-point-id (cdr (assoc :user-point-id data)))
           (attribute (cdr (assoc :attribute data)))
           (description (cdr (assoc :description data)))
           (numeric-description (cdr (assoc :numeric-description data)))
           (user-point-table-name
            (user-point-table-name presentation-project-name)))
      (assert
       (not (string-equal user-role "read")) ;that is, "write" or "admin"
       () "No write permission.")
      (with-connection *postgresql-credentials*
        (assert
         (= 1 (execute (:update user-point-table-name :set
                                'attribute attribute
                                'description description
                                'numeric-description numeric-description
                                'creation-date 'current-timestamp
                                :where (:and (:= 'user-point-id user-point-id)
                                             (:= (if (string-equal user-role "admin")
                                                     user-id
                                                     'user-id)
                                                 user-id)))))
         () "No point stored.  Did you try to update someone else's point without having admin permission?")))))

(define-easy-handler
    (delete-point :uri "/phoros-lib/delete-point" :default-request-type :post)
    ()
  "Delete user point if user is allowed to do so."
  (when (session-value 'authenticated-p)
    (let* ((presentation-project-name (session-value 'presentation-project-name))
           (user-id (session-value 'user-id))
           (user-role (session-value 'user-role))
           (user-point-table-name
            (user-point-table-name presentation-project-name))
           (data (json:decode-json-from-string (raw-post-data))))
      (with-connection *postgresql-credentials*
        (assert
         (eql 1 (cond ((string-equal user-role "admin")
                     (execute (:delete-from user-point-table-name
                                            :where (:= 'user-point-id data))))
                    ((string-equal user-role "write")
                     (execute (:delete-from user-point-table-name
                                            :where (:and
                                                    (:= 'user-point-id data)
                                                    (:= 'user-id user-id)))))))
            () "No point deleted.  This should not happen.")))))


(defun common-table-names (presentation-project-id)
  "Return a list of common-table-names of table sets that contain data
of presentation project with presentation-project-id."
  (handler-case
      (with-connection *postgresql-credentials*
        (query
         (:select 'common-table-name
                  :distinct
                  :from 'sys-presentation 'sys-measurement 'sys-acquisition-project
                  :where (:and
                          (:= 'sys-presentation.presentation-project-id presentation-project-id)
                          (:= 'sys-presentation.measurement-id 'sys-measurement.measurement-id)
                          (:= 'sys-measurement.acquisition-project-id 'sys-acquisition-project.acquisition-project-id)))
               :column))
    (condition (c)
      (cl-log:log-message
       :server
       "While fetching common-table-names of presentation-project-id ~D: ~A"
       presentation-project-id c))))

(defun encode-geojson-to-string (features &rest junk-keys)
  "Encode a list of property lists into a GeoJSON FeatureCollection.
Each property list must contain keys for coordinates, :x, :y, :z; and
for a numeric point :id, followed by zero or more pieces of extra
information.  The extra information is stored as GeoJSON Feature
properties.  Exclude property list elements with keys that are in
junk-keys."
  (with-output-to-string (s)
    (json:with-object (s)
      (json:encode-object-member :type :*feature-collection s)
      (json:as-object-member (:features s) 
        (json:with-array (s)
          (mapcar
           #'(lambda (point-with-properties)
               (dolist (junk-key junk-keys)
                 (remf point-with-properties junk-key))
               (destructuring-bind (&key x y z id &allow-other-keys) ;TODO: z probably bogus
                   point-with-properties
                 (json:as-array-member (s)
                   (json:with-object (s)
                     (json:encode-object-member :type :*feature s)
                     (json:as-object-member (:geometry s)
                       (json:with-object (s)
                         (json:encode-object-member :type :*point s)
                         (json:as-object-member (:coordinates s)
                           (json:encode-json (list x y z) s))))
                     (json:encode-object-member :id id s)
                     (json:as-object-member (:properties s)
                       (dolist (key '(:x :y :z :id))
                         (remf point-with-properties key))
                       (json:encode-json-plist point-with-properties s))))))
           features))))))

(defun box3d (bbox)
  "Return a WKT-compliant BOX3D string from string bbox."
  (concatenate 'string "BOX3D("
               (substitute #\Space #\,
                           (substitute #\Space #\, bbox :count 1)
                           :from-end t :count 1)
               ")"))

(define-easy-handler (points :uri "/phoros-lib/points") (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (when (session-value 'authenticated-p)
    (handler-case 
        (let* ((presentation-project-id (session-value 'presentation-project-id))
               (common-table-names
                (common-table-names presentation-project-id)))
          (encode-geojson-to-string
           (with-connection *postgresql-credentials*
             (query
              (sql-compile
               `(:limit
                 (:order-by
                  (:union
                   ,@(loop
                        for common-table-name in common-table-names
                        for aggregate-view-name
                        = (aggregate-view-name common-table-name)
                        collect
                        `(:select
                          (:as
                           (:st_x
                            (:st_transform 'coordinates ,*standard-coordinates*))
                           x)
                          (:as
                           (:st_y
                            (:st_transform 'coordinates ,*standard-coordinates*))
                           y)
                          (:as
                           (:st_z
                            (:st_transform 'coordinates ,*standard-coordinates*))
                           z)
                          (:as 'point-id 'id) ;becomes fid on client
                          (:as (:random) random)
                          :from ',aggregate-view-name
                          :natural :left-join 'sys-presentation
                          :where
                          (:and
                           (:= 'presentation-project-id ,presentation-project-id)
                           (:&&
                            (:st_transform 'coordinates ,*standard-coordinates*)
                            (:st_setsrid  (:type ,(box3d bbox) box3d)
                                          ,*standard-coordinates*))))))
                  random)
                 ,*number-of-features-per-layer*))
              :plists))
           :random))
      (condition (c)
        (cl-log:log-message
         :server "While fetching points from inside bbox ~S: ~A"
         bbox c)))))

(defun presentation-project-bbox (presentation-project-id)
  "Return bounding box of the entire presentation-project as a string
  \"x1,y1,x2,y2\"."
  (let* ((common-table-names
          (common-table-names presentation-project-id)))
    (with-connection *postgresql-credentials*
      (substitute
       #\, #\Space
       (string-trim
        "BOX()"
        (query
         (sql-compile
          `(:select
            (:st_extent (:st_transform 'coordinates ,*standard-coordinates*))
                    :from
                    (:as (:union
                          ,@(loop
                               for common-table-name in common-table-names
                               for aggregate-view-name
                               = (aggregate-view-name common-table-name)
                               collect
                               `(:select
                                 'coordinates
                                 :from ',aggregate-view-name
                                 :natural :left-join 'sys-presentation
                                 :where
                                 (:= 'presentation-project-id
                                     ,presentation-project-id))))
                         all-coordinates)))
         :single!))))))

(define-easy-handler (user-points :uri "/phoros-lib/user-points") (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (when (session-value 'authenticated-p)
    (handler-case 
        (let ((user-point-table-name
               (user-point-table-name (session-value 'presentation-project-name))))
          (encode-geojson-to-string
           (with-connection *postgresql-credentials*
             (query
              (:limit
               (:order-by
                (:select
                 (:as
                  (:st_x (:st_transform 'coordinates *standard-coordinates*))
                  'x)
                 (:as
                  (:st_y (:st_transform 'coordinates *standard-coordinates*))
                  'y)
                 (:as
                  (:st_z (:st_transform 'coordinates *standard-coordinates*))
                  'z)
                 (:as 'user-point-id 'id) ;becomes fid on client
                 'attribute
                 'description
                 'numeric-description
                 'user-name
                 (:as (:to-char 'creation-date "IYYY-MM-DD HH24:MI:SS TZ")
                      'creation-date)
                 :from user-point-table-name :natural :left-join 'sys-user
                 :where (:&&
                         (:st_transform 'coordinates *standard-coordinates*)
                         (:st_setsrid  (:type (box3d bbox) box3d)
                                       *standard-coordinates*)))
                (:random))
               *number-of-features-per-layer*)
              :plists))))
      (condition (c)
        (cl-log:log-message
         :server "While fetching user-points from inside bbox ~S: ~A"
         bbox c)))))

(define-easy-handler photo-handler
    ((bayer-pattern :init-form "#00ff00,#ff0000")
     (color-raiser :init-form "1,1,1"))
  "Serve an image from a .pictures file."
  (when (session-value 'authenticated-p)
    (handler-case
        (let* ((s (cdr (cl-utilities:split-sequence #\/ (script-name*)
                                                    :remove-empty-subseqs t)))
               (directory (last (butlast s 2)))
               (file-name-and-type (cl-utilities:split-sequence
                                    #\. (first (last s 2))))
               (byte-position (parse-integer (car (last s)) :junk-allowed t))
               (path-to-file
                (car
                 (directory
                  (make-pathname
                   :directory (append (pathname-directory *common-root*)
                                      directory '(:wild-inferiors))
                   :name (first file-name-and-type)
                   :type (second file-name-and-type)))))
               stream)
          (setf (content-type*) "image/png")
          (setf stream (send-headers))
          (send-png stream path-to-file byte-position
                    :bayer-pattern (canonicalize-bayer-pattern bayer-pattern)
                    :color-raiser (canonicalize-color-raiser color-raiser)))
      (condition (c)
        (cl-log:log-message
         :server "While serving image ~S: ~A" (request-uri*) c)))))

(pushnew (create-prefix-dispatcher "/phoros-lib/photo" 'photo-handler)
         *dispatch-table*)

;;; for debugging; this is the multi-file OpenLayers
(pushnew (create-folder-dispatcher-and-handler
          "/phoros-lib/openlayers/" "OpenLayers-2.10/")
         *dispatch-table*)

(pushnew (create-folder-dispatcher-and-handler "/phoros-lib/ol/" "ol/")
         *dispatch-table*)

(pushnew (create-folder-dispatcher-and-handler "/phoros-lib/css/" "css/") ;TODO: merge this style.css into public_html/style.css
         *dispatch-table*)

(pushnew (create-folder-dispatcher-and-handler
          "/phoros-lib/public_html/" "public_html/")
         *dispatch-table*)

(pushnew (create-static-file-dispatcher-and-handler
          "/favicon.ico" "public_html/favicon.ico")
         *dispatch-table*)

(define-easy-handler (phoros.js :uri "/phoros-lib/phoros.js") ()
  "Serve some Javascript."
  (when (session-value 'authenticated-p)
    (ps
       
      (setf debug-info (@ *open-layers *console info))

      (defmacro inner-html-with-id (id)
        "innerHTML of element with id=\"id\"."
        `(chain document (get-element-by-id ,id) inner-h-t-m-l))

      (defvar *help-topics*
        (create
         :user-role
         (who-ps-html (:p "User role.  \"Read\" can't write anything.  \"Write\" may write user points and delete their own ones. \"Admin\" may write user points and delete points written by others."))
         :presentation-project-name
         (who-ps-html (:p "Presentation project name."))
         :h2-controls
         (who-ps-html (:p "Next action."))
         :finish-point-button
         (who-ps-html (:p "Store point with its attribute, description and numeric description into database.  Afterwards, increment the numeric description if possible."))
         :delete-point-button
         (who-ps-html (:p "Delete current point."))
         :point-attribute
         (who-ps-html (:p "One of a few possible point attributes.")
                      (:p "TODO: currently only the hard-coded ones are available."))
         :point-description
         (who-ps-html (:p "Optional verbal description of point."))
         :point-numeric-description
         (who-ps-html (:p "Optional additional description of point.  Preferrably numeric and if so, automatically incremented after finishing point."))
         :point-creation-date
         (who-ps-html (:p "Creation date of current point.  Will be updated when you change this point."))
         :creator
         (who-ps-html (:p "Creator of current point.  Will be updated when you change this point."))
         :remove-work-layers-button
         (who-ps-html (:p "Discard the current, unstored point but let the rest of the workspace untouched."))
         :blurb-button
         (who-ps-html (:p "View some info about phoros."))
         :logout-button
         (who-ps-html (:p "Finish this session.  Fresh login is required to continue."))
         :streetmap
         (who-ps-html (:p "Clicking into the streetmap fetches images which most probably feature the clicked point.")
                      (:p "TODO: This is not quite so.  Currently images taken from points nearest to the clicked one are displayed.")
                      (:p "To pan the map, drag the mouse.  To zoom, spin the mouse wheel or hold shift down whilst dragging a box."))
         :image
         (who-ps-html (:p "Clicking into an image sets or resets the active point there.  Once a feature is marked by active points in more than one image, the estimated position is calculated.")
                      (:p "To pan an image, drag the mouse.  To zoom, spin the mouse wheel or hold shift down whilst dragging a box."))
         ol-Control-Pan-West-Item-Inactive
         (who-ps-html (:p "Move viewport left."))
         ol-Control-Pan-East-Item-Inactive
         (who-ps-html (:p "Move viewport right."))
         ol-Control-Pan-North-Item-Inactive
         (who-ps-html (:p "Move viewport up."))
         ol-Control-Pan-South-Item-Inactive
         (who-ps-html (:p "Move viewport down."))
         ol-Control-Zoom-In-Item-Inactive
         (who-ps-html (:p "Zoom in."))
         ol-Control-Zoom-Out-Item-Inactive
         (who-ps-html (:p "Zoom out."))
         streetmap-Zoom-To-Max-Extent-Item-Inactive
         (who-ps-html (:p "Zoom to the extent of presentation project."))
         ol-Control-Zoom-To-Max-Extent-Item-Inactive
         (who-ps-html (:p "Zoom out completely, restoring the original view."))
         :image-layer-switcher
         (who-ps-html (:p "Toggle display of image."))
         :streetmap-layer-switcher
         (who-ps-html (:p "Toggle visibility of data layers, or choose a background streetmap. (TODO: currently only one \"choice\")"))
         :streetmap-overview
         (who-ps-html (:p "Click to re-center streetmap, or drag the red rectangle."))
         :help-display
         (who-ps-html (:p "Hints on Phoros' displays and controls are shown here while hovering over the respective elements."))))

      (defun add-help-topic (topic element)
        "Add mouse events to DOM element that initiate display of a
help message."
        (when element
          (setf (@ element onmouseover)
                ((lambda (x)
                   (lambda () (show-help x)))
                 topic))
          (setf (@ element onmouseout) show-help)))

      (defun add-help-events ()
        "Add mouse events to DOM elements that initiate display of a
help message."
        (for-in
         (topic *help-topics*)
         (add-help-topic topic (chain document (get-element-by-id topic)))
         (dolist (element (chain document (get-elements-by-class-name topic)))
           (add-help-topic topic element))))
          
      (defun show-help (&optional topic)
        "Put text on topic into help-display"
        (setf (inner-html-with-id "help-display")
              (+ (who-ps-html (:h2 "Help"))
                 (let ((help-body (getprop *help-topics* topic)))
                   (if (undefined help-body)
                       ""
                       help-body)))))
      
      (defvar *click-control*
        (chain
         *open-layers
         (*class
          (@ *open-layers *control)
          (create
           :default-handler-options
           (create :single t
                   :double false
                   :pixel-tolerance 0
                   :stop-single false
                   :stop-double false)
           :initialize
           (lambda (options)
             (setf 
              (@ this handler-options)
              (chain *open-layers
                     *util
                     (extend
                      (create)
                      (@ this default-handler-options))))
             (chain *open-layers
                    *control
                    prototype
                    initialize
                    (apply this arguments))
             (setf (@ this handler)
                   (new (chain *open-layers
                               *handler
                               (*click this
                                       (create
                                        :click (@ this trigger))
                                       (@ this handler-options))))))))))

      (defvar +geographic+
        (new (chain *open-layers (*projection "EPSG:4326"))))
      (defvar +spherical-mercator+
        (new (chain *open-layers (*projection "EPSG:900913"))))

      (defvar +user-name+ (lisp (session-value 'user-name))
        "User's (short) name")
      (defvar +user-role+ (lisp (string-downcase (session-value 'user-role)))
        "User's permissions")

      (defvar +presentation-project-bounds+ 
        (chain (new (chain *open-layers
                           *bounds
                           (from-string
                            (lisp (session-value 'presentation-project-bbox)))))
               (transform +geographic+ +spherical-mercator+))
        "Bounding box of the entire presentation project.")

      (defvar *images* (array) "Collection of the photos currently shown.")
      (defvar *streetmap* undefined
        "The streetmap shown to the user.")
      (defvar *streetmap-estimated-position-layer*)
      (defvar *point-attributes-select* undefined
        "The HTML element for selecting user point attributes.")

      (defvar *global-position*
        "Coordinates of the current estimated position")

      (defvar *bbox-strategy* (chain *open-layers *strategy *bbox*))
      (setf (chain *bbox-strategy* prototype ratio) 1.5)
      (setf (chain *bbox-strategy* prototype res-factor) 1.5)

      (defvar *json-parser* (new (chain *open-layers *format *json*)))

      (defvar *geojson-format* (chain *open-layers *format *geo-j-s-o-n))
      (setf (chain *geojson-format* prototype ignore-extra-dims) t) ;doesn't handle height anyway
      (setf (chain *geojson-format* prototype external-projection) +geographic+)
      (setf (chain *geojson-format* prototype internal-projection) +geographic+)

      (defvar *http-protocol* (chain *open-layers *protocol *http*))
      (setf (chain *http-protocol* prototype format) (new *geojson-format*))
      
      (defvar *survey-layer*
        (new (chain
              *open-layers *layer
              (*vector
               "Survey"
               (create
                strategies (array (new (*bbox-strategy*)))
                protocol
                (new (*http-protocol*
                      (create :url "/phoros-lib/points"))))))))

      (defvar *user-point-layer*
        (new (chain
              *open-layers *layer
              (*vector
               "User Points"
               (create
                strategies (array (new *bbox-strategy*))
                protocol
                (new (*http-protocol*
                      (create :url "/phoros-lib/user-points"))))))))
      
      (defvar *pristine-images-p* t
        "T if none of the current images has been clicked into yet.")
      
      (defvar *current-user-point* undefined
        "The currently selected user-point.")

      (defvar *user-points-select-control*
        (new (chain *open-layers *control (*select-feature *user-point-layer*))))
      ;;(defvar google (new ((@ *open-layers *Layer *google) "Google Streets")))
      (defvar *osm-layer* (new (chain *open-layers *layer (*osm*))))
      (defvar *click-streetmap*
        (new (*click-control* (create :trigger request-photos))))

      (defun write-permission-p (&optional (current-owner +user-name+))
        "Nil if current user can't edit stuff created by current-owner or, without arguments, new stuff."
        (or (== +user-role+ "admin")
            (and (== +user-role+ "write")
                 (== +user-name+ current-owner))))
      
      (defun *image ()
        "Anything necessary to deal with a photo."
        (setf (getprop this 'map)
              (new ((getprop *open-layers '*map)
                    (create projection +spherical-mercator+
                            all-overlays t
                            controls (array (new (chain *open-layers
                                                        *control
                                                        (*navigation))))))))
        (setf (getprop this 'dummy) false) ;TODO why? (omitting splices map components directly into *image)
              )

      (setf (getprop *image 'prototype 'show-photo) show-photo)
      (setf (getprop *image 'prototype 'draw-epipolar-line) draw-epipolar-line)
      (setf (getprop *image 'prototype 'draw-active-point) draw-active-point)
      (setf (getprop *image 'prototype 'draw-estimated-positions)
            draw-estimated-positions)

      (defun photo-path (photo-parameters)
        "Create from stuff found in photo-parameters a path for use in
an image url."
        (+ "/phoros-lib/photo/" (@ photo-parameters directory) "/"
           (@ photo-parameters filename) "/"
           (@ photo-parameters byte-position) ".png"))

      (defun has-layer-p (map layer-name)
        "False if map doesn't have a layer called layer-name."
        (chain map (get-layers-by-name layer-name) length))

      (defun some-active-point-p ()
        "False if no image in *images* has an Active Point."
        (loop
           for i across *images*
           sum (has-layer-p (getprop i 'map) "Active Point")))

      (defun remove-layer (map layer-name)
        "Destroy layer layer-name in map."
        (when (has-layer-p map layer-name)
          (chain map (get-layers-by-name layer-name) 0 (destroy))))

      (defun remove-any-layers (layer-name)
        "Destroy in all *images* and in *streetmap* the layer named layer-name."
        (loop
           for i across *images* do (remove-layer (getprop i 'map) layer-name))
        (remove-layer *streetmap* layer-name))

      (defun reset-controls ()
        "Destroy user-generated layers in *streetmap* and in all *images*."
        (disable-element-with-id "finish-point-button")
        (disable-element-with-id "delete-point-button")
        (disable-element-with-id "remove-work-layers-button")
        (setf (inner-html-with-id "h2-controls") "Create Point")
        (setf (inner-html-with-id "creator") nil)
        (setf (inner-html-with-id "point-creation-date") nil))

      (defun reset-layers-and-controls ()
        (remove-any-layers "Epipolar Line")
        (remove-any-layers "Active Point")
        (remove-any-layers "Estimated Position")
        (remove-any-layers "User Point")
        (when (and (!= undefined *current-user-point*)
                   (chain *current-user-point* layer))
          (chain *user-points-select-control* (unselect *current-user-point*)))
        (reset-controls)
        (setf *pristine-images-p* t)
        )

      (defun enable-element-with-id (id)
        "Activate HTML element with id=\"id\"."
        (setf (chain document (get-element-by-id id) disabled) nil))

      (defun disable-element-with-id (id)
        "Grey out HTML element with id=\"id\"."
        (setf (chain document (get-element-by-id id) disabled) t))

      (defmacro value-with-id (id)
        "Value of element with id=\"id\"."
        `(chain document (get-element-by-id ,id) value))

      (defun refresh-layer (layer)
        "Have layer re-request and redraw features."
        (chain layer (refresh (create :force t))))

      (defun present-photos ()
        "Handle the response triggered by request-photos."
        (let ((photo-parameters
               (chain *json-parser*
                      (read (@ photo-request-response response-text)))))
          (loop
             for p across photo-parameters
             for i across *images*
             do
             (setf (getprop i 'photo-parameters) p)
             ((getprop i 'show-photo)))
          ;; (setf (@ (aref photo-parameters 0) angle180) 1) ; Debug: coordinate flipping
          ))

      (defun request-photos (event)
        "Handle the response to a click into *streetmap*; fetch photo data."
        (disable-element-with-id "finish-point-button")
        (disable-element-with-id "remove-work-layers-button")
        (remove-any-layers "Estimated Position")
        (let* ((lonlat
                ((@ ((@ *streetmap* get-lon-lat-from-pixel) (@ event xy)) transform)
                 +spherical-mercator+   ; why?
                 +geographic+))
               (content
                (chain *json-parser*
                       (write
                        (create :longitude (@ lonlat lon)
                                :latitude (@ lonlat lat)
                                :zoom ((@ *streetmap* get-zoom))
                                :count (lisp *number-of-images*))))))
          (setf photo-request-response
                ((@ *open-layers *Request *POST*)
                 (create :url "/phoros-lib/local-data"
                         :data content
                         :headers (create "Content-type" "text/plain"
                                          "Content-length" (@ content length))
                         :success present-photos)))))

      (defun draw-epipolar-line ()
        "Draw an epipolar line from response triggered by clicking
into a (first) photo."
        (enable-element-with-id "remove-work-layers-button")
        (let* ((epipolar-line
                (chain *json-parser*
                       (read
                        (@ this epipolar-request-response response-text))))
               (points
                (chain epipolar-line
                       (map (lambda (x)
                              (new ((@ *open-layers *geometry *point)
                                    (@ x :m) (@ x :n)))))))
               (feature
                (new (chain *open-layers
                            *feature
                            (*vector
                             (new (chain
                                   *open-layers
                                   *geometry
                                   (*line-string points))))))))
          (setf (chain feature render-intent) "temporary")
          (chain this epipolar-layer
                 (add-features feature))))
      ;; either *line-string or *multi-point are usable
      
      (defun draw-estimated-positions ()
        "Draw into streetmap and into all images points at Estimated
Position.  Estimated Position is the point returned so far from
photogrammetric calculations that are triggered by clicking into
another photo."
        (when (write-permission-p)
          (setf (chain document
                       (get-element-by-id "finish-point-button")
                       onclick)
                finish-point)
          (enable-element-with-id "finish-point-button"))
        (let* ((estimated-positions-request-response
                (chain *json-parser*
                       (read
                        (getprop this
                                 'estimated-positions-request-response
                                 'response-text))))
               (estimated-positions
                (aref estimated-positions-request-response 1)))
          (setf *global-position*
                (aref estimated-positions-request-response 0))
          (let ((feature
                 (new ((@ *open-layers *feature *vector)
                       ((@ (new ((@ *open-layers *geometry *point)
                                 (getprop *global-position* 'longitude)
                                 (getprop *global-position* 'latitude)))
                           transform) +geographic+ +spherical-mercator+)))))
            (setf (chain feature render-intent) "temporary")
            (setf *streetmap-estimated-position-layer*
                  (new (chain *open-layers
                              *layer
                              (*vector "Estimated Position"
                                       (create display-in-layer-switcher nil)))))
            (chain *streetmap-estimated-position-layer*
                   (add-features feature))
            (chain *streetmap* (add-layer *streetmap-estimated-position-layer*)))
          (let ((estimated-position-style
                 (create stroke-color (chain *open-layers *feature *vector
                                             style "temporary" stroke-color)
                         point-radius 9
                         fill-opacity 0)))
            (loop
               for i in *images*
               for p in estimated-positions
               do
               (when i   ;otherwise a photogrammetry error has occured
                 (setf (@ i estimated-position-layer)
                       (new
                        (chain *open-layers *layer
                               (*vector "Estimated Position"
                                        (create display-in-layer-switcher nil)))))
                 (setf (chain i estimated-position-layer style)
                       estimated-position-style)
                 (let* ((point
                         (new
                          (chain *open-layers *geometry (*point
                                                         (getprop p 'm)
                                                         (getprop p 'n)))))
                        (feature
                         (new
                          (chain *open-layers *feature (*vector point)))))
                   (chain i map
                          (add-layer (@ i estimated-position-layer)))
                   (chain i estimated-position-layer
                          (add-features feature))))))))

      (defun draw-user-point ()
        "Draw currently selected user point into all images."
        (let* ((user-point-in-images
                (chain *json-parser*
                       (read
                        (getprop *user-point-in-images-response*
                                 'response-text)))))
          (loop
             for i in *images*
             for p in user-point-in-images
             do
             (when i     ;otherwise a photogrammetry error has occured
               (setf (@ i user-point-layer)
                     (new (chain *open-layers
                                 *layer
                                 (*vector "User Point"
                                          (create display-in-layer-switcher nil)))))
               (let* ((point
                       (new (chain *open-layers *geometry (*point
                                                           (getprop p 'm)
                                                           (getprop p 'n)))))
                      (feature
                       (new (chain *open-layers *feature (*vector point)))))
                 (setf (chain feature render-intent) "select")
                 (chain i map (add-layer (@ i user-point-layer)))
                 (chain i user-point-layer (add-features feature)))))))

      (defun finish-point ()
        "Send current *global-position* as a user point to the database."
        (let ((global-position-etc *global-position*))
          (setf (chain global-position-etc attribute)
                (chain
                 (elt (chain *point-attributes-select* options)
                      (chain *point-attributes-select* options selected-index))
                 text))
          (setf (chain global-position-etc description)
                (value-with-id "point-description"))
          (setf (chain global-position-etc numeric-description)
                (value-with-id "point-numeric-description"))
          (let ((content 
                 (chain *json-parser*
                        (write global-position-etc))))
            ((@ *open-layers *Request *POST*)
             (create :url "/phoros-lib/store-point"
                     :data content
                     :headers (create "Content-type" "text/plain"
                                      "Content-length" (@ content length))
                     :success (lambda ()
                                (refresh-layer *user-point-layer*)
                                (reset-layers-and-controls)))))
          (let* ((previous-numeric-description ;increment if possible
                  (chain global-position-etc numeric-description))
                 (current-numeric-description
                  (1+ (parse-int previous-numeric-description 10))))
            (setf (value-with-id "point-numeric-description")
                  (if (is-finite current-numeric-description)
                      current-numeric-description
                      previous-numeric-description)))))

      (defun update-point ()
        "Send changes to currently selected user point to database."
        (let* ((point-data
                (create user-point-id (chain *current-user-point* fid)
                        attribute
                        (chain
                         (elt (chain *point-attributes-select* options)
                              (chain *point-attributes-select* options selected-index))
                         text)
                        description
                        (value-with-id "point-description")
                        numeric-description
                        (value-with-id "point-numeric-description")))
               (content 
                (chain *json-parser*
                       (write point-data))))
          ((@ *open-layers *Request *POST*)
           (create :url "/phoros-lib/update-point"
                   :data content
                   :headers (create "Content-type" "text/plain"
                                    "Content-length" (@ content length))
                   :success (lambda ()
                              (refresh-layer *user-point-layer*)
                              (reset-layers-and-controls))))))

      (defun delete-point ()
        "Purge currently selected user point from database."
        (let ((user-point-id (chain *current-user-point* fid)))
          (setf content 
                (chain *json-parser*
                       (write user-point-id)))
          ((@ *open-layers *Request *POST*)
           (create :url "/phoros-lib/delete-point"
                   :data content
                   :headers (create "Content-type" "text/plain"
                                    "Content-length" (@ content length))
                   :success (lambda ()
                              (refresh-layer *user-point-layer*)
                              (reset-layers-and-controls))))))
      
      (defun draw-active-point ()
        "Draw an Active Point, i.e. a point used in subsequent
photogrammetric calculations."
        (chain this active-point-layer
               (add-features
                (new ((@ *open-layers *feature *vector)
                      (new ((@ *open-layers *geometry *point)
                            (getprop this 'photo-parameters 'm)
                            (getprop this 'photo-parameters 'n))))))))

      (defun image-click-action (clicked-image)
        (lambda (event)
          "Do appropriate things when an image is clicked into."
          (let* ((lonlat
                  ((@ (@ clicked-image map) get-lon-lat-from-view-port-px)
                   (@ event xy)))
                 (photo-parameters
                  (getprop clicked-image 'photo-parameters))
                 pristine-image-p content request)
            (setf (@ photo-parameters m) (@ lonlat lon)
                  (@ photo-parameters n) (@ lonlat lat))
            (remove-layer (getprop clicked-image 'map) "Active Point")
            (remove-any-layers "Epipolar Line")
            (setf *pristine-images-p* (not (some-active-point-p)))
            (setf (@ clicked-image active-point-layer)
                  (new (chain *open-layers
                              *layer
                              (*vector "Active Point"
                                       (create display-in-layer-switcher nil)))))
            ((@ clicked-image map add-layer)
             (@ clicked-image active-point-layer))
            ((getprop clicked-image 'draw-active-point))
            (if
             *pristine-images-p*
             (progn
               (reset-controls)
               (remove-any-layers "User Point") ;from images
               (when (and (!= undefined *current-user-point*)
                          (chain *current-user-point* layer))
                 (chain *user-points-select-control* (unselect *current-user-point*)))
               (loop
                  for i across *images* do
                  (unless (== i clicked-image)
                    (setf
                     (@ i epipolar-layer)
                     (new (chain *open-layers
                                 *layer
                                 (*vector "Epipolar Line"
                                          (create display-in-layer-switcher nil))))
                     content (chain *json-parser*
                                    (write
                                     (append (array photo-parameters)
                                             (@ i photo-parameters))))
                     (@ i epipolar-request-response)
                     ((@ *open-layers *Request *POST*)
                      (create :url "/phoros-lib/epipolar-line"
                              :data content
                              :headers (create "Content-type" "text/plain"
                                               "Content-length"
                                               (@ content length))
                              :success (getprop i 'draw-epipolar-line)
                              :scope i)))
                    ((@ i map add-layer) (@ i epipolar-layer)))))
             (progn
               (remove-any-layers "Epipolar Line")
               (remove-any-layers "Estimated Position")
               (let* ((active-pointed-photo-parameters
                       (loop
                          for i across *images*
                          when (has-layer-p (getprop i 'map) "Active Point")
                          collect (getprop i 'photo-parameters)))
                      (content
                       (chain *json-parser*
                              (write
                               (list active-pointed-photo-parameters
                                     (chain *images*
                                            (map #'(lambda (x)
                                                     (getprop
                                                      x 'photo-parameters)))))))))
                 (setf (@ clicked-image estimated-positions-request-response)
                       ((@ *open-layers *Request *POST*)
                        (create :url "/phoros-lib/estimated-positions"
                                :data content
                                :headers (create "Content-type" "text/plain"
                                                 "Content-length"
                                                 (@ content length))
                                :success (getprop clicked-image
                                                  'draw-estimated-positions)
                                :scope clicked-image)))))))))

      (defun show-photo ()
        "Show the photo described in this object's photo-parameters."
        (loop
           repeat ((getprop this 'map 'get-num-layers))
           do ((getprop this 'map 'layers 0 'destroy)))
        ((getprop this 'map 'add-layer)
         (new ((@ *open-layers *layer *image)
               "Photo"
               (photo-path (getprop this 'photo-parameters))
               (new ((@ *open-layers *bounds) -.5 -.5
                     (+ (getprop this 'photo-parameters 'sensor-width-pix)
                        .5)
                     (+ (getprop this 'photo-parameters 'sensor-height-pix)
                        .5)))           ; coordinates shown
               (new ((@ *open-layers *size) 512 256))
               (create))))
        (chain this map (zoom-to-max-extent)))

      (defun initialize-image (image-index)
        "Create an image usable for displaying photos at position
image-index in array *images*."
        (setf (aref *images* image-index) (new *image))
        (setf (@ (aref *images* image-index) image-click-action)
              (image-click-action (aref *images* image-index)))
        (setf (@ (aref *images* image-index) click)
              (new (*click-control*
                    (create :trigger (@ (aref *images* image-index)
                                        image-click-action)))))
        (chain (aref *images* image-index)
               map
               (add-control
                (@ (aref *images* image-index) click)))
        (chain (aref *images* image-index) click (activate))
        ;;(chain (aref *images* image-index)
        ;;       map
        ;;       (add-control
        ;;        (new (chain *open-layers
        ;;                    *control
        ;;                    (*mouse-position
        ;;                     (create
        ;;                      div (chain
        ;;                           document
        ;;                           (get-element-by-id
        ;;                            (+ "image-" image-index "-zoom")))))))))
        (chain (aref *images* image-index)
               map
               (add-control
                (new (chain *open-layers
                            *control
                            (*layer-switcher
                             (create
                              div (chain
                                   document
                                   (get-element-by-id
                                    (+ "image-" image-index "-layer-switcher")))
                              rounded-corner nil))))))
        (let ((pan-west-control
               (new (chain *open-layers *control (*pan "West"))))
              (pan-north-control
               (new (chain *open-layers *control (*pan "North"))))
              (pan-south-control
               (new (chain *open-layers *control (*pan "South"))))
              (pan-east-control
               (new (chain *open-layers *control (*pan "East"))))
              (zoom-in-control
               (new (chain *open-layers *control (*zoom-in))))
              (zoom-out-control
               (new (chain *open-layers *control (*zoom-out))))
              (zoom-to-max-extent-control
               (new (chain *open-layers *control (*zoom-to-max-extent))))
              (pan-zoom-panel
               (new (chain *open-layers
                           *control
                           (*panel
                            (create div
                                    (chain
                                     document
                                     (get-element-by-id
                                      (+ "image-" image-index "-zoom")))))))))
          (chain (aref *images* image-index) map (add-control pan-zoom-panel))
          (chain pan-zoom-panel (add-controls (array pan-west-control
                                                     pan-north-control
                                                     pan-south-control
                                                     pan-east-control
                                                     zoom-in-control
                                                     zoom-out-control
                                                     zoom-to-max-extent-control))))
        (chain (aref *images* image-index)
               map
               (render (chain document
                              (get-element-by-id
                               (+ "image-" image-index))))))
      
      (defun user-point-selected (event)
        (setf *current-user-point* (chain event feature))
        (remove-any-layers "Active Point")
        (remove-any-layers "Epipolar Line")
        (remove-any-layers "Estimated Position")
        (remove-any-layers "User Point")
        (if (write-permission-p (chain event feature attributes user-name))
            (progn
              (setf (chain document (get-element-by-id "finish-point-button") onclick) update-point)
              (enable-element-with-id "finish-point-button")
              (enable-element-with-id "delete-point-button")
              (setf (inner-html-with-id "h2-controls") "Edit Point"))
            (progn
              (disable-element-with-id "finish-point-button")
              (disable-element-with-id "delete-point-button")
              (setf (inner-html-with-id "h2-controls") "View Point")))
        (setf (inner-html-with-id "creator")
              (+ "(by " (chain event feature attributes user-name) ")"))
        (setf (value-with-id "point-attribute") (chain event feature attributes attribute))
        (setf (value-with-id "point-description") (chain event feature attributes description))
        (setf (value-with-id "point-numeric-description") (chain event feature attributes numeric-description))
        (setf (inner-html-with-id "point-creation-date") (chain event feature attributes creation-date))
        (setf content
              (chain *json-parser*
                     (write
                      (array (chain event feature fid)
                             (loop
                                for i across *images*
                                collect (chain i photo-parameters))))))
        (setf *user-point-in-images-response*
              ((@ *open-layers *Request *POST*)
               (create :url "/phoros-lib/user-point-positions"
                       :data content
                       :headers (create "Content-type" "text/plain"
                                        "Content-length" (@ content length))
                       :success draw-user-point))))

      (defun init ()
        "Prepare user's playground."
        (when (write-permission-p)
          (enable-element-with-id "point-attribute")
          (enable-element-with-id "point-description")
          (enable-element-with-id "point-numeric-description")
          (setf (inner-html-with-id "h2-controls") "Create Point"))
        (setf *point-attributes-select* (chain document (get-element-by-id "point-attribute")))

        (loop for i in '("solitary" "polyline" "polygon") do
             (setf point-attribute-item (chain document (create-element "option")))
             (setf (chain point-attribute-item text) i)
             (chain *point-attributes-select* (add point-attribute-item null))) ;TODO: input of user-defined attributes
        (setf *streetmap*
              (new (chain
                    *open-layers
                    (*map "streetmap"
                          (create projection +geographic+
                                  display-projection +geographic+
                                  controls (array (new (chain *open-layers
                                                              *control
                                                              (*navigation)))
                                                  (new (chain *open-layers
                                                              *control
                                                              (*attribution)))))))))


        (chain *streetmap*
               (add-control
                (new (chain *open-layers
                            *control
                            (*layer-switcher
                             (create
                              div (chain
                                   document
                                   (get-element-by-id
                                    "streetmap-layer-switcher"))
                              rounded-corner nil))))))
        (let ((pan-west-control
               (new (chain *open-layers *control (*pan "West"))))
              (pan-north-control
               (new (chain *open-layers *control (*pan "North"))))
              (pan-south-control
               (new (chain *open-layers *control (*pan "South"))))
              (pan-east-control
               (new (chain *open-layers *control (*pan "East"))))
              (zoom-in-control
               (new (chain *open-layers *control (*zoom-in))))
              (zoom-out-control
               (new (chain *open-layers *control (*zoom-out))))
              (zoom-to-max-extent-control
               (new (chain
                     *open-layers
                     *control
                     (*button
                      (create
                       display-class "streetmapZoomToMaxExtent"
                       trigger (lambda ()
                                 (chain *streetmap*
                                        (zoom-to-extent
                                         +presentation-project-bounds+ ))))))))
              (pan-zoom-panel
               (new (chain *open-layers
                           *control
                           (*panel
                            (create div
                                    (chain
                                     document
                                     (get-element-by-id
                                      "streetmap-zoom")))))))
              (overview-map
               (new (chain *open-layers
                            *control
                            (*overview-map
                             (create
                              min-ratio 14
                              max-ratio 16
                              div (chain document
                                         (get-element-by-id
                                          "streetmap-overview"))))))))
          (chain *streetmap* (add-control pan-zoom-panel))
          (chain pan-zoom-panel
                 (add-controls (array pan-west-control
                                      pan-north-control
                                      pan-south-control
                                      pan-east-control
                                      zoom-in-control
                                      zoom-out-control
                                      zoom-to-max-extent-control)))


          (chain *streetmap* (add-control *click-streetmap*))
          (chain *click-streetmap* (activate))

          (chain *user-point-layer*
                 events
                 (register "featureselected"
                           *user-point-layer* user-point-selected))
          (chain *user-point-layer*
                 events
                 (register "featureunselected"
                           *user-point-layer* reset-controls))
          (chain *streetmap* (add-control *user-points-select-control*))
          (chain *user-points-select-control* (activate))
          
          (chain *streetmap* (add-layer *osm-layer*))
          ;;(chain *streetmap* (add-layer *google*))
          (chain *streetmap* (add-layer *survey-layer*))
          (chain *streetmap* (add-layer *user-point-layer*))
          (setf (chain overview-map element)
                (chain document (get-element-by-id
                                 "streetmap-overview-element")))
          (chain *streetmap*
                 (add-control
                  (new (chain *open-layers *control (*mouse-position)))))
          (chain *streetmap* (add-control overview-map))
          (chain *streetmap*
                 (zoom-to-extent +presentation-project-bounds+)))
        (loop
           for i from 0 to (lisp (1- *number-of-images*))
           do (initialize-image i))
        (add-help-events)))))

(define-easy-handler
    (view :uri "/phoros-lib/view" :default-request-type :post) ()
  "Serve the client their main workspace."
  (if
   (session-value 'authenticated-p)
   (who:with-html-output-to-string (s nil :indent t)
     (:html
      :xmlns "http://www.w3.org/1999/xhtml"
      (:head
       (:title (who:str
                (concatenate
                 'string
                 "Phoros: " (session-value 'presentation-project-name))))
       (if *use-multi-file-openlayers*
           (who:htm
            (:script :src "/phoros-lib/openlayers/lib/Firebug/firebug.js")
            (:script :src "/phoros-lib/openlayers/lib/OpenLayers.js")
            ;;(:script :src "/phoros-lib/openlayers/lib/proj4js.js") ;TODO: we don't seem to use this
            )
           (who:htm (:script :src "/phoros-lib/ol/OpenLayers.js")))
       (:link :rel "stylesheet" :href "/phoros-lib/css/style.css" :type "text/css")
       (:script :src "/phoros-lib/phoros.js")
       ;;(:script :src "http://maps.google.com/maps/api/js?sensor=false")
       )
      (:body
       :onload (ps (init))
       (:h1 :id "title"
            "Phoros: " (who:str (session-value 'user-full-name))
            (who:fmt " (~A)" (session-value 'user-name))
            "with " (:span :id "user-role"
                           (who:str (session-value 'user-role)))
            "permission on "
            (:span :id "presentation-project-name"
                   (who:str (session-value 'presentation-project-name))))
       (:div :class "controlled-streetmap"
             (:div :id "streetmap-controls" :class "streetmap-controls"
                   (:div :class "streetmap-zoom-and-layer-switcher"
                         (:div :id "streetmap-layer-switcher"
                               :class "streetmap-layer-switcher")
                         (:div :id "streetmap-zoom" :class "streetmap-zoom"))
                   (:div :id "streetmap-overview" :class "streetmap-overview"))
             (:div :id "streetmap" :class "smallmap" :style "cursor:crosshair"))
       (:div :class "phoros-controls"
             (:button :id "blurb-button"
                      :type "button"
                      :onclick "self.location.href = \"/phoros-lib/blurb\""
                      "blurb")
             (:button :id "logout-button"
                      :type "button"
                      :onclick "self.location.href = \"/phoros-lib/logout\""
                      "bye")
             :br
             (:button :id "remove-work-layers-button" :disabled t
                      :type "button" :onclick (ps-inline (reset-layers-and-controls))
                      "start over")
             (:h2 (:span :id "h2-controls") (:span :id "creator"))
             (:small (:code :id "point-creation-date"))
             :br
             (:select :id "point-attribute" :disabled t
                      :size 1 :name "point-attribute")
             (:input :id "point-numeric-description" :disabled t
                     :type "text" :size 6 :name "point-numeric-description")
             :br
             (:input :id "point-description" :disabled t
                     :type "text" :size 20 :name "point-description")
             :br
             (:button :disabled t :id "finish-point-button"
                      :type "button"
                      "finish")
             (:button :id "delete-point-button" :disabled t
                      :type "button" :onclick (ps-inline (delete-point))
                      "delete"))
       (:div :id "help-display" :class "smalltext")
       (:div :id "images" :style "clear:both"
             (loop
                for i from 0 below *number-of-images* do 
                (who:htm
                 (:div :class "controlled-image"
                       (:div :id (format nil "image-~S-controls" i)
                             :class "image-controls"
                             (:div :id (format nil "image-~S-zoom" i)
                                   :class "image-zoom")
                             (:div :id (format nil "image-~S-layer-switcher" i)
                                   :class "image-layer-switcher"))
                       (:div :id (format nil "image-~S" i)
                             :class "image" :style "cursor:crosshair"))))))))
   (redirect
    (concatenate 'string "/phoros/" (session-value 'presentation-project-name))
    :add-session-id t)))

(define-easy-handler (epipolar-line :uri "/phoros-lib/epipolar-line") ()
  "Receive vector of two sets of picture parameters, respond with
JSON encoded epipolar-lines."
  (when (session-value 'authenticated-p)
    (let* ((data (json:decode-json-from-string (raw-post-data))))
      (json:encode-json-to-string
       (photogrammetry :epipolar-line (first data) (second data))))))

(define-easy-handler
    (estimated-positions :uri "/phoros-lib/estimated-positions")
    ()
  "Receive a two-part JSON vector comprising (1) a vector containing
sets of picture-parameters including clicked (\"active\") points
stored in :m, :n; and (2) a vector containing sets of
picture-parameters; respond with a JSON encoded two-part vector
comprising (1) a point in global coordinates; and (2) a vector of
image coordinates (m, n) for the global point that correspond to the
images from the received second vector.  TODO: report error on bad
data (ex: points too far apart)."
  ;; TODO: global-point-for-display should probably contain a proj string in order to make sense of the (cartesian) standard deviations.
  (when (session-value 'authenticated-p)
    (let* ((data (json:decode-json-from-string (raw-post-data)))
           (active-point-photo-parameters (first data))
           (destination-photo-parameters (second data))
           (cartesian-system (cdr (assoc :cartesian-system (first active-point-photo-parameters))))
           (global-point-cartesian (photogrammetry :multi-position-intersection active-point-photo-parameters))
           (global-point-geographic-radians
            (proj:cs2cs (list (cdr (assoc :x-global global-point-cartesian))
                              (cdr (assoc :y-global global-point-cartesian))
                              (cdr (assoc :z-global global-point-cartesian)))
                        :source-cs cartesian-system))
           (global-point-for-display    ;points geographic cs, degrees; std deviations in cartesian cs
            (pairlis '(:longitude :latitude :ellipsoid-height
                       :stdx-global :stdy-global :stdz-global)
                     (list
                      (proj:radians-to-degrees (first global-point-geographic-radians))
                      (proj:radians-to-degrees (second global-point-geographic-radians))
                      (third global-point-geographic-radians)
                      (cdr (assoc :stdx-global global-point-cartesian))
                      (cdr (assoc :stdy-global global-point-cartesian))
                      (cdr (assoc :stdz-global global-point-cartesian)))))
           (image-coordinates
            (loop
               for i in destination-photo-parameters
               collect
                 (ignore-errors
                   (photogrammetry :reprojection i global-point-cartesian)))))
      (json:encode-json-to-string
        (list global-point-for-display image-coordinates)))))

(define-easy-handler
    (user-point-positions :uri "/phoros-lib/user-point-positions")
    ()
  "Receive a two-part JSON vector comprising (1) a user-point-id and
\(2) a vector containing sets of picture-parameters; respond with a
JSON encoded vector of image coordinates (m, n) for the global
coordinates of the user point with user-point-id that correspond to
the images from the received image vector."
  (when (session-value 'authenticated-p)
    (let* ((user-point-table-name
            (user-point-table-name (session-value 'presentation-project-name)))
           (data (json:decode-json-from-string (raw-post-data)))
           (user-point-id (first data))
           (destination-photo-parameters (second data))
           (cartesian-system (cdr (assoc :cartesian-system (first destination-photo-parameters)))) ;TODO: in rare cases, coordinate systems of the images shown may differ
           (global-point-geographic
            (with-connection *postgresql-credentials*
              (query
               (:select
                (:as
                 (:st_x (:st_transform 'coordinates *standard-coordinates*))
                 'x-global)
                (:as
                 (:st_y (:st_transform 'coordinates *standard-coordinates*))
                 'y-global)
                (:as
                 (:st_z (:st_transform 'coordinates *standard-coordinates*))
                 'z-global)
                :from user-point-table-name
                :where (:= 'user-point-id user-point-id))
               :list)))
           (global-point-cartesian
            (pairlis '(:x-global :y-global :z-global)
                     (proj:cs2cs
                      (list
                       (proj:degrees-to-radians (first global-point-geographic))
                       (proj:degrees-to-radians (second global-point-geographic))
                       (third global-point-geographic))
                      :destination-cs cartesian-system)))
           (image-coordinates
            (loop
               for i in destination-photo-parameters
               collect
                 (ignore-errors
                   (photogrammetry :reprojection i global-point-cartesian)))))
      (json:encode-json-to-string
       image-coordinates))))

(define-easy-handler (multi-position-intersection :uri "/phoros-lib/intersection") ()
  "Receive vector of sets of picture parameters, respond with stuff."
  (when (session-value 'authenticated-p)
    (let* ((data (json:decode-json-from-string (raw-post-data))))
      (json:encode-json-to-string (photogrammetry :multi-position-intersection data)))))

(defgeneric photogrammetry (mode photo-1 &optional photo-2)
  (:documentation "Call to photogrammetry library.  Dispatch on mode."))

(defmethod photogrammetry :around (mode clicked-photo &optional other-photo)
  "Prepare and clean up a run of photogrammetry."
  (declare (ignore other-photo))
  (bt:with-lock-held (*photogrammetry-mutex*)
    (del-all)
    (unwind-protect
         (call-next-method)
      (del-all))))

(defmethod photogrammetry ((mode (eql :epipolar-line)) clicked-photo &optional other-photo)
  "Return in an alist an epipolar line in coordinates of other-photo from m and n in clicked-photo."
  (add-cam* clicked-photo)
  (add-bpoint* clicked-photo)
  (add-global-car-reference-point* clicked-photo t)
  (add-cam* other-photo)
  (add-global-car-reference-point* other-photo t)
  (loop
     for i = 2d0 then (* i 1.4) until (> i 50)
     do
       (set-distance-for-epipolar-line i)
     when (ignore-errors (calculate))
     collect (pairlis '(:m :n) (list (flip-m-maybe (get-m) other-photo)
                                     (flip-n-maybe (get-n) other-photo)))))

(defmethod photogrammetry ((mode (eql :reprojection)) photo &optional global-point)
  "Calculate reprojection from photo."
  (add-cam* photo)
  (add-global-measurement-point* global-point)
  (add-global-car-reference-point* photo)
  (set-global-reference-frame)
  (calculate)
  (pairlis '(:m :n)
           (list (flip-m-maybe (get-m) photo) (flip-n-maybe (get-n) photo))))

(defmethod photogrammetry ((mode (eql :multi-position-intersection)) photos &optional other-photo)
  "Calculate intersection from photos."
  (declare (ignore other-photo))
  (set-global-reference-frame)
  (loop
     for photo in photos
     do
       (add-cam* photo)
       (add-bpoint* photo)
       (add-global-car-reference-point* photo t))
  (calculate)
  (pairlis '(:x-global :y-global :z-global
             :stdx-global :stdy-global :stdz-global)
           (list
            (get-x-global) (get-y-global) (get-z-global)
            (get-stdx-global) (get-stdy-global) (get-stdz-global))))

(defmethod photogrammetry ((mode (eql :intersection)) photo &optional other-photo)
  "Calculate intersection from two photos that are taken out of the
same local coordinate system.  (Used for debugging only)."
  (add-cam* photo)
  (add-bpoint* photo)
  (add-cam* other-photo)
  (add-bpoint* other-photo)
  (calculate)
  (pairlis '(:x-local :y-local :z-local
             :stdx-local :stdy-local :stdz-local)
           (list
            (get-x-local) (get-y-local) (get-z-local)
            (get-stdx-local) (get-stdy-local) (get-stdz-local)
            (get-x-global) (get-y-global) (get-z-global))))

(defmethod photogrammetry ((mode (eql :mono)) photo &optional floor)
  "Return in an alist the intersection point of the ray through m and n in photo, and floor."
  (add-cam* photo)
  (add-bpoint* photo)
  (add-ref-ground-surface* floor)
  (add-global-car-reference-point* photo)
  (set-global-reference-frame)
  (calculate)
  (pairlis '(:x-global :y-global :z-global)
          (list
           (get-x-global) (get-y-global) (get-z-global))))

(defun flip-m-maybe (m photo)
  "Flip coordinate m when :mounting-angle in photo suggests it necessary."
  (if (= 180 (cdr (assoc :mounting-angle photo)))
      (- (cdr (assoc :sensor-width-pix photo)) m)
      m))
(defun flip-n-maybe (n photo)
  "Flip coordinate n when :mounting-angle in photo suggests it necessary."
  (if (zerop (cdr (assoc :mounting-angle photo)))
      (- (cdr (assoc :sensor-height-pix photo)) n)
      n))

(defun photogrammetry-arglist (alist &rest keys)
  "Construct an arglist from alist values corresponding to keys."
  (mapcar #'(lambda (x) (cdr (assoc x alist))) keys))

(defun add-cam* (photo-alist)
  "Call add-cam with arguments taken from photo-alist."
  (let ((integer-args
         (photogrammetry-arglist
          photo-alist :sensor-height-pix :sensor-width-pix))
        (double-float-args
         (mapcar #'(lambda (x) (coerce x 'double-float))
                 (photogrammetry-arglist photo-alist
                                         :pix-size
                                         :dx :dy :dz :omega :phi :kappa
                                         :c :xh :yh
                                         :a-1 :a-2 :a-3 :b-1 :b-2 :c-1 :c-2 :r-0
                                         :b-dx :b-dy :b-dz :b-ddx :b-ddy :b-ddz
                                         :b-rotx :b-roty :b-rotz
                                         :b-drotx :b-droty :b-drotz))))
    (apply #'add-cam (nconc integer-args double-float-args))))

(defun add-bpoint* (photo-alist)
  "Call add-bpoint with arguments taken from photo-alist."
    (add-bpoint (coerce (flip-m-maybe (cdr (assoc :m photo-alist)) photo-alist) 'double-float)
                (coerce (flip-n-maybe (cdr (assoc :n photo-alist)) photo-alist) 'double-float)))

(defun add-ref-ground-surface* (floor-alist)
  "Call add-ref-ground-surface with arguments taken from floor-alist."
  (let ((double-float-args
         (mapcar #'(lambda (x) (coerce x 'double-float))
                 (photogrammetry-arglist floor-alist
                                         :nx :ny :nz :d))))
    (apply #'add-ref-ground-surface double-float-args)))

(defun add-global-car-reference-point* (photo-alist &optional cam-set-global-p)
  "Call add-global-car-reference-point with arguments taken from photo-alist.  When cam-set-global-p is t, call add-global-car-reference-point-cam-set-global instead."
  (let* ((longitude-radians (proj:degrees-to-radians (car (photogrammetry-arglist photo-alist :longitude))))
         (latitude-radians (proj:degrees-to-radians (car (photogrammetry-arglist photo-alist :latitude))))
         (ellipsoid-height (car (photogrammetry-arglist photo-alist :ellipsoid-height)))
         (destination-cs (car (photogrammetry-arglist photo-alist :cartesian-system)))
         (cartesian-coordinates
          (proj:cs2cs (list longitude-radians latitude-radians ellipsoid-height)
                      :destination-cs destination-cs))
         (other-args
          (mapcar #'(lambda (x) (coerce x 'double-float))
                  (photogrammetry-arglist photo-alist
                                          :roll :pitch :heading
                                          :latitude :longitude)))
         (double-float-args
          (nconc cartesian-coordinates other-args)))
    (apply (if cam-set-global-p
               #'add-global-car-reference-point-cam-set-global
               #'add-global-car-reference-point)
           double-float-args)))

(defun add-global-measurement-point* (point)
  "Call add-global-measurement-point with arguments taken from point."
  (let ((double-float-args
         (mapcar #'(lambda (x) (coerce x 'double-float))
                 (photogrammetry-arglist point
                                         :x-global :y-global :z-global))))
    (apply #'add-global-measurement-point double-float-args)))
  
