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

(defvar *postgresql-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl)")

(defvar *postgresql-aux-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl)")

(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))

(setf *read-default-float-format* 'double-float)

(defparameter *phoros-server* nil "Hunchentoot acceptor.")

(defparameter *common-root* nil
  "Root directory; contains directories of measuring data.")

(defparameter *login-intro* nil
  "A few friendly words to be shown below the login form.")

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

(defun start-server (&key (http-port 8080) address (common-root "/"))
  "Start the presentation project server which listens on http-port
at address.  Address defaults to all addresses of the local machine."
  (setf *phoros-server*
        (make-instance 'hunchentoot:acceptor
                       :port http-port
                       :address address
                       :access-logger #'log-http-access
                       :message-logger #'log-hunchentoot-message))
  (setf *session-max-time* (* 3600 24))
  (setf *common-root* common-root)
  (check-db *postgresql-credentials*)
  (with-connection *postgresql-credentials*
    (assert-phoros-db-major-version))
  (hunchentoot:reset-session-secret)
  (hunchentoot:start *phoros-server*))

(defun stop-server () (hunchentoot:stop *phoros-server*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-sql-operators :2+-ary :&& :overlaps))

(setf *default-handler*
      #'(lambda ()
          "Http default response."
          (setf (return-code*) +http-not-found+)))

(define-easy-handler phoros-handler ()
  "First HTTP contact: if necessary, check credentials, establish new
session."
  (with-connection *postgresql-credentials*
    (let* ((presentation-project-name
            (second (cl-utilities:split-sequence
                     #\/ (script-name*) :remove-empty-subseqs t)))
           (presentation-project-id
            (ignore-errors
              (query
               (:select 'presentation-project-id
                        :from 'sys-presentation-project
                        :where (:= 'presentation-project-name
                                   presentation-project-name))
               :single))))
      (cond
        ((null presentation-project-id)
         (setf (return-code*) +http-not-found+))
        ((and (equal (session-value 'presentation-project-name)
                     presentation-project-name)
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
                    :action "/phoros-lib/authenticate" :name "login-form"
                    "User:"
                    :br
                    (:input :type "text" :name "user-name")
                    :br
                    "Password:"
                    :br
                    (:input :type "password" :name "user-password")
                    :br
                    (:input :type "submit" :value "Submit")
                    (:script :type "text/javascript"
                             (who:str (ps (chain document
                                                 :login-form
                                                 :user-name
                                                 (focus))))))
             (:p (who:str *login-intro*)))))))))
      
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
      (let ((presentation-project-name
             (session-value 'presentation-project-name)))
        (remove-session *session*)
        (who:with-html-output-to-string (s nil :prologue t :indent t)
          (:html
           (:head
            (:title (who:str
                     (concatenate
                      'string
                      "Phoros: logged out" )))
            (:link :rel "stylesheet"
                   :href "/phoros-lib/css/style.css" :type "text/css"))
           (:body
            (:h1 :id "title" "Phoros: logged out")
            (:p "Log back in to project "
                (:a :href (format nil "/phoros/~A" presentation-project-name)
                    (who:fmt "~A." presentation-project-name)))))))
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
    (setf (content-type*) "application/json")
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
           (input-size (cdr (assoc :input-size data)))
           (attribute (cdr (assoc :attribute data)))
           (description (cdr (assoc :description data)))
           (numeric-description (cdr (assoc :numeric-description data)))
           (point-form
            (format nil "SRID=4326; POINT(~S ~S ~S)"
                    longitude-input latitude-input ellipsoid-height-input))
           (aux-numeric-raw (cdr (assoc :aux-numeric data)))
           (aux-text-raw (cdr (assoc :aux-text data)))
           (aux-numeric (if aux-numeric-raw
                            (apply #'vector aux-numeric-raw)
                            :null))
           (aux-text (if aux-text-raw
                         (apply #'vector aux-text-raw)
                         :null))
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
                                     'input-size input-size
                                     'aux-numeric aux-numeric
                                     'aux-text aux-text
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
         () "No point stored.  Did you try to update someone else's point ~
             without having admin permission?")))))

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
       :error
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

(define-easy-handler (points :uri "/phoros-lib/points.json") (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
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
         :error "While fetching points from inside bbox ~S: ~A"
         bbox c)))))

(define-easy-handler (aux-points :uri "/phoros-lib/aux-points.json") (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
    (handler-case 
        (let ((limit *number-of-features-per-layer*)
              (aux-view-name
               (aux-point-view-name (session-value
                                     'presentation-project-name))))
          (encode-geojson-to-string
           (with-connection *postgresql-credentials*
             (query
              (s-sql:sql-compile
               `(:limit
                 (:order-by
                  (:select
                   (:as
                    (:st_x (:st_transform 'coordinates ,*standard-coordinates*))
                    'x)
                   (:as
                    (:st_y (:st_transform 'coordinates ,*standard-coordinates*))
                    'y)
                   (:as
                    (:st_z (:st_transform 'coordinates ,*standard-coordinates*))
                    'z)
                   :from ,aux-view-name
                   :where (:&&
                           (:st_transform 'coordinates ,*standard-coordinates*)
                           (:st_setsrid  (:type ,(box3d bbox) box3d)
                                         ,*standard-coordinates*)))
                  (:random))
                 ,limit))
              :plists))))
      (condition (c)
        (cl-log:log-message
         :error "While fetching aux-points from inside bbox ~S: ~A"
         bbox c)))))

(define-easy-handler
    (aux-local-data :uri "/phoros-lib/aux-local-data" :default-request-type :post)
    ()
  "Receive coordinates, respond with the count nearest json objects
containing arrays aux-numeric, aux-text, and distance to the
coordinates received, wrapped in an array."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
    (let* ((aux-view-name (aux-point-view-name (session-value 'presentation-project-name)))
           (data (json:decode-json-from-string (raw-post-data)))
           (longitude-input (cdr (assoc :longitude data)))
           (latitude-input (cdr (assoc :latitude data)))
           (count (cdr (assoc :count data)))
           (point-form
            (format nil "POINT(~F ~F)" longitude-input latitude-input)))
      (encode-geojson-to-string
       (ignore-errors
         (with-connection *postgresql-credentials*
           (nsubst
            nil :null
            (query
             (s-sql:sql-compile
              `(:limit
                (:order-by
                 (:select
                  (:as
                   (:st_x (:st_transform 'coordinates ,*standard-coordinates*))
                   'x)
                  (:as
                   (:st_y (:st_transform 'coordinates ,*standard-coordinates*))
                   'y)
                  (:as
                   (:st_z (:st_transform 'coordinates ,*standard-coordinates*))
                   'z)
                  aux-numeric
                  aux-text
                  (:as
                   (:st_distance
                    'coordinates
                    (:st_geomfromtext ,point-form ,*standard-coordinates*))
                   distance)                       
                  :from ',aux-view-name)
                 'distance)             ;TODO: convert into metres
                ,count))
             :plists))))))))

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

(define-easy-handler (user-points :uri "/phoros-lib/user-points.json") (bbox)
  "Send *number-of-features-per-layer* randomly chosen GeoJSON-encoded
points from inside bbox to client.  If there is no bbox parameter,
send all points."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
    (handler-case 
        (let ((bounding-box (or bbox "-180,-90,180,90"))
              (limit (if bbox *number-of-features-per-layer* :null))
              (order-criterion (if bbox '(:random) 'id))
              (user-point-table-name
               (user-point-table-name (session-value
                                       'presentation-project-name))))
          (encode-geojson-to-string
           (with-connection *postgresql-credentials*
             (nsubst
              nil :null
              (query
               (s-sql:sql-compile
                `(:limit
                  (:order-by
                   (:select
                    (:as
                     (:st_x (:st_transform 'coordinates ,*standard-coordinates*))
                     'x)
                    (:as
                     (:st_y (:st_transform 'coordinates ,*standard-coordinates*))
                     'y)
                    (:as
                     (:st_z (:st_transform 'coordinates ,*standard-coordinates*))
                     'z)
                    (:as 'user-point-id 'id) ;becomes fid on client
                    'stdx-global 'stdy-global 'stdz-global
                    'input-size
                    'attribute
                    'description
                    'numeric-description
                    'user-name
                    (:as (:to-char 'creation-date "IYYY-MM-DD HH24:MI:SS TZ")
                         'creation-date)
                    'aux-numeric
                    'aux-text
                    :from ,user-point-table-name :natural :left-join 'sys-user
                    :where (:&&
                            (:st_transform 'coordinates ,*standard-coordinates*)
                            (:st_setsrid  (:type ,(box3d bounding-box) box3d)
                                          ,*standard-coordinates*)))
                   ,order-criterion)
                  ,limit))
               :plists)))))
      (condition (c)
        (cl-log:log-message
         :error "While fetching user-points~@[ from inside bbox ~S~]: ~A"
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
         :error "While serving image ~S: ~A" (request-uri*) c)))))

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

(define-easy-handler
    (view :uri "/phoros-lib/view" :default-request-type :post) ()
  "Serve the client their main workspace."
  (if
   (session-value 'authenticated-p)
   (who:with-html-output-to-string (s nil :prologue t :indent t)
     (:html
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
       (:link :rel "stylesheet"
              :href "/phoros-lib/css/style.css" :type "text/css")
       (:script :src "/phoros-lib/phoros.js")
       (:script :src "http://maps.google.com/maps/api/js?sensor=false"))
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
             (:div :id "streetmap" :class "streetmap" :style "cursor:crosshair")
             (:div :id "streetmap-controls" :class "streetmap-controls"
                   (:div :id "streetmap-vertical-strut"
                         :class "streetmap-vertical-strut")
                   (:div :id "streetmap-layer-switcher"
                         :class "streetmap-layer-switcher")
                   (:div :id "streetmap-overview" :class "streetmap-overview")
                   (:div :id "streetmap-mouse-position"
                         :class "streetmap-mouse-position")
                   (:div :id "streetmap-zoom" :class "streetmap-zoom")))
       (:div :class "phoros-controls"
             (:div :id "phoros-controls-vertical-strut"
                   :class "phoros-controls-vertical-strut")
             (:div :id "real-phoros-controls"
                   (:h2 (:span :id "h2-controls") (:span :id "creator"))
                   (:select :id "point-attribute" :disabled t
                            :size 1 :name "point-attribute")
                   (:input :id "point-numeric-description" :class "vanilla-input "
                           :disabled t
                           :type "text" :name "point-numeric-description")
                   (:input :id "point-description" :class "vanilla-input"
                           :disabled t
                           :type "text" :name "point-description")
                   (:div (:button :id "delete-point-button" :disabled t
                                  :type "button" :onclick (ps-inline (delete-point))
                                  "delete")
                         (:button :disabled t :id "finish-point-button"
                                  :type "button"
                                  "finish"))
                   (:div :id "aux-point-distance-or-point-creation-date"
                         (:code :id "point-creation-date")
                         (:input :id "include-aux-data-p"
                                 :type "checkbox" :checked t :name "include-aux-data-p"
                                 :onchange (ps-inline (flip-aux-data-inclusion)))
                         (:select :id "aux-point-distance" :disabled t
                                  :size 1 :name "aux-point-distance"
                                  :onchange (ps-inline (aux-point-distance-selected))
                                  :onclick (ps-inline (enable-aux-point-selection))))
                   (:div :id "aux-data"
                         (:div :id "aux-numeric-list")
                         (:div :id "aux-text-list")))
             (:div :id "multiple-points-phoros-controls"
                   (:h2 "Multiple Points Selected")
                   (:p "You have selected multiple user points.")
                   (:p "Unselect all but one to edit or view its properties."))
             (:div :class "image-main-controls"
                   (:div :id "auto-zoom"
                         (:input :id "zoom-to-point-p" :class "tight-input"
                                 :type "checkbox" :checked t "auto zoom"))
                   (:div :id "zoom-images-to-max-extent"
                         :onclick (ps-inline (zoom-images-to-max-extent)))
                   (:div :id "remove-work-layers-button" :disabled t
                         :onclick (ps-inline (reset-layers-and-controls))
                         "start over")))
       (:div :class "help-div"
             (:button :id "download-user-points-button"
                      :type "button" :onclick "self.location.href = \"/phoros-lib/user-points.json\""
                      "download points") ;TODO: offer other formats and maybe projections
             (:button :id "blurb-button"
                      :type "button"
                      :onclick (ps-inline
                                (chain window
                                       (open "/phoros-lib/blurb" "About Phoros")))
                      (:img :src "/phoros-lib/public_html/phoros-logo-plain.png"
                            :alt "Phoros" :style "vertical-align:middle"
                            :height 20))
             (:button :id "logout-button"
                      :type "button"
                      :onclick "self.location.href = \"/phoros-lib/logout\""
                      "bye")
             (:h2 :id "h2-help" "Help")
             (:div :id "help-display"))
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
                                   :class "image-layer-switcher")
                             (:div :id (format nil "image-~S-trigger-time" i)
                                   :class "image-trigger-time"))
                       (:div :id (format nil "image-~S" i)
                             :class "image" :style "cursor:crosshair"))))))))
   (redirect
    (concatenate 'string "/phoros/" (session-value 'presentation-project-name))
    :add-session-id t)))

(define-easy-handler (epipolar-line :uri "/phoros-lib/epipolar-line") ()
  "Receive vector of two sets of picture parameters, respond with
JSON encoded epipolar-lines."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
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
    (setf (content-type*) "application/json")
    (let* ((data
            (json:decode-json-from-string (raw-post-data)))
           (active-point-photo-parameters
            (first data))
           (number-of-active-points
            (length active-point-photo-parameters))
           (destination-photo-parameters
            (second data))
           (cartesian-system
            (cdr (assoc :cartesian-system
                        (first active-point-photo-parameters))))
           (global-point-cartesian
            (photogrammetry
             :multi-position-intersection active-point-photo-parameters))
           (global-point-geographic-radians
            (proj:cs2cs (list (cdr (assoc :x-global global-point-cartesian))
                              (cdr (assoc :y-global global-point-cartesian))
                              (cdr (assoc :z-global global-point-cartesian)))
                        :source-cs cartesian-system))
           (global-point-for-display    ;points geographic cs, degrees; std deviations in cartesian cs
            (pairlis '(:longitude :latitude :ellipsoid-height
                       :stdx-global :stdy-global :stdz-global
                       :input-size)
                     (list
                      (proj:radians-to-degrees (first global-point-geographic-radians))
                      (proj:radians-to-degrees (second global-point-geographic-radians))
                      (third global-point-geographic-radians)
                      (cdr (assoc :stdx-global global-point-cartesian))
                      (cdr (assoc :stdy-global global-point-cartesian))
                      (cdr (assoc :stdz-global global-point-cartesian))
                      number-of-active-points)))
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
  "Receive a two-part JSON vector comprising
- a vector of user-point-id's and
- a vector containing sets of picture-parameters;
respond with a JSON object comprising the elements
- image-points, a vector whose elements 
   - correspond to the elements of the picture-parameters vector
     received and
   - are GeoJSON feature collections containing one point (in picture
     coordinates) for each user-point-id received;
- user-point-count, the number of user-points we tried to fetch
  image-points for."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
    (let* ((user-point-table-name
            (user-point-table-name (session-value 'presentation-project-name)))
           (data (json:decode-json-from-string (raw-post-data)))
           (user-point-ids (first data))
           (user-point-count (length user-point-ids))
           (destination-photo-parameters (second data))
           (cartesian-system
            (cdr (assoc :cartesian-system
                        (first destination-photo-parameters)))) ;TODO: in rare cases, coordinate systems of the images shown may differ
           (user-points
            (with-connection *postgresql-credentials*
              (query
               (:select
                (:as
                 (:st_x (:st_transform 'coordinates *standard-coordinates*))
                 'longitude)
                (:as
                 (:st_y (:st_transform 'coordinates *standard-coordinates*))
                 'latitude)
                (:as
                 (:st_z (:st_transform 'coordinates *standard-coordinates*))
                 'ellipsoid-height)
                (:as 'user-point-id 'id) ;becomes fid on client
                'attribute
                'description
                'numeric-description
                'user-name
                (:as (:to-char 'creation-date "IYYY-MM-DD HH24:MI:SS TZ")
                     'creation-date)
                'aux-numeric
                'aux-text
                :from user-point-table-name :natural :left-join 'sys-user
                :where (:in 'user-point-id (:set user-point-ids)))
               :plists)))
           (global-points-cartesian
            (loop
               for global-point-geographic in user-points
               collect
               (ignore-errors ;in case no destination-photo-parameters have been sent
                 (pairlis '(:x-global :y-global :z-global)
                          (proj:cs2cs
                           (list
                            (proj:degrees-to-radians
                             (getf global-point-geographic :longitude))
                            (proj:degrees-to-radians
                             (getf global-point-geographic :latitude))
                            (getf global-point-geographic :ellipsoid-height))
                           :destination-cs cartesian-system)))))
           (image-coordinates
            (loop
               for photo-parameter-set in destination-photo-parameters
               collect
               (encode-geojson-to-string
                (loop
                   for global-point-cartesian in global-points-cartesian
                   for user-point in user-points
                   collect
                   (ignore-errors
                     (let ((photo-coordinates
                            (photogrammetry :reprojection
                                            photo-parameter-set
                                            global-point-cartesian))
                           (photo-point
                            user-point))
                       (setf (getf photo-point :x)
                             (cdr (assoc :m photo-coordinates)))
                       (setf (getf photo-point :y)
                             (cdr (assoc :n photo-coordinates)))
                       photo-point)))
                :longitude :latitude :ellipsoid-height))))
      (with-output-to-string (s)
        (json:with-object (s)
          (json:encode-object-member :user-point-count user-point-count s)
          (json:as-object-member (:image-points s)
            (json:with-array (s)
              (loop for i in image-coordinates do
                   (json:as-array-member (s) (princ i s))))))))))

(define-easy-handler
    (multi-position-intersection :uri "/phoros-lib/intersection")
    ()
  "Receive vector of sets of picture parameters, respond with stuff."
  (when (session-value 'authenticated-p)
    (setf (content-type*) "application/json")
    (let* ((data (json:decode-json-from-string (raw-post-data))))
      (json:encode-json-to-string
       (photogrammetry :multi-position-intersection data)))))
