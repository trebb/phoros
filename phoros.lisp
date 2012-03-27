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

(defparameter *spherical-mercator* 900913
  "EPSG code of the coordinate system used for some distance calculations.")

(defvar *postgresql-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

(defvar *postgresql-aux-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))

(setf *read-default-float-format* 'double-float)

(defparameter *phoros-server* nil "Hunchentoot acceptor.")

(defparameter *common-root* nil
  "Root directory; contains directories of measuring data.")

(defparameter *proxy-root* "phoros"
  "First directory element of the server URL.  Must correspond to the
proxy configuration if Phoros is hidden behind a proxy.")

(defparameter *login-intro* nil
  "A few friendly words to be shown below the login form.")

(defparameter *log-sql-p* nil
  "If t, log SQL queries and results.")

(defparameter *postgresql-warnings* nil
  "If t, show PostgreSQL's WARNINGs and NOTICEs.")

(defparameter *render-footprints-p* nil
  "If t, put image footprints into images on client.")

(defparameter *use-multi-file-openlayers* nil
  "If t, use OpenLayers uncompiled from openlayers/*, which makes
  debugging easier.  Otherwise use a single-file shrunk
  ol/Openlayers.js.")

(defparameter *number-of-images* 4
  "Number of photos shown to the HTTP client.")

(defparameter *aux-numeric-labels* nil
  "Labels for auxiliary numeric data rows shown to the HTTP client.")

(defparameter *aux-text-labels* nil
  "Labels for auxiliary text data rows shown to the HTTP client.")

(defparameter *browser-cache-max-age* (* 3600 24 7)
  "Value x for Cache-Control:max-age=x, for images on client.")

(defparameter *number-of-features-per-layer* 500
  "What we think a browser can swallow.")

(defparameter *number-of-points-per-aux-linestring* 500
  "What we think a browser can swallow.")

(defparameter *user-point-creation-date-format* "IYYY-MM-DD HH24:MI:SS TZ"
  "SQL date format used for display and GeoJSON export of user points.")

(defparameter *phoros-version*
  (asdf:component-version (asdf:find-system :phoros))
  "Phoros version as defined in system definition.")

(defparameter *phoros-description*
  (asdf:system-description (asdf:find-system :phoros))
  "Phoros description as defined in system definition.")

(defparameter *phoros-long-description*
  (asdf:system-long-description (asdf:find-system :phoros))
  "Phoros long-description as defined in system definition.")

(defparameter *phoros-licence*
  (asdf:system-licence (asdf:find-system :phoros))
  "Phoros licence as defined in system definition.")

(defun phoros-version (&key major minor revision)
  "Return version of this program, either one integer part as denoted by
the key argument, or the whole dotted string."
  (let ((version-components
         (mapcar #'parse-integer
                 (cl-utilities:split-sequence #\. *phoros-version*))))
    (cond (major (first version-components))
          (minor (second version-components))
          (revision (third version-components))
          (t *phoros-version*))))

(defun check-dependencies ()
  "Say OK if the necessary external dependencies are available."
  (handler-case
      (progn
        (geographic-to-utm 33 13 52)    ;check cs2cs
        (phoros-photogrammetry:del-all) ;check photogrammetry
        (initialize-leap-seconds)    ;check source of leap second info
        (format *error-output* "~&OK~%"))
    (error (e) (format *error-output* "~A~&" e))))

(defun muffle-postgresql-warnings ()
  "For current DB, silence PostgreSQL's warnings about implicitly
created stuff."
  (unless *postgresql-warnings*
    (execute "SET client_min_messages TO ERROR;")))

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

(defun ignore-warnings (c) (declare (ignore c)) (muffle-warning))

(defmethod hunchentoot:session-cookie-name (acceptor)
  (declare (ignore acceptor))
  "phoros-session")

(defun start-server (&key (proxy-root "phoros") (http-port 8080) address (common-root "/"))
  "Start the presentation project server which listens on http-port
at address.  Address defaults to all addresses of the local machine."
  (setf *phoros-server*
        (make-instance 'hunchentoot:acceptor
                       :port http-port
                       :address address
                       :access-logger #'log-http-access
                       :message-logger #'log-hunchentoot-message))
  (setf hunchentoot:*session-max-time* (* 3600 24))
  (setf *proxy-root* proxy-root)
  (setf *common-root* common-root)
  (check-db *postgresql-credentials*)
  (with-connection *postgresql-credentials*
    (assert-phoros-db-major-version))
  (hunchentoot:reset-session-secret)
  (hunchentoot:start *phoros-server*))

(defun stop-server () (hunchentoot:stop *phoros-server*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-sql-operators :2+-ary :&& :overlaps))

(setf hunchentoot:*default-handler*
      #'(lambda ()
          "Http default response."
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)))

(hunchentoot:define-easy-handler phoros-handler ()
  "First HTTP contact: if necessary, check credentials, establish new
session."
  (with-connection *postgresql-credentials*
    (let* ((s (cl-utilities:split-sequence
               #\/
               (hunchentoot:script-name*)
               :remove-empty-subseqs t))
           (presentation-project-name (second s))
           (presentation-project-id
            (ignore-errors
              (query
               (:select 'presentation-project-id
                        :from 'sys-presentation-project
                        :where (:= 'presentation-project-name
                                   presentation-project-name))
               :single))))
      
      ;; TODO: remove the following line (which seems to function as a
      ;; wakeup call of sorts)...
      (get-dao 'sys-user-role 0 0)
      ;; ...and make sure the following error doesn't occur any longer
      ;; while accessing the HTTP server:
      ;; #<POSTMODERN:DAO-CLASS PHOROS::SYS-USER-ROLE> cannot be printed readably.

      (cond
        ((null presentation-project-id)
         (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))
        ((and (equal (hunchentoot:session-value 'presentation-project-name)
                     presentation-project-name)
              (hunchentoot:session-value 'authenticated-p))
         (hunchentoot:redirect
          (format nil "/~A/lib/view-~A"
                  ;; *proxy-root*
                  "phoros"
                  (phoros-version))
          :add-session-id t))
        (t
         (progn
           (setf (hunchentoot:session-value 'presentation-project-name)
                 presentation-project-name)
           (setf (hunchentoot:session-value 'presentation-project-id)
                 presentation-project-id)
           (setf (hunchentoot:session-value 'presentation-project-bbox)
                 (let ((bbox
                        (ignore-errors
                          (bounding-box (get-dao 'sys-presentation-project
                                                 presentation-project-name)))))
                   (if (or (null bbox) (eq :null bbox))
                       nil
                       bbox)))
           (setf (hunchentoot:session-value 'aux-data-p)
                 (with-connection *postgresql-aux-credentials*
                   (view-exists-p (aux-point-view-name
                                   presentation-project-name))))
           (who:with-html-output-to-string (s nil :prologue t :indent t)
             (:body
              :style "font-family:sans-serif;"
              (:form
               :method "post" :enctype "multipart/form-data"
               :action (format nil "/~A/lib/authenticate"
                               *proxy-root*)
               :name "login-form"
               (:fieldset
                (:legend (:b (:a :href "http://phoros.boundp.org"
                                 :style "text-decoration:none;"
                                 "Phoros")
                             (who:fmt "&nbsp;[~A]" presentation-project-name)))
                (:noscript
                 (:b (:em "You can't do much without JavaScript there.")))
                (:p "User:"
                    :br
                    (:input :type "text" :name "user-name"))
                (:p "Password:"
                    :br
                    (:input :type "password" :name "user-password")
                    "&nbsp;&nbsp;&nbsp;"
                    (:span :id "cackle"))
                (:input :type "submit" :value "Submit"
                        :onclick (ps-inline
                                  (setf (chain document
                                               (get-element-by-id "cackle")
                                               inner-h-t-m-l)
                                        "Ok, let&#039;s see&#8230;"))))
               (:script :type "text/javascript"
                        (who:str (ps (chain document
                                            :login-form
                                            :user-name
                                            (focus))))))
              (loop
                 for i in *login-intro*
                 do (who:htm (:p (who:str i))))))))))))

(pushnew (hunchentoot:create-regex-dispatcher "/phoros/(?!lib/)"
                                              'phoros-handler)
         hunchentoot:*dispatch-table*)

(defun stored-bbox ()
  "Return stored bounding box for user and presentation project of
current session."
  (with-connection *postgresql-credentials*
    (let ((bbox (bounding-box
                 (get-dao 'sys-user-role
                          (hunchentoot:session-value
                           'user-id)
                          (hunchentoot:session-value
                           'presentation-project-id)))))
      (if (eq :null bbox)
          (hunchentoot:session-value 'presentation-project-bbox)
          bbox))))

(defun stored-cursor ()
  "Return stored cursor position for user and presentation project of
current session."
  (with-connection *postgresql-credentials*
    (let ((cursor
           (query
            (:select (:st_x 'cursor) (:st_y 'cursor)
                     :from 'sys-user-role
                     :where (:and (:= 'user-id
                                      (hunchentoot:session-value 'user-id))
                                  (:= 'presentation-project-id
                                      (hunchentoot:session-value
                                       'presentation-project-id))
                                  (:raw "cursor IS NOT NULL")))
            :list)))
      (when cursor
        (format nil "~{~F~#^,~}" cursor)))))


(hunchentoot:define-easy-handler
    (authenticate-handler :uri "/phoros/lib/authenticate"
                          :default-request-type :post)
    ()
  "Check user credentials."
  (with-connection *postgresql-credentials*
    (let* ((user-name (hunchentoot:post-parameter "user-name"))
           (user-password (hunchentoot:post-parameter "user-password"))
           (presentation-project-id (hunchentoot:session-value
                                     'presentation-project-id))
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
            (setf (hunchentoot:session-value 'authenticated-p) t
                  (hunchentoot:session-value 'user-name) user-name
                  (hunchentoot:session-value 'user-full-name) user-full-name
                  (hunchentoot:session-value 'user-id) user-id
                  (hunchentoot:session-value 'user-role) user-role)            
            (hunchentoot:redirect
             (format nil "/~A/lib/view-~A"
                     ;; *proxy-root*
                     "phoros"
                     (phoros-version))
             :add-session-id t))
          (who:with-html-output-to-string (s nil :prologue t :indent t)
            (:body
             :style "font-family:sans-serif;"
             (:b "Rejected. ")
             (:a :href (format nil "/~A/~A/"
                               *proxy-root*
                               (hunchentoot:session-value
                                'presentation-project-name))
                 "Retry?")))))))

(defun assert-authentication ()
  "Abort request handler on unauthorized access."
  (unless (hunchentoot:session-value 'authenticated-p)
    (setf (hunchentoot:return-code*) hunchentoot:+http-precondition-failed+)
    (hunchentoot:abort-request-handler)))

(hunchentoot:define-easy-handler logout-handler (bbox longitude latitude)
  (if (hunchentoot:session-value 'authenticated-p)
      (with-connection *postgresql-credentials*
        (let ((presentation-project-name
               (hunchentoot:session-value 'presentation-project-name))
              (sys-user-role
               (get-dao 'sys-user-role
                        (hunchentoot:session-value 'user-id)
                        (hunchentoot:session-value 'presentation-project-id))))
          (when sys-user-role
            (when bbox
              (setf (bounding-box sys-user-role) bbox))
            (when (and longitude latitude)
              (let* ;; kludge: should be done by some library, not by DB query
                  ((point-form (format nil "POINT(~F ~F)" longitude latitude))
                   (point-wkb (query (:select
                                      (:st_geomfromtext point-form))
                                     :single)))
                (setf (cursor sys-user-role) point-wkb)))
            (update-dao sys-user-role))
          (hunchentoot:remove-session hunchentoot:*session*)
          (who:with-html-output-to-string (s nil :prologue t :indent t)
            (:html
             (:head
              (:title (who:str
                       (concatenate
                        'string
                        "Phoros: logged out" )))
              (:link :rel "stylesheet"
                     :href (format nil "/~A/lib/css-~A/style.css"
                                   *proxy-root*
                                   (phoros-version))
                     :type "text/css"))
             (:body
              (:h1 :id "title" "Phoros: logged out")
              (:p "Log back in to project "
                  (:a :href (format nil "/~A/~A"
                                    *proxy-root*
                                    presentation-project-name)
                      (who:fmt "~A." presentation-project-name))))))))
      "Bye (again)."))

(pushnew (hunchentoot:create-regex-dispatcher "/logout" 'logout-handler)
         hunchentoot:*dispatch-table*)

(define-condition superseded () ()
  (:documentation
   "Tell a thread to finish as soon as possible taking any shortcuts
   available."))

(hunchentoot:define-easy-handler
    (nearest-image-data :uri "/phoros/lib/nearest-image-data"
                        :default-request-type :post)
    ()
  "Receive coordinates, respond with the count nearest json objects
containing picture url, calibration parameters, and car position,
wrapped in an array.  Wipe away any unfinished business first."
  (assert-authentication)
  (dolist (old-thread (hunchentoot:session-value 'recent-threads))
    (ignore-errors
      (bt:interrupt-thread old-thread
                           #'(lambda () (signal 'superseded)))))
  (setf (hunchentoot:session-value 'recent-threads) nil)
  (setf (hunchentoot:session-value 'number-of-threads) 1)
  (push (bt:current-thread) (hunchentoot:session-value 'recent-threads))
  (setf (hunchentoot:content-type*) "application/json")
  (with-connection *postgresql-credentials*
    (let* ((presentation-project-id (hunchentoot:session-value
                                     'presentation-project-id))
           (common-table-names (common-table-names
                                presentation-project-id))
           (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
           (longitude (cdr (assoc :longitude data)))
           (latitude (cdr (assoc :latitude data)))
           (count (cdr (assoc :count data)))
           (zoom (cdr (assoc :zoom data)))
           (snap-distance               ;bogus distance in degrees,
            (* 100e-5                   ; assuming geographic
               (expt 2 (-               ; coordinates
                        14              ; (1m = 1e-5 degrees)
                        (max 13
                             (min 18 zoom))))))
           (point-form (format nil "POINT(~F ~F)" longitude latitude))
           (nearest-footprint-centroid-query
            ;; Inserting the following into
            ;; image-data-with-footprints-query as a subquery would
            ;; work correctly but is way too slow.
            (sql-compile
             `(:limit
               (:select
                'centroid :from
                (:as
                 (:order-by
                  (:union
                   ,@(loop
                        for common-table-name
                        in common-table-names
                        for aggregate-view-name
                        = (aggregate-view-name
                           common-table-name)
                        collect  
                        `(:select
                          (:as
                           (:st_distance
                            (:st_centroid 'footprint)
                            (:st_geomfromtext
                             ,point-form
                             ,*standard-coordinates*))
                           'distance)
                          (:as (:st_centroid 'footprint)
                               'centroid)
                          :from
                          ',aggregate-view-name
                          :where
                          (:and
                           (:= 'presentation-project-id
                               ,presentation-project-id)
                           (:st_dwithin
                            'footprint
                            (:st_geomfromtext
                             ,point-form
                             ,*standard-coordinates*)
                            ,snap-distance)))))
                  'distance)
                 'centroids))
               1)))
           (nearest-footprint-centroid
            (ignore-errors (logged-query "centroid of nearest footprint"
                                         nearest-footprint-centroid-query
                                         :single)))
           (image-data-with-footprints-query
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
                        (:as (:st_distance 'coordinates
                                           ;; (:st_geomfromtext
                                           ;;  ,point-form
                                           ;;  ,*standard-coordinates*)
                                           ,nearest-footprint-centroid
                                           )
                             'distance)
                        'usable
                        'recorded-device-id        ;debug
                        'device-stage-of-life-id   ;debug
                        'generic-device-id         ;debug
                        'directory
                        'filename 'byte-position 'point-id
                        (:as (:not (:is-null 'footprint))
                             'footprintp)
                        ,(when *render-footprints-p*
                               '(:as (:st_asewkt 'footprint)
                                 'footprint-wkt))
                        'trigger-time
                        ;;'coordinates   ;the search target
                        'longitude 'latitude 'ellipsoid-height
                        'cartesian-system
                        'east-sd 'north-sd 'height-sd
                        'roll 'pitch 'heading
                        'roll-sd 'pitch-sd 'heading-sd
                        'sensor-width-pix 'sensor-height-pix
                        'pix-size
                        'bayer-pattern 'color-raiser
                        'mounting-angle
                        'dx 'dy 'dz 'omega 'phi 'kappa
                        'c 'xh 'yh 'a1 'a2 'a3 'b1 'b2 'c1 'c2 'r0
                        'b-dx 'b-dy 'b-dz 'b-rotx 'b-roty 'b-rotz
                        'b-ddx 'b-ddy 'b-ddz
                        'b-drotx 'b-droty 'b-drotz
                        :from
                        ',aggregate-view-name
                        :where
                        (:and
                         (:= 'presentation-project-id
                             ,presentation-project-id)
                         (:st_contains 'footprint
                                       ,nearest-footprint-centroid)))))
                'distance)
               ,count)))
           (image-data-without-footprints-query
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
                        (:as (:st_distance 'coordinates
                                           (:st_geomfromtext
                                            ,point-form
                                            ,*standard-coordinates*))
                             'distance)
                        'usable
                        'recorded-device-id        ;debug
                        'device-stage-of-life-id   ;debug
                        'generic-device-id         ;debug
                        'directory
                        'filename 'byte-position 'point-id
                        (:as (:not (:is-null 'footprint))
                             'footprintp)
                        'trigger-time
                        ;;'coordinates   ;the search target
                        'longitude 'latitude 'ellipsoid-height
                        'cartesian-system
                        'east-sd 'north-sd 'height-sd
                        'roll 'pitch 'heading
                        'roll-sd 'pitch-sd 'heading-sd
                        'sensor-width-pix 'sensor-height-pix
                        'pix-size
                        'bayer-pattern 'color-raiser
                        'mounting-angle
                        'dx 'dy 'dz 'omega 'phi 'kappa
                        'c 'xh 'yh 'a1 'a2 'a3 'b1 'b2 'c1 'c2 'r0
                        'b-dx 'b-dy 'b-dz 'b-rotx 'b-roty 'b-rotz
                        'b-ddx 'b-ddy 'b-ddz
                        'b-drotx 'b-droty 'b-drotz
                        :from
                        ',aggregate-view-name
                        :where
                        (:and (:= 'presentation-project-id
                                  ,presentation-project-id)
                              (:st_dwithin 'coordinates
                                           (:st_geomfromtext
                                            ,point-form
                                            ,*standard-coordinates*)
                                           ,snap-distance)))))
                'distance)
               ,count)))
           (result
            (handler-case
                (ignore-errors
                  (if nearest-footprint-centroid
                      (logged-query
                       "footprints are ready"
                       image-data-with-footprints-query :alists)
                      (logged-query
                       "no footprints yet"
                       image-data-without-footprints-query :alists)))
              (superseded () nil))))
      (when *render-footprints-p*
        (setf
         result
         (loop
            for photo-parameter-set in result
            for footprint-vertices =    ;something like this:
            ;; "SRID=4326;POLYGON((14.334342229 51.723293508 118.492667334,14.334386877 51.723294417 118.404764286,14.334347429 51.72327914 118.506316418,14.334383211 51.723279895 118.435823396,14.334342229 51.723293508 118.492667334))"
            (ignore-errors              ;probably no :footprint-wkt
              (mapcar (lambda (p)
                        (mapcar (lambda (x)
                                  (parse-number:parse-real-number x))
                                (cl-utilities:split-sequence #\Space p)))
                      (subseq
                       (cl-utilities:split-sequence-if
                        (lambda (x)
                          (or (eq x #\,)
                              (eq x #\()
                              (eq x #\))))
                        (cdr (assoc :footprint-wkt photo-parameter-set)))
                       2 7)))
            collect
            (if footprint-vertices
                (acons
                 :rendered-footprint
                 (pairlis
                  '(:type :coordinates)
                  (list
                   :line-string
                   (loop
                      for footprint-vertex in footprint-vertices
                      for reprojected-vertex =
                      (photogrammetry
                       :reprojection
                       ;; KLUDGE: translate keys, e.g. a1 -> a_1
                       (json:decode-json-from-string
                        (json:encode-json-to-string photo-parameter-set))
                       (pairlis '(:x-global :y-global :z-global)
                                (proj:cs2cs
                                 (list (proj:degrees-to-radians
                                        (first footprint-vertex))
                                       (proj:degrees-to-radians
                                        (second footprint-vertex))
                                       (third footprint-vertex))
                                 :destination-cs
                                 (cdr (assoc :cartesian-system
                                             photo-parameter-set)))))
                      collect
                      (list (cdr (assoc :m reprojected-vertex))
                            (cdr (assoc :n reprojected-vertex))))))
                 photo-parameter-set)
                photo-parameter-set))))
      (decf (hunchentoot:session-value 'number-of-threads))
      (json:encode-json-to-string result))))

(hunchentoot:define-easy-handler
    (nearest-image-urls :uri "/phoros/lib/nearest-image-urls"
                        :default-request-type :post)
    ()
  "Receive coordinates, respond with a json array of the necessary
ingredients for the URLs of the 256 nearest images."
  (assert-authentication)
  (push (bt:current-thread) (hunchentoot:session-value 'recent-threads))
  (if (<= (hunchentoot:session-value 'number-of-threads)
          0)              ;only stuff cache if everything else is done
      (progn
        (incf (hunchentoot:session-value 'number-of-threads))
        (setf (hunchentoot:content-type*) "application/json")
        (with-connection *postgresql-credentials*
          (let* ((presentation-project-id (hunchentoot:session-value
                                           'presentation-project-id))
                 (common-table-names (common-table-names
                                      presentation-project-id))
                 (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
                 (longitude (cdr (assoc :longitude data)))
                 (latitude (cdr (assoc :latitude data)))
                 (count 256)
                 (radius (* 5d-4))   ; assuming geographic coordinates
                 (point-form (format nil "POINT(~F ~F)" longitude latitude))
                 (result

                  (handler-case
                      (ignore-errors
                        (query
                         (sql-compile
                          `(:limit
                            (:select
                             'directory 'filename 'byte-position
                             'bayer-pattern 'color-raiser 'mounting-angle
                             :from
                             (:as
                              (:order-by
                               (:union
                                ,@(loop
                                     for common-table-name
                                     in common-table-names
                                     for aggregate-view-name
                                     = (aggregate-view-name common-table-name)
                                     collect  
                                     `(:select
                                       'directory
                                       'filename 'byte-position
                                       'bayer-pattern 'color-raiser
                                       'mounting-angle
                                       (:as (:st_distance
                                             'coordinates
                                             (:st_geomfromtext
                                              ,point-form
                                              ,*standard-coordinates*))
                                            'distance)
                                       :from
                                       ',aggregate-view-name
                                       :where
                                       (:and (:= 'presentation-project-id
                                                 ,presentation-project-id)
                                             (:st_dwithin
                                              'coordinates
                                              (:st_geomfromtext
                                               ,point-form
                                               ,*standard-coordinates*)
                                              ,radius)))))
                               'distance)
                              'raw-image-urls))
                            ,count))
                         :alists))
                    (superseded ()
                      (setf (hunchentoot:return-code*)
                            hunchentoot:+http-gateway-time-out+)
                      ;; (decf (hunchentoot:session-value 'number-of-threads))
                      nil))))
            (decf (hunchentoot:session-value 'number-of-threads))
            (json:encode-json-to-string result))))
      (setf (hunchentoot:return-code*) hunchentoot:+http-gateway-time-out+)))

(hunchentoot:define-easy-handler
    (store-point :uri "/phoros/lib/store-point" :default-request-type :post)
    ()
  "Receive point sent by user; store it into database."
  (assert-authentication)
  (let* ((presentation-project-name (hunchentoot:session-value
                                     'presentation-project-name))
         (user-id (hunchentoot:session-value 'user-id))
         (user-role (hunchentoot:session-value 'user-role))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
         (longitude (cdr (assoc :longitude data)))
         (latitude (cdr (assoc :latitude data)))
         (ellipsoid-height (cdr (assoc :ellipsoid-height data)))
         (stdx-global (cdr (assoc :stdx-global data)))
         (stdy-global (cdr (assoc :stdy-global data)))
         (stdz-global (cdr (assoc :stdz-global data)))
         (input-size (cdr (assoc :input-size data)))
         (attribute (cdr (assoc :attribute data)))
         (description (cdr (assoc :description data)))
         (numeric-description (cdr (assoc :numeric-description data)))
         (point-form
          (format nil "SRID=4326; POINT(~S ~S ~S)"
                  longitude latitude ellipsoid-height))
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
                                   'aux-text aux-text)))
       () "No point stored.  This should not happen."))))

(hunchentoot:define-easy-handler
    (update-point :uri "/phoros/lib/update-point" :default-request-type :post)
    ()
  "Update point sent by user in database."
  (assert-authentication)
  (let* ((presentation-project-name (hunchentoot:session-value
                                     'presentation-project-name))
         (user-id (hunchentoot:session-value 'user-id))
         (user-role (hunchentoot:session-value 'user-role))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
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
       (= 1 (execute
             (:update user-point-table-name :set
                      'user-id user-id
                      'attribute attribute
                      'description description
                      'numeric-description numeric-description
                      'creation-date 'current-timestamp
                      :where (:and (:= 'user-point-id user-point-id)
                                   (:or (:= (if (string-equal user-role
                                                              "admin")
                                                user-id
                                                'user-id)
                                            user-id)
                                        (:is-null 'user-id)
                                        (:exists
                                         (:select 'user-name
                                                  :from 'sys-user
                                                  :where (:= 'user-id
                                                             user-id))))))))
       () "No point stored.  Did you try to update someone else's point ~
             without having admin permission?"))))

(defun increment-numeric-string (text)
  "Increment rightmost numeric part of text if any; otherwise append a
three-digit numeric part."
  (let* ((end-of-number
          (1+ (or (position-if #'digit-char-p text :from-end t)
                  (1- (length text)))))
         (start-of-number
          (1+ (or (position-if-not #'digit-char-p text :from-end t
                                   :end end-of-number)
                  -1)))
         (width-of-number (- end-of-number start-of-number))
         (prefix-text (subseq text 0 start-of-number))
         (suffix-text (subseq text end-of-number)))
    (when (zerop width-of-number)
      (setf width-of-number 3))
    (format nil "~A~V,'0D~A"
            prefix-text
            width-of-number
            (1+ (or (ignore-errors
                      (parse-integer
                       text
                       :start start-of-number :end end-of-number))
                    0))
            suffix-text)))

(hunchentoot:define-easy-handler
    (uniquify-point-attributes :uri "/phoros/lib/uniquify-point-attributes"
                               :default-request-type :post)
    ()
  "Check if received set of point-attributes are unique.  If so,
return null; otherwise return (as a suggestion) a uniquified version
of point-attributes by modifying element numeric-description."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((presentation-project-name (hunchentoot:session-value
                                     'presentation-project-name))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
         (user-point-id (cdr (assoc :user-point-id data)))
         (attribute (cdr (assoc :attribute data)))
         (description (cdr (assoc :description data)))
         (numeric-description (cdr (assoc :numeric-description data)))
         (user-point-table-name
          (user-point-table-name presentation-project-name)))
    (flet ((uniquep (user-point-id attribute description numeric-description)
             "Check if given set of user-point attributes will be
              unique in database"
             (not
              (if user-point-id
                  (query
                   (:select
                    (:exists
                     (:select
                      '*
                      :from user-point-table-name
                      :where (:and (:!= 'user-point-id user-point-id)
                                   (:= 'attribute attribute)
                                   (:= 'description description)
                                   (:= 'numeric-description
                                       numeric-description)))))
                   :single!)
                  (query
                   (:select
                    (:exists
                     (:select
                      '*
                      :from user-point-table-name
                      :where (:and (:= 'attribute attribute)
                                   (:= 'description description)
                                   (:= 'numeric-description
                                       numeric-description)))))
                   :single!)))))
      (with-connection *postgresql-credentials*
        (json:encode-json-to-string
         (unless (uniquep
                  user-point-id attribute description numeric-description)
           (loop
              for s = numeric-description
              then (increment-numeric-string s)
              until (uniquep user-point-id attribute description s)
              finally
                (setf (cdr (assoc :numeric-description data))
                      s)
                (return data))))))))

(hunchentoot:define-easy-handler
    (delete-point :uri "/phoros/lib/delete-point" :default-request-type :post)
    ()
  "Delete user point if user is allowed to do so."
  (assert-authentication)
  (let* ((presentation-project-name (hunchentoot:session-value
                                     'presentation-project-name))
         (user-id (hunchentoot:session-value 'user-id))
         (user-role (hunchentoot:session-value 'user-role))
         (user-point-table-name
          (user-point-table-name presentation-project-name))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data))))
    (with-connection *postgresql-credentials*
      (assert
       (eql 1 (cond ((string-equal user-role "admin")
                     (execute (:delete-from user-point-table-name
                                            :where (:= 'user-point-id data))))
                    ((string-equal user-role "write")
                     (execute
                      (:delete-from
                       user-point-table-name
                       :where (:and
                               (:= 'user-point-id data)
                               (:or (:= 'user-id user-id)
                                    (:is-null 'user-id)
                                    (:exists
                                     (:select 'user-name
                                              :from 'sys-user
                                              :where (:= 'user-id
                                                         user-id))))))))))
       () "No point deleted.  This should not happen."))))

(defun common-table-names (presentation-project-id)
  "Return a list of common-table-names of table sets that contain data
of presentation project with presentation-project-id."
  (handler-case
      (query
       (:select 'common-table-name
                :distinct
                :from 'sys-presentation 'sys-measurement 'sys-acquisition-project
                :where (:and
                        (:= 'sys-presentation.presentation-project-id
                            presentation-project-id)
                        (:= 'sys-presentation.measurement-id
                            'sys-measurement.measurement-id)
                        (:= 'sys-measurement.acquisition-project-id
                            'sys-acquisition-project.acquisition-project-id)))
       :column)
    (condition (c)
      (cl-log:log-message
       :error
       "While fetching common-table-names of presentation-project-id ~D: ~A"
       presentation-project-id c))))

(defun encode-geojson-to-string (features &key junk-keys)
  "Encode a list of property lists into a GeoJSON FeatureCollection.
Each property list must contain keys for coordinates, :x, :y, :z; it
may contain a numeric point :id and zero or more pieces of extra
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
           features)))
      (json:encode-object-member :phoros-version (phoros-version) s))))

(defun box3d (bbox)
  "Return a WKT-compliant BOX3D string from string bbox."
  (concatenate 'string "BOX3D("
               (substitute #\Space #\,
                           (substitute #\Space #\, bbox :count 1)
                           :from-end t :count 1)
               ")"))

(hunchentoot:define-easy-handler (points :uri "/phoros/lib/points.json") (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case 
      (with-connection *postgresql-credentials*
        (let* ((presentation-project-id
                (hunchentoot:session-value 'presentation-project-id))
               (common-table-names
                (common-table-names presentation-project-id)))
          (encode-geojson-to-string
           (query
            (sql-compile
             `(:limit
               (:order-by
                (:union
                 ,@(loop
                      for common-table-name in common-table-names
                      for aggregate-view-name
                      = (point-data-table-name common-table-name)
                      ;; would have been nice, was too slow:
                      ;; = (aggregate-view-name common-table-name)
                      collect
                      `(:select
                        (:as (:st_x 'coordinates) x)
                        (:as (:st_y 'coordinates) y)
                        (:as (:st_z 'coordinates) z)
                        (:as 'point-id 'id) ;becomes fid on client
                        'random
                        :distinct-on 'random
                        :from ',aggregate-view-name
                        :natural :left-join 'sys-presentation
                        :where
                        (:and
                         (:= 'presentation-project-id
                             ,presentation-project-id)
                         (:&&
                          'coordinates
                          (:st_setsrid  (:type ,(box3d bbox) box3d)
                                        ,*standard-coordinates*))))))
                random)
               ,*number-of-features-per-layer*))
            :plists)
           :junk-keys '(:random))))
    (condition (c)
      (cl-log:log-message
       :error "While fetching points from inside bbox ~S: ~A"
       bbox c))))

(hunchentoot:define-easy-handler
    (aux-points :uri "/phoros/lib/aux-points.json")
    (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case 
      (let ((limit *number-of-features-per-layer*)
            (aux-view-name
             (aux-point-view-name (hunchentoot:session-value
                                   'presentation-project-name))))
        (encode-geojson-to-string
         (with-connection *postgresql-aux-credentials*
           (query
            (s-sql:sql-compile
             `(:limit
               (:order-by
                (:select
                 (:as (:st_x 'coordinates) 'x)
                 (:as (:st_y 'coordinates) 'y)
                 (:as (:st_z 'coordinates) 'z)
                 :from ,aux-view-name
                 :where (:&&
                         'coordinates
                         (:st_setsrid  (:type ,(box3d bbox) box3d)
                                       ,*standard-coordinates*)))
                (:random))
               ,limit))
            :plists))))
    (condition (c)
      (cl-log:log-message
       :error "While fetching aux-points from inside bbox ~S: ~A"
       bbox c))))

(hunchentoot:define-easy-handler
    (aux-local-data :uri "/phoros/lib/aux-local-data"
                    :default-request-type :post)
    ()
  "Receive coordinates, respond with the count nearest json objects
containing arrays aux-numeric, aux-text, and distance to the
coordinates received, wrapped in an array."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((aux-view-name
          (aux-point-view-name (hunchentoot:session-value
                                'presentation-project-name)))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
         (longitude (cdr (assoc :longitude data)))
         (latitude (cdr (assoc :latitude data)))
         (count (cdr (assoc :count data)))
         (point-form
          (format nil "POINT(~F ~F)" longitude latitude))
         (snap-distance 1e-3) ;about 100 m, TODO: make this a defparameter
         (bounding-box
          (format nil "~A,~A,~A,~A"
                  (- longitude snap-distance)
                  (- latitude snap-distance)
                  (+ longitude snap-distance)
                  (+ latitude snap-distance))))
    (encode-geojson-to-string
     (ignore-errors
       (with-connection *postgresql-aux-credentials*
         (nsubst
          nil :null
          (query
           (s-sql:sql-compile
            `(:limit
              (:order-by
               (:select
                (:as (:st_x 'coordinates) 'x)
                (:as (:st_y 'coordinates) 'y)
                (:as (:st_z 'coordinates) 'z)
                aux-numeric
                aux-text
                (:as
                 (:st_distance
                  (:st_transform
                   'coordinates
                   ,*spherical-mercator*)
                  (:st_transform
                   (:st_geomfromtext ,point-form ,*standard-coordinates*)
                   ,*spherical-mercator*))
                 distance)                       
                :from ',aux-view-name
                :where (:&& 'coordinates
                            (:st_setsrid (:type
                                          ,(box3d bounding-box) box3d)
                                         ,*standard-coordinates*)))
               'distance)
              ,count))
           :plists)))))))

(hunchentoot:define-easy-handler
    (aux-local-linestring :uri "/phoros/lib/aux-local-linestring.json"
                          :default-request-type :post)
    ()
  "Receive longitude, latitude, radius, and step-size; respond
with the a JSON object comprising the elements linestring (a WKT
linestring stitched together of the nearest auxiliary points from
within radius around coordinates), current-point (the point on
linestring closest to coordinates), and previous-point and next-point
\(points on linestring step-size before and after current-point
respectively)."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((thread-aux-points-function-name
          (thread-aux-points-function-name (hunchentoot:session-value
                                            'presentation-project-name)))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
         (longitude (cdr (assoc :longitude data)))
         (latitude (cdr (assoc :latitude data)))
         (radius (cdr (assoc :radius data)))
         (step-size (cdr (assoc :step-size data)))
         (azimuth (if (numberp (cdr (assoc :azimuth data)))
                      (cdr (assoc :azimuth data))
                      0))
         (point-form 
          (format nil "POINT(~F ~F)" longitude latitude))
         (sql-response
          (ignore-errors
            (with-connection *postgresql-aux-credentials*
              (nsubst
               nil :null
               (query
                (sql-compile
                 `(:select '* :from
                           (,thread-aux-points-function-name
                            (:st_geomfromtext
                             ,point-form ,*standard-coordinates*)
                            ,radius
                            ,*number-of-points-per-aux-linestring*
                            ,step-size
                            ,azimuth
                            ,(proj:degrees-to-radians 91))))
                :plist))))))
    (with-output-to-string (s)
      (json:with-object (s)
        (json:encode-object-member
         :linestring (getf sql-response :threaded-points) s)
        (json:encode-object-member
         :current-point (getf sql-response :current-point) s)
        (json:encode-object-member
         :previous-point (getf sql-response :back-point) s)
        (json:encode-object-member
         :next-point (getf sql-response :forward-point) s)
        (json:encode-object-member
         :azimuth (getf sql-response :new-azimuth) s)))))

(defun get-user-points (user-point-table-name &key
                        (bounding-box "-180,-90,180,90")
                        (limit :null)
                        (order-criterion 'id))
  "Return limit points from user-point-table-name in GeoJSON format,
and the number of points returned."
  (let ((user-point-plist
         (query
          (s-sql:sql-compile
           `(:limit
             (:order-by
              (:select
               (:as (:st_x 'coordinates) 'x)
               (:as (:st_y 'coordinates) 'y)
               (:as (:st_z 'coordinates) 'z)
               (:as 'user-point-id 'id) ;becomes fid in OpenLayers
               'stdx-global 'stdy-global 'stdz-global
               'input-size
               'attribute 'description 'numeric-description
               'user-name
               (:as (:to-char 'creation-date
                              ,*user-point-creation-date-format*)
                    'creation-date)
               'aux-numeric 'aux-text
               :from ,user-point-table-name :natural :left-join 'sys-user
               :where (:&& 'coordinates
                           (:st_setsrid  (:type ,(box3d bounding-box) box3d)
                                         ,*standard-coordinates*)))
              ,order-criterion)
             ,limit))
          :plists)))
    (values
     (encode-geojson-to-string (nsubst nil :null user-point-plist))
     (length user-point-plist))))

(hunchentoot:define-easy-handler
    (user-points :uri "/phoros/lib/user-points.json")
    (bbox)
  "Send *number-of-features-per-layer* randomly chosen GeoJSON-encoded
points from inside bbox to client.  If there is no bbox parameter,
send all points."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case 
      (let ((bounding-box (or bbox "-180,-90,180,90"))
            (limit (if bbox *number-of-features-per-layer* :null))
            (order-criterion (if bbox '(:random) 'id))
            (user-point-table-name
             (user-point-table-name (hunchentoot:session-value
                                     'presentation-project-name))))
        (with-connection *postgresql-credentials*
          (nth-value 0 (get-user-points user-point-table-name
                                        :bounding-box bounding-box
                                        :limit limit
                                        :order-criterion order-criterion))))
    (condition (c)
      (cl-log:log-message
       :error "While fetching user-points~@[ from inside bbox ~S~]: ~A"
       bbox c))))

(hunchentoot:define-easy-handler
    (user-point-attributes :uri "/phoros/lib/user-point-attributes.json")
    ()
  "Send JSON object comprising arrays attributes and descriptions,
each containing unique values called attribute and description
respectively, and count being the frequency of value in the user point
table."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case 
      (let ((user-point-table-name
             (user-point-table-name (hunchentoot:session-value
                                     'presentation-project-name))))
        (with-connection *postgresql-credentials*
          (with-output-to-string (s)
            (json:with-object (s)
              (json:as-object-member (:descriptions s)
                (json:with-array (s)
                  (mapcar #'(lambda (x) (json:as-array-member (s)
                                          (json:encode-json-plist x s)))
                          (query
                           (:limit
                            (:order-by
                             (:select 'description
                                      (:count 'description)
                                      :from user-point-table-name
                                      :group-by 'description)
                             'description)
                            100)
                           :plists))))
              (json:as-object-member (:attributes s)
                (json:with-array (s)
                  (mapcar #'(lambda (x) (json:as-array-member (s)
                                          (json:encode-json-plist x s)))
                          (query (format nil "~
                              (SELECT attribute, count(attribute) ~
                                 FROM ((SELECT attribute FROM ~A) ~
                                       UNION ALL ~
                                       (SELECT attribute ~
                                          FROM (VALUES ('solitary'), ~
                                                       ('polyline'), ~
                                                       ('polygon')) ~
                                          AS defaults(attribute))) ~
                                        AS attributes_union(attribute) ~
                                 GROUP BY attribute) ~
                              ORDER BY attribute LIMIT 100"
                                         ;; Counts of solitary,
                                         ;; polyline, polygon may be
                                         ;; to big by one if we
                                         ;; collect them like this.
                                         (s-sql:to-sql-name user-point-table-name))
                                 :plists))))))))
    (condition (c)
      (cl-log:log-message
       :error "While fetching user-point-attributes: ~A"
       c))))

(hunchentoot:define-easy-handler photo-handler
    ((bayer-pattern :init-form "65280,16711680")
     (color-raiser :init-form "1,1,1")
     (mounting-angle :init-form "0")
     brightenp)
  "Serve an image from a .pictures file."
  (assert-authentication)
  (handler-case
      (prog2
          (progn
            (push (bt:current-thread)
                  (hunchentoot:session-value 'recent-threads))
            (incf (hunchentoot:session-value 'number-of-threads)))
          (let* ((s
                  (cl-utilities:split-sequence #\/
                                               (hunchentoot:script-name*)
                                               :remove-empty-subseqs t))
                 (directory
                  (cdddr            ;remove leading phoros, lib, photo
                   (butlast s 2)))
                 (file-name-and-type
                  (cl-utilities:split-sequence #\. (first (last s 2))))                     
                 (byte-position
                  (parse-integer (car (last s)) :junk-allowed t))
                 (path-to-file
                  (car
                   (directory
                    (make-pathname
                     :directory (append (pathname-directory *common-root*)
                                        directory
                                        '(:wild-inferiors))
                     :name (first file-name-and-type)
                     :type (second file-name-and-type)))))
                 (result
                  (flex:with-output-to-sequence (stream)
                    (send-png
                     stream path-to-file byte-position
                     :bayer-pattern
                     (apply #'vector (mapcar
                                      #'parse-integer
                                      (cl-utilities:split-sequence
                                       #\, bayer-pattern)))
                     :color-raiser
                     (apply #'vector (mapcar
                                      #'parse-number:parse-positive-real-number
                                      (cl-utilities:split-sequence
                                       #\,
                                       color-raiser)))
                     :reversep (= 180 (parse-integer mounting-angle))
                     :brightenp brightenp))))
            (setf (hunchentoot:header-out 'cache-control)
                  (format nil "max-age=~D" *browser-cache-max-age*))
            (setf (hunchentoot:content-type*) "image/png")
            result)
        (decf (hunchentoot:session-value 'number-of-threads)))
    (superseded ()
      (setf (hunchentoot:return-code*) hunchentoot:+http-gateway-time-out+)
      ;; (decf (hunchentoot:session-value 'number-of-threads))
      nil)
    (condition (c)
      (cl-log:log-message
       :error "While serving image ~S: ~A" (hunchentoot:request-uri*) c))))

(pushnew (hunchentoot:create-prefix-dispatcher "/phoros/lib/photo"
                                               'photo-handler)
         hunchentoot:*dispatch-table*)

;;; for debugging; this is the multi-file OpenLayers
(pushnew (hunchentoot:create-folder-dispatcher-and-handler
          "/phoros/lib/openlayers/" "OpenLayers-2.10/")
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-folder-dispatcher-and-handler
          "/phoros/lib/ol/" "ol/")
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-folder-dispatcher-and-handler
          "/phoros/lib/public_html/" "public_html/")
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-static-file-dispatcher-and-handler
          "/favicon.ico" "public_html/favicon.ico")
         hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler
    (view :uri (format nil "/phoros/lib/view-~A" (phoros-version))
          :default-request-type :post)
    ()
  "Serve the client their main workspace."
  (if
   (hunchentoot:session-value 'authenticated-p)
   (who:with-html-output-to-string (s nil :prologue t :indent t)
     (:html
      (:head
       (:title (who:str
                (concatenate
                 'string
                 "Phoros: " (hunchentoot:session-value
                             'presentation-project-name))))
       (if *use-multi-file-openlayers*
           (who:htm
            (:script
             :src (format nil "/~A/lib/openlayers/lib/Firebug/firebug.js"
                          *proxy-root*))
            (:script
             :src (format nil "/~A/lib/openlayers/lib/OpenLayers.js"
                          *proxy-root*)))
           (who:htm
            (:script
             :src (format nil "/~A/lib/ol/OpenLayers.js"
                          *proxy-root*))))
       (:link :rel "stylesheet"
              :href (format nil "/~A/lib/css-~A/style.css"
                            *proxy-root*
                            (phoros-version))
              :type "text/css")
       (:script :src (format         ;variability in script name is
                      nil            ; supposed to fight browser cache
                      "/~A/lib/phoros-~A-~A-~A.js"
                      *proxy-root*
                      (phoros-version)
                      (hunchentoot:session-value 'user-name)
                      (hunchentoot:session-value 'presentation-project-name)))
       (:script :src "http://maps.google.com/maps/api/js?sensor=false"))
      (:body
       :onload (ps (init))
       (:noscript (:b (:em "You can't do much without JavaScript here.")))
       (:h1 :id "title"
            "Phoros: " (who:str (hunchentoot:session-value 'user-full-name))
            (who:fmt " (~A)" (hunchentoot:session-value 'user-name))
            "with " (:span :id "user-role"
                           (who:str (hunchentoot:session-value 'user-role)))
            "permission on "
            (:span :id "presentation-project-name"
                   (who:str (hunchentoot:session-value
                             'presentation-project-name)))
            (:span :id "presentation-project-emptiness")
            (:span :id "recommend-fresh-login")
            (:span :class "h1-right"
                   (:span :id "caching-indicator")
                   (:span :id "phoros-version"
                          (who:fmt "v~A" (phoros-version)))))
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
       (:div :class "phoros-controls" :id "phoros-controls"
             (:div :id "real-phoros-controls"
                   (:h2 (:span :id "h2-controls") (:span :id "creator"))
                   (:div :id "point-attribute"
                         :class "combobox"
                         (:select :id "point-attribute-select"
                                  :name "point-attribute-select"
                                  :class "combobox-select"
                                  :onchange (ps-inline
                                             (consolidate-combobox
                                              "point-attribute"))
                                  :disabled t)
                         (:input :id "point-attribute-input"
                                 :name "point-attribute-input"
                                 :class "combobox-input"
                                 :onchange (ps-inline
                                            (unselect-combobox-selection
                                             "point-attribute"))
                                 :disabled t
                                 :type "text"))
                   (:input :id "point-numeric-description"
                           :class "vanilla-input"
                           :disabled t
                           :type "text" :name "point-numeric-description")

                   (:div :id "point-description"
                         :class "combobox"
                         (:select :id "point-description-select"
                                  :name "point-description-select"
                                  :class "combobox-select"
                                  :onchange (ps-inline
                                             (consolidate-combobox
                                              "point-description"))
                                  :disabled t)
                         (:input :id "point-description-input"
                                 :name "point-description-input"
                                 :class "combobox-input"
                                 :onchange (ps-inline
                                            (unselect-combobox-selection
                                             "point-description"))
                                 :disabled t
                                 :type "text"))
                   (:button :id "delete-point-button" :disabled t
                            :type "button"
                            :onclick (ps-inline (delete-point))
                            "del")
                   (:button :disabled t :id "finish-point-button"
                            :type "button"
                            (:b "finish"))
                   (:div :id "uniquify-buttons"
                         (:button :id "suggest-unique-button"
                                  :type "button"
                                  :onclick (ps-inline
                                            (insert-unique-suggestion))
                                  (:b "suggest"))
                         (:button :id "force-duplicate-button"
                                  :type "button"
                                  "push"))
                   (:div :id "aux-point-distance-or-point-creation-date"
                         (:code :id "point-creation-date")
                         (:select :id "aux-point-distance" :disabled t
                                  :size 1 :name "aux-point-distance"
                                  :onchange (ps-inline
                                             (aux-point-distance-selected))
                                  :onclick (ps-inline
                                            (enable-aux-point-selection)))
                         (:div :id "include-aux-data"
                               (:label
                                (:input :id "include-aux-data-p"
                                        :class "tight-input"
                                        :type "checkbox" :checked t
                                        :name "include-aux-data-p"
                                        :onchange (ps-inline
                                                   (flip-aux-data-inclusion)))
                                "aux data")))
                   (:div :id "aux-data"
                         (:div :id "aux-numeric-list")
                         (:div :id "aux-text-list")))
             (:div :id "multiple-points-phoros-controls"
                   (:h2 "Multiple Points Selected")
                   (:p "You have selected multiple user points.")
                   (:p "Unselect all but one to edit or view its properties."))
             (:div :class "walk-mode-controls"
                   (:div :id "walk-mode"
                         (:input :id "walk-p"
                                 :class "tight-input"
                                 :type "checkbox" :checked nil
                                 :onchange (ps-inline
                                            (flip-walk-mode)))
                         (:label :for "walk-p"
                                 "snap+walk"))
                   (:div :id "decrease-step-size"
                         :onclick (ps-inline (decrease-step-size)))
                   (:div :id "step-size"
                         :onclick (ps-inline (increase-step-size))
                         "4")
                   (:div :id "increase-step-size"
                         :onclick (ps-inline (increase-step-size))
                         :ondblclick (ps-inline (increase-step-size)
                                                (increase-step-size)))
                   (:div :id "step-button" :disabled nil
                         :onclick (ps-inline (step))
                         :ondblclick (ps-inline (step t))
                         "step"))
             (:div :class "image-main-controls"
                   (:div :id "auto-zoom"
                         (:input :id "zoom-to-point-p"
                                 :class "tight-input"
                                 :type "checkbox" :checked t)
                         (:label :for "zoom-to-point-p"
                                 "auto"))
                   (:div :id "brighten-images"
                         (:input :id "brighten-images-p"
                                 :class "tight-input"
                                 :type "checkbox" :checked nil)
                         (:label :for "brighten-images-p"
                                 "bright"))
                   (:div :id "zoom-images-to-max-extent"
                         :onclick (ps-inline (zoom-images-to-max-extent)))
                   (:div :id "no-footprints-p"
                         (:b "?"))
                   (:div :id "remove-work-layers-button" :disabled t
                         :onclick (ps-inline (reset-layers-and-controls))
                         "restart")))
       (:div :class "help-div"
             (:button :id "download-user-points-button"
                      :type "button"
                      :onclick (format nil "self.location.href = \"/~A/lib/user-points.json\""
                                       *proxy-root*)
                      "download points") ;TODO: offer other formats and maybe projections
             (:button :id "blurb-button"
                      :type "button"
                      :onclick (ps-inline
                                (chain window
                                       (open
                                        (+ "/"
                                           +proxy-root+
                                           "/lib/blurb?openlayers-version="
                                           (@ *open-layers *version_number*))
                                        "About Phoros")))
                      (:img :src (format nil "/~A/lib/public_html/phoros-logo-plain.png"
                                         *proxy-root*)
                            :alt "Phoros" :style "vertical-align:middle"
                            :height 20))
             (:button :id "logout-button"
                      :type "button"
                      :onclick (ps-inline (bye))
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
                             (:div :id (format nil "image-~S-usable" i)
                                   :class "image-usable"
                                   (:b "!"))
                             (:div :id (format nil "image-~S-trigger-time" i)
                                   :class "image-trigger-time"))
                       (:div :id (format nil "image-~S" i)
                             :class "image" :style "cursor:crosshair"))))))))
   (hunchentoot:redirect
    (format nil "/~A/~A"
            *proxy-root*
            (hunchentoot:session-value 'presentation-project-name))
    :add-session-id t)))

(hunchentoot:define-easy-handler
    (epipolar-line :uri "/phoros/lib/epipolar-line")
    ()
  "Receive vector of two sets of picture parameters, the first of
which containing coordinates (m, n) of a clicked point. Respond with a
JSON encoded epipolar-line."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((data (json:decode-json-from-string (hunchentoot:raw-post-data))))
    (json:encode-json-to-string
     (photogrammetry :epipolar-line (first data) (second data)))))

(hunchentoot:define-easy-handler
    (estimated-positions :uri "/phoros/lib/estimated-positions")
    ()
  "Receive a two-part JSON vector comprising (1) a vector containing
sets of picture-parameters with clicked (\"active\") points
stored in :m, :n; and (2) a vector containing sets of
picture-parameters; respond with a JSON encoded two-part vector
comprising (1) a point in global coordinates; and (2) a vector of
image coordinates (m, n) for the global point that correspond to the
images from the received second vector.  TODO: report error on bad
data (ex: points too far apart)."
  ;; TODO: global-point-for-display should probably contain a proj string in order to make sense of the (cartesian) standard deviations.
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((data
          (json:decode-json-from-string (hunchentoot:raw-post-data)))
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
         (global-point-for-display ;points geographic cs, degrees; std deviations in cartesian cs
          (pairlis '(:longitude :latitude :ellipsoid-height
                     :stdx-global :stdy-global :stdz-global
                     :input-size)
                   (list
                    (proj:radians-to-degrees
                     (first global-point-geographic-radians))
                    (proj:radians-to-degrees
                     (second global-point-geographic-radians))
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
     (list global-point-for-display image-coordinates))))

(hunchentoot:define-easy-handler
    (user-point-positions :uri "/phoros/lib/user-point-positions")
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
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((user-point-table-name
          (user-point-table-name (hunchentoot:session-value
                                  'presentation-project-name)))
         (data (json:decode-json-from-string (hunchentoot:raw-post-data)))
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
              (:as (:st_x 'coordinates) 'longitude)
              (:as (:st_y 'coordinates) 'latitude)
              (:as (:st_z 'coordinates) 'ellipsoid-height)
              (:as 'user-point-id 'id)  ;becomes fid on client
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
              :junk-keys '(:longitude :latitude :ellipsoid-height)))))
    (with-output-to-string (s)
      (json:with-object (s)
        (json:encode-object-member :user-point-count user-point-count s)
        (json:as-object-member (:image-points s)
          (json:with-array (s)
            (loop for i in image-coordinates do
                 (json:as-array-member (s) (princ i s)))))))))

(hunchentoot:define-easy-handler
    (multi-position-intersection :uri "/phoros/lib/intersection")
    ()
  "Receive vector of sets of picture parameters, respond with stuff."
  (assert-authentication)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((data (json:decode-json-from-string (hunchentoot:raw-post-data))))
    (json:encode-json-to-string
     (photogrammetry :multi-position-intersection data))))
