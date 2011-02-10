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

(defparameter *t* nil)
(defparameter *tt* nil)

(cffi:define-foreign-library photogrammetrie
  (:unix (:or "./libphotogrammetrie.so"
              "../photogrammetrie/lib/libphotogrammetrie.so"))
  (t (:default "libphotogrammetrie")))

(defparameter *standard-coordinates* 4326
  "EPSG code of the coordinate system that we use for communication.")

(defparameter *postgresql-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl)")

(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))

(setf *read-default-float-format* 'double-float)

(defparameter *phoros-server* nil "Hunchentoot acceptor.")
(defparameter *common-root* nil "Root directory; contains directories of measuring data.")
(defparameter *verbose* 0 "Integer (interpreted as a bit mask) denoting various kinds of debugging output.")

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
  "First HTTP contact: if necessary, check credentials, establish new session."
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
        ((null presentation-project-id) "No such project.")
        ((and (equal (session-value 'presentation-project-name) presentation-project-name)
              (session-value 'authenticated-p))
         (redirect "/view" :add-session-id t))
        (t
         (progn
           (setf (session-value 'presentation-project-name) presentation-project-name
                 (session-value 'presentation-project-id) presentation-project-id)
           (who:with-html-output-to-string (s nil :prologue t :indent t)
             (:form :method "post" :enctype "multipart/form-data"
                    :action "/authenticate"
                    "User:" :br
                    (:input :type "text" :name "user-name") :br
                    "Password:" :br
                    (:input :type "password" :name "user-password") :br
                    (:input :type "submit" :value "Submit")))))))))
      
(pushnew (create-prefix-dispatcher "/phoros/" 'phoros-handler)
         *dispatch-table*)

(define-easy-handler (authenticate-handler :uri "/authenticate" :default-request-type :post) ()
  (with-connection *postgresql-credentials*
    (let* ((user-name (post-parameter "user-name"))
           (user-password (post-parameter "user-password"))
           (presentation-project-id (session-value 'presentation-project-id))
           (user-full-name
            (when presentation-project-id
              (query
               (:select 'user-full-name
                        :from 'sys-user-role 'sys-user
                        :where (:and
                                (:= 'presentation-project-id presentation-project-id)
                                (:= 'sys-user-role.user-id 'sys-user.user-id)
                                (:= 'user-name user-name)
                                (:= 'user-password user-password)))
               :single))))
      (if user-full-name
          (progn
            (setf (session-value 'authenticated-p) t
                  (session-value 'user-name) user-name
                  (session-value 'user-full-name) user-full-name)
            (redirect "/view" :add-session-id t))
          "Rejected."))))

(define-easy-handler (logout :uri "/logout") ()
  (if (session-verify *request*)
      (progn (remove-session *session*)
             "Bye.")
      "Bye (again)."))

(define-easy-handler (test :uri "/test") ()
  "Authenticated.")

(define-easy-handler (local-data :uri "/local-data" :default-request-type :post) ()
  "Receive coordinates, respond with the count nearest json objects containing picture url, calibration parameters, and car position, wrapped in an array."
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
    (condition (c) (cl-log:log-message :server "While fetching common-table-names of presentation-project-id ~D: ~A" presentation-project-id c))))

(define-easy-handler (points :uri "/points") (bbox)
  "Send a bunch of GeoJSON-encoded points from inside bbox to client."
  (when (session-value 'authenticated-p)
    (handler-case 
        (let ((box3d-form
               (concatenate 'string "BOX3D("
                            (substitute #\Space #\,
                                        (substitute #\Space #\, bbox :count 1)
                                        :from-end t :count 1)
                            ")"))
              (common-table-names (common-table-names (session-value 'presentation-project-id))))
          (with-connection *postgresql-credentials*
            (json:encode-json-alist-to-string
             (acons
              'type '*geometry-collection
              (acons 'geometries
                     (mapcar
                      #'(lambda (x)
                          (acons 'type '*point
                                 (acons 'coordinates x nil)))
                      (loop
                         for common-table-name in common-table-names
                         for point-table-name = (make-symbol (concatenate 'string "dat-" common-table-name "-point"))
                         append 
                           (query (:select (:st_x (:st_transform 'coordinates *standard-coordinates*))
                                           (:st_y (:st_transform 'coordinates *standard-coordinates*))
                                           :from point-table-name
                                           :where (:&& (:st_transform 'coordinates *standard-coordinates*)
                                                       (:st_setsrid  (:type box3d-form box3d) *standard-coordinates*))))))
                     nil)))))
      (condition (c) (cl-log:log-message :server "While fetching points from inside bbox ~S: ~A" bbox c)))))

(define-easy-handler photo-handler
    ((bayer-pattern :init-form "#00ff00,#ff0000")
     (color-raiser :init-form "1,1,1"))
  "Serve an image from a .pictures file."
  (when (session-value 'authenticated-p)
    (handler-case
        (let* ((s (cdr (cl-utilities:split-sequence #\/ (script-name*)
                                                    :remove-empty-subseqs t)))
               (directory (butlast s 2))
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
      (condition (c) (cl-log:log-message :server "While serving image ~S: ~A" (request-uri*) c)))))

(pushnew (create-prefix-dispatcher "/photo" 'photo-handler)
         *dispatch-table*)

(pushnew (create-folder-dispatcher-and-handler "/lib/" "") ;TODO: is this secure enough?
         *dispatch-table*)

(define-easy-handler (phoros.js :uri "/phoros.js") ()
  "Serve some Javascript."
  (when (session-value 'authenticated-p)
    (ps

      (setf
       (@ *open-layers *control *click)
       ((@ *open-layers *class) 
        (@ *open-layers *control)
        (create :default-handler-options
                (create :single t
                        :double false
                        :pixel-tolerance 0
                        :stop-single false
                        :stop-double false)
                :initialize
                (lambda (options)
                  (setf 
                   (@ this handler-options) ((@ *open-layers *util extend)
                                             (create)
                                             (@ this default-handler-options)))
                  ((@ *open-layers *control prototype initialize apply)
                   this arguments)
                  (setf (@ this handler)
                        (new ((@ *open-layers *handler *click) this
                              (create :click (@ this trigger))
                              (@ this handler-options))))))))

      (setf geographic
            (new ((@ *open-layers *projection) "EPSG:4326")))
      (setf spherical-mercator
            (new ((@ *open-layers *projection) "EPSG:900913")))
          
      (defvar images (array) "Collection of the photos currently shown.")
      (defvar streetmap "The streetmap shown to the user.")
      (defvar streetmap-estimated-position-layer)
              
      (defun *image ()
        "Anything necessary to deal with a photo."
        (setf (getprop this 'map)
              (new ((getprop *open-layers '*map)
                    (create projection spherical-mercator
                            all-overlays t)))
              (getprop this 'dummy) false ;TODO why? (omitting splices map components directly into *image)
              ))

      (setf (getprop *image 'prototype 'show-photo) show-photo)
      (setf (getprop *image 'prototype 'draw-epipolar-line) draw-epipolar-line)
      (setf (getprop *image 'prototype 'draw-active-point) draw-active-point)
      (setf (getprop *image 'prototype 'draw-estimated-positions)
            draw-estimated-positions)

      (defun photo-path (photo-parameters)
        "Create from stuff found in photo-parameters a path for use in
a image url."
        (+ "/photo/" (@ photo-parameters directory) "/"
           (@ photo-parameters filename) "/"
           (@ photo-parameters byte-position) ".png"))

      (defun has-layer-p (map layer-name)
        "False if map doesn't have a layer called layer-name."
        (chain map (get-layers-by-name layer-name) length))

      (defun some-active-point-p ()
        "False if no image in images has an Active Point."
        (loop
           for i across images
           sum (has-layer-p (getprop i 'map) "Active Point")))

      (defun remove-layer (map layer-name)
        "Destroy layer layer-name in map."
        (when (has-layer-p map layer-name)
          (chain map (get-layers-by-name layer-name) 0 (destroy))))

      (defun remove-any-layers (layer-name)
        "Destroy in all images and in streetmap the layer named layer-name."
        (loop
           for i across images do (remove-layer (getprop i 'map) layer-name))
        (remove-layer streetmap layer-name))

      (defun remove-work-layers ()
        "Destroy user-generated layers in streetmap and in all images."
        (remove-any-layers "Epipolar Line")
        (remove-any-layers "Active Point")
        (remove-any-layers "Estimated Position")
        (setf pristine-images-p t))

      (defun present-photos ()
        "Handle the response triggered by request-photos."
        (let ((photo-parameters ((@ *json* parse)
                                 (@ photo-request-response response-text))))
          (loop
             for p across photo-parameters
             for i across images
             do
               (setf (getprop i 'photo-parameters) p)
               ((getprop i 'show-photo)))
          ;; (setf (@ (aref photo-parameters 0) angle180) 1) ; Debug: coordinate flipping
          ))

      (defun request-photos (event)
        "Handle the response to a click into streetmap; fetch photo data."
        (remove-any-layers "Estimated Position")
        (let ((lonlat
               ((@ ((@ streetmap get-lon-lat-from-pixel) (@ event xy)) transform)
                spherical-mercator      ; why?
                geographic)))
          (setf content
                ((@ *json* stringify)
                 (create :longitude (@ lonlat lon) ; TODO: use OpenLayer's JSON.
                         :latitude (@ lonlat lat)
                         :zoom ((@ streetmap get-zoom))
                         :count 6)))
          (setf photo-request-response
                ((@ *open-layers *Request *POST*)
                 (create :url "local-data"
                         :data content
                         :headers (create "Content-type" "text/plain"
                                          "Content-length" (@ content length))
                         :success present-photos)))))

      (defun draw-epipolar-line ()
        "Draw an epipolar line from response triggered by clicking
into a (first) photo."
        (let ((epipolar-line ((@ *json* parse)
                              (@ this epipolar-request-response response-text))))
          (chain this epipolar-layer
                 (add-features
                  (new ((@ *open-layers *feature *vector)
                        (new ((@ *open-layers *geometry *line-string)
                              ((@ epipolar-line map)
                               (lambda (x)
                                 (new ((@ *open-layers *geometry *point)
                                       (@ x :m) (@ x :n)))))))))))))
      ;; either *line-string or *multi-point are usable
    
      (defun draw-estimated-positions ()
        "Draw into streetmap and into all images points at Estimated
Position.  Estimated Position is the point returned so far from
photogrammetric calculations that are triggered by clicking into
another photo."
        (let* ((estimated-positions-request-response
                ((@ *json* parse)
                 (getprop this
                          'estimated-positions-request-response
                          'response-text)))
               (global-position
                (aref estimated-positions-request-response 0))
               (estimated-positions
                (aref estimated-positions-request-response 1)))
          ((@ console log) global-position)
          (setf streetmap-estimated-position-layer
                (new ((@ *open-layers *layer *vector) "Estimated Position")))
          (chain streetmap-estimated-position-layer
                 (add-features
                  (new ((@ *open-layers *feature *vector)
                        ((@ (new ((@ *open-layers *geometry *point)
                                  (getprop global-position 'longitude)
                                  (getprop global-position 'latitude)))
                            transform) geographic spherical-mercator)))))
          ((@ streetmap add-layer) streetmap-estimated-position-layer)
          (loop
             for i in images
             for p in estimated-positions
             do
             (setf (@ i estimated-position-layer)
                   (new ((@ *open-layers *layer *vector) "Estimated Position")))
             ((@ i map add-layer) (@ i estimated-position-layer))
             (chain i estimated-position-layer
                    (add-features
                     (new ((@ *open-layers *feature *vector)
                           (new ((@ *open-layers *geometry *point)
                                 (getprop p 'm)
                                 (getprop p 'n))))))))))

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
            (setf pristine-images-p (not (some-active-point-p)))
            (setf (@ clicked-image active-point-layer)
                  (new ((@ *open-layers *layer *vector) "Active Point")))
            ((@ clicked-image map add-layer)
             (@ clicked-image active-point-layer))
            ((getprop clicked-image 'draw-active-point))
            (if
             pristine-images-p
             (progn
               (loop
                  for i across images do
                  (unless (== i clicked-image)
                    (setf
                     (@ i epipolar-layer) (new ((@ *open-layers *layer *vector)
                                                "Epipolar Line"))
                     content ((@ *json* stringify)
                              (append (array photo-parameters)
                                      (@ i photo-parameters)))
                     (@ i epipolar-request-response)
                     ((@ *open-layers *Request *POST*)
                      (create :url "epipolar-line"
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
                             for i across images
                             when (has-layer-p (getprop i 'map) "Active Point")
                             collect (getprop i 'photo-parameters)))
                         (content
                          ((@ *json* stringify)
                           (list active-pointed-photo-parameters
                                 (chain images
                                        (map #'(lambda (x)
                                                 (getprop
                                                  x 'photo-parameters))))))))
                    (setf (@ clicked-image estimated-positions-request-response)
                          ((@ *open-layers *Request *POST*)
                           (create :url "estimated-positions"
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
        ((getprop this 'map 'zoom-to-extent)
         (new ((@ *open-layers *bounds) -.5 -.5 
               (1+ (getprop this 'photo-parameters 'sensor-width-pix))
               (1+ (getprop this 'photo-parameters 'sensor-height-pix)))))) ; in coordinates shown

      (defun initialize-image (image-index)
        "Create an image usable for displaying photos at position
image-index in array images."
        (setf (aref images image-index) (new *image))
        (setf (@ (aref images image-index) image-click-action)
              (image-click-action (aref images image-index)))
        (setf (@ (aref images image-index) click)
              (new ((@ *open-layers *control *click)
                    (create :trigger (@ (aref images image-index)
                                        image-click-action)))))
        ((@ (aref images image-index) map add-control)
         (@ (aref images image-index) click))
        ((@ (aref images image-index) click activate))
        ((@ (aref images image-index) map add-control)
         (new ((@ *open-layers *control *mouse-position))))
        ((@ (aref images image-index) map add-control)
         (new ((@ *open-layers *control *layer-switcher))))
        ((@ (aref images image-index) map render) (+ image-index "")))        
      
      (defun init ()
        "Prepare user's playground."
        (setf streetmap (new ((@ *open-layers *map) "streetmap"
                        (create projection geographic
                                display-projection geographic))))
        (let* ((survey-layer
                (new ((@ *open-layers *layer *vector) "Survey"
                      (create :strategies
                              (array (new ((@ *open-layers *strategy *bbox*)
                                           (create :ratio 1.1))))
                              :protocol
                              (new ((@ *open-layers *protocol *http*)
                                    (create
                                     :url "points"
                                     :format
                                     (new ((@ *open-layers *format *geo-j-s-o-n)
                                           (create
                                            external-projection geographic
                                            internal-projection geographic))))))))))
               ;;(google (new ((@ *open-layers *Layer *google) "Google Streets")))
               (osm-layer (new ((@ *open-layers *layer *osm*))))
               (click-streetmap (new ((@ *open-layers *control *click)
                                      (create :trigger request-photos)))))
          ((@ streetmap add-control) click-streetmap)
          ((@ click-streetmap activate))
          ;;((@ map add-layers) (array osm-layer google survey-layer))
          ((@ streetmap add-layers) (array survey-layer osm-layer))
          ((@ streetmap add-control)
           (new ((@ *open-layers *control *layer-switcher))))
          ((@ streetmap add-control)
           (new ((@ *open-layers *control *mouse-position))))
          ((@ streetmap zoom-to-extent)
           ((@ (new ((@ *open-layers *bounds)
                    14.32066 51.72693 14.32608 51.72862))
               transform) geographic spherical-mercator)))
        (loop
           for i from 0 to 3
           do
           (initialize-image i))))))

(define-easy-handler (view :uri "/view" :default-request-type :post) ()
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
       ;;(:link :rel "stylesheet" :href "lib/theme/default/style.css" :type "text/css")
       (:link :rel "stylesheet" :href "lib/style.css" :type "text/css")
       (:script :src "lib/openlayers/lib/OpenLayers.js")
       (:script :src "lib/openlayers/lib/proj4js.js") ;TODO: we should be able to make this redundant.
       (:script :src "/phoros.js")
       ;;(:script :src "http://maps.google.com/maps/api/js?sensor=false")
       )
      (:body :onload (ps (init))
             (:h1 :id "title" (who:str (concatenate 'string "Phoros: " (session-value 'presentation-project-name))))
             (:p :id "shortdesc"
                 "unfinished prototype")
             (:div :id "finish-point-button" :style "float:left" (:button :type "button" :onclick (ps ()) "finish point"))
             (:div :id "remove-work-layers-button" :style "float:left" (:button :type "button" :onclick (ps (remove-work-layers)) "start over (keep photos)"))
             (:div :id "blurb-button" :style "float:left" (:button :type "button" :onclick "self.location.href = \"/blurb\"" "blurb"))
             (:div :id "logout-button" :style "float:left" (:button :type "button" :onclick "self.location.href = \"/logout\"" "bye"))
             
             (:div :style "clear:both"
                   (:div :id "streetmap" :class "smallmap" :style "float:left")
                   (loop
                      for i from 0 to 3 do 
                        (who:htm (:div :id i :class "image" :style "float:left")))))))
   (redirect
    (concatenate 'string "/phoros/" (session-value 'presentation-project-name))
    :add-session-id t)))

(define-easy-handler (epipolar-line :uri "/epipolar-line") ()
  "Receive vector of two sets of picture parameters, respond with
JSON encoded epipolar-lines."
  (when (session-value 'authenticated-p)
    (let* ((data (json:decode-json-from-string (raw-post-data))))
      (json:encode-json-to-string (photogrammetry :epipolar-line (first data) (second data))))))

(define-easy-handler (estimated-positions :uri "/estimated-positions") ()
  "Receive a two-part JSON vector comprising (1) a vector containing
sets of picture-parameters including clicked points stored in :m, :n;
and (2) a vector containing sets of picture-parameters; respond with
a JSON encoded two-part vector comprising (1) a point in global
coordinates; and (2) a vector of image coordinates (m, n) for the
global point that correspond to the images from the received second
vector.  TODO: report error on bad data (ex: points too far apart)."
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
               collect (photogrammetry :reprojection i global-point-cartesian))))
      (json:encode-json-to-string
        (list global-point-for-display image-coordinates)))))

(define-easy-handler (multi-position-intersection :uri "/intersection") ()
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
  
