;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2010 Bert Burgemeister
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
  (t (:or "../photogrammetrie/lib/libphotogrammetrie.so"
          "libphotogrammetrie.so")))

(defparameter *standard-coordinates* 4326 "EPSG code of the coordinate system that we use for communication.")
(defparameter *postgresql-credentials* nil "A list: (database user password host &key (port 5432) use-ssl)")
(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))
(setf *read-default-float-format* 'double-float)

(defparameter *phoros-server* nil "Hunchentoot acceptor.")
(defparameter *common-root* nil "Root directory; contains directories of measuring data.")
(defparameter *verbose* 0 "Integer denoting increasing amounts of debugging output.")

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
  (setf *show-lisp-errors-p* t)      ;TODO: tie this to --debug option
  ;; Doesn't seem to exist(setf *show-lisp-backtraces-p* t)  ;TODO: tie this to --debug option
  (setf *message-log-pathname* "hunchentoot-messages.log") ;TODO: try using cl-log
  (setf *access-log-pathname* "hunchentoot-access.log") ;TODO: try using cl-log
  (check-db *postgresql-credentials*)
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
           (with-html-output-to-string (s nil :prologue t :indent t)
             (:form :method "post" :enctype "multipart/form-data"
                    :action "/authenticate"
                    "User:" :br
                    (:input :type "text" :name "user-name") :br
                    "Password:" :br
                    (:input :type "password" :name "user-password") :br
                    (:input :type "submit" :value "Submit")))))))))
      
(pushnew (create-prefix-dispatcher "/phoros" 'phoros-handler)
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
    (condition (c) (cl-log:log-message :server "While serving image ~S: ~A" (request-uri*) c))))

(pushnew (create-prefix-dispatcher "/photo" 'photo-handler)
         *dispatch-table*)

(pushnew (create-folder-dispatcher-and-handler "/lib/" "/home/bertb/lisphack/phoros/")
         *dispatch-table*)

(define-easy-handler (view :uri "/view" :default-request-type :post) ()
  (if (session-value 'authenticated-p)
      (with-html-output-to-string (s nil :indent t)
        (:html
         :xmlns
         "http://www.w3.org/1999/xhtml"
         (:head
          (:title (str (concatenate 'string "Phoros: " (session-value 'presentation-project-name))))
          (:link :rel "stylesheet" :href "lib/theme/default/style.css" :type "text/css")
          (:link :rel "stylesheet" :href "lib/style.css" :type "text/css")
          (:script :src "lib/openlayers/lib/OpenLayers.js")
          (:script :src "lib/openlayers/lib/proj4js.js")
          ;;(:script :src "http://maps.google.com/maps/api/js?sensor=false")
          (:script
           :type "text/javascript"
           (str
            (ps
              (setf (@ *open-layers *control *click)
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
                               ((@ *open-layers *control prototype initialize apply) this arguments)
                               (setf (@ this handler)
                                     (new ((@ *open-layers *handler *click) this
                                           (create :click (@ this trigger))
                                           (@ this handler-options))))))))
              (defun photo-path (photo-parameters)
                (+ "/photo/" (@ photo-parameters directory) "/" (@ photo-parameters filename) "/" (@ photo-parameters byte-position) ".png"))

              (defun map-click-handler ()
                (let ((photo-parameters ((@ *json* parse) (@ photo-request response-text))))
                  (show-photo (aref photo-parameters 0) (aref image 0))
                  ;; (setf (@ (aref photo-parameters 0) angle180) 1) ; Debug: coordinate flipping
                  (show-photo (aref photo-parameters 1) (aref image 1))))

              (defun request-photo (e)
                (let ((lonlat
                       ((@ ((@ map get-lon-lat-from-pixel) (@ e xy))
                           transform)
                        (new ((@ *open-layers *projection) "EPSG:900913")) ; why?
                        (new ((@ *open-layers *projection) "EPSG:4326")))))
              
                  (setf content ((@ *json* stringify) (create :longitude (@ lonlat lon) ; TODO: use OpenLayer's JSON.
                                                              :latitude (@ lonlat lat)
                                                              :zoom ((@ map get-zoom))
                                                              :count 2)) ; that's left and right
                        photo-request ((@ *open-layers *Request *POST*)
                                       (create :url "local-data"
                                               :data content
                                               :headers (create "Content-type" "text/plain"
                                                                "Content-length" (@ content length))
                                               :success map-click-handler)))))


              (defun epipolar-handler ()
                (let ((epi ((@ *json* parse) (@ this epipolar-request response-text))))
                  ((@ console log) "EPI:")
                  ((@ console log) epi)
                  ((@ this epipolar-layer add-features)
                   (new ((@ *open-layers *feature *vector)
                         (new ((@ *open-layers *geometry *line-string) ((@ epi map) (lambda (x) (new ((@ *open-layers *geometry *point) (@ x :m) (@ x :n))))))))))))
              ;; either *line-string or *multi-point are usable

              (defun request-epipolar-lines* (e)
                (let* ((lonlat
                        ((@ (@ this map) get-lon-lat-from-view-port-px) (@ e xy)))
                       (photo-parameters (@ (@ this map) layers 0 photo-parameters))
                       content
                       request)
                  ((@ console log) "Photo-Parameters (naked/from this):")
                  ((@ console log) photo-parameters)
                  (setf (@ photo-parameters m) (@ lonlat lon)
                        (@ photo-parameters n) (@ lonlat lat))
                  ((@ console log) "m, n:")
                  ((@ console log) (@ lonlat lon))
                  ((@ console log) (@ lonlat lat))
                  ((@ console log) (@ (@ this map) layers 0 photo-parameters))
                  (loop
                     for i across image
                     do
                     ((@ console log) "unsorted image window: ") ((@ console log) i)
                     (when (> (@ ((@ i get-layers-by-name) "Epipolar Line") length) 0)
                       ((@ ((@ i get-layers-by-name) "Epipolar Line") 0 destroy)))

                     when (!= (@ i layers 0 photo-parameters) photo-parameters)
                     do
                     (setf content ((@ *json* stringify)
                                    (append (array photo-parameters)
                                            (@ i layers 0 photo-parameters)))
                           (@ i epipolar-request) ((@ *open-layers *Request *POST*)
                                                   (create :url "epipolar-line"
                                                           :data content
                                                           :headers (create "Content-type" "text/plain"
                                                                            "Content-length" (@ content length))
                                                           :success epipolar-handler
                                                           :scope i))

                           (@ i epipolar-layer) (new ((@ *open-layers *layer *vector) "Epipolar Line")))
                     ((@ console log) "Content:")
                     ((@ console log) content)
                     ((@ console log) "non-clicked image window: ") ((@ console log) i)
                     ((@ i add-layer) (@ i epipolar-layer)))))
              

              ;;(setf (@ *Proj4js defs "EPSG:4326") "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
              (setf geographic (new ((@ *open-layers *projection) "EPSG:4326")))
              (setf spherical-mercator (new ((@ *open-layers *projection) "EPSG:900913")))
          
              (defun show-photo (photo-parameters image-map)
                ((@ console log) (photo-path photo-parameters))
                ((@ image-map add-layer)
                 (new ((@ *open-layers *layer *image)
                       "Photo"
                       (photo-path photo-parameters)
                       (new ((@ *open-layers *bounds) -.5 -.5 (+ (@ photo-parameters sensor-width-pix) .5) (+ (@ photo-parameters sensor-height-pix) .5))) ; coordinates shown
                       (new ((@ *open-layers *size) 512 256))
                       (create photo-parameters photo-parameters))))
                ((@ image-map add-control) (new ((@ *open-layers *control *layer-switcher))))
                ((@ image-map zoom-to-extent)
                 (new ((@ *open-layers *bounds) -.5 -.5 (1+ (@ photo-parameters sensor-width-pix)) (1+ (@ photo-parameters sensor-height-pix))))) ; in coordinates shown
                (when (> (@ ((@ image-map get-layers-by-name) "Photo") length) 1)
                  ((@ ((@ image-map get-layers-by-name) "Photo") 0 destroy)))
                ;; TODO: etc. (other layers)
                )

              (defvar map)
              (defvar image (array))

              (defun init ()
                (setf map (new ((@ *open-layers *map) "map"
                                (create projection geographic
                                        display-projection geographic))))
                (loop
                   for i from 0 to 1
                   do
                   (setf (aref image i) (new ((@ *open-layers *map)
                                              (create projection spherical-mercator
                                                      all-overlays t))))
                   (setf (@ (aref image i) request-epipolar-lines) request-epipolar-lines*)
                   (setf (@ (aref image i) click) (new ((@ *open-layers *control *click) (create :trigger (@ (aref image i) request-epipolar-lines)))))
                   ((@ (aref image i) add-control) (@ (aref image i) click))
                   ((@ (aref image i) click activate)))

                ((@ (aref image 0) render) "image-a")
                ((@ (aref image 1) render) "image-b")

                (let* ((osm (new ((@ *open-layers *layer *osm*))))
                       ;;(google (new ((@ *open-layers *Layer *google) "Google Streets")))
                       (survey (new ((@ *open-layers *layer *vector) "Survey"
                                     (create :strategies (array (new ((@ *open-layers *strategy *bbox*)
                                                                      (create :ratio 1.1))))
                                             :protocol (new ((@ *open-layers *protocol *http*)
                                                             (create :url "points"
                                                                     :format (new ((@ *open-layers *format *geo-j-s-o-n)
                                                                                   (create external-projection geographic
                                                                                           internal-projection geographic))))))))))
                       (click-map (new ((@ *open-layers *control *click) (create :trigger request-photo))))
                       )
                  ((@ map add-control) click-map)
                  ((@ click-map activate))
                  ;;((@ map add-layers) (array osm google survey))
                  ((@ map add-layers) (array osm survey))
                  ((@ map add-control) (new ((@ *open-layers *control *layer-switcher))))
                  ((@ map add-control) (new ((@ *open-layers *control *mouse-position))))
                  ((@ map zoom-to-extent)
                   ((@ (new ((@ *open-layers *bounds) 14.3258 51.75615 14.33124 51.75778))
                       transform) geographic spherical-mercator))
                  ((@ console log) ((@ (new ((@ *open-layers *bounds) 14.26 51.78 14.37 51.75))
                                       transform) geographic spherical-mercator))
                  ((@ image 0 add-control) (new ((@ *open-layers *control *mouse-position))))
                  ((@ image 1 add-control) (new ((@ *open-layers *control *mouse-position))))
                  ))))))
         (:body :onload (ps (init))
                (:h1 :id "title" (str (concatenate 'string "Phoros: " (session-value 'presentation-project-name))))
                (:p :id "shortdesc"
                    "This example shows the use of the click handler and 
                 getLonLatFromViewPortPx functions to trigger events on mouse click.")
                (:div :id "map" :class "smallmap" :style "float:left")
                (:div :id "image-a" :class "smallmap" :style "float:right")
                (:div :id "image-b" :class "smallmap" :style "float:right"))))
      (redirect
       (concatenate 'string "/phoros/" (session-value 'presentation-project-name))
       :add-session-id t)
      ))

(define-easy-handler (epipolar-line :uri "/epipolar-line") ()
  "Receive vector of two sets of pictures parameters, respond with stuff."
  (let* ((data (json:decode-json-from-string (raw-post-data))))
    (json:encode-json-to-string (photogrammetry :epipolar-line (first data) (second data)))))

(define-easy-handler (multi-position-intersection :uri "/forward-intersection") ()
  "Receive vector of two sets of picture parameters, respond with stuff."
  (let* ((data (json:decode-json-from-string (raw-post-data))))
    (json:encode-json-to-string (photogrammetry :multi-position-intersection data))))

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
  (print (list clicked-photo other-photo)) (terpri)
  (add-cam* clicked-photo)
  (add-bpoint* clicked-photo)
  (add-global-car-reference-point* clicked-photo t)
  (add-cam* other-photo)
  (add-global-car-reference-point* other-photo t)
  (loop
     for i from 3d0 upto 30 by .5d0
     do
       (set-distance-for-epipolar-line i)
       (calculate)
     collect (pairlis '(:m :n) (list (flip-m-maybe (get-m) other-photo)
                                     (flip-n-maybe (get-n) other-photo)))))

(defmethod photogrammetry ((mode (eql :reprojection)) photo &optional global-point)
  "Calculate reprojection from photo."
  (add-cam* photo)
  (add-global-measurement-point* global-point)
  (add-global-car-reference-point* photo)
  (set-global-reference-frame)
  (calculate)
  (pairlis '(:m :n) (list (get-m) (get-n))))

(defmethod photogrammetry ((mode (eql :multi-position-intersection)) photos &optional other-photo)
  "Calculate intersection from photos."
  (declare (ignore other-photo))
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
  "Calculate intersection from two photos.  (Used for debugging only)."
  (add-cam* photo)
  (add-bpoint* photo)
  (add-cam* other-photo)
  (add-bpoint* other-photo)
  (calculate)
  (pairlis '(:x-local :y-local :z-local
             :stdx-local :stdy-local :stdz-local)
           (list
            (get-x-local) (get-y-local) (get-z-local)
            (get-stdx-local) (get-stdy-local) (get-stdz-local))))

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
    (add-bpoint (coerce (print (flip-m-maybe (cdr (assoc :m photo-alist)) photo-alist)) 'double-float)
                (coerce (print (flip-n-maybe (cdr (assoc :n photo-alist)) photo-alist)) 'double-float)))

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
  
