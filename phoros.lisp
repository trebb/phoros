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


(cffi:define-foreign-library photogrammetrie
  (t (:or "../photogrammetrie/lib/libphotogrammetrie.so"
          "libphotogrammetrie.so")))

(defparameter *standard-coordinates* 4326 "EPSG code of the coordinate system that we use for communication.")
(defvar *postgresql-credentials* "A list: (database user password host &key (port 5432) use-ssl)")
(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))
(setf *read-default-float-format* 'double-float)

(defparameter *phoros-server* nil "Hunchentoot acceptor.")
(defparameter *common-root* nil "Root directory; contains directories of measuring data.")

(defun start-server (&key (server-port 8080) (common-root "/"))
  (setf *phoros-server* (make-instance 'hunchentoot:acceptor :port server-port))
  (setf *common-root* common-root)
  (hunchentoot:start *phoros-server*))

(defun stop-server () (hunchentoot:stop *phoros-server*))

(register-sql-operators :2+-ary :&& :overlaps)

(define-easy-handler (lon-lat-test :uri "/lon-lat-test" :default-request-type :post) ()
  "Receive coordinates, respond with the count nearest json objects containing picture url, calibration parameters, and car position, wrapped in an array."
  (let* ((data (json:decode-json-from-string (raw-post-data)))
         (longitude-input (cdr (assoc :longitude data)))
         (latitude-input (cdr (assoc :latitude data)))
         (count (cdr (assoc :count data)))
         (zoom-input (cdr (assoc :zoom data)))
         (snap-distance (* 10d-5 (expt 2 (- 18 zoom-input)))) ; assuming geographic coordinates
         (point-form
          (format nil "POINT(~F ~F)" longitude-input latitude-input))
         (result
          (ignore-errors
            (mapcar
             #'(lambda (x)
                 (pairlis
                  '(:measurement-id :cam ; debug
                    :directory :filename
                    :pix-cols :pix-rows
                    :pix-size :angle180
                    :dx :dy :dz         ; outer orientation
                    :omega :phi :kappa  ; outer orientation
                    :c :xh :yh          ; inner orientation
                    :a1 :a2 :a3 :b1 :b2 ; inner orientation: distortion: radial; asymmetric & tangential
                    :c1 :c2 :r0         ; inner orientation: distortion: affinity & shear; 2nd zero-crossing of distortion curve
                    :b-dx :b-dy :b-dz       ; boresight alignment
                    :b-rotx :b-roty :b-rotz ; boresight alignment
                    :b-ddx :b-ddy :b-ddz       ; boresight alignment
                    :b-drotx :b-droty :b-drotz ; boresight alignment
                    :time
                    :longitude :latitude
                    :easting :northing :ellipsoid-height
                    :d-easting :d-northing :d-ellipsoid-height
                    :roll :pitch :heading
                    :d-roll :d-pitch :d-heading)
                  x))
             (with-connection *postgresql-credentials*
               (query
                (:limit
                 (:order-by
                  (:select
                   'data.measurement-id 'data.cam
                   'projects.pictures-path 'data.filename
                   'camera.sensor-width-pix 'camera.sensor-height-pix
                   'camera.pix-size 'angle180
                   (:/ 'outer.dx 1000) ; TODO: store metres instead of millimetres, remove this division hack
                   (:/ 'outer.dy 1000) ; TODO: store metres instead of millimetres, remove this division hack
                   (:/ 'outer.dz 1000) ; TODO: store metres instead of millimetres, remove this division hack
                   'outer.rotx 'outer.roty 'outer.rotz
                   'inner.c 'inner.xh 'inner.yh
                   'inner.a1 'inner.a2 'inner.a3 'inner.b1 'inner.b2
                   'inner.c1 'inner.c2 'inner.r0
                   'bore.bdx 'bore.bdy 'bore.bdz
                   'bore.brotx 'bore.broty 'bore.brotz
                   'bore.bddx 'bore.bddy 'bore.bddz
                   'bore.bdrotx 'bore.bdroty 'bore.bdrotz
                   'data.time-appl-utc  ; should be in the-geom
                   (:st_x (:st_transform 'data.the-geom *standard-coordinates*))
                   (:st_y (:st_transform 'data.the-geom *standard-coordinates*))
                   (:st_x (:st_transform 'data.the-geom 32632)) ; TODO get projection from somewhere
                   (:st_y (:st_transform 'data.the-geom 32632)) ; TODO get projection from somewhere
                   'data.ellipsoid-height ; should be (:st_z (:st_transform 'the-geom *standard-coordinates*))
                   'data.east-sd 'data.north-sd 'data.height-sd
                   'data.roll 'data.pitch 'data.heading
                   'data.roll-sd 'data.pitch-sd 'data.heading-sd
                   :from
                   (:as 'projects-measurements 'projects)
                   (:as 'motorrad-ccd 'data)
                   (:as 'calib-set 'cal)
                   (:as 'calib-camera 'camera)
                   (:as 'calib-boreside-alignement 'bore)
                   (:as 'calib-inner-orientation 'inner)
                   (:as 'calib-outer-orientation 'outer)
                   :where
                   (:and (:= 'projects.project-id 6)
                         (:= 'projects.measurement-id 'data.measurement-id)
                         (:st_dwithin (:st_transform 'data.the_geom *standard-coordinates*)
                                      (:st_geomfromtext point-form *standard-coordinates*)
                                      snap-distance)
                         (:or (:= 'projects.calib-set-id-left 'cal.calib-set-id)   ; crazy distribution of 
                              (:= 'projects.calib-set-id-right 'cal.calib-set-id)) ; calib-set-id over two columns
                         (:= 'data.cam 'cal.camera-id)
                         (:= 'cal.camera-id 'camera.camera-id)
                         (:= 'cal.outer-id 'outer.outer-id)
                         (:= 'cal.inner-id 'inner.inner-id)
                         (:= 'cal.bore-id 'bore.bore-id)))
                  (:st_distance (:st_transform 'the_geom *standard-coordinates*) (:st_geomfromtext point-form *standard-coordinates*))
                  'cal.calib-set-id)
                 count)))))))
    (json:encode-json-to-string result)))

(define-easy-handler (json-test :uri "/json-test") (bbox)
  (let ((box3d-form
         (concatenate 'string "BOX3D("
                      (substitute #\Space #\,
                                  (substitute #\Space #\, bbox :count 1)
                                  :from-end t :count 1)
                      ")")))
    (with-connection *postgresql-credentials*
    (json:encode-json-alist-to-string
     (acons
      'type '*geometry-collection
      (acons 'geometries
             (mapcar
              #'(lambda (x)
                  (acons 'type '*point
                         (acons 'coordinates x nil)))
              (query (:select (:st_x (:st_transform 'the-geom *standard-coordinates*))
                              (:st_y (:st_transform 'the-geom *standard-coordinates*))
                              :from 'motorrad-ccd 
                              :where (:&& (:st_transform 'the-geom *standard-coordinates*)
                                          (:st_setsrid  (:type box3d-form box3d) *standard-coordinates*)))))
             nil))))))

(define-easy-handler photo-handler
    ((bayer-pattern :parameter-type #'canonicalize-bayer-pattern
                    :init-form #(#x00ff00 #x0000ff))
     (color-raiser :parameter-type #'canonicalize-color-raiser
                   :init-form #(1 1 1)))
  "Serve an image from a .pictures file."
  (let* ((s (cdr (cl-utilities:split-sequence #\/ (script-name*)
                                              :remove-empty-subseqs t)))
         (directory (butlast s 2))
         (file-name-and-type (cl-utilities:split-sequence
                              #\. (first (last s 2))))
         (byte-position (parse-integer (car (last s)) :junk-allowed t))
         (path-to-file
          (make-pathname
           :directory (append (pathname-directory *common-root*) directory)
           :name (first file-name-and-type)
           :type (second file-name-and-type)))
         stream)
    (print bayer-pattern)
    (terpri)
    (setf (content-type*) "image/png")
    (setf stream (send-headers))
    (send-png stream path-to-file byte-position
              :bayer-pattern bayer-pattern :color-raiser color-raiser)))

(pushnew (create-prefix-dispatcher "/photo" 'photo-handler)
         *dispatch-table*)

(pushnew (create-folder-dispatcher-and-handler "/lib/" "/home/bertb/lisphack/phoros/")
         *dispatch-table*)

(define-easy-handler (click :uri "/click" :default-request-type :post) ()
  (with-html-output-to-string (s nil :indent t)
    (:html
     :xmlns
     "http://www.w3.org/1999/xhtml"
     (:head
      (:title "OpenLayers Click Event Example")
      (:link :rel "stylesheet" :href "lib/theme/default/style.css" :type "text/css")
      (:link :rel "stylesheet" :href "lib/style.css" :type "text/css")
      (:script :src "lib/openlayers/lib/OpenLayers.js")
      (:script :src "lib/openlayers/lib/proj4js.js")
      (:script :src "http://maps.google.com/maps/api/js?sensor=false")
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
            (+ "photo-test" (@ photo-parameters directory) "/" (@ photo-parameters filename)))

          (defun map-click-handler ()
            (let ((photo-parameters ((@ *json* parse) (@ photo-request response-text))))
                (show-photo (aref photo-parameters 0) (aref image 0)) ; TODO: remove absolute paths from DB
                ;; (setf (@ (aref photo-parameters 0) angle180) 1) ; Debug: coordinate flipping
                (show-photo (aref photo-parameters 1) (aref image 1)))) ; TODO: remove absolute paths from DB

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
                                   (create :url "lon-lat-test"
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
              (setf (@ photo-parameters m) (@ lonlat lon)
                    (@ photo-parameters n) (@ lonlat lat))
              ((@ console log) "m, n:")
              ((@ console log) (@ lonlat lon))
              ((@ console log) (@ lonlat lat))
              ((@ console log) "Photo-Parameters (naked/from this):")
              ((@ console log) photo-parameters)
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
            ((@ image-map add-layer)
             (new ((@ *open-layers *layer *image) "Photo"
                   (photo-path photo-parameters)
                   (new ((@ *open-layers *bounds) -.5 -.5 (+ (@ photo-parameters pix-cols) .5) (+ (@ photo-parameters pix-rows) .5))) ; coordinates shown
                   (new ((@ *open-layers *size) 512 256))
                   (create photo-parameters photo-parameters))))
            ((@ image-map add-control) (new ((@ *open-layers *control *layer-switcher))))
            ((@ image-map zoom-to-extent)
             (new ((@ *open-layers *bounds) -.5 -.5 (1+ (@ photo-parameters pix-cols)) (1+ (@ photo-parameters pix-rows))))) ; in coordinates shown
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
                   (google (new ((@ *open-layers *Layer *google) "Google Streets")))
                   (survey (new ((@ *open-layers *layer *vector) "Survey"
                                 (create :strategies (array (new ((@ *open-layers *strategy *bbox*)
                                                                  (create :ratio 1.1))))
                                         :protocol (new ((@ *open-layers *protocol *http*)
                                                         (create :url "json-test"
                                                                 :format (new ((@ *open-layers *format *geo-j-s-o-n)
                                                                               (create external-projection geographic
                                                                                       internal-projection geographic))))))))))
                   (click-map (new ((@ *open-layers *control *click) (create :trigger request-photo))))
                   )
              ((@ map add-control) click-map)
              ((@ click-map activate))
              ((@ map add-layers) (array osm google survey))
              ((@ map add-control) (new ((@ *open-layers *control *layer-switcher))))
              ((@ map add-control) (new ((@ *open-layers *control *mouse-position))))
              ((@ map zoom-to-extent)
               ((@ (new ((@ *open-layers *bounds) 12.13 48.53 12.1415 48.52))
                   transform) geographic spherical-mercator))
              ((@ console log) ((@ (new ((@ *open-layers *bounds) 12.13 48.53 12.1415 48.52))
                                   transform) geographic spherical-mercator))
              ((@ image 0 add-control) (new ((@ *open-layers *control *mouse-position))))
              ((@ image 1 add-control) (new ((@ *open-layers *control *mouse-position))))
              ))))))
     (:body :onload (ps (init))
            (:h1 :id "title" "Click Event Example")
            (:p :id "shortdesc"
                "This example shows the use of the click handler and 
                 getLonLatFromViewPortPx functions to trigger events on mouse click.")
            (:div :id "map" :class "smallmap" :style "float:left")
            (:div :id "image-a" :class "smallmap" :style "float:right")
            (:div :id "image-b" :class "smallmap" :style "float:right")))))

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
  (add-cam* clicked-photo)
  (add-bpoint* clicked-photo)
  (add-global-car-reference-point* clicked-photo t)
  (add-cam* other-photo)
  (add-global-car-reference-point* other-photo t)
  (loop
     for i from 4d0 upto 30 by .5d0
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
  "Flip coordinate m when :angle180 in photo suggests it necessary."
  (if (plusp (cdr (assoc :angle-180 photo)))
      (- (cdr (assoc :pix-cols photo)) m)
      m))
(defun flip-n-maybe (n photo)
  "Flip coordinate n when :angle180 in photo suggests it necessary."
  (if (zerop (cdr (assoc :angle-180 photo)))
      (- (cdr (assoc :pix-rows photo)) n)
      n))

(defun photogrammetry-arglist (alist &rest keys)
  "Construct an arglist from alist values corresponding to keys."
  (mapcar #'(lambda (x) (cdr (assoc x alist))) keys))

(defun add-cam* (photo-alist)
  "Call add-cam with arguments taken from photo-alist."
  (let ((integer-args
         (photogrammetry-arglist
          photo-alist :pix-rows :pix-cols))
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
    (apply #'add-cam (print (nconc integer-args double-float-args)))))

(defun add-bpoint* (photo-alist)
  "Call add-bpoint with arguments taken from photo-alist."
    (add-bpoint (print (coerce (flip-m-maybe (cdr (assoc :m photo-alist)) photo-alist) 'double-float))
                (print (coerce (flip-n-maybe (cdr (assoc :n photo-alist)) photo-alist) 'double-float))))

(defun add-ref-ground-surface* (floor-alist)
  "Call add-ref-ground-surface with arguments taken from floor-alist."
  (let ((double-float-args
         (mapcar #'(lambda (x) (coerce x 'double-float))
                 (photogrammetry-arglist floor-alist
                                         :nx :ny :nz :d))))
    (apply #'add-ref-ground-surface (print double-float-args))))

(defun add-global-car-reference-point* (photo-alist &optional cam-set-global-p)
  "Call add-global-car-reference-point with arguments taken from photo-alist.  When cam-set-global-p is t, call add-global-car-reference-point-cam-set-global instead."
  (let ((double-float-args
         (mapcar #'(lambda (x) (coerce x 'double-float))
                 (photogrammetry-arglist photo-alist
                                         :easting :northing :ellipsoid-height
                                         :roll :pitch :heading
                                         :latitude :longitude))))
    (apply (if cam-set-global-p
               #'add-global-car-reference-point-cam-set-global
               #'add-global-car-reference-point)
           (print double-float-args))))

(defun add-global-measurement-point* (point)
  "Call add-global-measurement-point with arguments taken from point."
  (let ((double-float-args
         (mapcar #'(lambda (x) (coerce x 'double-float))
                 (photogrammetry-arglist point
                                         :x-global :y-global :z-global))))
    (apply #'add-global-measurement-point (print double-float-args))))
  
