;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :phoros)

(setf *js-target-version* 1.8)

(defparameter *phoros-server* (make-instance 'hunchentoot:acceptor :port 8080))
(defparameter *standard-coordinates* 4326 "EPSG code of the coordinate system that we use for communication.")

(defun start-server () (hunchentoot:start *phoros-server*))

(register-sql-operators :2+-ary :&&) ; PostGIS "intersects" operator

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
                    :pix-rows :pix-cols
                    :pix-size :angle180
                    :dx :dy :dz         ; outer orientation
                    :omega :phi :kappa  ; outer orientation
                    :c :xh :yh          ; inner orientation
                    :a1 :a2 :a3 :b1 :b2 ; inner orientation
                    :c1 :c2 :r0         ; inner orientation
                    :b-dx :b-dy :b-dz   ; boresight alignment
                    :b-rotx :b-roty :b-rotz ; boresight alignment
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
                   'outer.dx 'outer.dy 'outer.dz
                   'outer.rotx 'outer.roty 'outer.rotz
                   'inner.c 'inner.xh 'inner.yh
                   'inner.a1 'inner.a2 'inner.a3 'inner.b1 'inner.b2
                   'inner.c1 'inner.c2 'inner.r0
                   'bore.bdx 'bore.bdy 'bore.bdz
                   'bore.brotx 'bore.broty 'bore.brotz
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

          (defun map-click-handler ()
            (when (and (== (@ request ready-state) 4)
                       (== (@ request status) 200))
              (let ((ttt ((@ *json* parse) (@ request response-text))))
                (show-photo (+ "photo-test" (@ ttt 0 directory) "/" (@ ttt 0 filename)) image-a "photo-0") ; TODO: remove absolute paths from DB
                (show-photo (+ "photo-test" (@ ttt 1 directory) "/" (@ ttt 1 filename)) image-b "photo-1") ; TODO: remove absolute paths from DB
                )
              ))

          (defun request-photo (e)
            (let ((lonlat
                   ((@ ((@ map get-lon-lat-from-pixel) (@ e xy))
                       transform)
                    (new ((@ *open-layers *projection) "EPSG:900913")) ; why?
                    (new ((@ *open-layers *projection) "EPSG:4326")))))
                      
              (setf content ((@ *json* stringify) (create :longitude (@ lonlat lon) ; TODO: use OpenLayer's JSON (or GeoJSON).
                                                          :latitude (@ lonlat lat)
                                                          :zoom ((@ map get-zoom))
                                                          :count 2)) ; that's left and right
                    request ((@ *open-layers *Request *POST*)
                             (create :url "lon-lat-test"
                                     :data content
                                     :headers (create "Content-type" "text/plain"
                                                      "Content-length" (@ content length))
                                     :callback map-click-handler)))))

          (defun request-photogrammetry (e)
            (let ((lonlat
                   ((@ ((@ image-a get-lon-lat-from-view-port-px) (@ e xy))
                       transform)
                    photo-coordinates
                    spherical-mercator)))
              (alert (+ "Clicked: " (@ lonlat lat) "  " (@ lonlat lon)))))

          (setf (@ *Proj4js defs "EPSG:900913") "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
          (setf (@ *Proj4js defs "EPSG:900914") "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0 +y_0=1040 +k=1.0 +units=m +nadgrids=@null +no_defs")
          (setf (@ *Proj4js defs "EPSG:900915") "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=1392 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
          (setf (@ *Proj4js defs "EPSG:4326") "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
          (setf geographic (new ((@ *open-layers *projection) "EPSG:4326")))
          (setf spherical-mercator (new ((@ *open-layers *projection) "EPSG:900913")))
          (setf photo-coordinates (new ((@ *open-layers *projection) "EPSG:900914")))
          (setf photo-coordinates-rotated180 (new ((@ *open-layers *projection) "EPSG:900915")))
                  
          (defun show-photo (url image-map layer-name)
            ((@ console log) url)
            ((@ console log) image-map)
            ((@ console log) ((@ image-map get-layers-by-name) layer-name))
            (setf (@ image-map projection) photo-coordinates) ; TODO: needs to be a parameter
            (when (> (@ ((@ image-map get-layers-by-name) layer-name) length) 0)
              ((@ console log) layer-name)
              ;;((@ image-map remove-layer) layer-name)
              (delete (@ image-map layers 0))
              (delete (@ image-map layers 1))
              ((@ console log) layer-name)
              )
            ((@ console log) ((@ image-map get-layers-by-name) layer-name))
            ((@ image-map add-layer)
             (new ((@ *open-layers *layer *image) layer-name
                   url
                   (new ((@ *open-layers *bounds) 0 0 1392 1040)) ; coordinates shown
                   (new ((@ *open-layers *size) 139 104)))))
            ((@ image-map add-control) (new ((@ *open-layers *control *layer-switcher))))
            ((@ image-map zoom-to-extent)
             (new ((@ *open-layers *bounds) 0 0 1392 1040))) ; in coordinates shown
            )

          (defvar map)
          (defvar image-a)
          (defvar image-b)
          ;;(defvar photo)

          (defun init ()
            (setf map (new ((@ *open-layers *map) "map"
                            (create projection geographic
                                    display-projection geographic
                                    all-overlays t))))

            (setf image-a (new ((@ *open-layers *map) "image-a"
                                (create display-projection spherical-mercator
                                        all-overlays t))))

            (setf image-b (new ((@ *open-layers *map) "image-b"
                                (create display-projection spherical-mercator
                                        all-overlays t))))

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
                   (click-photo (new ((@ *open-layers *control *click) (create :trigger request-photogrammetry)))))
              ((@ map add-control) click-map)
              ((@ click-map activate))
              ((@ map add-layers) (array osm google survey))
              ((@ map add-control) (new ((@ *open-layers *control *layer-switcher))))
              ((@ map add-control) (new ((@ *open-layers *control *mouse-position))))
              ((@ map zoom-to-extent)
               ((@ (new ((@ *open-layers *bounds) 12.13 48.53 12.1415 48.52))
                   transform) geographic spherical-mercator))

              ;;((@ image-a add-control) (new ((@ *open-layers *control *mouse-position))))
              ;;((@ image-b add-control) (new ((@ *open-layers *control *mouse-position))))
              ;;((@ image-a add-control) click-photo)
              ;;((@ click-photo activate))
              ))))))
     (:body :onload (ps (init))
            (:h1 :id "title" "Click Event Example")
            (:p :id "shortdesc"
                "This example shows the use of the click handler and 
                 getLonLatFromViewPortPx functions to trigger events on mouse click.")
            (:div :id "map" :class "smallmap" :style "float:left")
            (:div :id "image-a" :class "smallmap" :style "float:right")
            (:div :id "image-b" :class "smallmap" :style "float:right")
            ))))

(pushnew (create-folder-dispatcher-and-handler "/lib/" "/home/bertb/clbuild/source/phoros/") *dispatch-table*)
(pushnew (create-folder-dispatcher-and-handler "/photo-test/" "/home/bertb/phoros-testdata/") *dispatch-table*)
