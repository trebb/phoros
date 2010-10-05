;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :phoros)

(setf *js-target-version* 1.8)

(defparameter *phoros-server* (make-instance 'hunchentoot:acceptor :port 8080))
(defparameter *standard-coordinates* 4326 "EPSG code of the coordinate system that we use for communication")

(defun start-server () (hunchentoot:start *phoros-server*))

(register-sql-operators :2+-ary :&&) ; PostGIS "intersects" operator

(define-easy-handler (lon-lat-test :uri "/lon-lat-test" :default-request-type :post) ()
  "Receive coordinates, respond with picture url and calibration parameters."
  (let* ((data (json:decode-json-from-string (raw-post-data)))
         (longitude (cdr (assoc :longitude data)))
         (latitude (cdr (assoc :latitude data)))
         (point-form
          (format nil "POINT(~F ~F)" longitude latitude))
         (result
          (with-connection *postgresql-credentials*          
            (query (:limit
                    (:order-by
                     (:select (:st_x (:st_transform 'the-geom *standard-coordinates*))
                              (:st_y (:st_transform 'the-geom *standard-coordinates*))
                              'measurement-id
                              'filename
                              :from 'motorrad-ccd 
                              :where (:st_dwithin (:st_transform 'the_geom *standard-coordinates*)
                                                  (:st_geomfromtext point-form *standard-coordinates*)
                                                  30d-4)) ; TODO: make this zoom level-dependent
                     (:st_distance (:st_transform 'the_geom *standard-coordinates*) (:st_geomfromtext point-form *standard-coordinates*)))
                    1)))))
    (format nil "~A~%~A~%~A" point-form longitude (json:encode-json-to-string (car result)))
    ))

(define-easy-handler (json-test :uri "/json-test") (bbox)
  (let ((box3d-form
         (concatenate 'string "BOX3D("
                      (substitute #\Space #\,
                                  (substitute #\Space #\, bbox :count 1)
                                  :from-end t :count 1)
                      ")")))
    (setf *t* box3d-form)
    (with-connection *postgresql-credentials*
    (json:encode-json-alist-to-string
     (acons 'type '*geometry-collection
            (acons 'geometries
                   (mapcar #'(lambda (x)
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
    (:html :xmlns "http://www.w3.org/1999/xhtml"
           (:head
            (:title "OpenLayers Click Event Example")
            (:link :rel "stylesheet" :href "lib/theme/default/style.css" :type "text/css")
            (:link :rel "stylesheet" :href "lib/style.css" :type "text/css")
            (:script :src "lib/openlayers/lib/OpenLayers.js")
            (:script :src "lib/openlayers/lib/proj4js.js")
            (:script :src "http://maps.google.com/maps/api/js?sensor=false")
            (:script :type "text/javascript"
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
                                                     (@ this handler-options)))))
                                       :trigger
                                       (lambda (e)
                                         (let ((lonlat
                                                ((@ ((@ map get-lon-lat-from-pixel) (@ e xy))
                                                    transform)
                                                 (new ((@ *open-layers *projection) "EPSG:900913")) ; why?
                                                 (new ((@ *open-layers *projection) "EPSG:4326")))))
                                           (defun handler ()
                                             (when (and (== (@ request ready-state) 4)
                                                        (== (@ request status) 200))
                                               (alert (@ request response-text))
                                               ))
                                           (setf content ((@ *json* stringify) (create :longitude (@ lonlat lon) ; TODO: use OpenLayer's JSON (or GeoJSON).
                                                                                       :latitude (@ lonlat lat)))
                                                 request ((@ *open-layers *Request *POST*)
                                                          (create :url "lon-lat-test"
                                                                  :data content
                                                                  :headers (create "Content-type" "text/plain"
                                                                                   "Content-length" (@ content length))
                                                                  :callback handler))))))))

                        (setf (@ *Proj4js defs "EPSG:32632") "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
                        (setf (@ *Proj4js defs "EPSG:900913") "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
                        (setf (@ *Proj4js defs "EPSG:4326") "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
                        
                        (defun show-photo (url image-map)
                          (when (> (@ ((@ image-map get-layers-by-name) "photo") length) 0)
                            ((@ image-map remove-layer) photo))
                          (setf photo (new ((@ *open-layers *layer *image) "photo"
                                            url
                                            (new ((@ *open-layers *bounds) 0 0 1392 1040)) ; coordinates shown
                                            (new ((@ *open-layers *size) 139 104))))) ; pixels shown
                          ((@ image-map add-layer) photo)
                          ((@ image-map zoom-to-extent)
                           (new ((@ *open-layers *bounds) 0 0 1392 1040))) ; in coordinates shown
                          )
                          

                        (defvar map)
                        (defvar image-a)
                        (defvar image-b)
                        (defvar photo)
                        (defun init ()
                          (setf map (new ((@ *open-layers *map) "map"
                                          (create projection (new ((@ *open-layers *projection) "EPSG:4326"))
                                                  display-projection (new ((@ *open-layers *projection) "EPSG:4326"))))))

                          (setf image-a (new ((@ *open-layers *map) "image-a"
                                            (create projection (new ((@ *open-layers *projection) "EPSG:900913"))
                                                    display-projection (new ((@ *open-layers *projection) "EPSG:900913"))))))

                          (setf image-b (new ((@ *open-layers *map) "image-b"
                                            (create projection (new ((@ *open-layers *projection) "EPSG:900913"))
                                                    display-projection (new ((@ *open-layers *projection) "EPSG:900913"))))))

                          (let* ((osm (new ((@ *open-layers *layer *osm*))))
                                 (google (new ((@ *open-layers *Layer *google) "Google Streets")))
                                 (survey (new ((@ *open-layers *layer *vector) "Survey"
                                               (create :strategies (array (new ((@ *open-layers *strategy *bbox*)
                                                                                (create :ratio 1.1))))
                                                       :protocol (new ((@ *open-layers *protocol *http*)
                                                                       (create :url "json-test"
                                                                               :format (new ((@ *open-layers *format *geo-j-s-o-n)
                                                                                             (create external-projection (new ((@ *open-layers *projection) "EPSG:4326"))
                                                                                                     internal-projection (new ((@ *open-layers *projection) "EPSG:4326"))))))))))))

                                 (click (new ((@ *open-layers *control *click)))))
                            ((@ map add-control) click)
                            ((@ click activate))
                            ((@ map add-layers) (array osm google survey))
                            ((@ map add-control) (new ((@ *open-layers *control *layer-switcher))))
                            ((@ map add-control) (new ((@ *open-layers *control *mouse-position))))
                            ((@ map zoom-to-extent)
                             ((@ (new ((@ *open-layers *bounds) 12.13 48.53 12.1415 48.52))
                                 transform)
                              (new ((@ *open-layers *projection) "EPSG:4326"))
                              (new ((@ *open-layers *projection) "EPSG:900913"))))

                            ((@ image-a add-control) (new ((@ *open-layers *control *mouse-position))))
                            ((@ image-b add-control) (new ((@ *open-layers *control *mouse-position))))
                            (show-photo "photo-test/BAST_Bayern/Front/by_501_pic/by_501_pic_cam1_000024.jpg" image-a)
                            (show-photo "photo-test/BAST_Bayern/Front/by_501_pic/by_501_pic_cam1_000014.jpg" image-b)

                            ))))))
           (:body :onload (ps (init))
                  (:h1 :id "title" "Click Event Example")
                  (:p :id "shortdesc"
                      "This example shows the use of the click handler and 
                 getLonLatFromViewPortPx functions to trigger events on mouse click.")
                  (:div :id "map" :class "smallmap")
                  (:div :id "image-a" :class "smallmap")
                  (:div :id "image-b" :class "smallmap")
                  ))))

(pushnew (create-folder-dispatcher-and-handler "/lib/" "/home/bertb/clbuild/source/phoros/") *dispatch-table*)
(pushnew (create-folder-dispatcher-and-handler "/photo-test/" "/home/bertb/phoros-testdata/") *dispatch-table*)
