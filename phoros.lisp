;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :phoros)

(setf *js-target-version* 1.8)

(defparameter *phoros-server* (make-instance 'hunchentoot:acceptor :port 8080))

(defun start-server () (hunchentoot:start *phoros-server*))

(define-easy-handler (lon-lat-test :uri "/lon-lat-test" :default-request-type :post) ()
  (let ((data (json:decode-json-from-string (raw-post-data))))
    (format t "abc ~% ~A" data)
    (json:encode-json-to-string data)))

(define-easy-handler (click :uri "/click" :default-request-type :post) ()
  (with-html-output-to-string (s nil :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
           (:head
            (:title "OpenLayers Click Event Example")
            (:link :rel "stylesheet" :href "lib/theme/default/style.css" :type "text/css")
            (:link :rel "stylesheet" :href "lib/style.css" :type "text/css")
            (:script :src "lib/openlayers/lib/OpenLayers.js")
            (:script :type "text/javascript"
                     (str
                      (ps (setf (@ *open-layers *control *click)
                                ((@ *open-layers *class) 
                                 (@ *open-layers *control)
                                 (create default-handler-options
                                         (create single t
                                                 double false
                                                 pixel-tolerance 0
                                                 stop-single false
                                                 stop-double false)
                                         initialize
                                         (lambda (options)
                                           (setf 
                                            (@ this handler-options)
                                            ((@ *open-layers *util extend)
                                             (create)
                                             (@ this default-handler-options)
                                             ))
                                           ((@ *open-layers *control prototype initialize apply)
                                            this arguments)
                                           (setf
                                            (@ this handler)
                                            (new ((@ *open-layers *handler *click) 
                                                  this
                                                  (create click (@ this trigger))
                                                  (@ this handler-options)
                                                  ))))
                                         trigger
                                         (lambda (e)
                                           (let ((lonlat
                                                  ((@ map get-lon-lat-from-view-port-px) (@ e xy))))
                                             (alert (+ "You clicked near " (@ lonlat lat) " N, " (@ lonlat lon) " E"))

                                             (let* ((http (new (*x-m-l-http-request)))
                                                    (url "lon-lat-test")
                                                    (content (create longitude (@ lonlat lat)
                                                                     latitude (@ lonlat lon) )))
                                               ((@ http open) "POST" url t)
                                               ((@ http set-request-header) "Content-type" "text/plain")
                                               ((@ http set-request-header) "Content-length" content.length)
                                               ((@ http set-request-header) "Connection" "close")
                                               (setf (@ http onreadystatechange)
                                                     (lambda ()
                                                       (when (and (== (@ http ready-state) 4)
                                                                  (== (@ http status) 200))
                                                         (alert (@ http response-text)))))
                                               ((@ http send) ((@ *json* stringify) content))))))))
                          (defvar map)
                          (defun init ()
                            (setf map (new ((@ *open-layers *map) "map")))
                            (let ((ol_wms (new ((@ *open-layers *layer *wms*) "OpenLayers WMS"
                                                "http://vmap0.tiles.osgeo.org/wms/vmap0?"
                                                (create layers "basic")))))
                              ((@ map add-layers) (array ol_wms))
                              ((@ map add-control) (new ((@ *open-layers *control *layer-switcher))))
                              ((@ map zoom-to-max-extent))
                              (let ((click (new ((@ *open-layers *control *click)))))
                                ((@ map add-control) click)
                                ((@ click activate))))))
                      )))
           (:body :onload (ps (init))
                  (:h1 :id "title" "Click Event Example")
                  (:p :id "shortdesc"
                      "This example shows the use of the click handler and 
                 getLonLatFromViewPortPx functions to trigger events on mouse click.")
                  (:div :id "map" :class "smallmap")))))

(pushnew (create-folder-dispatcher-and-handler "/lib/" "/home/bertb/clbuild/source/phoros/") *dispatch-table*)
