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
         (who-ps-html (:p "User role.  \"Read\" can't write or modify anything.  \"Write\" may write user points and delete their own ones. \"Admin\" may write user points and delete points written by others."))
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
         (who-ps-html (:p "Discard the current, unstored point and zoom out all images. Keep the rest of the workspace untouched."))
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
         :zoom-images-to-max-extent
         (who-ps-html (:p "Zoom all images out completely, restoring the original view."))
         :auto-zoom
         (who-ps-html (:p "Check this to automatically zoom into images once they get an estimated position."))
         :image-layer-switcher
         (who-ps-html (:p "Toggle display of image."))
         :image-trigger-time
         (who-ps-html (:p "Time this image was taken."))
         :streetmap-layer-switcher
         (who-ps-html (:p "Toggle visibility of data layers, or choose a background streetmap. (TODO: currently only one \"choice\")"))
         :streetmap-overview
         (who-ps-html (:p "Click to re-center streetmap, or drag the red rectangle."))
         :streetmap-mouse-position
         (who-ps-html (:p "Cursor position in geographic coordinates when cursor is in streetmap."))
         :h2-help
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
              (let ((help-body (getprop *help-topics* topic)))
                (if (undefined help-body)
                    ""
                    help-body))))

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

      (defvar +unix-epoch+ (lisp *unix-epoch*)
        "Seconds between Lisp epoch and UNIX epoch.")
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
        (zoom-images-to-max-extent))

      (defun enable-element-with-id (id)
        "Activate HTML element with id=\"id\"."
        (setf (chain document (get-element-by-id id) disabled) nil))

      (defun disable-element-with-id (id)
        "Grey out HTML element with id=\"id\"."
        (setf (chain document (get-element-by-id id) disabled) t))

      (defmacro value-with-id (id)
        "Value of element with id=\"id\"."
        `(chain document (get-element-by-id ,id) value))

      (defmacro checkbox-status-with-id (id)
        "Whether checkbox with id=\"id\" is checked or not."
        `(chain document (get-element-by-id ,id) checked))

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
                 (setf (chain i estimated-position-lonlat)
                       (new (chain *open-layers (*lon-lat
                                                 (getprop p 'm)
                                                 (getprop p 'n)))))
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
                          (add-features feature)))))))
        (zoom-images-to-point))

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

      (defun iso-time-string (lisp-time)
        "Return Lisp universal time formatted as ISO time string"
        (let* ((unix-time (- lisp-time +unix-epoch+))
               (js-date (new (*date (* 1000 unix-time)))))
          (chain *open-layers *date (to-i-s-o-string js-date))))

      (defun show-photo ()
        "Show the photo described in this object's photo-parameters."
        (loop
           repeat ((getprop this 'map 'get-num-layers))
           do ((getprop this 'map 'layers 0 'destroy)))
        ((getprop this 'map 'add-layer)
         (new (chain
               *open-layers
               *layer
               (*image
                "Photo"
                (photo-path (getprop this 'photo-parameters))
                (new ((@ *open-layers *bounds) -.5 -.5
                      (+ (getprop this 'photo-parameters 'sensor-width-pix)
                         .5)
                      (+ (getprop this 'photo-parameters 'sensor-height-pix)
                         .5)))          ; coordinates shown
                (new ((@ *open-layers *size) 512 256))
                (create)))))
        (chain this map (zoom-to-max-extent))
        (setf (chain this trigger-time-div inner-h-t-m-l)
              (iso-time-string (getprop this 'photo-parameters 'trigger-time))))

      (defun zoom-images-to-max-extent ()
        "Zoom out all images."
        (loop for i across *images* do (chain i map (zoom-to-max-extent))))

      (defun zoom-images-to-point ()
        "For images that have an Active Point or an Estimated
Position, zoom in and recenter."
        (loop for i across *images* do
             (let ((point-lonlat
                    (cond
                      ((has-layer-p (chain i map) "Active Point")
                       (new (chain *open-layers (*lon-lat
                                                 (chain i photo-parameters m)
                                                 (chain i photo-parameters n)))))
                      ((has-layer-p (chain i map) "Estimated Position")
                       (chain i estimated-position-lonlat))
                      (t false))))
               (when (and point-lonlat
                          (checkbox-status-with-id "zoom-to-point-p"))
                 (chain i map (set-center point-lonlat 4 nil t))))))

      (defun initialize-image (image-index)
        "Create an image usable for displaying photos at position
image-index in array *images*."
        (setf (aref *images* image-index) (new *image))
        (setf (@ (aref *images* image-index) trigger-time-div)
              (chain
               document
               (get-element-by-id (+ "image-" image-index "-trigger-time"))))
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
                                         "streetmap-overview")))))))
              (mouse-position-control
               (new (chain *open-layers
                           *control
                           (*mouse-position
                            (create div (chain document
                                               (get-element-by-id
                                                "streetmap-mouse-position"))
                                    empty-string "longitude, latitude"))))))
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
          (chain *streetmap* (add-control overview-map))
          (chain *streetmap*
                 (zoom-to-extent +presentation-project-bounds+))
          (chain *streetmap* (add-control mouse-position-control)))
        (loop
           for i from 0 to (lisp (1- *number-of-images*))
           do (initialize-image i))
        (add-help-events)))))