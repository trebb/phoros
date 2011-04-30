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
    (ps*
     '(progn
       (setf debug-info (@ *open-layers *console info))

       (defmacro inner-html-with-id (id)
         "innerHTML of element with id=\"id\"."
         `(chain document (get-element-by-id ,id) inner-h-t-m-l))

       (defmacro value-with-id (id)
         "Value of element with id=\"id\"."
         `(chain document (get-element-by-id ,id) value))

       (defmacro checkbox-status-with-id (id)
         "Whether checkbox with id=\"id\" is checked or not."
         `(chain document (get-element-by-id ,id) checked))

       (defvar *help-topics*
         (create
          :user-role
          (who-ps-html
           (:p "User role.  \"Read\" can't write or modify anything.
           \"Write\" may write user points and delete their own
           ones. \"Admin\" may write user points and delete points
           written by others."))
          :presentation-project-name
          (who-ps-html
           (:p "Presentation project name."))
          :h2-controls
          (who-ps-html
           (:p "Next action."))
          :finish-point-button
          (who-ps-html
           (:p "Store point with its attribute, description and
           numeric description into database.  Afterwards, increment
           the numeric description if possible."))
          :delete-point-button
          (who-ps-html
           (:p "Delete current point."))
          :download-user-points-button
          (who-ps-html
           (:p "Download all user points as GeoJSON-fomatted text file."))
          :point-attribute
          (who-ps-html
           (:p "One of a few possible user point attributes.")
           (:p "TODO: currently only the hard-coded ones are available."))
          :point-description
          (who-ps-html
           (:p "Optional verbal description of user point."))
          :point-numeric-description
          (who-ps-html
           (:p "Optional additional description of user point.
           Preferrably numeric and if so, automatically incremented
           after finishing point."))
          :point-creation-date
          (who-ps-html
           (:p "Creation date of current user point.  Will be updated
           when you change this point."))
          :include-aux-data-p
          (who-ps-html
           (:p "Check this if the user point being created should
           include auxiliary data."))
          :aux-point-distance
          (who-ps-html
           (:p "Select a set of auxiliary data, either by its distance
           from the current estimated position, or by clicking its
           representation in streetmap.")
           (:p "TODO:  This is not a decent length unit."))
          :aux-data
          (who-ps-html
           (:p "Auxiliary data connected to this presentation project;
           all the numeric values followed by all the text values if
           any."))
          :creator
          (who-ps-html
           (:p "Creator of current user point.  Will be updated when
           you change this point."))
          :remove-work-layers-button
          (who-ps-html
           (:p "Discard the current, unstored user point and zoom out
           all images. Keep the rest of the workspace untouched."))
          :blurb-button
          (who-ps-html
           (:p "View some info about Phoros."))
          :logout-button
          (who-ps-html
           (:p "Finish this session.  Fresh login is required to
           continue."))
          :streetmap
          (who-ps-html
           (:p "Clicking into the streetmap fetches images which most
           probably feature the clicked point.")
           (:p "TODO: This is not quite so.  Currently images taken
           from points nearest to the clicked one are displayed.")
           (:p "To pan the map, drag the mouse.  To zoom, spin the
           mouse wheel, or hold shift down whilst dragging a box, or
           double-click (shift double-click for larger zoom steps) a
           point of interest."))
          :image
          (who-ps-html
           (:p "Clicking into an image sets or resets the active point
           there.  Once a feature is marked by active points in more
           than one image, the estimated position is calculated.")
           (:p "To pan an image, drag the mouse.  To zoom, spin the
           mouse wheel, or hold shift down whilst dragging a box, or
           double-click (shift double-click for larger zoom steps) a
           point of interest."))
          ol-Control-Pan-West-Item-Inactive
          (who-ps-html
           (:p "Move viewport left."))
          ol-Control-Pan-East-Item-Inactive
          (who-ps-html
           (:p "Move viewport right."))
          ol-Control-Pan-North-Item-Inactive
          (who-ps-html
           (:p "Move viewport up."))
          ol-Control-Pan-South-Item-Inactive
          (who-ps-html
           (:p "Move viewport down."))
          ol-Control-Zoom-In-Item-Inactive
          (who-ps-html
           (:p "Zoom in."))
          ol-Control-Zoom-Out-Item-Inactive
          (who-ps-html
           (:p "Zoom out."))
          streetmap-Zoom-To-Max-Extent-Item-Inactive
          (who-ps-html
           (:p "Zoom to the extent of presentation project."))
          ol-Control-Zoom-To-Max-Extent-Item-Inactive
          (who-ps-html
           (:p "Zoom out completely, restoring the original view."))
          :zoom-images-to-max-extent
          (who-ps-html
           (:p "Zoom all images out completely, restoring the original
           view."))
          :auto-zoom
          (who-ps-html
           (:p "Check this to automatically zoom into images once they
           get an estimated position."))
          :image-layer-switcher
          (who-ps-html
           (:p "Toggle display of image."))
          :image-trigger-time
          (who-ps-html
           (:p "Time this image was taken."))
          base-layers-div
          (who-ps-html
           (:p "Choose a background streetmap."))
          data-layers-div
          (who-ps-html
           (:p "Toggle visibility of data layers."))
          :streetmap-overview
          (who-ps-html
           (:p "Click to re-center streetmap, or drag the red rectangle."))
          :streetmap-mouse-position
          (who-ps-html
           (:p "Cursor position in geographic coordinates when cursor
           is in streetmap."))
          :h2-help
          (who-ps-html
           (:p "Hints on Phoros' displays and controls are shown here
           while hovering over the respective elements."))))

       (defun add-help-topic (topic element)
         "Add mouse events to DOM element that initiate display of a
nhelp message."
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

       (defvar *aux-point-distance-select* undefined
         "The HTML element for selecting one of a few nearest auxiliary points.")

       (defvar *global-position* undefined
         "Coordinates of the current estimated position")

       (defvar *current-nearest-aux-point*
         (create attributes (create aux-numeric undefined
                                    aux-text undefined))
         "Attributes of currently selected point of auxiliary data.")


       (defvar *bbox-strategy* (chain *open-layers *strategy *bbox*))
       (setf (chain *bbox-strategy* prototype ratio) 1.5)
       (setf (chain *bbox-strategy* prototype res-factor) 1.5)

       (defvar *json-parser* (new (chain *open-layers *format *json*)))

       (defvar *geojson-format* (chain *open-layers *format *geo-j-s-o-n))
       (setf (chain *geojson-format* prototype ignore-extra-dims)
        t)                              ;doesn't handle height anyway
       (setf (chain *geojson-format* prototype external-projection)
        +geographic+)
       (setf (chain *geojson-format* prototype internal-projection)
        +geographic+)

       (defvar *http-protocol* (chain *open-layers *protocol *http*))
       (setf (chain *http-protocol* prototype format) (new *geojson-format*))

       (defvar *survey-layer*
         (let ((survey-layer-style
                (create stroke-color (chain *open-layers *feature *vector
                                            style "default" stroke-color)
                        stroke-width 1
                        point-radius 2
                        fill-opacity 0
                        graphic-name "circle")))
           (new (chain
                 *open-layers *layer
                 (*vector
                  "survey"
                  (create
                   strategies (array (new (*bbox-strategy*)))
                   protocol
                   (new (*http-protocol*
                         (create :url "/phoros-lib/points.json")))
                   style survey-layer-style
                   ))))))

       (defvar *user-point-layer*
         (let ((user-point-layer-style-map
                (new (chain *open-layers
                            (*style-map
                             (create "default"
                                     (create stroke-color "OrangeRed"
                                             stroke-opacity .5
                                             stroke-width 2
                                             point-radius 5
                                             fill-opacity 0
                                             graphic-name "triangle")
                                     "select"
                                     (create stroke-color "OrangeRed"
                                             stroke-opacity 1
                                             stroke-width 2
                                             point-radius 5
                                             fill-opacity 0
                                             graphic-name "triangle")
                                     "temporary"
                                     (create stroke-color "OrangeRed"
                                             fill-color "OrangeRed"
                                             stroke-opacity .5
                                             stroke-width 2
                                             point-radius 5
                                             fill-opacity .5
                                             graphic-name "triangle")))))))
           (new (chain
                 *open-layers *layer
                 (*vector
                  "user points"
                  (create
                   strategies (array (new *bbox-strategy*))
                   protocol
                   (new (*http-protocol*
                         (create :url "/phoros-lib/user-points.json")))
                   style-map user-point-layer-style-map))))))

       (defvar *aux-point-layer*
         (let ((aux-layer-style
                (create stroke-color "grey"
                        stroke-width 1
                        point-radius 2
                        fill-opacity 0
                        graphic-name "circle")))
           (new (chain
                 *open-layers *layer
                 (*vector
                  "auxiliary data"
                  (create
                   strategies (array (new (*bbox-strategy*)))
                   protocol
                   (new (*http-protocol*
                         (create :url "/phoros-lib/aux-points.json")))
                   style aux-layer-style
                   visibility nil))))))

       (defvar *streetmap-nearest-aux-points-layer*
         (let ((nearest-aux-point-layer-style-map
                (new (chain *open-layers
                            (*style-map
                             (create "default"
                                     (create stroke-color "grey"
                                             stroke-width 1
                                             point-radius 5
                                             fill-opacity 0
                                             graphic-name "circle")
                                     "select"
                                     (create stroke-color "black"
                                             stroke-width 1
                                             point-radius 5
                                             fill-opacity 0
                                             graphic-name "circle")
                                     "temporary"
                                     (create stroke-color "grey"
                                             stroke-width 1
                                             point-radius 5
                                             fill-color "grey"
                                             fill-opacity 1
                                             graphic-name "circle")))))))
           (new (chain *open-layers
                       *layer
                       (*vector "Nearest Aux Points"
                                (create
                                 display-in-layer-switcher nil
                                 style-map nearest-aux-point-layer-style-map
                                 visibility t))))))

       (defvar *nearest-aux-points-hover-control*
         (new (chain *open-layers
                     *control
                     (*select-feature *streetmap-nearest-aux-points-layer*
                                      (create render-intent "temporary"
                                              hover t
                                              highlight-only t)))))
       
       (defvar *nearest-aux-points-select-control*
         (new (chain *open-layers
                     *control
                     (*select-feature *streetmap-nearest-aux-points-layer*))))
       
       (defvar *pristine-images-p* t
         "T if none of the current images has been clicked into yet.")

       (defvar *current-user-point* undefined
         "The currently selected user-point.")

       (defvar *user-points-hover-control*
         (new (chain *open-layers
                     *control
                     (*select-feature *user-point-layer*
                                      (create render-intent "temporary"
                                              hover t
                                              highlight-only t)))))
       
       (defvar *user-points-select-control*
         (new (chain *open-layers
                     *control
                     (*select-feature *user-point-layer*))))
       
       (defvar *google-streetmap-layer* 
         (new (chain *open-layers
                     *layer
                     (*google "Google Streets"
                              (create num-zoom-levels 22)))))

       (defvar *osm-layer*
         (new (chain *open-layers
                     *layer
                     (*osm* "OpenStreetMap"
                            nil (create num-zoom-levels 19)))))
       
       (defvar *click-streetmap*
         (new (*click-control* (create :trigger request-photos))))

       (defun write-permission-p (&optional (current-owner +user-name+))
         "Nil if current user can't edit stuff created by
current-owner or, without arguments, new stuff."
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

       (setf (getprop *image 'prototype 'show-photo)
        show-photo)
       (setf (getprop *image 'prototype 'draw-epipolar-line)
        draw-epipolar-line)
       (setf (getprop *image 'prototype 'draw-active-point)
        draw-active-point)
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
            for i across *images* do
              (remove-layer (getprop i 'map) layer-name))
         (remove-layer *streetmap* layer-name))

       (defun reset-controls ()
         (disable-element-with-id "finish-point-button")
         (disable-element-with-id "delete-point-button")
         (disable-element-with-id "remove-work-layers-button")
         (setf (inner-html-with-id "h2-controls") "Create Point")
         (setf (inner-html-with-id "creator") nil)
         (setf (inner-html-with-id "point-creation-date") nil)
         (hide-aux-data-choice)
         (setf (inner-html-with-id "aux-numeric-list") nil)
         (setf (inner-html-with-id "aux-text-list") nil))

       (defun disable-streetmap-nearest-aux-points-layer ()
         "Get *streetmap-nearest-aux-points-layer* out of the way,
I.e., remove features and disable feature select control so it won't
shadow any other control."
         (chain *streetmap-nearest-aux-points-layer* (remove-all-features))
         (chain *nearest-aux-points-select-control* (deactivate))
         (chain *user-points-select-control* (activate)))

       (defun reset-layers-and-controls ()
         "Destroy user-generated layers in *streetmap* and in all
*images*, and put controls into pristine state."
         (remove-any-layers "Epipolar Line")
         (remove-any-layers "Active Point")
         (remove-any-layers "Estimated Position")
         (remove-any-layers "User Point")
         (disable-streetmap-nearest-aux-points-layer)
         (when (and (!= undefined *current-user-point*)
                    (chain *current-user-point* layer))
           (chain *user-points-select-control*
                  (unselect *current-user-point*)))
         (reset-controls)
         (setf *pristine-images-p* t)
         (zoom-images-to-max-extent))

       (defun enable-element-with-id (id)
         "Activate HTML element with id=\"id\"."
         (setf (chain document (get-element-by-id id) disabled) nil))

       (defun disable-element-with-id (id)
         "Grey out HTML element with id=\"id\"."
         (setf (chain document (get-element-by-id id) disabled) t))

       (defun hide-element-with-id (id)
         "Hide HTML element wit id=\"id\"."
         (setf (chain document (get-element-by-id id) style display)
               "none"))

       (defun reveal-element-with-id (id)
         "Reveal HTML element wit id=\"id\"."
         (setf (chain document (get-element-by-id id) style display)
               ""))

       (defun hide-aux-data-choice ()
         "Disable selector for auxiliary data."
         ;;(disable-element-with-id "include-aux-data-p")
         (hide-element-with-id "include-aux-data-p")
         (hide-element-with-id "aux-point-distance")
         (setf (chain document
                      (get-element-by-id "aux-point-distance")
                      options
                      length)
               0))

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
         (disable-streetmap-nearest-aux-points-layer)
         (reset-controls)
         (let* ((lonlat
                 (chain *streetmap*
                        (get-lon-lat-from-pixel (@ event xy))
                        (transform +spherical-mercator+
                                   +geographic+)))
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
                               (new (chain *open-layers *geometry (*point
                                     (@ x :m) (@ x :n))))))))
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

       (defun request-nearest-aux-points (global-position count)
         "Draw into streetmap the count nearest points of auxiliary
data."
         (let ((global-position-etc global-position)
               content)
           (setf (chain global-position-etc count) count)
           (setf content (chain *json-parser*
                                (write global-position-etc)))
           (setf (@ *streetmap* aux-local-data-request-response)
                 ((@ *open-layers *Request *POST*)
                  (create :url "/phoros-lib/aux-local-data"
                          :data content
                          :headers (create "Content-type" "text/plain"
                                           "Content-length"
                                           (@ content length))
                          :success draw-nearest-aux-points)))))

       (defun draw-estimated-positions ()
         "Draw into streetmap and into all images points at Estimated
Position.  Estimated Position is the point returned so far from
photogrammetric calculations that are triggered by clicking into
another photo.  Also draw into streetmap the nearest auxiliary points
to Estimated Position."
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
                 (aref estimated-positions-request-response 1))
                (estimated-position-style
                 (create stroke-color (chain *open-layers *feature *vector
                                             style "temporary" stroke-color)
                         point-radius 9
                         fill-opacity 0)))
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
             (setf (chain *streetmap-estimated-position-layer* style)
                   estimated-position-style)
             (chain *streetmap-estimated-position-layer*
                    (add-features feature))
             (chain *streetmap*
                    (add-layer *streetmap-estimated-position-layer*)))
           (request-nearest-aux-points *global-position* 5)
           (loop
              for i in *images*
              for p in estimated-positions
              do
                (when i    ;otherwise a photogrammetry error has occured
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
                           (add-features feature))))))
         (zoom-anything-to-point))

       (defun draw-nearest-aux-points ()
         "Draw a few auxiliary points into streetmap."
         (reveal-element-with-id "include-aux-data-p")
         (reveal-element-with-id "aux-point-distance")
         (let ((features
                (chain *json-parser*
                       (read
                        (getprop *streetmap*
                                 'aux-local-data-request-response
                                 'response-text))
                       features)))
           (disable-streetmap-nearest-aux-points-layer)
           (chain *user-points-select-control* (deactivate))
           (chain *nearest-aux-points-select-control* (activate))
           (chain *nearest-aux-points-hover-control* (activate))
           (setf (chain *aux-point-distance-select*
                        options
                        length)
                 0)
           (loop
              for i in features
              for n from 0 do
                (let* ((point
                        (chain
                         (new
                          (chain *open-layers
                                 *geometry
                                 (*point (chain i geometry coordinates 0)
                                         (chain i geometry coordinates 1))))
                         (transform +geographic+ +spherical-mercator+)))
                       (feature
                        (new
                         (chain *open-layers *feature (*vector point)))))
                  (setf (chain feature attributes)
                        (chain i properties))
                  (setf (chain feature fid) ;this is supposed to correspond to
                        n)                  ; option of *aux-point-distance-select*
                  (chain *streetmap-nearest-aux-points-layer*
                         (add-features feature))
                  (setf aux-point-distance-item
                        (chain document (create-element "option")))
                  (setf (chain aux-point-distance-item text)
                        (+
                         "("
                         n ;let's hope add-features alway stores features in order of arrival
                         ") "
                         (chain i properties distance)))
                  (chain *aux-point-distance-select*
                         (add aux-point-distance-item null))))
           (chain *nearest-aux-points-select-control*
                  (select 
                   (chain
                    (elt (chain *streetmap-nearest-aux-points-layer* features)
                         0))))
           (enable-element-with-id "aux-point-distance")))

       (defun draw-user-point ()
         "Draw currently selected user point into all images."
         (let* ((user-point-positions-response
                 (chain *json-parser*
                        (read
                         (getprop *user-point-in-images-response*
                                  'response-text))))
                (user-point-collections
                 (chain user-point-positions-response image-points))
                ;;(user-point-globally (chain user-point global-position)) ;TODO: what for?
                (user-point-layer-style
                 (create stroke-color "OrangeRed"
                         stroke-width 2
                         point-radius 5
                         fill-opacity 0
                         graphic-name "triangle")))
           (loop
              for i in *images*
              for user-point-collection in user-point-collections
              do
                (when i  ;otherwise a photogrammetry error has occured
                  (let ((features
                         (loop
                            for raw-feature in
                            (chain user-point-collection features)
                            collect
                            (let* ((x
                                    (chain raw-feature geometry coordinates 0))
                                   (y
                                    (chain raw-feature geometry coordinates 1))
                                   (point
                                    (new (chain *open-layers
                                                *geometry
                                                (*point x y))))
                                   (fid
                                    (chain raw-feature id))
                                   (attributes
                                    (chain raw-feature properties))
                                   (feature
                                    (new (chain *open-layers
                                                *feature
                                                (*vector point attributes)))))
                              (setf (chain feature fid) fid)
                              feature))))
                    (setf
                     (@ i user-point-layer)
                     (new (chain *open-layers
                                 *layer
                                 (*vector
                                  "User Point"
                                  (create display-in-layer-switcher nil
                                          style user-point-layer-style)))))
                    (chain i map (add-layer (@ i user-point-layer)))
                    (chain i user-point-layer (add-features features)))))))

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
           (when (checkbox-status-with-id "include-aux-data-p")
             (setf (chain global-position-etc aux-numeric)
                   (chain *current-nearest-aux-point*
                          attributes
                          aux-numeric))
             (setf (chain global-position-etc aux-text)
                   (chain *current-nearest-aux-point*
                          attributes
                          aux-text)))
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
                          (elt (chain *point-attributes-select*
                                      options)
                               (chain *point-attributes-select*
                                      options
                                      selected-index))
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
                                        (create display-in-layer-switcher
                                                nil)))))
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
                  (chain *user-points-select-control*
                         (unselect *current-user-point*)))
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
                          .5)))         ; coordinates shown
                 (new ((@ *open-layers *size) 512 256))
                 (create)))))
         (chain this map (zoom-to-max-extent))
         (setf (chain this trigger-time-div inner-h-t-m-l)
               (iso-time-string (getprop this 'photo-parameters 'trigger-time))))

       (defun zoom-images-to-max-extent ()
         "Zoom out all images."
         (loop for i across *images* do (chain i map (zoom-to-max-extent))))

       (defun zoom-anything-to-point ()
         "For streetmap and for images that have an Active Point or an
Estimated Position, zoom in and recenter."
         (when (checkbox-status-with-id "zoom-to-point-p")
           (let ((point-lonlat
                  (new (chain *open-layers
                              (*lon-lat (chain *global-position* longitude)
                                        (chain *global-position* latitude))
                              (transform +geographic+ +spherical-mercator+)))))
             (when point-lonlat
               (chain *streetmap*
                      (set-center point-lonlat 18 nil t))))
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
                  (when point-lonlat
                    (chain i map (set-center point-lonlat 4 nil t)))))))

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
         "Things to do once a user point is selected."
         (setf *current-user-point* (chain event feature))
         (hide-aux-data-choice)
         (remove-any-layers "Active Point")
         (remove-any-layers "Epipolar Line")
         (remove-any-layers "Estimated Position")
         (remove-any-layers "User Point")
         (if (write-permission-p (chain event feature attributes user-name))
             (progn
               (setf (chain document
                            (get-element-by-id "finish-point-button")
                            onclick)
                     update-point)
               (enable-element-with-id "finish-point-button")
               (enable-element-with-id "delete-point-button")
               (setf (inner-html-with-id "h2-controls") "Edit Point"))
             (progn
               (disable-element-with-id "finish-point-button")
               (disable-element-with-id "delete-point-button")
               (setf (inner-html-with-id "h2-controls") "View Point")))
         (setf (inner-html-with-id "creator")
               (+ "(by " (chain event feature attributes user-name) ")"))
         (setf (value-with-id "point-attribute")
               (chain event feature attributes attribute))
         (setf (value-with-id "point-description")
               (chain event feature attributes description))
         (setf (value-with-id "point-numeric-description")
               (chain event feature attributes numeric-description))
         (setf (inner-html-with-id "point-creation-date")
               (chain event feature attributes creation-date))
         (setf (inner-html-with-id "aux-numeric-list")
               (html-ordered-list
                (chain event feature attributes aux-numeric)))
         (setf (inner-html-with-id "aux-text-list")
               (html-ordered-list
                (chain event feature attributes aux-text)))
         (setf content
               (chain *json-parser*
                      (write
                       (array (array
                               (chain event feature fid)) ;TODO: feed in multiple selection
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

       (defun aux-point-distance-selected ()
         "Things to do on change of aux-point-distance select element."
         (chain *nearest-aux-points-select-control*
                (unselect-all))
         (chain *nearest-aux-points-select-control*
                (select 
                 (chain
                  (elt (chain *streetmap-nearest-aux-points-layer* features)
                       (chain *aux-point-distance-select*
                              options
                              selected-index))))))

       (defun enable-aux-point-selection ()
         "Check checkbox include-aux-data-p and act accordingly."
         (setf (checkbox-status-with-id "include-aux-data-p") t)
         (flip-aux-data-inclusion))

       (defun flip-aux-data-inclusion ()
         "Query status of checkbox include-aux-data-p and act
accordingly."
         (if (checkbox-status-with-id "include-aux-data-p")
             (chain *streetmap-nearest-aux-points-layer*
                    (set-visibility t))
             (chain *streetmap-nearest-aux-points-layer*
                    (set-visibility nil))))

       (defun html-ordered-list (aux-data)
         "Return a html-formatted list from aux-data."
         (if aux-data
             (who-ps-html
              (:ol :class "aux-data-list"
                   (chain aux-data
                          (reduce (lambda (x y)
                                    (+ x (who-ps-html (:li y))))
                                  ""))))
             ""))

       (defun nearest-aux-point-selected (event)
         "Things to do once a nearest auxiliary point is selected in streetmap."
         (setf *current-nearest-aux-point* (chain event feature))
         (let ((aux-numeric
                (chain event feature attributes aux-numeric))
               (aux-text
                (chain event feature attributes aux-text))
               (distance
                (chain event feature attributes distance)))
           (setf (chain *aux-point-distance-select* options selected-index)
                 (chain event feature fid))
           (setf (inner-html-with-id "aux-numeric-list")
                 (html-ordered-list aux-numeric))
           (setf (inner-html-with-id "aux-text-list")
                 (html-ordered-list aux-text))))
       
       (defun init ()
         "Prepare user's playground."
         (when (write-permission-p)
           (enable-element-with-id "point-attribute")
           (enable-element-with-id "point-description")
           (enable-element-with-id "point-numeric-description")
           (setf (inner-html-with-id "h2-controls") "Create Point"))
         (setf *point-attributes-select*
               (chain document (get-element-by-id "point-attribute")))
         (setf *aux-point-distance-select*
               (chain document (get-element-by-id "aux-point-distance")))
         (loop for i in '("solitary" "polyline" "polygon") do
              (setf point-attribute-item (chain document (create-element "option")))
              (setf (chain point-attribute-item text) i)
              (chain *point-attributes-select* (add point-attribute-item null))) ;TODO: input of user-defined attributes
         (hide-aux-data-choice)
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
                                     empty-string "longitude, latitude")))))
               (scale-line-control
                (new (chain *open-layers
                            *control
                            *scale-line))))
           (chain *streetmap*
                  (add-control pan-zoom-panel))
           (chain pan-zoom-panel
                  (add-controls (array pan-west-control
                                       pan-north-control
                                       pan-south-control
                                       pan-east-control
                                       zoom-in-control
                                       zoom-out-control
                                       zoom-to-max-extent-control)))
           (chain *streetmap*
                  (add-control *click-streetmap*))
           (chain *click-streetmap* (activate))

           (chain *user-point-layer*
                  events
                  (register "featureselected"
                            *user-point-layer*
                            user-point-selected))
           (chain *streetmap-nearest-aux-points-layer*
                  events
                  (register "featureselected"
                            *streetmap-nearest-aux-points-layer*
                            nearest-aux-point-selected))
           (chain *streetmap* (add-control *nearest-aux-points-hover-control*))
           (chain *streetmap* (add-control *nearest-aux-points-select-control*))
           (chain *streetmap* (add-control *user-points-hover-control*))
           (chain *streetmap* (add-control *user-points-select-control*))
           (chain *user-points-hover-control* (activate))
           (chain *user-points-select-control* (activate))
           (chain *nearest-aux-points-hover-control* (activate))
           (chain *nearest-aux-points-select-control* (activate))
           
           (chain *streetmap* (add-layer *osm-layer*))
           (try (chain *streetmap* (add-layer *google-streetmap-layer*))
                (:catch (c)
                  (chain *streetmap* (remove-layer *google-streetmap-layer*))))
           (chain *streetmap* (add-layer *streetmap-nearest-aux-points-layer*))
           (chain *streetmap* (add-layer *survey-layer*))
           (chain *streetmap* (add-layer *aux-point-layer*))
           (chain *streetmap* (add-layer *user-point-layer*))
           (setf (chain overview-map element)
                 (chain document (get-element-by-id
                                  "streetmap-overview-element")))
           (chain *streetmap* (add-control overview-map))
           (chain *streetmap*
                  (zoom-to-extent +presentation-project-bounds+))
           (chain *streetmap* (add-control mouse-position-control))
           (chain *streetmap* (add-control scale-line-control)))
         (loop
            for i from 0 to (lisp (1- *number-of-images*))
            do (initialize-image i))
         (add-help-events))))))