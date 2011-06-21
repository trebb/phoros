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

(hunchentoot:define-easy-handler (phoros.js) ()
  "Serve some Javascript."
  (when (hunchentoot:session-value 'authenticated-p)
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
          :presentation-project-emptiness
          (who-ps-html
           (:p "This presentation project is empty.  You can't do much
           with it."))
          :phoros-version
          (who-ps-html
           (:p "Phoros version.")
           (:p "In a version string A.B.C, changes in A denote
           incompatible changes in data (you can't access a database
           set up by a different version of Phoros); changes in B mean
           user-visible changes in feature set; changes in C denote
           bug fixes and minor improvements."))
          :h2-controls
          (who-ps-html
           (:p "Current action."))
          :multiple-points-phoros-controls
          (who-ps-html
           (:p "Try reading the text under mouse pointer."))
          :finish-point-button
          (who-ps-html
           (:p "Store user point with its attribute,
           numeric-description, description, and auxiliary data into
           database."))
          :delete-point-button
          (who-ps-html
           (:p "Delete current point."))
          :download-user-points-button
          (who-ps-html
           (:p "Download all user points as GeoJSON-fomatted text
           file.  Do this regularly if you don't want to lose your
           work due to server crashes or major Phoros updates.")
           (:p "Points saved this way can be fed back into your
           project using the command line interface (on server or on
           any other host where the database is reachable)."))
          :point-attribute
          (who-ps-html
           (:h3 "\"attribute\"")
           (:p "The standard ones, polygon, polyline, and solitary are
           rendered as asterisk, square, and triangle
           respectively.  Anything else is rendered as an X."))
          :point-description
          (who-ps-html
           (:h3 "\"description\"")
           (:p "Optional textual description of user point."))
          :point-numeric-description
          (who-ps-html
           (:h3 "\"numeric-description\"")
           (:p "Optional additional description of user point.  It is
           occasionally used to label representions of this
           point in streetmap and in images.")
           (:p "If parts of it look like numbers, the leftmost such
           part is automatically incremented during first click into
           an image."))
          :point-creation-date
          (who-ps-html
           (:p "Creation date of current user point.  Will be updated
           when you change this point."))
          :include-aux-data
          (who-ps-html
           (:p "Check this if the user point being created is to
           include auxiliary data."))
          :aux-point-distance
          (who-ps-html
           (:p "Select a set of auxiliary data, either by its distance
           (in metres) from the current estimated position, or by
           clicking its representation in streetmap."))
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
           (:p "Discard the current, unstored user point or unselect
           any selected user points.  Zoom out all images. Keep
           the rest of the workspace untouched."))
          :blurb-button
          (who-ps-html
           (:p "View some info about Phoros."))
          :logout-button
          (who-ps-html
           (:p "Finish this session after storing current streetmap
           zoom status and your cursor position.")
           (:p "Fresh login is required to continue."))
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
          :walk-mode
          (who-ps-html
           (:p "Check this to snap your current position onto a line
           along points of auxiliary data, and to keep streetmap
           centered around current position."))
          :decrease-step-size
          (who-ps-html
           (:p "Decrease step size.  Double-click to decrease harder."))
          :step-size
          (who-ps-html
           (:p "Step size in metres.  Click to increase; double-click
           to increase harder."))
          :increase-step-size
          (who-ps-html
           (:p "Increase step size.  Double-click to increase harder."))
          :step-button
          (who-ps-html
           (:p "Move your position by one step on a line along points
           of auxiliary data.  Double-click to change direction."))
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
           (:p "Click to re-center streetmap, or drag the red
           rectangle."))
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
            :initialize
            (lambda (options)
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
                                         :click (@ this trigger)))))))))))

       (defvar +unix-epoch+ (lisp *unix-epoch*)
         "Seconds between Lisp epoch and UNIX epoch.")
       (defvar +geographic+
         (new (chain *open-layers (*projection "EPSG:4326"))))
       (defvar +spherical-mercator+
         (new (chain *open-layers (*projection "EPSG:900913"))))

       (defvar +user-name+ (lisp (hunchentoot:session-value 'user-name))
         "User's (short) name.")
       (defvar +user-role+ (lisp (string-downcase (hunchentoot:session-value
                                                   'user-role)))
         "User's permissions.")

       (defvar +presentation-project-bbox-text+
         (lisp (hunchentoot:session-value 'presentation-project-bbox)))

       (defvar +presentation-project-bounds+
         (chain (new (chain *open-layers
                            *bounds
                            (from-string
                             (or +presentation-project-bbox-text+
                                 "-180,-89,180,89"))))
                (transform +geographic+ +spherical-mercator+))
         "Bounding box of the entire presentation project.")

       (defvar +aux-data-p+
         (lisp (hunchentoot:session-value 'aux-data-p)))

       (defvar *images* (array) "Collection of the photos currently shown.")

       (defvar *streetmap* undefined
         "The streetmap shown to the user.")

       (defvar *point-attributes-select* undefined
         "The HTML element for selecting user point attributes.")

       (defvar *aux-point-distance-select* undefined
         "The HTML element for selecting one of a few nearest
         auxiliary points.")

       (defvar *global-position* undefined
         "Coordinates of the current estimated position")

       (defvar *linestring-step-ratio* 4
         "Look for auxiliary points to include into linestring within
         a radius of *linestring-step-ratio* multilied by multiplied by
         step-size.")

       (defvar *current-nearest-aux-point*
         (create attributes (create aux-numeric undefined
                                    aux-text undefined))
         "Attributes of currently selected point of auxiliary data.")


       (defvar *bbox-strategy* (@ *open-layers *strategy *bbox*))
       (setf (@ *bbox-strategy* prototype ratio) 1.5)
       (setf (@ *bbox-strategy* prototype res-factor) 1.5)

       (defvar *json-parser* (new (chain *open-layers *format *json*)))

       (defvar *geojson-format* (chain *open-layers *format *geo-j-s-o-n))
       (setf (@ *geojson-format* prototype ignore-extra-dims)
        t)                              ;doesn't handle height anyway
       (setf (@ *geojson-format* prototype external-projection)
        +geographic+)
       (setf (@ *geojson-format* prototype internal-projection)
        +geographic+)

       (defvar *wkt-parser*
         (new (chain *open-layers
                     *format
                     (*wkt*
                      (create external-projection +geographic+
                              internal-projection +spherical-mercator+)))))

       (defvar *http-protocol* (chain *open-layers *protocol *http*))
       (setf (chain *http-protocol* prototype format) (new *geojson-format*))

       (defvar *pristine-images-p* t
         "T if none of the current images has been clicked into yet.")

       (defvar *current-user-point* undefined
         "The currently selected user-point.")

       (defun write-permission-p (&optional (current-owner +user-name+))
         "Nil if current user can't edit stuff created by
current-owner or, without arguments, new stuff."
         (or (equal +user-role+ "admin")
             (and (equal +user-role+ "write")
                  (equal +user-name+ current-owner))))

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
         (+ "/phoros/lib/photo/" (@ photo-parameters directory) "/"
            (@ photo-parameters filename) "/"
            (@ photo-parameters byte-position) ".png"
            "?mounting-angle=" (@ photo-parameters mounting-angle)
            "&bayer-pattern=" (@ photo-parameters bayer-pattern)
            "&color-raiser=" (@ photo-parameters color-raiser)))

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
         (reveal-element-with-id "real-phoros-controls")
         (hide-element-with-id "multiple-points-phoros-controls")
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
         "Get (@ *streetmap* nearest-aux-points-layer) out of the way,
I.e., remove features and disable feature select control so it won't
shadow any other control."
         (chain *streetmap* nearest-aux-points-layer (remove-all-features))
         (chain *streetmap* nearest-aux-points-select-control (deactivate))
         (chain *streetmap* user-points-select-control (activate)))

       (defun reset-layers-and-controls ()
         "Destroy user-generated layers in *streetmap* and in all
*images*, and put controls into pristine state."
         (remove-any-layers "Epipolar Line")
         (remove-any-layers "Active Point")
         (remove-any-layers "Estimated Position")
         (remove-any-layers "User Point")
         (chain *streetmap* user-points-select-control (unselect-all))
         (disable-streetmap-nearest-aux-points-layer)
         (when (and (not (equal undefined *current-user-point*))
                    (@ *current-user-point* layer))
           (chain *streetmap*
                  user-points-select-control
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
         (hide-element-with-id "include-aux-data")
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
         "Handle the response triggered by request-photos-for-point."
         (let ((photo-parameters
                (chain *json-parser*
                       (read (@ *streetmap*
                                photo-request-response response-text)))))
           (loop
              for p across photo-parameters
              for i across *images*
              do
              (setf (getprop i 'photo-parameters) p)
              ((getprop i 'show-photo)))
           ;; (setf (@ (aref photo-parameters 0) angle180) 1) ; Debug: coordinate flipping
           ))

       (defun consolidate-combobox (combobox-id)
         "Help faking a combobox: copy selected option into input."
         (let ((combobox-select (+ combobox-id "-select"))
               (combobox-input (+ combobox-id "-input")))
           (setf (value-with-id combobox-input)
                 (getprop (chain document
                                 (get-element-by-id combobox-select)
                                 options)
                          (chain document
                                 (get-element-by-id combobox-select)
                                 selected-index)
                          'value))
           (chain document
                  (get-element-by-id combobox-input)
                  (focus))))

       (defun unselect-combobox-selection (combobox-id)
         "Help faking a combobox: unset selected option so any
         selection there will trigger an onchange event."
         (let ((combobox-select (+ combobox-id "-select")))
           (setf (chain document
                        (get-element-by-id combobox-select)
                        selected-index)
                 -1)))
          
       (defun stuff-combobox (combobox-id values &optional selection)
         "Stuff combobox with values.  If selection is a number,
         select the respective item."
         (let ((combobox-select (+ combobox-id "-select"))
               (combobox-input (+ combobox-id "-input")))
           (setf (chain document
                        (get-element-by-id combobox-select)
                        options
                        length)
                 0)
           (loop for i in values do
                (setf combobox-item
                      (chain document (create-element "option")))
                (setf (@ combobox-item text) i)
                (chain document
                       (get-element-by-id combobox-select)
                       (add combobox-item null)))
           (when selection
             (setf (chain document
                          (get-element-by-id combobox-select)
                          selected-index)
                   selection)
             (consolidate-combobox combobox-id))))

       (defun stuff-user-point-comboboxes (&optional selectp)
         "Stuff user point attribute comboboxes with sensible values.
         If selectp it t, select the most frequently used one."
         (let* ((response
                 (chain *json-parser*
                        (read (@ *streetmap*
                                 user-point-choice-response response-text))))
                (attributes
                 (chain response attributes (map (lambda (x)
                                                   (@ x attribute)))))
                (descriptions
                 (chain response descriptions (map (lambda (x)
                                                     (@ x description)))))
                best-used-attribute
                best-used-description)
           (when selectp
             (loop
                with maximum = 0
                for i across (@ response descriptions)
                for k from 0
                do (when (< maximum (@ i count))
                     (setf maximum (@ i count))
                     (setf best-used-description k)))
             (loop
                with maximum = 0
                for i across (@ response attributes)
                for k from 0
                do (when (< maximum (@ i count))
                     (setf maximum (@ i count))
                     (setf best-used-attribute k))))
           (stuff-combobox
            "point-attribute" attributes best-used-attribute)
           (stuff-combobox
            "point-description" descriptions best-used-description)))

       (defun request-user-point-choice (&optional selectp)
         "Stuff user point attribute comboboxes with sensible values.
         If selectp it t, select the most frequently used one."
         (setf (@ *streetmap* user-point-choice-response)
               ((@ *open-layers *Request *POST*)
                (create :url "/phoros/lib/user-point-attributes.json"
                        :data nil
                        :headers (create "Content-type" "text/plain")
                        :success (lambda ()
                                   (stuff-user-point-comboboxes selectp))))))

       (defun request-photos-after-click (event)
         "Handle the response to a click into *streetmap*; fetch photo
          data.  Set or update streetmap cursor."
         (request-photos (chain *streetmap*
                                (get-lon-lat-from-pixel (@ event xy)))))

       (defun request-photos (lonlat)
         "Fetch photo data for a point near lonlat.  Set or update
          streetmap cursor."
         (setf (@ *streetmap* clicked-lonlat) lonlat)
         (if (checkbox-status-with-id "walk-p")
             (request-aux-data-linestring-for-point
              (@ *streetmap* clicked-lonlat))
             (request-photos-for-point (@ *streetmap* clicked-lonlat))))

       (defun request-aux-data-linestring-for-point (lonlat-spherical-mercator)
         "Fetch a linestring along auxiyliary points near
          lonlat-spherical-mercator."
         (let ((lonlat-geographic
                (chain lonlat-spherical-mercator
                       (clone)
                       (transform +spherical-mercator+ +geographic+))))
           (request-aux-data-linestring (@ lonlat-geographic lon)
                                        (@ lonlat-geographic lat)
                                        (* *linestring-step-ratio*
                                           (step-size-degrees))
                                        (step-size-degrees))))

       (defun request-photos-for-point (lonlat-spherical-mercator)
         "Fetch photo data near lonlat-spherical-mercator; set or
          update streetmap cursor."
         (disable-element-with-id "finish-point-button")
         (disable-element-with-id "remove-work-layers-button")
         (remove-any-layers "Estimated Position")
         (disable-streetmap-nearest-aux-points-layer)
         (reset-controls)
         (let* ((lonlat-geographic
                 (chain lonlat-spherical-mercator
                        (clone)
                        (transform +spherical-mercator+ +geographic+)))
                (content
                 (chain *json-parser*
                        (write
                         (create :longitude (@ lonlat-geographic lon)
                                 :latitude (@ lonlat-geographic lat)
                                 :zoom ((@ *streetmap* get-zoom))
                                 :count (lisp *number-of-images*))))))
           (chain *streetmap*
                  cursor-layer
                  (remove-all-features))
           (chain *streetmap*
                  cursor-layer
                  (add-features
                   (new (chain *open-layers
                               *feature
                               (*vector
                                (new (chain
                                      *open-layers
                                      *geometry
                                      (*point (@ lonlat-spherical-mercator
                                                 lon)
                                              (@ lonlat-spherical-mercator
                                                 lat)))))))))
           (chain *streetmap*
                  overview-cursor-layer
                  (remove-all-features))
           (chain *streetmap*
                  overview-cursor-layer
                  (add-features
                   (new (chain *open-layers
                               *feature
                               (*vector
                                (new (chain
                                      *open-layers
                                      *geometry
                                      (*point (@ lonlat-spherical-mercator
                                                 lon)
                                              (@ lonlat-spherical-mercator
                                                 lat)))))))))
           (setf (@ *streetmap* photo-request-response)
                 ((@ *open-layers *Request *POST*)
                  (create :url "/phoros/lib/local-data"
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
                               (new (chain *open-layers
                                           *geometry
                                           (*point
                                            (@ x :m) (@ x :n))))))))
                (feature
                 (new (chain *open-layers
                             *feature
                             (*vector
                              (new (chain
                                    *open-layers
                                    *geometry
                                    (*line-string points))))))))
           (setf (@ feature render-intent) "temporary")
           (chain this epipolar-layer
                  (add-features feature))))
       ;; either *line-string or *multi-point are usable

       (defun request-nearest-aux-points (global-position count)
         "Draw into streetmap the count nearest points of auxiliary
data."
         (let ((global-position-etc global-position)
               content)
           (setf (@ global-position-etc count) count)
           (setf content (chain *json-parser*
                                (write global-position-etc)))
           (setf (@ *streetmap* aux-local-data-request-response)
                 ((@ *open-layers *Request *POST*)
                  (create :url "/phoros/lib/aux-local-data"
                          :data content
                          :headers (create "Content-type" "text/plain"
                                           "Content-length"
                                           (@ content length))
                          :success draw-nearest-aux-points)))))

       (defun request-aux-data-linestring (longitude latitude radius step-size)
         "Draw into streetmap a piece of linestring threaded along the
          nearest points of auxiliary data inside radius."
         (let* ((payload (create longitude longitude
                                 latitude latitude
                                 radius radius
                                 step-size step-size
                                 azimuth (@ *streetmap*
                                            linestring-central-azimuth)))
                (content (chain *json-parser* (write payload))))
           (setf (@ *streetmap* aux-data-linestring-request-response)
                 ((@ *open-layers *Request *POST*)
                  (create :url "/phoros/lib/aux-local-linestring.json"
                          :data content
                          :headers (create "Content-type" "text/plain"
                                           "Content-length"
                                           (@ content length))
                          :success draw-aux-data-linestring)))))

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
                 (create stroke-color (chain *open-layers
                                             *feature
                                             *vector
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
             (setf (@ feature render-intent) "temporary")
             (setf (@ *streetmap* estimated-position-layer)
                   (new (chain *open-layers
                               *layer
                               (*vector
                                "Estimated Position"
                                (create display-in-layer-switcher nil)))))
             (setf (@ *streetmap* estimated-position-layer style)
                   estimated-position-style)
             (chain *streetmap* estimated-position-layer (add-features feature))
             (chain *streetmap*
                    (add-layer (@ *streetmap* estimated-position-layer))))
           (request-nearest-aux-points *global-position* 7)
           (loop
              for i in *images*
              for p in estimated-positions
              do
              (when p    ;otherwise a photogrammetry error has occured
                (setf (@ i estimated-position-layer)
                      (new
                       (chain *open-layers
                              *layer
                              (*vector
                               "Estimated Position"
                               (create display-in-layer-switcher nil)))))
                (setf (@ i estimated-position-lonlat)
                      (new (chain *open-layers (*lon-lat
                                                (getprop p 'm)
                                                (getprop p 'n)))))
                (setf (@ i estimated-position-layer style)
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
         (zoom-anything-to-point)
         (chain document
                (get-element-by-id "finish-point-button")
                (focus)))

       (defun draw-nearest-aux-points ()
         "Draw a few auxiliary points into streetmap."
         (reveal-element-with-id "include-aux-data")
         (reveal-element-with-id "aux-point-distance")
         (let ((features
                (chain *json-parser*
                       (read
                        (getprop *streetmap*
                                 'aux-local-data-request-response
                                 'response-text))
                       features)))
           (disable-streetmap-nearest-aux-points-layer)
           (chain *streetmap* user-points-select-control (deactivate))
           (chain *streetmap* nearest-aux-points-select-control (activate))
           (chain *streetmap* nearest-aux-points-hover-control (activate))
           (setf (@ *aux-point-distance-select* options length)
                 0)
           (loop
              for i in features
              for n from 0 do
              (let* ((point
                      (chain
                       (new
                        (chain *open-layers
                               *geometry
                               (*point (@ i geometry coordinates 0)
                                       (@ i geometry coordinates 1))))
                       (transform +geographic+ +spherical-mercator+)))
                     (feature
                      (new
                       (chain *open-layers *feature (*vector point)))))
                (setf (@ feature attributes)
                      (@ i properties))
                (setf (@ feature fid) ;this is supposed to correspond to
                      n)       ; option of *aux-point-distance-select*
                (chain *streetmap*
                       nearest-aux-points-layer
                       (add-features feature))
                (setf aux-point-distance-item
                      (chain document (create-element "option")))
                (setf (@ aux-point-distance-item text)
                      (+
                       "("
                       n ;let's hope add-features alway stores features in order of arrival
                       ") "
                       (chain *open-layers
                              *number
                              (format (@ i properties distance) 3 ""))))
                (chain *aux-point-distance-select*
                       (add aux-point-distance-item null))))
           (chain *streetmap*
                  nearest-aux-points-select-control
                  (select 
                   (chain
                    (elt (@ *streetmap* nearest-aux-points-layer features)
                         0))))
           (enable-element-with-id "aux-point-distance")))

       (defun draw-aux-data-linestring ()
         "Draw a piece of linestring along a few auxiliary points into
          streetmap.  Pan streetmap accordingly."
         (let* ((data
                 (@ *streetmap*
                    aux-data-linestring-request-response
                    response-text))
                (linestring-wkt
                 (chain *json-parser* (read data) linestring))
                (current-point-wkt
                 (chain *json-parser* (read data) current-point))
                (previous-point-wkt
                 (chain *json-parser* (read data) previous-point))
                (next-point-wkt
                 (chain *json-parser* (read data) next-point))
                (azimuth
                 (chain *json-parser* (read data) azimuth))
                (linestring
                 (chain *wkt-parser* (read linestring-wkt)))
                (current-point
                 (chain *wkt-parser* (read current-point-wkt)))
                (previous-point
                 (chain *wkt-parser* (read previous-point-wkt)))
                (next-point
                 (chain *wkt-parser* (read next-point-wkt)))
                (current-point-lonlat
                 (new (chain *open-layers
                             (*lon-lat (@ current-point geometry x)
                                       (@ current-point geometry y))))))
           (chain *streetmap* (pan-to current-point-lonlat))
           (setf (@ *streetmap* linestring-central-azimuth) azimuth)
           (request-photos-for-point current-point-lonlat)
           (setf (@ *streetmap* step-back-point) previous-point)
           (setf (@ *streetmap* step-forward-point) next-point)
           (chain *streetmap* aux-data-linestring-layer (remove-all-features))
           (chain *streetmap*
                  aux-data-linestring-layer
                  (add-features linestring))))

       (defun step (&optional back-p)
         "Enable walk-mode if necessary, and do a step along
          aux-data-linestring."
         (if (checkbox-status-with-id "walk-p")
             (let ((next-point-geometry
                    (if back-p
                        (progn
                          (if (< (- (@ *streetmap* linestring-central-azimuth) pi) 0)
                              (setf (@ *streetmap* linestring-central-azimuth)
                                    (+ (@ *streetmap* linestring-central-azimuth) pi))
                              (setf (@ *streetmap* linestring-central-azimuth)
                                    (- (@ *streetmap* linestring-central-azimuth) pi)))
                          (chain *streetmap*
                                 step-back-point
                                 (clone)
                                 geometry
                                 (transform +spherical-mercator+ +geographic+)))
                        (chain *streetmap*
                               step-forward-point
                               (clone)
                               geometry
                               (transform +spherical-mercator+ +geographic+)))))
               (request-aux-data-linestring (@ next-point-geometry x)
                                            (@ next-point-geometry y)
                                            (* *linestring-step-ratio*
                                               (step-size-degrees))
                                            (step-size-degrees)))
             (progn
               (setf (checkbox-status-with-id "walk-p") t) ;doesn't seem to trigger event
               (flip-walk-mode))))    ; so we have to do it explicitly

       (defun step-size-degrees ()
         "Return inner-html of element step-size (metres)
converted into map units (degrees).  You should be close to the
equator."
         (/ (inner-html-with-id "step-size") 1855.325 60))

       (defun decrease-step-size ()
         (when (> (inner-html-with-id "step-size") 0.5)
           (setf (inner-html-with-id "step-size")
                 (/ (inner-html-with-id "step-size") 2))))

       (defun increase-step-size ()
         (when (< (inner-html-with-id "step-size") 100)
           (setf (inner-html-with-id "step-size")
                 (* (inner-html-with-id "step-size") 2))))

       (defun user-point-style-map (label-property)
         "Create a style map where styles dispatch on feature property
         \"attribute\" and features are labelled after feature
         property label-property."
         (let* ((symbolizer-property "attribute")
                (solitary-filter
                 (new (chain *open-layers
                             *filter
                             (*comparison (create type (chain *open-layers
                                                              *filter
                                                              *comparison
                                                              *like*)
                                                  property symbolizer-property
                                                  value "solitary")))))
                (polyline-filter
                 (new (chain *open-layers
                             *filter
                             (*comparison (create type (chain *open-layers
                                                              *filter
                                                              *comparison
                                                              *like*)
                                                  property symbolizer-property
                                                  value "polyline")))))
                (polygon-filter
                 (new (chain *open-layers
                             *filter
                             (*comparison (create type (chain *open-layers
                                                              *filter
                                                              *comparison
                                                              *like*)
                                                  property symbolizer-property
                                                  value "polygon")))))
                (solitary-rule
                 (new (chain *open-layers
                             (*rule (create
                                     filter solitary-filter 
                                     symbolizer (create
                                                 graphic-name "triangle"))))))
                (polyline-rule
                 (new (chain *open-layers
                             (*rule (create
                                     filter polyline-filter 
                                     symbolizer (create
                                                 graphic-name "square"
                                                 point-radius 4))))))
                (polygon-rule
                 (new (chain *open-layers
                             (*rule (create
                                     filter polygon-filter 
                                     symbolizer (create
                                                 graphic-name "star"))))))
                (else-rule
                 (new (chain *open-layers
                             (*rule (create
                                     else-filter t
                                     symbolizer (create
                                                 graphic-name "x"))))))
                (user-point-default-style
                 (new (chain
                       *open-layers
                       (*style (create stroke-color "OrangeRed"
                                       fill-color "OrangeRed"
                                       label-align "cb"
                                       label-y-offset 5
                                       font-color "OrangeRed"
                                       font-family "'andale mono', 'lucida console', monospace"
                                       stroke-opacity .5
                                       stroke-width 2
                                       point-radius 5
                                       fill-opacity 0)
                               (create rules (array solitary-rule
                                                    polyline-rule
                                                    polygon-rule
                                                    else-rule))))))
                (user-point-select-style
                 (new (chain
                       *open-layers
                       (*style (create stroke-opacity 1
                                       label label-property)
                               (create rules (array solitary-rule
                                                    polyline-rule
                                                    polygon-rule
                                                    else-rule))))))
                (user-point-temporary-style
                 (new (chain
                       *open-layers
                       (*style (create fill-opacity .5)
                               (create rules (array solitary-rule
                                                    polyline-rule
                                                    polygon-rule
                                                    else-rule)))))))
           (new (chain *open-layers
                       (*style-map
                        (create "default" user-point-default-style
                                "temporary" user-point-temporary-style
                                "select" user-point-select-style))))))

       (defun draw-user-points ()
         "Draw currently selected user points into all images."
         (let* ((user-point-positions-response
                 (chain *json-parser*
                        (read
                         (getprop *user-point-in-images-response*
                                  'response-text))))
                (user-point-collections
                 (chain user-point-positions-response image-points))
                (user-point-count
                 (chain user-point-positions-response user-point-count))
                (label
                 (when (> user-point-count 1) "${numericDescription}")))
           (loop
              for i in *images*
              for user-point-collection in user-point-collections
              do
              (when i    ;otherwise a photogrammetry error has occured
                (let ((features
                       (loop
                          for raw-feature in
                          (@ user-point-collection features)
                          collect
                          (let* ((x
                                  (@ raw-feature geometry coordinates 0))
                                 (y
                                  (@ raw-feature geometry coordinates 1))
                                 (point
                                  (new (chain *open-layers
                                              *geometry
                                              (*point x y))))
                                 (fid
                                  (@ raw-feature id))
                                 (attributes
                                  (@ raw-feature properties))
                                 (feature
                                  (new (chain *open-layers
                                              *feature
                                              (*vector point attributes)))))
                            (setf (@ feature fid) fid)
                            (setf (@ feature render-intent) "select")
                            feature))))
                  (setf
                   (@ i user-point-layer)
                   (new (chain *open-layers
                               *layer
                               (*vector
                                "User Point"
                                (create display-in-layer-switcher nil
                                        style-map (user-point-style-map
                                                   label))))))
                  (chain i map (add-layer (@ i user-point-layer)))
                  (chain i user-point-layer (add-features features)))))))

       (defun finish-point ()
         "Send current *global-position* as a user point to the database."
         (let ((global-position-etc *global-position*))
           (setf (@ global-position-etc attribute)
                 (value-with-id "point-attribute-input"))
           (setf (@ global-position-etc description)
                 (value-with-id "point-description-input"))
           (setf (@ global-position-etc numeric-description)
                 (value-with-id "point-numeric-description"))
           (when (checkbox-status-with-id "include-aux-data-p")
             (setf (@ global-position-etc aux-numeric)
                   (@ *current-nearest-aux-point*
                      attributes
                      aux-numeric))
             (setf (@ global-position-etc aux-text)
                   (@ *current-nearest-aux-point*
                      attributes
                      aux-text)))
           (let ((content 
                  (chain *json-parser*
                         (write global-position-etc))))
             ((@ *open-layers *Request *POST*)
              (create :url "/phoros/lib/store-point"
                      :data content
                      :headers (create "Content-type" "text/plain"
                                       "Content-length" (@ content length))
                      :success (lambda ()
                                 (refresh-layer
                                  (@ *streetmap* user-point-layer))
                                 (reset-layers-and-controls)
                                 (request-user-point-choice)))))))
           
       (defun increment-numeric-text (text)
         "Increment text if it looks like a number, and return it."
         (let* ((parts (chain (regex "(\\D*)(\\d*)(.*)") (exec text)))
                (old-number (elt parts 2))
                (new-number (1+ (parse-int old-number 10)))))
         (if (is-finite new-number)
             (+ (elt parts 1) new-number (elt parts 3))
             text))

       (defun update-point ()
         "Send changes to currently selected user point to database."
         (let* ((point-data
                 (create user-point-id (@ *current-user-point* fid)
                         attribute
                         (value-with-id "point-attribute-input")
                         description
                         (value-with-id "point-description-input")
                         numeric-description
                         (value-with-id "point-numeric-description")))
                (content 
                 (chain *json-parser*
                        (write point-data))))
           ((@ *open-layers *Request *POST*)
            (create :url "/phoros/lib/update-point"
                    :data content
                    :headers (create "Content-type" "text/plain"
                                     "Content-length" (@ content length))
                    :success (lambda ()
                               (refresh-layer
                                (@ *streetmap* user-point-layer))
                               (reset-layers-and-controls)
                               (request-user-point-choice))))))

       (defun delete-point ()
         "Purge currently selected user point from database."
         (let ((user-point-id (@ *current-user-point* fid)))
           (setf content 
                 (chain *json-parser*
                        (write user-point-id)))
           ((@ *open-layers *Request *POST*)
            (create :url "/phoros/lib/delete-point"
                    :data content
                    :headers (create "Content-type" "text/plain"
                                     "Content-length" (@ content length))
                    :success (lambda ()
                               (refresh-layer
                                (@ *streetmap* user-point-layer))
                               (reset-layers-and-controls)
                               (request-user-point-choice true))))))

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
                (chain *streetmap* user-points-select-control (unselect-all))
                (reset-controls)
                (setf (value-with-id "point-numeric-description")
                      (increment-numeric-text
                       (value-with-id "point-numeric-description")))
                (remove-any-layers "User Point") ;from images
                (loop
                   for i across *images* do
                   (unless (equal i clicked-image)
                     (setf
                      (@ i epipolar-layer)
                      (new (chain *open-layers
                                  *layer
                                  (*vector "Epipolar Line"
                                           (create
                                            display-in-layer-switcher nil))))
                      content (chain *json-parser*
                                     (write
                                      (append (array photo-parameters)
                                              (@ i photo-parameters))))
                      (@ i epipolar-request-response)
                      ((@ *open-layers *Request *POST*)
                       (create :url "/phoros/lib/epipolar-line"
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
                         (create :url "/phoros/lib/estimated-positions"
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
         (let ((image-div-width
                (parse-int (chain (get-computed-style (@ this map div) nil)
                                  width)))
               (image-div-height
                (parse-int (chain (get-computed-style (@ this map div) nil)
                                  height)))
               (image-width
                (getprop this 'photo-parameters 'sensor-width-pix))
               (image-height
                (getprop this 'photo-parameters 'sensor-height-pix)))
           ((getprop this 'map 'add-layer)
            (new (chain
                  *open-layers
                  *layer
                  (*image
                   "Photo"
                   (photo-path (getprop this 'photo-parameters))
                   (new (chain *open-layers
                               (*bounds
                                -.5 -.5
                                (+ image-width .5) (+ image-height .5))))
                   (new (chain *open-layers
                               (*size image-div-width
                                      image-div-height)))
                   (create
                    max-resolution (chain
                                    *math
                                    (max (/ image-width image-div-width)
                                         (/ image-height image-div-height))))))))
           (chain this map (zoom-to-max-extent))
           (setf (@ this trigger-time-div inner-h-t-m-l)
                 (iso-time-string (getprop this 'photo-parameters 'trigger-time)))))

       (defun zoom-images-to-max-extent ()
         "Zoom out all images."
         (loop for i across *images* do (chain i map (zoom-to-max-extent))))

       (defun zoom-anything-to-point ()
         "For streetmap and for images that have an Active Point or an
Estimated Position, zoom in and recenter."
         (when (checkbox-status-with-id "zoom-to-point-p")
           (let ((point-lonlat
                  (new (chain *open-layers
                              (*lon-lat (@ *global-position* longitude)
                                        (@ *global-position* latitude))
                              (transform +geographic+ +spherical-mercator+)))))
             (when point-lonlat
               (chain *streetmap*
                      (set-center point-lonlat 18 nil t))))
           (loop for i across *images* do
                (let ((point-lonlat
                       (cond
                         ((has-layer-p (@ i map) "Active Point")
                          (new (chain *open-layers (*lon-lat
                                                    (@ i photo-parameters m)
                                                    (@ i photo-parameters n)))))
                         ((has-layer-p (@ i map) "Estimated Position")
                          (@ i estimated-position-lonlat))
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
           (chain (aref *images* image-index)
                  map
                  (add-control pan-zoom-panel))
           (chain pan-zoom-panel
                  (add-controls (array pan-west-control
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
         (remove-any-layers "Active Point")
         (remove-any-layers "Epipolar Line")
         (remove-any-layers "Estimated Position")
         (user-point-selection-changed))

       (defun user-point-unselected (event)
         "Things to do once a user point is unselected."
         (reset-controls)
         (user-point-selection-changed))

       (defun user-point-selection-changed ()
         "Things to do once a user point is selected or unselected."
         (hide-aux-data-choice)
         (setf *current-user-point*
               (@ *streetmap* user-point-layer selected-features 0))
         (let ((selected-features-count
                (@ *streetmap* user-point-layer selected-features length)))
           (setf (@ *streetmap* user-point-layer style-map)
                 (user-point-style-map 
                  (when (> selected-features-count 1)
                    "${numericDescription}")))
           (cond
             ((> selected-features-count 1)
              (hide-element-with-id "real-phoros-controls")
              (reveal-element-with-id "multiple-points-phoros-controls"))
             ((= selected-features-count 1)
              (setf (value-with-id "point-attribute-input")
                    (@ *current-user-point* attributes attribute))
              (setf (value-with-id "point-description-input")
                    (@ *current-user-point* attributes description))
              (setf (value-with-id "point-numeric-description")
                    (@ *current-user-point* attributes numeric-description))
              (setf (inner-html-with-id "point-creation-date")
                    (@ *current-user-point* attributes creation-date))
              (setf (inner-html-with-id "aux-numeric-list")
                    (html-ordered-list
                     (@ *current-user-point* attributes aux-numeric)))
              (setf (inner-html-with-id "aux-text-list")
                    (html-ordered-list
                     (@ *current-user-point* attributes aux-text)))
              (if (write-permission-p
                   (@ *current-user-point* attributes user-name))
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
                    (+ "(by "
                       (@ *current-user-point* attributes user-name)
                       ")")))
             (t
              (hide-element-with-id "multiple-points-phoros-controls")
              (reveal-element-with-id "real-phoros-controls"))))
         (chain *streetmap* user-point-layer (redraw))
         (remove-any-layers "User Point") ;from images
         (setf content
               (chain *json-parser*
                      (write
                       (array (chain *streetmap*
                                     user-point-layer
                                     selected-features
                                     (map (lambda (x) (@ x fid))))
                              (loop
                                 for i across *images*
                                 collect (@ i photo-parameters))))))
         (setf *user-point-in-images-response*
               ((@ *open-layers *Request *POST*)
                (create :url "/phoros/lib/user-point-positions"
                        :data content
                        :headers (create "Content-type" "text/plain"
                                         "Content-length" (@ content length))
                        :success draw-user-points))))

       (defun aux-point-distance-selected ()
         "Things to do on change of aux-point-distance select element."
         (chain *streetmap*
                nearest-aux-points-select-control
                (unselect-all))
         (chain *streetmap*
                nearest-aux-points-select-control
                (select 
                 (chain
                  (elt (@ *streetmap* nearest-aux-points-layer features)
                       (@ *aux-point-distance-select*
                          options
                          selected-index))))))

       (defun enable-aux-point-selection ()
         "Check checkbox include-aux-data-p and act accordingly."
         (setf (checkbox-status-with-id "include-aux-data-p") t)
         (flip-aux-data-inclusion))

       (defun flip-walk-mode ()
         "Query status of checkbox walk-p and induce first walking
          step if it's just been turned on.  Otherwise delete our
          walking path."
         (if (checkbox-status-with-id "walk-p")
             (request-aux-data-linestring-for-point (@ *streetmap*
                                                       clicked-lonlat))
             (chain *streetmap*
                    aux-data-linestring-layer
                    (remove-all-features))))

       (defun flip-aux-data-inclusion ()
         "Query status of checkbox include-aux-data-p and act
          accordingly."
         (if (checkbox-status-with-id "include-aux-data-p")
             (chain *streetmap*
                    nearest-aux-points-layer
                    (set-visibility t))
             (chain *streetmap*
                    nearest-aux-points-layer
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
         (setf *current-nearest-aux-point* (@ event feature))
         (let ((aux-numeric
                (@ event feature attributes aux-numeric))
               (aux-text
                (@ event feature attributes aux-text))
               (distance
                (@ event feature attributes distance)))
           (setf (@ *aux-point-distance-select* options selected-index)
                 (@ event feature fid))
           (setf (inner-html-with-id "aux-numeric-list")
                 (html-ordered-list aux-numeric))
           (setf (inner-html-with-id "aux-text-list")
                 (html-ordered-list aux-text))))

       (defun bye ()
         "Store user's current map extent and log out."
         (let* ((bbox (chain *streetmap*
                             (get-extent)
                             (transform +spherical-mercator+ +geographic+)
                             (to-b-b-o-x)))
                (href (+ "/phoros/lib/logout?bbox=" bbox)))
           (when (@ *streetmap* cursor-layer features length)
             (let* ((lonlat-geographic (chain *streetmap*
                                              cursor-layer
                                              features
                                              0
                                              geometry 
                                              (clone)
                                              (transform +spherical-mercator+
                                                         +geographic+))))
               (setf href (+ href
                             "&longitude=" (@ lonlat-geographic x)
                             "&latitude=" (@ lonlat-geographic y)))))
           (setf (@ location href) href)))

       (defun init ()
         "Prepare user's playground."
         (unless +presentation-project-bbox-text+
           (setf (inner-html-with-id "presentation-project-emptiness")
                 "(no data)"))
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
         (unless +aux-data-p+
           (disable-element-with-id "walk-p")
           (hide-element-with-id "decrease-step-size")
           (hide-element-with-id "step-size")
           (hide-element-with-id "increase-step-size")
           (hide-element-with-id "step-button"))
         (when (write-permission-p)
           (enable-element-with-id "point-attribute-input")
           (enable-element-with-id "point-attribute-select")
           (enable-element-with-id "point-description-input")
           (enable-element-with-id "point-description-select")
           (enable-element-with-id "point-numeric-description")
           (request-user-point-choice true))
         (setf (inner-html-with-id "h2-controls") "Create Point")
         (hide-element-with-id "multiple-points-phoros-controls")
         (setf *point-attributes-select*
               (chain document (get-element-by-id "point-attribute-select")))
         (setf *aux-point-distance-select*
               (chain document (get-element-by-id "aux-point-distance")))
         (hide-aux-data-choice)
         (let ((cursor-layer-style
                (create
                 graphic-width 14
                 external-graphic "/phoros/lib/public_html/phoros-cursor.png")))
           (setf (@ *streetmap* cursor-layer)
                 (new (chain
                       *open-layers *layer
                       (*vector
                        "you"
                        (create
                         style cursor-layer-style)))))
           (setf (@ *streetmap* overview-cursor-layer)
                 (new (chain
                       *open-layers *layer
                       (*vector
                        "you"
                        (create
                         style cursor-layer-style))))))
         (let ((survey-layer-style
                (create stroke-color (chain *open-layers *feature *vector
                                            style "default" stroke-color)
                        stroke-width 1
                        point-radius 2
                        fill-opacity 0
                        graphic-name "circle")))
           (setf (@ *streetmap* survey-layer)
                 (new (chain
                       *open-layers *layer
                       (*vector
                        "survey"
                        (create
                         strategies (array (new (*bbox-strategy*)))
                         protocol
                         (new (*http-protocol*
                               (create :url "/phoros/lib/points.json")))
                         style survey-layer-style))))))
         (setf (@ *streetmap* user-point-layer)
               (new (chain
                     *open-layers *layer
                     (*vector
                      "user points"
                      (create
                       strategies (array (new *bbox-strategy*))
                       protocol
                       (new (*http-protocol*
                             (create :url "/phoros/lib/user-points.json")))
                       style-map (user-point-style-map nil))))))
         (setf (@ *streetmap* user-points-hover-control)
               (new (chain *open-layers
                           *control
                           (*select-feature (@ *streetmap* user-point-layer)
                                            (create render-intent "temporary"
                                                    hover t
                                                    highlight-only t)))))
         (setf (@ *streetmap* user-points-select-control)
               (new (chain *open-layers
                           *control
                           (*select-feature (@ *streetmap* user-point-layer)
                                            (create toggle t
                                                    multiple t)))))
         (let ((aux-layer-style
                (create stroke-color "grey"
                        stroke-width 1
                        point-radius 2
                        fill-opacity 0
                        graphic-name "circle")))
           (setf (@ *streetmap* aux-point-layer)
                 (new (chain
                       *open-layers *layer
                       (*vector
                        "auxiliary data"
                        (create
                         strategies (array (new (*bbox-strategy*)))
                         protocol
                         (new (*http-protocol*
                               (create :url "/phoros/lib/aux-points.json")))
                         style aux-layer-style
                         visibility nil))))))
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
           (setf (@ *streetmap* nearest-aux-points-layer)
                 (new (chain *open-layers
                             *layer
                             (*vector
                              "Nearest Aux Points"
                              (create
                               display-in-layer-switcher nil
                               style-map nearest-aux-point-layer-style-map
                               visibility t))))))
         (setf (@ *streetmap* nearest-aux-points-hover-control)
               (new (chain *open-layers
                           *control
                           (*select-feature
                            (@ *streetmap* nearest-aux-points-layer)
                            (create render-intent "temporary"
                                    hover t
                                    highlight-only t)))))
         (setf (@ *streetmap* nearest-aux-points-select-control)
               (new (chain *open-layers
                           *control
                           (*select-feature
                            (@ *streetmap* nearest-aux-points-layer)))))
         (setf (@ *streetmap* aux-data-linestring-layer)
               (new (chain *open-layers
                           *layer
                           (*vector
                            "Aux Data Linestring"
                            (create
                             display-in-layer-switcher nil
                             style-map nearest-aux-point-layer-style-map
                             visibility t)))))
         (setf (@ *streetmap* google-streetmap-layer) 
               (new (chain *open-layers
                           *layer
                           (*google "Google Streets"
                                    (create num-zoom-levels 23)))))
         (setf (@ *streetmap* osm-layer)
               (new (chain *open-layers
                           *layer
                           (*osm*
                            "OpenStreetMap"
                            nil
                            (create num-zoom-levels 23
                                    attribution
                                    "Data CC-By-SA by openstreetmap.org")))))
         (setf (@ *streetmap* overview-osm-layer)
               (new (chain *open-layers
                           *layer
                           (*osm* "OpenStreetMap"))))
         (setf (@ *streetmap* click-streetmap)
               (new (*click-control*
                     (create :trigger request-photos-after-click))))
         (setf (@ *streetmap* nirvana-layer)
               (new (chain
                     *open-layers
                     (*layer
                      "Nirvana"
                      (create is-base-layer t
                              projection (@ *streetmap* osm-layer projection)
                              max-extent (@ *streetmap* osm-layer max-extent)
                              max-resolution (@ *streetmap*
                                                osm-layer
                                                max-resolution)
                              units (@ *streetmap* osm-layer units)
                              num-zoom-levels (@ *streetmap*
                                                 osm-layer
                                                 num-zoom-levels))))))
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

                              layers (array
                                      (@ *streetmap* overview-osm-layer)
                                      (@ *streetmap* overview-cursor-layer))

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
                  (add-control (@ *streetmap* click-streetmap)))
           (chain *streetmap* click-streetmap (activate))

           (chain *streetmap*
                  user-point-layer
                  events
                  (register "featureselected"
                            (@ *streetmap* user-point-layer)
                            user-point-selected))
           (chain *streetmap*
                  user-point-layer
                  events
                  (register "featureunselected"
                            (@ *streetmap* user-point-layer)
                            user-point-unselected))
           (chain *streetmap*
                  nearest-aux-points-layer
                  events
                  (register "featureselected"
                            (@ *streetmap* nearest-aux-points-layer)
                            nearest-aux-point-selected))
           (chain *streetmap*
                  (add-control
                   (@ *streetmap* nearest-aux-points-hover-control)))
           (chain *streetmap*
                  (add-control
                   (@ *streetmap* nearest-aux-points-select-control)))
           (chain *streetmap*
                  (add-control
                   (@ *streetmap* user-points-hover-control)))
           (chain *streetmap*
                  (add-control
                   (@ *streetmap* user-points-select-control)))
           (chain *streetmap* user-points-hover-control (activate))
           (chain *streetmap* user-points-select-control (activate))
           (chain *streetmap* nearest-aux-points-hover-control (activate))
           (chain *streetmap* nearest-aux-points-select-control (activate))
           (chain *streetmap* (add-layer (@ *streetmap* osm-layer)))
           (try (chain *streetmap*
                       (add-layer (@ *streetmap* google-streetmap-layer)))
                (:catch (c)
                  (chain *streetmap*
                         (remove-layer (@ *streetmap*
                                          google-streetmap-layer)))))
           (chain *streetmap* (add-layer (@ *streetmap* nirvana-layer)))
           (chain *streetmap*
                  (add-layer (@ *streetmap* nearest-aux-points-layer)))
           (chain *streetmap* (add-layer (@ *streetmap* survey-layer)))
           (chain *streetmap*
                  (add-layer (@ *streetmap* cursor-layer)))
           (chain *streetmap*
                  (add-layer (@ *streetmap* aux-point-layer)))
           (chain *streetmap*
                  (add-layer (@ *streetmap* aux-data-linestring-layer)))
           (chain *streetmap*
                  (add-layer (@ *streetmap* user-point-layer)))
           (setf (@ overview-map element)
                 (chain document (get-element-by-id
                                  "streetmap-overview-element")))
           (chain *streetmap* (add-control overview-map))
           (chain *streetmap* (add-control mouse-position-control))
           (chain *streetmap* (add-control scale-line-control)))
         (loop
            for i from 0 to (lisp (1- *number-of-images*))
            do (initialize-image i))
         (add-help-events)
         (chain *streetmap*
                (zoom-to-extent
                 (if (lisp (stored-bbox))
                     (new (chain *open-layers
                             *bounds
                             (from-string (lisp (stored-bbox)))
                             (transform +geographic+ +spherical-mercator+)))
                     +presentation-project-bounds+)))
           (let ((stored-cursor (lisp (stored-cursor))))
             (when stored-cursor
               (request-photos
                (new (chain *open-layers
                            *lon-lat
                            (from-string stored-cursor)
                            (transform +geographic+
                                       +spherical-mercator+)))))))))))

(pushnew (hunchentoot:create-regex-dispatcher
          (format nil "/phoros/lib/phoros-~A-\\S*-\\S*\.js"
                  (phoros-version))
          'phoros.js)
         hunchentoot:*dispatch-table*)
