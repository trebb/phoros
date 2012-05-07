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
           \"Write\" may write user points and edit/delete their own
           ones (and ownerless points).  \"Admin\" may write user
           points and edit/delete points written by anyone."))
          :presentation-project-name
          (who-ps-html
           (:p "Presentation project name."))
          :presentation-project-emptiness
          (who-ps-html
           (:p "This presentation project is empty.  You can't do much
           with it."))
          :recommend-fresh-login
          (who-ps-html
           (:p "Sorry, but you are no longer authenticated.  Your
           session may have expired due to prolonged inactivity, or an
           administrator has kicked you out by restarting the server.")
           (:p "Please repeat the login process."))
          :caching-indicator
          (who-ps-html
           (:p "Caching images.")
           (:p "As I'm currently idle, I'm preemptively putting images
           into your browser's cache which later on may help speed up
           things a bit.")
           (:p "Type")
           (:code "about:cache?device=disk")
           (:p "into your address bar to see what's going on there.")
           (:p "Your browser cache size should be set to 2000 GB or
           bigger. Bigger is better."))
          :phoros-version
          (who-ps-html
           (:p "Phoros version.")
           (:p "In a version string A.B.C, changes in A denote
           incompatible changes in data (you can't access a database
           set up by a different version of Phoros); changes in B mean
           user-visible changes in feature set; changes in C denote
           bug fixes and minor improvements."))
          :h2-phoros-controls
          (who-ps-html
           (:p "Current action."))
          :multiple-points-viewer
          (who-ps-html
           (:p "Try reading the text under mouse pointer."))
          :display-aux-data-dismiss-button
          (who-ps-html
           (:p "Leave display of auxiliary data."))
          :display-aux-data-button
          (who-ps-html
           (:p "Cancel current action and display auxiliary data from
           an area around map cursor."))
          :delete-point-button
          (who-ps-html
           (:p "Delete current point."))
          :finish-point-button
          (who-ps-html
           (:p "Store user point with its attributes kind,
           numeric-description and description, and with its auxiliary
           data into database; warn if the given set of attributes
           isn't unique."))
          :suggest-unique-button
          (who-ps-html
           (:h3 "Non-unique set of user point attributes")
           (:p "Recommend a set of user point attributes that is
           unique among the currently defined user points, preferably
           by incrementing a portion of attribute numeric-description
           that looks like a number."))
          :force-duplicate-button
          (who-ps-html
           (:h3 "Non-unique set of user point attributes")
           (:p "Store user point with its attributes kind,
           numeric-description and description, and with its auxiliary
           data into database; don't care whether the given set of
           attributes is unique."))
          :download-user-points-button
          (who-ps-html
           (:p "Download all user points as GeoJSON-fomatted text
           file.  Do this regularly if you don't want to lose your
           work due to server crashes or major Phoros updates.")
           (:p "Points saved this way can be fed back into your
           project using the command line interface (on server or on
           any other host where the database is reachable)."))
          :point-kind
          (who-ps-html
           (:h3 "\"kind\"")
           (:p "The standard ones, polygon, polyline, and solitary are
           rendered as asterisk, square, and triangle respectively.
           Anything else is rendered as an X."))
          :point-description
          (who-ps-html
           (:h3 "\"description\"")
           (:p "Optional textual description of the set of user points
           the current point belongs to."))
          :point-numeric-description
          (who-ps-html
           (:h3 "\"numeric-description\"")
           (:p "Optional description of the current user point.  It is
           occasionally used to label representations of this point in
           streetmap and in images.")
           (:p "It should contain a numeric part, possibly with
           leading zeros, which will be incremented automatically to
           make the attribute sets of points with otherwise identical
           attributes unique."))
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
           (:p "Select a set of auxiliary data by its distance (in
           metres) from the current estimated position if any, or its
           distance from streetmap cursor otherwise.")
           (:p "Alternatively, a set of auxiliary data is also
           selectable by clicking its representation in streetmap."))
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
          :no-footprints-p
          (who-ps-html
           (:p "I haven't been able to display a set of images that
           cover a common area because I couldn't find the necessary
           information.  As a fallback, I'm displaying a set of images
           with points of view close to the point you selected.")
           (:p "The server is probably trying to remedy this problem
           but this may take some time."))
          :auto-zoom
          (who-ps-html
           (:h3 "Auto Zoom")
           (:p "Check this to automatically zoom into images once they
           get an estimated position."))
          :brighten-images
          (who-ps-html
           (:p "Check this to have underexposed images brightened up.")
           (:p "Brightening starts with the next set of images and may
           slow things down a bit."))
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
          :image-usable
          (who-ps-html
           (:p "No photogrammetric survey possible as there isn't any
           usable calibration data available for this image.")
           (:p "This means no image footprints can be calculated
           either which prevents me from selecting images covering a
           common area."))
          :image-trigger-time
          (who-ps-html
           (:p "Time this image was taken."))
          base-layers-div
          (who-ps-html
           (:p "Choose a background streetmap."))
          data-layers-div
          (who-ps-html
           (:p "Toggle visibility of data layers."))
          :unselect-all-restrictions-button
          (who-ps-html
           (:h3 "Image Restrictions")
           (:p "Remove all image restrictions."))
          :restriction-select
          (who-ps-html
           (:h3 "Image Restrictions")
           (:p "Select one ore more of the restrictions in order to
           consider only a subset of the images available.  No
           selection at all means no restriction.")
           (:p "Shift-click selects a range of restrictions,
           control-click selects or unselects a particular
           restriction, click selects a restriction unselecting
           anything else."))
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

       (defvar +proxy-root+
         (lisp *proxy-root*)
         "First element of URL path; defaults to phoros but may be
         turned into something different by an HTTP proxy
         definition.")

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

       (defvar +aux-numeric-labels+
         (lisp (when *aux-numeric-labels*
                 (coerce *aux-numeric-labels* 'vector))))

       (defvar +aux-text-labels+
         (lisp (when *aux-text-labels*
                 (coerce *aux-text-labels* 'vector))))

       (defvar *images* (array) "Collection of the photos currently shown.")

       (defvar *streetmap* undefined
         "The streetmap shown to the user.")

       ;; (defvar *point-attributes-select* undefined
       ;;   "The HTML element for selecting user point attributes.")

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

       (defvar *geojson-parser* (new (chain *open-layers *format *geo-j-s-o-n)))

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
                  (or (equal +user-name+ current-owner)
                      (not current-owner)))))

       (defun *image ()
         "Anything necessary to deal with a photo."
         (setf (@ this map)
               (new
                (chain
                 *open-layers
                 (*map
                  (create projection +spherical-mercator+
                          all-overlays t
                          controls (array (new (chain *open-layers
                                                      *control
                                                      (*navigation)))))))))
         (setf (@ this dummy) false) ;TODO why? (omitting splices map components directly into *image)
         )

       (setf (@ *image prototype delete-photo)
        delete-photo)
       (setf (@ *image prototype photop)
        photop)
       (setf (@ *image prototype show-photo)
        show-photo)
       (setf (@ *image prototype draw-epipolar-line)
        draw-epipolar-line)
       (setf (@ *image prototype draw-active-point)
        draw-active-point)
       (setf (@ *image prototype draw-estimated-positions)
        draw-estimated-positions)

       (defun photo-path (photo-parameters)
         "Create from stuff found in photo-parameters and in checkbox
         brighten-images-p a path with parameters for use in an image
         url."
         (+ "/" +proxy-root+
            "/lib/photo/"
            (@ photo-parameters directory) "/"
            (@ photo-parameters filename) "/"
            (@ photo-parameters byte-position) ".png"
            "?mounting-angle=" (@ photo-parameters mounting-angle)
            "&bayer-pattern=" (@ photo-parameters bayer-pattern)
            "&color-raiser=" (@ photo-parameters color-raiser)
            (if (checkbox-status-with-id "brighten-images-p")
                "&brightenp"
                "")))

       (defun has-layer-p (map layer-name)
         "False if map doesn't have a layer called layer-name."
         (chain map (get-layers-by-name layer-name) length))

       (defun some-active-point-p ()
         "False if no image in *images* has an Active Point."
         (loop
            for i across *images*
            sum (has-layer-p (@ i map) "Active Point")))

       (defun remove-layer (map layer-name)
         "Destroy layer layer-name in map."
         (when (has-layer-p map layer-name)
           (chain map (get-layers-by-name layer-name) 0 (destroy))))

       (defun remove-any-layers (layer-name)
         "Destroy in all *images* and in *streetmap* the layer named layer-name."
         (loop
            for i across *images* do
            (remove-layer (@ i map) layer-name))
         (remove-layer *streetmap* layer-name))

       (defun reset-controls ()
         (disable-element-with-id "finish-point-button")
         (disable-element-with-id "delete-point-button")
         (disable-element-with-id "remove-work-layers-button")
         (hide-elements-of-class "multiple-points-viewer")
         (hide-elements-of-class "point-editor")
         (hide-elements-of-class "point-viewer")
         (hide-elements-of-class "aux-data-viewer")
         (reveal-elements-of-class "point-creator")
         (setf (inner-html-with-id "creator") nil)
         (setf (inner-html-with-id "point-creation-date") nil)
         (hide-aux-data-choice)
         (setf (inner-html-with-id "aux-numeric-list") nil)
         (setf (inner-html-with-id "aux-text-list") nil))

       (defun disable-streetmap-nearest-aux-points-layer ()
         "Get (@ *streetmap* nearest-aux-points-layer) out of the way,
         I.e., remove features and disable feature select control so
         it won't shadow any other control."
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
         "Activate HTML element with id=\"id\".  Return t if element
         was greyed out before."
         (prog1
             (chain document (get-element-by-id id) disabled)
           (setf (chain document (get-element-by-id id) disabled) nil)))

       (defun disable-element-with-id (id)
         "Grey out HTML element with id=\"id\".  Return t if element
         was active before."
         (prog1
             (not (chain document (get-element-by-id id) disabled))
           (setf (chain document (get-element-by-id id) disabled) t)))

       (defun hide-element-with-id (id)
         "Hide HTML element with id=\"id\"."
         (setf (chain document (get-element-by-id id) style display)
               "none"))

       (defun hide-elements-of-class (class-name)
         "Hide HTML elements with class=\"class\"."
         (loop
            for element in (chain document
                                  (get-elements-by-class-name class-name))
            do (setf (@ element style display) "none")))

       (defun reveal-element-with-id (id)
         "Reveal HTML element with id=\"id\"."
         (setf (chain document (get-element-by-id id) style display)
               ""))

       (defun reveal-elements-of-class (class-name)
         "Reveal HTML elements with class=\"class\"."
         (loop
            for element in (chain document
                                  (get-elements-by-class-name class-name))
            do (setf (@ element style display) "")))

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
              for i across *images*
              do (chain i (delete-photo)))
           (if (@ photo-parameters 0 footprintp)
               (hide-element-with-id "no-footprints-p")
               (reveal-element-with-id "no-footprints-p"))
           (loop
              for p across photo-parameters
              for i across *images*
              do
              (setf (@ i photo-parameters) p)
              (chain i (show-photo)))))

       (defun recommend-fresh-login ()
         "Notify user about invalid authentication."
         (setf (inner-html-with-id "recommend-fresh-login")
               "(not authenticated)")
         (disable-element-with-id "download-user-points-button")
         (disable-element-with-id "blurb-button")
         (hide-element-with-id "phoros-controls")
         (hide-element-with-id "images"))

       (defun consolidate-combobox (combobox-id)
         "Help faking a combobox: copy selected option into input."
         (let* ((combobox-select (+ combobox-id "-select"))
                (combobox-input (+ combobox-id "-input"))
                (combobox-selected-index
                 (chain document
                        (get-element-by-id combobox-select)
                        selected-index)))
           (when (< -1 combobox-selected-index)
             (setf (value-with-id combobox-input)
                   (getprop (chain document
                                   (get-element-by-id combobox-select)
                                   options)
                            combobox-selected-index
                            'value)))
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
          
       (defun stuff-combobox (combobox-id values &optional (selection -1))
         "Stuff combobox with values.  If selection is a non-negative
         integer, select the respective item."
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
           (setf (chain document
                        (get-element-by-id combobox-select)
                        selected-index)
                 selection)
           (consolidate-combobox combobox-id)))

       (defun stuff-user-point-comboboxes (&optional selectp)
         "Stuff user point attribute comboboxes with sensible values.
         If selectp it t, select the most frequently used one."
         (let* ((response
                 (chain *json-parser*
                        (read (@ *streetmap*
                                 user-point-choice-response response-text))))
                (kinds
                 (chain response kinds (map (lambda (x)
                                              (@ x kind)))))
                (descriptions
                 (chain response descriptions (map (lambda (x)
                                                     (@ x description)))))
                (best-used-kind -1)
                (best-used-description -1))
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
                for i across (@ response kinds)
                for k from 0
                do (when (< maximum (@ i count))
                     (setf maximum (@ i count))
                     (setf best-used-kind k))))
           (stuff-combobox
            "point-kind" kinds best-used-kind)
           (stuff-combobox
            "point-description" descriptions best-used-description)))

       (defun request-user-point-choice (&optional selectp)
         "Stuff user point attribute comboboxes with sensible values.
         If selectp it t, select the most frequently used one."
         (setf (@ *streetmap* user-point-choice-response)
               (chain
                *open-layers
                *request
                (*post*
                 (create :url (+ "/" +proxy-root+
                                 "/lib/user-point-attributes.json")
                         :data nil
                         :headers (create "Content-type" "text/plain")
                         :success (lambda ()
                                    (stuff-user-point-comboboxes selectp))
                         :failure recommend-fresh-login)))))
           
       (defun stuff-restriction-select ()
         "Stuff available restriction IDs into restriction-select."
         (let ((response
                (chain *json-parser*
                       (read (@ *streetmap*
                                restriction-select-choice-response
                                response-text))))
               (restriction-select-options
                (chain document
                       (get-element-by-id "restriction-select")
                       options)))
           (loop
              for restriction in response
              for i from 0
              do (setf (elt restriction-select-options i)
                       (new (chain (*option restriction)))))))

       (defun request-restriction-select-choice ()
         "Stuff available restriction IDs into restriction-select."
         (setf (@ *streetmap* restriction-select-choice-response)
               (chain
                *open-layers
                *request
                (*post*
                 (create :url (+ "/" +proxy-root+
                                 "/lib/selectable-restrictions.json")
                         :data nil
                         :headers (create "Content-type" "text/plain")
                         :success stuff-restriction-select
                         :failure recommend-fresh-login)))))

       (defun selected-restrictions ()
         "Return list of restriction IDs selected by user."
         (let ((restriction-select-options
                (chain document
                       (get-element-by-id "restriction-select")
                       options)))
           (loop
              for restriction in restriction-select-options
              when (@ restriction selected)
              collect (@ restriction text))))

       (defun unselect-all-restrictions ()
         "Clear any selected restrictions."
         (loop
            for option across (chain document
                                     (get-element-by-id "restriction-select")
                                     options)
            do (setf (@ option selected) f))
         (request-photos))

       (defun request-photos-after-click (event)
         "Handle the response to a click into *streetmap*; fetch photo
         data.  Set or update streetmap cursor."
         (request-photos (chain *streetmap*
                                (get-lon-lat-from-pixel (@ event xy)))))

       (defun request-photos (&optional lonlat)
         "Fetch photo data for a point near lonlat.  Set or update
         streetmap cursor."
         (when lonlat
             (setf (@ *streetmap* clicked-lonlat) lonlat))
         (if (checkbox-status-with-id "walk-p")
             (request-aux-data-linestring-for-point
              (@ *streetmap* clicked-lonlat))
             (request-photos-for-point (@ *streetmap* clicked-lonlat)))
         (request-cache-fodder (@ *streetmap* clicked-lonlat)))

       (defun request-aux-data-linestring-for-point (lonlat-spherical-mercator)
         "Fetch a linestring along auxiliary points near
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
         (hide-elements-of-class "aux-data-viewer")
         (reveal-elements-of-class "point-creator")
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
                                 :zoom (chain *streetmap* (get-zoom))
                                 :count (lisp *number-of-images*)
                                 :selected-restriction-ids
                                 (selected-restrictions))))))
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
                 (chain
                  *open-layers
                  *request
                  (*post*
                   (create
                    :url (+ "/" +proxy-root+ "/lib/nearest-image-data")
                    :data content
                    :headers (create "Content-type" "text/plain"
                                     "Content-length" (@ content length))
                    :success present-photos
                    :failure recommend-fresh-login))))))

       (defvar *cache-stuffer*
         (create xhr undefined          ;instance of XMLHttpRequest
                 cache-fodder-request-response undefined
                 photo-url-ingredients undefined
                 index undefined        ;current element of
                                        ; photo-url-ingredients
                 caching-photo-p nil
                 cache-size (* 2084000 1024)
                                        ;we assume cache-size is set
                                        ; to 2000MB by browser user
                 average-image-size undefined
                 current-center undefined
                 cache-photo-timeout undefined
                 request-cache-fodder-group-timeout undefined)
         "Things used to preemptively stuff the browser cache.")

       (defun request-cache-fodder (lonlat-spherical-mercator)
         "Abort any previous cache stuffing activities, wait a few
         seconds, and start a new cache stuffing session centered at
         lonlat-spherical-mercator."
         (setf (@ *cache-stuffer* current-center)
               (chain lonlat-spherical-mercator
                      (clone)
                      (transform +spherical-mercator+ +geographic+)))
         (setf (@ *cache-stuffer* average-image-size) 0)
         (clear-timeout (@ *cache-stuffer* cache-photo-timeout))
         (clear-timeout (@ *cache-stuffer* request-cache-fodder-group-timeout))
         (hide-element-with-id "caching-indicator")
         (setf (@ *cache-stuffer* request-cache-fodder-group-timeout)
               (set-timeout request-cache-fodder-group 15000)))

       (defun request-cache-fodder-group ()
         "Request a bunch of image url ingredients, initiate caching
         of the respective images.  Keep trying if unsuccessful."
         (let ((content
                (chain *json-parser*
                       (write
                        (create
                         :longitude (@ *cache-stuffer* current-center lon)
                         :latitude (@ *cache-stuffer* current-center lat))))))
           (setf (@ *cache-stuffer* cache-fodder-request-response)
                 (chain
                  *open-layers
                  *request
                  (*post*
                   (create
                    :url (+ "/" +proxy-root+ "/lib/nearest-image-urls")
                    :data content
                    :headers (create "Content-type" "text/plain"
                                     "Content-length" (@ content length))
                    :success handle-request-cache-fodder-group
                    :failure (lambda ()
                               (if (= (@ *cache-stuffer* cache-fodder-request-response status) 504)
                                   (progn
                                     (clear-timeout
                                      (@ *cache-stuffer*
                                         request-cache-fodder-group-timeout))
                                     (setf (@ *cache-stuffer*
                                              request-cache-fodder-group-timeout)
                                           (set-timeout request-cache-fodder-group
                                                        5000)))
                                   (recommend-fresh-login)))))))))

       (defun handle-request-cache-fodder-group ()
         "Handle the response triggered by request-cache-fodder-group."
         (setf (@ *cache-stuffer* photo-url-ingredients)
               (chain *json-parser*
                      (read (@ *cache-stuffer*
                               cache-fodder-request-response
                               response-text))))
         (setf (@ *cache-stuffer* index) 0)
         (reveal-element-with-id "caching-indicator")
         (cache-photo))

       (defun cache-photo ()
         "Cache another image if the previous one is done."
         (if (and (< (@ *cache-stuffer* index)
                     (length (@ *cache-stuffer* photo-url-ingredients)))
                  (< (* (@ *cache-stuffer* index)
                        (@ *cache-stuffer* average-image-size))
                     (* .5 (@ *cache-stuffer* cache-size))))
             (if (@ *cache-stuffer* caching-photo-p)
                 (progn
                   (clear-timeout (@ *cache-stuffer* cache-photo-timeout))
                   (setf (@ *cache-stuffer* cache-photo-timeout)
                         (set-timeout cache-photo 3000)))
                 (progn
                   (setf (@ *cache-stuffer* caching-photo-p) t)
                   (setf (@ *cache-stuffer* xhr) (new (*x-m-l-http-request)))
                   (chain *cache-stuffer*
                          xhr
                          (open "GET"
                                (photo-path
                                 (aref (@ *cache-stuffer* photo-url-ingredients)
                                       (@ *cache-stuffer* index)))
                                t))
                   (setf (@ *cache-stuffer* xhr onload)
                         (lambda (event)
                           (setf (@ *cache-stuffer* average-image-size)
                                 (/ (+ (* (@ *cache-stuffer* average-image-size)
                                          (@ *cache-stuffer* index))
                                       (@ event total)) ;bytes received
                                    (1+ (@ *cache-stuffer* index))))
                           (setf (@ *cache-stuffer* caching-photo-p) nil)
                           (incf (@ *cache-stuffer* index))))
                   ;; We do our best to have the browser use its cache.
                   ;; Note however that in certain cases use of the
                   ;; cache may be hampered by pressing the browser's
                   ;; reload button.
                   (chain *cache-stuffer*
                          xhr
                          (set-request-header
                           "Cache-control"
                           (+ "max-age=" (lisp *browser-cache-max-age*))))
                   (chain *cache-stuffer* xhr (send))
                   (clear-timeout (@ *cache-stuffer* cache-photo-timeout))
                   (setf (@ *cache-stuffer* cache-photo-timeout)
                         (set-timeout
                          cache-photo     ;come back quickly in case
                          500))))         ; photo is already in cache
             (hide-element-with-id "caching-indicator")))

       (defun draw-epipolar-line ()
         "Draw an epipolar line from response triggered by clicking
         into a (first) photo."
         (disable-streetmap-nearest-aux-points-layer)
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

       (defun request-aux-points-near-cursor (count)
         "Draw into streetmap the count nearest points of auxiliary
         data around streetmap cursor."
         (reset-layers-and-controls)
         (hide-elements-of-class "point-creator")
         (hide-elements-of-class "point-editor")
         (hide-elements-of-class "point-viewer")
         (reveal-elements-of-class "aux-data-viewer")
         (let ((lonlat-geographic
                (chain (@ *streetmap* clicked-lonlat)
                       (clone)
                       (transform +spherical-mercator+ +geographic+))))
           (request-nearest-aux-points (create :longitude (@ lonlat-geographic lon)
                                               :latitude (@ lonlat-geographic lat))
                                       count)))

       (defun dismiss-aux-data ()
         "Dismiss the display of aux data near cursor."
         (reset-layers-and-controls))

       (defun request-nearest-aux-points (global-position count)
         "Draw into streetmap the count nearest points of auxiliary
         data around global-position."
         (let ((global-position-etc global-position)
               content)
           (setf (@ global-position-etc count) count)
           (setf content (chain *json-parser*
                                (write global-position-etc)))
           (setf (@ *streetmap* aux-local-data-request-response)
                 (chain *open-layers
                        *request
                        (*post*
                         (create :url (+ "/" +proxy-root+
                                         "/lib/aux-local-data")
                                 :data content
                                 :headers (create "Content-type" "text/plain"
                                                  "Content-length"
                                                  (@ content length))
                                 :success draw-nearest-aux-points
                                 :failure recommend-fresh-login))))))

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
                 (chain *open-layers
                        *request
                        (*post*
                         (create :url (+ "/" +proxy-root+
                                         "/lib/aux-local-linestring.json")
                                 :data content
                                 :headers (create "Content-type" "text/plain"
                                                  "Content-length"
                                                  (@ content length))
                                 :success draw-aux-data-linestring
                                 :failure recommend-fresh-login))))))

       (defun draw-estimated-positions ()
         "Draw into streetmap and into all images points at Estimated
         Position.  Estimated Position is the point returned so far
         from photogrammetric calculations that are triggered by
         clicking into another photo.  Also draw into streetmap the
         nearest auxiliary points to Estimated Position."
         (when (write-permission-p)
           (setf (chain document
                        (get-element-by-id "finish-point-button")
                        onclick)
                 (lambda () (finish-point #'store-point)))
           (enable-element-with-id "finish-point-button"))
         (let* ((estimated-positions-request-response
                 (chain *json-parser*
                        (read
                         (@ this
                            estimated-positions-request-response
                            response-text))))
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
                  (new
                   (chain *open-layers
                          *feature
                          (*vector
                           (chain
                            (new (chain *open-layers
                                        *geometry
                                        (*point
                                         (@ *global-position* longitude)
                                         (@ *global-position* latitude))))
                            (transform +geographic+ +spherical-mercator+)))))))
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
           (hide-elements-of-class "aux-data-viewer")
           (reveal-elements-of-class "point-creator")
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
                      (new (chain *open-layers (*lon-lat (@ p m)
                                                         (@ p n)))))
                (setf (@ i estimated-position-layer style)
                      estimated-position-style)
                (let* ((point
                        (new
                         (chain *open-layers *geometry (*point (@ p m)
                                                               (@ p n)))))
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
         (let ((features
                (chain *json-parser*
                       (read
                        (@ *streetmap*
                           aux-local-data-request-response
                           response-text))
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
         "Return inner-html of element step-size (metres) converted
         into map units (degrees).  You should be close to the
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
         \"kind\" and features are labelled after feature
         property label-property."
         (let* ((symbolizer-property "kind")
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
                         (@ *user-point-in-images-response* response-text))))
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

       (defun finish-point (database-writer)
         "Try, with some user interaction, to uniquify user-point
          attributes and call database-writer."
         (let* ((point-data
                 (create user-point-id (if (defined *current-user-point*)
                                           (@ *current-user-point* fid)
                                           nil)
                         kind
                         (value-with-id "point-kind-input")
                         description
                         (value-with-id "point-description-input")
                         numeric-description
                         (value-with-id "point-numeric-description")))
                (content 
                 (chain *json-parser*
                        (write point-data)))
                (delete-point-button-active-p
                 (disable-element-with-id "delete-point-button")))
           (disable-element-with-id "finish-point-button")
           (setf *uniquify-point-attributes-response* nil)
           (setf *uniquify-point-attributes-response*
                 (chain
                  *open-layers
                  *request
                  (*post*
                   (create
                    :url (+ "/" +proxy-root+ "/lib/uniquify-point-attributes")
                    :data content
                    :headers (create "Content-type" "text/plain"
                                     "Content-length" (@ content
                                                         length))
                    :success
                    (lambda ()
                      (enable-element-with-id "finish-point-button")
                      (when delete-point-button-active-p
                        (enable-element-with-id "delete-point-button"))
                      (let ((response
                             (chain
                              *json-parser*
                              (read
                               (@ *uniquify-point-attributes-response*
                                  response-text)))))
                        (if (equal null response)
                            (database-writer)
                            (progn
                              (setf
                               (chain document
                                      (get-element-by-id
                                       "force-duplicate-button")
                                      onclick)
                               (lambda ()
                                 (hide-element-with-id "uniquify-buttons")
                                 (reveal-element-with-id "finish-point-button")
                                 (database-writer)))
                              (hide-element-with-id "finish-point-button")
                              (reveal-element-with-id "uniquify-buttons")))))
                    :failure recommend-fresh-login))))))

       (defun insert-unique-suggestion ()
         "Insert previously received set of unique user-point
         attributes into their respective input elements; switch
         buttons accordingly."
         (let* ((point-data
                 (create user-point-id (if (defined *current-user-point*)
                                           (@ *current-user-point* fid)
                                           nil)
                         kind
                         (value-with-id "point-kind-input")
                         description
                         (value-with-id "point-description-input")
                         numeric-description
                         (value-with-id "point-numeric-description")))
                (content 
                 (chain *json-parser*
                        (write point-data)))
                (delete-point-button-active-p
                 (disable-element-with-id "delete-point-button")))
           (disable-element-with-id "finish-point-button")
           (hide-element-with-id "uniquify-buttons")
           (reveal-element-with-id "finish-point-button")
           (setf *uniquify-point-attributes-response* nil)
           (setf *uniquify-point-attributes-response*
                 (chain
                  *open-layers
                  *request
                  (*post*
                   (create :url (+ "/"
                                   +proxy-root+
                                   "/lib/uniquify-point-attributes")
                           :data content
                           :headers (create "Content-type" "text/plain"
                                            "Content-length" (@ content
                                                                length))
                           :success
                           (lambda ()
                             (enable-element-with-id "finish-point-button")
                             (when delete-point-button-active-p
                               (enable-element-with-id "delete-point-button"))
                             (let ((response
                                    (chain
                                     *json-parser*
                                     (read
                                      (@ *uniquify-point-attributes-response*
                                         response-text)))))
                               (unless (equal null response)
                                 (setf (value-with-id
                                        "point-numeric-description")
                                       (@ response numeric-description)))))
                           :failure recommend-fresh-login))))))

       (defun store-point ()
         "Send freshly created user point to the database."
         (let ((global-position-etc *global-position*))
           (setf (@ global-position-etc kind)
                 (value-with-id "point-kind-input"))
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
             (disable-element-with-id "finish-point-button")
             (chain
              *open-layers
              *request
              (*post*
               (create :url (+ "/" +proxy-root+ "/lib/store-point")
                       :data content
                       :headers (create "Content-type" "text/plain"
                                        "Content-length" (@ content length))
                       :success (lambda ()
                                  (refresh-layer
                                   (@ *streetmap* user-point-layer))
                                  (reset-layers-and-controls)
                                  (request-user-point-choice))
                       :failure recommend-fresh-login))))))
           
       (defun update-point ()
         "Send changes to currently selected user point to database."
         (let* ((point-data
                 (create user-point-id (@ *current-user-point* fid)
                         kind
                         (value-with-id "point-kind-input")
                         description
                         (value-with-id "point-description-input")
                         numeric-description
                         (value-with-id "point-numeric-description")))
                (content 
                 (chain *json-parser*
                        (write point-data))))
           (disable-element-with-id "finish-point-button")
           (disable-element-with-id "delete-point-button")
           (chain *open-layers
                  *request
                  (*post*
                   (create :url (+ "/" +proxy-root+ "/lib/update-point")
                           :data content
                           :headers (create "Content-type" "text/plain"
                                            "Content-length" (@ content
                                                                length))
                           :success (lambda ()
                                      (refresh-layer
                                       (@ *streetmap* user-point-layer))
                                      (reset-layers-and-controls)
                                      (request-user-point-choice))
                           :failure recommend-fresh-login)))))

       (defun delete-point ()
         "Purge currently selected user point from database."
         (let* ((user-point-id (@ *current-user-point* fid))
                (content 
                 (chain *json-parser*
                        (write user-point-id))))
           (disable-element-with-id "finish-point-button")
           (disable-element-with-id "delete-point-button")
           (chain *open-layers
                  *request
                  (*post*
                   (create :url (+ "/" +proxy-root+ "/lib/delete-point")
                           :data content
                           :headers (create "Content-type" "text/plain"
                                            "Content-length" (@ content
                                                                length))
                           :success (lambda ()
                                      (refresh-layer
                                       (@ *streetmap* user-point-layer))
                                      (reset-layers-and-controls)
                                      (request-user-point-choice true))
                           :failure recommend-fresh-login)))))

       (defun draw-active-point ()
         "Draw an Active Point, i.e. a point used in subsequent
         photogrammetric calculations."
         (chain this
                active-point-layer
                (add-features
                 (new (chain *open-layers
                             *feature
                             (*vector
                              (new (chain *open-layers
                                          *geometry
                                          (*point
                                           (@ this photo-parameters m)
                                           (@ this photo-parameters n))))))))))

       (defun image-click-action (clicked-image)
         (lambda (event)
           "Do appropriate things when an image is clicked into."
           (let* ((lonlat
                   (chain clicked-image map (get-lon-lat-from-view-port-px
                                             (@ event xy))))
                  (photo-parameters
                   (@ clicked-image photo-parameters))
                  pristine-image-p content request)
             (when (and (@ photo-parameters usable)
                        (chain clicked-image (photop)))
               (setf (@ photo-parameters m) (@ lonlat lon)
                     (@ photo-parameters n) (@ lonlat lat))
               (remove-layer (@ clicked-image map) "Active Point")
               (remove-any-layers "Epipolar Line")
               (setf *pristine-images-p* (not (some-active-point-p)))
               (setf (@ clicked-image active-point-layer)
                     (new (chain *open-layers
                                 *layer
                                 (*vector "Active Point"
                                          (create display-in-layer-switcher
                                                  nil)))))
               (chain clicked-image
                      map
                      (add-layer (@ clicked-image active-point-layer)))
               (chain clicked-image (draw-active-point))
               (if
                *pristine-images-p*
                (progn
                  (chain *streetmap* user-points-select-control (unselect-all))
                  (reset-controls)
                  (remove-any-layers "User Point") ;from images
                  (loop
                     for i across *images* do
                     (when (and (not (equal i clicked-image))
                                (chain i (photop)))
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
                        (chain *open-layers
                               *request
                               (*post*
                                (create :url (+ "/" +proxy-root+
                                                "/lib/epipolar-line")
                                        :data content
                                        :headers (create
                                                  "Content-type" "text/plain"
                                                  "Content-length"
                                                  (@ content length))
                                        :success (@ i draw-epipolar-line)
                                        :failure recommend-fresh-login
                                        :scope i))))
                       (chain i
                              map
                              (add-layer (@ i epipolar-layer))))))
                (progn
                  (remove-any-layers "Epipolar Line")
                  (remove-any-layers "Estimated Position")
                  (let* ((active-pointed-photo-parameters
                          (loop
                             for i across *images*
                             when (has-layer-p (@ i map) "Active Point")
                             collect (@ i photo-parameters)))
                         (content
                          (chain *json-parser*
                                 (write
                                  (list active-pointed-photo-parameters
                                        (chain *images*
                                               (map
                                                #'(lambda (x)
                                                    (@ x
                                                       photo-parameters)))))))))
                    (setf (@ clicked-image estimated-positions-request-response)
                          (chain *open-layers
                                 *request
                                 (*post*
                                  (create :url (+ "/" +proxy-root+
                                                  "/lib/estimated-positions")
                                          :data content
                                          :headers (create
                                                    "Content-type" "text/plain"
                                                    "Content-length"
                                                    (@ content length))
                                          :success (@ clicked-image
                                                      draw-estimated-positions)
                                          :failure recommend-fresh-login
                                          :scope clicked-image)))))))))))

       (defun iso-time-string (lisp-time)
         "Return Lisp universal time formatted as ISO time string"
         (let* ((unix-time (- lisp-time +unix-epoch+))
                (js-date (new (*date (* 1000 unix-time)))))
           (chain *open-layers *date (to-i-s-o-string js-date))))

       (defun delete-photo ()
         "Delete this object's photo."
         (loop
            repeat (chain this map (get-num-layers))
            do (chain this map layers 0 (destroy)))
         (hide-element-with-id (@ this usable-id))
         (setf (@ this trigger-time-div inner-h-t-m-l) nil))

       (defun photop ()
         "Check if this object contains a photo."
         (@ this trigger-time-div inner-h-t-m-l))

       (defun show-photo ()
         "Show the photo described in this object's photo-parameters."
         (let ((image-div-width
                (parse-int (chain (get-computed-style (@ this map div) nil)
                                  width)))
               (image-div-height
                (parse-int (chain (get-computed-style (@ this map div) nil)
                                  height)))
               (image-width
                (@ this photo-parameters sensor-width-pix))
               (image-height
                (@ this photo-parameters sensor-height-pix)))
           (chain
            this
            map
            (add-layer
             (new (chain
                   *open-layers
                   *layer
                   (*image
                    "Photo"
                    (photo-path (@ this photo-parameters))
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
                                     (max
                                      (/ image-width image-div-width)
                                      (/ image-height image-div-height)))))))))
           (when (@ this photo-parameters rendered-footprint)
             (setf (@ this footprint-layer)
                   (new (chain
                         *open-layers
                         *layer
                         (*vector "Footprint"
                                  (create display-in-layer-switcher nil
                                          style (create stroke-color "yellow"
                                                        stroke-width 1
                                                        stroke-opacity .3))))))
             (chain this
                    footprint-layer
                    (add-features
                     (chain *geojson-parser*
                            (read (@ this
                                     photo-parameters
                                     rendered-footprint)))))
             (chain this
                    map
                    (add-layer (@ this footprint-layer))))
           (chain this map (zoom-to-max-extent))
           (if (@ this photo-parameters usable)
               (hide-element-with-id (@ this usable-id))
               (reveal-element-with-id (@ this usable-id)))
           (setf (@ this trigger-time-div inner-h-t-m-l)
                 (iso-time-string (@ this photo-parameters trigger-time)))))

       (defun zoom-images-to-max-extent ()
         "Zoom out all images."
         (loop
            for i across *images*
            do (when (> (@ i map layers length) 0)
                 (chain i map (zoom-to-max-extent)))))

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
         (setf (@ (aref *images* image-index) usable-id)
               (+ "image-" image-index "-usable"))
         (hide-element-with-id (+ "image-" image-index "-usable"))
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
         (unselect-combobox-selection "point-kind")
         (unselect-combobox-selection "point-description")
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
              (hide-elements-of-class "point-editor")
              (hide-elements-of-class "point-viewer")
              (hide-elements-of-class "aux-data-viewer")
              (hide-elements-of-class "point-creator")
              (reveal-elements-of-class "multiple-points-viewer"))
             ((= selected-features-count 1)
              (setf (value-with-id "point-kind-input")
                    (@ *current-user-point* attributes kind))
              (setf (value-with-id "point-description-input")
                    (@ *current-user-point* attributes description))
              (setf (value-with-id "point-numeric-description")
                    (@ *current-user-point* attributes numeric-description))
              (setf (inner-html-with-id "point-creation-date")
                    (@ *current-user-point* attributes creation-date))
              (setf (inner-html-with-id "aux-numeric-list")
                    (html-table
                     (@ *current-user-point* attributes aux-numeric)
                     +aux-numeric-labels+))
              (setf (inner-html-with-id "aux-text-list")
                    (html-table
                     (@ *current-user-point* attributes aux-text)
                     +aux-text-labels+))
              (hide-elements-of-class "aux-data-viewer")
              (reveal-elements-of-class "point-editor")
              (if (write-permission-p
                   (@ *current-user-point* attributes user-name))
                  (progn
                    (setf (chain document
                                 (get-element-by-id "finish-point-button")
                                 onclick)
                          (lambda () (finish-point #'update-point)))
                    (enable-element-with-id "finish-point-button")
                    (enable-element-with-id "delete-point-button")
                    (hide-elements-of-class "point-creator")
                    (hide-elements-of-class "point-viewer")
                    (hide-elements-of-class "aux-data-viewer")
                    (reveal-elements-of-class "point-editor"))
                  (progn
                    (disable-element-with-id "finish-point-button")
                    (disable-element-with-id "delete-point-button")
                    (hide-elements-of-class "point-creator")
                    (hide-elements-of-class "point-editor")
                    (hide-elements-of-class "aux-data-viewer")
                    (reveal-elements-of-class "point-viewer")))
              (setf (inner-html-with-id "creator")
                    (if (@ *current-user-point* attributes user-name)
                        (+ "(by "
                           (@ *current-user-point* attributes user-name)
                           ")")
                        "(ownerless)")))
             (t
              (hide-elements-of-class "multiple-points-viewer")
              (hide-elements-of-class "point-editor")
              (hide-elements-of-class "point-viewer")
              (hide-elements-of-class "aux-data-viewer")
              (reveal-elements-of-class "point-creator"))))
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
               (chain *open-layers
                      *request
                      (*post*
                       (create :url (+ "/" +proxy-root+
                                       "/lib/user-point-positions")
                               :data content
                               :headers (create "Content-type" "text/plain"
                                                "Content-length" (@ content
                                                                    length))
                               :success draw-user-points
                               :failure recommend-fresh-login)))))

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

       (defun html-table (aux-data labels)
         "Return an html-formatted table with a label column from
         labels and a data column from aux-data."
         (if aux-data
             (who-ps-html
              (:table :class "aux-data-table"
                      (chain aux-data
                             (reduce (lambda (x y i)
                                       (+ x (who-ps-html
                                             (:tr
                                              (:td :class "aux-data-label"
                                                   (+
                                                    (if labels
                                                        (elt labels i)
                                                        i)
                                                    ":"))
                                              (:td :class "aux-data-value"
                                                   y)))))
                                     ""))))
             ""))

       (defun nearest-aux-point-selected (event)
         "Things to do once a nearest auxiliary point is selected in
         streetmap."
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
                 (html-table aux-numeric +aux-numeric-labels+))
           (setf (inner-html-with-id "aux-text-list")
                 (html-table aux-text +aux-text-labels+))))

       (defun bye ()
         "Store user's current map extent and log out."
         (let* ((bbox (chain *streetmap*
                             (get-extent)
                             (transform +spherical-mercator+ +geographic+)
                             (to-b-b-o-x)))
                (href (+ "/" +proxy-root+ "/lib/logout?bbox=" bbox)))
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
           (enable-element-with-id "point-kind-input")
           (enable-element-with-id "point-kind-select")
           (enable-element-with-id "point-description-input")
           (enable-element-with-id "point-description-select")
           (enable-element-with-id "point-numeric-description")
           (request-user-point-choice true))
         (hide-elements-of-class "point-editor")
         (hide-elements-of-class "point-viewer")
         (hide-elements-of-class "aux-data-viewer")
         (hide-elements-of-class "multiple-points-viewer")
         (reveal-elements-of-class "point-creator")
         (hide-element-with-id "no-footprints-p")
         (hide-element-with-id "caching-indicator")
         (hide-element-with-id "uniquify-buttons")
         (setf *aux-point-distance-select*
               (chain document (get-element-by-id "aux-point-distance")))
         (hide-aux-data-choice)
         (let ((cursor-layer-style
                (create
                 graphic-width 14
                 external-graphic (+ "/" +proxy-root+
                                     "/lib/public_html/phoros-cursor.png"))))
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
                               (create :url (+ "/" +proxy-root+
                                               "/lib/points.json"))))
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
                             (create :url (+ "/" +proxy-root+ "/lib/user-points.json"))))
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
                               (create :url (+ "/" +proxy-root+
                                               "/lib/aux-points.json"))))
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
                                          +presentation-project-bounds+))))))))
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
            for i from 0 below (lisp *number-of-images*)
            do (initialize-image i))
         (add-help-events)
         (request-restriction-select-choice)
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
