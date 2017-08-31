;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2012, 2016 Bert Burgemeister
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


(in-package #:phoros-fasttrack)

(defparameter *phoros-version*
  (asdf:component-version (asdf:find-system :phoros))
  "Phoros version as defined in system definition.")

(cffi:define-foreign-library phoml
  (:unix (:or "./libphoml.so"
              "./phoml/lib/libphoml.so"))
  (t (:default "libphoml")))

(setf *read-default-float-format* 'double-float)

(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))

(defparameter *fasttrack-version*
  (asdf:component-version (asdf:find-system :phoros))
  "Fasttrack version as defined in system definition.  TODO: enforce equality with *phoros-version*")

(defstruct (db-credentials (:type list)) database user password host (port-key :port :read-only t) (port 5432) (ssl-key :use-ssl :read-only t) (ssl :no) (modifiedp-key :modifiedp :read-only t) (modifiedp t) (table-key :table :read-only t) table (allow-other-keys-key :allow-other-keys :read-only t) (allow-other-keys-value t :read-only t))

(defvar *postgresql-road-network-credentials* (make-db-credentials)
  "A list: (database user password host :port 5432 :use-ssl ssl-p.")

(defvar *postgresql-zeb-credentials* (make-db-credentials)
  "A list: (database user password host :port 5432 use-ssl :ssl-p.")

(defvar *road-network-chart-configuration* nil
  "Database columns selected for rendering.")

(defvar *zeb-chart-configuration* nil
  "Database columns selected for rendering.")

(defvar *accidents-chart-configuration* (list nil nil nil)
  "Accidents rendering parameters.")

(defvar *postgresql-accidents-credentials* (make-db-credentials)
  "A list: (database user password host &key :port 5432 :use-ssl ssl-p.")

(defvar *postgresql-road-network-ok* nil
  "t if database connection could be established.")

(defvar *postgresql-zeb-ok* nil
  "t if database connection could be established.")

(defvar *postgresql-accidents-ok* nil
  "t if database connection could be established.")

(defvar *station* 0
  "Current station.")

(defvar *road-section* nil
  "If there is a chart, we store a list of its parameters (table vnk
  nnk road-section-length) here.")

(defvar *road-section-raw-data* (make-hash-table :size 97)
  "Undigested selected row numbers from road section dialog")

(defvar *road-section-selection* '()
  "Row numbers of the road sections selected for processing.")

(defvar *road-network-chart-raw-data* (make-hash-table :size 997 :test #'equal)
  "Raw messages from the road network part of the chart dialog")

(defvar *accidents-chart-raw-data* (list nil nil nil)
  "Undigested input from the accidents part of the chart dialog.")

(defvar *zeb-chart-raw-data* (make-hash-table :size 997 :test #'equal)
  "Raw messages from road section dialog")

(defparameter *aggregate-view-columns*
  (list 'usable
        'recorded-device-id                ;debug
        'device-stage-of-life-id           ;debug
        'generic-device-id                 ;debug
        'directory
        'measurement-id
        'filename 'byte-position 'point-id
        'trigger-time
        ;;'coordinates   ;the search target
        'longitude 'latitude 'ellipsoid-height
        'cartesian-system
        'east-sd 'north-sd 'height-sd
        'roll 'pitch 'heading
        'roll-sd 'pitch-sd 'heading-sd
        'sensor-width-pix 'sensor-height-pix
        'pix-size
        'bayer-pattern 'color-raiser
        'mounting-angle
        'dx 'dy 'dz 'omega 'phi 'kappa
        'c 'xh 'yh 'a1 'a2 'a3 'b1 'b2 'c1 'c2 'r0
        'b-dx 'b-dy 'b-dz 'b-rotx 'b-roty 'b-rotz
        'b-ddx 'b-ddy 'b-ddz
        'b-drotx 'b-droty 'b-drotz)
  "Most of the column names of aggregate-view.")

(defvar *phoros-cookies* nil
  "Container for cookies sent by Phoros server")

(defvar *phoros-url* nil
  "URL of the Phoros project currently in use.")

(defvar *phoros-credentials* '("user" "password")
  "List of (user password) used for login at *phoros-url*.")

(defvar *cache-dir* '(:relative "cache"))

(defparameter *image-size* '(800 700)
  "Image size in pixels in a list (width height).")

(defparameter *chart-height* 155
  "Height of chart in pixels.")

(defparameter *chart-fringe* 20
  "Lower, uncharted part of chart.")

(defparameter *chart-tail* 200
  "Rightmost, uncharted part of chart.")

(defparameter *scale-distance* 40
  "Horizontal distance between two scales.")

(defvar *cruise-control* nil)
(defvar *cruise-control-backward-p* nil)

(defvar *rear-view-image-done* nil)

(defvar *front-view-image-done* nil)

(defvar *pipeglade-pid-file* "fasttrack-pipeglade.pid")

(defparameter *cursor-color* "orange"
  "Color of cursor in both chart and images.")

(defparameter *big-step* 10
  "Station increment/decrement.")

(defparameter *pipeglade-out-lock* (bt:make-lock))
(defparameter *pipeglade-out-fifo* "in.fifo")
(defparameter *pipeglade-in-fifo* "out.fifo")

(defun pipeglade-out (widget action &rest data)
  "Send a pipeglade command to UI."
  (bt:with-lock-held (*pipeglade-out-lock*)
    (with-open-file (out *pipeglade-out-fifo*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :error)
      (format out "~A:~A~{ ~@[~A~]~}~%" widget action data))))


(defun ensure-hyphen-before-digit (symbol)
  "Return symbol with hyphens inserted after each letter that is
followed by a digit. "
  (intern
   (coerce
    (loop
       with need-hyphen-before-next-digit-p
       for c across (string symbol)
       if (and need-hyphen-before-next-digit-p (digit-char-p c))
       collect #\- and collect c and do (setf need-hyphen-before-next-digit-p nil)
       else collect c and do (setf need-hyphen-before-next-digit-p nil)
       end
       if (alpha-char-p c) do (setf need-hyphen-before-next-digit-p t) end)
    'string)))


(defmacro with-statusbar-message (message &body body)
  "Push message to statusbar while body is executing."
  `(unwind-protect
        (progn
          (pipeglade-out "statusbar" "push_id" (sxhash ,message) ,message)
          ,@body)
     (pipeglade-out "statusbar" "pop_id" (sxhash ,message))))

(defmacro with-spinner (spinner &body body)
  "Let spinner spin while body is executing."
  `(unwind-protect
        (progn
          (pipeglade-out ,spinner "start")
          ,@body)
     (pipeglade-out ,spinner "stop")))

(define-condition attention () ())

(defmacro defun-cached (name (&rest args) &body body &aux (doc ""))
  "Define a function whose return value must be readibly printable, is
  being read from a chache if possible, and is being cached if
  necessary.  The function defined has a secondary return value
  cached-p.  If function is called with :from-cache-only t, let it
  return nil and nil if there is nothing cached.  If function is
  called with a :message keyarg, a pretty-printed version will be
  shown as part of the statusbar message."
  (when (stringp (car body))
    (setf doc (car body))
    (setf body (cdr body)))
  (cl-utilities:with-unique-names (input-stream output-stream)
    `(defun ,name (,@args &key from-cache-only create-fresh-cache message)
       ,doc
       (flet ((read-from-cache ()
                (with-open-file (,input-stream (cache-file-name ',name ,@args)
                                               :direction :input
                                               :if-does-not-exist :error)
                  (values (read ,input-stream) t)))
              (run-and-cache ()
                (values (with-statusbar-message (format nil "populating cache [~(~A~)~@[ ~A~]]" ',name message)
                          (with-open-file (,output-stream (cache-file-name ',name ,@args)
                                                          :direction :output
                                                          :if-exists :supersede)
                            (prin1 (progn ,@body)
                                   ,output-stream)))
                        nil)))
         (ensure-directories-exist (cache-file-name ',name ,@args))

         (handler-bind
             ((file-error (lambda (c)
                       (invoke-restart 'restart-create-fresh-cache "FILE")))
              (end-of-file (lambda (c)
                       (invoke-restart 'restart-create-fresh-cache "EOF"))))
           (restart-case (if create-fresh-cache
                             (run-and-cache)
                             (read-from-cache))
             (restart-create-fresh-cache (para)
               (if from-cache-only
                   (values nil nil)
                   (,name ,@args :create-fresh-cache t :message message)))))))))

(defun empty-image-data-p (image-data)
  (and (not (image-data-station image-data))
       (empty-coordinates-p (image-data-station-coordinates image-data))))

(defun empty-coordinates-p (coordinates)
  (not (or (coordinates-longitude coordinates)
           (coordinates-latitude coordinates)
           (coordinates-ellipsoid-height coordinates)
           (coordinates-azimuth coordinates))))

(defun image-data-equal (i1 i2)
  (and (eql (image-data-station i1) (image-data-station i2))
       (coordinates-equal (image-data-station-coordinates i1) (image-data-station-coordinates i2))
       (equal (image-data-filename i1) (image-data-filename i2))
       (eql (image-data-byte-position i1) (image-data-byte-position i2))))

(defun coordinates-equal (c1 c2)
  (and (eql (coordinates-longitude c1) (coordinates-longitude c2))
       (eql (coordinates-latitude c1) (coordinates-latitude c2))
       (eql (coordinates-ellipsoid-height c1) (coordinates-ellipsoid-height c2))
       (eql (coordinates-azimuth c1) (coordinates-azimuth c2))))

(defun display-date-and-image (time-widget img-widget draw-widget spinner-widget image-data)
  "Display image and its trigger time on UI.  Return the time the UI
is estimated to take."
  (let ((sleep-duration 0))
    (with-spinner spinner-widget
      (pipeglade-out time-widget "set_text" (iso-time (image-data-trigger-time image-data)))
      (handler-case
          (let ((image-filename (namestring (download-image image-data))))
            (if image-filename
                (progn
                  (pipeglade-out draw-widget "remove" 2)
                  (pipeglade-out img-widget "set_from_file" image-filename)
                  (setf sleep-duration .3))
                (progn
                  (pipeglade-out img-widget "set_from_file" "public_html/phoros-logo-background.png")
                  (setf sleep-duration .1))))
        (phoros-server-error ()
          (pipeglade-out draw-widget "remove" 2)
          (pipeglade-out img-widget "set_from_file" "public_html/phoros-logo-background.png")
          (setf sleep-duration 1)))
      sleep-duration)))

(defun clear-date-image-and-arrow (time-widget img-widget draw-widget)
  (pipeglade-out img-widget "set_from_file" "public_html/phoros-logo-background.png")
  (pipeglade-out time-widget "set_text")
  (pipeglade-out draw-widget "remove" 2))

(defun display-image-arrow (draw-widget image-arrow-coordinates station)
  "Display a station marker in the image on UI.  Return the time the
UI is estimated to take."
  (if image-arrow-coordinates
      (let* ((point-radius 5)
             (image-label-coordinates (ignore-errors
                                        (list (- (first image-arrow-coordinates) point-radius)
                                              (- (second image-arrow-coordinates) point-radius)))))
        (pipeglade-out draw-widget "remove" 2)
        (pipeglade-out draw-widget "move_to" 2 (first image-arrow-coordinates) (second image-arrow-coordinates))
        (pipeglade-out draw-widget "line_to" 2 (first (last image-arrow-coordinates 2)) (second (last image-arrow-coordinates 2)))
        (pipeglade-out draw-widget "stroke" 2)
        (pipeglade-out draw-widget "arc" 2 (first image-arrow-coordinates) (second image-arrow-coordinates) point-radius 0 360)
        (pipeglade-out draw-widget "stroke" 2)
        (pipeglade-out draw-widget "move_to" 2 (first image-label-coordinates) (second image-label-coordinates))
        (pipeglade-out draw-widget "rel_move_for" 2 "se" station)
        (pipeglade-out draw-widget "show_text" 2 station)
        0)
      (progn
        (pipeglade-out draw-widget "remove" 2)
        0)))

(defmacro image-worker (view-direction)
  (let (global-image-data global-image-arrow-coordinates global-image-done time-widget spinner-widget draw-widget img-widget)
    (ecase view-direction
      (:rear-view
       (setf global-image-data '*rear-view-image-data*)
       (setf global-image-arrow-coordinates '*rear-view-image-arrow-coordinates*)
       (setf global-image-done '*rear-view-image-done*)
       (setf time-widget "rear_view_time")
       (setf spinner-widget "spinner_rearview")
       (setf draw-widget "draw_rearview")
       (setf img-widget "img_rearview"))
      (:front-view
       (setf global-image-data '*front-view-image-data*)
       (setf global-image-arrow-coordinates '*front-view-image-arrow-coordinates*)
       (setf global-image-done '*front-view-image-done*)
       (setf time-widget "front_view_time")
       (setf spinner-widget "spinner_frontview")
       (setf draw-widget "draw_frontview")
       (setf img-widget "img_frontview")))
    (cl-utilities:with-unique-names (current-image-data
				     current-station
				     current-image-arrow-coordinates
                                     current-road-section
				     station
                                     road-section
				     image-data
				     image-arrow-coordinates
                                     sleep-duration
				     point-radius
				     image-filename
				     image-label-coordinates)
      `(lambda ()
	 (let ((current-image-data *empty-image-data*)
	       (current-station 0)
               (current-road-section nil)
	       (current-image-arrow-coordinates nil))
	   (loop
	      (let ((station *station*)
                    (road-section *road-section*)
		    (image-data ,global-image-data)
		    (image-arrow-coordinates ,global-image-arrow-coordinates)
                    (sleep-duration 0))
                (block image-worker
                  (block image-output
                    (if (image-data-equal current-image-data image-data)
                        (if (and (eql current-station station)
                                 (equal current-road-section road-section))
                            (progn
                              (incf sleep-duration .1)
                              (bt:thread-yield)
                              (return-from image-worker))
                            (progn
                              (psetf current-station station
                                     current-road-section road-section)
                              ;; (incf sleep-duration .1)
                              (return-from image-output)))
                        (progn
                          (psetf current-image-data image-data
                                 current-station station
                                 current-road-section road-section)
                          (if (empty-image-data-p image-data)
                              (progn
                                (clear-date-image-and-arrow ,time-widget ,img-widget ,draw-widget)
                                ;; (incf sleep-duration .1)
                                (return-from image-worker))
                              (incf sleep-duration
                                    (display-date-and-image ,time-widget ,img-widget ,draw-widget ,spinner-widget image-data))))))
                  (if (equal current-image-arrow-coordinates image-arrow-coordinates)
                      (progn
                        (incf sleep-duration .1)
                        (return-from image-worker))
                      (progn
                        (setf current-image-arrow-coordinates image-arrow-coordinates)
                        (incf sleep-duration
                              (display-image-arrow ,draw-widget image-arrow-coordinates station)))))
                (sleep sleep-duration)
                (setf ,global-image-done t))))))))

(eval '(defstruct coordinates
        longitude
        latitude
        ellipsoid-height
        azimuth))

(eval `(defstruct image-data
         ;; fasttrack auxiliary slots
         station
         station-coordinates
         (rear-view-p nil)
         ;; original Phoros image data slots
         ,@(mapcar #'ensure-hyphen-before-digit *aggregate-view-columns*)))

(defparameter *empty-coordinates*
  (make-coordinates :longitude nil
                    :latitude nil
                    :ellipsoid-height nil
                    :azimuth nil)
  "Representation of a zero value for coordinates.")

(defparameter *empty-image-data*
  (make-image-data :station nil
                   :station-coordinates *empty-coordinates*)
  "Representation of a zero value for image-data.")

(defvar *rear-view-image-data* *empty-image-data*
  "The currently displayed image.")

(defvar *front-view-image-data* *empty-image-data*
  "The currently displayed image.")

(defvar *rear-view-image-arrow-coordinates* nil)

(defvar *front-view-image-arrow-coordinates* nil)

(defvar *show-rear-view-p* t)

(defvar *show-front-view-p* t)

(defun start-pipeglade ()
  (let* ((stale-pipeglade-pid
	  (with-open-file (stream *pipeglade-pid-file*
				  :direction :input :if-does-not-exist :create)
	    (read stream nil)))
	 (stale-pipeglade-program-name
	  (uiop:run-program (format nil "ps -p ~A -o comm=" stale-pipeglade-pid) :output :string :ignore-error-status t))
	 (length (min (length "pipeglade") (length stale-pipeglade-program-name))))
    (when (string= "pipeglade" stale-pipeglade-program-name :end2 length)
      (uiop:run-program (format nil "kill ~A" stale-pipeglade-pid))))
  (let ((pipeglade-args "-i in.fifo -o out.fifo -u fasttrack.ui -b -l log.log --name fasttrack --class Phoros"))
    (loop
       for i in '("./pipeglade" "~/pipeglade/pipeglade" "pipeglade")
       until (probe-file i)
       finally (uiop:run-program (format nil "~A ~A" i pipeglade-args) :output *pipeglade-pid-file*))))
  
(defun version-number-parts (dotted-string)
  "Return the three version number components of something like
  \"11.22.33\"."
  (when dotted-string
    (values-list (mapcar #'parse-integer
                         (cl-utilities:split-sequence #\. dotted-string)))))

(defun fasttrack-version (&key major minor revision)
  "Return version of this program, either one integer part as denoted by
the key argument, or the whole dotted string."
  (multiple-value-bind (major-number minor-number revision-number)
      (version-number-parts *fasttrack-version*)
    (cond (major major-number)
          (minor minor-number)
          (revision revision-number)
          (t *fasttrack-version*))))

(defun main ()
  (handler-case
      (progn
	(in-package #:phoros-fasttrack) ;for reading of cached #S(...) forms
	(cffi:use-foreign-library phoml)
	(start-pipeglade)
	(restore-road-network-credentials)
	(restore-zeb-credentials)
	(restore-accidents-credentials)
	(restore-phoros-credentials)
	(restore-road-network-chart-configuration)
	(restore-zeb-chart-configuration)
	(restore-accidents-chart-configuration)
	(restore-road-section)
	(update-credentials-dialog)
	;; Kludge: tickle the dialog to make spinbuttons receptive
	(pipeglade-out "chart_configuration" "set_visible" 1)
	(pipeglade-out "chart_configuration" "set_visible" 0)
	(pipeglade-out "chart_road_network" "set_line_cap" 1 "round")
	(pipeglade-out "chart_road_network" "set_line_join" 1 "round")
	(pipeglade-out "chart_zeb" "set_line_cap" 1 "round")
	(pipeglade-out "chart_zeb" "set_line_join" 1 "round")
	(pipeglade-out "chart_accidents" "set_line_join" 1 "miter")
	(pipeglade-out "chart_accidents" "set_line_width" 1 1)
	(pipeglade-out "chart_cursor" "set_source_rgba" 1 *cursor-color*)
	(pipeglade-out "chart_cursor" "set_line_width" 1 3)
	(pipeglade-out "chart_cursor" "set_dash" 1 3)
	(pipeglade-out "chart_cursor" "set_font_size" 1 10)
	(pipeglade-out "chart_road_network_scale" "set_font_size" 1 10)
	(pipeglade-out "zeb_network_scale" "set_font_size" 1 10)
	(pipeglade-out "draw_rearview" "set_source_rgba" 1 *cursor-color*)
	(pipeglade-out "draw_rearview" "set_line_cap" 1 "round")
	(pipeglade-out "draw_rearview" "set_line_width" 1 2)
	(pipeglade-out "draw_rearview" "set_font_size" 1 10)
	(pipeglade-out "draw_frontview" "set_source_rgba" 1 *cursor-color*)
	(pipeglade-out "draw_frontview" "set_line_cap" 1 "round")
	(pipeglade-out "draw_frontview" "set_line_width" 1 2)
	(pipeglade-out "draw_frontview" "set_font_size" 1 10)
	(pipeglade-out "version" "set_text" "version" *phoros-version*)
	(with-open-file (in *pipeglade-in-fifo*
			    :direction :input
			    :if-does-not-exist :error)
	  (bt:make-thread
	   (image-worker :rear-view)
	   :name "rear-view-image-worker")
	  (bt:make-thread
	   (image-worker :front-view)
	   :name "front-view-image-worker")
	  (bt:make-thread
	   #'jump-to-station-worker
	   :name "jump-to-station-worker")
	  (bt:make-thread
	   #'cruise-control-worker
	   :name "cruise-control-worker")
          (check-credentials-dialog-statuses)
          (handler-case
              (apply #'phoros-login *phoros-url* *phoros-credentials*)
            (phoros-server-error ()))
	  ;; getting rid of initial feedback from credentials dialog:
	  (with-statusbar-message "please wait" (sleep 1))
	  (clear-input in)
	  (populate-road-section-dialog)
	  (restore-road-section-image-counts)
	  (restore-road-section-selection)
	  (update-road-section-selection)
	  ;; (set-road-section)
	  (update-station (saved-station))
	  (populate-chart-dialog)
	  (refresh-chart)
	  (with-statusbar-message "starting browser"
	    (uiop:run-program (format nil "firefox '~A' &" *phoros-url*)))
	  (loop
	     for message = (read-line in nil)
	     do
	       (cond
		 ((message-name= "quit" message)
		  (pipeglade-out "_" "main_quit")
		  (signal 'attention)
		  (loop-finish))
		 ((and (message-name= "main" message)
		       (string= (message-info message) "closed"))
		  (pipeglade-out "_" "main_quit")
		  (loop-finish))
		 ((message-name= "station_scale" message) ;the sole invocation of jump-to-station
		  (jump-to-station (parse-integer (message-data message) :junk-allowed t)))
		 ((message-name= "show_road_network_chart" message)
		  (pipeglade-out "chart_road_network" "set_visible" (message-info message))
		  (pipeglade-out "chart_road_network_scale" "set_visible" (message-info message)))
		 ((message-name= "show_zeb_chart" message)
		  (pipeglade-out "chart_zeb" "set_visible" (message-info message))
		  (pipeglade-out "chart_zeb_scale" "set_visible" (message-info message)))
		 ((message-name= "show_accidents_chart" message)
		  (pipeglade-out "chart_accidents" "set_visible" (message-info message)))
		 ((message-name= "show_rear_view" message)
		  (setf *show-rear-view-p* (string= (message-info message) "1")))
		 ((message-name= "show_front_view" message)
		  (setf *show-front-view-p* (string= (message-info message) "1")))
		 ((message-name= "big_step" message)
		  (let* ((step (parse-integer (message-data message) :junk-allowed t))
			 (label-text (format nil "~D m" step)))
		    (pipeglade-out "back" "set_label" label-text)
		    (pipeglade-out "forward" "set_label" label-text)
		    (pipeglade-out "big_step_back" "set_label" label-text)
		    (pipeglade-out "big_step_forward" "set_label" label-text)
		    (pipeglade-out "station_scale" "set_increments" 1 step)
		    (setf *big-step* step)))
		 ((message-name= "step_back" message)
		  (stop-cruise-control)
                  (update-station (1- (saved-station))))
		 ((message-name= "step_forward" message)
		  (stop-cruise-control)
                  (update-station (1+ (saved-station))))
		 ((message-name= "big_step_back" message)
		  (stop-cruise-control)
                  (update-station (- (saved-station) *big-step*)))
		 ((message-name= "big_step_forward" message)
		  (stop-cruise-control)
                  (update-station (+ (saved-station) *big-step*)))
		 ((message-name= "back" message)
		  (stop-cruise-control)
		  (cruise-control :backwardp t))
		 ((message-name= "forward" message)
		  (stop-cruise-control)
		  (cruise-control :backwardp nil))
		 ((message-name= "stop" message)
		  (stop-cruise-control))
		 ((message-name= "first_section" message)
                  (stop-cruise-control)
		  (set-road-section :direction :first)
		  (refresh-chart)
                  (update-station 0))
		 ((message-name= "previous_section" message)
                  (stop-cruise-control)
		  (set-road-section :direction :predecessor)
		  (refresh-chart)
                  (update-station 0))
		 ((message-name= "next_section" message)
                  (stop-cruise-control)
		  (set-road-section :direction :successor)
		  (refresh-chart)
                  (update-station 0))
		 ((message-name= "last_section" message)
                  (stop-cruise-control)
		  (set-road-section :direction :last)
		  (refresh-chart)
                  (update-station 0))
		 ((message-name= "road_sections" message)
		  (collect-road-section-select message))
		 ((message-name= "road_section_ok" message)
		  (digest-road-section-raw-data))
		 ((message-name= "road_section_cncl" message)
		  (restore-road-section-selection)
		  (pipeglade-out "road_section" "set_visible" 0))
		 ((message-name= "road_network" message)
		  (collect-raw-message message *road-network-chart-raw-data*))
		 ((message-name= "zeb" message)
		  (collect-raw-message message *zeb-chart-raw-data*))
		 ((message-name= "render_accidents" message)
		  (setf (first *accidents-chart-raw-data*) (message-info message)))
		 ((message-name= "accidents_from" message)
		  (setf (second *accidents-chart-raw-data*) (message-data message)))
		 ((message-name= "accidents_to" message)
		  (setf (third *accidents-chart-raw-data*) (message-data message)))
		 ((message-name= "chart_configuration_ok" message)
		  (setf *road-network-chart-configuration* (digest-chart-raw-data *road-network-chart-raw-data*))
		  (save-place *road-network-chart-configuration* 'road-network-chart-configuration)
		  (setf *zeb-chart-configuration* (digest-chart-raw-data *zeb-chart-raw-data*))
		  (save-place *zeb-chart-configuration* 'zeb-chart-configuration)
		  (digest-accidents-chart-raw-data)
		  (update-accidents-chart-dialog)
		  (pipeglade-out "text_values" "clear")
		  (refresh-chart))
		 ((message-name= "chart_configuration_cncl" message)
		  (update-accidents-chart-dialog)
		  (setf *accidents-chart-raw-data* (list nil nil nil))
		  (pipeglade-out "chart_configuration" "set_visible" 0))
		 ((message-name= "credentials_check" message)
		  (check-credentials-dialog-statuses))
		 ((message-name= "credentials_ok" message)
		  (check-credentials-dialog-statuses)
		  (when (db-credentials-modifiedp *postgresql-road-network-credentials*)
		    (invalidate-road-section-selection)
		    (invalidate-road-section)
		    (invalidate-road-network-chart-configuration)
		    (populate-road-section-dialog)
		    (update-chart-dialog)
		    (save-road-network-credentials nil))
		  (when (db-credentials-modifiedp *postgresql-zeb-credentials*)
		    (update-chart-dialog)
		    (invalidate-zeb-chart-configuration)
		    (pipeglade-out "text_values" "clear")
		    (refresh-chart)
		    (save-zeb-credentials nil))
		  (when (db-credentials-modifiedp *postgresql-accidents-credentials*)
		    (refresh-chart)
		    (save-accidents-credentials nil))
		  (handler-case (apply #'phoros-login *phoros-url* *phoros-credentials*)
                    (phoros-server-error ()))
		  (forget-images-being-launched)
                  (update-station (saved-station))
		  (update-chart-dialog))
		 ((message-name= "road_network_host" message)
		  (setf (db-credentials-host *postgresql-road-network-credentials*) (message-data message))
		  (save-road-network-credentials t))
		 ((message-name= "road_network_port" message)
		  (setf (db-credentials-port *postgresql-road-network-credentials*)
			(parse-integer (message-data message) :junk-allowed t))
		  (save-road-network-credentials t))
		 ((message-name= "road_network_ssl" message)
		  (setf (db-credentials-ssl *postgresql-road-network-credentials*) (if (string= (message-data message) "1") :yes :no))
		  (save-road-network-credentials t))
		 ((message-name= "road_network_database" message)
		  (setf (db-credentials-database *postgresql-road-network-credentials*) (message-data message))
		  (save-road-network-credentials t))
		 ((message-name= "road_network_user" message)
		  (setf (db-credentials-user *postgresql-road-network-credentials*) (message-data message))
		  (save-road-network-credentials t))
		 ((message-name= "road_network_password" message)
		  (setf (db-credentials-password *postgresql-road-network-credentials*) (message-data message))
		  (save-road-network-credentials t))
		 ((message-name= "road_network_table" message)
		  (setf (db-credentials-table *postgresql-road-network-credentials*) (message-data message))
		  (save-road-network-credentials t))
		 ((message-name= "zeb_host" message)
		  (setf (db-credentials-host *postgresql-zeb-credentials*) (message-data message))
		  (save-zeb-credentials t))
		 ((message-name= "zeb_port" message)
		  (setf (db-credentials-port *postgresql-zeb-credentials*)
			(parse-integer (message-data message) :junk-allowed t))
		  (save-zeb-credentials t))
		 ((message-name= "zeb_ssl" message)
		  (setf (db-credentials-ssl *postgresql-zeb-credentials*) (if (string= (message-info message) "1") :yes :no))
		  (save-zeb-credentials t))
		 ((message-name= "zeb_database" message)
		  (setf (db-credentials-database *postgresql-zeb-credentials*) (message-data message))
		  (save-zeb-credentials t))
		 ((message-name= "zeb_user" message)
		  (setf (db-credentials-user *postgresql-zeb-credentials*) (message-data message))
		  (save-zeb-credentials t))
		 ((message-name= "zeb_password" message)
		  (setf (db-credentials-password *postgresql-zeb-credentials*) (message-data message))
		  (save-zeb-credentials t))
		 ((message-name= "zeb_table" message)
		  (setf (db-credentials-table *postgresql-zeb-credentials*) (message-data message))
		  (save-zeb-credentials t))
		 ((message-name= "accidents_host" message)
		  (setf (db-credentials-host *postgresql-accidents-credentials*) (message-data message))
		  (save-accidents-credentials t))
		 ((message-name= "accidents_port" message)
		  (setf (db-credentials-port *postgresql-accidents-credentials*)
			(parse-integer (message-data message) :junk-allowed t))
		  (save-accidents-credentials t))
		 ((message-name= "accidents_ssl" message)
		  (setf (db-credentials-ssl *postgresql-accidents-credentials*) (if (string= (message-data message) "1") :yes :no))
		  (save-accidents-credentials t))
		 ((message-name= "accidents_database" message)
		  (setf (db-credentials-database *postgresql-accidents-credentials*) (message-data message))
		  (save-accidents-credentials t))
		 ((message-name= "accidents_user" message)
		  (setf (db-credentials-user *postgresql-accidents-credentials*) (message-data message))
		  (save-accidents-credentials t))
		 ((message-name= "accidents_password" message)
		  (setf (db-credentials-password *postgresql-accidents-credentials*) (message-data message))
		  (save-accidents-credentials t))
		 ((message-name= "accidents_table" message)
		  (setf (db-credentials-table *postgresql-accidents-credentials*) (message-data message))
		  (save-accidents-credentials t))
		 ((message-name= "phoros_url" message)
		  (setf *phoros-url* (message-data message))
		  (save-phoros-credentials))
		 ((message-name= "phoros_user" message)
		  (setf (first *phoros-credentials*) (message-data message))
		  (save-phoros-credentials))
		 ((message-name= "phoros_password" message)
		  (setf (second *phoros-credentials*) (message-data message))
		  (save-phoros-credentials))
		 ((message-name= "phoros" message)
		  (run-phoros-browser))
		 (t
		  (print (list "fallen through:" message)))))))
    (sb-sys:interactive-interrupt () (kill-pipeglade))
    ;; (error (e)
    ;;   (print e)
    ;;   (kill-pipeglade))
    ))

(defun kill-pipeglade ()
  (let ((pipeglade-pid
         (with-open-file (stream *pipeglade-pid-file* :direction :input)
           (read stream nil))))
    (uiop:run-program (format nil "kill ~A" pipeglade-pid))))

(defun invalidate-road-section ()
  (setf *road-section* nil)
  (save-road-section))

(defun invalidate-road-section-selection ()
  (setf *road-section-selection* '())
  (save-road-section-selection))

(defun invalidate-road-network-chart-configuration ()
  (setf *road-network-chart-configuration* nil)
  (save-place *road-network-chart-configuration* 'road-network-chart-configuration))

(defun invalidate-zeb-chart-configuration ()
  (setf *zeb-chart-configuration* nil)
  (save-place *zeb-chart-configuration* 'zeb-chart-configuration))

(defun message-name= (string message)
  (let ((colon-position (position #\: message)))
    (string= string (subseq message 0 colon-position))))

(defun message-info (message)
  (let ((colon-position (position #\: message))
        (space-position (position #\Space message)))
    (subseq message (1+ colon-position) space-position)))

(defun message-data (message)
  (let ((space-position (position #\Space message)))
    (when space-position
      (subseq message (1+ space-position)))))

(defun message-data-list (message)
  (cl-utilities:split-sequence #\Space (message-data message)))

(defun collect-road-section-select (message)
  (let ((data (message-data-list message)))
    (if (string= (second data) "4")                 ;"select" column
        (setf (gethash (parse-integer (first data)) ;row number
                       *road-section-raw-data*)
              (string= (third data) "1")))))

(defun collect-accidents-message-data (&key (renderp 0 renderp-p) (from nil from-p) (to nil to-p) (ok-pressed nil ok-pressed-p))
  (when renderp-p (setf (first *accidents-chart-raw-data*) renderp))
  (when from-p (setf (second *accidents-chart-raw-data*) (parse-integer from :junk-allowed t)))
  (when to-p (setf (third *accidents-chart-raw-data*) (parse-integer to :junk-allowed t))))

(defun collect-raw-message (message place)
  (unless (string= (message-info message) "clicked")
    (let ((data (message-data-list message)))
      (setf (gethash (list (parse-integer (first data))   ;row number
                           (parse-integer (second data))) ;column number
                     place)
            (third data)))))

(defun digest-road-section-raw-data ()
  (when (and *postgresql-road-network-credentials* *postgresql-road-network-ok*)
    (let ((sections (sections (make-symbol (db-credentials-table *postgresql-road-network-credentials*)))))
      (maphash (lambda (key value)
                 (if value
                     (pushnew key *road-section-selection*)
                     (setf *road-section-selection* (remove key *road-section-selection*))))
               *road-section-raw-data*)
      (setf *road-section-selection* (sort *road-section-selection* #'<))
      (save-road-section-selection)
      (set-road-section :direction :first)
      ;; (save-road-section)
    
      ;; (when (update-station)            ;new section
      ;;   (restore-road-section-image-counts)
      ;;   (prepare-chart))
      (clrhash *road-section-raw-data*))))

(defstruct (data-style (:type list)) chartp drawablep textp name color width dash)

(defun clear-main-window ()
  (dolist (drawingarea '("chart_accidents" "chart_road_network" "chart_zeb" "chart_cursor" "chart_road_network_scale" "chart_zeb_scale" "draw_rearview" "draw_frontview"))
    (pipeglade-out drawingarea "remove" 2))
  (dolist (image '("img_rearview" "img_frontview"))
    (pipeglade-out image "set_from_file"))
  (pipeglade-out "text_values" "clear"))

(defun digest-chart-raw-data (raw-data)
  "Return the information read from raw-data in chart configuration format."
  (let* ((row-count
          (loop
             for (row column) being each hash-key of raw-data
             when (zerop column) ;arbitrary column representing its row
             count it))
         (chart-configuration
          (make-array (list row-count))))
    (loop
       for i from 0 below row-count
       do
         (setf (svref chart-configuration i)
               (make-data-style)))
    (loop
       for (row column) being each hash-key of raw-data using (hash-value value)
       do
         (case column
           (0                     ;column name
            (setf (data-style-name (svref chart-configuration row)) value))
           ;; 1 would be type
           (2                     ;width
            (setf (data-style-width (svref chart-configuration row)) value))
           (3                     ;color
            (setf (data-style-color (svref chart-configuration row)) value))
           (4                     ;dash
            (setf (data-style-dash (svref chart-configuration row)) value))
           (5                     ;selected
            (setf (data-style-chartp (svref chart-configuration row)) (string= value "1")))
           (6
            (setf (data-style-textp (svref chart-configuration row)) (string= value "1")))
           (7
            (setf (data-style-drawablep (svref chart-configuration row)) (string= value "1")))))
    chart-configuration))

(defun digest-accidents-chart-raw-data ()
  (setf *accidents-chart-configuration*
        (mapcar (lambda (configuration-value raw-value)
                  (or (format nil "~D" (parse-integer raw-value :junk-allowed t)) configuration-value))
                *accidents-chart-configuration*
                *accidents-chart-raw-data*))
  (save-accidents-chart-configuration))

(defun road-network-chart-data (column vnk nnk chart-height)
  "Return a list of lists of station and column values between vnk
and nnk scaled into chart-height; the minimum column value; and the
maximum column value.  Both minimum and maximum are nil if data is
constant."
  (let ((table (intern (db-credentials-table *postgresql-road-network-credentials*))))
    (with-connection *postgresql-road-network-credentials*
      (setf column (intern (string-upcase column)))
      (destructuring-bind (minimum maximum)
          (query (:select (:type (:min column) real)
                          (:type (:max column) real)
                          :from table
                          :where (:and (:= 'vnk vnk)
                                       (:= 'nnk nnk)))
                 :list)
        (if (and (numberp minimum) (numberp maximum))
            (let* ((span (- maximum minimum))
                   (m (if (zerop span)
                          0
                          (/ chart-height span)))
                   (b (if (zerop span)
                          (* chart-height 1/2)
                          (+ chart-height (* m minimum)))))
              (values
               (query (:order-by
                       (:select 'nk-station
                                (:- b (:* m (:type column real)))
                                :from table
                                :where (:and (:= 'vnk vnk)
                                             (:= 'nnk nnk)))
                       'nk-station))
               ;; (unless (zerop span) minimum)
               ;; (unless (zerop span) maximum)
               minimum
               maximum
               ))
            (values nil nil nil))))))

(defun zeb-chart-data (column vnk nnk chart-height)
  "Return a list of lists of station and column values between vnk
and nnk scaled into chart-height; the minimum column value; and the
maximum column value.  Both minimum and maximum are nil if data is
constant."
  (let ((table (intern (db-credentials-table *postgresql-zeb-credentials*))))
    (with-connection *postgresql-zeb-credentials*
      (setf column (intern (string-upcase column)))
      (destructuring-bind (minimum maximum)
          (query (:select (:type (:min column) real)
                          (:type (:max column) real)
                          :from table
                          :where (:and (:= 'vnk vnk)
                                       (:= 'nnk nnk)))
                 :list)
        (if (and (numberp minimum) (numberp maximum))
            (let* ((span (- maximum minimum))
                   (m (if (zerop span)
                          0
                          (/ chart-height span)))
                   (b (if (zerop span)
                          (* chart-height 1/2)
                          (+ chart-height (* m minimum)))))
              (values
               (query (:order-by
                       (:select 'vst
                                (:- b (:* m (:type column real)))
                                'bst
                                (:- b (:* m (:type column real)))
                                :from table
                                :where (:and (:= 'vnk vnk)
                                             (:= 'nnk nnk)))
                       'vst))
               ;; (unless (zerop span) minimum)
               ;; (unless (zerop span) maximum)
               minimum
               maximum
               ))
            (values nil nil nil))))))



(defun road-network-text-value (column vnk nnk station)
  "Return column value at station between vnk and nnk."
  (when *postgresql-road-network-ok*
    (let ((table (intern (db-credentials-table *postgresql-road-network-credentials*))))
      (with-connection *postgresql-road-network-credentials*
        (setf column (intern (string-upcase column)))
        (query (:select column
                        :from table
                        :where (:and (:= 'vnk vnk)
                                     (:= 'nnk nnk)
                                     (:= 'nk_station station)))
               :single)))))

(defun zeb-text-value (column vnk nnk station)
  "Return column value at station between vnk and nnk."
  (when *postgresql-zeb-ok*
    (let ((table (intern (db-credentials-table *postgresql-zeb-credentials*))))
      (with-connection *postgresql-zeb-credentials*
        (setf column (intern (string-upcase column)))
        (query (:select column
                        :from table
                        :where (:and (:= 'vnk vnk)
                                     (:= 'nnk nnk)
                                     (:between station 'vst 'bst)))
               :single)))))

(defun show-text (row-number station text-data-function column vnk nnk color width dash)
  (let ((value (funcall text-data-function column vnk nnk station)))
    (pipeglade-out "text_values" "set" row-number 0 column)
    (pipeglade-out "text_values" "set" row-number 1 value)
    (pipeglade-out "text_values" "set" row-number 2 color)
    (pipeglade-out "text_values" "set" row-number 3 (* 4 (parse-integer width :junk-allowed t))))) ;text size

(defun put-text-values (vnk nnk station)
  (let ((row-number 0))
    (when (vectorp *road-network-chart-configuration*)
      (loop
         for style-definition across *road-network-chart-configuration*
         do
           (when (data-style-textp style-definition)
             (show-text row-number station #'road-network-text-value (data-style-name style-definition) vnk nnk (data-style-color style-definition) (data-style-width style-definition) (data-style-dash style-definition))
             (incf row-number))))
    (when (vectorp *zeb-chart-configuration*)
      (loop
         for style-definition across *zeb-chart-configuration*
         do
           (when (data-style-textp style-definition)
             (show-text row-number station #'zeb-text-value (data-style-name style-definition) vnk nnk (data-style-color style-definition) (data-style-width style-definition) (data-style-dash style-definition))
             (incf row-number))))))

(defun accidents-data (vnk nnk &key
                                 (year-min most-negative-fixnum)
                                 (year-max most-positive-fixnum))
  "Return a list of plists containing accident data for the road
section between vnk and nnk."
  (when *postgresql-accidents-ok*
    (let ((table (intern (db-credentials-table *postgresql-accidents-credentials*))))
      (with-connection *postgresql-accidents-credentials*
        (query (:order-by
                (:select 'nk-station 'fahrtrichtung 'unfalltyp 'unfallkategorie 'alkohol
                         :from table
                         :where (:and (:= 'vnk vnk)
                                      (:= 'nnk nnk)
                                      (:between 'jahr year-min year-max)))
                'nk-station 'jahr 'monat 'tag 'stunde 'minuten)
               :plists)))))

(defun populate-road-section-dialog ()
  (when *postgresql-road-network-ok*
    (with-statusbar-message "populating road section list"
      (with-spinner "road_section_spinner"
        (pipeglade-out "road_sections" "clear")
        (with-connection *postgresql-road-network-credentials*
          (let ((sections (sections (make-symbol (db-credentials-table *postgresql-road-network-credentials*)))))
            (loop
               for (vnk nnk length) in sections
               for row-number from 0
               do
                 (add-vnk-nnk-leaf vnk nnk length row-number))))))))

(defun update-credentials-dialog ()
  (with-statusbar-message "initialising credentials"
    (pipeglade-out "road_network_host" "set_text" (db-credentials-host *postgresql-road-network-credentials*))
    (pipeglade-out "road_network_port" "set_text" (db-credentials-port *postgresql-road-network-credentials*))
    (pipeglade-out "road_network_ssl" "set_active" (if (eq (db-credentials-ssl *postgresql-road-network-credentials*) :no) 0 1))
    (pipeglade-out "road_network_database" "set_text" (db-credentials-database *postgresql-road-network-credentials*))
    (pipeglade-out "road_network_table" "set_text" (db-credentials-table *postgresql-road-network-credentials*))
    (pipeglade-out "road_network_user" "set_text" (db-credentials-user *postgresql-road-network-credentials*))
    (pipeglade-out "road_network_password" "set_text" (db-credentials-password *postgresql-road-network-credentials*))
    (pipeglade-out "road_network_status" "set_text" "?")
    (pipeglade-out "zeb_host" "set_text" (db-credentials-host *postgresql-zeb-credentials*))
    (pipeglade-out "zeb_port" "set_text" (db-credentials-port *postgresql-zeb-credentials*))
    (pipeglade-out "zeb_ssl" "set_active" (if (eq (db-credentials-ssl *postgresql-zeb-credentials*) :no) 0 1))
    (pipeglade-out "zeb_database" "set_text" (db-credentials-database *postgresql-zeb-credentials*))
    (pipeglade-out "zeb_table" "set_text" (db-credentials-table *postgresql-zeb-credentials*))
    (pipeglade-out "zeb_user" "set_text" (db-credentials-user *postgresql-zeb-credentials*))
    (pipeglade-out "zeb_password" "set_text" (db-credentials-password *postgresql-zeb-credentials*))
    (pipeglade-out "zeb_status" "set_text" "?")
    (pipeglade-out "accidents_host" "set_text" (db-credentials-host *postgresql-accidents-credentials*))
    (pipeglade-out "accidents_port" "set_text" (db-credentials-port *postgresql-accidents-credentials*))
    (pipeglade-out "accidents_ssl" "set_active" (if (eq (db-credentials-ssl *postgresql-accidents-credentials*) :no) 0 1))
    (pipeglade-out "accidents_database" "set_text" (db-credentials-database *postgresql-accidents-credentials*))
    (pipeglade-out "accidents_table" "set_text" (db-credentials-table *postgresql-accidents-credentials*))
    (pipeglade-out "accidents_user" "set_text" (db-credentials-user *postgresql-accidents-credentials*))
    (pipeglade-out "accidents_password" "set_text" (db-credentials-password *postgresql-accidents-credentials*))
    (pipeglade-out "accidents_status" "set_text" "?")
    (when *phoros-credentials*
      (destructuring-bind (user password) *phoros-credentials*
        (pipeglade-out "phoros_url" "set_text" *phoros-url*)
        (pipeglade-out "phoros_user" "set_text" user)
        (pipeglade-out "phoros_password" "set_text" password)
        (pipeglade-out "phoros_status" "set_text" "?")))))

(defun check-credentials-dialog-statuses ()
  (with-statusbar-message "checking road network db connection"
    (multiple-value-bind (message successp) (check-db *postgresql-road-network-credentials*)
      (pipeglade-out "road_network_status" "set_text" message)
      (setf *postgresql-road-network-ok* successp)))
  (with-statusbar-message "checking zeb db connection"
    (multiple-value-bind (message successp) (check-db *postgresql-zeb-credentials*)
      (pipeglade-out "zeb_status" "set_text" message)
      (setf *postgresql-zeb-ok* successp)))
  (with-statusbar-message "checking accidents db connection"
    (multiple-value-bind (message successp) (check-db *postgresql-accidents-credentials*)
      (pipeglade-out "accidents_status" "set_text" message)
      (setf *postgresql-accidents-ok* successp)))
  (with-statusbar-message "checking Phoros connection"
    (pipeglade-out "phoros_status" "set_text" (and *phoros-url*
                                                   *phoros-credentials*
                                                   (apply #'check-phoros *phoros-url* *phoros-credentials*)))))

(defun save-place (place filename-stump)
  "Save place into a file whose name is based on symbol filename-stump."
  (let ((cache-file-name (cache-file-name filename-stump)))
    (ensure-directories-exist cache-file-name)
    (with-open-file (stream cache-file-name
                            :direction :output
                            :if-exists :supersede)
      (prin1 place stream))))

(defmacro restore-place (place filename-stump &optional default)
  "Restore place from a file whose name is based on symbol filename-stump."
  (cl-utilities:with-unique-names (stream)
    `(with-open-file (stream (cache-file-name ,filename-stump)
			     :direction :input
			     :if-does-not-exist nil)
       (if stream
	   (setf ,place (read stream))
	   (setf ,place ,default)))))


(defun save-road-section-selection ()
  "Save the list of road sections selected for processing."
  (save-place *road-section-selection* 'road-section-selection))

(defun restore-road-section-selection ()
  (restore-place *road-section-selection* 'road-section-selection))

(defun update-road-section-selection ()
  (when *postgresql-road-network-ok*
    (with-statusbar-message "restoring road section selection"
      (with-spinner "road_section_spinner"
        (with-connection *postgresql-road-network-credentials*
          (let ((sections (sections (make-symbol (db-credentials-table *postgresql-road-network-credentials*)))))
            (loop
               for row-number from 0 below (length sections)
               do
                 (if (find row-number *road-section-selection*)
                     (pipeglade-out "road_sections" "set" row-number 4 1)
                     (pipeglade-out "road_sections" "set" row-number 4 0))))))
      (pipeglade-out "road_sections" "scroll"
                     (or (ignore-errors (apply #'min *road-section-selection*))
                         0)
                     0))))

(defun restore-road-section-image-counts ()
  (when *postgresql-road-network-ok*
    (with-statusbar-message "restoring road section image counts"
      (with-connection *postgresql-road-network-credentials*
        (let* ((table (make-symbol (db-credentials-table *postgresql-road-network-credentials*)))
               (sections (sections table)))
          (loop
             for (vnk nnk) in sections
             for row-number from 0
             do (multiple-value-bind (rearview-image-data rearview-cached-p)
                    (road-section-image-data (provenience-string *phoros-url*) table vnk nnk 10 t :from-cache-only t)
                  (multiple-value-bind (frontview-image-data frontview-cached-p)
                      (road-section-image-data (provenience-string *phoros-url*) table vnk nnk 10 nil :from-cache-only t)
                    (when (and rearview-cached-p frontview-cached-p)
                      (pipeglade-out "road_sections" "set" row-number 3 (+ (length rearview-image-data) (length frontview-image-data))))))))))))

(defun save-road-network-credentials (modifiedp)
  (setf (db-credentials-modifiedp *postgresql-road-network-credentials*) modifiedp)
  (save-place *postgresql-road-network-credentials* 'road-network-credentials))

(defun restore-road-network-credentials ()
  (restore-place *postgresql-road-network-credentials* 'road-network-credentials *postgresql-road-network-credentials*))

(defun save-zeb-credentials (modifiedp)
  (setf (db-credentials-modifiedp *postgresql-zeb-credentials*) modifiedp)
  (save-place *postgresql-zeb-credentials* 'zeb-credentials))

(defun restore-zeb-credentials ()
  (restore-place *postgresql-zeb-credentials* 'zeb-credentials *postgresql-zeb-credentials*))

(defun save-accidents-credentials (modifiedp)
  (setf (db-credentials-modifiedp *postgresql-accidents-credentials*) modifiedp)
  (save-place *postgresql-accidents-credentials* 'accidents-credentials))

(defun restore-accidents-credentials ()
  (restore-place *postgresql-accidents-credentials* 'accidents-credentials *postgresql-accidents-credentials*))

(defun save-phoros-credentials ()
  (save-place *phoros-credentials* 'phoros-credentials)
  (save-place *phoros-url* 'phoros-url))

(defun restore-phoros-credentials ()
  (restore-place *phoros-credentials* 'phoros-credentials *phoros-credentials*)
  (restore-place *phoros-url* 'phoros-url *phoros-url*))

(defun save-road-section ()
  "Save road-section into cache directory."
  (save-place *road-section* 'road-section))

(defun restore-road-section ()
  (restore-place *road-section* 'road-section))

(defun save-accidents-chart-configuration ()
  (save-place *accidents-chart-configuration* 'accidents-chart-configuration))

(defun saved-station ()
  (let ((cache-file-name (cache-file-name 'station))
        station)
    (ensure-directories-exist cache-file-name)
    (with-open-file (stream cache-file-name
                            :direction :input
                            :if-does-not-exist nil)
      (when stream (setf station (read stream)))
      (or station 0))))

(defun restore-road-network-chart-configuration ()
  (unless (db-credentials-modifiedp *postgresql-road-network-credentials*)
    (restore-place *road-network-chart-configuration* 'road-network-chart-configuration)))

(defun restore-zeb-chart-configuration ()
  (unless (db-credentials-modifiedp *postgresql-zeb-credentials*)
    (restore-place *zeb-chart-configuration* 'zeb-chart-configuration)))

(defun restore-accidents-chart-configuration ()
  (unless (db-credentials-modifiedp *postgresql-accidents-credentials*)
    (restore-place *accidents-chart-configuration* 'accidents-chart-configuration (list "1" "1999" "2030"))))

(defun set-road-section (&key direction)
  (let* ((table (make-symbol (db-credentials-table *postgresql-road-network-credentials*)))
         (sections (sections table))
         (sections-current (position (cdr *road-section*) sections :test #'equal))
         (selection-current (position sections-current *road-section-selection*)))
    (cond ((and *road-section-selection* (eq direction :predecessor))
           (let ((selection-predecessor (ignore-errors (nth (1- selection-current) *road-section-selection*))))
             (when selection-predecessor
               (setf *road-section*
                     (cons table (nth selection-predecessor sections)))
               (save-road-section))))
          ((and *road-section-selection* (eq direction :successor))
           (let* ((selection-successor (nth (1+ selection-current) *road-section-selection*)))
             (when selection-successor
               (setf *road-section*
                     (cons table (nth selection-successor sections)))
               (save-road-section))))
          ((and *road-section-selection* (eq direction :last))
           (setf *road-section* (cons table
                                      (nth (car (last *road-section-selection*)) sections)))
           (save-road-section))
          ((and *road-section-selection* (eq direction :first))
           (setf *road-section* (cons table
                                      (nth (first *road-section-selection*) sections)))
           (save-road-section))
          ((not *road-section-selection*)
           (setf *road-section* nil))
          (t
           (error "impossible road section")))))

(defun update-station (station)
  "Change station widget in UI."
  (when (numberp station)
    (pipeglade-out "station_scale" "set_value" station)))

(let ((old-road-section nil))
  (defun jump-to-station (station)
    (cond ((not *road-section*)
           nil)
          ((not old-road-section)
           (setf old-road-section *road-section*)
           (setf *station* station)
           (save-place *station* 'station))
          ((not (and (equal (cdr *road-section*) (cdr old-road-section))
                     (equal (string (car *road-section*)) (string (car *road-section*))))) ;comparing uninterned symbols
           (setf old-road-section *road-section*)
           (setf *station* 0)     ;picked up by jump-to-station-worker
           (save-place *station* 'station))
          (t
           (setf *station* station) ;picked up by jump-to-station-worker
           (save-place *station* 'station)))))

(defun jump-to-station-worker ()
  (let ((current-station)
        (current-road-section))
    (loop
       (if (and (eql current-station *station*)
                (equal current-road-section *road-section*))
           (bt:thread-yield)
           (progn
             (psetf current-station *station*
                    current-road-section *road-section*)
             (handler-case
                 (destructuring-bind (table vnk nnk road-section-length)
                     current-road-section
                   (pipeglade-out "station" "set_text" current-station)
                   (place-chart-cursor current-station)
                   (put-image :vnk vnk :nnk nnk :station current-station :step 10 :rear-view-p t)
                   (put-image :vnk vnk :nnk nnk :station current-station :step 10 :rear-view-p nil)
                   (put-text-values vnk nnk current-station))
               (database-connection-error ())))))))

(defun check-db (db-credentials &aux result)
  "Check database connection and presence of table or view table-name.
Return a string describing the outcome."
  (let ((table-name (db-credentials-table db-credentials)))
    (handler-case
        (trivial-timeout:with-timeout (3)
          (with-connection db-credentials
            (if (or (table-exists-p table-name)
                    (view-exists-p table-name))
                (setf result (list "ok" t))
                (setf result (list "table or view missing" nil)))))
      (database-connection-error (e) (setf result (list e nil)))
      (cl+ssl:ssl-error-verify (e) (setf result (list e nil)))
      (sb-bsd-sockets:name-service-error (e) (setf result (list e nil)))
      (trivial-timeout:timeout-error () (setf result (list "timeout" nil))))
    (values-list result)))

(defun check-phoros (url user-name password)
  "Check connection to phoros server.  Return a string describing the
outcome."
  (let ((*phoros-url* url)
        (*phoros-cookies* nil))
    (unwind-protect
         (handler-case (phoros-login url user-name password)
           (usocket:ns-host-not-found-error () "host not found")
           (usocket:connection-refused-error () "connection refused")
           (error (c) (format nil "~A" c))
           (:no-error (result) (if result "ok" "wrong user or password")))
      (phoros-logout))))

(defun populate-chart-dialog ()
  (with-statusbar-message "initialising chart configuration"
    (when *postgresql-road-network-ok*
      (update-chart-dialog-treeview "road_network" *postgresql-road-network-credentials* *road-network-chart-configuration*))
    (when *postgresql-zeb-ok*
      (update-chart-dialog-treeview "zeb" *postgresql-zeb-credentials* *zeb-chart-configuration*))
    (when *postgresql-accidents-ok*
      (update-accidents-chart-dialog))))


(defun update-chart-dialog ()
  (with-statusbar-message "updating chart configuration"
    (when (and (db-credentials-modifiedp *postgresql-road-network-credentials*)
               *postgresql-road-network-ok*)
      (update-chart-dialog-treeview "road_network" *postgresql-road-network-credentials* *road-network-chart-configuration*)
      (save-road-network-credentials nil))
    (when (and (db-credentials-modifiedp *postgresql-zeb-credentials*)
               *postgresql-road-network-ok*)
      (update-chart-dialog-treeview "zeb" *postgresql-zeb-credentials* *zeb-chart-configuration*)
      (save-zeb-credentials nil))
    (when (and (db-credentials-modifiedp *postgresql-accidents-credentials*)
               *postgresql-accidents-ok*)
      (update-accidents-chart-dialog)
      (save-accidents-credentials nil))))

(defun update-chart-dialog-treeview (treeview db-credentials chart-configuration)
  (with-statusbar-message "updating treeview configuration"
    (handler-case
      (with-connection db-credentials
        (present-db-columns (table-description (db-credentials-table db-credentials)) treeview chart-configuration))
      (database-connection-error ()))))

(defun update-accidents-chart-dialog ()
  (pipeglade-out "render_accidents" "set_active" (first *accidents-chart-configuration*))
  (pipeglade-out "accidents_from" "set_text" (second *accidents-chart-configuration*))
  (pipeglade-out "accidents_to" "set_text" (third *accidents-chart-configuration*)))

(defun present-db-columns (columns treeview chart-configuration)
  (pipeglade-out treeview "clear")
  (loop
     for (column-name type) in (sort columns #'string-lessp :key #'car)
     for row-number from 0
     do
       (let ((selected-column (find column-name chart-configuration :key #'data-style-name :test #'string-equal))
             (drawablep (numeric-type-p type)))
         (pipeglade-out treeview "set" row-number 0 column-name)
         (pipeglade-out treeview "set" row-number 1 type)
         (pipeglade-out treeview "set" row-number 2 (or (data-style-width selected-column) 2))
         (pipeglade-out treeview "set" row-number 3 (or (data-style-color selected-column) "black"))
         (pipeglade-out treeview "set" row-number 4 (or (data-style-dash selected-column) ""))
         (pipeglade-out treeview "set" row-number 5 (if (and drawablep (data-style-chartp selected-column)) 1 0))
         (pipeglade-out treeview "set" row-number 6 (if (data-style-textp selected-column) 1 0))
         (pipeglade-out treeview "set" row-number 7 (if drawablep 1 0))
         (pipeglade-out treeview "set_cursor" row-number))) ;tickle initial pipeglade output
  (pipeglade-out treeview "set_cursor")
  (pipeglade-out treeview "scroll" 0 0))

(defun numeric-type-p (type)
  (some #'identity (mapcar (lambda (x) (search x type))
                           '("float" "double" "int" "numeric" "serial"))))

(defun add-vnk-nnk-leaf (vnk nnk length row-number)
  "Put a leaf into road-sections tree."
  (pipeglade-out "road_sections" "set" row-number 0 vnk)
  (pipeglade-out "road_sections" "set" row-number 1 nnk)
  (pipeglade-out "road_sections" "set" row-number 2 length))

(defun prepare-chart ()
  "Prepare chart for the road section between vnk and nnk in table in
current database."
  (when *road-section*
    (destructuring-bind (table vnk nnk road-section-length) *road-section*
      (pipeglade-out "ovl_chart" "set_size_request" (+ *chart-tail* road-section-length) (+ *chart-height* *chart-fringe*))
      (pipeglade-out "vnk" "set_text" vnk)
      (pipeglade-out "nnk" "set_text" nnk)
      (pipeglade-out "length" "set_text" road-section-length)
      (draw-chart-cursor-scale road-section-length)
      (pipeglade-out "station_scale" "set_range" 0 road-section-length)
      ;; (setf *road-section* (list table vnk nnk road-section-length))
      ;; (save-road-section)
      (draw-graphs vnk nnk)
      (update-station (saved-station))
      ;; (pipeglade-out "station_scale" "set_value" (saved-station))
      )))

(defun place-chart-cursor (station)
  "Move chart cursor to station."
  (when station
    (pipeglade-out "chart_cursor" "remove" 2)
    (pipeglade-out "chart_cursor" "move_to" 2 station 0)
    (pipeglade-out "chart_cursor" "line_to" 2 station (+ *chart-height* *chart-fringe*))
    (pipeglade-out "chart_cursor" "stroke" 2)
    (pipeglade-out "chart_scroll" "hscroll_to_range" (- station 200) (+ station 200))
    (pipeglade-out "chart_road_network_scale" "translate" "=3" station 0)
    (pipeglade-out "chart_zeb_scale" "translate" "=3" station 0)))

(defun refresh-chart ()
  "Redraw chart."
  (when (= (length *road-section*) 4)
    (prepare-chart)))

(defun draw-graphs (vnk nnk)
  "Draw graphs for the columns in *zeb-chart-configuration* and
*road-network-chart-configuration*.  Delete existing graphs first."
  (with-statusbar-message "drawing chart"
    (with-spinner "chart_spinner"
      (pipeglade-out "chart_road_network" "remove" 2)
      (pipeglade-out "chart_road_network_scale" "remove" 2)
      (pipeglade-out "chart_road_network_scale" "translate" "=3" 0 0)
      (pipeglade-out "chart_zeb" "remove" 2)
      (pipeglade-out "chart_zeb_scale" "remove" 2)
      (pipeglade-out "chart_zeb_scale" "translate" "=3" 0 0)
      (let ((scale-position *scale-distance*))
        (with-statusbar-message "drawing road-network chart"
          (when (vectorp *road-network-chart-configuration*)
            (loop
               for style-definition across *road-network-chart-configuration*
               do
                 (when (data-style-chartp style-definition)
                   (handler-case
                       (progn
                         (draw-graph #'road-network-chart-data "chart_road_network" (data-style-name style-definition) vnk nnk (data-style-color style-definition) (data-style-width style-definition) (data-style-dash style-definition))
                         (draw-scale scale-position #'road-network-chart-data "chart_road_network_scale" (data-style-name style-definition) vnk nnk (data-style-color style-definition) (data-style-width style-definition) (data-style-dash style-definition))
                         (incf scale-position *scale-distance*))
                     (database-error (e) (format t "(draw-graphs), road-network: ~A~%" e)))))))
        (with-statusbar-message "drawing zeb chart"
          (when (vectorp *zeb-chart-configuration*)
            (loop
               for style-definition across *zeb-chart-configuration*
               do
                 (when (data-style-chartp style-definition)
                   (handler-case
                       (progn
                         (draw-graph #'zeb-chart-data "chart_zeb" (data-style-name style-definition) vnk nnk (data-style-color style-definition) (data-style-width style-definition) (data-style-dash style-definition))
                         (draw-scale scale-position #'zeb-chart-data "chart_zeb_scale" (data-style-name style-definition) vnk nnk (data-style-color style-definition) (data-style-width style-definition) (data-style-dash style-definition))
                         (incf scale-position *scale-distance*))
                     (database-error (e) (format t "(draw-graphs), zeb: ~A~%" e))))))))
      (pipeglade-out "chart_accidents" "remove" 2)
      (handler-case
          (progn
            (draw-accidents vnk nnk))
        (database-error (e) (format t "(draw-graphs), accidents: ~A~%" e))))))

(defun draw-graph (chart-data-function chart column vnk nnk color width dash)
  (multiple-value-bind (line minimum maximum)
      (funcall chart-data-function column vnk nnk *chart-height*)
    (let ((line-fragments
           (cl-utilities:split-sequence-if #'(lambda (x)
                                               (eq (second x) :null))
                                           line
                                           :remove-empty-subseqs t)))
      (pipeglade-out chart "set_source_rgba" 2 color)
      (pipeglade-out chart "set_line_width" 2 width)
      (pipeglade-out chart "set_dash" 2 dash)
      (dolist (line-fragment line-fragments)
        (pipeglade-out chart "move_to" 2 (first (car line-fragment)) (second (car line-fragment)))
        (dolist (line-vertex (cdr line-fragment))
          (pipeglade-out chart "line_to" 2 (first line-vertex) (second line-vertex)))
        (pipeglade-out chart "stroke" 2)))))

(defun draw-scale (position chart-data-function chart column vnk nnk color width dash)
  (multiple-value-bind (line minimum maximum)
      (funcall chart-data-function column vnk nnk *chart-height*)
    (pipeglade-out chart "set_source_rgba" 2 color)
    (pipeglade-out chart "set_line_width" 2 width)
    (pipeglade-out chart "move_to" 2 position 0)
    (pipeglade-out chart "line_to" 2 position *chart-height*)
    (dolist (tick (axis-ticks minimum maximum 5 *chart-height* t))
      (pipeglade-out chart "move_to" 2 position (format nil "~F" (second tick)))
      (pipeglade-out chart "rel_line_to" 2 (* 2 (parse-integer width)) 0)
      (pipeglade-out chart "move_to" 2 position (format nil "~F" (second tick)))
      (pipeglade-out chart "rel_move_to" 2 (- (parse-integer width)) 0)
      (pipeglade-out chart "rel_move_for" 2 "e" (first tick))
      (pipeglade-out chart "show_text" 2 (first tick)))
    (pipeglade-out chart "move_to" 2 position (format nil "~F" (+ *chart-height* (/ *chart-fringe* 2))))
    (pipeglade-out chart "rel_move_for" 2 "c" column)
    (pipeglade-out chart "show_text" 2 column)
    (pipeglade-out chart "stroke" 2)))

(defun draw-chart-cursor-scale (length)
  (let ((y-position (+ *chart-height* *chart-fringe*))
        (number-of-ticks (round length 100)))
    (pipeglade-out "chart_cursor" "remove" 4)
    (dolist (tick (axis-ticks 0 length number-of-ticks length nil))
      (pipeglade-out "chart_cursor" "move_to" 4 (second tick) y-position)
      (pipeglade-out "chart_cursor" "line_to" 4 (second tick) (- y-position 3))
      (pipeglade-out "chart_cursor" "rel_move_for" 4 "s" (first tick))
      (pipeglade-out "chart_cursor" "show_text" 4 (first tick)))
    (pipeglade-out "chart_cursor" "stroke" 4)))

(defun axis-ticks (minimum maximum n chart-size reversep)
  (let ((range (- maximum minimum)))
    (if (zerop range)
        (list (list (format nil "~F" minimum) (/ chart-size 2)))
        (let* ((a (if reversep
                      (- (/ chart-size range))
                      (/ chart-size range)))
               (b (if reversep
                      (* a maximum)
                      (- (* a maximum) chart-size)))
               (min-step (/ range (1+ n)))
               (max-step (/ range (1- n)))
               (max-exp (log max-step 10))
               (int-exp (floor max-exp))
               (norm-min (floor (/ min-step (expt 10 int-exp))))
               (norm-max (floor (/ max-step (expt 10 int-exp))))
               (norm (cond
                       ((or (= norm-min 1) (= norm-max 1))
                        1)
                       ((or (<= norm-min 4 norm-max) (<= norm-min 5 norm-max) (<= norm-min 6 norm-max))
                        5)
                       ((or (<= norm-min 2) (<= norm-min 3 norm-max))
                        2.5)
                       ((<= 7 norm-max)
                        10)
                       (t
                        norm-max)))     ;can't happen
               (step (* norm (expt 10 int-exp)))
               (start (- minimum (nth-value 1 (fceiling minimum step)))))
          (loop
             for i from start to maximum by step
             collect (list (if (minusp int-exp)
                               (format nil "~,VF" (- int-exp) i)
                               (format nil "~A" (round i)))
                           (- (* a i) b)))))))

(defun draw-accidents (vnk nnk)
  (when (string-equal (first *accidents-chart-configuration*) "1")
    (let* ((year-min (second *accidents-chart-configuration*))
           (year-max (third *accidents-chart-configuration*))
           (accidents (accidents-data vnk nnk :year-min year-min :year-max year-max))
           (current-station -1)
           (zeroth-position -1)
           y1-position
           y2-position)
      (dolist (accident accidents)
        (unless (= current-station (getf accident :nk-station))
          (setf y1-position (- *chart-height* zeroth-position))
          (setf y2-position zeroth-position)
          (setf y0-position (+ (/ *chart-height* 2) zeroth-position)))
        (setf current-station (getf accident :nk-station))
        (cond ((= 1 (getf accident :fahrtrichtung))
               (draw-accident accident (decf y1-position 10)))
              ((= 2 (getf accident :fahrtrichtung))
               (draw-accident accident (incf y2-position 10)))
              (t
               (draw-accident accident (incf y0-position 10))))))))

(defun draw-accident (accident y-position)
  "Put graphical representation of accident on chart."
  (destructuring-bind (&key nk-station fahrtrichtung unfalltyp unfallkategorie alkohol)
      accident
    (when (and (numberp alkohol) (plusp alkohol)) (draw-triangle nk-station y-position "lightblue"))
    (case unfallkategorie
      (1 (draw-rectangle nk-station y-position 10 "black")
         (draw-circle nk-station y-position 8 (accident-type-color unfalltyp)))
      (2 (draw-circle nk-station y-position 8 (accident-type-color unfalltyp)))
      (3 (draw-circle nk-station y-position 6 (accident-type-color unfalltyp)))
      (4 (draw-circle nk-station y-position 6 "white")
         (draw-circle nk-station y-position 4 (accident-type-color unfalltyp)))
      (5 (draw-circle nk-station y-position 4 (accident-type-color unfalltyp)))
      (6 (draw-triangle nk-station y-position "lightblue")
         (draw-circle nk-station y-position 4 (accident-type-color unfalltyp)))
      (t (draw-circle nk-station y-position 4 (accident-type-color unfalltyp))))))

(defun draw-circle (x y diameter color)
  (pipeglade-out "chart_accidents" "set_source_rgba" 2 "black")
  (pipeglade-out "chart_accidents" "arc" 2 x y (/ diameter 2) 0 360)
  (pipeglade-out "chart_accidents" "stroke_preserve" 2)
  (pipeglade-out "chart_accidents" "set_source_rgba" 2 color)
  (pipeglade-out "chart_accidents" "fill" 2))

(defun draw-rectangle (x y diameter color)
  (let ((radius (/ diameter 2)))
    (pipeglade-out "chart_accidents" "set_source_rgba" 2 color)
    (pipeglade-out "chart_accidents" "rectangle" 2 (- x radius) (- y radius) diameter diameter)
    (pipeglade-out "chart_accidents" "fill" 2)))

(defun draw-triangle (x y color)
  (pipeglade-out "chart_accidents" "set_source_rgba" 2 "black")
  (pipeglade-out "chart_accidents" "move_to" 2 (- x 3) (- y 6))
  (pipeglade-out "chart_accidents" "line_to" 2 (+ x 3) (- y 6))
  (pipeglade-out "chart_accidents" "line_to" 2 x (+ y 9))
  (pipeglade-out "chart_accidents" "close_path" 2)
  (pipeglade-out "chart_accidents" "stroke_preserve" 2)
  (pipeglade-out "chart_accidents" "set_source_rgba" 2 color)
  (pipeglade-out "chart_accidents" "fill" 2))

(defun accident-type-color (accident-type)
  (case accident-type
    (1 "green")
    (2 "yellow")
    (3 "red")
    (4 "white")
    (5 "lightblue")
    (6 "orange")
    (7 "black")
    (t "darkblue")))

(defun iso-time (time)
  (when time
    (multiple-value-bind (seconds deciseconds)
        (floor time)
      (multiple-value-bind (second minute hour date month year day daylight-p zone)
          (decode-universal-time seconds)
        (format nil "~D-~2,'0D-~2,'0D\\n~2,'0D:~2,'0D:~2,'0D~3,3FZ" year month date hour minute second deciseconds)))))

(defun image-point-coordinates (image-data-alist global-point-coordinates)
  "Return a list (m n) of image coordinates representing
global-point-coordinates in the image described in image-data-alist
but scaled to fit into *image-size*."
  (ignore-errors
    (convert-image-coordinates
     (photogrammetry :reprojection
                     image-data-alist
                     (pairlis '(:x-global :y-global :z-global)
                              (proj:cs2cs
                               (list
                                (proj:degrees-to-radians
                                 (coordinates-longitude global-point-coordinates))
                                (proj:degrees-to-radians
                                 (coordinates-latitude global-point-coordinates))
                                (coordinates-ellipsoid-height global-point-coordinates))
                               :destination-cs (cdr (assoc :cartesian-system image-data-alist)))))
     image-data-alist)))

(defun in-image-p (m n)
  "Check if m, n lay inside *image-size*."
  (and m n (<= 0 m (first *image-size*)) (<= 0 n (second *image-size*))))

(defun-cached sections (table)
  "Return list of distinct pairs of vnk, nnk found in table in
current database."
  (query (:order-by (:select 'vnk 'nnk (:max 'nk-station)
                               :from (intern (db-credentials-table *postgresql-road-network-credentials*))
                               :where (:and (:not-null 'vnk) (:not-null 'nnk))
                               :group-by 'vnk 'nnk)
                      'vnk 'nnk)))

(defun stations (table vnk nnk &optional (step 1))
  "Return a list of plists of :longitude, :latitude,
:ellipsoid-height, :station, :azimuth of stations step metres apart
between vnk and nnk."
  (when (and table vnk nnk)
    (let ((stations
	   (prog2
	       (with-open-file (s "ttt" :direction :output :if-exists :append :if-does-not-exist :create))
	       (query
		(:order-by
		 (:select (:as (:st_x 't1.the-geom) 'longitude)
			  (:as (:st_y 't1.the-geom) 'latitude)
			  (:as (:st_z 't1.the-geom) 'ellipsoid-height)
			  (:as 't1.nk-station 'station)
			  (:as (:st_azimuth 't1.the-geom 't2.the-geom) 'azimuth)
			  :from (:as table 't1)
			  :left-join (:as table 't2)
			  :on (:and (:= 't1.nk-station (:- 't2.nk-station 1))
				    (:= 't2.vnk vnk)
				    (:= 't2.nnk nnk))
			  :where (:and (:= 't1.vnk vnk)
				       (:= 't1.nnk nnk)
				       (:= 0 (:% 't1.nk-station step))))
		 't1.nk-station)
		:plists)
	     (with-open-file (s "ttt" :direction :output :if-exists :append :if-does-not-exist :create)))))
      (setf
       (getf (nth (- (length stations) 1) stations) :azimuth)
       (getf (nth (- (length stations) 2) stations) :azimuth))
      stations)))

(defun-cached all-stations (table vnk nnk)
  "Return a vector of coordinates of all points between vnk and nnk,
station (in metres) being the vector index."
  (when (and table vnk nnk)
    (let* ((stations (stations table vnk nnk))
           (result (make-array (list (1+ (getf (first (last stations)) :station)))
                               :initial-element nil)))
      (loop
         for i in stations
         do (destructuring-bind (&key longitude latitude ellipsoid-height station azimuth)
                i
              (setf (svref result station)
                    (make-coordinates :longitude longitude
                                      :latitude latitude
                                      :ellipsoid-height ellipsoid-height
                                      :azimuth azimuth))))
      result)))

(defun-cached road-section-image-data (provenience-string table vnk nnk step rear-view-p)
  "Return a list of instances of image data corresponding to stations,
which are step metres apart, found in table in current database.
provenience-string only serves as a marker of the provenience of image
data once cached."
  (remove nil ;; (mapcar #'(lambda (x)
          ;;             (apply #'image-data :rear-view-p rear-view-p x))
          ;;         (stations table vnk nnk step))
          (loop
             with azimuth-fallback = nil
             for station in (stations table vnk nnk step)
             when (not (eq (getf station :azimuth) :null))
             do (setf azimuth-fallback (getf station :azimuth))
             and collect (apply #'image-data :rear-view-p rear-view-p station)
             end
             when (and azimuth-fallback
                       (eq (getf station :azimuth) :null))
             do (setf (getf station :azimuth) azimuth-fallback)
             and collect (apply #'image-data :rear-view-p rear-view-p station))))

(defun provenience-string (url)
  "Turn url recognisably into something suitable as part of a file
name."
  (let ((parsed-url (puri:parse-uri url)))
    (format nil "~A_~A~{_~A~}"
            (puri:uri-host parsed-url)
            (puri:uri-port parsed-url)
            (cl-utilities:split-sequence
             #\/ (puri:uri-path parsed-url) :remove-empty-subseqs t))))


(defun cache-file-name (kind &rest args)
  "Return pathname for a cache file distinguishable by kind and args."
  (make-pathname :directory *cache-dir*
                 :name (format nil "~{~:[f~;~:*~(~A~)~]_~}~S.~S"
                               args
                               (fasttrack-version :major t)
                               (fasttrack-version :minor t))
                 :type (string-downcase kind)))

(defun cache-images (road-section-image-data)
  "Download images described in road-section-image-data into their
canonical places."
  (loop
     for i in road-section-image-data
     do (download-image i)))

(defun get-image-data (road-section-image-data station step)
  "Return image data for the image near station."
  (or (find (* step (round station step)) road-section-image-data
        :key #'image-data-station
        :test #'=)
      *empty-image-data*))

(defun get-image-data-alist (road-section-image-data station step)
  "Return as an alist data for the image near station."
  (image-data-alist (get-image-data road-section-image-data station step)))

(defun image-data (&key longitude latitude ellipsoid-height station azimuth rear-view-p)
  "Get from Phoros server image data for location near longitude,
latitude."
  (handler-case
      (let* ((coordinates (make-coordinates :longitude longitude
                                            :latitude latitude
                                            :ellipsoid-height ellipsoid-height
                                            :azimuth azimuth))
             (image-data (phoros-nearest-image-data coordinates rear-view-p)))
        (when (image-data-p image-data)
          (setf (image-data-station image-data) station)
          (setf (image-data-station-coordinates image-data) coordinates)
          image-data))
    (phoros-server-error (e) (format t "(image-data): ~A" e))))

(define-condition phoros-server-error (error)
  ((body :reader body :initarg :body)
   (status-code :reader status-code :initarg :status-code)
   (headers :reader headers :initarg :headers)
   (url :reader url :initarg :url)
   (reason-phrase :reader reason-phrase :initarg :reason-phrase))
  (:report (lambda (condition stream)
             (format stream "Can't connect to Phoros server: ~A (~D)"
                     (reason-phrase condition) (status-code condition)))))

(defun phoros-lib-url (canonical-url suffix)
  "Replace last path element of canonical-url by lib/<suffix>."
  (let* ((parsed-canonical-url (puri:parse-uri canonical-url))
         (old-path (puri:uri-parsed-path parsed-canonical-url))
         (new-path (append (butlast old-path) (list "lib" suffix)))
         (new-url (puri:copy-uri parsed-canonical-url)))
    (setf (puri:uri-parsed-path new-url) new-path)
    new-url))

(defun phoros-login (url user-name user-password)
  "Log into Phoros server; return T if successful.  Try logging out
first."
  ;; (setf *phoros-url* url)
  (setf drakma:*allow-dotless-cookie-domains-p* t)
  (pushnew (cons "application" "json") drakma:*text-content-types* :test #'equal)
  (phoros-logout)
  (setf *phoros-cookies* (make-instance 'drakma:cookie-jar))
  (multiple-value-bind (body status-code headers url stream must-close reason-phrase)
      (drakma:http-request (puri:parse-uri url) :cookie-jar *phoros-cookies*)
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :url url :reason-phrase reason-phrase)
    (multiple-value-bind (body status-code headers authenticate-url stream must-close reason-phrase)
        (drakma:http-request (phoros-lib-url url "authenticate")
                             :cookie-jar *phoros-cookies*
                             :form-data t
                             :method :post
                             :parameters (pairlis '("user-name" "user-password")
                                                  (list user-name user-password)))
      (declare (ignore stream must-close))
      (assert (< status-code 400) ()
              'phoros-server-error :body body :status-code status-code :headers headers :url authenticate-url :reason-phrase reason-phrase)
      (let ((body-strings (cl-utilities:split-sequence #\Space (substitute-if-not #\Space #'alphanumericp body))))
        (and (not (find "Rejected" body-strings :test #'string=))
             (not (find "Retry" body-strings :test #'string=))
             (= status-code 200))))))   ;should be 302 (?)

(defun phoros-logout ()
  (drakma:http-request (phoros-lib-url *phoros-url* "logout")))

(defun run-phoros-browser ()
  (when *road-section*
    (with-statusbar-message "calling browser synchronously"
      (destructuring-bind (table vnk nnk road-section-length)
          *road-section*
        (let ((current-coordinates (svref (all-stations table vnk nnk) (saved-station))))
          (handler-case
              (uiop:run-program (format nil "firefox '~A/lib/set-cursor?bbox=~F,~F,~F,~F&longitude=~F&latitude=~F'"
                                        *phoros-url*
                                        (- (coordinates-longitude current-coordinates) .02)
                                        (- (coordinates-latitude current-coordinates) .01)
                                        (+ (coordinates-longitude current-coordinates) .02)
                                        (+ (coordinates-latitude current-coordinates) .01)
                                        (coordinates-longitude current-coordinates)
                                        (coordinates-latitude current-coordinates)
                                        ))
            (type-error () nil))
          (uiop:run-program (format nil "firefox '~A'" *phoros-url*)))))))


(defun heading (azimuth rear-view-p)
  "Return as a string the one of east, west, north, south which best
describes azimuth."
  (cond ((<= (* 1/4 pi) azimuth (* 3/4 pi)) (if rear-view-p "west" "east"))
        ((<= (* 3/4 pi) azimuth (* 5/4 pi)) (if rear-view-p "north" "south"))
        ((<= (* 5/4 pi) azimuth (* 7/4 pi)) (if rear-view-p "east" "west"))
        ((or (<= (* 5/4 pi) azimuth pi) (<= 0 (* 1/4 pi))) (if rear-view-p "south" "north"))))

(defun phoros-nearest-image-data (coordinates rear-view-p)
  "Return a set of image-data."
  (multiple-value-bind (body status-code headers url stream must-close reason-phrase)
      (drakma:http-request (phoros-lib-url *phoros-url* "nearest-image-data")
                           :cookie-jar *phoros-cookies*
                           :method :post
                           :content-type "text/plain; charset=UTF-8"
                           :content (json:encode-json-plist-to-string (list :longitude (coordinates-longitude coordinates)
                                                                            :latitude (coordinates-latitude coordinates)
                                                                            :zoom 20
                                                                            :count 1
                                                                            :selected-restriction-ids (vector "Device_21" (heading (coordinates-azimuth coordinates) rear-view-p))))) ;TODO: document requirement for restrictions tagged north, east, south, west, and front_cam; actually use the latter
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :url url :reason-phrase reason-phrase)
    (unless (string-equal body "null")
      (apply #'make-image-data :allow-other-keys t
             (plist-from-alist
              (car (json:decode-json-from-string body)))))))

(defun download-file (url path)
  "Unless already there, store content from url under path.  Return
nil if nothing needed storing."
  (when path
    (ensure-directories-exist path)
    (with-open-file (file-stream path :direction :output
				 :element-type 'unsigned-byte
				 :if-exists nil)
      (when file-stream
        (with-statusbar-message (format nil "downloading ~A" url)
          (multiple-value-bind
                (body status-code headers url stream must-close reason-phrase)
              (drakma:http-request url
                                   :cookie-jar *phoros-cookies*
                                   :method :get)
            (declare (ignore stream must-close))
            (setf *t* url)
            (assert (= status-code 200) ()
                    'phoros-server-error :body body :status-code status-code :headers headers :url url :reason-phrase reason-phrase)
            (write-sequence body file-stream)
            reason-phrase))))))

(defun download-image (image-data)
  "If not already there, download a png image, shrink it, convert it
into jpg, and store it under the cache path.  Return that path."
  (multiple-value-bind (url origin-path destination-path)
      (image-url image-data)
    (when destination-path
      (unless (probe-file destination-path)
	(download-file url origin-path)
	(apply #'convert-image-file origin-path destination-path *image-size*)
	(delete-file origin-path))
      destination-path)))

(defun remember-image-being-launched (image-data image-arrow-coordinates rear-view-p)
  (if rear-view-p
      (if *show-rear-view-p*
          (progn
            (psetf *rear-view-image-data* image-data
                   *rear-view-image-arrow-coordinates* image-arrow-coordinates))
          (progn
            (setf *rear-view-image-data* *empty-image-data*)
            (pipeglade-out "draw_rearview" "remove" 2)
            (pipeglade-out "img_rearview" "set_from_file" "public_html/phoros-logo-background.png")))
      (if *show-front-view-p*
          (progn
            (psetf *front-view-image-data* image-data
                   *front-view-image-arrow-coordinates* image-arrow-coordinates))
          (progn
            (setf *front-view-image-data* *empty-image-data*)
            (pipeglade-out "draw_frontview" "remove" 2)
            (pipeglade-out "img_frontview" "set_from_file" "public_html/phoros-logo-background.png")))))

(defun forget-images-being-launched ()
  (setf *rear-view-image-data* *empty-image-data*)
  (setf *front-view-image-data* *empty-image-data*))

(defun image-data-alist (image-data)
  "Return an alist representation of image-data."
  (when image-data
    (loop
       for i in (append (mapcar #'ensure-hyphen-before-digit *aggregate-view-columns*) '(station station-coordinates))
       collect (intern (string i) 'keyword) into keys
       collect (funcall (intern (concatenate 'string (string 'image-data-)
                                             (string i)))
                        image-data)
       into values
       finally (return (pairlis keys values)))))

(defun plist-from-alist (alist)
  (loop
     for (key . value) in alist
     collect key
     collect value))

(defun image-url (image-data)
  "Return an image URL made from ingredients found in image-data, the
corresponding cache path, and the corresponding cache path for the
shrunk image."
  (when image-data
    (let* ((path
            (format nil "~A/~A/~A/~D.png"
                    (puri:uri-path (phoros-lib-url *phoros-url* "photo"))
                    (image-data-directory image-data)
                    (image-data-filename image-data)
                    (image-data-byte-position image-data)))
           (query
            (format nil "mounting-angle=~D~
                       &bayer-pattern=~{~D~#^,~}~
                       &color-raiser=~{~D~#^,~}"
                    (image-data-mounting-angle image-data)
                    (map 'list #'identity (image-data-bayer-pattern image-data))
                    (map 'list #'identity (image-data-color-raiser image-data))))
           (url (puri:copy-uri (puri:parse-uri *phoros-url*) :path path :query query))
           (host (puri:uri-host url))
           (port (puri:uri-port url))
           (cache-directory (append *cache-dir*
                                    (list (format nil "~A_~D" host port))
                                    (cdr (pathname-directory (puri:uri-path url)))))
           (cache-name (pathname-name (puri:uri-path url)))
           (cache-type (pathname-type (puri:uri-path url))))
      (values url
              (make-pathname :directory cache-directory
                             :name cache-name
                             :type cache-type)
              (make-pathname :directory cache-directory
                             :name cache-name
                             :type "jpg")))))

(defun convert-image-file (origin-file destination-file width height)
  "Convert origin-file into destination-file of a maximum size of
width x height."
  (handler-case
      (lisp-magick-wand:with-magick-wand (wand :load (namestring origin-file))
        (let ((a (/ (lisp-magick-wand:get-image-width wand)
                    (lisp-magick-wand:get-image-height wand))))
          (if (> a (/ width height))
              (lisp-magick-wand:scale-image wand width (truncate (/ width a)))
              (lisp-magick-wand:scale-image wand (truncate (* a height)) height)))
        (lisp-magick-wand:write-image wand (namestring destination-file)))
    (lisp-magick-wand:magick-wand-error ()))) ;ignore

(defun convert-image-coordinates (original-coordinates-alist image-data-alist)
  "Convert image coordinates from original-coordinates-alist for the
image in image-data-alist into a list of coordinates for that image
scaled and centered to *image-size*."
  (let* ((original-m (cdr (assoc :m original-coordinates-alist)))
         (original-n (cdr (assoc :n original-coordinates-alist)))
         (original-width (cdr (assoc :sensor-width-pix image-data-alist)))
         (original-height (cdr (assoc :sensor-height-pix image-data-alist)))
         (new-width (first *image-size*))
         (new-height (second *image-size*))
         (scaling-factor (min (/ new-width original-width) (/ new-height original-height)))
         (new-m-offset (/ (- new-width (* original-width scaling-factor)) 2))
         (new-n-offset (/ (- new-height (* original-height scaling-factor)) 2))
         (new-m (+ (* original-m scaling-factor) new-m-offset))
         (new-n (- new-height           ;flip n
                   (+ (* original-n scaling-factor) new-n-offset))))
    (mapcar #'round (list new-m new-n))))

(defun put-image (&key vnk nnk station step rear-view-p)
  "Put an image along with a labelled station marker on screen."
  (when (and vnk nnk station)
    (with-connection *postgresql-road-network-credentials*
      (setf station (or station 0))
      (let* ((table (make-symbol (db-credentials-table *postgresql-road-network-credentials*)))
             (point-radius 5)
             (image-widget (if rear-view-p "img_rearview" "img_frontview"))
             (drawing-widget (if rear-view-p "draw_rearview" "draw_frontview"))
             (spinner-widget (if rear-view-p "spinner_rearview" "spinner_frontview"))
             (time-widget (if rear-view-p "rear_view_time" "front_view_time"))
             global-point-coordinates
             image-data-alist
             image-data
             image-arrow-coordinates
             global-point-coordinates-thread)
        (setf global-point-coordinates-thread
              (bt:make-thread
               (lambda ()
                 (with-connection *postgresql-road-network-credentials*
                   (setf global-point-coordinates
                         (subseq (all-stations table vnk nnk :message (list vnk nnk))
                                 (min (length (all-stations table vnk nnk)) station)
                                 (min (length (all-stations table vnk nnk)) (+ station 4))))))
               :name "global-point-coordinates"))
        (bt:join-thread global-point-coordinates-thread)
        (setf image-data-alist
              (get-image-data-alist (road-section-image-data (provenience-string *phoros-url*) table vnk nnk step rear-view-p :message (list "get-image-data-alist" vnk nnk (if rear-view-p "rear-view" "front-view")))
                                    station
                                    step))
        (setf image-arrow-coordinates
              (loop
                 for i across global-point-coordinates
                 append (image-point-coordinates image-data-alist i)))
        (setf image-label-coordinates (ignore-errors
                                        (list (- (first image-arrow-coordinates) point-radius)
                                              (- (second image-arrow-coordinates) point-radius))))
        (setf image-data (get-image-data (road-section-image-data (provenience-string *phoros-url*) table vnk nnk step rear-view-p :message (list "image-data" vnk nnk (if rear-view-p "rear-view" "front-view"))) station step))
        (remember-image-being-launched image-data image-arrow-coordinates rear-view-p)))))

(defun cruise-control (&key backwardp)
  (setf *cruise-control-backward-p* backwardp)
  (setf *cruise-control* t))       ;picked up by cruise-control-worker
  
(defun stop-cruise-control ()
  (setf *cruise-control* nil))

(defun cruise-control-worker ()
  (loop
     (let ((road-section-length (fourth *road-section*)))
       (if (and *cruise-control*
                *rear-view-image-done*
                *front-view-image-done*)
           (progn
             (let ((next-station
                    (+ *station* (if *cruise-control-backward-p*
                                     (- *big-step*)
                                     *big-step*))))
               (when (< next-station 0)
                 (setf next-station 0)
                 (stop-cruise-control))
               (when (> next-station road-section-length)
                 (setf next-station road-section-length)
                 (stop-cruise-control))
               (setf *rear-view-image-done* nil)
               (setf *front-view-image-done* nil)
               (sleep .2)
               (update-station next-station)))
           (progn
             (sleep .2)
             (bt:thread-yield))))))
