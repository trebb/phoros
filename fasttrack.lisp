;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2012 Bert Burgemeister
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

;;; Debug helpers.  TODO: remove them.
(defparameter *t* nil)
(defparameter *tt* nil)

(cffi:define-foreign-library phoml
  (:unix (:or "./libphoml.so"
              "./phoml/lib/libphoml.so"))
  (t (:default "libphoml")))

(setf *read-default-float-format* 'double-float)

(defparameter *photogrammetry-mutex* (bt:make-lock "photogrammetry"))

(defparameter *fasttrack-version*
  (asdf:component-version (asdf:find-system :fasttrack))
  "Fasttrack version as defined in system definition.  TODO: enforce equality with *phoros-version*")

(defvar *postgresql-road-network-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

(defvar *postgresql-road-network-table* "phoros_project_aux_point"
  "Name of table or view in database described by
  *postgresql-road-network-credentials*")

(defvar *postgresql-zeb-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

(defvar *postgresql-zeb-table* "zeb"
  "Name of table or view in database described by
  *postgresql-zeb-credentials*")

(defvar *zeb-column-selection* nil
  "Database columns selected for rendering.")

(defvar *accidents-column-selection* nil
  "Database columns selected for rendering.")

(defvar *postgresql-accidents-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

(defvar *postgresql-accidents-table* "unfaelle"
  "Name of table or view in database described by
  *postgresql-accidents-credentials*")

(defvar *chart-parameters* nil
  "If there is a chart, we store a list of its parameters (table vnk
  nnk road-section-length) here.")

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

(defvar *cache-dir* '(:absolute "home" "bertb" "lisphack" "phoros" "cache"))
;; TODO: invent cache validity checks

(defparameter *image-size* '(800 800)
  "Image size in pixels in a list (width height).")

(defparameter *chart-height* 200
  "Height of chart in pixels.")

(defvar *jump-to-station-event* nil
  "Remembering event id of chart click event jumptostation.")

(defvar *choose-road-section-event* nil)

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

(defmacro defun-cached (name (&rest args) &body body &aux (doc ""))
  "Define a function whose return value must be readibly printable, is
  being read from a chache if possible, and is being cached if
  necessary.  The function defined has a secondary return value
  cached-p.  If function is called with :from-cache-only t, let it
  return nil and nil if there is nothing cached."
  (when (stringp (car body))
    (setf doc (car body))
    (setf body (cdr body)))
  (cl-utilities:with-unique-names (input-stream output-stream)
    `(defun ,name (,@args &key from-cache-only)
       ,doc
       (ensure-directories-exist (cache-file-name ',name ,@args))
       (with-open-file (,input-stream (cache-file-name ',name ,@args)
                                      :direction :input
                                      :if-does-not-exist nil)
         (if ,input-stream
             (values (read ,input-stream) t)
             (values (unless from-cache-only
                       (with-open-file (,output-stream (cache-file-name ',name ,@args)
                                                       :direction :output)
                         (prin1 (progn ,@body)
                                ,output-stream)))
                     nil))))))

(defun main ()
  (restore-credentials)
  (restore-column-selection)
  (apply #'phoros-login *phoros-url* *phoros-credentials*)
  (with-tk ((make-instance 'ffi-tk))
    (tcl "package" "require" "Img")
    (tcl "option" "add" "*tearOff" 0)
    (tcl "wm" "title" "." "Phoros Fasttrack")
    (tcl "menu" ".menubar")
    (tcl "." "configure" :menu ".menubar")
    (tcl "menu" ".menubar.file")
    (tcl ".menubar" "add" "cascade" :label "File" :menu ".menubar.file" :underline 0)
    (tcl ".menubar.file" "add" "command" :label "Kaputt" :command (tcl{ "destroy" "."))
    (tcl ".menubar.file" "add" "command" :label "choose road section ..." :command (event-handler* (road-section-dialog)))
    (tcl ".menubar.file" "add" "command" :label "server credentials ..." :command (event-handler* (credentials-dialog)))
    (tcl ".menubar.file" "add" "command" :label "chart configuration ..." :command (event-handler* (chart-dialog)))
    (tcl ".menubar.file" "add" "command" :label "Do Stuff" :command (event-handler* (print "doing stuff") (print "doing more stuff") (tcl "set" "feet" 500)))

    (bind-event "." "<<check.blah>>" ((ddd #\d)) (print (list "ddd" ddd)))
    (tcl ".menubar.file" "add" "checkbutton" :label "Check" :variable "check" :onvalue 1 :offvalue 0 :command (tcl{ "event" "generate" "." "<<check.blah>>" :data (lit "$check")))

    (tcl "grid" (tcl[ "ttk::frame" ".f" :borderwidth 3 :relief "groove") :column 0 :row 0 :sticky "nwes")
    
    (tcl "set" "chart1" (tcl[ "canvas" ".f.chart1" :xscrollcommand ".f.h set" :height *chart-height*))

    (tcl "grid" (tcl[ "canvas" ".f.rearview" :background "black" (mapcan #'list '(:width :height) *image-size*)) :column 0 :row 0 :sticky "nwes")
    (tcl "grid" (tcl[ "canvas" ".f.frontview" :background "black" (mapcan #'list '(:width :height) *image-size*)) :column 1 :row 0 :sticky "nwes")
    (tcl "grid" (lit "$chart1") :column 0 :row 1 :sticky "nwes" :columnspan 2)
    (tcl "grid" (tcl[ "tk::scrollbar" ".f.h" :orient "horizontal" :command ".f.chart1 xview") :column 0 :row 2 :sticky "we" :columnspan 2)
    (tcl "grid" (tcl[ "ttk::label" ".f.l1" :background "grey") :column 0 :row 3 :sticky "nwes")
    (tcl "grid" (tcl[ "ttk::label" ".f.l2" :textvariable "meters" :background "red") :column 1 :row 3 :sticky "nwes")


    (tcl "image" "create" "photo" "rearview")
    (tcl "image" "create" "photo" "frontview")

    (tcl ".f.rearview" "create" "image" (mapcar #'(lambda (x) (/ x 2)) *image-size*) :image "rearview")
    (tcl ".f.frontview" "create" "image" (mapcar #'(lambda (x) (/ x 2)) *image-size*) :image "frontview")

    (tcl "set" "chartbackground" (tcl[ ".f.chart1" "create" "rectangle" 0 0 0 *chart-height* :width 0 :fill "white"))

    ;; (tcl "set" "ppp" (tcl ".f.chart1" "create" "line"
    ;;                       (loop
    ;;                          for coordinates across (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011")
    ;;                          for i from 0
    ;;                          when coordinates collect i and collect (format nil "~F" (* (- (coordinates-longitude coordinates) 14) 500)))
    ;;                       :fill "green" :width 10))
    ;; (loop
    ;;    for coordinates across (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011")
    ;;    for i from 0
    ;;    when coordinates do (tcl ".f.chart1" "create" "oval" i (format nil "~F" (coordinates-longitude coordinates)) i (format nil "~F" (coordinates-longitude coordinates))))

    ;; (tcl ".f.chart1" "create" "line" 100 100 100 100 :capstyle "round" :width 5) ;a point

    (tcl ".f.chart1" "bind" (lit "$chartbackground") "<ButtonPress-1>" "event generate . <<jumptostation>> -data [.f.chart1 canvasx %x]")

    ;; (tcl "foreach w [ winfo children .f ] {grid configure $w -padx 5 -pady 5}")
    ;; (tcl "focus" ".f.feet")
    (chart-dialog)
    (mainloop)))

(defun zeb-data (column vnk nnk chart-height)
  "Return a list of alternating station and column values between vnk
and nnk scaled into chart-height; the minimum column value; and the
maximum column value.  Both minimum and maximum are nil if data is
constant."
  (with-connection *postgresql-zeb-credentials*
    (setf column (intern (string-upcase column)))
    (destructuring-bind (minimum maximum)
        (mapcar #'(lambda (x) (if (numberp x)
                                  (coerce x 'double-float)
                                  x))
                (query (:select (:min column)
                                (:max column)
                                :from (intern *postgresql-zeb-table*)
                                :where (:and (:= 'vnk vnk)
                                             (:= 'nnk nnk)))
                       :list))
      (if (and (numberp minimum) (numberp maximum))
          (let* ((span (- maximum minimum))
                 (m (if (zerop span)
                        0
                        (/ chart-height span)))
                 (b (if (zerop span)
                        (* chart-height 1/2)
                        (+ chart-height (* m minimum)))))
            (values
             (mapcar #'(lambda (x) (if (numberp x)
                                       (coerce x 'double-float)
                                       x))
                     (reduce #'nconc
                             (query (:select 'vst
                                             (:- b (:* m column))
                                             'bst
                                             (:- b (:* m column))
                                             :from (intern *postgresql-zeb-table*)
                                             :where (:and (:= 'vnk vnk)
                                                          (:= 'nnk nnk))))))
             (unless (zerop span) minimum)
             (unless (zerop span) maximum)))
          (values nil nil nil)))))

(defun road-section-dialog ()
  (tcl "tk::toplevel" ".choose-road-section")
  (tcl "set" "chooseroadsectiontree" (tcl[ "ttk::treeview" ".choose-road-section.tree" :columns "length number-of-images" :yscrollcommand ".choose-road-section.v set" :height 40))
  (tcl "grid" (lit "$chooseroadsectiontree") :column 0 :row 0 :sticky "nwes")
  (tcl "grid" (tcl[ "tk::scrollbar" ".choose-road-section.v" :orient "vertical" :command ".choose-road-section.tree yview") :column 1 :row 0 :sticky "ns")
  (tcl "grid" (tcl[ "ttk::button" ".choose-road-section.close-button" :text "close" :command (event-handler* (print *choose-road-section-event*)
                                                                                                             (unregister-event *choose-road-section-event*)
                                                                                                             (tcl "destroy" ".choose-road-section")))
       :column 0 :row 1)
  (tcl ".choose-road-section.tree" "heading" "length" :text "m")
  (tcl ".choose-road-section.tree" "column" "length" :width 50 :anchor "e")

  (with-connection *postgresql-road-network-credentials*
    (let ((sections (sections (make-symbol *postgresql-road-network-table*))))
      (loop
         for (vnk nnk length) in sections
         do (multiple-value-bind (rearview-image-data rearview-cached-p)
                (road-section-image-data *postgresql-road-network-table* vnk nnk 10 t :from-cache-only t)
              (multiple-value-bind (frontview-image-data frontview-cached-p)
                  (road-section-image-data *postgresql-road-network-table* vnk nnk 10 nil :from-cache-only t)
                (add-vnk-nnk-leaf vnk nnk length (and rearview-cached-p frontview-cached-p (+ (length rearview-image-data) (length frontview-image-data))))))))
    (setf *choose-road-section-event*
          (bind-event ".choose-road-section.tree" "<ButtonPress-1>" ()
            (let ((vnk-nnk-length (read-from-string (tcl ".choose-road-section.tree" "focus"))))
              (apply #'prepare-chart (make-symbol *postgresql-road-network-table*) vnk-nnk-length)))))
  (mainloop))

(defun credentials-dialog ()
  (flet ((send-credentials (purpose)
           (tcl{ "event" "generate" ".credentials-dialog" "<<credentials>>"
                 :data (tcl[ "list"
                             (string purpose)
                             (lit "$roadnetworkdatabase") (lit "$roadnetworkhost") (lit "$roadnetworkport") (lit "$roadnetworkusessl") (lit "$roadnetworktable") (lit "$roadnetworkuser") (lit "$roadnetworkpassword")
                             (lit "$zebdatabase") (lit "$zebhost") (lit "$zebport") (lit "$zebusessl") (lit "$zebtable") (lit "$zebuser") (lit "$zebpassword")
                             (lit "$accidentsdatabase") (lit "$accidentshost") (lit "$accidentsport") (lit "$accidentsusessl") (lit "$accidentstable") (lit "$accidentsuser") (lit "$accidentspassword")
                             (lit "$phorosurl") (lit "$phorosuser") (lit "$phorospassword")))))

    (tcl "tk::toplevel" ".credentials-dialog")

    (tcl "grid" (tcl[ "ttk::labelframe" ".credentials-dialog.db" :text "database credentials") :column 0 :row 0 :columnspan 5 :sticky "w")
    (tcl "grid" (tcl[ "ttk::labelframe" ".credentials-dialog.phoros" :text "phoros credentials") :column 0 :row 1 :sticky "w")

    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.hosts" :text "host" :font "TkHeadingFont") :column 0 :row 1 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.ports" :text "port" :font "TkHeadingFont") :column 0 :row 2 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.use-ssls" :text "ssl" :font "TkHeadingFont") :column 0 :row 3 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.databases" :text "database" :font "TkHeadingFont") :column 0 :row 4 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.tables" :text "table" :font "TkHeadingFont") :column 0 :row 5 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.users" :text "user" :font "TkHeadingFont") :column 0 :row 6 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.passwords" :text "password" :font "TkHeadingFont") :column 0 :row 7 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.status" :text "status" :font "TkHeadingFont") :column 0 :row 8 :sticky "w")

    (destructuring-bind (database user password host &key (port 5432) (use-ssl :no))
        *postgresql-road-network-credentials*
      (tcl "set" "roadnetworkhost" host)
      (tcl "set" "roadnetworkport" port)
      (tcl "set" "roadnetworkusessl" (string use-ssl))
      (tcl "set" "roadnetworkdatabase" database)
      (tcl "set" "roadnetworktable" *postgresql-road-network-table*)
      (tcl "set" "roadnetworkuser" user)
      (tcl "set" "roadnetworkpassword" password))
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.road-network-header" :text "road network" :width 30 :font "TkHeadingFont") :column 1 :row 0 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.road-network-host" :textvariable "roadnetworkhost") :column 1 :row 1 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.road-network-port" :textvariable "roadnetworkport") :column 1 :row 2 :sticky "we")
    (tcl "grid" (tcl[ "ttk::checkbutton" ".credentials-dialog.db.roadnetwork-use-ssl" :variable "roadnetworkusessl" :onvalue "yes" :offvalue "no") :column 1 :row 3 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.road-network-database" :textvariable "roadnetworkdatabase") :column 1 :row 4 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.road-network-table" :textvariable "roadnetworktable") :column 1 :row 5 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.road-network-user" :textvariable "roadnetworkuser") :column 1 :row 6 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.road-network-password" :textvariable "roadnetworkpassword") :column 1 :row 7 :sticky "we")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.road-network-status" :text "?") :column 1 :row 8 :sticky "w")

    (destructuring-bind (database user password host &key (port 5432) (use-ssl :no))
        *postgresql-zeb-credentials*
      (tcl "set" "zebhost" host)
      (tcl "set" "zebport" port)
      (tcl "set" "zebusessl" (string use-ssl))
      (tcl "set" "zebdatabase" database)
      (tcl "set" "zebtable" *postgresql-zeb-table*)
      (tcl "set" "zebuser" user)
      (tcl "set" "zebpassword" password))
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.zeb-header" :text "ZEB" :width 30 :font "TkHeadingFont") :column 2 :row 0 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.zeb-host" :textvariable "zebhost") :column 2 :row 1 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.zeb-port" :textvariable "zebport") :column 2 :row 2 :sticky "we")
    (tcl "grid" (tcl[ "ttk::checkbutton" ".credentials-dialog.db.zeb-use-ssl" :variable "zebusessl" :onvalue "yes" :offvalue "no") :column 2 :row 3 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.zeb-database" :textvariable "zebdatabase") :column 2 :row 4 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.zeb-table" :textvariable "zebtable") :column 2 :row 5 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.zeb-user" :textvariable "zebuser") :column 2 :row 6 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.zeb-password" :textvariable "zebpassword") :column 2 :row 7 :sticky "we")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.zeb-status" :text "?") :column 2 :row 8 :sticky "w")

    (destructuring-bind (database user password host &key (port 5432) (use-ssl :no))
        *postgresql-accidents-credentials*
      (tcl "set" "accidentshost" host)
      (tcl "set" "accidentsport" port)
      (tcl "set" "accidentsusessl" (string use-ssl))
      (tcl "set" "accidentsdatabase" database)
      (tcl "set" "accidentstable" *postgresql-accidents-table*)
      (tcl "set" "accidentsuser" user)
      (tcl "set" "accidentspassword" password))
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.accidents-header" :text "accidents" :width 30 :font "TkHeadingFont") :column 3 :row 0 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.accidents-host" :textvariable "accidentshost") :column 3 :row 1 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.accidents-port" :textvariable "accidentsport") :column 3 :row 2 :sticky "we")
    (tcl "grid" (tcl[ "ttk::checkbutton" ".credentials-dialog.db.accidents-use-ssl" :variable "accidentsusessl" :onvalue "yes" :offvalue "no") :column 3 :row 3 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.accidents-database" :textvariable "accidentsdatabase") :column 3 :row 4 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.accidents-table" :textvariable "accidentstable") :column 3 :row 5 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.accidents-user" :textvariable "accidentsuser") :column 3 :row 6 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.db.accidents-password" :textvariable "accidentspassword") :column 3 :row 7 :sticky "we")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.db.accidents-status" :text "?") :column 3 :row 8 :sticky "w")

    (destructuring-bind (user password) *phoros-credentials*
      (tcl "set" "phorosurl" (with-output-to-string (s) (puri:render-uri *phoros-url* s)))
      (tcl "set" "phorosuser" user)
      (tcl "set" "phorospassword" password))
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.phoros.url" :text "URL" :font "TkHeadingFont") :column 0 :row 0 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.phoros.user" :text "user" :font "TkHeadingFont") :column 0 :row 1 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.phoros.password" :text "password" :font "TkHeadingFont") :column 0 :row 2 :sticky "w")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.phoros.status" :text "status" :font "TkHeadingFont") :column 0 :row 3 :sticky "w")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.phoros.phoros-url" :textvariable "phorosurl" :width 45) :column 1 :row 0)
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.phoros.phoros-user" :textvariable "phorosuser") :column 1 :row 1 :sticky "we")
    (tcl "grid" (tcl[ "ttk::entry" ".credentials-dialog.phoros.phoros-password" :textvariable "phorospassword") :column 1 :row 2 :sticky "we")
    (tcl "grid" (tcl[ "ttk::label" ".credentials-dialog.phoros.phoros-status" :text "?") :column 1 :row 3 :sticky "w")

    (bind-event ".credentials-dialog" "<<credentials>>" ((payload #\d))
      (let ((purpose (first (cl-utilities:split-sequence #\Space payload))))
        (cond ((string-equal purpose "ok")
               (apply #'phoros-login *phoros-url* *phoros-credentials*)
               (restore-credentials payload)
               (tcl "destroy" ".credentials-dialog"))
              ((string-equal purpose "save")
               (save-credentials payload))
              ((string-equal purpose "check")
               (let (*postgresql-road-network-credentials*
                     *postgresql-zeb-credentials*
                     *postgresql-accidents-credentials*)
                 (restore-credentials payload)
                 (tcl ".credentials-dialog.db.road-network-status" "configure" :text (check-db *postgresql-road-network-credentials* *postgresql-road-network-table*))
                 (tcl ".credentials-dialog.db.zeb-status" "configure" :text (check-db *postgresql-zeb-credentials* *postgresql-zeb-table*))
                 (tcl ".credentials-dialog.db.accidents-status" "configure" :text (check-db *postgresql-accidents-credentials* *postgresql-accidents-table*))
                 (tcl ".credentials-dialog.phoros.phoros-status" "configure" :text (apply #'check-phoros (with-output-to-string (s) (puri:render-uri *phoros-url* s)) *phoros-credentials*)))))))

    (tcl "grid" (tcl[ "ttk::button" ".credentials-dialog.cancel-button" :text "cancel" :command (tcl{ "destroy" ".credentials-dialog"))
         :column 1 :row 1 :sticky "s")
    (tcl "grid" (tcl[ "ttk::button" ".credentials-dialog.save-button" :text "save" :command (send-credentials :save))
         :column 2 :row 1 :sticky "s")
    (tcl "grid" (tcl[ "ttk::button" ".credentials-dialog.check-button" :text "check" :command (send-credentials :check))
         :column 3 :row 1 :sticky "s")
    (tcl "grid" (tcl[ "ttk::button" ".credentials-dialog.ok-button" :text "ok" :command (send-credentials :ok))
         :column 4 :row 1 :sticky "s")

    (tcl ".credentials-dialog.check-button" "invoke")

    (mainloop)))

(defun save-credentials (credentials-string)
  "Save input from credentials-dialog into cache directory."
  (let ((cache-file-name (cache-file-name 'credentials)))
    (ensure-directories-exist cache-file-name)
    (with-open-file (stream cache-file-name
                            :direction :output
                            :if-exists :supersede)
      (prin1 credentials-string stream))))

(defun restore-credentials (&optional credentials-string)
  "Put credentials (from credentials-string if any, or previously
saved by save-credentials if not) into their respective variables."
  (let ((cache-file-name (cache-file-name 'credentials)))
    (with-open-file (stream cache-file-name
                            :direction :input
                            :if-does-not-exist nil)
      (when (and stream (not credentials-string))
        (setf credentials-string (read stream)))
      (when credentials-string
        (destructuring-bind (purpose road-network-database road-network-host road-network-port road-network-use-ssl road-network-table road-network-user road-network-password
                                     zeb-database zeb-host zeb-port zeb-use-ssl zeb-table zeb-user zeb-password
                                     accidents-database accidents-host accidents-port accidents-use-ssl accidents-table accidents-user accidents-password
                                     phoros-url phoros-user phoros-password)
            (cl-utilities:split-sequence #\Space credentials-string)
          (declare (ignore purpose))
          (setf *postgresql-road-network-credentials*
                (list road-network-database road-network-user road-network-password road-network-host :port (parse-integer road-network-port :junk-allowed t) :use-ssl (intern (string-upcase road-network-use-ssl) 'keyword)))
          (setf *postgresql-road-network-table* road-network-table)
          (setf *postgresql-zeb-credentials*
                (list zeb-database zeb-user zeb-password zeb-host :port (parse-integer zeb-port :junk-allowed t) :use-ssl (intern (string-upcase zeb-use-ssl) 'keyword)))
          (setf *postgresql-zeb-table* zeb-table)
          (setf *postgresql-accidents-credentials*
                (list accidents-database accidents-user accidents-password accidents-host :port (parse-integer accidents-port :junk-allowed t) :use-ssl (intern (string-upcase accidents-use-ssl) 'keyword)))
          (setf *postgresql-accidents-table* accidents-table)
          (setf *phoros-url* (puri:parse-uri phoros-url))
          (setf *phoros-credentials* (list phoros-user phoros-password)))))))

(defun save-column-selection (column-selection-string)
  "Save input from chart-dialog into cache directory."
  (let ((cache-file-name (cache-file-name 'column-selection)))
    (ensure-directories-exist cache-file-name)
    (with-open-file (stream cache-file-name
                            :direction :output
                            :if-exists :supersede)
      (prin1 column-selection-string stream))))

(defun restore-column-selection (&optional column-selection-string)
  "Put database columns selected for rendering (from
column-selection-string if any, or previously saved by
save-column-selection if not) into their respective variables."
  (let ((cache-file-name (cache-file-name 'column-selection)))
    (with-open-file (stream cache-file-name
                            :direction :input
                            :if-does-not-exist nil)
      (when (and stream (not column-selection-string))
        (setf column-selection-string (read stream)))
      (when column-selection-string
        (loop
           for column-definition on (cdr (cl-utilities:split-sequence #\Space column-selection-string)) ;ignore purpose string
           by #'(lambda (x) (nthcdr 6 x)) ;by number of values per column definition
           for (table-kind column-name selectedp color width dash) = column-definition
           when (and (string-equal selectedp "1")
                     (string-equal table-kind "zeb"))
           collect (list column-name color width dash) into zeb-column-selection
           when (and (string-equal selectedp "1")
                     (string-equal table-kind "accidents"))
           collect (list column-name color width dash) into accidents-column-selection
           finally
             (setf *zeb-column-selection* zeb-column-selection)
             (setf *accidents-column-selection* accidents-column-selection))))))

(defun check-db (db-credentials table-name &aux result)
  "Check database connection and presence of table or view table-name.
Return a string describing the outcome."
  (unless
      (ignore-errors
        (with-connection db-credentials
          (if (or (table-exists-p table-name)
                  (view-exists-p table-name))
              (setf result "ok")
              (setf result "table or view missing"))))
    (setf result "connection failure"))
  result)

(defun check-phoros (url user-name password)
  "Check connection to phoros server.  Return a string describing the
outcome."
  (let ((*phoros-url* nil)
        (*phoros-cookies* nil))
    (unwind-protect
         (handler-case (phoros-login url user-name password)
           (usocket:ns-host-not-found-error () "host not found")
           (usocket:connection-refused-error () "connection refused")
           (error (c) (format nil "~A" c))
           (:no-error (result) (if result "ok" "wrong user or password")))
      (ignore-errors (phoros-logout)))))

(defun chart-dialog ()
  (flet ((send-column-selection (purpose)
           (tcl{ "event" "generate" ".chart-dialog" "<<columnselection>>"
                 :data (tcl[ "list"
                             (string purpose)
                             (with-connection *postgresql-zeb-credentials*
                               (loop
                                  for (column-name) in (table-description *postgresql-zeb-table*)
                                  collect (lit (concatenate 'string "zeb " column-name " $zeb_" column-name " $zeb_" column-name "_color" " $zeb_" column-name "_width" " $zeb_" column-name "_dash"))))
                             (with-connection *postgresql-accidents-credentials*
                               (loop
                                  for (column-name) in (table-description *postgresql-accidents-table*)
                                  collect (lit (concatenate 'string "accidents " column-name " $accidents_" column-name " $accidents_" column-name "_color" " $accidents_" column-name "_width" " $accidents_" column-name "_dash"))))))))
    (tcl "tk::toplevel" ".chart-dialog")
    (tcl "grid" (tcl[ "tk::text" ".chart-dialog.t" :width 80 :height 50 :xscrollcommand ".chart-dialog.h set" :yscrollcommand ".chart-dialog.v set") :column 0 :row 0)
    (tcl "grid" (tcl[ "tk::scrollbar" ".chart-dialog.h" :orient "horizontal" :command ".chart-dialog.t xview") :column 0 :row 1 :sticky "we")
    (tcl "grid" (tcl[ "tk::scrollbar" ".chart-dialog.v" :orient "vertical" :command ".chart-dialog.t yview") :column 1 :row 0 :sticky "sn")
    (tcl ".chart-dialog.t" "window" "create" "end" :window (tcl[ "ttk::frame" ".chart-dialog.t.f"))
    (tcl "grid" (tcl[ "ttk::labelframe" ".chart-dialog.t.f.zeb" :text "ZEB" :borderwidth 3 :relief "groove") :column 0 :row 0 :sticky "n")
    (tcl "grid" (tcl[ "ttk::labelframe" ".chart-dialog.t.f.accidents" :text "accidents" :borderwidth 3 :relief "groove") :column 1 :row 0 :sticky "ns")
    (tcl "grid" (tcl[ "ttk::frame" ".chart-dialog.buttons") :column 2 :row 0 :sticky "n")
    (tcl "grid" (tcl[ "ttk::button" ".chart-dialog.buttons.cancel" :text "cancel" :command (tcl{ "destroy" ".chart-dialog")) :column 0 :row 0)
    (tcl "grid" (tcl[ "ttk::button" ".chart-dialog.buttons.save" :text "save" :command (send-column-selection :save)) :column 0 :row 1)
    (tcl "grid" (tcl[ "ttk::button" ".chart-dialog.buttons.ok" :text "ok" :command (send-column-selection :ok)) :column 0 :row 2)

    (with-connection *postgresql-zeb-credentials*
      (present-db-columns (table-description *postgresql-zeb-table*) ".chart-dialog.t.f.zeb" "zeb_" *zeb-column-selection*))
    (with-connection *postgresql-accidents-credentials*
      (present-db-columns (table-description *postgresql-accidents-table*) ".chart-dialog.t.f.accidents" "accidents_" *accidents-column-selection*))
    (bind-event ".chart-dialog" "<<columnselection>>" ((payload #\d))
      (let ((purpose (first (cl-utilities:split-sequence #\Space payload))))
        (cond ((string-equal purpose "ok")
               (restore-column-selection payload)
               (refresh-chart)
               (tcl "destroy" ".chart-dialog"))
              ((string-equal purpose "save")
               (save-column-selection payload)))))
    (mainloop)))

(defun lit$ (tcl-variable)
  (lit (concatenate 'string "$" tcl-variable)))

(defun present-db-columns (columns tcl-path variable-prefix column-selection)
  (loop
     for (column-name type) in columns
     ;; name of checkbutton and trunk of the other element's names
     for variable-name = (concatenate 'string variable-prefix column-name)
     for path-name = (concatenate 'string tcl-path "." column-name)
     ;; rest of the input elements
     for label-path-name = (concatenate 'string tcl-path "." column-name "_label")
     for width-variable-name = (concatenate 'string variable-name "_width")
     for width-path-name = (concatenate 'string tcl-path "." column-name "_width")
     for color-variable-name = (concatenate 'string variable-name "_color")
     for color-path-name = (concatenate 'string tcl-path "." column-name "_color")
     for dash-variable-name = (concatenate 'string variable-name "_dash")
     for dash-path-name = (concatenate 'string tcl-path "." column-name "_dash")
     for sample-path-name = (concatenate 'string tcl-path "." column-name "_sample")
     for sample-line-path-name = (concatenate 'string tcl-path "." column-name "_sample_line")
     for i from 0
     do
       (when (zerop (mod i 25))
         (let* ((name-header-path-name (concatenate 'string tcl-path "." column-name "_name_header"))
                (type-header-path-name (concatenate 'string tcl-path "." column-name "_type_header"))
                (width-header-path-name (concatenate 'string tcl-path "." column-name "_width_header"))
                (color-header-path-name (concatenate 'string tcl-path "." column-name "_color_header"))
                (dash-header-path-name (concatenate 'string tcl-path "." column-name "_dash_header"))
                (sample-header-path-name (concatenate 'string tcl-path "." column-name "_sample_header")))
           (tcl "grid" (tcl[ "ttk::label" name-header-path-name :text "column" :font "TkHeadingFont") :column 0 :row i :sticky "w")
           (tcl "grid" (tcl[ "ttk::label" type-header-path-name :text "type" :font "TkHeadingFont") :column 1 :row i :sticky "w")
           (tcl "grid" (tcl[ "ttk::label" width-header-path-name :text "width" :font "TkHeadingFont") :column 2 :row i :sticky "w")
           (tcl "grid" (tcl[ "ttk::label" color-header-path-name :text "color" :font "TkHeadingFont") :column 3 :row i :sticky "w")
           (tcl "grid" (tcl[ "ttk::label" dash-header-path-name :text "dash" :font "TkHeadingFont") :column 4 :row i :sticky "w")
           (tcl "grid" (tcl[ "ttk::label" sample-header-path-name :text "sample" :font "TkHeadingFont") :column 5 :row i))
         (incf i))
       (let ((selected-column (find column-name column-selection :key #'first :test #'string-equal)))
         (tcl "grid" (tcl[ "ttk::checkbutton" path-name :text column-name :variable variable-name) :column 0 :row i :sticky "w")
         (tcl "grid" (tcl[ "ttk::label" label-path-name :text type) :column 1 :row i :sticky "w")
         (tcl "grid" (tcl[ "tk::spinbox" width-path-name :width 2 :textvariable width-variable-name :values "1 2 3 4 5 6"
                           :state "readonly"
                           :command (tcl{ "set" variable-name 1 (lit ";")
                                          sample-path-name "itemconfigure" sample-line-path-name :width (lit$ width-variable-name)))
              :column 2 :row i)
         (tcl "grid" (tcl[ "ttk::button" color-path-name
                           :width 1
                           :command  (tcl{ "set" "tmp" (tcl[ "tk_chooseColor" :initialcolor (lit$ color-variable-name))
                                           (lit ";")
                                           (lit "if { $tmp != {}} { set") color-variable-name (lit$ "tmp; set") variable-name 1 (lit "}")
                                           (lit ";")
                                           sample-path-name "itemconfigure" sample-line-path-name :fill (lit$ color-variable-name)))
              :column 3 :row i)
         (tcl "grid" (tcl[ "tk::spinbox" dash-path-name :width 2 :textvariable dash-variable-name :values "{} -.-. --- ... "
                           :state "readonly"
                           :command (tcl{ "set" variable-name 1 (lit ";")
                                          sample-path-name "itemconfigure" sample-line-path-name :dash (lit$ dash-variable-name))) :column 4 :row i)
          
         (tcl "grid" (tcl[ "canvas" sample-path-name :background "white" :width 100 :height 20) :column 5 :row i)
         (if selected-column
             (progn
               (tcl "set" variable-name 1)
               (tcl "set" color-variable-name (color selected-column))
               (tcl "set" width-variable-name (line-width selected-column))
               (tcl "set" dash-variable-name (dash selected-column)))
             (progn
               (tcl "set" variable-name 0)
               (tcl "set" color-variable-name "black")
               (tcl "set" width-variable-name 2)
               (tcl "set" dash-variable-name "")))
         (tcl sample-path-name "create" "line" 0 18 20 2 60 10 100 10 :tags sample-line-path-name :joinstyle "round" :capstyle "round" :fill (lit$ color-variable-name) :width (lit$ width-variable-name) :dash (lit$ dash-variable-name)))))

(defun color (column-definition)
  (second column-definition))

(defun line-width (column-definition)
  (third column-definition))

(defun dash (column-definition)
  (fourth column-definition))

(defun add-vnk-nnk-leaf (vnk nnk length number-of-images)
  "Put a leaf labelled vnk-nnk into road-sections tree."
  (tcl ".choose-road-section.tree" "insert" "" "end" :id (format nil "(~S ~S ~D)" vnk nnk length) :text (format nil "~A - ~A" vnk nnk) :values (tcl[ "list" length (or number-of-images "?"))))

(defun prepare-chart (table vnk nnk road-section-length)
  "Prepare chart for the road section between vnk and nnk in table in
current database."
  (setf *chart-parameters* (list table vnk nnk road-section-length))
  (when *jump-to-station-event* (unregister-event *jump-to-station-event*))
  (tcl ".f.chart1" "configure" :scrollregion (format nil "~D ~D ~D ~D" 0 0 road-section-length *chart-height*))
  (tcl ".f.chart1" "coords" (lit "$chartbackground") 0 0 road-section-length *chart-height*)

  (draw-zeb-graphs vnk nnk)

  (tcl "if" (tcl[ "info" "exists" "cursor") (tcl{ ".f.chart1" "delete" (lit "$cursor")))
  (tcl "set" "cursor" (tcl[ ".f.chart1" "create" "line" 0 0 0 *chart-height* :width 2))
  (setf *jump-to-station-event*
        (bind-event "." "<<jumptostation>>" ((station #\d))
          (setf station (max 0   ;appearently necessary; not sure why.
                             (round (parse-number:parse-number station))))
          (tcl "set" "meters" station)
          (tcl ".f.chart1" "coords" (lit "$cursor") station 0 station *chart-height*)
          (put-image :table table :vnk vnk :nnk nnk :station station :step 10 :rear-view-p t)
          (put-image :table table :vnk vnk :nnk nnk :station station :step 10 :rear-view-p nil)))
  (tcl "event" "generate" "." "<<jumptostation>>" :data (tcl[ ".f.chart1" "canvasx" 0)))

(defun refresh-chart ()
  "Redraw chart."
  (when *chart-parameters* (apply #'prepare-chart *chart-parameters*)))

(defun draw-zeb-graphs (vnk nnk)
  "Draw graphs for the columns in *zeb-column-selection*.  Delete
existing graphs first."
  (tcl ".f.chart1" "delete" (lit "graph"))
  (loop
     for (column-name color width dash) in *zeb-column-selection*
     do (draw-zeb-graph column-name vnk nnk color width dash)))

(defun draw-zeb-graph (column vnk nnk color width dash)
  (multiple-value-bind (line minimum maximum)
      (zeb-data column vnk nnk *chart-height*) ;TODO: take care of data with gaps
    (print (list :column column :min minimum :max maximum :color color :width width :dash dash))
    (when line
      (tcl ".f.chart1" "create" "line" line :tags "graph" :joinstyle "round" :capstyle "round" :fill color :width width :dash dash))))

(defun put-image (&key table vnk nnk station step rear-view-p)
  "Put an image along with a labelled station marker on screen."
  (with-connection *postgresql-road-network-credentials*
    (let* ((point-radius 5)
           (line-width 2)
           (photo (if rear-view-p "rearview" "frontview"))
           (canvas (concatenate 'string ".f." photo))
           (cursor-name (concatenate 'string photo "cursor"))
           (label-name (concatenate 'string photo "label"))
           (arrow-name (concatenate 'string photo "arrow"))
           (global-point-coordinates 
            (subseq (all-stations table vnk nnk)
                    (min (length (all-stations table vnk nnk)) station)
                    (min (length (all-stations table vnk nnk)) (+ station 4))))
           (image-data-alist
            (get-image-data-alist (road-section-image-data table vnk nnk step rear-view-p)
                                  station
                                  step))
           (image-arrow-coordinates
            (loop
               for i across global-point-coordinates
               append (image-point-coordinates image-data-alist i)))
           (image-cursor-coordinates (ignore-errors
                                       (list (- (first image-arrow-coordinates) point-radius)
                                             (- (second image-arrow-coordinates) point-radius)
                                             (+ (first image-arrow-coordinates) point-radius)
                                             (+ (second image-arrow-coordinates) point-radius))))
           (image-label-coordinates  (ignore-errors
                                       (list (+ (first image-arrow-coordinates) point-radius line-width)
                                             (second image-arrow-coordinates)))))
      (tcl photo "configure" :file (or (get-image-namestring (road-section-image-data table vnk nnk step rear-view-p)
                                                             station
                                                             step)
                                       "public_html/phoros-logo-plain.png"))
      (tcl "if" (tcl[ "info" "exists" cursor-name) (tcl{ canvas "delete" (lit (concatenate 'string "$" cursor-name))))
      (tcl "if" (tcl[ "info" "exists" label-name) (tcl{ canvas "delete" (lit (concatenate 'string "$" label-name))))
      (tcl "if" (tcl[ "info" "exists" arrow-name) (tcl{ canvas "delete" (lit (concatenate 'string "$" arrow-name))))
      (when image-cursor-coordinates
        (tcl "set" cursor-name (tcl[ canvas "create" "oval" image-cursor-coordinates :width line-width)))
      (when image-label-coordinates
        (tcl "set" label-name (tcl[ canvas "create" "text" image-label-coordinates :text station :anchor "w")))
      (when (and image-arrow-coordinates
                 (loop
                    for tail on image-arrow-coordinates by #'cddr
                    always (in-image-p (first tail) (second tail))))
        (tcl "set" arrow-name (tcl[ canvas "create" "line" image-arrow-coordinates :arrow "last" :width line-width))))))

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
                             :from table
                             :where (:and (:not-null 'vnk) (:not-null 'nnk))
                             :group-by 'vnk 'nnk)
                    'vnk 'nnk)))

(defun stations (table vnk nnk &optional (step 1))
  "Return a list of plists of :longitude, :latitude,
:ellipsoid-height, :station, :azimuth of stations step metres apart
between vnk and nnk."
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
   :plists))

(defun-cached all-stations (table vnk nnk)
  "Return a vector of coordinates of all points between vnk and nnk,
station (in metres) being the vector index."
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
    result))

(defun-cached road-section-image-data (table vnk nnk step rear-view-p)
  "Return a list of instances of image data corresponding to stations,
which are step metres apart, found in table in current database."
  (remove nil (mapcar #'(lambda (x)
                          (apply #'image-data :rear-view-p rear-view-p x))
                      (stations table vnk nnk step))))

(defun cache-file-name (kind &rest args)
  "Return pathname for a cache file distinguishable by kind and args."
  (make-pathname :directory *cache-dir*
                 :name (format nil "~{~:[f~;~:*~(~A~)~]_~}~A"
                               args
                               *fasttrack-version*)
                 :type (string-downcase kind)))

;; (defun road-section-image-data-pathname (vnk nnk step rear-view-p)
;;   "Return pathname of a cached set of image data between vnk and nnk,
;; step metres apart."
;;   (make-pathname :directory *cache-dir*
;;                  :name (format nil "~A_~A_~D_~:[f~;r~]_~A"
;;                                vnk nnk step rear-view-p
;;                                *fasttrack-version*)
;;                  :type "image-data"))

(defun cache-images (road-section-image-data)
  "Download images described in road-section-image-data into their
canonical places."
  (loop
     for i in road-section-image-data
     do (download-image i)))

(defun get-image-data (road-section-image-data station step)
  "Return image data for the image near station."
  (find (* step (round station step)) road-section-image-data
        :key #'image-data-station
        :test #'=))

(defun get-image-namestring (road-section-image-data station step)
  "Return path to image near station.  Download it if necessary."
  (let ((image-data (get-image-data road-section-image-data station step)))
    (when image-data (namestring (download-image image-data)))))

(defun get-image-data-alist (road-section-image-data station step)
  "Return as an alist data for the image near station."
  (image-data-alist (get-image-data road-section-image-data station step)))

(defun image-data (&key longitude latitude ellipsoid-height station azimuth rear-view-p)
  "Get from Phoros server image data for location near longitude,
latitude."
  (let* ((coordinates (make-coordinates :longitude longitude
                                        :latitude latitude
                                        :ellipsoid-height ellipsoid-height
                                        :azimuth azimuth))
         (image-data (phoros-nearest-image-data coordinates rear-view-p)))
    (when (image-data-p image-data)
      (setf (image-data-station image-data) station)
      (setf (image-data-station-coordinates image-data) coordinates)
      image-data)))

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
  (let* ((old-path (puri:uri-parsed-path canonical-url))
         (new-path (append (butlast old-path) (list "lib" suffix)))
         (new-url (puri:copy-uri canonical-url)))
    (setf (puri:uri-parsed-path new-url) new-path)
    new-url))

(defun phoros-login (url user-name user-password)
  "Log into Phoros server; return T if successful.  Try logging out
first."
  (setf *phoros-url* (puri:parse-uri url))
  (setf drakma:*allow-dotless-cookie-domains-p* t)
  (pushnew (cons "application" "json") drakma:*text-content-types* :test #'equal)
  (phoros-logout)
  (setf *phoros-cookies* (make-instance 'drakma:cookie-jar))
  (multiple-value-bind (body status-code headers url stream must-close reason-phrase)
      (drakma:http-request *phoros-url* :cookie-jar *phoros-cookies*)
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :url url :reason-phrase reason-phrase)
    (multiple-value-bind (body status-code headers authenticate-url stream must-close reason-phrase)
        (drakma:http-request (phoros-lib-url *phoros-url* "authenticate")
                             :cookie-jar *phoros-cookies*
                             :form-data t
                             :method :post
                             :parameters (pairlis '("user-name" "user-password")
                                                  (list user-name user-password)))
      (declare (ignore stream must-close))
      (assert (< status-code 400) ()
              'phoros-server-error :body body :status-code status-code :headers headers :url authenticate-url :reason-phrase reason-phrase)
      (= status-code 302))))

(defun phoros-logout ()
  (multiple-value-bind (body status-code headers url stream must-close reason-phrase)
      (drakma:http-request (phoros-lib-url *phoros-url* "logout")
                           :cookie-jar *phoros-cookies*)
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :url url :reason-phrase reason-phrase)))

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
  (ensure-directories-exist path)
  (with-open-file (file-stream path :direction :output
                               :element-type 'unsigned-byte
                               :if-exists nil)
    (when file-stream
      (multiple-value-bind
            (body status-code headers url stream must-close reason-phrase)
          (drakma:http-request url
                               :cookie-jar *phoros-cookies*
                               :method :get)
        (declare (ignore stream must-close))
        (assert (= status-code 200) ()
                'phoros-server-error :body body :status-code status-code :headers headers :url url :reason-phrase reason-phrase)
        (write-sequence body file-stream)
        reason-phrase))))

(defun download-image (image-data)
  "If not already there, download a png image, shrink it, convert it
into jpg, and store it under the cache path.  Return that path."
  (multiple-value-bind (url origin-path destination-path)
      (image-url image-data)
    (unless (probe-file destination-path)
      (download-file url origin-path)
      (apply #'convert-image-file origin-path destination-path *image-size*)
      (delete-file origin-path))
    destination-path))
    
(defstruct coordinates
  longitude
  latitude
  ellipsoid-height
  azimuth)

(eval `(defstruct image-data
         ;; fasttrack auxiliary slots
         station
         station-coordinates
         (rear-view-p nil) 
         ;; original Phoros image data slots
         ,@(mapcar #'ensure-hyphen-before-digit *aggregate-view-columns*)))

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
  (let* ((path
          (format nil "~A/~A~A/~D.png"
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
         (url (puri:copy-uri *phoros-url* :path path :query query))
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
                           :type "jpg"))))

(defun convert-image-file (origin-file destination-file width height)
  "Convert origin-file into destination-file of a maximum size of
width x height."
  (lisp-magick:with-magick-wand (wand :load (namestring origin-file))
    (let ((a (/ (lisp-magick:magick-get-image-width wand)
                (lisp-magick:magick-get-image-height wand))))
      (if (> a (/ width height))
          (lisp-magick:magick-scale-image wand width (truncate (/ width a)))
          (lisp-magick:magick-scale-image wand (truncate (* a height)) height)))
    (lisp-magick:magick-write-image wand (namestring destination-file))))

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
