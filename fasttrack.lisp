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

(defvar *postgresql-aux-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

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

(defvar *cache-dir* '(:absolute "home" "bertb" "lisphack" "phoros" "cache"))
;; TODO: invent cache validity checks

(defparameter *image-size* '(800 800)
  "Image size in pixels in a list (width height).")

(defvar *jump-to-station-event* nil
  "Remembering event id of chart click event jumptostation.")

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
  (with-tk ((make-instance 'ffi-tk))
    (tcl "package" "require" "Img")
    (tcl "option" "add" "*tearOff" 0)
    (tcl "wm" "title" "." "Phoros Fasttrack")
    (tcl "menu" ".menubar")
    (tcl "." "configure" :menu ".menubar")
    (tcl "menu" ".menubar.file")
    (tcl ".menubar" "add" "cascade" :label "File" :menu ".menubar.file" :underline 0)
    (tcl ".menubar.file" "add" "command" :label "Kaputt" :command (tcl{ "destroy" "."))
    (tcl ".menubar.file" "add" "command" :label "Do Stuff" :command (event-handler* (print "doing stuff") (print "doing more stuff") (tcl "set" "feet" 500)))

    (bind-event "." "<<check.blah>>" ((ddd #\d)) (print (list "ddd" ddd)))
    (tcl ".menubar.file" "add" "checkbutton" :label "Check" :variable "check" :onvalue 1 :offvalue 0 :command (tcl{ "event" "generate" "." "<<check.blah>>" :data (lit "$check")))

    (tcl "grid" (tcl[ "ttk::frame" ".f" :borderwidth 3 :relief "groove") :column 0 :row 0 :sticky "nwes")
    
    (tcl "set" "chart1" (tcl[ "canvas" ".f.chart1" :xscrollcommand ".f.h set"))

    (tcl "grid" (tcl[ "canvas" ".f.rearview" :bg "black" (mapcan #'list '(:width :height) *image-size*)) :column 0 :row 0 :sticky "nwes")
    (tcl "grid" (tcl[ "canvas" ".f.frontview" :bg "black" (mapcan #'list '(:width :height) *image-size*)) :column 1 :row 0 :sticky "nwes")
    (tcl "grid" (lit "$chart1") :column 0 :row 1 :sticky "nwes" :columnspan 2)
    (tcl "grid" (tcl[ "tk::scrollbar" ".f.h" :orient "horizontal" :command ".f.chart1 xview") :column 0 :row 2 :sticky "we" :columnspan 2)
    (tcl "grid" (tcl[ "ttk::label" ".f.l1" :background "grey") :column 0 :row 3 :sticky "nwes")
    (tcl "grid" (tcl[ "ttk::label" ".f.l2" :textvariable "meters" :background "red") :column 1 :row 3 :sticky "nwes")


    (tcl ".f.chart1" "create" "line" '(30 30 40 40 50 30 600 40) :fill "red" :tags "lll")
    ;; (tcl ".f.chart1" "scale" "lll" 0 0 .1 1)
 
    (tcl "image" "create" "photo" "rearview")
    (tcl "image" "create" "photo" "frontview")

    (tcl ".f.rearview" "create" "image" (mapcar #'(lambda (x) (/ x 2)) *image-size*) :image "rearview")
    (tcl ".f.frontview" "create" "image" (mapcar #'(lambda (x) (/ x 2)) *image-size*) :image "frontview")

    (tcl "set" "chartbackground" (tcl[ ".f.chart1" "create" "rectangle" 0 0 0 400 :width 0 :fill "green"))

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

    ;; (prepare-chart 'bew-landstr-kleinpunkte "4252017" "4252011")

    ;; (tcl "foreach w [ winfo children .f ] {grid configure $w -padx 5 -pady 5}")
    ;; (tcl "focus" ".f.feet")
    
    (tcl "tk::toplevel" ".choose-road-section")
    (tcl "grid" (tcl[ "ttk::treeview" ".choose-road-section.tree" :columns "length number-of-images") :column 0 :row 0 :sticky "nwes")
    (tcl ".choose-road-section.tree" "heading" "length" :text "m")
    (tcl ".choose-road-section.tree" "column" "length" :width 50 :anchor "e")

    (let ((sections (sections 'bew-landstr-kleinpunkte)))
      (loop
         for (vnk nnk length) in sections
         do (multiple-value-bind (rearview-image-data rearview-cached-p)
                (road-section-image-data 'bew-landstr-kleinpunkte vnk nnk 10 t :from-cache-only t)
              (multiple-value-bind (frontview-image-data frontview-cached-p)
                  (road-section-image-data 'bew-landstr-kleinpunkte vnk nnk 10 nil :from-cache-only t)
                (add-vnk-nnk-leaf vnk nnk length (and rearview-cached-p frontview-cached-p (+ (length rearview-image-data) (length frontview-image-data))))))))
    (bind-event ".choose-road-section.tree" "<ButtonPress-1>" ()
      (let ((vnk-nnk-length (read-from-string (tcl ".choose-road-section.tree" "focus"))))
        (apply #'prepare-chart 'bew-landstr-kleinpunkte vnk-nnk-length)))
    (mainloop)))

(defun add-vnk-nnk-leaf (vnk nnk length number-of-images)
  "Put a leaf labelled vnk-nnk into road-sections tree."
  (tcl ".choose-road-section.tree" "insert" "" "end" :id (format nil "(~S ~S ~D)" vnk nnk length) :text (format nil "~A - ~A" vnk nnk) :values (tcl[ "list" length (or number-of-images "?"))))

(defun prepare-chart (table vnk nnk road-section-length)
  "Prepare chart for the road section between vnk and nnk in table in
current database."
  (when *jump-to-station-event* (unregister-event *jump-to-station-event*))
  (tcl ".f.chart1" "configure" :scrollregion (format nil "~D ~D ~D ~D" 0 0 road-section-length 400))
  (tcl ".f.chart1" "coords" (lit "$chartbackground") 0 0 road-section-length 400)
  (tcl "if" (tcl[ "info" "exists" "cursor") (tcl{ ".f.chart1" "delete" (lit "$cursor")))
  (tcl "set" "cursor" (tcl[ ".f.chart1" "create" "line" 0 0 0 400 :width 2))
  (setf *jump-to-station-event*
        (bind-event "." "<<jumptostation>>" ((station #\d))
          (setf station (max 0   ;appearently necessary; not sure why.
                             (round (parse-number:parse-number station))))
          (tcl "set" "meters" station)
          (tcl ".f.chart1" "coords" (lit "$cursor") station 0 station 400)
          (put-image :table table :vnk vnk :nnk nnk :station station :step 10 :rear-view-p t)
          (put-image :table table :vnk vnk :nnk nnk :station station :step 10 :rear-view-p nil)))
  (tcl "event" "generate" "." "<<jumptostation>>" :data (tcl[ ".f.chart1" "canvasx" 0)))

(defun put-image (&key table vnk nnk station step rear-view-p)
  "Put an image along with a labelled station marker on screen."
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
      (tcl "set" arrow-name (tcl[ canvas "create" "line" image-arrow-coordinates :arrow "last" :width line-width)))))

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
  (setf drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*))
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
