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
  necessary."
  (when (stringp (car body))
    (setf doc (car body))
    (setf body (cdr body)))
  (cl-utilities:with-unique-names (input-stream output-stream)
    `(defun ,name (,@args)
       ,doc
       (ensure-directories-exist (cache-file-name ',name ,@args))
       (with-open-file (,input-stream (cache-file-name ',name ,@args)
                                      :direction :input
                                      :if-does-not-exist nil)
         (if ,input-stream
             (read ,input-stream)
             (with-open-file (,output-stream (cache-file-name ',name ,@args)
                                            :direction :output)
               (prin1 (progn ,@body)
                      ,output-stream)))))))

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

    (bind-event ".menubar.file" "<<check.blah>>" ((ddd #\d)) (print (list "ddd" ddd)))
    (tcl ".menubar.file" "add" "checkbutton" :label "Check" :variable "check" :onvalue 1 :offvalue 0 :command (tcl{ "event" "generate" ".menubar.file" "<<check.blah>>" :data (lit "$check")))

    (tcl "grid" (tcl[ "ttk::frame" ".f" :padding "3 3 12 12") :column 0 :row 0 :sticky "nwes")
    
    ;; (tcl "event" "generate" "." "<<boom>>" :data "Blahbla")
    
    (tcl "set" "chart1" (tcl[ "canvas" ".f.chart1" :bg "yellow" :scrollregion "0 0 2500 400" :xscrollcommand ".f.h set"))

    (tcl "grid" (tcl[ "canvas" ".f.image1" :bg "black" (mapcan #'list '(:width :height) *image-size*)) :column 0 :row 0 :sticky "nwes")
    (tcl "grid" (tcl[ "canvas" ".f.image2" :bg "black" (mapcan #'list '(:width :height) *image-size*)) :column 1 :row 0 :sticky "nwes")
    (tcl "grid" (lit "$chart1") :column 0 :row 1 :sticky "nwes" :columnspan 2)
    (tcl "grid" (tcl[ "tk::scrollbar" ".f.h" :orient "horizontal" :command ".f.chart1 xview") :column 0 :row 3 :sticky "we" :columnspan 2)
    (tcl "grid" (tcl[ "ttk::label" ".f.l1" :background "grey") :column 0 :row 2 :sticky "nwes")
    (tcl "grid" (tcl[ "ttk::label" ".f.l2" :textvariable "meters" :background "red") :column 1 :row 2 :sticky "nwes")


    (tcl ".f.chart1" "create" "line" '(30 30 40 40 50 30 600 40) :fill "red" :tags "lll")
    (tcl ".f.chart1" "scale" "lll" 0 0 .1 1)
 
    (tcl "image" "create" "photo" "rear-view")
    (tcl "image" "create" "photo" "front-view")

    (tcl ".f.image1" "create" "image" (mapcar #'(lambda (x) (/ x 2)) *image-size*) :image "rear-view")
    (tcl ".f.image2" "create" "image" (mapcar #'(lambda (x) (/ x 2)) *image-size*) :image "front-view")

    (tcl "set" "ppp" (tcl ".f.chart1" "create" "line"
                          (loop
                             for coordinates across (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011")
                             for i from 0
                             when coordinates collect i and collect (format nil "~F" (* (- (coordinates-longitude coordinates) 14) 500)))
                          :fill "green" :width 10))
    (loop
       for coordinates across (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011")
       for i from 0
       when coordinates do (tcl ".f.chart1" "create" "oval" i (format nil "~F" (coordinates-longitude coordinates)) i (format nil "~F" (coordinates-longitude coordinates))))

    (tcl ".f.chart1" "create" "line" 100 100 100 100 :capstyle "round" :width 5) ;a point

    (tcl "set" "cursor" (tcl[ ".f.chart1" "create" "line" 10 0 10 100))

    (tcl ".f.chart1" "bind" (lit "$ppp") "<ButtonPress-1>"
         ;; Some canvasx voodoo required, possibly involving virtual events
         (event-handler
          #'(lambda (xx)
              (progn (tcl "set" "meters" xx)
                     (tcl ".f.chart1" "delete" (lit "$cursor"))
                     (tcl "set" "cursor" (tcl[ ".f.chart1" "create" "line" xx 0 xx 100))
                     (tcl "rear-view" "configure" :file (or (get-image-namestring (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100 t)
                                                                                  (parse-integer xx)
                                                                                  100)
                                                            "public_html/phoros-logo-plain.png"))
                     (print xx)
                     (print (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx)))
                     (print (ignore-errors
                              (photogrammetry :reprojection
                                              (get-image-data-alist (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100 t)
                                                                    (parse-integer xx)
                                                                    100)
                                              (pairlis '(:x-global :y-global :z-global)
                                                       (proj:cs2cs
                                                        (list
                                                         (proj:degrees-to-radians
                                                          (coordinates-longitude (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx))))
                                                         (proj:degrees-to-radians
                                                          (coordinates-latitude (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx))))
                                                         (coordinates-ellipsoid-height (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx))))
                                                        :destination-cs (cdr (assoc :cartesian-system (get-image-data-alist (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100 t)
                                                                                                                            (parse-integer xx)
                                                                                                                            100))))))))
                     (tcl "front-view" "configure" :file (or (get-image-namestring (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100 nil)
                                                                                   (parse-integer xx)
                                                                                   100)
                                                             "public_html/phoros-logo-background.png"))
                     (print (ignore-errors
                              (photogrammetry :reprojection
                                              (get-image-data-alist (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100 nil)
                                                                    (parse-integer xx)
                                                                    100)
                                              (pairlis '(:x-global :y-global :z-global)
                                                       (proj:cs2cs
                                                        (list
                                                         (proj:degrees-to-radians
                                                          (coordinates-longitude (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx))))
                                                         (proj:degrees-to-radians
                                                          (coordinates-latitude (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx))))
                                                         (coordinates-ellipsoid-height (svref (all-stations 'bew-landstr-kleinpunkte "4252017" "4252011") (parse-integer xx))))
                                                        :destination-cs (cdr (assoc :cartesian-system (get-image-data-alist (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100 nil)
                                                                                                                            (parse-integer xx)
                                                                                                                            100))))))))))
          '(#\x)))

    ;; (bind-event ".f.chart1" "<ButtonPress-1>" ((xx #\x))
    ;;   (progn (tcl "set" "meters" xx)
    ;;          (tcl ".f.chart1" "delete" (lit "$cursor"))
    ;;          (tcl "set" "cursor" (tcl[ ".f.chart1" "create" "line" xx 0 xx 100))
    ;;          (tcl "rear-view" "configure" :file (get-image-namestring (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100)
    ;;                                                                 (parse-integer xx)
    ;;                                                                 100))
    ;;          (tcl "front-view" "configure" :file (get-image-namestring (road-section-image-data 'bew-landstr-kleinpunkte "4252017" "4252011" 100)
    ;;                                                                 (parse-integer xx)
    ;;                                                                 100))))


    ;; (tcl "grid" (tcl[ "ttk::entry" ".f.feet" :width 7 :textvariable "feet") :column 2 :row 1 :sticky "we")
    ;; (tcl "grid" (tcl[ "ttk::label" ".f.meters" :textvariable "meters") :column 2 :row 2 :sticky "we")
    ;; (tcl "grid" (tcl[ "ttk::button" ".f.calc" :text "Calculate" :command "calculate") :column 3 :row 3 :sticky "w")
    ;; (tcl "grid" (tcl[ "ttk::label" ".f.flbl" :text "feet") :column 3 :row 1 :sticky "w")
    ;; (tcl "grid" (tcl[ "ttk::label" ".f.islbl" :text "is equivalent to") :column 1 :row 2 :sticky "e")
    ;; (tcl "grid" (tcl[ "ttk::label" ".f.mlbl" :text "meters") :column 3 :row 2 :sticky "w")
    ;; (tcl "foreach w [ winfo children .f ] {grid configure $w -padx 5 -pady 5}")
    ;; (tcl "focus" ".f.feet")
    
    (mainloop)))



(defun sections (table &key (start 0) (end most-positive-fixnum))
  "Return list of distinct pairs of vnk, nnk found in table in
current database."
  (query (:limit (:order-by (:select 'vnk 'nnk
                                     :from table
                                     :group-by 'vnk 'nnk)
                            'vnk 'nnk)
                 (- end start) start)))

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
  "Download images described in image data into their canonical places."
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
        ((or (<= (* 5/4 pi) azimuth pi) (<= 0 (* 1/4 pi))) (if rear-view-p "north" "south"))))

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
              (print (car (json:decode-json-from-string body))))))))

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
