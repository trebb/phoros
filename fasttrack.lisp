(in-package #:phoros-fasttrack)

(defvar *postgresql-aux-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

(defvar *phoros-cookies* nil
  "Container for cookies sent by Phoros server")

(defvar *phoros-url* nil
  "URL of the Phoros project currently in use.")

(defvar *cache-dir* '(:absolute "home" "bertb" "lisphack" "phoros" "cache"))

(defun main ()

  (with-tk ((make-instance 'ffi-tk))
    (let ((c ".c"))
      (tcl "package" "require" "Img")
      (tcl "option" "add" "*tearOff" 0)
      (tcl "wm" "title" "." "Conway's life")
      (tcl "menu" ".menubar")
      (tcl "." "configure" :menu ".menubar")
      (tcl "menu" ".menubar.file")
      (tcl ".menubar" "add" "cascade" :label "File" :menu ".menubar.file" :underline 0)
      (tcl ".menubar.file" "add" "command" :label "Kaputt" :command (tcl{ "destroy" "."))
      (tcl ".menubar.file" "add" "command" :label "Do Stuff" :command (event-handler* (print "doing stuff") (print "doing more stuff") (tcl "set" "feet" 500)))

      (bind-event ".menubar.file" "<<check.blah>>" ((ddd #\d)) (print (list "ddd" ddd)))
      (tcl ".menubar.file" "add" "checkbutton" :label "Check" :variable "check" :onvalue 1 :offvalue 0 :command (tcl{ "event" "generate" ".menubar.file" "<<check.blah>>" :data (lit "$check")))

      (tcl "grid" (tcl[ "ttk::frame" ".c" :padding "3 3 12 12") :column 0 :row 0 :sticky "nwes")
      ;;       (tcl "grid" "columnconfigure" "." 0 :weight 1)
      ;;       (tcl "grid" "rowconfigure" "." 0 :weight 1)
;      (tcl "event" "generate" "." "<<boom>>" :data "Blahbla")
      (tcl "grid" (tcl[ "canvas" ".c.c" :bg "grey") :column 4 :row 1 :sticky "we")

      (tcl "image" "create" "photo" "imgobj" :file "270970851.png")
      (tcl "grid" (tcl[ "label" ".c.l" :bg "grey") :column 1 :row 4 :sticky "we")
      ;; (tcl ".c.l" "configure" :image "imgobj")
      (tcl ".c.c" "create" "image" 100 100 :image "imgobj")


      (tcl "grid" (tcl[ "ttk::entry" ".c.feet" :width 7 :textvariable "feet") :column 2 :row 1 :sticky "we")
;;       (tcl "grid" (tcl[ "ttk::label" ".c.meters" :textvariable "meters") :column 2 :row 2 :sticky "we")
;;       (tcl "grid" (tcl[ "ttk::button" ".c.calc" :text "Calculate" :command "calculate") :column 3 :row 3 :sticky "w")
;;       (tcl "grid" (tcl[ "ttk::label" ".c.flbl" :text "feet") :column 3 :row 1 :sticky "w")
;;       (tcl "grid" (tcl[ "ttk::label" ".c.islbl" :text "is equivalent to") :column 1 :row 2 :sticky "e")
;;       (tcl "grid" (tcl[ "ttk::label" ".c.mlbl" :text "meters") :column 3 :row 2 :sticky "w")
;;       (tcl "foreach w [ winfo children .c ] {grid configure $w -padx 5 -pady 5}")
;;       (tcl "focus" ".c.feet")
;;       (tcl "bind" "." "<Return>" "{calculate}")
;;       (tcl "proc calculate {} {  
      (mainloop)
      )))

(defun sections (table &key (start 0) (end most-positive-fixnum))
  "Return list of distinct pairs of vnk, nnk found in table in
current database."
  (query (:limit (:order-by (:select 'vnk 'nnk
                                     :from table
                                     :group-by 'vnk 'nnk)
                            'vnk 'nnk)
                 (- end start) start)))

(defun stations (table vnk nnk step)
  "Return a list of plists of :longitude, :latitude, :station of
stations step metres apart between vnk and nnk."
  (query (:order-by (:select (:as (:st_x 'the-geom) 'longitude)
                             (:as (:st_y 'the-geom) 'latitude)
                             (:as 'nk-station 'station)
                             :from table
                             :where (:and (:= 'vnk vnk)
                                          (:= 'nnk nnk)
                                          (:= 0 (:% 'nk-station step))))
                    'nk-station)
         :plists))

(defun image-data (table vnk nnk step)
  "Return a list of instances of image data corresponding to stations,
which are step metres apart, found in table in current database."
  (let ((cache-file-name (image-data-pathname vnk nnk step)))
    (ensure-directories-exist cache-file-name)
    (with-open-file (stream cache-file-name
                            :direction :input
                            :if-does-not-exist nil)
      (if stream
          (read stream)
          (with-open-file (stream cache-file-name
                                  :direction :output)
            (prin1 (remove nil (mapcar #'(lambda (x)
                                           (apply #'image-data-point x))
                                       (stations table vnk nnk step)))
                   stream))))))

(defun cache-images (image-data)
  "Download images described in image data into their canonical places."
  (loop
     for i in image-data
     for (url path) = (multiple-value-list (image-url i))
     do (download-file url path)))

(defun image-data-point (&key longitude latitude station)
  "Get from Phoros server image data for location near longitude,
latitude."
  (let ((image-data (phoros-nearest-image-data longitude latitude)))
    (when (image-data-p image-data)
      (setf (image-data-station image-data) station)
      (setf (image-data-station-longitude image-data) longitude)
      (setf (image-data-station-latitude image-data) latitude)
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


(defun phoros-nearest-image-data (longitude latitude)
  "Return a set of image-data."
  (multiple-value-bind (body status-code headers url stream must-close reason-phrase)
      (drakma:http-request (phoros-lib-url *phoros-url* "nearest-image-data")
                           :cookie-jar *phoros-cookies*
                           :method :post
                           :content-type "text/plain; charset=UTF-8"
                           :content (json:encode-json-plist-to-string (list :longitude longitude
                                                                        :latitude latitude
                                                                        :zoom 20
                                                                        :count 1
                                                                        :selected-restriction-ids #())))
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
    
    

(defstruct image-data
  ;; fasttrack auxiliary slots
  station
  station-longitude
  station-latitude
  ;; original Phoros image data slots
  usable
  recorded-device-id
  device-stage-of-life-id
  generic-device-id
  directory
  measurement-id
  filename
  byte-position
  point-id
  trigger-time
  longitude
  latitude
  ellipsoid-height
  cartesian-system
  east-sd
  north-sd
  height-sd
  roll
  pitch
  heading
  roll-sd
  pitch-sd
  heading-sd
  sensor-width-pix
  sensor-height-pix
  pix-size
  bayer-pattern
  color-raiser
  mounting-angle
  dx
  dy
  dz
  omega
  phi
  kappa
  c
  xh
  yh
  a-1
  a-2
  a-3
  b-1
  b-2
  c-1
  c-2
  r-0
  b-dx
  b-dy
  b-dz
  b-rotx
  b-roty
  b-rotz
  b-ddx
  b-ddy
  b-ddz
  b-drotx
  b-droty
  b-drotz
  distance)

(defun plist-from-alist (alist)
  (loop
     for (key . value) in alist
     collect key
     collect value))

(defun image-url (image-data)
  "Return an image URL made from ingredients found in image-data, and
the corresponding cache path."
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
                           :type cache-type))))

(defun image-data-pathname (vnk nnk step)
  "Return pathname of a cached set of image data between vnk and nnk,
step metres apart."
  (make-pathname :directory *cache-dir*
                 :name (format nil "~A_~A_~D" vnk nnk step)
                 :type "image-data"))