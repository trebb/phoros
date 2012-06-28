(in-package #:phoros-fasttrack)

(defvar *postgresql-aux-credentials* nil
  "A list: (database user password host &key (port 5432) use-ssl).")

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


;; (with-connection '("phoros_aux" "postgres" "ser,!db" "db2")
;;   (query (:limit (:select 'vnk 'nnk 'nk-station
;;                          :from 'bew-landstr-kleinpunkte)
;;                 10)))

(defun sections (table &key (start 0) (end most-positive-fixnum))
  "Return list of distinct pairs of vnk, nnk found in table in
current database."
  (query (:limit (:order-by (:select 'vnk 'nnk
                                     :from table
                                     :group-by 'vnk 'nnk)
                            'vnk 'nnk)
                 (- end start) start)))

(defun station (table vnk nnk &optional station)
  "Return longitude and latitude of point at station between vnk and
nnk, and its station.  Return values of last station if station isn't
given."
  (values-list
   (if station
       (query (:select (:st_x 'the-geom) (:st_y 'the-geom) 'nk-station
                       :from table
                       :where (:and (:= 'vnk vnk)
                                    (:= 'nnk nnk)
                                    (:= 'nk-station station)))
              :row)
       (query (:limit (:order-by (:select (:st_x 'the-geom) (:st_y 'the-geom) 'nk-station
                                          :from table
                                          :where (:and (:= 'vnk vnk)
                                                       (:= 'nnk nnk)))
                                 (:desc 'nk-station))
                      1)
              :row))))

(defun image-data (table vnk nnk station)
  "Get from Phoros server image data for location found for vnk, nnk,
station in table in current database."
  (multiple-value-bind (longitude latitude)
      (station table vnk nnk station)
    (phoros-nearest-image-data longitude latitude)))

(define-condition phoros-server-error (error)
   ((body :reader body :initarg :body)
    (status-code :reader status-code :initarg :status-code)
    (headers :reader headers :initarg :headers)
    (uri :reader uri :initarg :uri)
    (reason-phrase :reader reason-phrase :initarg :reason-phrase))
  (:report (lambda (condition stream)
             (format stream "Can't connect to Phoros server: ~A (~D)"
                     (reason-phrase condition) (status-code condition)))))

(defvar *phoros-cookies* nil)
(defvar *phoros-uri* nil)

(defun phoros-login (url user-name user-password)
  "Log into Phoros server; return T if successful.  Try logging out
first."
  (setf *phoros-uri* (puri:parse-uri url))
  (setf drakma:*allow-dotless-cookie-domains-p* t)
  (setf drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*))
  (phoros-logout)
  (setf *phoros-cookies* (make-instance 'drakma:cookie-jar))
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request *phoros-uri* :cookie-jar *phoros-cookies*)
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :uri uri :reason-phrase reason-phrase)
    (multiple-value-bind (body status-code headers authenticate-uri stream must-close reason-phrase)
        (drakma:http-request (phoros-lib-uri *phoros-uri* "authenticate")
                             :cookie-jar *phoros-cookies*
                             :form-data t
                             :method :post
                             :parameters (pairlis '("user-name" "user-password")
                                                  (list user-name user-password)))
      (declare (ignore stream must-close))
      (assert (< status-code 400) ()
              'phoros-server-error :body body :status-code status-code :headers headers :uri authenticate-uri :reason-phrase reason-phrase)
      (= status-code 302))))

(defun phoros-logout ()
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (phoros-lib-uri *phoros-uri* "logout")
                           :cookie-jar *phoros-cookies*)
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :uri uri :reason-phrase reason-phrase)))


(defun phoros-nearest-image-data (longitude latitude)
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (phoros-lib-uri *phoros-uri* "nearest-image-data")
                           :cookie-jar *phoros-cookies*
                           :method :post
                           :content-type "text/plain; charset=UTF-8"
                           :content (json:encode-json-plist-to-string (list :longitude longitude
                                                                        :latitude latitude
                                                                        :zoom 11
                                                                        :count 1
                                                                        :selected-restriction-ids #())))
    (declare (ignore stream must-close))
    (assert (= status-code 200) ()
            'phoros-server-error :body body :status-code status-code :headers headers :uri uri :reason-phrase reason-phrase)
    (json:decode-json-from-string body)))
