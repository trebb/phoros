;;; PHOROS -- Photogrammetric Road Survey
;;; Copyright (C) 2011 Bert Burgemeister
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

(hunchentoot:define-easy-handler
    (blurb :uri "/phoros/lib/blurb")
    (openlayers-version)
  (when
      (hunchentoot:session-value 'authenticated-p)
    (who:with-html-output-to-string (s nil :indent t)
      (:html
       :xmlns "http://www.w3.org/1999/xhtml"
       (:head
        (:title "Phoros")
        (:link :rel "stylesheet"
               :href (format nil "/phoros/lib/css-~A/style.css"
                             (phoros-version))
               :type "text/css"))
       (:body 
        (:h1 :id "title" "Phoros: A Tool for Photogrammetric Road Survey")
        (:button :type "button"
                 :style "float:right"
                 :onclick (ps-inline (chain self (close)))
                 "close")
        (:p "This is "
            (:a :href "http://phoros.berlios.de"
                (:img :src "/phoros/lib/public_html/phoros-logo-plain.png"
                      :height 30 :style "vertical-align:middle"
                      :alt "Phoros"))
            (who:fmt "Phoros version ~A," (phoros-version))
            " a means for photogrammetric road survey written by"
            (:a :href "mailto:Bert Burgemeister <trebbu@googlemail.com>"
                "Bert Burgemeister."))
        (:p "Its photogrammetric workhorse is "
            (:a :href "mailto:Steffen.Scheller.home@gmail.com" "Steffen Scheller's")
            (who:fmt " library PhoML (version ~A)." (phoml:get-version-number)))
        (:a :style "float:left" :href "http://en.wikipedia.org/wiki/Common_Lisp"
            (:img :src "http://www.lisperati.com/lisplogo_128.png"
                  :alt "Common Lisp"))
        (:p (who:fmt
             "Phoros is implemented using Steel Bank Common Lisp (version ~A)"
             (lisp-implementation-version))
            ", an implementation of Common Lisp."
            (:a :href "http://www.sbcl.org"
                (:img :src "http://www.sbcl.org/sbclbutton.png"
                      :height 30 :style "vertical-align:middle"
                      :alt "SBCL")))
        (:p "You are communicating with "
            (:a :href "http://weitz.de/hunchentoot"
                (:img :src "http://www.htg1.de/hunchentoot/hunchentoot11.png"
                      :height 30 :style "vertical-align:middle"
                      :alt "Hunchentoot"))
            ", a Common Lisp web server.")
        (:p "Most of the client code running in your browser is or uses"
            (:a :href "http://openlayers.org"
                (:img :src "http://www.openlayers.org/images/OpenLayers.trac.png"
                      :height 30 :style "vertical-align:middle"
                      :alt "OpenLayers"))
            (who:fmt " ~A." (string-trim " " (remove #\$ openlayers-version))))
        (:p "Phoros stores data in a"
            (:a :href "http://postgresql.org"
                (:img :src "http://www.postgresql.org/files/community/propaganda/32x32_1.gif"
                      :height 30 :style "vertical-align:middle"
                      :alt "PostgreSQL"))
            (who:fmt " ~{~A (v~A)~}"
                     (with-connection *postgresql-credentials*
                       (cl-utilities:split-sequence
                        #\Space
                        (query (:select (:version)) :single)
                        :count 2)))
             " database that is spatially enabled by "
            (:a :href "http://postgis.refractions.net"
                (:img :src "http://postgis.refractions.net/download/logo_suite/stock_text/stock_text_180.gif"
                      :height 30 :style "vertical-align:middle"
                      :alt "PostGIS"))
            (who:fmt "version ~A"
                     (car (cl-utilities:split-sequence
                           #\Space
                           (with-connection *postgresql-credentials*
                             (query
                              (:select (:postgis_version))
                              :single)))))
            "."
            )
        (:h2 "Command Line Interface")
        (:p "Most of the administrative tasks are done through the
        command line interface. The output of")
        (:code "./phoros --help")
        (:p "is given below for reference.")
        (:pre (who:str (who:escape-string-minimal
                        (with-output-to-string (*standard-output*)
                          (cli:help-action))))))))))
