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

(define-easy-handler (blurb :uri "/blurb") ()
  (when
   (session-value 'authenticated-p)
   (who:with-html-output-to-string (s nil :indent t)
     (:html
      :xmlns "http://www.w3.org/1999/xhtml"
      (:head
       (:title "Phoros")
       (:link :rel "stylesheet" :href "lib/style.css" :type "text/css"))
      (:body 
       (:h1 :id "title" "Phoros")
       (:p :id "shortdesc"
           "Photogrammetric Road Survey")
       (:div :id "logout-button" :style "float:left" (:button :type "button" :onclick "self.location.href = \"/logout\"" "bye"))
             
       (:div :style "clear:both"
             (:div :style "float:left")
             (:a :href "http://www.sbcl.org" (:img :src "/lib/phoros-logo-plain.png" :alt "Phoros")) (format s "Phoros version ~A " (phoros-version))
             (format s "Photogrammetrie version ~A" (photogrammetrie:get-version-number))
             (:a :href "http://www.sbcl.org" (:img :src "http://www.sbcl.org/sbclbutton.png" :alt "SBCL"))
             (:a :href "http://en.wikipedia.org/wiki/Common_Lisp" (:img :src "http://www.lisperati.com/lisplogo_128.png" :alt "Common Lisp"))
             (:a :href "http://weitz.de/hunchentoot" (:img :src "http://www.htg1.de/hunchentoot/hunchentoot11.png" :alt "Hunchentoot"))
             (:a :href "http://openlayers.org" (:img :src "http://www.openlayers.org/images/OpenLayers.trac.png" :alt "OpenLayers"))
             (:a :href "http://postgresql.org" (:img :src "http://www.postgresql.org/files/community/propaganda/32x32_1.gif" :alt "PostgreSQL"))
             (:a :href "http://postgis.refractions.net" (:img :src "http://postgis.refractions.net/download/logo_suite/stock_text/stock_text_180.gif" :alt "PostGIS"))
             ))))))

