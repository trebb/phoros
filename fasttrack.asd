(defsystem :fasttrack

  :description                 ;goes with --version --verbose=1 output
  "PHOROS (Photogrammetric Road Survey)"

  :author "Bert Burgemeister <trebbu@googlemail.com>"

  :maintainer "Bert Burgemeister <trebbu@googlemail.com>"

  :long-description                     ;goes with --help output
  "Phoros (http://phoros.boundp.org) is a tool for photogrammetric
road survey.  It stores data in a PostgreSQL database and then makes
it available over a web interface."

  :version                              ;goes with --version output
  ;; :version is MAJOR.MINOR.REVISION where
  ;; - different MAJOR means data incompatibility,
  ;; - different MINOR means changed feature set,
  ;; - different REVISION means any other change.
  ;; 
  ;; There should be a corresponding git tag which marks the point this
  ;; version number becomes official.

  "13.8.2"

  :licence                              ;goes with --licence output
  "Copyright (C) 2010, 2011, 2012 Bert Burgemeister

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA."

  :serial t

  :components ((:file "package")
               (:file "proj4-sh")
               (:file "fasttrack")
               (:file "photogrammetry"))

  :depends-on (:phoml
               :sb-daemon
               :cl-tk
               :lisp-magick
               :trivial-shell           ;for proj4-sh
               :cl-ppcre
               :hunchentoot
               :cl-who
               :parenscript
               :cl-json
               :postmodern
               :simple-date
               #-phoros-uses-cl-png :zpng
               #+phoros-uses-cl-png :png
               :drakma
               :com.dvlsoft.clon
               :cl-utilities
               :parse-number
               :named-readtables
               :cl-log
               :trivial-backtrace))
