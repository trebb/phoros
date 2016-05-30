(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; There are two alternative means of image creation:
  ;; zpng and cl-png.
  ;; zpng is faster and has no dependencies, but it makes images twice
  ;; as big as cl-png.  cl-png depends on libpng.so.
  ;; 
  ;; To choose zpng, leave *features* alone.  To choose cl-png, do:
  ;; (pushnew :phoros-uses-cl-png *features*)
  )

(defsystem :phoros

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

  "13.10.2"

  :licence                              ;goes with --licence output
  "Copyright (C) 2010, 2011, 2012, 2015 Bert Burgemeister

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

  :components ((:file #+build-phoros "package-phoros"
                      #+build-fasttrack "package-fasttrack")
               #+build-phoros (:file "util")
               (:file "proj4-sh")
               #+build-phoros (:file "log")
               (:file "photogrammetry")
               #+build-phoros (:file "indent-json")
               #+build-phoros (:file "phoros")
               #+build-phoros (:file "css")
               #+build-phoros (:file "cli")
               #+build-phoros (:file "phoros-js")
               #+build-phoros (:file "blurb")
               #+build-phoros (:file "db-tables")
               #+build-phoros (:file "stuff-db")
               #+build-phoros (:file "image-reader")
               #+build-fasttrack (:file "fasttrack"))

  :depends-on (:phoml
               #+build-phoros :swank
               #+build-phoros :sb-daemon
               :trivial-shell           ;for proj4-sh
               #+build-phoros :cl-ppcre
               #+build-phoros :hunchentoot
               #+build-phoros :cl-who
               #+build-phoros :parenscript
               :cl-json
               :postmodern
               #+build-phoros :simple-date
               #+(and build-phoros (not phoros-uses-cl-png)) :zpng
               #+(and build-phoros phoros-uses-cl-png) :png
               :drakma
               #+build-phoros :net.didierverna.clon
               :cl-utilities
               :parse-number
               #+build-phoros :named-readtables
               #+build-phoros :cl-log
               #+build-phoros :trivial-backtrace
               #+build-fasttrack :lisp-magick-wand))

