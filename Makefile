# PHOROS -- Photogrammetric Road Survey
# Copyright (C) 2010, 2011 Bert Burgemeister
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

LISP = $(shell echo ../sbcl/bin/sbcl || which sbcl)
MEATWARE_DRIVER = echo
LIBPHOML_DIR = phoml/lib
LIBPHOML = libphotogrammetrie.so
SERVER_CSS = css/style.css
SERVER_JAVASCRIPT = openlayers/
LOGO = doc/phoros-logo-plain.png
LOGO_CHROME = css/phoros-logo-chrome.png
FAVICON = doc/favicon.ico
INDEX_HTML = doc/index.html
PHOROS_VERSION = $(shell ./phoros --version)
PHOROS_HELP_OUTPUT = doc/phoros-help.txt
SOURCE = *.lisp *.asd Makefile

phoros : $(SOURCE) $(LIBPHOML_DIR)/$(LIBPHOML)
	$(LISP) --load make.lisp

$(LIBPHOML_DIR)/$(LIBPHOML) :
	cd phoml; $(MAKE)

$(LOGO) : Makefile
	 convert \
		-size 113x125 xc:transparent \
		-font Gentium-Regular \
		-pointsize 200 -gravity center -draw "text 3,3 'Φ'" \
		-pointsize 57 -gravity center -draw "text 23,2 'Σ'" \
		$@
# Font Gentium-Regular is in Debian package ttf-sil-gentium.

$(FAVICON) : favicon.png
	icotool -c -o $@ $<

.INTERMEDIATE : favicon.png

favicon.png : $(LOGO)
	convert $< -resize 16x16 $@

$(LOGO_CHROME) : $(LOGO)
	$(MEATWARE_DRIVER) Go get GIMP and make $@ from $<.
	false

$(PHOROS_HELP_OUTPUT) : phoros
	./phoros --help > $@

$(INDEX_HTML) : doc/index.org $(PHOROS_HELP_OUTPUT) $(LOGO)
	emacs --batch --visit=$< --funcall org-export-as-html-batch

tarball : phoros TimeSteps.history $(SERVER_CSS) $(SERVER_JAVASCRIPT) \
          $(LOGO) $(LOGO_CHROME) $(LIBPHOML_DIR)/$(LIBPHOML)
	tar -cf - \
		--transform='s,^,phoros-$(PHOROS_VERSION)/,' \
		phoros TimeSteps.history $(SERVER_CSS) $(SERVER_JAVASCRIPT) \
		$(LOGO) $(LOGO_CHROME) --directory=$(LIBPHOML_DIR) \
		$(LIBPHOML) \
		| gzip -f \
		> phoros-$(PHOROS_VERSION)-bin.tar.gz

clean :
	rm -f *.fasl *.log phoros phoros*.tar.gz $(LOGO) $(FAVICON) $(PHOROS_HELP_OUTPUT) $(INDEX_HTML)
