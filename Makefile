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
LIBPHOML_DIR = phoml/lib
LIBPHOML = libphoml.so
PRISTINE_OPENLAYERS_DIR = OpenLayers-2.10
OPENLAYERS_DIR = ol		#for compiled/shrunk OpenLayers
OPENLAYERS_JS = ol/OpenLayers.js
OPENLAYERS_THEME = ol/theme
OPENLAYERS_IMG = ol/img
SERVER_CSS = css/style.css
LOGO = public_html/phoros-logo-plain.png
FAVICON = public_html/favicon.ico
INDEX_HTML = public_html/index.html
PUBLIC_CSS = public_html/style.css
PHOROS_VERSION = $(shell ./phoros --version)
PHOROS_HELP_OUTPUT = phoros-help.txt
SOURCE = *.lisp *.asd Makefile

phoros : $(SOURCE) $(LIBPHOML_DIR)/$(LIBPHOML) $(OPENLAYERS_JS) $(OPENLAYERS_THEME) $(OPENLAYERS_IMG)
	$(LISP) --load make.lisp

$(OPENLAYERS_JS) : $(PRISTINE_OPENLAYERS_DIR)/build/OpenLayers.js
	mkdir -p $(OPENLAYERS_DIR) && cp $< $@

$(PRISTINE_OPENLAYERS_DIR)/build/OpenLayers.js : $(PRISTINE_OPENLAYERS_DIR)/lib/*
	cd $(PRISTINE_OPENLAYERS_DIR)/build && ./build.py full.cfg

.INTERMEDIATE : $(PRISTINE_OPENLAYERS_DIR)/build/OpenLayers.js

$(OPENLAYERS_THEME) : $(PRISTINE_OPENLAYERS_DIR)/theme $(OPENLAYERS_JS)
	cp -R $< $@

$(OPENLAYERS_IMG) : $(PRISTINE_OPENLAYERS_DIR)/img $(OPENLAYERS_JS)
	cp -R $< $@

$(LIBPHOML_DIR)/$(LIBPHOML) :
	cd phoml; $(MAKE)

public_html :
	mkdir -p public_html

$(LOGO) : Makefile public_html
	 convert \
		-size 113x125 xc:transparent \
		-font Gentium-Regular \
		-pointsize 200 -gravity center -draw "text 3,3 'Φ'" \
		-pointsize 57 -gravity center -draw "text 23,2 'Σ'" \
		$@
# Font Gentium-Regular is in Debian package ttf-sil-gentium.

$(FAVICON) : favicon.png
	icotool -c -o $@ $<

favicon.png : $(LOGO)
	convert $< -resize 16x16 $@

.INTERMEDIATE : favicon.png $(PHOROS_HELP_OUTPUT)

$(PHOROS_HELP_OUTPUT) : phoros
	./phoros --help > $@

$(INDEX_HTML) : doc/index.org $(PHOROS_HELP_OUTPUT) $(LOGO)
	emacs --batch --visit=$< --funcall org-export-as-html-batch \
	&& mv doc/index.html $@

$(PUBLIC_CSS) : doc/style.css public_html
	cp $< $@

tarball : phoros TimeSteps.history README \
          $(SERVER_CSS) $(OPENLAYERS_DIR) \
          $(LOGO) $(FAVICON) $(LIBPHOML_DIR)/$(LIBPHOML)
	tar -cf - \
		--transform='s,^,phoros-$(PHOROS_VERSION)/,' \
		phoros TimeSteps.history README \
		$(SERVER_CSS) $(OPENLAYERS_DIR) \
		$(LOGO) $(FAVICON) --directory=$(LIBPHOML_DIR) \
		$(LIBPHOML) \
		| gzip -f \
		> phoros-$(PHOROS_VERSION)-bin.tar.gz

html : $(INDEX_HTML) $(PUBLIC_CSS) $(FAVICON)

git-tag : phoros
	git tag -a $(PHOROS_VERSION) -m ""

clean :
	rm -f *.fasl *.log phoros phoros*.tar.gz $(LOGO) $(FAVICON) $(PHOROS_HELP_OUTPUT) $(INDEX_HTML) $(PUBLIC_CSS)
	rm -rf $(OPENLAYERS_DIR)

.PHONY : tarball html git-tag clean
