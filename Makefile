# PHOROS -- Photogrammetric Road Survey
# Copyright (C) 2010, 2011, 2012 Bert Burgemeister
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
OPENLAYERS_TARBALL = OpenLayers-2.10.tar.gz
PRISTINE_OPENLAYERS_DIR = OpenLayers-2.10
EXAMPLES_DIR = examples
ETC_DIR = etc
OPENLAYERS_DIR = ol		#for compiled/shrunk OpenLayers
OPENLAYERS_CONFIG = phoros.cfg
OPENLAYERS_JS = ol/OpenLayers.js
OPENLAYERS_THEME = ol/theme
OPENLAYERS_IMG = ol/img
LOGO = public_html/phoros-logo-plain.png
BACKGROUND_IMAGE = public_html/phoros-logo-background.png
CURSOR_IMAGE = public_html/phoros-cursor.png
FAVICON = public_html/favicon.ico
INDEX_HTML = public_html/index.html
PHOROS_HELP_HTML = public_html/phoros--help.html
DEPLOYMENT_HTML = public_html/deployment.html
PUBLIC_CSS = public_html/style.css
NOT_FOUND_HTML = public_html/404.html
PHOROS_VERSION = $(shell ./phoros --version)
LATEST_TAG = $(shell git describe)
MACHINE_TYPE = $(shell uname -m)
PHOROS_HELP_OUTPUT = phoros-help.txt
SOURCE = *.lisp *.asd Makefile

phoros : $(SOURCE) photogrammetry_lib $(OPENLAYERS_JS) \
		$(OPENLAYERS_THEME) $(OPENLAYERS_IMG) \
		$(BACKGROUND_IMAGE) $(LOGO) $(FAVICON) $(CURSOR_IMAGE)
	CC=gcc \
	$(LISP) --lose-on-corruption \
		--disable-ldb \
		--dynamic-space-size 4096 \
		--end-runtime-options \
		--disable-debugger \
		--load make.lisp

fasttrack : $(SOURCE) photogrammetry_lib \
		$(BACKGROUND_IMAGE) $(LOGO) $(CURSOR_IMAGE)
	CC=gcc \
	$(LISP) --lose-on-corruption \
		--disable-ldb \
		--dynamic-space-size 4096 \
		--end-runtime-options \
		--disable-debugger \
		--load make-fasttrack.lisp

$(OPENLAYERS_TARBALL) :
	wget http://openlayers.org/download/$@

$(PRISTINE_OPENLAYERS_DIR)/lib/* : $(OPENLAYERS_TARBALL)
	tar -xmzf $<

$(OPENLAYERS_JS) : $(PRISTINE_OPENLAYERS_DIR)/build/OpenLayers.js
	mkdir -p $(OPENLAYERS_DIR) && cp $< $@

$(PRISTINE_OPENLAYERS_DIR)/build/OpenLayers.js : \
                  $(PRISTINE_OPENLAYERS_DIR)/lib/* $(OPENLAYERS_CONFIG)
	cp $(OPENLAYERS_CONFIG) $(PRISTINE_OPENLAYERS_DIR)/build/ && \
	cd $(PRISTINE_OPENLAYERS_DIR)/build && ./build.py $(OPENLAYERS_CONFIG)
######  cd $(PRISTINE_OPENLAYERS_DIR)/build && ./build.py full.cfg

.INTERMEDIATE : $(PRISTINE_OPENLAYERS_DIR)/build/OpenLayers.js

$(OPENLAYERS_THEME) : $(PRISTINE_OPENLAYERS_DIR)/theme $(OPENLAYERS_JS)
	cp -R $< $@

$(OPENLAYERS_IMG) : $(PRISTINE_OPENLAYERS_DIR)/img $(OPENLAYERS_JS)
	cp -R $< $@

photogrammetry_lib :
	cd phoml; $(MAKE)

public_html :
	mkdir -p public_html

$(LOGO) : Makefile public_html
	 ! convert \
		-size 113x125 xc:transparent \
		-font Gentium-Regular \
		-fill black \
		-pointsize 200 -gravity center -draw "text 3,3 'Φ'" \
		-pointsize 57 -gravity center -draw "text 23,2 'Σ'" \
		$@ 2>&1 | grep convert:

$(BACKGROUND_IMAGE) : Makefile public_html
	 ! convert \
		-size 113x125 xc:transparent \
		-font Gentium-Regular \
		-fill "#f5f5f5" \
		-pointsize 200 -gravity center -draw "text 3,3 'Φ'" \
		-pointsize 57 -gravity center -draw "text 23,2 'Σ'" \
		-resize 150% \
		$@ 2>&1 | grep convert:
$(CURSOR_IMAGE) : Makefile public_html
	 ! convert \
		-size 14x16 xc:transparent \
		-font Gentium-Regular \
		-fill Navy \
		-pointsize 22 -gravity center -draw "text 0.21,1 'Φ'" \
		-pointsize 7 -gravity center -draw "text 1.61,.1 'Σ'" \
		$@ 2>&1 | grep convert:
# Font Gentium-Regular is in Debian package ttf-sil-gentium.

$(FAVICON) : favicon.png
	icotool -c -o $@ $<

favicon.png : $(LOGO)
	convert $< -resize 16x16 $@

.INTERMEDIATE : favicon.png

$(PHOROS_HELP_OUTPUT) : phoros
	./phoros --help > $@

.INTERMEDIATE : $(PHOROS_HELP_OUTPUT)

$(INDEX_HTML) : doc/index.org $(LOGO)
	emacs --batch --visit=$< --funcall org-export-as-html-batch \
	&& mv doc/index.html $@

$(DEPLOYMENT_HTML) : doc/deployment.org $(LOGO)
	emacs --batch --visit=$< --funcall org-export-as-html-batch \
	&& mv doc/deployment.html $@

$(PHOROS_HELP_HTML) : doc/phoros--help.org $(PHOROS_HELP_OUTPUT) $(LOGO)
	emacs --batch --visit=$< --funcall org-export-as-html-batch \
	&& mv doc/phoros--help.html $@

$(PUBLIC_CSS) : doc/style.css public_html
	cp $< $@

$(NOT_FOUND_HTML) : doc/404.org $(LOGO)
	emacs --batch --visit=$< --funcall org-export-as-html-batch \
	&& mv doc/404.html $@

phoros-proper.tar :
	git archive --prefix=phoros_$(LATEST_TAG)/ --output=$@ $(LATEST_TAG)

phoml.tar :
	cd phoml \
	&& git archive --prefix=phoros_$(LATEST_TAG)/phoml/ --output=../$@ HEAD

.INTERMEDIATE : phoros-proper.tar phoml.tar

bin-tarball : phoros TimeSteps.history README				\
          $(EXAMPLES_DIR)						\
	  $(ETC_DIR)							\
          $(OPENLAYERS_DIR)						\
          $(BACKGROUND_IMAGE) $(LOGO) $(FAVICON) $(CURSOR_IMAGE)	\
	  $(LIBPHOML_DIR)/$(LIBPHOML)
	tar -cf -							\
		--transform='s,^,phoros_$(PHOROS_VERSION)/,'		\
		phoros TimeSteps.history README				\
                $(EXAMPLES_DIR)						\
		$(ETC_DIR)						\
		$(OPENLAYERS_DIR)					\
		$(BACKGROUND_IMAGE) $(LOGO) $(FAVICON) $(CURSOR_IMAGE)	\
		--directory=$(LIBPHOML_DIR) $(LIBPHOML)			\
		| gzip -f						\
		> phoros_$(PHOROS_VERSION)_$(MACHINE_TYPE).tar.gz

src-tarball : phoros-proper.tar phoml.tar
	tar --concatenate -f phoros_$(LATEST_TAG).tar $^ \
	&& gzip -f phoros_$(LATEST_TAG).tar

html : $(INDEX_HTML) $(DEPLOYMENT_HTML) $(PHOROS_HELP_HTML) $(PUBLIC_CSS) $(FAVICON) $(NOT_FOUND_HTML)

git-tag : phoros	    #tag name is :version string in phoros.asd
	git tag -a $(PHOROS_VERSION) -m ""

clean :
	rm -f *.fasl *.log phoros phoros*.tar.gz			\
		$(LOGO) $(BACKGROUND_IMAGE) $(FAVICON) $(CURSOR_IMAGE)	\
		$(PHOROS_HELP_OUTPUT)					\
                $(INDEX_HTML) $(DEPLOYMENT_HTML) 			\
                $(PUBLIC_CSS)
	rm -rf $(OPENLAYERS_DIR) $(PRISTINE_OPENLAYERS_DIR)
	cd phoml; $(MAKE) clean


.PHONY : bin-tarball src-tarball html git-tag clean


# Github

gh-publish:
	rm -rf gh-pages
	mkdir gh-pages
	$(MAKE) gh-pages/index.html		\
		gh-pages/deployment.html	\
		gh-pages/phoros--help.html	\
		gh-pages/phoros-12.8.1.png	\
		gh-pages/404.html		\
		gh-pages/CNAME			\
		gh-pages/README			\
		gh-pages/favicon.ico		\
		gh-pages/robots.txt		\
		gh-pages/style.css
	cd gh-pages; git init; git add ./; git commit -a -m "gh-pages pseudo commit"; git push git@github.com:trebb/phoros.git +master:gh-pages

gh-pages/%.html: public_html/%.html
	cp $< $@

gh-pages/favicon.ico: public_html/favicon.ico
	cp $< $@

gh-pages/%: doc/%
	cp $< $@
