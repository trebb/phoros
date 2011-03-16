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

LISP = ../sbcl/bin/sbcl
MEATWARE_DRIVER = echo
LIBPHOTOGRAMMETRIE_DIR = ../photogrammetrie/lib
LIBPHOTOGRAMMETRIE = libphotogrammetrie.so
SERVER_CSS = css/style.css
SERVER_JAVASCRIPT = openlayers/
LOGO = css/phoros-logo-plain.png
LOGO_CHROME = css/phoros-logo-chrome.png
PHOROS_VERSION = $(shell ./phoros --version)
SOURCE = *.lisp *.asd Makefile

phoros : $(SOURCE)
	$(LISP) --load make.lisp


$(LOGO) :
	 convert \
		-size 113x125 xc:transparent \
		-font Gentium-Regular \
		-pointsize 200 -gravity center -draw "text 3,3 'Φ'" \
		-pointsize 57 -gravity center -draw "text 23,2 'Σ'" \
		$@

$(LOGO_CHROME) : $(LOGO)
	$(MEATWARE_DRIVER) Go get GIMP and make $(LOGO_CHROME) from $<.
	false

tarball : phoros TimeSteps.history $(SERVER_CSS) $(SERVER_JAVASCRIPT) \
          $(LOGO) $(LOGO_CHROME) $(LIBPHOTOGRAMMETRIE_DIR)/$(LIBPHOTOGRAMMETRIE)
	tar -cf - \
		--transform='s,^,phoros-$(PHOROS_VERSION)/,' \
		phoros TimeSteps.history $(SERVER_CSS) $(SERVER_JAVASCRIPT) \
		$(LOGO) $(LOGO_CHROME) --directory=$(LIBPHOTOGRAMMETRIE_DIR) \
		$(LIBPHOTOGRAMMETRIE) \
		| gzip -f \
		> phoros-$(PHOROS_VERSION)-bin.tar.gz

clean :
	rm -f *.fasl *.log phoros phoros*.tar.gz $(LOGO)