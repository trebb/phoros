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
SERVER_CSS = style.css
SERVER_JAVASCRIPT = openlayers/
LOGOS = phoros-logo-plain.png phoros-logo-chrome.png
PHOROS_VERSION = $(shell ./phoros --version)
SOURCE = *.lisp *.asd Makefile

phoros : $(SOURCE)
	$(LISP) --load make.lisp

$(LOGOS) : phoros-logo-plain.xcf
	$(MEATWARE_DRIVER) Go get GIMP and make $(LOGOS) from $<.
	false

tarball : phoros TimeSteps.history $(SERVER_CSS) $(SERVER_JAVASCRIPT) \
          $(LOGOS) $(LIBPHOTOGRAMMETRIE_DIR)/$(LIBPHOTOGRAMMETRIE)
	tar -cf - \
		--transform='s,^,phoros-$(PHOROS_VERSION)/,' \
		phoros TimeSteps.history $(SERVER_CSS) $(SERVER_JAVASCRIPT) \
		$(LOGOS) --directory=$(LIBPHOTOGRAMMETRIE_DIR) \
		$(LIBPHOTOGRAMMETRIE) \
		| gzip -f \
		> phoros-$(PHOROS_VERSION)-bin.tar.gz

clean :
	rm -f *.fasl *.log phoros phoros*.tar.gz