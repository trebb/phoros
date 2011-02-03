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
TAR = tar
GZIP = gzip
LIBPHOTOGRAMMETRIE_DIR = ../photogrammetrie/lib
LIBPHOTOGRAMMETRIE = libphotogrammetrie.so
SERVER_CSS = style.css
SERVER_JAVASCRIPT = openlayers/

SOURCE = *.lisp *.asd

phoros: $(SOURCE)
	$(LISP) --load make.lisp

phoros-bin.tar: phoros TimeSteps.history
	$(TAR) -cf $@ $^ $(SERVER_CSS) $(SERVER_JAVASCRIPT) -C $(LIBPHOTOGRAMMETRIE_DIR) $(LIBPHOTOGRAMMETRIE)

phoros-bin.tar.gz: phoros-bin.tar $(LIBPHOTOGRAMMETRIE_DIR)/$(LIBPHOTOGRAMMETRIE)
	rm -f $@
	$(GZIP) $<

clean:
	rm -f *.fasl *.log phoros phoros.tar.gz