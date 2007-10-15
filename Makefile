# $Id: Makefile,v 1.6 2003/09/30 13:44:23 ndim Exp $
#
# Makefile for prime number tests in misc languages
# Copyright (C) 2003 Hans Ulrich Niedermann <primes@n-dimensional.de>
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
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

TIME = /usr/bin/time
export TIME

########################################################################

.PHONY: build benchmark
build benchmark:
	$(MAKE) -C src $@

########################################################################
benchmark.html: analyze-benchmark.py $(wildcard results/*)
	python $< results/ > $@

########################################################################

.PHONY: clean distclean
clean:
	rm -rf build/

distclean: clean
	rm -rf results/


########################################################################
# distribution tarball
########################################################################

DISTFILES=Makefile COPYING README benchmark.mk src/ testcases/ ChangeLog
DISTDIR:="primenumbers-$(shell date -I)"

.PHONY: dist
dist: $(DISTDIR).tar.gz

$(DISTDIR).tar.gz: $(DISTFILES)
	rm -rf $(DISTDIR)
	mkdir $(DISTDIR)
	cp -rl $(DISTFILES) $(DISTDIR)
	tar cvfz $(DISTDIR).tar.gz \
		--exclude='*~' --exclude=CVS \
		$(DISTDIR)
	rm -rf $(DISTDIR)

# This isn't a real GNU style changelog yet, but it is still
# better than nothing.
.PHONY: ChangeLog
ChangeLog:
	tla changelog > "$@"