# Copyright © 2025 Barry Schwartz
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of Pipchix and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

.DELETE_ON_ERROR:

ACLOCAL_AMFLAGS = -I m4 --install
AUTOMAKE_OPTIONS =
AUTOMAKE_OPTIONS += foreign

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST =
BUILT_SOURCES =
r7rs_DATA =

EXTRA_DIST += COPYING
EXTRA_DIST += README.adoc
#EXTRA_DIST += INSTALL
#EXTRA_DIST += AUTHORS

r7rs_DATA += pipchix/base.sld
r7rs_DATA += pipchix/abstract-syntax-tree.sld

DISTCLEANFILES += Makefile
DISTCLEANFILES += GNUmakefile

# local variables:
# coding: utf-8
# end:
