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

@GNU@ include $(top_srcdir)/silent-rules.mk

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST =
BUILT_SOURCES =

DISTCLEANFILES += Makefile
DISTCLEANFILES += GNUmakefile

#---------------------------------------------------------------------

#
# TinySCHEME as a scripting language for the Makefiles. (We do not
# wish to force people to link their GNU Make with libguile, nor to
# assume one has some particular Scheme implementation.)
#

EXTRA_DIST += BUILDING
EXTRA_DIST += COPYING
EXTRA_DIST += dynload.h
EXTRA_DIST += init.scm
EXTRA_DIST += MiniSCHEMETribute.txt
EXTRA_DIST += scheme.c
EXTRA_DIST += scheme-private.h
EXTRA_DIST += CHANGES
EXTRA_DIST += dynload.c
EXTRA_DIST += hack.txt
EXTRA_DIST += makefile.ORIGINAL
EXTRA_DIST += Manual.txt
EXTRA_DIST += opdefines.h
EXTRA_DIST += scheme.h
EXTRA_DIST += SConstruct

TINYSCHEME_C = $(addsuffix .c, scheme)
TINYSCHEME_H = $(addsuffix .h, dynload opdefines \
                               scheme scheme-private)
TINYSCHEME_CFLAGS = \
	${CFLAGS} $(CFLAGS_FOR_STATIC) $(CFLAGS_FOR_PIC) \
	-DUSE_DL=0 -DUSE_MATH=1 -DUSE_ASCII_NAMES=0

tinyscheme-exe$(EXEEXT): $(TINYSCHEME_C) $(TINYSCHEME_H)
	$(call v,CC) $(CC) -o $(@) \
	  $(TINYSCHEME_CFLAGS) $(TINYSCHEME_C) -lm

CLEANFILES += tinyscheme-exe$(EXEEXT)
CLEANFILES += *.$(OBJEXT)

#---------------------------------------------------------------------
# local variables:
# coding: utf-8
# end:
