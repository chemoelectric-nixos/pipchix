# Copyright © 2025 Barry Schwartz
#
# This file is part of Pipchix.
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

SUBDIRS = tinyscheme-1.42 . tests

ACLOCAL_AMFLAGS = -I m4 --install

AUTOMAKE_OPTIONS =
AUTOMAKE_OPTIONS += foreign

@GNU@ include $(top_srcdir)/silent-rules.mk

CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST =
EXTRA_PROGRAMS =
EXTRA_SCRIPTS =
BUILT_SOURCES =
nodist_r6rspipchix_DATA =
nodist_r7rspipchix_DATA =

EXTRA_DIST += silent-rules.mk
EXTRA_DIST += COPYING
EXTRA_DIST += README.adoc
EXTRA_DIST += INSTALL
EXTRA_DIST += AUTHORS

DISTCLEANFILES += Makefile
DISTCLEANFILES += GNUmakefile

M4_PREPROCESS = $(GNU_M4) -P -I '$(srcdir)'

@GNU@ define copy-file
@GNU@ $(call v,COPY) cp -f $(1) $(2)
@GNU@ endef

CLEANFILES += tmp.*

PIPCHXINC =
PIPCHXINC += $(srcdir)/pipchix/pipchix-includes.m4
EXTRA_DIST += $(PIPCHXINC)

R6RS_SCHEME =
R6RS_SCHEME += r6rs/pipchix/general-purpose/srfi-39.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/box.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/cut.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/continuation-capture.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/list.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/division.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/match.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/base64.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/gensym.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/define-record-factory.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/e-macros-environment.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/e-macros.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/eager-syntax.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/eager-macros.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/ec.sls
R6RS_SCHEME += r6rs/pipchix/general-purpose/combinators.sls
R6RS_SCHEME += r6rs/pipchix/generate-identifier.sls
R6RS_SCHEME += r6rs/pipchix/essentials.sls
R6RS_SCHEME += r6rs/pipchix/expressions.sls
R6RS_SCHEME += r6rs/pipchix/macros.sls
R6RS_SCHEME += r6rs/pipchix/nix-embed.sls
R6RS_SCHEME += r6rs/pipchix/nix-list.sls
R6RS_SCHEME += r6rs/pipchix/macros/nix-set.sls
R6RS_SCHEME += r6rs/pipchix/macros/nix-letrec.sls
R6RS_SCHEME += r6rs/pipchix/macros/nix-lambda.sls
R6RS_SCHEME += r6rs/pipchix/abstract-syntax-tree.sls
R6RS_SCHEME += r6rs/pipchix/string-manipulation.sls
EXTRA_DIST += $(R6RS_SCHEME:%=%.m4)
EXTRA_DIST += $(R6RS_SCHEME:r6rs/pipchix/%.sls=pipchix/%.m4)
EXTRA_DIST += $(R6RS_SCHEME:r6rs/pipchix/%.sls=pipchix/%.exports.m4)
CLEANFILES += $(R6RS_SCHEME)
CLEANFILES += $(R6RS_SCHEME:%=%.tmp)
if INSTALL_R6RS_SOURCE
nodist_r6rspipchix_DATA += $(R6RS_SCHEME)
endif

R7RS_SCHEME =
R7RS_SCHEME += r7rs/pipchix/general-purpose/box.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/cut.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/continuation-capture.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/list.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/division.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/match.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/base64.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/gensym.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/define-record-factory.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/e-macros-environment.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/e-macros.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/eager-syntax.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/eager-macros.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/ec.sld
R7RS_SCHEME += r7rs/pipchix/general-purpose/combinators.sld
R7RS_SCHEME += r7rs/pipchix/generate-identifier.sld
R7RS_SCHEME += r7rs/pipchix/essentials.sld
R7RS_SCHEME += r7rs/pipchix/expressions.sld
R7RS_SCHEME += r7rs/pipchix/macros.sld
R7RS_SCHEME += r7rs/pipchix/nix-embed.sld
R7RS_SCHEME += r7rs/pipchix/nix-list.sld
R7RS_SCHEME += r7rs/pipchix/macros/nix-set.sld
R7RS_SCHEME += r7rs/pipchix/macros/nix-letrec.sld
R7RS_SCHEME += r7rs/pipchix/macros/nix-lambda.sld
R7RS_SCHEME += r7rs/pipchix/abstract-syntax-tree.sld
R7RS_SCHEME += r7rs/pipchix/string-manipulation.sld
EXTRA_DIST += $(R7RS_SCHEME:%=%.m4)
EXTRA_DIST += $(R7RS_SCHEME:r7rs/pipchix/%.sld=pipchix/%.m4)
EXTRA_DIST += $(R7RS_SCHEME:r7rs/pipchix/%.sld=pipchix/%.exports.m4)
CLEANFILES += $(R7RS_SCHEME)
CLEANFILES += $(R7RS_SCHEME:%=%.tmp)
if INSTALL_R7RS_SOURCE
nodist_r7rspipchix_DATA += $(R7RS_SCHEME)
endif

$(builddir)/r6rs/pipchix/%.sls: $(srcdir)/r6rs/pipchix/%.sls.m4 \
	                        $(srcdir)/pipchix/%.m4 \
	                        $(srcdir)/pipchix/%.exports.m4 \
	                        $(PIPCHXINC)
	$(call v,SLS.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) -DRNRS_NUMBER=6 $(<) > $(@).tmp && \
		mv $(@){.tmp,}

$(builddir)/r7rs/pipchix/%.sld: $(srcdir)/r7rs/pipchix/%.sld.m4 \
	                        $(srcdir)/pipchix/%.m4 \
	                        $(srcdir)/pipchix/%.exports.m4 \
	                        $(PIPCHXINC)
	$(call v,SLD.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) -DRNRS_NUMBER=7 $(<) > $(@).tmp && \
		mv $(@){.tmp,}

#
# A way to get names for temporary files within the build directories.
# This is NOT for use in place of mktemp.
#
TEMPNAME=$(shell ${top_builddir}/tempname)
DISTCLEANFILES += ${top_builddir}/tempname

#---------------------------------------------------------------------

include tests-preparations.am

tests/run-r6rs-%.sh: tests/run-r6rs-%.sh.in
tests/run-r7rs-%.sh: tests/run-r7rs-%.sh.in

config.status: tests/run-r6rs-chez.sh.in

config.status: tests/run-r7rs-chibi.sh.in

config.status: tests/run-r7rs-gauche.sh.in

config.status: tests/run-r6rs-guile.sh.in
config.status: tests/run-r7rs-guile.sh.in

config.status: tests/run-r6rs-loko.sh.in
config.status: tests/run-r7rs-loko.sh.in

config.status: tests/run-r6rs-racket.sh.in

config.status: tests/run-r6rs-sagittarius.sh.in
config.status: tests/run-r7rs-sagittarius.sh.in

#---------------------------------------------------------------------

TINYSCHEME_DIR = tinyscheme-1.42
TINYSCHEME_EXE = $(TINYSCHEME_DIR)/tinyscheme-exe$(EXEEXT)

@GNU@ define tinyscheme-1
@GNU@ ( \
@GNU@   cd $(abs_top_builddir)/$(TINYSCHEME_DIR) && \
@GNU@   ./tinyscheme-exe$(EXEEXT) -1 $(1) \
@GNU@ )
@GNU@ endef

$(TINYSCHEME_EXE):
	$(call v,MAKE) \
	  $(MAKE) $(AM_MAKEFLAGS) \
	    -C $(TINYSCHEME_DIR) tinyscheme-exe$(EXEEXT)

#
# A pretty-printer (nicely-print obj) for TinySCHEME (or any other
# standard Scheme). It uses ’write’ for anything except a list. Lists
# it prints with newlines and indentation.
#
DEFINE_NICELY_PRINT = ' \
  \
  (define (indent-to depth) \
    (let loop ((i depth)) \
      (unless (zero? i) \
        (display " ") \
        (loop (- i 1))))) \
  \
  (define (nicely-print obj) \
    (%%nicely-print obj 0 \#f) \
    (newline)) \
  (define (%%nicely-print obj depth newline?) \
    (let ((indentation " ")) \
      (cond \
	((pair? obj) \
	  (when newline? \
	    (newline) \
	    (indent-to depth)) \
	  (display "(") \
	  (let loop ((lst obj) \
		     (separator "")) \
	    (cond \
	      ((pair? lst) \
		(unless (pair? (car lst)) \
		  (display separator)) \
		(%%nicely-print (car lst) (+ depth 1) \
				(not (equal? separator ""))) \
		(loop (cdr lst) indentation)) \
	      (else \
		(display ")"))))) \
	 (else \
	   (write obj)))))'

#---------------------------------------------------------------------
#
# ‘is-older’ is a program that augments Make itself in checking for
# whether files need rebuilding.
#

EXTRA_PROGRAMS += is-older
BUILT_SOURCES += is-older
DISTCLEANFILES += is-older

#---------------------------------------------------------------------
# local variables:
# coding: utf-8
# end:
