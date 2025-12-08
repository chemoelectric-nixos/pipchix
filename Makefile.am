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
nodist_chickeneggs5pipchix_DATA =

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

EXTRA_DIST += pipchix/pipchix-includes.m4

if INSTALL_CHICKEN_5_EGG_SOURCE
nodist_chickeneggs5pipchix_DATA += $(CHICKEN_5_EGGFILES)
endif

R6RS_SCHEME =
R6RS_SCHEME += r6rs/pipchix/srfi-9.sls
R6RS_SCHEME += r6rs/pipchix/srfi-39.sls
R6RS_SCHEME += r6rs/pipchix/srfi-111.sls
R6RS_SCHEME += r6rs/pipchix/continuation-capture.sls
R6RS_SCHEME += r6rs/pipchix/define-record-factory.sls
R6RS_SCHEME += r6rs/pipchix/base.sls
R6RS_SCHEME += r6rs/pipchix/nix-embed.sls
R6RS_SCHEME += r6rs/pipchix/nix-list.sls
R6RS_SCHEME += r6rs/pipchix/nix-set.sls
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
R7RS_SCHEME += r7rs/pipchix/srfi-111.sld
R7RS_SCHEME += r7rs/pipchix/continuation-capture.sld
R7RS_SCHEME += r7rs/pipchix/define-record-factory.sld
R7RS_SCHEME += r7rs/pipchix/base.sld
R7RS_SCHEME += r7rs/pipchix/nix-embed.sld
R7RS_SCHEME += r7rs/pipchix/nix-list.sld
R7RS_SCHEME += r7rs/pipchix/nix-set.sld
R7RS_SCHEME += r7rs/pipchix/abstract-syntax-tree.sld
R7RS_SCHEME += r7rs/pipchix/string-manipulation.sld
EXTRA_DIST += $(R7RS_SCHEME:%=%.m4)
EXTRA_DIST += $(R7RS_SCHEME:r7rs/pipchix/%.sld=pipchix/%.m4)
EXTRA_DIST += $(R7RS_SCHEME:r7rs/pipchix/%.sld=pipchix/%.exports.m4)
CLEANFILES += $(R7RS_SCHEME)
CLEANFILES += $(R7RS_SCHEME:%=%.tmp)
if INSTALL_R7RS_SOURCE
nodist_r7rspipchix_DATA += $(R7RS_SCHEME)
nodist_r7rspipchix_DATA += r7rs/gerbil.pkg
endif

$(builddir)/r6rs/pipchix/%.sls: $(srcdir)/r6rs/pipchix/%.sls.m4 \
	                        $(srcdir)/pipchix/%.m4 \
	                        $(srcdir)/pipchix/%.exports.m4 \
	                        $(srcdir)/pipchix/pipchix-includes.m4
	$(call v,SLS.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

$(builddir)/r7rs/pipchix/%.sld: $(srcdir)/r7rs/pipchix/%.sld.m4 \
	                        $(srcdir)/pipchix/%.m4 \
	                        $(srcdir)/pipchix/%.exports.m4 \
	                        $(srcdir)/pipchix/pipchix-includes.m4
	$(call v,SLD.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

r7rs/gerbil.pkg: Makefile.am	# Gerbil has to be told to be R⁷RS.
	$(call v,PRINTF) \
	    $(MKDIR_P) $(@D) && \
	    printf '(prelude: :scheme/r7rs)' > $(@).tmp && \
	    mv $(@){.tmp,}

check: r7rs/gerbil.pkg
DISTCLEANFILES += r7rs/gerbil.pkg

#
# A way to get names for temporary files within the build directories.
# This is NOT for use in place of mktemp.
#
TEMPNAME=$(shell ${top_builddir}/tempname)
DISTCLEANFILES += ${top_builddir}/tempname

#---------------------------------------------------------------------
#
# CHICKEN Scheme support.
#

CHICKEN_EGGSPEC_TOP = ' \
  ((synopsis "Use Scheme to write Nix code") \
   (version "$(PACKAGE_VERSION)") \
   (author "Barry Schwartz") \
   (category code-generation) \
   (license "MIT")'

CHICKEN_EGGSPEC_BOTTOM = ' \
   (components \
    (extension pipchix.srfi-111) \
    (extension pipchix.continuation-capture) \
    (extension pipchix.define-record-factory) \
    (extension pipchix.string-manipulation) \
    (extension pipchix.abstract-syntax-tree \
      (component-dependencies \
       pipchix.define-record-factory)) \
    (extension pipchix.nix-embed \
      (component-dependencies \
       pipchix.abstract-syntax-tree)) \
    (extension pipchix.nix-list \
      (component-dependencies \
       pipchix.abstract-syntax-tree)) \
    (extension pipchix.nix-set \
      (component-dependencies \
       pipchix.abstract-syntax-tree)) \
    (extension pipchix.base \
      (component-dependencies \
       pipchix.string-manipulation \
       pipchix.abstract-syntax-tree \
       pipchix.nix-list \
       pipchix.nix-set))))'

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

$(builddir)/$(CHICKEN_5_EGGDIR)/pipchix.%.scm: \
	                   $(srcdir)/r7rs/pipchix/%.sld.m4 \
	                   $(srcdir)/pipchix/%.m4 \
	                   $(srcdir)/pipchix/pipchix-includes.m4
	$(call v,SCM.M4) $(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

CHICKEN_5_EGGS = chicken/eggs/5
CHICKEN_5_EGGDIR = $(CHICKEN_5_EGGS)/pipchix
CHICKEN_5_EGGFILES = \
  $(addprefix $(CHICKEN_5_EGGDIR)/, \
    pipchix.egg \
    pipchix.srfi-111.scm \
    pipchix.continuation-capture.scm \
    pipchix.define-record-factory.scm \
    pipchix.string-manipulation.scm \
    pipchix.abstract-syntax-tree.scm \
    pipchix.nix-embed.scm \
    pipchix.nix-list.scm \
    pipchix.nix-set.scm \
    pipchix.base.scm \
    COPYING README.adoc)
CLEANFILES += $(CHICKEN_5_EGGFILES)

$(CHICKEN_5_EGGDIR)/COPYING: COPYING
	$(call copy-file, $(<), $(@))

$(CHICKEN_5_EGGDIR)/README.adoc: README.adoc
	$(call copy-file, $(<), $(@))

$(CHICKEN_5_EGGDIR)/pipchix.egg: GNUmakefile $(TINYSCHEME_EXE)
	$(call v,SEXP) \
	script=$(TEMPNAME) && \
	$(MKDIR_P) $(@D) && \
	if true; then \
	  printf '%s' $(DEFINE_NICELY_PRINT); \
	  printf '(define eggfile (quote %s))' \
	                 $(CHICKEN_5_EGGSPEC); \
	  printf '(nicely-print eggfile)'; \
	fi > "$${script}" && \
	$(call tinyscheme-1, ${abs_builddir}/"$${script}") \
			> $(abs_builddir)/$(@) && \
	rm "$${script}"

CHICKEN_5_EGGSPEC = ' \
 '$(CHICKEN_EGGSPEC_TOP)' \
   (dependencies r7rs utf8 srfi-111) \
   (component-options \
    (csc-options \
     "-R" "utf8" \
     "-R" "r7rs" \
     "-X" "r7rs" \
     "-O2" "-d1" \
     "-compile-syntax")) \
 '$(CHICKEN_EGGSPEC_BOTTOM)

$(PACKAGE_TARNAME)-$(PACKAGE_VERSION).egg-5.tar.xz: $(CHICKEN_5_EGGFILES)
	( \
	  cd $(CHICKEN_5_EGGDIR) && \
	  tar --format=posix -chf - $(notdir $(CHICKEN_5_EGGFILES)) \
	     | Z_OPT=$${XZ_OPT--e} xz -c \
	) > $(@).tmp && mv $(@){.tmp,}

dist-chicken-egg-5: $(PACKAGE_TARNAME)-$(PACKAGE_VERSION).egg-5.tar.xz

if CHICKEN_INSTALL_5
chicken-install-5: $(CHICKEN_5_EGGFILES)
	( \
	  cd $(CHICKEN_5_EGGDIR) && \
	  env SUDO="$${INSTALL_AS}" $(CHICKEN_INSTALL_5) -s \
	)
endif CHICKEN_INSTALL_5

if CHICKEN_UNINSTALL_5
chicken-uninstall-5:
	env SUDO="$${INSTALL_AS}" \
	  $(CHICKEN_UNINSTALL_5) -s -force pipchix
endif CHICKEN_UNINSTALL_5

CLEANFILES += $(CHICKEN_5_EGGDIR)/*.{$(OBJEXT),so,link,import.scm}
CLEANFILES += $(CHICKEN_5_EGGDIR)/pipchix.{build,install}.sh

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if CSI_5
@GNU@define get-chicken-5-repository-path
@GNU@$(CSI_5) -n -b -R r7rs -R utf8-srfi-13 -R chicken.platform \
@GNU@    -e '(display (string-join (cons "$(abs_builddir)/$(CHICKEN_5_EGGDIR)" (repository-path)) ":"))'
@GNU@endef
else !CSI_5
@GNU@define get-chicken-5-repository-path
@GNU@true
@GNU@endef
endif !CSI_5

# The following may require that you have installed the eggs needed by
# Pipchix.
chicken-5-egg-stamp: $(CHICKEN_5_EGGFILES)
	@if ! test -e chicken-5-egg-stamp; then \
	  ( \
	    $(MKDIR_P) $(CHICKEN_5_EGGDIR) && \
	    cd $(CHICKEN_5_EGGDIR) && \
	    $(CHICKEN_INSTALL_5) -n \
	  ); \
	  touch $(@); \
	fi

CLEANFILES += chicken-5-egg-stamp

#---------------------------------------------------------------------

include tests-preparations.am

tests/run-r6rs-%.sh: tests/run-r6rs-%.sh.in
tests/run-r7rs-%.sh: tests/run-r7rs-%.sh.in

config.status: tests/run-r6rs-chez.sh.in

config.status: tests/run-r7rs-chibi.sh.in

config.status: tests/run-r7rs-csc-5.sh.in
config.status: tests/run-r7rs-csi-5.sh.in

# config.status: tests/run-r7rs-gsc.sh.in
config.status: tests/run-r7rs-gsi.sh.in

config.status: tests/run-r7rs-gauche.sh.in

config.status: tests/run-r7rs-gxi.sh.in
config.status: tests/run-r7rs-gxc.sh.in

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
# FIXME: FIGURE OUT HOW TO USE THIS! CURRENTLY I AM NOT USING IT FOR
#        ANYTHING. I TRIED BUT COULD NOT FIGURE OUT HOW TO MAKE IT
#        WORK. THE PROBLEM IS THAT THE CHICKEN EGG IS NOT REBUILT WHEN
#        TESTS ARE MODIFIED. PROBABLY is-older IS NOT NECESSARY, BUT
#        MAYBE IT WOULD BE HELPFUL.
#
#        In any case, what one must do to deal with the problem is do
#        a ‘make clean’ before running tests on the CHICKEN egg.
#

EXTRA_PROGRAMS += is-older
BUILT_SOURCES += is-older
DISTCLEANFILES += is-older

#---------------------------------------------------------------------
# local variables:
# coding: utf-8
# end:
