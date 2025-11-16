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

SUBDIRS = tinyscheme-1.42

ACLOCAL_AMFLAGS = -I m4 --install
AUTOMAKE_OPTIONS =
AUTOMAKE_OPTIONS += foreign

@GNU@ include $(top_srcdir)/silent-rules.mk

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST =
BUILT_SOURCES =
nodist_r7rs_DATA =

EXTRA_DIST += silent-rules.mk
EXTRA_DIST += COPYING
EXTRA_DIST += README.adoc
EXTRA_DIST += INSTALL
EXTRA_DIST += AUTHORS

DISTCLEANFILES += Makefile
DISTCLEANFILES += GNUmakefile

EXTRA_DIST += pipchix-includes.m4
EXTRA_DIST += $(addsuffix .m4, $(nodist_r7rs_DATA))

nodist_r7rs_DATA += pipchix/base.sld
nodist_r7rs_DATA += pipchix/nix-embed.sld
nodist_r7rs_DATA += pipchix/nix-list.sld
nodist_r7rs_DATA += pipchix/abstract-syntax-tree.sld

MOSTLYCLEANFILES += $(nodist_r7rs_DATA)
MOSTLYCLEANFILES += $(addsuffix .tmp, $(nodist_r7rs_DATA))

M4_PREPROCESS = $(GNU_M4) -P -I '$(srcdir)'

%.sld: %.sld.m4 pipchix-includes.m4
	$(call v,SLD.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

@GNU@ define copy-file
@GNU@ $(call v,COPY) cp -f $(1) $(2)
@GNU@ endef

MOSTLYCLEANFILES += tmp.*

#---------------------------------------------------------------------
#
# CHICKEN Scheme support.
#

#removing_egg_scripts = yes
removing_egg_scripts = no

CHICKEN_EGGSPEC_TOP = ' \
  ((synopsis "Use Scheme to write Nix code") \
   (version "$(PACKAGE_VERSION)") \
   (author "Barry Schwartz") \
   (category code-generation) \
   (license "MIT")'

CHICKEN_EGGSPEC_BOTTOM = ' \
   (components \
    (extension pipchix.abstract-syntax-tree) \
    (extension pipchix.nix-embed \
      (component-dependencies \
       pipchix.abstract-syntax-tree)) \
    (extension pipchix.nix-list \
      (component-dependencies \
       pipchix.abstract-syntax-tree)) \
    (extension pipchix.base \
      (component-dependencies \
       pipchix.abstract-syntax-tree \
       pipchix.nix-list))))'

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

all-recursive: $(CHICKEN_5_EGGFILES)

CHICKEN_5_EGGS = chicken/eggs/5
CHICKEN_5_EGGDIR = $(CHICKEN_5_EGGS)/pipchix
CHICKEN_5_EGGFILES = \
  $(addprefix $(CHICKEN_5_EGGDIR)/, \
    pipchix.egg \
    pipchix.abstract-syntax-tree.scm \
    pipchix.nix-embed.scm \
    pipchix.nix-list.scm \
    pipchix.base.scm \
    COPYING README.adoc)
CLEANFILES += $(CHICKEN_5_EGGFILES)

$(CHICKEN_5_EGGDIR)/COPYING: COPYING
	$(call copy-file, $(<), $(@))

$(CHICKEN_5_EGGDIR)/README.adoc: README.adoc
	$(call copy-file, $(<), $(@))

$(CHICKEN_5_EGGDIR)/pipchix.egg: GNUmakefile $(TINYSCHEME)
	$(call v,SEXP) \
	script=`$(MKTEMP)` && \
	$(MKDIR_P) $(@D) && \
	if true; then \
	  printf '%s' $(DEFINE_NICELY_PRINT); \
	  printf '(define eggfile (quote %s))' \
	                 $(CHICKEN_5_EGGSPEC); \
	  printf '(nicely-print eggfile)'; \
	fi > "$${script}" && \
	(cd $(TINYSCHEME_DIR) && \
	   ./tinyscheme -1 ${abs_builddir}/"$${script}" \
			> $(abs_builddir)/$(@)) && \
	(test $(removing_egg_scripts) = yes && rm "$${script}" || true)

$(CHICKEN_5_EGGDIR)/pipchix.%.scm: pipchix/%.sld.m4 pipchix-includes.m4
	$(call v,SCM.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

CHICKEN_5_EGGSPEC = ' \
 '$(CHICKEN_EGGSPEC_TOP)' \
   (dependencies r7rs srfi-151 utf8) \
   (component-options \
    (csc-options \
     "-R" "utf8" \
     "-R" "r7rs" \
     "-X" "r7rs" \
     "-O2" "-d1" \
     "-compile-syntax")) \
 '$(CHICKEN_EGGSPEC_BOTTOM)

if CHICKEN_INSTALL_5
.PHONY: chicken-install-5
chicken-install-5: $(CHICKEN_5_EGGFILES)
	( \
	  cd $(CHICKEN_5_EGGDIR) && \
	  env SUDO="$${INSTALL_AS}" $(CHICKEN_INSTALL_5) -s \
	)
endif CHICKEN_INSTALL_5

if CHICKEN_UNINSTALL_5
.PHONY: chicken-uninstall-5
chicken-uninstall-5:
	env SUDO="$${INSTALL_AS}" \
	  $(CHICKEN_UNINSTALL_5) -s -force pipchix
endif CHICKEN_UNINSTALL_5

CLEANFILES += $(CHICKEN_5_EGGDIR)/*.{$(OBJEXT),so,link,import.scm}
CLEANFILES += $(CHICKEN_5_EGGDIR)/pipchix.{build,install}.sh

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# README: At the time of this writing (2025-11-15), it is not yet
#         possible to compile the egg with CHICKEN 6.
#
#

# all-recursive: $(CHICKEN_6_EGGFILES)

CHICKEN_6_EGGS = chicken/eggs/6
CHICKEN_6_EGGDIR = $(CHICKEN_6_EGGS)/pipchix
CHICKEN_6_EGGFILES = \
  $(addprefix $(CHICKEN_6_EGGDIR)/, \
    pipchix.egg \
    pipchix.abstract-syntax-tree.scm \
    pipchix.nix-embed.scm \
    pipchix.nix-list.scm \
    pipchix.base.scm \
    COPYING README.adoc)
CLEANFILES += $(CHICKEN_6_EGGFILES)

$(CHICKEN_6_EGGDIR)/COPYING: COPYING
	$(call copy-file, $(<), $(@))

$(CHICKEN_6_EGGDIR)/README.adoc: README.adoc
	$(call copy-file, $(<), $(@))

$(CHICKEN_6_EGGDIR)/pipchix.egg: GNUmakefile $(TINYSCHEME)
	$(call v,SEXP) \
	script=`$(MKTEMP)` && \
	$(MKDIR_P) $(@D) && \
	if true; then \
	  printf '%s' $(DEFINE_NICELY_PRINT); \
	  printf '(define eggfile (quote %s))' \
	                 $(CHICKEN_6_EGGSPEC); \
	  printf '(nicely-print eggfile)'; \
	fi > "$${script}" && \
	(cd $(TINYSCHEME_DIR) && \
	   ./tinyscheme -1 ${abs_builddir}/"$${script}" \
			> $(abs_builddir)/$(@)) && \
	(test $(removing_egg_scripts) = yes && rm "$${script}" || true)

$(CHICKEN_6_EGGDIR)/pipchix.%.scm: pipchix/%.sld.m4 pipchix-includes.m4
	$(call v,SCM.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

CHICKEN_6_EGGSPEC = ' \
 '$(CHICKEN_EGGSPEC_TOP)' \
   (component-options \
    (csc-options \
     "-O2" "-d1" \
     "-compile-syntax")) \
 '$(CHICKEN_EGGSPEC_BOTTOM)

if CHICKEN_INSTALL_6
.PHONY: chicken-install-6
chicken-install-6: $(CHICKEN_6_EGGFILES)
	( \
	  cd $(CHICKEN_6_EGGDIR) && \
	  $(CHICKEN_INSTALL_6) -n && \
	  env SUDO="$${INSTALL_AS}" $(CHICKEN_INSTALL_6) -s \
	)
endif CHICKEN_INSTALL_6

if CHICKEN_UNINSTALL_6
.PHONY: chicken-uninstall-6
chicken-uninstall-6:
	env SUDO="$${INSTALL_AS}" \
	  $(CHICKEN_UNINSTALL_6) -s -force pipchix
endif CHICKEN_UNINSTALL_6

CLEANFILES += $(CHICKEN_6_EGGDIR)/*.{$(OBJEXT),so,link,import.scm}
CLEANFILES += $(CHICKEN_6_EGGDIR)/pipchix.{build,install}.sh

#---------------------------------------------------------------------

TINYSCHEME_DIR = tinyscheme-1.42
TINYSCHEME = $(TINYSCHEME_DIR)/tinyscheme

$(TINYSCHEME):
	$(call v,MAKE) \
	  $(MAKE) $(AM_MAKEFLAGS) -C $(TINYSCHEME_DIR) tinyscheme

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
# local variables:
# coding: utf-8
# end:
