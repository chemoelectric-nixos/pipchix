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
nodist_r7rs_DATA =

EXTRA_DIST += silent-rules.mk
EXTRA_DIST += COPYING
EXTRA_DIST += README.adoc
#EXTRA_DIST += INSTALL
#EXTRA_DIST += AUTHORS

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

#---------------------------------------------------------------------

#
# CHICKEN Scheme support.
#

CHICKEN_5_DIR = chicken-5
CHICKEN_5_EGGDIR = $(CHICKEN_5_DIR)/$(PACKAGE_TARNAME)
CHICKEN_5_EGG_TARBALL = \
  $(PACKAGE_TARNAME)-$(PACKAGE_VERSION).chicken-5-egg.tar.xz

CHICKEN_6_DIR = chicken-6
CHICKEN_6_EGGDIR = $(CHICKEN_6_DIR)/$(PACKAGE_TARNAME)
CHICKEN_6_EGG_TARBALL = \
  $(PACKAGE_TARNAME)-$(PACKAGE_VERSION).chicken-6-egg.tar.xz

CHICKEN_5_EGGFILES = \
  $(addprefix $(CHICKEN_5_EGGDIR)/, \
    pipchix.egg \
    pipchix.abstract-syntax-tree.scm \
    pipchix.nix-embed.scm \
    pipchix.nix-list.scm \
    pipchix.base.scm \
    COPYING README.adoc)

CLEANFILES += $(CHICKEN_5_EGGFILES)

EXTRA_DIST += $(CHICKEN_5_EGG_TARBALL)
CLEANFILES += $(CHICKEN_5_EGG_TARBALL)
CLEANFILES += $(CHICKEN_5_EGG_TARBALL).tmp

$(CHICKEN_5_EGG_TARBALL): egg-for-chicken-5
	$(call v,TAR) \
	  tar --format=posix -chf - -C $(CHICKEN_5_DIR) \
	    $(addprefix $(PACKAGE_TARNAME)/, \
	       $(notdir $(CHICKEN_5_EGGFILES))) \
	  | XZ_OPT=${XZ_OPT--e} xz -c > $(@).tmp && \
	mv $(@){.tmp,}

.PHONY: egg-for-chicken-5 GNUmakefile $(TINYSCHEME)
egg-for-chicken-5: $(CHICKEN_5_EGGFILES)

$(CHICKEN_5_EGGDIR)/COPYING: COPYING
	$(call copy-file, $(<), $(@))

$(CHICKEN_5_EGGDIR)/README.adoc: README.adoc
	$(call copy-file, $(<), $(@))

CHICKEN_5_EGGSPEC = ' \
  ((synopsis "Use Scheme to write Nix code") \
   (version "$(PACKAGE_VERSION)") \
   (author "Barry Schwartz") \
   (category code-generation) \
   (license "MIT") \
   (dependencies r7rs) \
   (component-options \
    (csc-options \
     "-R" "r7rs" "-X" "r7rs" \
     "-O2" "-d1" \
     "-compile-syntax")) \
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

$(CHICKEN_5_EGGDIR)/pipchix.egg: GNUmakefile $(TINYSCHEME)
	$(call v,SEXP)$(MKDIR_P) $(@D) && \
	if true; then \
	  printf '%s' $(DEFINE_NICELY_PRINT); \
	  printf '(define eggfile (quote %s))' \
	                 $(CHICKEN_5_EGGSPEC); \
	  printf '(nicely-print eggfile)'; \
	fi > script.tmp && \
	(cd $(TINYSCHEME_DIR) && \
	   ./tinyscheme -1 ${abs_builddir}/script.tmp \
			> $(abs_builddir)/$(@)) && \
	rm script.tmp

MOSTLYCLEANFILES += script.tmp

$(CHICKEN_5_EGGDIR)/pipchix.%.scm: pipchix/%.sld.m4 pipchix-includes.m4
	$(call v,SCM.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

#---------------------------------------------------------------------

#
# TinySCHEME as a scripting language for the Makefile. (We do not wish
# to assume that GNU Make was linked with libguile.)
#

EXTRA_DIST += $(wildcard $(TINYSCHEME_DIR)/*)
TINYSCHEME_DIR = tinyscheme-1.42
TINYSCHEME_C = $(addprefix $(TINYSCHEME_DIR)/, \
			$(addsuffix .c, scheme))
TINYSCHEME_H = $(addprefix $(TINYSCHEME_DIR)/, \
			$(addsuffix .h, dynload opdefines \
					scheme scheme-private))
TINYSCHEME_CFLAGS = \
	${CFLAGS} $(CFLAGS_FOR_STATIC) $(CFLAGS_FOR_PIC) \
	-DUSE_DL=0 -DUSE_MATH=1 -DUSE_ASCII_NAMES=0
$(TINYSCHEME): $(TINYSCHEME_C) $(TINYSCHEME_H)
	$(call v,CC) $(MKDIR_P) $(@D) && \
		$(CC) -o $(@) $(TINYSCHEME_CFLAGS) $(TINYSCHEME_C) -lm

TINYSCHEME = $(TINYSCHEME_DIR)/tinyscheme$(EXEEXT)

MOSTLYCLEANFILES += $(TINYSCHEME)
MOSTLYCLEANFILES += $(TINYSCHEME_DIR)/*.$(OBJEXT)

#
# A pretty-printer (nicely-print obj). It uses ’write’ for anything
# except a list. Lists it prints with newlines and indentation.
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
                              (equal? separator " ")) \
              (loop (cdr lst) " ")) \
            (else \
              (display ")"))))) \
       (else \
         (write obj))))'

#---------------------------------------------------------------------
# local variables:
# coding: utf-8
# end:
