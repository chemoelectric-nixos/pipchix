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

CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST =
EXTRA_PROGRAMS =
EXTRA_SCRIPTS =
BUILT_SOURCES =
nodist_r7rspipchix_DATA =
nodist_chickeneggs5pipchix_DATA =

EXTRA_DIST += silent-rules.mk
EXTRA_DIST += COPYING
EXTRA_DIST += README.adoc
EXTRA_DIST += INSTALL
EXTRA_DIST += AUTHORS

DISTCLEANFILES += Makefile
DISTCLEANFILES += GNUmakefile

EXTRA_DIST += pipchix-includes.m4
EXTRA_DIST += $(addsuffix .m4, $(nodist_r7rs_DATA))

M4_PREPROCESS = $(GNU_M4) -P -I '$(srcdir)'

@GNU@ define copy-file
@GNU@ $(call v,COPY) cp -f $(1) $(2)
@GNU@ endef

CLEANFILES += tmp.*

R7RS_SCHEME =
R7RS_SCHEME += r7rs/pipchix/base.sld
R7RS_SCHEME += r7rs/pipchix/nix-embed.sld
R7RS_SCHEME += r7rs/pipchix/nix-list.sld
R7RS_SCHEME += r7rs/pipchix/abstract-syntax-tree.sld

CLEANFILES += $(R7RS_SCHEME)
CLEANFILES += $(addsuffix .tmp, $(R7RS_SCHEME))

if INSTALL_R7RS_SOURCE
nodist_r7rspipchix_DATA += $(R7RS_SCHEME)
endif

if INSTALL_CHICKEN_5_EGG_SOURCE
nodist_chickeneggs5pipchix_DATA += $(CHICKEN_5_EGGFILES)
endif

$(builddir)/r7rs/pipchix/%.sld: $(srcdir)/r7rs/pipchix/%.sld.m4 \
	                        $(srcdir)/pipchix-includes.m4
	$(call v,SLD.M4)$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

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

$(builddir)/$(CHICKEN_5_EGGDIR)/pipchix.%.scm: \
	                           $(srcdir)/r7rs/pipchix/%.sld.m4 \
	                           $(srcdir)/pipchix-includes.m4
	$(call v,SCM.M4) $(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

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

$(CHICKEN_5_EGGDIR)/pipchix.egg: GNUmakefile $(TINYSCHEME_EXE)
	$(call v,SEXP) \
	script=`$(MKTEMP)` && \
	$(MKDIR_P) $(@D) && \
	if true; then \
	  printf '%s' $(DEFINE_NICELY_PRINT); \
	  printf '(define eggfile (quote %s))' \
	                 $(CHICKEN_5_EGGSPEC); \
	  printf '(nicely-print eggfile)'; \
	fi > "$${script}" && \
	$(call tinyscheme-1, ${abs_builddir}/"$${script}") \
			> $(abs_builddir)/$(@) && \
	(test $(removing_egg_scripts) = yes && rm "$${script}" || true)

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

#---------------------------------------------------------------------

if CSI_5
@GNU@ define get-chicken-5-repository-path
@GNU@ $(CSI_5) -n -b -R r7rs -R utf8-srfi-13 -R chicken.platform \
@GNU@    -e '(display (string-join (cons "$(abs_builddir)/$(CHICKEN_5_EGGDIR)/.." (repository-path)) ":"))'
@GNU@ endef
else !CSI_5
@GNU@ define get-chicken-5-repository-path
@GNU@ true
@GNU@ endef
endif !CSI_5

#
# make-chicken-5-egg may fail if you have not installed the
# dependencies of pipchix.
#
make-chicken-5-egg: $(CHICKEN_5_EGGFILES)
	( \
	  $(MKDIR_P) $(CHICKEN_5_EGGDIR) && \
	  cd $(CHICKEN_5_EGGDIR) && \
	  $(CHICKEN_INSTALL_5) -n \
	)

chicken-5-egg-stamp: make-chicken-5-egg
	test -e $(@) || touch $(@)

CLEANFILES += chicken-5-egg-stamp

tests/r7rs/chicken-5/%-csi: tests/r7rs/% $(CHICKEN_5_EGGFILES) \
	                    chicken-5-egg-stamp GNUmakefile
	$(call v,AWK) $(MKDIR_P) $(@D) && \
	$(AWK) 'BEGIN{print "#!/usr/bin/env -S $(CSI_5) -R r7rs -s"} \
	        {print}' $(<) \
	  > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

tests/r7rs/chicken-5/%-csi.sh: tests/r7rs/% GNUmakefile
	$(call v,PRINTF) $(MKDIR_P) $(@D) && \
	( \
	  scheme='$(realpath $(<))' && \
	  printf '#!$(SHELL)\n' && \
	  printf 'export CHICKEN_REPOSITORY_PATH\n' && \
	  printf 'CHICKEN_REPOSITORY_PATH="%s"\n' \
	         "$(call get-chicken-5-repository-path)" \
	  printf 'exec $(CSI_5) -R r7rs -n -b -s %s\n' "$${scheme}" \
	) > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

tests/r7rs/chicken-5/%-csc: tests/r7rs/% $(CHICKEN_5_EGGFILES) \
	                    chicken-5-egg-stamp GNUmakefile
	$(call v,AWK) $(MKDIR_P) $(@D) && \
	$(CSC_5) -R r7rs -X r7rs $(<) -o $(@)

tests/r7rs/chicken-5/%-csc.sh: tests/r7rs/% GNUmakefile
	$(call v,PRINTF) $(MKDIR_P) $(@D) && \
	( \
	  scheme='$(realpath $(<))' && \
	  printf '#!$(SHELL)\n' && \
	  printf 'export CHICKEN_REPOSITORY_PATH\n' && \
	  printf 'CHICKEN_REPOSITORY_PATH="%s"\n' \
	         "$(call get-chicken-5-repository-path)" \
	  printf 'exec %s\n' "$${scheme}" \
	) > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

tests/r7rs/%-chibi: tests/r7rs/% $(R7RS_SCHEME) GNUmakefile
	$(call v,AWK) $(MKDIR_P) $(@D) && \
	$(AWK) 'BEGIN{print "#!$(CHIBI_SCHEME) -I'$(abs_builddir)/r7rs'"} \
	        {print}' $(<) \
	  > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

tests/r7rs/%-gauche: tests/r7rs/% $(R7RS_SCHEME) GNUmakefile
	$(call v,AWK) $(MKDIR_P) $(@D) && \
	$(AWK) 'BEGIN{print "#!$(GAUCHE) -I'$(abs_builddir)/r7rs'"} \
	        {print}' $(<) \
	  > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

tests/r7rs/%-guile: tests/r7rs/% $(R7RS_SCHEME) GNUmakefile
	$(call v,AWK) $(MKDIR_P) $(@D) && \
	$(AWK) 'BEGIN{print "#!$(GUILE) \\"; \
	              print "-L '$(abs_builddir)/r7rs' --r7rs --no-auto-compile -s"; \
	              print "!#"} \
	        {print}' $(<) \
	  > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

tests/r7rs/%-sagittarius: tests/r7rs/% $(R7RS_SCHEME) GNUmakefile
	$(call v,AWK) $(MKDIR_P) $(@D) && \
	$(AWK) 'BEGIN{print "#!$(SAGITTARIUS) --loadpath='$(abs_builddir)/r7rs'"} \
	        {print}' $(<) \
	  > $(@).tmp && mv $(@){.tmp,} && chmod +x $(@)

CLEANFILES += tests/r7rs/*.tmp
CLEANFILES += tests/r7rs/chicken-5/*.tmp

TESTS =
TESTS += $(TESTS_WE_MADE)

CHEX_R7RS =
CHEX_R7RS += check-nix-list

EXTRA_DIST += $(CHEX_R7RS:%=tests/r7rs/%)

CHEX_WE_MADE =
CHEX_WE_MADE += $(CHEX_R7RS:%=tests/r7rs/chicken-5/%-csi.sh)
CHEX_WE_MADE += $(CHEX_R7RS:%=tests/r7rs/chicken-5/%-csc.sh)
CHEX_WE_MADE += $(CHEX_R7RS:%=tests/r7rs/%-chibi)
CHEX_WE_MADE += $(CHEX_R7RS:%=tests/r7rs/%-gauche)
CHEX_WE_MADE += $(CHEX_R7RS:%=tests/r7rs/%-guile)
CHEX_WE_MADE += $(CHEX_R7RS:%=tests/r7rs/%-sagittarius)

CLEANFILES += $(CHEX_WE_MADE)
CLEANFILES += $(CHEX_R7RS:%=tests/r7rs/chicken-5/%-csi)
CLEANFILES += $(CHEX_R7RS:%=tests/r7rs/chicken-5/%-csc)

if CHICKEN_INSTALL_5
if CSI_5
TESTS += tests/r7rs/chicken-5/check-nix-list-csi.sh
endif
if CSC_5
TESTS += tests/r7rs/chicken-5/check-nix-list-csc.sh
endif
endif

if CHIBI_SCHEME
TESTS += tests/r7rs/check-nix-list-chibi
endif CHIBI_SCHEME

if GAUCHE
TESTS += tests/r7rs/check-nix-list-gauche
endif GAUCHE

if GUILE
TESTS += tests/r7rs/check-nix-list-guile
endif GUILE

if SAGITTARIUS
TESTS += tests/r7rs/check-nix-list-sagittarius
endif SAGITTARIUS

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
# local variables:
# coding: utf-8
# end:
