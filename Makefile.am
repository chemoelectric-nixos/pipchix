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
nodist_r7rs_DATA =

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
	$(MKDIR_P) $(@D) && \
		$(M4_PREPROCESS) $(<) > $(@).tmp && \
		mv $(@){.tmp,}

DEFINE_NICELY_PRINT = ' \
  (define (indent-to depth) \
    (let loop ((i depth)) \
      (unless (zero? i) \
        (display " ") \
        (loop (- i 1))))) \
  (define (nicely-print obj) \
    (%%nicely-print obj 0 \#f)) \
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

chicken-6/pipchix.egg: GNUmakefile $(TINYSCHEME)
	if true; then \
	  printf '$(TINYSCHEME_SHEBANG)\n'; \
	  printf '%s' $(DEFINE_NICELY_PRINT); \
	  printf '(define eggfile(quote('; \
	  printf '(synopsis "Nix coding with Scheme")'; \
	  printf '(author "Barry Schwartz")'; \
	  printf '(category code-generation)'; \
	  printf '(license "MIT")'; \
	  printf '(components'; \
	  printf ' (extension (pipchix abstract-syntax-tree))'; \
	  printf ' )'; \
	  printf '(csc-options "-O3")'; \
	  printf ')))'; \
	  printf '(nicely-print eggfile)'; \
	fi > $(TINYSCHEME_DIR)/script.tmp
	( \
	  $(MKDIR_P) $(@D) && \
	  cd $(TINYSCHEME_DIR) && \
	  chmod +x script.tmp && \
	  ./script.tmp > $(abs_top_builddir)/$(@) && \
	  rm -f xscript.tmp \
	)

MOSTLYCLEANFILES += $(TINYSCHEME_DIR)/script.tmp

# TinySCHEME is used here to manipulate s-expressions without having
# to have any particular Scheme installed on the system.
EXTRA_DIST += $(wildcard $(TINYSCHEME_DIR)/*)
TINYSCHEME_DIR = tinyscheme-1.42
TINYSCHEME_C = $(addprefix $(srcdir)/$(TINYSCHEME_DIR)/, \
			$(addsuffix .c, scheme))
TINYSCHEME_H = $(addprefix $(srcdir)/$(TINYSCHEME_DIR)/, \
			$(addsuffix .h, dynload opdefines \
					scheme scheme-private))
TINYSCHEME_CFLAGS = \
	${CFLAGS} $(CFLAGS_FOR_STATIC) $(CFLAGS_FOR_PIC) \
	-DUSE_DL=0 -DUSE_MATH=1 -DUSE_ASCII_NAMES=0
$(TINYSCHEME): $(TINYSCHEME_C) $(TINYSCHEME_H)
	$(MKDIR_P) $(@D)
	$(CC) -o $(@) $(TINYSCHEME_CFLAGS) $(TINYSCHEME_C) -lm

TINYSCHEME = $(TINYSCHEME_DIR)/tinyscheme$(EXEEXT)
TINYSCHEME_SHEBANG=\#!$(abs_builddir)/$(TINYSCHEME) -1

MOSTLYCLEANFILES += $(TINYSCHEME)
MOSTLYCLEANFILES += $(TINYSCHEME_DIR)/*.$(OBJEXT)

# local variables:
# coding: utf-8
# end:
