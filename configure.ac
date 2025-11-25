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

AC_PREREQ([2.72])
AC_INIT([Pipchix],[0.0.0])

AC_CONFIG_SRCDIR([pipchix/pipchix-includes.m4])

AC_CONFIG_TESTDIR([tests])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])
AC_CANONICAL_BUILD                 dnl On what platform are we compiling?
AC_CANONICAL_HOST                  dnl For what platform are we compiling?
AC_USE_SYSTEM_EXTENSIONS           dnl Define such macros as _GNU_SOURCE.

AM_INIT_AUTOMAKE([tar-pax no-dist-gzip dist-xz])
AM_SILENT_RULES([yes])
AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
AC_PROG_AWK
AC_PROG_SED
AC_PROG_GREP
AC_PROG_FGREP
AC_PROG_EGREP

CHEM_PROG_GNU_M4
if test -z "${GNU_M4}"; then
   AC_MSG_ERROR([GNU m4 not found. Perhaps set GNU_M4 to OpenBSD m4.])
fi

CHEM_PATH_PROGS_CACHED_AND_PRECIOUS([ENV],[env with a working -S option],[env],[
     if LC_ALL=C LANG=C \
            printf '@%:@!%s -S %s -v DAFFYDUCK=12345 -f\nBEGIN { print DAFFYDUCK }\n' \
              "${ac_path_ENV}" "${AWK}" > conftest.sh 2> /dev/null &&
          chmod +x conftest.sh &&
          LC_ALL=C LANG=C ./conftest.sh > conftest.out 2> /dev/null &&
          LC_ALL=C LANG=C ${FGREP} 12345 conftest.out 2> /dev/null > /dev/null; then
       ac_cv_path_ENV="${ac_path_ENV}"
       ac_path_ENV_found=:
     fi
     rm -f conftest.{sh,out}
   ])
if test "${chem_cv_path_ENV_found}" != no; then
  ENV_S="${ac_cv_path_ENV} -S"
else
  ENV_S="\$(abs_builddir)/return77"
fi
AC_SUBST([ENV_S])

#
# A way to get names for temporary files.
#
AC_CHECK_PROGS([MKTEMP],[mktemp],[echo tmp.\"\$\${RANDOM}\"])
AC_ARG_VAR([MKTEMP],[mktemp command])
MKTEMP="TMPDIR=. ${MKTEMP}"

m4_include([ac-r6rs-install-options.m4])
m4_include([ac-r7rs-install-options.m4])
m4_include([ac-chicken-install-options.m4])

m4_define([offer_disable_of_check],
[AC_ARG_ENABLE([check-[$1]-[$2]],
  [AS_HELP_STRING([--disable-check-[$1]-[]m4_translit([$2],[-],[_])],
        [$3 (enabled by default if the Scheme implementation is found)])],
  [eval enable_check_[$1]_[]m4_translit([$2],[-],[_])="${enableval}"],
  [eval enable_check_[$1]_[]m4_translit([$2],[-],[_])=yes])
AC_SUBST(enable_check_[$1]_[]m4_translit([$2],[-],[_]))
AM_CONDITIONAL(ENABLE_CHECK_[]m4_toupper([$1])_[]m4_toupper(m4_translit([$2],[-],[_])),
               [test "${enable_check_[$1]_[]m4_translit([$2],[-],[_])}" != no])])

offer_disable_of_check([r6rs],[chez],
  [disable tests with Chez Scheme])

offer_disable_of_check([r7rs],[chibi],
  [disable tests with Chibi Scheme])

offer_disable_of_check([r7rs],[csi-5],
  [disable tests with CHICKEN 5 interpreter])

offer_disable_of_check([r7rs],[csc-5],
  [disable tests with CHICKEN 5 compiler])

offer_disable_of_check([r7rs],[gauche],
  [disable tests with Gauche Scheme])

offer_disable_of_check([r6rs],[guile],
  [disable R⁶RS tests with Guile])

offer_disable_of_check([r7rs],[guile],
  [disable R⁷RS tests with Guile])

offer_disable_of_check([r6rs],[loko],
  [disable R⁶RS tests with Loko Scheme])

offer_disable_of_check([r7rs],[loko],
  [disable R⁷RS tests with Loko Scheme])

offer_disable_of_check([r7rs],[mit],
  [disable tests with MIT/GNU Scheme])

offer_disable_of_check([r6rs],[racket],
  [disable tests with Racket])

offer_disable_of_check([r6rs],[racketbc],
  [disable tests with Racket BC])

offer_disable_of_check([r6rs],[sagittarius],
  [disable R⁶RS tests with Sagittarius])

offer_disable_of_check([r7rs],[sagittarius],
  [disable R⁷RS tests with Sagittarius])

m4_include([ac-tinyscheme.m4])
m4_include([ac-chez-scheme.m4])
m4_include([ac-chicken-scheme.m4])
m4_include([ac-chibi-scheme.m4])
m4_include([ac-gauche-scheme.m4])
m4_include([ac-guile-scheme.m4])
m4_include([ac-loko-scheme.m4])
m4_include([ac-mit-scheme.m4])
m4_include([ac-racket.m4])
m4_include([ac-sagittarius-scheme.m4])

# You can use ‘@GNU@’ to make GNU Make co@GNU@ nstructs unbothersome to
# Automake. (By the way, you often can use ‘$(eval ...)’ as well, but
# @GNU@ here may be better.)
AC_SUBST([GNU],[])

CHEM_REQUIRE_GNU_MAKE_IN_PATH
CHEM_CONFIG_MAKEFILES
CHEM_CONFIG_MAKEFILES([tinyscheme-1.42/Makefile],
                      [tinyscheme-1.42/GNUmakefile])
CHEM_CONFIG_MAKEFILES([tests/Makefile],
                      [tests/GNUmakefile])
AC_CONFIG_FILES([tests/atlocal])
AC_CONFIG_FILES([tests/run-r6rs-chez.sh],
                [chmod +x tests/run-r6rs-chez.sh])
AC_CONFIG_FILES([tests/run-r7rs-chibi.sh],
                [chmod +x tests/run-r7rs-chibi.sh])
AC_CONFIG_FILES([tests/run-r7rs-csc-5.sh],
                [chmod +x tests/run-r7rs-csc-5.sh])
AC_CONFIG_FILES([tests/run-r7rs-csi-5.sh],
                [chmod +x tests/run-r7rs-csi-5.sh])
AC_CONFIG_FILES([tests/run-r7rs-gauche.sh],
                [chmod +x tests/run-r7rs-gauche.sh])
AC_CONFIG_FILES([tests/run-r6rs-guile.sh],
                [chmod +x tests/run-r6rs-guile.sh])
AC_CONFIG_FILES([tests/run-r7rs-guile.sh],
                [chmod +x tests/run-r7rs-guile.sh])
AC_CONFIG_FILES([tests/run-r7rs-mit.sh],
                [chmod +x tests/run-r7rs-mit.sh])
AC_CONFIG_FILES([tests/run-r6rs-loko.sh],
                [chmod +x tests/run-r6rs-loko.sh])
AC_CONFIG_FILES([tests/run-r7rs-loko.sh],
                [chmod +x tests/run-r7rs-loko.sh])
AC_CONFIG_FILES([tests/run-r6rs-racket.sh],
                [chmod +x tests/run-r6rs-racket.sh])
AC_CONFIG_FILES([tests/run-r6rs-sagittarius.sh],
                [chmod +x tests/run-r6rs-sagittarius.sh])
AC_CONFIG_FILES([tests/run-r7rs-sagittarius.sh],
                [chmod +x tests/run-r7rs-sagittarius.sh])

m4_foreach([f],
  [[tinyscheme-1.42/init.scm],
   [tinyscheme-1.42/dynload.c],
   [tinyscheme-1.42/dynload.h],
   [tinyscheme-1.42/scheme.c],
   [tinyscheme-1.42/scheme.h],
   [tinyscheme-1.42/scheme-private.h],
   [tinyscheme-1.42/opdefines.h]],
  [AC_CONFIG_LINKS(f:f)])

TEST_LOCALE=C.utf8
AC_ARG_VAR([TEST_LOCALE],
           [locale to use when running tests
            (default: C.utf8)])

AC_OUTPUT

m4_include([ac-what-was-found.m4])

if test "${chem_cv_path_ENV_found}" = no; then
  AC_MSG_WARN([${ac_path_ENV} does not have a working -S option.])
  AC_MSG_WARN([Some tests will be skipped.])
  AC_MSG_WARN([Consider installing GNU coreutils.])
fi

# local variables:
# coding: utf-8
# end:
