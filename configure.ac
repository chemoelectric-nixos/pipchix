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

CHEM_UNIQUE_ID([UNIQUE_ID])

AC_ARG_ENABLE([color],
  [AS_HELP_STRING([--disable-color],
    [disable color in configure and test outputs (enabled by default)])],
  [enable_color="${enableval}"],
  [enable_color=yes])
AC_SUBST([enable_color])

m4_define([large_circle],
  [`if test "${enable_color}" = no; then echo ⬤; else echo 🔵; fi`])

m4_include([ac-chicken-install-options.m4])
m4_include([ac-r6rs-install-options.m4])
m4_include([ac-r7rs-install-options.m4])

m4_define([offer_disable_of_check],
[AC_ARG_ENABLE([check-[$1]-[$2]],
  [AS_HELP_STRING([--disable-check-[$1]-[]m4_translit([$2],[-],[_])],
        [$3 (enabled by default if the Scheme implementation is found)])],
  [eval enable_check_[$1]_[]m4_translit([$2],[-],[_])="${enableval}"],
  [eval enable_check_[$1]_[]m4_translit([$2],[-],[_])=yes])
AC_SUBST(enable_check_[$1]_[]m4_translit([$2],[-],[_]))
AM_CONDITIONAL(ENABLE_CHECK_[]m4_toupper([$1])_[]m4_toupper(m4_translit([$2],[-],[_])),
               [test "${enable_check_[$1]_[]m4_translit([$2],[-],[_])}" != no])])

m4_define([offer_enable_of_check],
[AC_ARG_ENABLE([check-[$1]-[$2]],
  [AS_HELP_STRING([--enable-check-[$1]-[]m4_translit([$2],[-],[_])],
        [$3 (disabled by default, due to difficulties of the tests)])],
  [eval enable_check_[$1]_[]m4_translit([$2],[-],[_])="${enableval}"],
  [eval enable_check_[$1]_[]m4_translit([$2],[-],[_])=no])
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

offer_disable_of_check([r7rs],[gsi],
  [disable tests with Gambit Scheme interpreter])

dnl offer_disable_of_check([r7rs],[gsc],
dnl   [disable tests with Gambit Scheme compiler])

offer_disable_of_check([r7rs],[gauche],
  [disable tests with Gauche Scheme])

offer_disable_of_check([r7rs],[gxi],
  [disable tests with Gerbil Scheme interpreter])

# The current gxc tests are slow, do not run in parallel, and do not
# run in distcheck.
offer_enable_of_check([r7rs],[gxc],
  [enable tests with Gerbil Scheme compiler])

offer_disable_of_check([r6rs],[guile],
  [disable R⁶RS tests with Guile])

offer_disable_of_check([r7rs],[guile],
  [disable R⁷RS tests with Guile])

offer_disable_of_check([r6rs],[loko],
  [disable R⁶RS tests with Loko Scheme])

offer_disable_of_check([r7rs],[loko],
  [disable R⁷RS tests with Loko Scheme])

offer_disable_of_check([r6rs],[racket],
  [disable tests with Racket])

offer_disable_of_check([r6rs],[sagittarius],
  [disable R⁶RS tests with Sagittarius])

offer_disable_of_check([r7rs],[sagittarius],
  [disable R⁷RS tests with Sagittarius])

m4_include([ac-tinyscheme.m4])
m4_include([ac-chez-scheme.m4])
m4_include([ac-chicken-scheme.m4])
m4_include([ac-chibi-scheme.m4])
m4_include([ac-gambit-scheme.m4])
m4_include([ac-gauche-scheme.m4])
m4_include([ac-gerbil-scheme.m4])
m4_include([ac-guile-scheme.m4])
m4_include([ac-loko-scheme.m4])
m4_include([ac-racket.m4])
m4_include([ac-sagittarius-scheme.m4])

# You can use ‘@GNU@’ to make GNU Make constructs unbothersome to
# Automake. (Often you can use ‘$(eval ...)’ instead, but @GNU@ is
# more straightforward.)
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
AC_CONFIG_FILES([tests/run-r7rs-gsi.sh],
                [chmod +x tests/run-r7rs-gsi.sh])
dnl AC_CONFIG_FILES([tests/run-r7rs-gsc.sh],
dnl                 [chmod +x tests/run-r7rs-gsc.sh])
AC_CONFIG_FILES([tests/run-r7rs-gauche.sh],
                [chmod +x tests/run-r7rs-gauche.sh])
AC_CONFIG_FILES([tests/run-r7rs-gxi.sh],
                [chmod +x tests/run-r7rs-gxi.sh])
AC_CONFIG_FILES([tests/run-r7rs-gxc.sh],
                [chmod +x tests/run-r7rs-gxc.sh])
AC_CONFIG_FILES([tests/run-r6rs-guile.sh],
                [chmod +x tests/run-r6rs-guile.sh])
AC_CONFIG_FILES([tests/run-r7rs-guile.sh],
                [chmod +x tests/run-r7rs-guile.sh])
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

AC_CONFIG_COMMANDS([tempname],
[
  printf '@%:@!%s\nprintf '\''tmp.'\''; %s\n' "${SHELL}" "${UNIQUE_ID}" > tempname
  chmod +x tempname
],
[
  UNIQUE_ID="$(echo "${UNIQUE_ID}" | sed 's/\$/\\\$/g;s/\"/\\\"/g')"
  SHELL="${SHELL}"
])

TEST_LOCALE=C.utf8
AC_ARG_VAR([TEST_LOCALE],
           [locale to use when running tests
            (default: C.utf8)])

AC_OUTPUT

m4_include([ac-what-was-found.m4])

AC_MSG_NOTICE([large_circle ‘make dist-chicken-egg-5’ to make a compressed tar])
AC_MSG_NOTICE([     of the CHICKEN 5 egg sources.])
AC_MSG_NOTICE([])

if test "${install_chicken_5_egg_source}" != no; then
  AC_MSG_NOTICE([‘make install’ will put CHICKEN 5 egg sources in])
  AC_MSG_NOTICE([   ${chickeneggs5pipchixdir}])
  AC_MSG_NOTICE([   where \@S|@(chickeneggsdir) is ${chickeneggsdir}])
  AC_MSG_NOTICE([])
fi

if test "${install_r6rs_source}" != no; then
  AC_MSG_NOTICE([‘make install’ will put R⁶RS sources in])
  AC_MSG_NOTICE([   ${r6rspipchixdir}])
  AC_MSG_NOTICE([])
fi

if test "${install_r7rs_source}" != no; then
  AC_MSG_NOTICE([‘make install’ will put R⁷RS sources in])
  AC_MSG_NOTICE([   ${r7rspipchixdir}])
  AC_MSG_NOTICE([])
fi

# local variables:
# coding: utf-8
# end:
