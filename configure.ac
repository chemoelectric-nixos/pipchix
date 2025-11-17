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

AC_PREREQ([2.72])
AC_INIT([Pipchix],[0.0.0])

AC_CONFIG_SRCDIR([pipchix-includes.m4])

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
StM_PROG_GNU_M4

if test -z "${GNU_M4}"; then
   AC_MSG_ERROR([GNU m4 not found. Perhaps set GNU_M4 to OpenBSD m4.])
fi

#
# A way to get names for temporary files.
#
AC_CHECK_PROGS([MKTEMP],[mktemp],[echo tmp.\"\$\${RANDOM}\"])
AC_ARG_VAR([MKTEMP],[mktemp command])
MKTEMP="TMPDIR=. ${MKTEMP}"

AC_ARG_ENABLE([install-r7rs-source],
  [AS_HELP_STRING([--disable-install-r7rs-source[=DIR]],
        [install R⁷RS source code [installed by default]])],
  [install_r7rs_source="${enableval}"],
  [install_r7rs_source=yes])
AC_SUBST([install_r7rs_source])
AM_CONDITIONAL([INSTALL_R7RS_SOURCE],
               [test "${install_r7rs_source}" != no]) dnl FIXME: COME UP WITH A BETTER yes/no TEST.##################################
r7rspipchixdir="${r7rspipchixdir:-\$(datadir)/r7rs/pipchix}"
AC_SUBST([r7rspipchixdir])

AC_ARG_ENABLE([install-chicken-5-egg-source],
  [AS_HELP_STRING([--enable-install-chicken-5-egg-source[=DIR]],
        [install CHICKEN 5 egg source code [not installed by default]])],
  [install_chicken_5_egg_source="${enableval}"],
  [install_chicken_5_egg_source=no])
AC_SUBST([install_chicken_5_egg_source])
AM_CONDITIONAL([INSTALL_CHICKEN_5_EGG_SOURCE],
               [test "${install_chicken_5_egg_source}" != no])
chickeneggs5pipchixdir="${chickeneggs5pipchixdir:-\$(datadir)/chicken/eggs/5/pipchix}"
AC_SUBST([chickeneggs5pipchixdir])

m4_include([ac-tinyscheme.m4])
m4_include([ac-chicken-scheme.m4])
m4_include([ac-chibi-scheme.m4])
m4_include([ac-gauche-scheme.m4])
m4_include([ac-guile-scheme.m4])
m4_include([ac-sagittarius-scheme.m4])

# You can use ‘@GNU@’ to make GNU Make co@GNU@ nstructs unbothersome to
# Automake. (By the way, you often can use ‘$(eval ...)’ as well, but
# @GNU@ here may be better.)
AC_SUBST([GNU],[])

StM_REQUIRE_GNU_MAKE_IN_PATH
StM_CONFIG_MAKEFILES
StM_CONFIG_MAKEFILES([tinyscheme-1.42/Makefile],
                     [tinyscheme-1.42/GNUmakefile])

m4_foreach([f],
  [[tinyscheme-1.42/init.scm],
   [tinyscheme-1.42/dynload.c],
   [tinyscheme-1.42/dynload.h],
   [tinyscheme-1.42/scheme.c],
   [tinyscheme-1.42/scheme.h],
   [tinyscheme-1.42/scheme-private.h],
   [tinyscheme-1.42/opdefines.h]],
  [AC_CONFIG_LINKS(f:f)])

AC_OUTPUT

m4_include([ac-what-was-found.m4])

# local variables:
# coding: utf-8
# end:
