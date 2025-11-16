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

AC_CHECK_PROGS([MKTEMP],[mktemp],[echo tmp.\"\$\${RANDOM}\"])
AC_ARG_VAR([MKTEMP],[mktemp command])
MKTEMP="TMPDIR=. ${MKTEMP}"

AX_CHECK_COMPILE_FLAG([-std=gnu99],
  [CFLAGS="${CFLAGS}${CFLAGS+ }-std=gnu99"],
  [],[],[AC_LANG_SOURCE([int main(){return 0;}])])

AX_CHECK_COMPILE_FLAG([-static],
  [CFLAGS_FOR_STATIC="-static"],
  [CFLAGS_FOR_STATIC=""],
  [],[AC_LANG_SOURCE([int main(){return 0;}])])
AC_SUBST([CFLAGS_FOR_STATIC])

AX_CHECK_COMPILE_FLAG([-fPIC],
  [CFLAGS_FOR_PIC="-fPIC"],
  [CFLAGS_FOR_PIC=""],
  [],[AC_LANG_SOURCE([int main(){return 0;}])])
AC_SUBST([CFLAGS_FOR_PIC])

if test -z "${GNU_M4}"; then
   AC_MSG_ERROR([GNU m4 not found. Perhaps set GNU_M4 to OpenBSD m4.])
fi

AC_ARG_WITH([r7rsdir],
  [AS_HELP_STRING([--with-r7rsdir=DIR],
        [where to put R⁷RS source code [DATAROOTDIR/r7rs]])],
  [r7rsdir="${withval}"],
  [r7rsdir='${datadir}/r7rs'])
AC_SUBST([r7rsdir])

StM_PATH_PROGS_CACHED_AND_PRECIOUS([CHICKEN_INSTALL_5],
  [chicken-install version 5],[chicken-install chicken-install-5],
  [
    if LC_ALL=C LANG=C ${ac_path_CHICKEN_INSTALL_5} -version 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^5\.' 2> /dev/null > /dev/null; then
        ac_cv_path_CHICKEN_INSTALL_5="${ac_path_CHICKEN_INSTALL_5}"
        ac_path_CHICKEN_INSTALL_5_found=:
    fi
  ])
AM_CONDITIONAL([CHICKEN_INSTALL_5],
               [test "${__ac_cv_path_CHICKEN_INSTALL_5_found}" != no])

StM_PATH_PROGS_CACHED_AND_PRECIOUS([CHICKEN_UNINSTALL_5],
  [chicken-uninstall version 5],[chicken-uninstall chicken-uninstall-5],
  [
    if LC_ALL=C LANG=C ${ac_path_CHICKEN_UNINSTALL_5} -version 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^5\.' 2> /dev/null > /dev/null; then
        ac_cv_path_CHICKEN_UNINSTALL_5="${ac_path_CHICKEN_UNINSTALL_5}"
        ac_path_CHICKEN_UNINSTALL_5_found=:
    fi
  ])
AM_CONDITIONAL([CHICKEN_UNINSTALL_5],
               [test "${__ac_cv_path_CHICKEN_UNINSTALL_5_found}" != no])

StM_PATH_PROGS_CACHED_AND_PRECIOUS([CHICKEN_INSTALL_6],
  [chicken-install version 6],[chicken-install chicken-install-6],
  [
    if LC_ALL=C LANG=C ${ac_path_CHICKEN_INSTALL_6} -version 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^6\.' 2> /dev/null > /dev/null; then
        ac_cv_path_CHICKEN_INSTALL_6="${ac_path_CHICKEN_INSTALL_6}"
        ac_path_CHICKEN_INSTALL_6_found=:
    fi
  ])
AM_CONDITIONAL([CHICKEN_INSTALL_6],
               [test "${__ac_cv_path_CHICKEN_INSTALL_6_found}" != no])

StM_PATH_PROGS_CACHED_AND_PRECIOUS([CHICKEN_UNINSTALL_6],
  [chicken-uninstall version 6],[chicken-uninstall chicken-uninstall-6],
  [
    if LC_ALL=C LANG=C ${ac_path_CHICKEN_UNINSTALL_6} -version 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^6\.' 2> /dev/null > /dev/null; then
        ac_cv_path_CHICKEN_UNINSTALL_6="${ac_path_CHICKEN_UNINSTALL_6}"
        ac_path_CHICKEN_UNINSTALL_6_found=:
    fi
  ])
AM_CONDITIONAL([CHICKEN_UNINSTALL_6],
               [test "${__ac_cv_path_CHICKEN_UNINSTALL_6_found}" != no])

# You can use ‘@GNU@’ to make GNU Make constructs unbothersome to
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

if test "${__ac_cv_path_CHICKEN_INSTALL_5_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Support for ${CHICKEN_INSTALL_5} will be included.])
  AC_MSG_NOTICE([Use ‘make chicken-install-5 INSTALL_AS=sudo’ to install the pipchix egg.])
fi
if test "${__ac_cv_path_CHICKEN_INSTALL_6_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Support for ${CHICKEN_INSTALL_6} will be included.])
  AC_MSG_NOTICE([Use ‘make chicken-install-6 INSTALL_AS=sudo’ to install the pipchix egg.])
  AC_MSG_NOTICE([(This CHICKEN 6 support is likely not to work yet, however.)])
fi
AC_MSG_NOTICE([])

# local variables:
# coding: utf-8
# end:
