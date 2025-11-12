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

AC_CONFIG_SRCDIR([pipchix/base.sld])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])
AC_CANONICAL_BUILD                 dnl On what platform are we compiling?
AC_CANONICAL_HOST                  dnl For what platform are we compiling?
AC_USE_SYSTEM_EXTENSIONS           dnl Define such macros as _GNU_SOURCE.

AM_INIT_AUTOMAKE([tar-pax no-dist-gzip dist-xz])
AM_MAINTAINER_MODE
#LT_INIT

#AC_PROG_CC
#AC_PROG_CC_C_O
#AX_CHECK_COMPILE_FLAG([-std=gnu23],
#  [CFLAGS="${CFLAGS}${CFLAGS+ }-std=gnu23"],
#  [],[],[AC_LANG_SOURCE([int main(){return 0;}])])
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
AC_PROG_AWK
AC_PROG_SED
AC_PROG_FGREP
AC_PROG_EGREP
#AC_CHECK_PROGS([HELP2MAN],[help2man])

#if test -z "${HELP2MAN}"; then
#   AC_MSG_ERROR([Please install help2man. It is needed to make the manpage.])
#fi

AC_ARG_WITH([r7rsdir],
  [AS_HELP_STRING([--with-r7rsdir=DIR],
        [where to put R⁷RS source code [DATAROOTDIR/r7rs]])],
  [r7rsdir="${withval}"],
  [r7rsdir='${datadir}/r7rs'])
AC_SUBST([r7rsdir])

# You can use ‘@GNU@’ to make GNU Make constructs unbothersome to
# Automake. (By the way, you often can use ‘$(eval ...)’ as well, but
# @GNU@ here may be better.)
AC_SUBST([GNU],[])

StM_REQUIRE_GNU_MAKE_IN_PATH
StM_CONFIG_MAKEFILES

m4_foreach([f],
  [[pipchix/base.sld],
   [pipchix/abstract-syntax-tree.sld]],
  [AC_CONFIG_LINKS(f:f)])


AC_OUTPUT

# local variables:
# coding: utf-8
# end:
