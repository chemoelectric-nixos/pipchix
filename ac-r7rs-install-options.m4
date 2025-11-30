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

m4_define([r7rspipchixdir_reldefault],[r7rs/pipchix])
m4_define([r7rspipchixdir_default],[\$(datadir)/r7rspipchixdir_reldefault])
AC_ARG_ENABLE([install-r7rs-source],
  [AS_HELP_STRING([--enable-install-r7rs-source[[=DIR]]],
        [install R⁷RS source code
         @<:@DATAROOTDIR/]r7rspipchixdir_reldefault@:>@
         [(by default not installed)])],
  [install_r7rs_source="${enableval}"],
  [install_r7rs_source=no])
AC_SUBST([install_r7rs_source])
AM_CONDITIONAL([INSTALL_R7RS_SOURCE],
               [test "${install_r7rs_source}" != no]) dnl FIXME: COME UP WITH A BETTER yes/no TEST.##################################
r7rspipchixdir="${r7rspipchixdir:-r7rspipchixdir_default}"
AC_SUBST([r7rspipchixdir])
