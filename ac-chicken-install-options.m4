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

m4_define([chickeneggsdir_reldefault],[chicken/eggs])
m4_define([chickeneggsdir_default],[\$(datadir)/chickeneggsdir_reldefault])
chickeneggsdir="${chickeneggsdir:-chickeneggsdir_default}"
AC_SUBST([chickeneggsdir])

m4_define([chickeneggs5pipchixdir_reldefault],[5/pipchix])
m4_define([chickeneggs5pipchixdir_default],[\$(chickeneggsdir)/chickeneggs5pipchixdir_reldefault])
AC_ARG_ENABLE([install-chicken-5-egg-source],
  [AS_HELP_STRING([--enable-install-chicken-5-egg-source[[=DIR]]],
        [install CHICKEN 5 egg source code
         @<:@DATAROOTDIR/]chickeneggsdir_reldefault/chickeneggs5pipchixdir_reldefault@:>@
         [(by default not installed)])],
  [install_chicken_5_egg_source="${enableval}"],
  [install_chicken_5_egg_source=no])
AC_SUBST([install_chicken_5_egg_source])
AM_CONDITIONAL([INSTALL_CHICKEN_5_EGG_SOURCE],
               [test "${install_chicken_5_egg_source}" != no])
chickeneggs5pipchixdir="${chickeneggs5pipchixdir:-chickeneggs5pipchixdir_default}"
AC_SUBST([chickeneggs5pipchixdir])
