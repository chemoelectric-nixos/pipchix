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

#
# C compiler flags for TinySCHEME.
#

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
