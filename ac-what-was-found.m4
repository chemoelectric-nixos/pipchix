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

if test "${__ac_cv_path_CHEZ_SCHEME_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Chez Scheme ${CHEZ_SCHEME} was found.])
fi
if test "${__ac_cv_path_CHIBI_SCHEME_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Chibi Scheme ${CHIBI_SCHEME} was found.])
fi
if test "${__ac_cv_path_CSI_5_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([The CHICKEN 5 interpreter ${CSI_5} was found.])
fi
if test "${__ac_cv_path_CSC_5_found}" != no; then
  test "${__ac_cv_path_CSI_5_found}" != no || AC_MSG_NOTICE([])
  AC_MSG_NOTICE([The CHICKEN 5 compiler ${CSC_5} was found.])
fi
if test "${__ac_cv_path_CHICKEN_INSTALL_5_found}" != no; then
  AC_MSG_NOTICE([${CHICKEN_INSTALL_5} was found.])
  AC_MSG_NOTICE([🔵 Use ‘make chicken-install-5 INSTALL_AS=sudo’ to install the ‘pipchix’ egg.])
  if test "${__ac_cv_path_CHICKEN_UNINSTALL_5_found}" != no; then
    AC_MSG_NOTICE([${CHICKEN_UNINSTALL_5} was found.])
    AC_MSG_NOTICE([🔵 Use ‘make chicken-uninstall-5 INSTALL_AS=sudo’ to uninstall the ‘pipchix’ egg.])
  fi
fi
if test "${__ac_cv_path_GAUCHE_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Gauche Scheme ${GAUCHE} was found.])
fi
if test "${__ac_cv_path_GUILE_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Guile version 3 ${GUILE} was found.])
fi
if test "${__ac_cv_path_MOSH_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Mosh Scheme was found.])
fi
if test "${__ac_cv_path_SAGITTARIUS_found}" != no; then
  AC_MSG_NOTICE([])
  AC_MSG_NOTICE([Sagittarius Scheme ${SAGITTARIUS} was found.])
fi
AC_MSG_NOTICE([])
