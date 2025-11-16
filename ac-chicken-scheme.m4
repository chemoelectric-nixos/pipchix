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

#
# CHICKEN Scheme configuration.
#

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

StM_PATH_PROGS_CACHED_AND_PRECIOUS([CSI_5],
  [CHICKEN interpreter version 5],[csi csi-5],
  [
    if LC_ALL=C LANG=C ${ac_path_CSI_5} -version 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^CHICKEN' 2> /dev/null > /dev/null; then
        if LC_ALL=C LANG=C ${ac_path_CSI_5} -version 2>&1 | \
          LC_ALL=C LANG=C ${GREP} '^Version 5\.' 2> /dev/null > /dev/null; then
            ac_cv_path_CSI_5="${ac_path_CSI_5}"
            ac_path_CSI_5_found=:
        fi
    fi
  ])
AM_CONDITIONAL([CSI_5],[test "${__ac_cv_path_CSI_5_found}" != no])

StM_PATH_PROGS_CACHED_AND_PRECIOUS([CSC_5],
  [CHICKEN compiler version 5],[csc csc-5],
  [
    if LC_ALL=C LANG=C ${ac_path_CSC_5} -version 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^CHICKEN' 2> /dev/null > /dev/null; then
        if LC_ALL=C LANG=C ${ac_path_CSC_5} -version 2>&1 | \
          LC_ALL=C LANG=C ${GREP} '^Version 5\.' 2> /dev/null > /dev/null; then
            ac_cv_path_CSC_5="${ac_path_CSC_5}"
            ac_path_CSC_5_found=:
        fi
    fi
  ])
AM_CONDITIONAL([CSC_5],[test "${__ac_cv_path_CSC_5_found}" != no])

################################################################################
#
# FIXME: NO SUPPORT FOR CHICKEN 6 YET.
#
__ac_cv_path_CHICKEN_INSTALL_6_found=no
__ac_cv_path_CHICKEN_UNINSTALL_6_found=no
AM_CONDITIONAL([CHICKEN_INSTALL_6],[false])
AM_CONDITIONAL([CHICKEN_UNINSTALL_6],[false])
#
#StM_PATH_PROGS_CACHED_AND_PRECIOUS([CHICKEN_INSTALL_6],
#  [chicken-install version 6],[chicken-install chicken-install-6],
#  [
#    if LC_ALL=C LANG=C ${ac_path_CHICKEN_INSTALL_6} -version 2>&1 | \
#      LC_ALL=C LANG=C ${GREP} '^6\.' 2> /dev/null > /dev/null; then
#        ac_cv_path_CHICKEN_INSTALL_6="${ac_path_CHICKEN_INSTALL_6}"
#        ac_path_CHICKEN_INSTALL_6_found=:
#    fi
#  ])
#AM_CONDITIONAL([CHICKEN_INSTALL_6],
#               [test "${__ac_cv_path_CHICKEN_INSTALL_6_found}" != no])
#
#StM_PATH_PROGS_CACHED_AND_PRECIOUS([CHICKEN_UNINSTALL_6],
#  [chicken-uninstall version 6],[chicken-uninstall chicken-uninstall-6],
#  [
#    if LC_ALL=C LANG=C ${ac_path_CHICKEN_UNINSTALL_6} -version 2>&1 | \
#      LC_ALL=C LANG=C ${GREP} '^6\.' 2> /dev/null > /dev/null; then
#        ac_cv_path_CHICKEN_UNINSTALL_6="${ac_path_CHICKEN_UNINSTALL_6}"
#        ac_path_CHICKEN_UNINSTALL_6_found=:
#    fi
#  ])
#AM_CONDITIONAL([CHICKEN_UNINSTALL_6],
#               [test "${__ac_cv_path_CHICKEN_UNINSTALL_6_found}" != no])
#
################################################################################
