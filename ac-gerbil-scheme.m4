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
# Gerbil Scheme configuration.
#

CHEM_PATH_PROGS_CACHED_AND_PRECIOUS([GXI],
  [Gerbil Scheme interpreter (R⁷RS)],[gxi],
  [
    if LC_ALL=C LANG=C ${ac_path_GXI} -v 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^Gerbil .* on Gambit' \
               2> /dev/null > /dev/null; then
        ac_cv_path_GXI="${ac_path_GXI}"
        ac_path_GXI_found=:
    fi
  ])
AM_CONDITIONAL([GXI],[test "${chem_cv_path_GXI_found}" != no])

CHEM_PATH_PROGS_CACHED_AND_PRECIOUS([GXC],
  [Gerbil Scheme compiler (R⁷RS)],[gxc],
  [
    if LC_ALL=C LANG=C ${ac_path_GXC} -v 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^Gerbil .* on Gambit' \
               2> /dev/null > /dev/null; then
        ac_cv_path_GXC="${ac_path_GXC}"
        ac_path_GXC_found=:
    fi
  ])
AM_CONDITIONAL([GXC],[test "${chem_cv_path_GXC_found}" != no])

