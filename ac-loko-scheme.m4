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
# Loko Scheme configuration.
#

CHEM_PATH_PROGS_CACHED_AND_PRECIOUS([LOKO],
  [Loko Scheme],[loko],
  [
    if LC_ALL=C LANG=C ${ac_path_LOKO} --verbose 2>&1 | \
      LC_ALL=C LANG=C ${GREP} '^Loko Scheme' \
               2> /dev/null > /dev/null; then
        ac_cv_path_LOKO="${ac_path_LOKO}"
        ac_path_LOKO_found=:
    fi
  ])
AM_CONDITIONAL([LOKO],[test "${chem_cv_path_LOKO_found}" != no])
