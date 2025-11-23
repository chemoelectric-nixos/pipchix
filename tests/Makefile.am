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

.DELETE_ON_ERROR:

EXTRA_DIST =
BUILT_SOURCES =
CLEANFILES =
DISTCLEANFILES =

check_PROGRAMS =
check_SCRIPTS =

DISTCLEANFILES += Makefile
DISTCLEANFILES += GNUmakefile
DISTCLEANFILES += atconfig

$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	$(AWK) \
	  'BEGIN{print "# Signature of the current package."; \
	         print "m4_define([AT_PACKAGE_NAME],"; \
	         print "  [$(PACKAGE_NAME)])"; \
	         print "m4_define([AT_PACKAGE_TARNAME],"; \
	         print "  [$(PACKAGE_TARNAME)])"; \
	         print "m4_define([AT_PACKAGE_VERSION],"; \
	         print "  [$(PACKAGE_VERSION)])"; \
	         print "m4_define([AT_PACKAGE_STRING],"; \
	         print "  [$(PACKAGE_STRING)])"; \
	         print "m4_define([AT_PACKAGE_BUGREPORT],"; \
	         print "  [$(PACKAGE_BUGREPORT)])"; \
	         print "m4_define([AT_PACKAGE_URL],"; \
	         print "  [$(PACKAGE_URL)])"}' \
	  > $(@).tmp && mv $(@){.tmp,}

CLEANFILES += *.tmp
CLEANFILES += tmp.*

EXTRA_DIST += testsuite.at
EXTRA_DIST += $(srcdir)/package.m4
EXTRA_DIST += $(TESTSUITE)
EXTRA_DIST += atlocal.in

CHECK_LOCAL_DEPS =
CHECK_LOCAL_DEPS += atconfig
CHECK_LOCAL_DEPS += atlocal
CHECK_LOCAL_DEPS += $(TESTSUITE)

check-local: $(CHECK_LOCAL_DEPS)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	  $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || \
	 $(SHELL) '$(TESTSUITE)' --clean

AUTOM4TE = $(SHELL) $(top_srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

TESTSUITE = $(srcdir)/testsuite

TESTSUITE_DEPS =
TESTSUITE_DEPS += $(srcdir)/package.m4
TESTSUITE_DEPS += $(srcdir)/testsuite.at
TESTSUITE_DEPS += $(srcdir)/tests-template.at

$(TESTSUITE): $(TESTSUITE_DEPS)
	$(AUTOTEST) -I '$(srcdir)' -o $(@).tmp $(@).at && \
	  mv $(@){.tmp,}

../config.status: run-r6rs-chez.sh.in
