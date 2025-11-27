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

$(srcdir)/package.m4: $(top_srcdir)/configure.ac $(srcdir)/Makefile.am
	$(AWK) \
	  'BEGIN{print "# Signature of the current package."; \
	         printf "m4_define([AT_PACKAGE_NAME],"; \
	         printf "[$(PACKAGE_NAME)])\n"; \
	         printf "m4_define([AT_PACKAGE_TARNAME],"; \
	         printf "[$(PACKAGE_TARNAME)])\n"; \
	         printf "m4_define([AT_PACKAGE_VERSION],"; \
	         printf "[$(PACKAGE_VERSION)])\n"; \
	         printf "m4_define([AT_PACKAGE_STRING],"; \
	         printf "[$(PACKAGE_STRING)])\n"; \
	         printf "m4_define([AT_PACKAGE_BUGREPORT],"; \
	         printf "[$(PACKAGE_BUGREPORT)])\n"; \
	         printf "m4_define([AT_PACKAGE_URL],"; \
	         printf "[$(PACKAGE_URL)])\n"}' \
	  > $(@).tmp && mv $(@){.tmp,}

CLEANFILES += *.tmp
CLEANFILES += tmp.*

EXTRA_DIST += testsuite.at
EXTRA_DIST += $(srcdir)/package.m4
EXTRA_DIST += $(TESTSUITE)
EXTRA_DIST += atlocal.in

@GNU@ define jobs-flag
@GNU@ $(filter -j%,$(MAKEFLAGS))
@GNU@ endef

@GNU@ define color-flag
@GNU@ $(shell \
@GNU@   if test "${enable_color}" = no; then \
@GNU@     printf '%s' "--color=never"; \
@GNU@   else \
@GNU@     printf '%s' "--color=auto"; \
@GNU@   fi \
@GNU@ )
@GNU@ endef

check-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' \
	   $(color-flag) $(jobs-flag) $(TESTSUITEFLAGS)

installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	   $(color-flag) $(jobs-flag) $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || \
	 $(SHELL) '$(TESTSUITE)' --clean

AUTOM4TE = $(SHELL) $(top_srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

TESTSUITE = $(srcdir)/testsuite

TESTSUITE_DEPS =
TESTSUITE_DEPS += $(srcdir)/atlocal.in
TESTSUITE_DEPS += $(srcdir)/package.m4
TESTSUITE_DEPS += $(srcdir)/tests-template.at
#
TESTSUITE_DEPS += $(srcdir)/ast-nodes-preds-template.at
#
TESTSUITE_DEPS += $(srcdir)/nix-attributebinding-node-template.at
TESTSUITE_DEPS += $(srcdir)/nix-attributepath-node-template.at
TESTSUITE_DEPS += $(srcdir)/nix-attributeset-node-template.at
TESTSUITE_DEPS += $(srcdir)/nix-data-node-template.at
TESTSUITE_DEPS += $(srcdir)/nix-embedded-node-template.at
TESTSUITE_DEPS += $(srcdir)/nix-inherit-node-template.at
TESTSUITE_DEPS += $(srcdir)/nix-path-node-template.at
#
TESTSUITE_DEPS += $(srcdir)/nix-embed-template.at
TESTSUITE_DEPS += $(srcdir)/nix-list-template.at

$(TESTSUITE): $(srcdir)/testsuite.at $(TESTSUITE_DEPS)
	$(AUTOTEST) -I '$(srcdir)' -o $(@).tmp $(<) && \
	  mv $(@){.tmp,}
