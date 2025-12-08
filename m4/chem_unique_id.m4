# Copyright (C) 2025 Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 2

# chem_unique_id([CHEM_UNIQUE_ID([VAR])
# -------------------------------------
#
# Set VAR to shell code for a unique identifier. The result is
# AC_SUBSTed and is also precious. Cached as chem_cv_unique_id.
#
# For no good reason, we prefer to create identifiers in the common
# UUID format. But our purpose is mainly to use the identifiers in
# filenames and such during build processes.
#
AC_DEFUN([CHEM_UNIQUE_ID],
[AC_ARG_VAR([$1],[shell code to create a unique identifier without spaces])
if test -n "@S|@{$1}"; then chem_cv_unique_id="@S|@{$1}"; fi
AC_CACHE_CHECK([for shell code to create a unique identifier without spaces or newline],[chem_cv_unique_id],
[if true; then
     # Using awk to chop a final newline is more portable than "head
     # -c-1", which is a GNU coreutils extension. The only option
     # POSIX requires of head is -n.
     __chop='awk '\''{printf "%s",@S|@0}'\'
     __os=`uname -s`
     if test "@S|@{__os}" = Linux &&
             test -r /proc/sys/kernel/random/uuid; then
         # Use the Linux proc filesystem.
         chem_cv_unique_id='if :;then cat /proc/sys/kernel/random/uuid|'@S|@{__chop}';fi'
     elif test "@S|@{__os}" = Linux &&
             uuidgen -V 2>&1 | grep -q 'uuidgen from util-linux'; then
         # Using the util-linux command uuidgen.
         chem_cv_unique_id='if :;then uuidgen|'@S|@{__chop}';fi'
     elif ruby -e 'require "securerandom";print SecureRandom.uuid' 1>/dev/null 2>/dev/null; then
         # Use Ruby.
         chem_cv_unique_id='ruby -e '\''require "securerandom";print SecureRandom.uuid'\'
     elif python -c 'import uuid; print (uuid.uuid4().urn)' 1>/dev/null 2>/dev/null; then
         # Use Python.
         chem_cv_unique_id='if :;then python -c '\''import uuid;print(uuid.uuid4().urn,end="")'\''|sed '\''s/^urn:uuid://'\'';fi'
     elif perl -e 'use Data::UUID;print Data::UUID->new()->create_str();' 1>/dev/null 2>/dev/null; then
         # Use Perl.
         chem_cv_unique_id='perl -e '\''use Data::UUID;print Data::UUID->new()->create_str();'\'
     elif node -e 'console.log(crypto.randomUUID())' 1>/dev/null 2>/dev/null; then
         # Use nodejs.
         chem_cv_unique_id='if :;then node -e '\''console.log(crypto.randomUUID())'\''|'@S|@{__chop}';fi'
     elif openssl rand -hex 16 1>/dev/null 2>/dev/null; then
         # Use OpenSSL to generate a random 16-digit hex number. Then
         # use someone's magical combination of POSIX commands to put that
         # number into UUID format.
         chem_cv_unique_id='if :;then openssl rand -hex 16|tr '\''@<:@:lower:@:>@'\'' '\''@<:@:upper:@:>@'\''|sed '\''s/\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)/\1\2\3\4-\5\6-\7\8-\9\10-\11\12\13\14\15\16/'\''|awk '\''{uuid=@S|@0;sub("-..-","-4!-",uuid);sub("-..-","-8!-",uuid);gsub("!","-",uuid);printf uuid}'\'';fi'
     else
         # As a last resort, use ${RANDOM} and issue a warning.
         chem_cv_unique_id='printf "%04x%s" ${RANDOM} `date +%Y%j%H%M%S`'
         AC_MSG_WARN([])
         AC_MSG_WARN([])
         AC_MSG_WARN([No good unique-identifier method found. Using "\@S|@{RANDOM}" and "date".])
         AC_MSG_WARN([])
         AC_MSG_WARN([])
     fi
 fi])
 $1="${chem_cv_unique_id}"
 AC_SUBST([$1])
])

# local variables:
# mode: shell-script
# sh-shell: sh
# coding: ascii
# end:
