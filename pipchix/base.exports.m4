nix-list       #|  Nix list from elements.          |#
nix-set        #|  Set attributes non-recursively.  |#
nix-setrec     #|  Set attributes recursively.      |#

base64-utf8    #|  Encode a string in base64. Thus  |#
               #|  a shell script can print it with |#
               #|  the coreutils ‘base64’ command.  |#

escaped-utf8   #|  Encode a string for printing     |#
               #|  with printf(1) or echo(1).       |#

print-utf8     #|  Return a string for shell code   |#
               #|  to print a string in UTF-8. The  |#
               #|  format for encoding the string   |#
               #|  to print is selectable.          |#
current-print-utf8-format  #| A parameter object.   |#
parameterize   #|  This has to be reëxported, in    |#
               #|  case it is was made available    |#
               #|  by (pipchix srfi-39).            |#
