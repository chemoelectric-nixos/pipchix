;;;
;;; Copyright © 2025 Barry Schwartz
;;;
;;; This file is part of Pipchix.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of Pipchix and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;

;;; Is an object a Nix AST node?
nix-abstract-syntax-tree?
nix-ast? ;; A synonym?

;;; Output Nix AST to a procedure, and by default to
;;; (current-output-port).
output-nix-abstract-syntax-tree
output-nix-ast ;; A synonym.

;;; Transform Scheme objects to Nix AST.
scheme->nix

;;; Some Nix builtins.
nix-false ;; (scheme->nix #f)
nix-true  ;; (scheme->nix #t)
nix-null  ;; (scheme->nix '())

nix-false?   ;; Is an object nix-false ?
nix-true?    ;; Is an object nix-true ?
nix-boolean? ;; Is an object nix-false or nix-true ?
nix-null?    ;; Is an object nix-null ?

;;; Nix list from elements.
nix-list

;;; Set attributes non-recursively.
nix-set

;;; Set attributes recursively.
nix-setrec

;;; Recursive let (that is, the ‘let’ built into Nix).
nix-letrec

;;; Non-recursive let.
nix-let

;;; One-after-another let.
nix-let*

;;; The value of an attribute or identifier.
nix-get
© ;; U+00A9 COPYRIGHT SIGN is a synonym for nix-get.

;;; Encode a string in base64, for printing with the base64(1) command
;;; (which is included in GNU coreutils).
base64-utf8

;;; Encode a string for printing with printf(1) or echo(1).
escaped-utf8

;;; Return a string for shell code to print a string, including a
;;; shell command to do the printing.
shell-print

;;; The encoding format used by shell-print, if the default is
;;; selected.
current-shell-print-format

;;; Set a new default format for shell-print, within a given thunk,
;;; but without changing the setting in the current dynamic context.
with-shell-print-format

;;; Set a new default format for shell-print, within the current
;;; dynamic context.
set-shell-print-format!

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
