delete-duplicate-identifiers

syx-proper-list-length

syx-identity
syx-values1 syx-values2 syx-values3

;;;-------------------------------------------------------------------
;;;
;;; Matchers for syntactic objects.
;;;

if-identifier
if-free-identifier=
if-bound-identifier=
if-...

if-identifier-in-list

;;;; if-unbound-or-equiv-variable

msyx-proper-list
msyx-list
msyx-null-list
msyx-vector
msyx-literal

msyx-number
msyx-complex
msyx-real
msyx-rational
msyx-integer
msyx-exact-integer
msyx-exact
msyx-inexact
msyx-finite
msyx-infinite
msyx-nan
msyx-zero
msyx-positive
msyx-negative
msyx-odd
msyx-even

msyx-string

msyx-char
msyx-char-alphabetic
msyx-char-numeric
msyx-char-whitespace
msyx-char-upper-case
msyx-char-lower-case

msyx-boolean
msyx-false
msyx-true

msyx-car
msyx-cdr

msyx-quote
msyx-quasiquote
msyx-unquote
msyx-unquote-splicing

msyx-not
msyx-or
msyx-and
msyx-xor

;;;-------------------------------------------------------------------
;;;
;;; Scanners for syntactic objects.
;;;

syx-split-at-last-pair
syx-split

;;;-------------------------------------------------------------------
;;;
;;; Support for matchers for runtime objects.
;;;

default-initialization
has-default-initialization?
if-default-initialization-or-equiv-object
if-default-initialization-or-equiv-variable
extract-identifiers-from-proper-list
make-identifiers-environment

;;;-------------------------------------------------------------------
;;;
;;; Matchers for runtime objects.
;;;

match-proper-list

;;;-------------------------------------------------------------------
