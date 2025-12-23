;;;;; --------------------------------------------------

cstx
cstx-quote
cstx-quasiquote
cstx-eval

cstx-true
cstx-false
cstx-if  cstx-if*
cstx-and cstx-and*
cstx-or  cstx-or*

;;;;; --------------------------------------------------


;;;;; FIXME:
;;;;; FIXME: ARE THESE WORTH KEEPING?
;;;;; FIXME:


if-stx-satisfied

if-stx-boolean
if-stx-number
if-stx-exact
if-stx-inexact
if-stx-integer
if-stx-exact-integer
if-stx-rational
if-stx-real
if-stx-complex
if-stx-finite
if-stx-infinite
if-stx-nan

if-stx-symbol
if-stx-string
if-stx-char
if-stx-vector
if-stx-bytevector

if-stx-null
if-stx-pair
if-stx-list

;;;;; --------------------------------------------------


;;;;; FIXME:
;;;;; FIXME: ARE THE stx- VERSIONS WORTH KEEPING?
;;;;; FIXME:


;;; m4_define(stx_cstx,stx-$1 cstx-$1)

stx_cstx(list?)
stx_cstx(proper-list?)
stx_cstx(circular-list?)
stx_cstx(dotted-list?)
stx_cstx(pair?)
stx_cstx(null?)
stx_cstx(null-list?)
stx_cstx(not-pair?)

cstx-exact-integer?

stx_cstx(number?)
stx_cstx(exact?)
stx_cstx(inexact?)
stx_cstx(integer?)
stx_cstx(rational?)
stx_cstx(real?)
stx_cstx(complex?)
stx_cstx(finite?)
stx_cstx(infinite?)
stx_cstx(nan?)
stx_cstx(boolean?)
stx_cstx(symbol?)
stx_cstx(string?)
stx_cstx(char?)
stx_cstx(vector?)
stx_cstx(bytevector?)

stx_cstx(car)
stx_cstx(cdr)
stx_cstx(caar)
stx_cstx(cadr)
stx_cstx(cdar)
stx_cstx(cddr)
stx_cstx(caaaar)
stx_cstx(caaar)
stx_cstx(caaddr)
stx_cstx(cadaar)
stx_cstx(cadar)
stx_cstx(cadddr)
stx_cstx(cdaaar)
stx_cstx(cdaar)
stx_cstx(cdaddr)
stx_cstx(cddaar)
stx_cstx(cddar)
stx_cstx(cddddr)
stx_cstx(caaadr)
stx_cstx(caadar)
stx_cstx(caadr)
stx_cstx(cadadr)
stx_cstx(caddar)
stx_cstx(caddr)
stx_cstx(cdaadr)
stx_cstx(cdadar)
stx_cstx(cdadr)
stx_cstx(cddadr)
stx_cstx(cdddar)
stx_cstx(cdddr)

stx_cstx(first)
stx_cstx(second)
stx_cstx(third)
stx_cstx(fourth)
stx_cstx(fifth)
stx_cstx(sixth)
stx_cstx(seventh)
stx_cstx(eighth)
stx_cstx(ninth)
stx_cstx(tenth)

stx_cstx(last)
stx_cstx(last-pair)
stx_cstx(length)
stx_cstx(reverse)
stx_cstx(list->vector)

stx_cstx(cons)
stx_cstx(xcons)
stx_cstx(cons*)
stx_cstx(list)
stx_cstx(append)

stx_cstx(not)

;;;;; --------------------------------------------------
