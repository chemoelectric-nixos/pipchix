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

;;;
;;; The continuations interface of Marc Feeley’s paper, ‘A Better API
;;; for First-Class Continuations’,
;;; http://www.iro.umontreal.ca/~feeley/papers/FeeleySW01.pdf
;;; https://web.archive.org/web/20250701033312/http://www.iro.umontreal.ca/~feeley/papers/FeeleySW01.pdf
;;;
;;; See also SRFI-226: Control Features, which includes an interface
;;; derived from this one.
;;;

#|(define-record-type <continuation>
  (%%make-continuation proc)
  continuation?
  (proc %%continuation-proc))|#

(define (continuation-capture receiver)
  ((call/cc receiver)))

(define (continuation-graft cont thunk)
  (cont thunk))

(define (continuation-return cont . returned-values)
  (continuation-graft
   cont
   (lambda () (apply values returned-values))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
