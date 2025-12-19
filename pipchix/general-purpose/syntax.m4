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

;;;m4_divert(-1)
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;m4_define(«simple_typetest_branch»,«
(define-syntax stx-$1?
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((args (cdr form)))
       (if ($1? (car args))
         (cadr args)
         (caddr args))))))
;;;»)
;;;»,«
;;;m4_define(«simple_typetest_branch»,«
(define-syntax stx-$1?
  (lambda (stx)
    (syntax-case stx ()
      ((_ x if-true if-false)
       (if ($1? (syntax->datum (syntax x)))
         (syntax if-true)
         (syntax if-false))))))
;;;»)
;;;»)
;;;;;;m4_divert

simple_typetest_branch(number)
simple_typetest_branch(exact)
simple_typetest_branch(inexact)
simple_typetest_branch(integer)
simple_typetest_branch(rational)
simple_typetest_branch(real)
simple_typetest_branch(complex)
simple_typetest_branch(finite)
simple_typetest_branch(infinite)
simple_typetest_branch(nan)

;;;m4_ifelse(scheme_standard,«r7rs»,«
simple_typetest_branch(exact-integer)
;;;»,«
(define-syntax stx-exact-integer?
  (lambda (stx)
    (syntax-case stx ()
      ((_ x if-true if-false)
       (let ((x^ (syntax->datum (syntax x))))
         (if (and (integer? x^) (exact? x^))
           (syntax if-true)
           (syntax if-false)))))))
;;;»)

simple_typetest_branch(symbol)
simple_typetest_branch(string)
simple_typetest_branch(char)
simple_typetest_branch(vector)
simple_typetest_branch(bytevector)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'with-syntax 'scheme-indent-function 1)
;;; eval: (put 'stx-integer? 'scheme-indent-function 1)
;;; eval: (put 'stx-exact? 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
