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

;;;m4_ifelse(scheme_standard,«r6rs»,«
;;;m4_define(«stx_satisfied_environment»,(environment '(rnrs)))
;;;»,scheme_standard,«r5rs»,«
;;;m4_define(«stx_satisfied_environment»,(interaction-environment))
;;;»,«
;;;m4_define(«stx_satisfied_environment»,(interaction-environment))
;;;»)
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-satisfied?
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((args (cdr form))
            (pred-form (caar args))
            (pred (eval pred-form stx_satisfied_environment))
            (x (cadar args)))
       (if (pred x)
         (cadr args)
         (if (pair? (cddr args))
           (caddr args)
           '(if #f #f))))))) ;; <unspecified>
;;;»,«
(define-syntax stx-satisfied?
  (lambda (stx)
    (syntax-case stx ()
      ((_ (predicate x) if-true if-false)
       (let ((pred (eval (syntax->datum (syntax predicate))
                         stx_satisfied_environment)))
         (if (pred (syntax->datum (syntax x)))
           (syntax if-true)
           (syntax if-false))))
      ((_ (predicate x) if-true)
       (let ((pred (eval (syntax->datum (syntax predicate))
                         stx_satisfied_environment)))
         (if (pred (syntax->datum (syntax x)))
           (syntax if-true)
           (syntax (if #f #f)))))))) ;; <unspecified>
;;;»)

;;;m4_divert(-1)
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;m4_define(«simple_predicate_branch»,«
(define-syntax $1
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((args (cdr form)))
       (if ($2 (car args))
         (cadr args)
         (if (pair? (cddr args))
           (caddr args)
           '(if #f #f))))))) ;; <unspecified>
;;;»)
;;;»,«
;;;m4_define(«simple_predicate_branch»,«
(define-syntax $1
  (lambda (stx)
    (syntax-case stx ()
      ((_ x if-true if-false)
       (if ($2 (syntax->datum (syntax x)))
         (syntax if-true)
         (syntax if-false)))
      ((_ x if-true)
       (if ($2 (syntax->datum (syntax x)))
         (syntax if-true)
         (syntax (if #f #f))))))) ;; <unspecified>
;;;»)
;;;»)
;;;m4_define(«simple_typetest_branch»,simple_predicate_branch(stx-$1?,$1?))
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
simple_predicate_branch(stx-exact-integer?,
                        (lambda (x)
                          (and (integer? x) (exact? x))))
;;;»)

simple_typetest_branch(symbol)
simple_typetest_branch(string)
simple_typetest_branch(char)
simple_typetest_branch(vector)
simple_typetest_branch(bytevector)

simple_typetest_branch(null)
simple_typetest_branch(pair)
simple_typetest_branch(list)

;;;m4_divert(-1)
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;m4_define(«one_argument_procedure»,«
(define-syntax stx-$1
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((args (cdr form)))
       (if (and (pair? args)
                (null? (cdr args)))
         ($1 (car args))
         '(if #f #f)))))) ;; <unspecified>
;;;»)
;;;»,«
;;;m4_define(«one_argument_procedure»,«
(define-syntax stx-$1
  (lambda (stx)
    (syntax-case stx ()
      ((¶ x)
       (let ((x^ (syntax->datum (syntax x))))
         (datum->syntax (syntax ¶) ($1 x^)))))))
;;;»)
;;;»)
;;;m4_divert

one_argument_procedure(car)
one_argument_procedure(cdr)
one_argument_procedure(caar)
one_argument_procedure(cadr)
one_argument_procedure(cdar)
one_argument_procedure(cddr)
one_argument_procedure(caaaar)
one_argument_procedure(caaar)
one_argument_procedure(caaddr)
one_argument_procedure(cadaar)
one_argument_procedure(cadar)
one_argument_procedure(cadddr)
one_argument_procedure(cdaaar)
one_argument_procedure(cdaar)
one_argument_procedure(cdaddr)
one_argument_procedure(cddaar)
one_argument_procedure(cddar)
one_argument_procedure(cddddr)
one_argument_procedure(caaadr)
one_argument_procedure(caadar)
one_argument_procedure(caadr)
one_argument_procedure(cadadr)
one_argument_procedure(caddar)
one_argument_procedure(caddr)
one_argument_procedure(cdaadr)
one_argument_procedure(cdadar)
one_argument_procedure(cdadr)
one_argument_procedure(cddadr)
one_argument_procedure(cdddar)
one_argument_procedure(cdddr)

(define-syntax stx-first (syntax-rules () ((_ (a . ω)) a)))
(define-syntax stx-second (syntax-rules () ((_ (a b . ω)) b)))
(define-syntax stx-third (syntax-rules () ((_ (a b c . ω)) c)))
(define-syntax stx-fourth (syntax-rules () ((_ (a b c d . ω)) d)))
(define-syntax stx-fifth (syntax-rules () ((_ (a b c d e . ω)) e)))
(define-syntax stx-sixth (syntax-rules () ((_ (a b c d e f . ω)) f)))
(define-syntax stx-seventh (syntax-rules () ((_ (a b c d e f g . ω)) g)))
(define-syntax stx-eighth (syntax-rules () ((_ (a b c d e f g h . ω)) h)))
(define-syntax stx-ninth (syntax-rules () ((_ (a b c d e f g h i . ω)) i)))
(define-syntax stx-tenth (syntax-rules () ((_ (a b c d e f g h i j . ω)) j)))

one_argument_procedure(reverse)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'with-syntax 'scheme-indent-function 1)
;;; eval: (put 'stx-satisfied? 'scheme-indent-function 1)
;;; eval: (put 'stx-integer? 'scheme-indent-function 1)
;;; eval: (put 'stx-exact? 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
