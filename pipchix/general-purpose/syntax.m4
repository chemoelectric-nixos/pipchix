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

(define-syntax c-cons (syntax-rules () ((_ s x y) (cstx-cons s x y))))
(define-syntax c-append (syntax-rules () ((_ s . args) (cstx-append s . args))))
(define-syntax c-list->vector (syntax-rules () ((_ s x y) (cstx-list->vector s x y))))

m4_define(«c_cons_provided»,«yes»)
m4_define(«c_append_provided»,«yes»)
m4_define(«c_list_to_vector_provided»,«yes»)
define_ck_macros

(define-syntax cstx
  (syntax-rules ()
    ((_ x)
     (ck () x))))

(define-syntax cstx-quote (syntax-rules () ((_ s x) (c-quote s x))))
(define-syntax cstx-quasiquote (syntax-rules () ((_ s x) (c-quasiquote s x))))
(define-syntax cstx-eval (syntax-rules () ((_ s x) (c-eval s x))))

;;;m4_ifelse(scheme_standard,«r6rs»,«
;;;m4_define(«eval_environment»,(environment '(rnrs)))
;;;»,scheme_standard,«r5rs»,«
;;;m4_define(«eval_environment»,(interaction-environment))
;;;»,«
;;;m4_define(«eval_environment»,(interaction-environment))
;;;»)

;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-satisfied?
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((args (cdr form))
            (pred-form (caar args))
            (pred (eval pred-form eval_environment))
            (x (cadar args)))
       (if (pred x)
         (cadr args)
         (when (pair? (cddr args))
           (caddr args)))))))
;;;»,«
(define-syntax stx-satisfied?
  (lambda (stx)
    (syntax-case stx ()
      ((_ (predicate x) if-true if-false)
       (let ((pred (eval (syntax->datum (syntax predicate))
                         eval_environment)))
         (if (pred (syntax->datum (syntax x)))
           (syntax if-true)
           (syntax if-false))))
      ((_ (predicate x) if-true)
       (let ((pred (eval (syntax->datum (syntax predicate))
                         eval_environment)))
         (when (pred (syntax->datum (syntax x)))
           (syntax if-true)))))))
;;;»)

;;;m4_divert(-1)
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;m4_define(«simple_predicate_branch»,«
(define-syntax $1
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((args (cdr form))
            (x (car args)))
       (if ((if (symbol? $2) (rename $2) $2) x)
         (cadr args)
         (when (pair? (cddr args))
           (caddr args)))))))
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
       (when ($2 (syntax->datum (syntax x)))
         (syntax if-true))))))
;;;»)
;;;»)
;;;m4_define(«simple_typetest_branch»,simple_predicate_branch(stx-$1?,$1?))
;;;;;;m4_divert

simple_typetest_branch(boolean)
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

;;;m4_define(«one_argument_procedure»,«
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-$1                   ; An ordinary macro.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((args (cdr form)))
       (let ((f (if (symbol? $1) (rename $1) $1))
             (x (car args)))
         (f x))))))

(define-syntax cstx-$1                  ; A ck-macro.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((ck (rename 'ck))
           (s (cadr form))
           (args (cddr form)))
       (let* ((f (if (symbol? $1) (rename $1) $1))
              (x (eval (car args) eval_environment))
              (y (f x))
              (retval `(,ck ,s ',y)))
         retval)))))
;;;»,«
(define-syntax stx-$1                   ; An ordinary macro.
  (lambda (stx)
    (syntax-case stx ()
      ((¶ x)
       (let ((x^ (syntax->datum (syntax x))))
         (datum->syntax (syntax ¶) ($1 x^)))))))

(define-syntax cstx-$1                  ; A ck-macro.
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s x)
       (let* ((f $1)
              (t (eval (syntax->datum (syntax x)) eval_environment))
              (y (datum->syntax (syntax ¶) (f t))))
         (quasisyntax (ck s '(unsyntax y))))))))
;;;»)
;;;») ;;; one_argument_procedure

;;;m4_define(«two_argument_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-NAME                 ; An ordinary macro.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((args (cdr form)))
       (let ((f (if (symbol? PROC) (rename PROC) PROC))
             (x (car args))
             (y (cadr args)))
         (f x y))))))

(define-syntax cstx-NAME                ; A ck-macro.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((ck (rename 'ck))
           (s (cadr form))
           (args (cddr form)))
       (let* ((f (if (symbol? PROC) (rename PROC) PROC))
              (x (eval (car args) eval_environment))
              (y (eval (cadr args) eval_environment))
              (z (f x y))
              (retval `(,ck ,s ',z)))
         retval)))))
;;;»,«
(define-syntax stx-NAME                 ; An ordinary macro.
  (lambda (stx)
    (syntax-case stx ()
      ((¶ x y)
       (let* ((f PROC)
              (u (syntax->datum (syntax x)))
              (v (syntax->datum (syntax y)))
              (z (datum->syntax (syntax ¶) (f u v))))
         z)))))

(define-syntax cstx-NAME                ; A ck-macro.
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s x y)
       (let* ((f PROC)
              (u (eval (syntax->datum (syntax x)) eval_environment))
              (v (eval (syntax->datum (syntax y)) eval_environment))
              (z (datum->syntax (syntax ¶) (f u v))))
         (quasisyntax (ck s '(unsyntax z))))))))
;;;»)
m4_popdef(«NAME»,«PROC»)
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

;;;
;;; These work fine, but cannot be made by our m4 code into ck-macros:
;;;
;;; (define-syntax stx-first (syntax-rules () ((_ (a . ω)) a)))
;;; (define-syntax stx-second (syntax-rules () ((_ (a b . ω)) b)))
;;; (define-syntax stx-third (syntax-rules () ((_ (a b c . ω)) c)))
;;; (define-syntax stx-fourth (syntax-rules () ((_ (a b c d . ω)) d)))
;;; (define-syntax stx-fifth (syntax-rules () ((_ (a b c d e . ω)) e)))
;;; (define-syntax stx-sixth (syntax-rules () ((_ (a b c d e f . ω)) f)))
;;; (define-syntax stx-seventh (syntax-rules () ((_ (a b c d e f g . ω)) g)))
;;; (define-syntax stx-eighth (syntax-rules () ((_ (a b c d e f g h . ω)) h)))
;;; (define-syntax stx-ninth (syntax-rules () ((_ (a b c d e f g h i . ω)) i)))
;;; (define-syntax stx-tenth (syntax-rules () ((_ (a b c d e f g h i j . ω)) j)))

one_argument_procedure(first)
one_argument_procedure(second)
one_argument_procedure(third)
one_argument_procedure(fourth)
one_argument_procedure(fifth)
one_argument_procedure(sixth)
one_argument_procedure(seventh)
one_argument_procedure(eighth)
one_argument_procedure(ninth)
one_argument_procedure(tenth)

one_argument_procedure(last)
one_argument_procedure(last-pair)
one_argument_procedure(length)
one_argument_procedure(reverse)
one_argument_procedure(list->vector)

two_argument_procedure(cons)

;;; - - - - - - - - - - - - - - - - - - - -

two_argument_procedure(append%%,append)

(define-syntax stx-append
  (syntax-rules ()
    ((_ x y)
     (stx-append%% x y))))

(define-syntax cstx-append
  (syntax-rules ( quote )
    ((_ s)
     (ck s '()))
    ((_ s '(a ...))
     (ck s '(a ...)))
    ((_ s x y)
     (ck s (cstx-append%% x y)))
    ((_ s x y . more)
     (ck s (cstx-append (cstx-append%% x y) . more)))))

;;; - - - - - - - - - - - - - - - - - - - -

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'with-syntax 'scheme-indent-function 1)
;;; eval: (put 'stx-satisfied? 'scheme-indent-function 1)
;;; eval: (put 'stx-integer? 'scheme-indent-function 1)
;;; eval: (put 'stx-exact? 'scheme-indent-function 1)
;;; eval: (put 'stx-list? 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
