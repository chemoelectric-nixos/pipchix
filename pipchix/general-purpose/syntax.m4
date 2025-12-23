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

(define-syntax c-cons (syntax-rules () ((_ s x y) (stx-cons s x y))))
(define-syntax c-append (syntax-rules () ((_ s . args) (stx-append s . args))))
(define-syntax c-list->vector (syntax-rules () ((_ s x y) (stx-list->vector s x y))))

m4_define(«c_cons_provided»,«yes»)m4_dnl
m4_define(«c_append_provided»,«yes»)m4_dnl
m4_define(«c_list_to_vector_provided»,«yes»)m4_dnl
define_ck_macros

(define-syntax stx
  (syntax-rules ()
    ((_ x)
     (ck () x))))

(define-syntax stx-quote (syntax-rules () ((_ s x) (c-quote s x))))
(define-syntax stx-quasiquote (syntax-rules () ((_ s x) (c-quasiquote s x))))
(define-syntax stx-eval (syntax-rules () ((_ s x) (c-eval s x))))

(define-syntax stx-true (syntax-rules () ((_ s . args) (c-true s . args))))
(define-syntax stx-false (syntax-rules () ((_ s . args) (c-false s . args))))
(define-syntax stx-if (syntax-rules () ((_ s x t f) (c-if s x t f))))
(define-syntax stx-if* (syntax-rules () ((_ s x t f) (c-if* s x t f))))
(define-syntax stx-and (syntax-rules () ((_ s . args) (c-and s . args))))
(define-syntax stx-and* (syntax-rules () ((_ s . args) (c-and* s . args))))
(define-syntax stx-or (syntax-rules () ((_ s . args) (c-or s . args))))
(define-syntax stx-or* (syntax-rules () ((_ s . args) (c-or* s . args))))

;;;m4_divert(-1)

m4_define(«stx_output»,«(if (boolean? $1) `,$1 $1)»)

;;;m4_ifelse(scheme_standard,«r6rs»,«
;;;m4_define(«eval_environment»,(environment '(rnrs)))
;;;»,scheme_standard,«r5rs»,«
;;;m4_define(«eval_environment»,(interaction-environment))
;;;»,«
;;;m4_define(«eval_environment»,(interaction-environment))
;;;»)

;;;m4_define(«one_argument_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-NAME
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((ck (rename 'ck))
           (s (cadr form))
           (args (cddr form)))
       (let* ((f (if (symbol? PROC) (rename PROC) PROC))
              (x (eval (car args) eval_environment))
              (y (f x))
              (retval `(,ck ,s ',y)))
         stx_output(retval))))))
;;;»,«
(define-syntax stx-NAME
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s x)
       (let* ((f PROC)
              (t (eval (syntax->datum (syntax x)) eval_environment))
              (y (datum->syntax (syntax ¶) (f t))))
         stx_output((quasisyntax (ck s '(unsyntax y)))))))))
;;;»)
m4_popdef(«NAME»,«PROC»)
;;;»)

;;;m4_define(«two_argument_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-NAME
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
         stx_output(retval))))))
;;;»,«
(define-syntax stx-NAME
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s x y)
       (let* ((f PROC)
              (u (eval (syntax->datum (syntax x)) eval_environment))
              (v (eval (syntax->datum (syntax y)) eval_environment))
              (z (datum->syntax (syntax ¶) (f u v))))
         stx_output((quasisyntax (ck s '(unsyntax z)))))))))
;;;»)
m4_popdef(«NAME»,«PROC»)
;;;»)

;;;m4_define(«general_arguments_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax stx-NAME
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((evaluate (lambda (ea)
                       (eval ea eval_environment)))
           (ck (rename 'ck))
           (s (cadr form))
           (args (cddr form)))
       (let* ((f (if (symbol? PROC) (rename PROC) PROC))
              (z (apply f (map evaluate args)))
              (retval `(,ck ,s ',z)))
         stx_output(retval))))))
;;;»,«
(define-syntax stx-NAME
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s . args)
       (let* ((evaluate (lambda (ea)
                          (eval ea eval_environment)))
              (f PROC)
              (x* (map evaluate (syntax->datum (syntax args))))
              (y (apply f x*))
              (z (datum->syntax (syntax ¶) y)))
         stx_output((quasisyntax (ck s '(unsyntax z)))))))))
;;;»)
m4_popdef(«NAME»,«PROC»)
;;;»)

;;;m4_divert

one_argument_procedure(list?)
one_argument_procedure(proper-list?)
one_argument_procedure(circular-list?)
one_argument_procedure(dotted-list?)
one_argument_procedure(pair?)
one_argument_procedure(null?)
one_argument_procedure(null-list?)
one_argument_procedure(not-pair?)

;;; m4_ifelse(scheme_standard,«r7rs»,«
one_argument_procedure(exact-integer?)
;;; »,«
(define-syntax stx-exact-integer?
  (syntax-rules ()
    ((_ s x)
     (ck s (stx-and* (stx-quote (stx-integer? x))
                     (stx-quote (stx-exact? x)))))))
;;; »)

one_argument_procedure(number?)
one_argument_procedure(exact?)
one_argument_procedure(inexact?)
one_argument_procedure(integer?)
one_argument_procedure(rational?)
one_argument_procedure(real?)
one_argument_procedure(complex?)
one_argument_procedure(finite?)
one_argument_procedure(infinite?)
one_argument_procedure(nan?)
one_argument_procedure(boolean?)
one_argument_procedure(symbol?)
one_argument_procedure(string?)
one_argument_procedure(char?)
one_argument_procedure(vector?)
one_argument_procedure(bytevector?)

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
two_argument_procedure(xcons)
general_arguments_procedure(cons*)
general_arguments_procedure(list)
general_arguments_procedure(append)

one_argument_procedure(not)

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
