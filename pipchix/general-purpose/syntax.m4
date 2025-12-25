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
;;;-------------------------------------------------------------------
;;;
;;; stx-bound-identifier=? and stx-free-identifier=? implementations
;;; are based on work by Marc Nieper-Wißkirchen for SRFI-148.
;;;
;;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;;-------------------------------------------------------------------
;;;
;;; Faster ck-macros, that can handle diverse Scheme types.
;;;
;;; These are implemented with er-macro-transformer or syntax-case,
;;; instead of in syntax-rules (with some exceptions, such as the ck
;;; abstract machine itself). The ck-macros themselves are for use
;;; with syntax-rules, and can be thought of as primitives.
;;;
;;; However, one should keep in mind that evaluation usually is done
;;; with ‘eval’ rather than by the syntax-rules mechanism. The
;;; environment passed to ‘eval’ depends on the Scheme implementation.
;;; An exception is for ‘user-defined’ ck-macro primitives. For these,
;;; a custom environment can be specified. The same environment will
;;; be used to evaluate both the procedure that defines the primitive
;;; and the arguments when they are passed to it.
;;;
;;;-------------------------------------------------------------------

(define-syntax c-cons
  (syntax-rules ()
    ((¶ s x y)
     (stx-cons s x y))))

(define-syntax c-append
  (syntax-rules ()
    ((¶ s . args)
     (stx-append s . args))))

(define-syntax c-list->vector
  (syntax-rules ()
    ((¶ s x y)
     (stx-list->vector s x y))))

m4_define(«c_cons_provided»,«yes»)m4_dnl
m4_define(«c_append_provided»,«yes»)m4_dnl
m4_define(«c_list_to_vector_provided»,«yes»)m4_dnl
define_ck_macros

(define-syntax stx-ck
  (syntax-rules ()
    ((¶ s x)
     (ck s x))))

(define-syntax stx
  (syntax-rules ()
    ((¶ x)
     (stx-ck () x))))

(define-syntax stx-quasiquote
  (syntax-rules ()
    ((¶ s x)
     (c-quasiquote s x))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;
;;; Some simple ck-macros based on those commented out in original
;;; source code included above.
;;;

(define-syntax stx-quote
  ;; Add an extra level of quote to an argument.
  (syntax-rules ( quote )
    ((¶ s x)
     (stx-ck s 'x))))

(define-syntax stx-dequote
  (syntax-rules ( quote )
    ((¶ s 'x)
     (stx-ck s x))))

(define-syntax stx-identity
  (syntax-rules ( quote )
    ((¶ s 'v)
     (stx-ck s 'v))))

(define-syntax stx-constantly
  ;; Return the first argument constantly.
  (syntax-rules ( quote )
    ((¶ s X Y ...)
     (stx-ck s X))))

(define-syntax stx-true
  (syntax-rules ( quote )
    ((¶ s X ...)
     (stx-ck s '#t))))

(define-syntax stx-false
  (syntax-rules ( quote )
    ((¶ s X ...)
     (stx-ck s '#f))))

(define-syntax stx-if
  ;; An ‘if’ that evaluates both branches.
  (syntax-rules ( quote )
    ((¶ s '#f 'pass 'fail)              ; If #f, expand to fail.
     (stx-ck s 'fail))
    ((¶ s otherwise 'pass 'fail)        ; Else, expand to pass.
     (stx-ck s 'pass))))

(define-syntax stx-if*
  ;; An ‘if’ that evaluates only the branch taken. The branches must
  ;; have an extra level of quoting.
  (syntax-rules ( quote )
    ((¶ s '#f 'pass 'fail)
     (stx-ck s fail))
    ((¶ s otherwise 'pass 'fail)
     (stx-ck s pass))))

(define-syntax stx-or
  ;; ‘or’ that evaluates eagerly.
  (syntax-rules ( quote )
    ((¶ s)
     (stx-ck s '#f))
    ((¶ s 'h)
     (stx-ck s 'h))
    ;; TODO: Can this be optimized to avoid expanding 'h twice?
    ((¶ s 'h . t)
     (stx-ck s (stx-if 'h 'h (stx-or . t))))))

(define-syntax stx-or*
  ;; Short-circuiting ‘or’. The arguments must have an extra level of
  ;; quoting.
  (syntax-rules ( quote )
    ((¶ s)
     (stx-ck s '#f))
    ((¶ s 'h)
     (stx-ck s h))
    ;; TODO: Can this be optimized to avoid expanding 'h twice?
    ((¶ s 'h . t)
     (stx-ck s (stx-if* h 'h '(stx-or* . t))))))

(define-syntax stx-and
  ;; ‘and’ that evaluates eagerly.
  (syntax-rules ( quote )
    ((¶ s)
     (stx-ck s '#t))
    ((¶ s 'h)
     (stx-ck s 'h))
    ((¶ s 'h . t)
     (stx-ck s (stx-if 'h (stx-and . t) '#f)))))

(define-syntax stx-and*
  ;; Short-circuiting ‘and’. The arguments must have an extra level of
  ;; quoting.
  (syntax-rules ( quote )
    ((¶ s)
     (stx-ck s '#t))
    ((¶ s 'h)
     (stx-ck s h))
    ((¶ s 'h . t)
     (stx-ck s (stx-if* h '(stx-and* . t) ''#f)))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;;m4_divert(-1)

m4_define(«stx_output»,
          «(if (or (boolean? $1) (number? $1)) `,$1 $1)»)

;;;m4_ifelse(scheme_standard,«r6rs»,«
;;;m4_define(«eval_environment»,(environment '(rnrs)))
;;;»,scheme_standard,«r5rs»,«
;;;m4_define(«eval_environment»,(interaction-environment))
;;;»,«
;;;m4_define(«eval_environment»,(interaction-environment))
;;;»)

;;;m4_define(«one_argument_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«MACR»,m4_ifelse($2,«»,«stx-$1»,«$1»))
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax MACR
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((env eval_environment)
           (Evaluate (lambda (ϑ) (eval ϑ env)))
           (ck (rename 'ck))
           (s (cadr form))
           (args (cddr form))
           (f (if (symbol? PROC) (rename PROC) PROC))
           (x (Evaluate (car args)))
           (y (f x))
           (retval `(,ck ,s ',y)))
       stx_output(retval)))))
;;;»,«
(define-syntax MACR
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s x)
       (let* ((env eval_environment)
              (Evaluate (lambda (ϑ) (eval ϑ env)))
              (f PROC)
              (t (Evaluate (syntax->datum (syntax x))))
              (y (datum->syntax (syntax ¶) (f t))))
         stx_output((quasisyntax (ck s '(unsyntax y)))))))))
;;;»)
m4_popdef(«NAME»,«MACR»,«PROC»)
;;;»)

;;;m4_define(«two_argument_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«MACR»,m4_ifelse($2,«»,«stx-$1»,«$1»))
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax MACR
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((env eval_environment)
            (Evaluate (lambda (ϑ) (eval ϑ env)))
            (ck (rename 'ck))
            (s (cadr form))
            (args (cddr form))
            (f (if (symbol? PROC) (rename PROC) PROC))
            (x (Evaluate (car args)))
            (y (Evaluate (cadr args)))
            (z (f x y))
            (retval `(,ck ,s ',z)))
       stx_output(retval)))))
;;;»,«
(define-syntax MACR
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s x y)
       (let* ((env eval_environment)
              (Evaluate (lambda (ϑ) (eval ϑ env)))
              (f PROC)
              (u (Evaluate (syntax->datum (syntax x))))
              (v (Evaluate (syntax->datum (syntax y))))
              (z (datum->syntax (syntax ¶) (f u v))))
         stx_output((quasisyntax (ck s '(unsyntax z)))))))))
;;;»)
m4_popdef(«NAME»,«MACR»,«PROC»)
;;;»)

;;;m4_define(«general_arguments_procedure»,«
m4_pushdef(«NAME»,«$1»)
m4_pushdef(«MACR»,m4_ifelse($2,«»,«stx-$1»,«$1»))
m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
(define-syntax MACR
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((env eval_environment)
            (Evaluate (lambda (ϑ) (eval ϑ env)))
            (ck (rename 'ck))
            (s (cadr form))
            (args (cddr form))
            (f (if (symbol? PROC) (rename PROC) PROC))
            (x* (map Evaluate args))
            (z (apply f x*))
            (retval `(,ck ,s ',z)))
       stx_output(retval)))))
;;;»,«
(define-syntax MACR
  (lambda (stx)
    (syntax-case stx ()
      ((¶ s . args)
       (let* ((env eval_environment)
              (Evaluate (lambda (ϑ) (eval ϑ env)))
              (f PROC)
              (x* (map Evaluate (syntax->datum (syntax args))))
              (y (apply f x*))
              (z (datum->syntax (syntax ¶) y)))
         stx_output((quasisyntax (ck s '(unsyntax z)))))))))
;;;»)
m4_popdef(«NAME»,«MACR»,«PROC»)
;;;»)

;;;m4_divert

;;; m4_ifelse(scheme_standard,«r7rs»,«
one_argument_procedure(exact-integer?)
;;; »,«
(define-syntax stx-exact-integer?
  (syntax-rules ()
    ((¶ s x)
     (ck s (stx-and* (stx-quote (stx-integer? x))
                     (stx-quote (stx-exact? x)))))))
;;; »)

;;;-------------------------------------------------------------------

(define-syntax stx-eval
  (syntax-rules ()
    ((¶ s x)
     (stx-ck s '(eval x eval_environment)))
    ((¶ s x env)
     (stx-ck s '(eval x env)))))

;;;-------------------------------------------------------------------

two_argument_procedure(equal?)
two_argument_procedure(eqv?)
two_argument_procedure(eq?)

one_argument_procedure(symbol->string)
one_argument_procedure(string->symbol)

general_arguments_procedure(number->string)
general_arguments_procedure(string->number)

;;;-------------------------------------------------------------------

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

;;;-------------------------------------------------------------------

one_argument_procedure(list?)
one_argument_procedure(proper-list?)
one_argument_procedure(circular-list?)
one_argument_procedure(dotted-list?)
one_argument_procedure(pair?)
one_argument_procedure(null?)
one_argument_procedure(null-list?)
one_argument_procedure(not-pair?)

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

two_argument_procedure(list-ref)
one_argument_procedure(length)
one_argument_procedure(length+)

one_argument_procedure(last)
one_argument_procedure(last-pair)
one_argument_procedure(reverse)
two_argument_procedure(take)
two_argument_procedure(drop)
two_argument_procedure(take-right)
two_argument_procedure(drop-right)
general_arguments_procedure(fold)
general_arguments_procedure(fold-right)
general_arguments_procedure(pair-fold)
general_arguments_procedure(pair-fold-right)
general_arguments_procedure(reduce)
general_arguments_procedure(reduce-right)
general_arguments_procedure(unfold)
general_arguments_procedure(unfold-right)
general_arguments_procedure(map)
general_arguments_procedure(map-in-order)
general_arguments_procedure(append-map)
general_arguments_procedure(filter-map)
two_argument_procedure(filter)
two_argument_procedure(remove)
general_arguments_procedure(delete)
general_arguments_procedure(delete-duplicates)

general_arguments_procedure(append)
two_argument_procedure(append-reverse)
one_argument_procedure(concatenate)
general_arguments_procedure(zip)
two_argument_procedure(cons)
two_argument_procedure(xcons)
general_arguments_procedure(cons*)
general_arguments_procedure(list)
one_argument_procedure(list-copy)
two_argument_procedure(list-tabulate)
general_arguments_procedure(iota)
general_arguments_procedure(circular-list)

general_arguments_procedure(list=)
general_arguments_procedure(count)
two_argument_procedure(find)
two_argument_procedure(find-tail)
two_argument_procedure(take-while)
two_argument_procedure(drop-while)
general_arguments_procedure(any)
general_arguments_procedure(every)
general_arguments_procedure(list-index)
general_arguments_procedure(member)
two_argument_procedure(memq)
two_argument_procedure(memv)

general_arguments_procedure(assoc)
two_argument_procedure(assq)
two_argument_procedure(assv)
general_arguments_procedure(alist-cons)
general_arguments_procedure(alist-delete)

one_argument_procedure(list->vector)
general_arguments_procedure(vector->list)

;;;-------------------------------------------------------------------

one_argument_procedure(not)

general_arguments_procedure(+)
general_arguments_procedure(-)
general_arguments_procedure(*)
general_arguments_procedure(/)
one_argument_procedure(abs)
two_argument_procedure(quotient)
two_argument_procedure(remainder)
two_argument_procedure(ceiling-quotient)
two_argument_procedure(ceiling-remainder)
two_argument_procedure(floor-quotient)
two_argument_procedure(floor-remainder)
two_argument_procedure(truncate-quotient)
two_argument_procedure(truncate-remainder)
two_argument_procedure(round-quotient)
two_argument_procedure(round-remainder)
two_argument_procedure(euclidean-quotient)
two_argument_procedure(euclidean-remainder)
two_argument_procedure(balanced-quotient)
two_argument_procedure(balanced-remainder)

general_arguments_procedure(=)
general_arguments_procedure(<)
general_arguments_procedure(>)
general_arguments_procedure(<=)
general_arguments_procedure(>=)

one_argument_procedure(zero?)
one_argument_procedure(positive?)
one_argument_procedure(negative?)
one_argument_procedure(odd?)
one_argument_procedure(even?)

;;;-------------------------------------------------------------------
;;;
;;; User-defined fast ck-macros.
;;;

(define-syntax define-stx-primitive
  (syntax-rules ()
    ((¶ NAME PROC)
     (define-stx-primitive NAME PROC eval_environment))
    ((¶ NAME PROC ENV)
     (begin
;;;m4_ifelse(general_macros,«er-macro-transformer»,«
       (define-syntax NAME
         (er-macro-transformer
          (lambda (form rename compare)
            (let* ((env ENV)
                   (Evaluate (lambda (ϑ) (eval ϑ env)))
                   (stx-ck (rename 'stx-ck))
                   (s (cadr form))
                   (args (cddr form))
                   (f (Evaluate PROC))
                   (x* (map Evaluate args))
                   (z (apply f x*))
                   (retval `(,stx-ck ,s ',z)))
              stx_output(retval)))))
;;;»,«
       (define-syntax NAME
         (lambda (stx)
           (syntax-case stx ()
             ((¶ s . args)
              (let* ((env ENV)
                     (Evaluate (lambda (ϑ) (eval ϑ env)))
                     (f (Evaluate PROC))
                     (x* (map Evaluate
                              (syntax->datum (syntax args))))
                     (y (apply f x*))
                     (z (datum->syntax (syntax ¶) y)))
                stx_output((quasisyntax
                            (stx-ck s '(unsyntax z)))))))))
;;;»)
       ))))

;;;-------------------------------------------------------------------

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
