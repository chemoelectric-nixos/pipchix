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

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

(define-syntax define-e-macro
  (syntax-rules ()
    ((¶ E-MACRO-NAME E-MACRO-PROCEDURE)
     (define-syntax E-MACRO-NAME
       (er-macro-transformer
        (lambda (form rename compare)
          (let ((arguments (cdr form)))
            (let* ((evaluate
                    (lambda (obj)
                      (e-macros-eval obj (apply e-macros-environment
                                                (e-macros-libraries)))))
                   (f-of-x*
                    (let loop ((arg* arguments)
                               (x* '()))
                      (if (pair? arg*)
                        (let-values (((arg arg*) (car+cdr arg*)))
                          (let* ((quot (rename 'quote))
                                 (quasiquot (rename 'quasiquote))
                                 (x (cond
                                      ((not-pair? arg)
                                       arg)
                                      ((not (compare (car arg) quot))
                                       arg)
                                      ((not-pair? (car arg))
                                       ;; Expand the argument.
                                       (evaluate arg))
                                      ((and (not (compare (caar arg) quot))
                                            (not (compare (caar arg) quasiquot)))
                                       ;; Expand the argument.
                                       (evaluate arg))
                                      (else
                                       ;; Drop the quoting by a level.
                                       (car arg)))))
                            (loop arg* (cons x x*))))
                        (cons `(e-macros-evaluate 'E-MACRO-PROCEDURE)
                              (reverse! x*))))))
              `,f-of-x*))))))))

;;; »,«

(define-syntax define-e-macro
  (syntax-rules ()
    ((¶ E-MACRO-NAME E-MACRO-PROCEDURE)
     (define-syntax E-MACRO-NAME
       (lambda (stx)
         (define (evaluate obj)
           (e-macros-eval obj (apply e-macros-environment
                                     (e-macros-libraries))))
         (define (syntax->dotted-list x)
           (syntax-case x ()
             (()       '())
             ((a . a*) (cons (syntax a)
                             (syntax->dotted-list (syntax a*))))
             (a        (syntax a))))
         (define (syntax-pair? x)
           (syntax-case x ()
             ((a . a*) #t)
             (a        #f)))
         (define (syntax-car x)
           (syntax-case x ()
             ((a . a*) (syntax a))))
         (define (quote=? u)
           (and (identifier? u)
                (free-identifier=? u (syntax quote))))
         (define (quote-or-quasiquote=? u)
           (and (identifier? u)
                (or (free-identifier=? u (syntax quote))
                    (free-identifier=? u (syntax quasiquote)))))
         (let* ((form (syntax->dotted-list stx))
                (µ (car form))
                (arguments (cdr form))
                (f-of-x*
                 (let loop ((arg* arguments)
                            (x* '()))
                   (if (pair? arg*)
                     (let-values (((arg arg*) (car+cdr arg*)))
                       (let* ((x (cond
                                   ((not (syntax-pair? arg))
                                    (syntax->datum arg))
                                   ((not (quote=? (syntax-car arg)))
                                    (syntax->datum arg))
                                   ((not (syntax-pair?
                                          (syntax-car arg)))
                                    ;; Expand the argument.
                                    (evaluate (syntax->datum arg)))
                                   ((not (quote-or-quasiquote=?
                                          (syntax-car
                                           (syntax-car arg))))
                                    ;; Expand the argument.
                                    (evaluate (syntax->datum arg)))
                                   (else
                                    ;; Drop the quoting by a level.
                                    (syntax->datum
                                     (syntax-car arg))))))
                         (loop arg* (cons x x*))))
                     (cons `(e-macros-evaluate 'E-MACRO-PROCEDURE)
                           (reverse! x*))))))
           (datum->syntax µ f-of-x*)))))))

;;; »)

;;; m4_define(«define_simple_e_macro»,«(define-e-macro e-$1 $1)»)

(define-e-macro e-false (lambda anything #f))
(define-e-macro e-true (lambda anything #t))

define_simple_e_macro(equal?)
define_simple_e_macro(eqv?)
define_simple_e_macro(eq?)

define_simple_e_macro(number?)
define_simple_e_macro(exact?)
define_simple_e_macro(inexact?)
define_simple_e_macro(integer?)
define_simple_e_macro(rational?)
define_simple_e_macro(real?)
define_simple_e_macro(complex?)
define_simple_e_macro(finite?)
define_simple_e_macro(infinite?)
define_simple_e_macro(nan?)
define_simple_e_macro(boolean?)
define_simple_e_macro(symbol?)
define_simple_e_macro(string?)
define_simple_e_macro(char?)
define_simple_e_macro(vector?)
define_simple_e_macro(bytevector?)

;;; m4_ifelse(scheme_standard,«r7rs»,«
define_simple_e_macro(exact-integer?)
;;; »,«
(define-e-macro e-exact-integer?
  (lambda (obj)
    (and (integer? obj) (exact? obj))))
;;; »)

define_simple_e_macro(list)
define_simple_e_macro(make-list)
define_simple_e_macro(circular-list)
define_simple_e_macro(cons)
define_simple_e_macro(xcons)
define_simple_e_macro(cons*)
define_simple_e_macro(iota)
define_simple_e_macro(list-tabulate)

define_simple_e_macro(pair?)
define_simple_e_macro(null?)
define_simple_e_macro(proper-list?)
define_simple_e_macro(circular-list?)
define_simple_e_macro(dotted-list?)
define_simple_e_macro(not-pair?)
define_simple_e_macro(null-list?)
define_simple_e_macro(list=)

define_simple_e_macro(take)
define_simple_e_macro(drop)
define_simple_e_macro(take-right)
define_simple_e_macro(drop-right)

define_simple_e_macro(car)
define_simple_e_macro(cdr)
define_simple_e_macro(caar)
define_simple_e_macro(cadr)
define_simple_e_macro(cdar)
define_simple_e_macro(cddr)
define_simple_e_macro(caaaar)
define_simple_e_macro(caaar)
define_simple_e_macro(caaddr)
define_simple_e_macro(cadaar)
define_simple_e_macro(cadar)
define_simple_e_macro(cadddr)
define_simple_e_macro(cdaaar)
define_simple_e_macro(cdaar)
define_simple_e_macro(cdaddr)
define_simple_e_macro(cddaar)
define_simple_e_macro(cddar)
define_simple_e_macro(cddddr)
define_simple_e_macro(caaadr)
define_simple_e_macro(caadar)
define_simple_e_macro(caadr)
define_simple_e_macro(cadadr)
define_simple_e_macro(caddar)
define_simple_e_macro(caddr)
define_simple_e_macro(cdaadr)
define_simple_e_macro(cdadar)
define_simple_e_macro(cdadr)
define_simple_e_macro(cddadr)
define_simple_e_macro(cdddar)
define_simple_e_macro(cdddr)

define_simple_e_macro(first)
define_simple_e_macro(second)
define_simple_e_macro(third)
define_simple_e_macro(fourth)
define_simple_e_macro(fifth)
define_simple_e_macro(sixth)
define_simple_e_macro(seventh)
define_simple_e_macro(eighth)
define_simple_e_macro(ninth)
define_simple_e_macro(tenth)

define_simple_e_macro(last)
define_simple_e_macro(last-pair)

define_simple_e_macro(list-ref)

define_simple_e_macro(length)
define_simple_e_macro(length+)
define_simple_e_macro(reverse)
define_simple_e_macro(append)
define_simple_e_macro(concatenate)
define_simple_e_macro(append-reverse)
define_simple_e_macro(zip)
define_simple_e_macro(count)

define_simple_e_macro(map)
define_simple_e_macro(map-in-order)
define_simple_e_macro(append-map)
define_simple_e_macro(filter-map)
define_simple_e_macro(fold)
define_simple_e_macro(unfold)
define_simple_e_macro(pair-fold)
define_simple_e_macro(fold-right)
define_simple_e_macro(unfold-right)
define_simple_e_macro(pair-fold-right)

define_simple_e_macro(filter)
define_simple_e_macro(remove)
define_simple_e_macro(delete)
define_simple_e_macro(delete-duplicates)

define_simple_e_macro(member)
define_simple_e_macro(memq)
define_simple_e_macro(memv)
define_simple_e_macro(find)
define_simple_e_macro(find-tail)
define_simple_e_macro(any)
define_simple_e_macro(every)
define_simple_e_macro(list-index)
define_simple_e_macro(take-while)
define_simple_e_macro(drop-while)

define_simple_e_macro(assoc)
define_simple_e_macro(assq)
define_simple_e_macro(assv)
define_simple_e_macro(alist-cons)
define_simple_e_macro(alist-delete)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
