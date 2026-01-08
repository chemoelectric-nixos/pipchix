;;;
;;; Copyright © 2026 Barry Schwartz
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

;;; m4_define(«simple_eager_macro»,«(define-syntax $1:e (eager-syntax $1))»)

(define-syntax identity:e
  ;; Expands to all its arguments.
  (eager-syntax (lambda args (apply values args))))

(define-syntax constant:e
  ;; Expands to c, regardless of other arguments.
  (eager-syntax (lambda (c . rest) c)))

simple_eager_macro(gensym) ;; Expands to a unique identifier.
simple_eager_macro(generate-temporaries) ;; List of gensyms.

(define-syntax error:e ;; Reports a syntax error.
  ;; (error:e message arg1 arg2 ...)
  (eager-syntax
   (case-lambda
     ((message)
      SLOW_SYNTAX_ERROR(«message»))
     m4_forloop(N,1,9,«
     ((message m4_forloop(M,1,N,« arg«»M»))
      SLOW_SYNTAX_ERROR(«message»m4_forloop(M,1,N,«,arg«»M»)))
     »)
     ((message . msg*)
      ;; If there are more than nine irritants, put them in a list.
      SLOW_SYNTAX_ERROR(«message»,«(list "More than nine:" msg*)»))
     )))


simple_eager_macro(cons)  ;; Forms pairs.
simple_eager_macro(xcons) ;; Forms pairs, with arguments exchanged.
simple_eager_macro(cons*) ;; Prepends elements to lists.
simple_eager_macro(list)  ;; Forms lists from elements.
simple_eager_macro(circular-list) ;; Forms circular lists.

(define-syntax make-list:e
  ;; Makes a list of n copies of element.
  (eager-syntax
   (lambda (n element)
     (make-list n element))))

simple_eager_macro(take)       ;; Takes the first n elements.
simple_eager_macro(drop)       ;; Drops the first n elements.
simple_eager_macro(take-right) ;; Takes the last n elements.
simple_eager_macro(drop-right) ;; Drops the last n elements.
simple_eager_macro(last)       ;; Returns the last element.
simple_eager_macro(last-pair)  ;; Returns the last pair.
simple_eager_macro(append)     ;; Appends lists.
simple_eager_macro(reverse)    ;; Reverses a list.
simple_eager_macro(length)     ;; Returns a lists length.
simple_eager_macro(every)      ;; Tests for everthing true.
simple_eager_macro(any)        ;; Tests for anything true.

simple_eager_macro(car)
simple_eager_macro(cdr)
simple_eager_macro(caar)
simple_eager_macro(cadr)
simple_eager_macro(cdar)
simple_eager_macro(cddr)
simple_eager_macro(caaaar)
simple_eager_macro(caaar)
simple_eager_macro(caaddr)
simple_eager_macro(cadaar)
simple_eager_macro(cadar)
simple_eager_macro(cadddr)
simple_eager_macro(cdaaar)
simple_eager_macro(cdaar)
simple_eager_macro(cdaddr)
simple_eager_macro(cddaar)
simple_eager_macro(cddar)
simple_eager_macro(cddddr)
simple_eager_macro(caaadr)
simple_eager_macro(caadar)
simple_eager_macro(caadr)
simple_eager_macro(cadadr)
simple_eager_macro(caddar)
simple_eager_macro(caddr)
simple_eager_macro(cdaadr)
simple_eager_macro(cdadar)
simple_eager_macro(cdadr)
simple_eager_macro(cddadr)
simple_eager_macro(cdddar)
simple_eager_macro(cdddr)

simple_eager_macro(first)
simple_eager_macro(second)
simple_eager_macro(third)
simple_eager_macro(fourth)
simple_eager_macro(fifth)
simple_eager_macro(sixth)
simple_eager_macro(seventh)
simple_eager_macro(eighth)
simple_eager_macro(ninth)
simple_eager_macro(tenth)

(define-syntax even?:e
  ;; Tests if a list is of even length. Does so by approximately the
  ;; algorithm for ‘em-even?’ from SRFI-148.
  (eager-syntax
   (lambda (lst)
     (unless (proper-list? lst)
       SCHEME_ERROR("expected a proper list", lst))
     (let loop ((lst lst))
       (cond
         ((null? lst) #t)
         ((null? (cdr lst)) #f)
         (else
          ;; This cannot be done (in any direct way) as a recursive
          ;; macro, because eager-syntax would not know when to stop
          ;; expansion.
          (loop (drop lst 2)))
         )))))

(define-syntax odd?:e
  ;; Tests if a list is of odd length.
  (eager-syntax
   (lambda (lst)
     (not (even?:e lst)))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'syntax-rules:e 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
