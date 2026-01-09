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

simple_eager_macro(equal?)
simple_eager_macro(eqv?)
simple_eager_macro(eq?)
simple_eager_macro(number?)
simple_eager_macro(exact?)
simple_eager_macro(inexact?)
simple_eager_macro(integer?)
;; m4_ifelse(RNRS_NUMBER,«6»,«
(define-syntax exact-integer?:e
  (eager-syntax
   (lambda (x) (and (integer? x) (exact? x)))))
;; »)
;; m4_ifelse(RNRS_NUMBER,«7»,«
simple_eager_macro(exact-integer?)
;; »)
simple_eager_macro(rational?)
simple_eager_macro(real?)
simple_eager_macro(complex?)
simple_eager_macro(finite?)
simple_eager_macro(infinite?)
simple_eager_macro(nan?)
simple_eager_macro(boolean?)
simple_eager_macro(symbol?)
simple_eager_macro(string?)
simple_eager_macro(char?)
simple_eager_macro(vector?)
simple_eager_macro(bytevector?)
simple_eager_macro(even?) ;; Is a number even?
simple_eager_macro(odd?)  ;; Is a number odd?
simple_eager_macro(=)
simple_eager_macro(<)
simple_eager_macro(<=)
simple_eager_macro(>)
simple_eager_macro(>=)
simple_eager_macro(string=?)
simple_eager_macro(string<?)
simple_eager_macro(string<=?)
simple_eager_macro(string>?)
simple_eager_macro(string>=?)
simple_eager_macro(string-ci=?)
simple_eager_macro(string-ci<?)
simple_eager_macro(string-ci<=?)
simple_eager_macro(string-ci>?)
simple_eager_macro(string-ci>=?)
simple_eager_macro(char=?)
simple_eager_macro(char<?)
simple_eager_macro(char<=?)
simple_eager_macro(char>?)
simple_eager_macro(char>=?)
simple_eager_macro(char-ci=?)
simple_eager_macro(char-ci<?)
simple_eager_macro(char-ci<=?)
simple_eager_macro(char-ci>?)
simple_eager_macro(char-ci>=?)
simple_eager_macro(char-alphabetic?)
simple_eager_macro(char-numeric?)
simple_eager_macro(char-whitespace?)
simple_eager_macro(char-upper-case?)
simple_eager_macro(char-lower-case?)
simple_eager_macro(not)

;; SRFI-1 predicates.
simple_eager_macro(proper-list?)
simple_eager_macro(circular-list?)
simple_eager_macro(dotted-list?)
simple_eager_macro(pair?)
simple_eager_macro(null?)
simple_eager_macro(not-pair?)
simple_eager_macro(null-list?)
simple_eager_macro(list=)

simple_eager_macro(cons)  ;; Forms pairs.
simple_eager_macro(xcons) ;; Forms pairs, with arguments exchanged.
simple_eager_macro(cons*) ;; Prepends elements to lists.
simple_eager_macro(list)  ;; Forms lists from elements.
simple_eager_macro(circular-list) ;; Forms circular lists.
simple_eager_macro(list-tabulate) ;; Make a list of given length.
simple_eager_macro(iota)          ;; Make a list by counting.

(define-syntax make-list:e
  ;; Makes a list of n copies of element.
  (eager-syntax
   (lambda (n element)
     (make-list n element))))

simple_eager_macro(take)           ;; Takes the first n elements.
simple_eager_macro(drop)           ;; Drops the first n elements.
simple_eager_macro(take-right)     ;; Takes the last n elements.
simple_eager_macro(drop-right)     ;; Drops the last n elements.
simple_eager_macro(last)           ;; Returns the last element.
simple_eager_macro(last-pair)      ;; Returns the last pair.
simple_eager_macro(list-ref)       ;; Returns the i’th element.
simple_eager_macro(append)         ;; Appends lists.
simple_eager_macro(concatenate)    ;; Concatenates a list of lists.
simple_eager_macro(reverse)        ;; Reverses a list.
simple_eager_macro(append-reverse) ;; Reverses, then appends a tail.
simple_eager_macro(zip)            ;; Join lists element by element.
simple_eager_macro(map)            ;; Maps elements from list to list.
simple_eager_macro(append-map)     ;; Maps lists and appends results.
simple_eager_macro(fold)
simple_eager_macro(fold-right)
simple_eager_macro(pair-fold)
simple_eager_macro(pair-fold-right)
simple_eager_macro(unfold)
simple_eager_macro(unfold-right)
simple_eager_macro(length)         ;; Returns a list’s length.
simple_eager_macro(length+)        ;; Returns a list’s length or #f.
simple_eager_macro(count)          ;; Counts elements passing a test.
simple_eager_macro(every)          ;; Tests for everything true.
simple_eager_macro(any)            ;; Tests for anything true.

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

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
