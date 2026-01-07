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

(define-syntax reverse:e
  (eager-syntax reverse))

(define-syntax append:e
  (eager-syntax append))

(define-syntax even?:e
  ;; Test if a list is of even length. By the algorithm for ‘em-even?’
  ;; from SRFI-148.
  (eager-syntax
   (lambda (lst)
     (unless (proper-list? lst)
       SCHEME_ERROR("expected a proper list", lst))
     (let loop ((lst lst))
       (cond
         ((null? lst) #t)
         ((null? (cdr lst)) #f)
         (else
          ;; This cannot be done as a recursive macro, because
          ;; eager-syntax would not know when to stop expansion.
          (loop (drop lst 2)))
         )))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'syntax-rules:e 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
