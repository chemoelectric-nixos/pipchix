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

(define (thrush proc . proc*)
  ;;
  ;; A ‘point-free’ combinator known to Racket programmers.
  ;;
  ;; (thrush f g h) returns a procedure whose data flow is
  ;; input-values => f => g => h => output-values
  ;;
  ;; CHICKEN programmers may recognize similarity to the ‘clojurian’
  ;; egg.
  ;;
  (lambda vals
    (let ((p proc*))
      (define loop
        (lambda vals
          (if (pair? p)
            (let ((proc (car p)))
              (set! p (cdr p))
              (call-with-values
                  (lambda () (apply proc vals))
                loop))
            (apply values vals))))
      (call-with-values
          (lambda () (apply proc vals))
        loop))))

(define λ~> thrush)      ;; A synonym known to Racket programmers.
(define lambda~> thrush) ;; A synonym without Greek script.

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
