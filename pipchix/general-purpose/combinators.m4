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

(define (thrush-maker caller)
  ;;
  ;; Creates ‘point-free’ combinators known to Racket programmers.
  ;;
  (lambda (proc . proc*)
    (lambda vals
      (let ((p proc*))
        (define loop
          (lambda vals
            (if (pair? p)
              (let ((proc (car p)))
                (set! p (cdr p))
                (caller loop proc vals))
              (apply values vals))))
        (caller loop proc vals)))))

(define thrush
  ;;
  ;; A ‘point-free’ combinator known to Racket programmers.
  ;;
  ;; (thrush f g h) returns a procedure whose data flow is
  ;; input-values => f => g => h => output-values
  ;;
  ;; One may wish to use this along with SRFI-26 (pipchix
  ;; general-purpose cut).
  ;;
  (thrush-maker
   (lambda (loop proc vals)
     (call-with-values
         (lambda () (apply proc vals))
       loop))))

(define λ~> thrush)      ;; A synonym known to Racket programmers.
(define lambda~> thrush) ;; A synonym without Greek script.

(define (thrush+ val . proc*)
  ;;
  ;; A combinator known to Racket programmers. It is also a
  ;; generalization of the CHICKEN Scheme ‘clojurian’ combinators.
  ;;
  ;; One may wish to use this along with SRFI-26 (pipchix
  ;; general-purpose cut).
  ;;
  (if (pair? proc*)
    ((apply thrush proc*) val)
    val))

(define ~> thrush+) ;; A synonym known to Racket programmers.

(define (thrush* val . val*)
  ;;
  ;; Another Racket combinator. One can write
  ;;
  ;;   ((thrush* 1 2 3) proc1 proc2 proc3)
  ;;
  ;; instead of
  ;;
  ;;   ((thrush proc1 proc2 proc3) 1 2 3)
  ;;
  ;; or
  ;;
  (lambda proc*
    (call-with-values
        (lambda () (apply values (cons val val*)))
      (apply thrush proc*))))

(define ~>* thrush*) ;; A synonym known to Racket programmers.

(define thrush-and
  ;;
  ;; Short-circuiting thrush. Stops and returns #f as soon as any
  ;; procedure returns the single value #f.
  ;;
  (let ((short-circuit? (lambda (vals)
                          (and (pair? vals)
                               (eq? (car vals) #f)
                               (null? (cdr vals))))))
    (thrush-maker
     (lambda (loop proc vals)
       (and (not (short-circuit? vals))
            (call-with-values
                (lambda () (apply proc vals))
              loop))))))

(define λand~> thrush-and)       ;; Racket’s synonym.
(define lambda-and~> thrush-and) ;; A synonym without Greek script.

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
