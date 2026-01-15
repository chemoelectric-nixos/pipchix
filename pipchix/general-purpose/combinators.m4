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

;;;-------------------------------------------------------------------

(define (thrush-maker custom)
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
                (custom loop proc vals))
              (apply values vals))))
        (custom loop proc vals)))))

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

(define (thrush+-and val . proc*)
  ;;
  ;; Short-circuiting thrush+
  ;;
  (if (pair? proc*)
    ((apply thrush-and proc*) val)
    val))

(define and~> thrush+-and) ;; A synonym known to Racket programmers.

(define (thrush*-and val . val*)
  ;;
  ;; Short-circuiting thrush*
  ;;
  (lambda proc*
    (call-with-values
        (lambda () (apply values (cons val val*)))
      (apply thrush-and proc*))))

(define and~>* thrush*-and) ;; A synonym known to Racket programmers.

;;;-------------------------------------------------------------------

(define (join proc . proc*)
  ;;
  ;; Join one-to-one procedures in parallel to make an n-to-n
  ;; procedure. This combinator may be familiar to Racket programmers.
  ;;
  (lambda (x . x*)
    (apply values
           (cons (proc x)
                 (let recurs ((proc* proc*)
                              (x* x*))
                   (if (pair? proc*)
                     (cons ((car proc*) (car x*))
                           (recurs (cdr proc*) (cdr x*)))
                     '()))))))

(define (wind-pre f g . g*)
  ;;
  ;; This combinator may be familiar to Racket programmers. Values
  ;; first go through a ‘join’ of the g and then are passed to f.
  ;;
  (let ((proc (apply join (cons g g*))))
    (wind-inner-outer proc f)))

(define (wind-post f g . g*)
  ;;
  ;; This combinator may be familiar to Racket programmers. The output
  ;; values of f are passed through the ‘join’ of the g.
  ;;
  (let ((proc (apply join (cons g g*))))
    (wind-inner-outer f proc)))

(define (wind f g . g*)
  ;;
  ;; This combinator may be familiar to Racket programmers. It
  ;; combines ‘wind-pre’ and ‘wind-post’. Values go through the ‘join’
  ;; of the g, then through f, then they go through the ‘join’ of the
  ;; h.
  ;;
  (let ((pre (apply wind-pre (cons* f g g*))))
    (lambda (h . h*)
      (apply wind-post (cons* pre h h*)))))

(define (join* proc)
  ;;
  ;; Apply a procedure to multiple values. This combinator may be
  ;; familiar to Racket programmers.
  ;;
  (lambda (x . x*)
    (apply values
           (cons (proc x)
                 (let recurs ((x* x*))
                   (if (pair? x*)
                     (cons (proc (car x*))
                           (recurs (cdr x*)))
                     '()))))))

(define (wind-pre* f g)
  ;;
  ;; wind-pre for join*
  ;;
  (let ((proc (join* g)))
    (wind-inner-outer proc f)))

(define (wind-post* f g)
  ;;
  ;; wind-post for join*
  ;;
  (let ((proc (join* g)))
    (wind-inner-outer f proc)))

(define (wind* f g h)
  ;;
  ;; wind for join*
  ;;
  (let ((pre (wind-pre* f g)))
    (wind-post* pre h)))

(define (wind-inner-outer inner outer)
  (lambda (x . x*)
    (call-with-values
        (lambda () (apply inner (cons x x*)))
      outer)))

;;;-------------------------------------------------------------------

(define (cps proc)
  ;;
  ;; Return a continuation-passing style variant of a procedure:
  ;;
  ;;     ((cps proc) k . arg*) --> (k (proc . arg*))
  ;;
  (lambda (k . arg*)
    (k (apply proc arg*))))

(define uncps
  ;;
  ;; Return an ordinary variant of a continuation-passing style
  ;; procedure:
  ;;
  ;;     ((uncps proc) . arg*) --> (proc identity . arg*)
  ;;
  (let ((identity (lambda x* (apply values x*))))
    (lambda (proc)
      (lambda arg*
        (apply proc (cons identity arg*))))))

(define (λcps~> proc . proc*)
  ;;
  ;; (λcps~> f g h ...) links a chain of continuation-passing style
  ;; procedures.
  ;;
  (let ((f* (map uncps (cons proc proc*))))
    (cps (apply thrush f*))))

(define lambda-cps~> λcps~>) ;; a synonym.

;;; m4_dnl  Using m4 here instead of Scheme’s own macro mechanism is
;;; m4_dnl  to avoid difficulties of Racket (that are presented by
;;; m4_dnl  none of the other Schemes we are using).
;;;
;;; m4_define(«cps_syntax_rules»,«
(syntax-rules ()
  ((µ k . t*)
   (k ($1 . t*))))»)
;;; m4_define(«uncps_syntax_rules»,«
(syntax-rules ()
  ((µ . t*)
   (let-syntax ((identity (syntax-rules () ((ι τ) τ))))
     ($1 identity . t*))))»)

(define-syntax define-cps-syntax
  ;;
  ;; Define a continuation-passing style macro. Start from an ordinary
  ;; macro or procedure f:
  ;;
  ;;     (define-cps-syntax cps-macro f) ==>
  ;;
  ;;           (cps-macro k . arg*) --> (k (f . arg*))
  ;;
  (syntax-rules ()
    ((¶ name f)
     (define-syntax name
       cps_syntax_rules(f)))))

(define-syntax define-uncps-syntax
  ;;
  ;; Define an ordinary macro from a continuation-passing style macro
  ;; or procedure f.
  ;;
  ;;     (define-uncps-syntax uncps-macro f) ==>
  ;;
  ;;           (uncps-macro . arg*) --> (f identity . arg*))
  ;;
  (syntax-rules ()
    ((¶ name f)
     (define-syntax name
       uncps_syntax_rules(f)))))

(define-syntax cps-syntax
  ;;
  ;; Create a continuation-passing style macro. Start from an ordinary
  ;; macro or procedure f:
  ;;
  ;;     (let-syntax ((cps-macro (cps-syntax f)))
  ;;       (cps-macro k . arg*))      -->     (k (f . arg*))
  ;;
  (syntax-rules ()
    ((¶ f)
     cps_syntax_rules(f))))

(define-syntax uncps-syntax
  ;;
  ;; Create an ordinary macro from a continuation-passing style macro.
  ;;
  ;;     (let-syntax ((uncps-syntax-fact (uncps-syntax cps-macro)))
  ;;       (uncps-macro . args*))
  ;;
  (syntax-rules ()
    ((¶ f)
     uncps_syntax_rules(f))))

;;;-------------------------------------------------------------------

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
