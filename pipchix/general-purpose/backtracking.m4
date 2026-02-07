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

;;; This stack IS NOT THREAD-SAFE.
(define *failure-stack* '())

(define (push-failure! failure)
  (set! *failure-stack* (cons failure *failure-stack*)))

(define (drop-failure!)
  (set! *failure-stack* (cdr *failure-stack*)))

(define (attempt-dynamic-wind thunk)
  (dynamic-wind (lambda () (if #f #f))
                thunk
                (lambda () (drop-failure!))))

(define *failure* '#("the\xA0;failure\xA0;object"))

(define (failure-object)
  ;; Return a failure object.
  *failure*)

(define (failure-object? obj)
  ;; Test if an object is a failure object.
  (eq? obj *failure*))

(define (fail)
  (if (pair? *failure-stack*)
    ((car *failure-stack*))
    *failure*))

(define-syntax attempt-not
  ;;
  ;; Reverse success and failure with each other. Returns nothing
  ;; useful.
  ;;
  (syntax-rules ()
    ((¶ form)
     (if (call/cc
              (lambda (leave)
                (attempt-dynamic-wind
                 (lambda ()
                   (call/cc
                    (lambda (failure)
                      (push-failure! failure)
                      form
                      (leave #t)))
                   (leave #f)))))
       (fail)
       (if #f #f)))))

(define-syntax attempt-or
  ;;
  ;; On success, returns the values returned by the successful form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (call/cc
      (lambda (leave)
        (begin
          (call/cc
           (lambda (next)
             (attempt-dynamic-wind
              (lambda ()
                (call/cc
                 (lambda (failure)
                   (push-failure! failure)
                   (call-with-values
                       (lambda () form1)
                     leave)
                   (next)))))))
          ...)
        (fail))))))

(define-syntax attempt-and
  ;;
  ;; On success, returns the values returned by the last form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (let-values ((val* (values)))
       (if (call/cc
            (lambda (leave)
              (begin
                (call/cc
                 (lambda (next)
                   (attempt-dynamic-wind
                    (lambda ()
                      (call/cc
                       (lambda (failure)
                         (push-failure! failure)
                         (let-values ((v* form1))
                           (set! val* v*)
                           (next))))
                      (leave #f)))))
                ...)
              (leave #t)))
         (apply values val*)
         (fail))))))

(define-syntax attempt-while
  ;;
  ;; Returns the values returned by the last successful form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (let-values ((val* (values)))
       (call/cc
        (lambda (leave)
          (begin
            (call/cc
             (lambda (next)
               (attempt-dynamic-wind
                (lambda ()
                  (call/cc
                   (lambda (failure)
                     (push-failure! failure)
                     (let-values ((v* form1))
                       (set! val* v*)
                       (next))))
                  (leave)))))
            ...)))
       (apply values val*)))))

(define-syntax attempt-every
  ;;
  ;; Returns the values returned by the last successful form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (let-values ((val* (values)))
       (begin
         (call/cc
          (lambda (next)
            (attempt-dynamic-wind
             (lambda ()
               (call/cc
                (lambda (failure)
                  (push-failure! failure)
                  (let-values ((v* form1))
                    (set! val* v*)
                    (next))))))))
         ...)
       (apply values val*)))))

(define-syntax attempt-or-ec
  ;;
  ;; On success, returns the values returned by ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (call/cc
      (lambda (leave)
        (do-ec
          qualifier1 ...
          (call/cc
           (lambda (next)
             (attempt-dynamic-wind
               (lambda ()
                 (call/cc
                  (lambda (failure)
                    (push-failure! failure)
                    (call-with-values
                        (lambda () command)
                      leave)
                    (next))))))))
        (fail))))))

(define-syntax attempt-and-ec
  ;;
  ;; On success, returns the values returned by the last call to
  ;; ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (let-values ((val* (values)))
       (if (call/cc
            (lambda (leave)
              (do-ec
                qualifier1 ...
                (call/cc
                 (lambda (next)
                   (attempt-dynamic-wind
                    (lambda ()
                      (call/cc
                       (lambda (failure)
                         (push-failure! failure)
                         (let-values ((v* command))
                           (set! val* v*)
                           (next))))
                      (leave #f))))))
              (leave #t)))
         (apply values val*)
         (fail))))))

(define-syntax attempt-while-ec
  ;;
  ;; Returns the values returned by the last successful call to
  ;; ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (let-values ((val* (values)))
       (call/cc
        (lambda (leave)
          (do-ec
            qualifier1 ...
            (call/cc
             (lambda (next)
               (attempt-dynamic-wind
                (lambda ()
                  (call/cc
                   (lambda (failure)
                     (push-failure! failure)
                     (let-values ((v* command))
                       (set! val* v*)
                       (next))))
                  (leave))))))))
       (apply values val*)))))

(define-syntax attempt-every-ec
  ;;
  ;; Returns the values returned by the last successful call to
  ;; ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (let-values ((val* (values)))
       (do-ec
         qualifier1 ...
         (call/cc
          (lambda (next)
            (attempt-dynamic-wind
             (lambda ()
               (call/cc
                (lambda (failure)
                  (push-failure! failure)
                  (let-values ((v* command))
                    (set! val* v*)
                    (next)))))))))
       (apply values val*)))))

(define-syntax attempt
  ;;
  ;; Try a predicate or fail. On success, return the result of the
  ;; predicate.
  ;;
  (syntax-rules ()
    ((¶ predicate obj ...)
     (or (predicate obj ...) (fail)))))

(define attempt-null (cut attempt null? <>))
(define attempt-pair (cut attempt pair? <>))

(define attempt-zero (cut attempt zero? <>))
(define attempt-positive (cut attempt positive? <>))
(define attempt-negative (cut attempt negative? <>))
(define attempt-odd (cut attempt odd? <>))
(define attempt-even (cut attempt even? <>))

(define-syntax general-reversible-set!
  (syntax-rules ()
    ((¶ getter! setter! () body ...)
     (begin (if #f #f) body ...))
    ((¶ getter! setter! ((obj value) ...) body ...)
     (let*-values
         ((previous-values (values (getter! obj) ...))
          ((successful? val* results)
           (call/cc
            (lambda (leave)
              (attempt-dynamic-wind
               (lambda ()
                 (call/cc
                  (lambda (failure)
                    (push-failure! failure)
                    (setter! obj value) ...
                    (let-values ((val* (begin (if #f #f) body ...)))
                      (leave #t val* (list (getter! obj) ...)))))
                 (let ((p previous-values))
                   (begin
                     (setter! obj (car p))
                     (set! p (cdr p)))
                   ...
                   (leave #f #f previous-values))))))))
       (let ((p results))
         (begin
           (setter! obj (car p))
           (set! p (cdr p)))
         ...)
       (if successful?
         (apply values val*)
         (fail))))))

(define-syntax reversible-set!
  (syntax-rules ()
    ((¶ ((obj value) ...) body ...)
     (let-syntax ((ref (syntax-rules () ((µ t) t))))
       (general-reversible-set! ref set!
                                ((obj value) ...)
                                body ...)))))

(define-syntax reversible-vector-set!
  (syntax-rules ()
    ((¶ (((obj i) value) ...) body ...)
     (let-syntax
         ((getter (syntax-rules ()
                    ((µ (t j)) (vector-ref t j))))
          (setter! (syntax-rules ()
                     ((µ (t j) v) (vector-set! t j v)))))
       (general-reversible-set! getter setter!
                                (((obj i) value) ...)
                                body ...)))))

(define-syntax reversible-bytevector-u8-set!
  (syntax-rules ()
    ((¶ (((obj i) value) ...) body ...)
     (let-syntax
         ((getter (syntax-rules ()
                    ((µ (t j)) (bytevector-u8-ref t j))))
          (setter! (syntax-rules ()
                     ((µ (t j) v) (bytevector-u8-set! t j v)))))
       (general-reversible-set! getter setter!
                                (((obj i) value) ...)
                                body ...)))))

(define-syntax reversible-list-set!
  (syntax-rules ()
    ((¶ (((obj i) value) ...) body ...)
     (let-syntax
         ((getter (syntax-rules ()
                    ((µ (t j)) (list-ref t j))))
          (setter! (syntax-rules ()
                     ((µ (t j) v) (list-set! t j v)))))
       (general-reversible-set! getter setter!
                                (((obj i) value) ...)
                                body ...)))))

;;;-------------------------------------------------------------------
m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
