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

(define *failure* '#("the\xA0;failure\xA0;object"))

(define (failure-object)
  ;; Return a failure object.
  *failure*)

(define (failure-object? obj)
  ;; Test if an object is a failure object.
  (eq? obj *failure*))

(define (fail)
  ;; Raise a failure exception.
  (raise-continuable *failure*))

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
             (with-exception-handler
                 (lambda (exc)
                   (if (failure-object? exc)
                     (next)
                     (raise-continuable exc)))
               (lambda ()
                 (call-with-values
                     (lambda () command)
                   leave))))))
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
                (with-exception-handler
                    (lambda (exc)
                      (if (failure-object? exc)
                        (leave #f)
                        (raise-continuable exc)))
                  (lambda ()
                    (let-values ((v* command))
                      (set! val* v*)))))
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
            (with-exception-handler
                (lambda (exc)
                  (if (failure-object? exc)
                    (leave)
                    (raise-continuable exc)))
              (lambda ()
                (let-values ((v* command))
                  (set! val* v*)))))))
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
            (with-exception-handler
                (lambda (exc)
                  (if (failure-object? exc)
                    (next)
                    (raise-continuable exc)))
              (lambda ()
                (let-values ((v* command))
                  (set! val* v*)))))))
       (apply values val*)))))

(define-syntax general-reversible-set!
  (syntax-rules ()
    ((¶ getter! setter! ((obj value) ...) body ...)
     (let-values
         ((previous-values (values (getter! obj) ...)))
       (with-exception-handler
           (lambda (exc)
             (when (failure-object? exc)
               (let ((p previous-values))
                 (begin
                   (setter! obj (car p))
                   (set! p (cdr p)))
                 ...))
             (raise-continuable exc))
         (lambda ()
           (if #f #f)
           (setter! obj value) ...
           body ...))))))

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
