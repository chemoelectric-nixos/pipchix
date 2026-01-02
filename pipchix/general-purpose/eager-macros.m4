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

(cond-expand
  (chibi

   (define (random-partial-identifier--linux)
     (with-input-from-file "/proc/sys/kernel/random/uuid"
       (lambda () (read-string 36))))

   (define (random-partial-identifier--general)
     ;; A UUID that was generated once upon a time.
     "c1f4e8ef-a7a5-4fc5-9917-3bbb355047cc")

   (define (random-partial-identifier)
     ;;
     ;; Choose an identifier generator and install it as a new
     ;; ‘random-partial-identifier’ procedure.
     ;;
     (continuation-capture
      (lambda (cc)
        (with-exception-handler
            (lambda (c)
              (set! random-partial-identifier
                random-partial-identifier--general)
              (continuation-return cc
                (random-partial-identifier--general)))
          (lambda ()
            (set! random-partial-identifier
              random-partial-identifier--linux)
            (continuation-return cc
              (random-partial-identifier--linux)))))))

   (define gensym
     (let ((i -1))
       (case-lambda
         (() (gensym ""))
         ((ignored)
          (set! i (+ i 1))
          (string->symbol
           (string-append
            "g-" (random-partial-identifier) "-"
            (number->string i))))))) )

  (else)) ;; cond-expand

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

(define-syntax syntax-rules:e
  (syntax-rules ()

    ((syntax-rules:e (reserved-for-expansion ...)
       (pattern receiver) ...)
     (syntax-rules:e-aux1
      #f (reserved-for-expansion ...)
      (pattern receiver) ...))

    ((syntax-rules:e reserved-for-alt-ellipsis
       (reserved-for-expansion ...)
       (pattern receiver) ...)
     (syntax-rules:e-aux1
      reserved-for-alt-ellipsis (reserved-for-expansion ...)
      (pattern receiver) ...)) ))

(define-syntax syntax-rules:e-aux1
  (syntax-rules ()
    ((¶ alt-ellipsis reserved ((ident . pattern) receiver) ...)
     (syntax-rules ()
       ((ident . pattern) (syntax-rules:e-aux2 pattern receiver))
       ...))))

(define-syntax syntax-rules:e-aux2
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((_LET_ (rename 'let))
            (arg* (cdr form))
            (actual-parameters (first arg*))
            (receiver (second arg*))
            (n (length actual-parameters))
            (tmp* (list-tabulate n (lambda (i) (gensym))))
            (f (gensym))
            (lets-list
             (cons
              (list f receiver) ;;;;;; FIXME: Do a recursive rename on the receiver.
              (list-tabulate
               n (lambda (i)
                   (list (list-ref tmp* i)
                         (list-ref actual-parameters i)))))))
       `(,_LET_ ,lets-list (,f . ,tmp*)) ))))

;;; »,«

;;; »)

(define-syntax reverse:e
  (syntax-rules:e ()
    ((¶ lst) reverse)))

;;;(define-syntax append:e
;;;  (syntax-rules:e ()
;;;    ((¶ x ...)
;;;     (append x ...))))

;;;;(define-macro:e append:e append)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'syntax-rules:e 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
