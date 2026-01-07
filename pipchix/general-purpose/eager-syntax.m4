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

;;; m4_define(«make_your_own_gensyms»,«
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
  (let ((i -1)
        (nonbreaking-space (string #\x00A0)))
    (case-lambda
      (() (gensym ""))
      ((ignored)
       (set! i (+ i 1))
       (string->symbol (string-append
                        "g"
                        nonbreaking-space
                        (random-partial-identifier)
                        nonbreaking-space
                        (number->string i)))))))
;;; »)

;;; m4_define(«define_finite_list_to_proper_list»,«
(define (finite-list->proper-list lst)
  ;; Copies a finite list, converting any dotted list to a
  ;; proper list.
  (cond
    ((null? lst) lst)
    ((not-pair? lst) (list lst))        ; Convert dotted list.
    (else
     (let ((next (cdr lst)))
       (cond
         ((null? next) lst)
         ((not-pair? next)
          (cons (car lst) next))        ; Convert dotted list.
         (else
          (cons (car lst)
                (finite-list->proper-list next))))))))
;;; »)

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

(cond-expand

  (chibi
   (begin
     make_your_own_gensyms
     (define-syntax eager-syntax
       (syntax-rules ()
         ((¶ receiver)
          (er-macro-transformer
           (lambda (form rename compare)
             make_your_own_gensyms
             define_finite_list_to_proper_list
             (let* ((arg* (cdr form))
                    (actual-parameters
                     (finite-list->proper-list arg*))
                    (f-x* (cons receiver actual-parameters))
                    (tmp* (map (lambda (x) (gensym)) f-x*))
                    (lets-list (map list tmp* f-x*)))
               `(,(rename 'let) ,lets-list ,tmp*)))))))) )

  (else
   (define-syntax eager-syntax
     (syntax-rules ()
       ((¶ receiver)
        (er-macro-transformer
         (lambda (form rename compare)
           define_finite_list_to_proper_list
           (let* ((arg* (cdr form))
                  (actual-parameters (finite-list->proper-list arg*))
                  (f-x* (cons receiver actual-parameters)))
             f-x*)))))) ))

;;; »)

;;; m4_ifelse(general_macros,«syntax-case»,«

(define-syntax eager-syntax
  (syntax-rules ()
    ((¶ receiver)
     (lambda (stx)
       (define (syntax->proper-list x)
         ;; Converts any dotted list to a proper list.
         (syntax-case x ()
           (()       '())
           ((a . a*) (cons (syntax a)
                           (syntax->proper-list (syntax a*))))
           (a        (list (syntax a))))) ;; The conversion.
       (let* ((form (syntax->proper-list stx))
              (arg* (cdr form))
              (f-arg* (cons (syntax receiver) arg*))
              (tmp* (generate-temporaries f-arg*))
              (lets-list (map list tmp* f-arg*)))
         (quasisyntax
          (let (unsyntax lets-list)
            (unsyntax tmp*))))))))

;;; »)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'syntax-rules:e 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
