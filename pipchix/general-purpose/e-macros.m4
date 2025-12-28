;;;
;;; Copyright © 2025 Barry Schwartz
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

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

(define-syntax define-e-macro
  (syntax-rules ()
    ((¶ E-MACRO-NAME E-MACRO-PROCEDURE)
     (define-syntax E-MACRO-NAME
       (er-macro-transformer
        (lambda (form rename compare)
          (let ((arguments (cdr form)))
            (let* ((evaluate
                    (lambda (obj)
                      (e-macros-eval obj (apply e-macros-environment
                                                (e-macros-libraries)))))
                   (f-of-x*
                    (let loop ((arg* arguments)
                               (x* '()))
                      (if (pair? arg*)
                        (let-values (((arg arg*) (car+cdr arg*)))
                          (let* ((quot (rename 'quote))
                                 (x (cond
                                      ((not-pair? arg)
                                       arg)
                                      ((not (compare (car arg) quot))
                                       arg)
                                      ((not-pair? (car arg))
                                       ;; Expand the argument.
                                       (evaluate arg))
                                      ((not (compare (caar arg) quot))
                                       ;; Expand the argument.
                                       (evaluate arg))
                                      (else
                                       ;; Drop the quoting by a level.
                                       (car arg)))))
                            (loop arg* (cons x x*))))
                        (cons `(e-macros-evaluate 'E-MACRO-PROCEDURE)
                              (reverse! x*))))))
              `,f-of-x*))))))))

;;; »,«

(define-syntax define-e-macro
  (syntax-rules ()
    ((¶ E-MACRO-NAME E-MACRO-PROCEDURE)
     (define-syntax E-MACRO-NAME
       (lambda (stx)
         (syntax-case stx ()
           ((µ . arguments)
            (let* ((evaluate
                    (lambda (obj)
                      (e-macros-eval obj (apply e-macros-environment
                                                (e-macros-libraries)))))
                   (f-of-x*
                    ;; The following strips all syntactic information
                    ;; from the ‘quote’ expressions.
                    (let loop ((arg* `,(syntax->datum
                                        (syntax arguments)))
                               (x* '()))
                      (if (pair? arg*)
                        (let-values (((arg arg*) (car+cdr arg*)))
                          (let ((x (cond
                                     ((not-pair? arg)
                                      arg)
                                     ((not (eq? (car arg) 'quote))
                                      arg)
                                     ((not-pair? (car arg))
                                      ;; Expand the argument.
                                      (evaluate arg))
                                     ((not (eq? (caar arg) 'quote))
                                      ;; Expand the argument.
                                      (evaluate arg))
                                     (else
                                      ;; Drop the quoting by a level.
                                      (car arg)))))
                            (loop arg* (cons x x*))))
                        (cons `(e-macros-evaluate 'E-MACRO-PROCEDURE)
                              (reverse! x*))))))
              (datum->syntax (syntax µ) f-of-x*)))))))))

;;;;;
;;;;; Does not work and probably is unnecessary:
;;;;;
;;;;;(define-syntax define-e-macro
;;;;;  (syntax-rules ()
;;;;;    ((¶ E-MACRO-NAME E-MACRO-PROCEDURE)
;;;;;     (define-syntax E-MACRO-NAME
;;;;;       (lambda (stx)
;;;;;         (define syntax->list
;;;;;           (lambda (stx_)
;;;;;             (syntax-case stx_ ()
;;;;;               (() '())
;;;;;               ((x . rest)
;;;;;                (cons (syntax x)
;;;;;                      (syntax->list (syntax rest)))))))
;;;;;         (define prepare-argument
;;;;;           (lambda (stx_)
;;;;;             (syntax-case stx_ (quote)
;;;;;               ((µ ''x)
;;;;;                (syntax 'x))
;;;;;               ((µ 'x)
;;;;;                (let* ((x_ (syntax->datum (syntax x)))
;;;;;                       (x_ (e-macros-eval
;;;;;                            x_ (apply e-macros-environment
;;;;;                                      (e-macros-libraries)))))
;;;;;                  (datum->syntax (syntax µ) x_)))
;;;;;               ((µ x)
;;;;;                (syntax x)))))
;;;;;         (syntax-case stx ()
;;;;;           ((µ . arguments)
;;;;;            (let loop ((arg* (syntax->list (syntax arguments)))
;;;;;                       (x* '()))
;;;;;              (if (pair? arg*)
;;;;;                (let* ((x (prepare-argument (list (syntax µ)
;;;;;                                                  (car arg*)))))
;;;;;                  (loop (cdr arg*) (cons x x*)))
;;;;;                (datum->syntax
;;;;;                 (syntax µ)
;;;;;                 `((e-macros-eval
;;;;;                    'E-MACRO-PROCEDURE
;;;;;                    ',(apply e-macros-environment
;;;;;                             (e-macros-libraries)))
;;;;;                   . ,(map syntax->datum (reverse! x*)))))))))))))

;;; »)

(define-e-macro e-false (lambda anything #f))
(define-e-macro e-true (lambda anything #t))

(define-e-macro e-list list)


m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
