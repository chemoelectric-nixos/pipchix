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

;; Bound and free identifier code for R⁷RS is based on the example
;; implementation for SRFI-148 by Marc Nieper-Wißkirchen, which is
;; licensed as follows:
;;
;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

(define-syntax eager-syntax
  (syntax-rules ()
    ((¶ receiver)
     (er-macro-transformer
      (lambda (form rename compare)
        (define (finite-list->proper-list lst)
          ;; Copies a finite list, converting any dotted list to a
          ;; proper list.
          (cond
            ((null? lst) lst)
            ((not-pair? lst) (list lst)) ;; Convert dotted list.
            (else
             (let ((next (cdr lst)))
               (cond
                 ((null? next) lst)
                 ((not-pair? next)
                  (cons (car lst) next)) ;; Convert dotted list.
                 (else
                  (cons (car lst)
                        (finite-list->proper-list next))))))))
        (let* ((arg* (cdr form))
               (actual-parameters
                (finite-list->proper-list arg*))
               (f-x* (cons receiver actual-parameters))
               (tmp* (map (lambda (x) (gensym)) f-x*))
               (lets-list (map list tmp* f-x*)))
          `(,(rename 'let) ,lets-list ,tmp*)))))))

(cond-expand
  (chibi

   (define-syntax bound-identifier-equiv?
     (er-macro-transformer
      (lambda (form rename compare)
        (let ((arg* (cdr form)))
          (eq? (first arg*) (second arg*))))))

   (define-syntax free-identifier-equiv?
     (er-macro-transformer
      (lambda (form rename compare)
        (let ((arg* (cdr form)))
          (compare (first arg*) (second arg*))))))

   )

  (else

   (define-syntax free-identifier-equiv?
     (syntax-rules ()
       ((¶ id1 id2)
        (let-syntax
            ((m (syntax-rules :::1 ()
	          ((µ kt kf)
	           (let-syntax
                       ((test
		         (syntax-rules :::2 (id1)
		           ((τ id1 kt^ kf^)
                            kt^)
		           ((τ x kt^ kf^)
                            kf^))))
	             (test id2 kt kf))))))
          (m #t #f)))))

   (define-syntax bound-identifier-equiv?
     (syntax-rules ()
       ((¶ id v)
        (let-syntax
            ((m (syntax-rules :::1 ()				       
	          ((m kt kf)
	           (letrec-syntax
                       ((id (syntax-rules :::2 ()
		              ((υ kt^ kf^)
                               kf^)))
	                (ok (syntax-rules ()
		              ((ω kt^ kf^)
                               kt^)))
	                (test (syntax-rules :::2 ()
		                ((τ v kt^ kf^)
                                 (id kt^ kf^))
   	                        ((τ _ kt^ kf^)
                                 (id kt^ kf^)))))
	             (test ok kt kf))))))
          (m #t #f)))))
   ))

(define-syntax dequote
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((_quote_ (rename 'quote))
            (_quasiquote_ (rename 'quasiquote))
            (_unquote_ (rename 'unquote))
            (_unquote-splicing_ (rename 'unquote-splicing))
            (quote=? (cut compare <> _quote_))
            (quasiquote=? (cut compare <> _quasiquote_))
            (unquote=? (cut compare <> _unquote_))
            (unquote-splicing=? (cut compare <> _unquote-splicing_)))
       (unless (= (length form) 2)
         SCHEME_ERROR("expected one argument", form))
       (let ((expr (second form)))
         (cond
           ((not-pair? expr)
            expr)
           ((quote=? (first expr))
            (unless (equal? (length+ expr) 2)
              SCHEME_ERROR("quote expects one argument", expr))
            (second expr))
           ((not (quasiquote=? (first expr)))
            expr)
           (else
            (unless (equal? (length+ expr) 2)
              SCHEME_ERROR("quasiquote expects one argument", expr))
            (let recurs ((expr (second expr))
                         (depth 0))
              (match expr

                (((? unquote=? _) x)
                 (if (zero? depth)
                   x
                   (recurs expr (- depth 1))))

                ((((? unquote-splicing=? _) x) . x*)
                 (if (zero? depth)
                   (begin
                     (unless (proper-list? x)
                       SCHEME_ERROR("expected a proper list", x))
                     (append x (recurs x* 0)))
                   (recurs expr (- depth 1))))

                (((? quasiquote=? _) _)
                 (recurs expr (+ depth 1)))

                ((? vector? x)
                 (list->vector (recurs (vector->list x) depth)))

                ((? not-pair? _)
                 expr)

                (_
                 (cons (recurs (car expr) depth)
                       (recurs (cdr expr) depth))) )))))))))

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

(define-syntax bound-identifier-equiv?
  (syntax-rules ()
    ((¶ s1 s2)
     (bound-identifier=? (syntax s1) (syntax s2)))))

(define-syntax free-identifier-equiv?
  (syntax-rules ()
    ((¶ s1 s2)
     (free-identifier=? (syntax s1) (syntax s2)))))

(define-syntax dequote
  (lambda (stx)
    (letrec
        ((syntax->proper-list
          (lambda (x)
            (syntax-case x ()
              (()      '())
              ((h . t) (cons (syntax h)
                             (syntax->proper-list (syntax t))))
              (x       (list (syntax x))))))
         (syntax-vector->syntax-list
          (lambda (x)
            (syntax-case x ()
              (#(a ...) (syntax (a ...))))))
         (proper-list->syntax-vector
          (lambda (x)
            (syntax-case x ()
              ((a ...) (syntax #(a ...)))))))
      (let ((quote=?
             (cut free-identifier=? <> (syntax quote)))
            (quasiquote=?
             (cut free-identifier=? <> (syntax quasiquote)))
            (unquote=?
             (cut free-identifier=? <> (syntax unquote)))
            (unquote-splicing=?
             (cut free-identifier=? <> (syntax unquote-splicing)))
            (syntax-vector?
             (lambda (v) (vector? (syntax->datum v))))
            (form (syntax->proper-list stx)))            
        (unless (= (length form) 2)
          SCHEME_ERROR("expected one argument", form))
        (let ((expr (second form)))
          (cond
            ((not-pair? expr)
             expr)
            ((quote=? (first expr))
             (unless (equal? (length+ expr) 2)
               SCHEME_ERROR("quote expects one argument", expr))
             (second expr))
            ((not (quasiquote=? (first expr)))
             expr)
            (else
             (unless (equal? (length+ expr) 2)
               SCHEME_ERROR("quasiquote expects one argument", expr))
             (let recurs ((expr (second expr))
                          (depth 0))
               (match expr

                 (((? unquote=? _) x)
                  (if (zero? depth)
                    x
                    (recurs expr (- depth 1))))

                 ((((? unquote-splicing=? _) x) . x*)
                  (if (zero? depth)
                    (begin
                      (unless (proper-list? x)
                        SCHEME_ERROR("expected a proper list", x))
                      (append x (recurs x* 0)))
                    (recurs expr (- depth 1))))

                 (((? quasiquote=? _) _)
                  (recurs expr (+ depth 1)))

                 ((? syntax-vector? x)
                  (proper-list->syntax-vector
                   (recurs (syntax->proper-list
                            (syntax-vector->syntax-list x))
                           depth)))

                 ((? not-pair? _)
                  expr)

                 (_
                  (cons (recurs (car expr) depth)
                        (recurs (cdr expr) depth))) )))))))))

;;; »)

(define-syntax eager-match-rules
  (syntax-rules ()
    ((¶ rule ...)
     (lambda vals
       (match (apply list vals)
         rule ...)))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
