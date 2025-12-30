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

(define-syntax em-syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (let-values
         (((alternate-ellipsis literals rules)
           (if (pair? (second form))
             (values #f (second form) (cddr form))
             (values (second form) (third form) (cdddr form)))))

       (define (check-literals litrls)
         (unless (pair? litrls)
           (err "malformed em-syntax-rules literals" form)))

       (define (identifier? rule)
         (symbol? rule))

       (define (keyword? rule)
         (identifier? rule))

       (define (id=? id symb)
         (and (identifier? id)
              (compare id (rename symb))
              #t))

       (define (quoted? form)
         (and (proper-list? form)
              (= (length form) 2)
              (id=? (first form) 'quote)))

       (define (quasiquoted? form)
         (and (proper-list? form)
              (= (length form) 2)
              (id=? (first form) 'quasiquote)))

       (define (pattern-identifier form)
         (and (identifier? form)
              (begin
                (when (id=? form '...)
                  (err "... is not allowed as a pattern identifier"
                       form))
                form)))

       (define (ellipsis form)
         (and (identifier? form)
              (if alternate-ellipsis
                (and (eq? form alternate-ellipsis)
                     form)
                (and (id=? form '...)
                     form))))

       (define (underscore form)
         (and (id=? form '_)
              form))

       (define (quoted-template form)
         (and (quoted? form)
              (template (second form))
              form))

       (define (template form)
         (or (pattern-identifier form)
             'FIXMExxxxxxxxxx))

       (define (template-element form i)
         ;; Returns two values: ('no-ellipsis . template), ('ellipsis . template),
         ;; or #f, and a next index.
         (let ((n (length form)))
           (cond ((= i n)
                  (values #f i))
                 ((= i (- n 1))
                  (let ((t (template (list-ref form i))))
                    (if t
                      (values (cons 'no-ellipsis t) n)
                      (values #f i))))
                 (else
                  (let ((t (template (list-ref form i))))
                    (if t
                      (let ((e (ellipsis (list-ref form (+ i 1)))))
                        (if e
                          (values (cons 'ellipsis t) (+ i 2))
                          (values (cons 'no-ellipsis t) (+ i 1))))
                      (values #f i)))))))
             
       (define (eager-macro-use form)
         (define (macro-use frm)
           (and (proper-list? frm)
                (positive? (length frm))
                (keyword? (first frm))
                'FIXME)) ;;;;;;;;;;;;;;;;;;;;;;;;; Presumably we recursively handle this and thus eagerly generate code.
         (and (proper-list? form)
              (case (length form)
                ((2) (and (id=? (first form) 'em)
                          (macro-use (second form))))
                ((1) (macro-use form))
                (else #f))))

       (define (eager-macro-datum form)
         (or (quoted-datum form)
             (quasiquoted-datum form)
             (eager-macro-use form)
             (datum form)))

       (define (quoted-datum form)
         (and (quoted? form)
              (datum (second form))
              form))

       (define (quasiquoted-datum form)
         (and (quasiquoted? form)
              (datum (second form))
              form))

       (define (datum form)
         (and (or (string? form)
                  (char? form)
                  (boolean? form)
                  (number? form)
                  (bytevector? form)))
         form)

       (define (slot-or-eager-macro-datum form)
         (or (id=? form '<>)
             (eager-macro-datum form)))

       (check-literals literals)
       'FIXME))))

;;; »,«

(define-syntax em-syntax-rules
  (lambda (stx)
    (syntax 'FIXME)))

;;; »)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'with-syntax 'scheme-indent-function 1)
;;; eval: (put 'stx-satisfied? 'scheme-indent-function 1)
;;; eval: (put 'stx-integer? 'scheme-indent-function 1)
;;; eval: (put 'stx-exact? 'scheme-indent-function 1)
;;; eval: (put 'stx-list? 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
