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
;;; end:
m4_divert«»m4_dnl
