;;;
;;; Copyright © 2025, 2026 Barry Schwartz
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

(cond-expand
  (chicken-5
   (export em-syntax-rules-1))
  (else))

(define-syntax em-syntax-rules-1
  (er-macro-transformer
   (lambda (form rename compare)
     (let*-values
         (((arg*)
           (cdr form))
          ((alternate-ellipsis literals rules)
           (values (first arg*) (second arg*) (cddr arg*)))
          ((code)
           '()))

       (define (identifier? rule)
         (symbol? rule))

       (define (keyword? rule)
         (identifier? rule))

       (define (id=? id symb)
         (and (identifier? id)
              (compare id (rename symb))
              #t))

       (define (underscore form)
         (and (id=? form '_)
              form))

       (define (ellipsis form)
         (and (identifier? form)
              (if alternate-ellipsis
                (and (eq? form alternate-ellipsis)
                     form)
                (and (id=? form '...)
                     form))))

       (define (pattern-identifier form)
         (and (identifier? form)
              (begin
                (when (id=? form '...)
                  (err
                   "... is not allowed as a pattern identifier"
                   form))
                form)))

       (define (quoted? form)
         (and (proper-list? form)
              (= (length form) 2)
              (id=? (first form) 'quote)))

       (define (quasiquoted? form)
         (and (proper-list? form)
              (= (length form) 2)
              (id=? (first form) 'quasiquote)))

;;;;       (define (pattern-binding-spec form)
;;;;         ;; Returns two values: a top-level template and a top-level
;;;;         ;; pattern element.
;;;;         (unless (and (proper-list? form)
;;;;                      (= (length form) 3)
;;;;                      (id=? (second form) '=>))
;;;;           (err "expected a pattern binding spec" form))
;;;;         (values (first form) (third form)))
;;;;
;;;;       (define (top-level-pattern form)
;;;;         ;; Returns three values: a list of top-level pattern elements
;;;;         ;; before any ellipsis, #f or #t on whether there is an
;;;;         ;; ellipsis, and a list of top-level pattern elements after an
;;;;         ;; ellipsis.
;;;;         (unless (and (proper-list? form)
;;;;                      (<= 1 (length form)))
;;;;           (err "expected a top level pattern" form))
;;;;         (unless (identifier? (first form))
;;;;           (err "expected an identifier" (first form)))
;;;;         (let ((i (list-index ellipsis (cdr form))))
;;;;           (if i
;;;;             (begin
;;;;               (when (zero? i)
;;;;                 (err "there is nothing before the ellipsis" form))
;;;;               (values (take (cdr form) i) #t
;;;;                       (drop (cdr form) (+ i 1))))
;;;;             (values (cdr form) #f #f))))
;;;;
;;;;       (define (top-level-template form)
;;;;         ;; Returns ('unquoted . template) or ('quoted . template).
;;;;         (cond ((quoted-template form)
;;;;                (cons 'quoted (second form)))
;;;;               (else
;;;;                (unless (template form)
;;;;                  (err "expected a template or quoted template" form))
;;;;                (cons 'unquoted form))))
;;;;
;;;;       (define (top-level-pattern-element form)
;;;;         ;; Returns ('unquoted . pattern), ('quoted . pattern), or
;;;;         ;; ('quasiquoted . pattern).
;;;;         (cond ((quoted-pattern form)
;;;;                (cons 'quoted (second form)))
;;;;               ((quasiquoted-pattern form)
;;;;                (cons 'quasiquoted (second form)))
;;;;               (else
;;;;                (unless (pattern form)
;;;;                  (err "expected a pattern, quoted pattern, or quasiquoted pattern"
;;;;                       form))
;;;;                (cons 'unquoted form))))

       (define (quoted-template form)
         (and (quoted? form)
              (template (second form))
              form))

       (define (template form)
         ;; Returns #f or the form without any processing.
         (and (or (pattern-identifier form)
                  (proper-list? form)
                  (dotted-list? form)
                  (vector? form)
                  (datum form))
              form))

;;;;       (define (eager-macro-use form)
;;;;         (define (macro-use frm)
;;;;           (and (proper-list? frm)
;;;;                (positive? (length frm))
;;;;                (keyword? (first frm))
;;;;                'FIXME)) ;;;;;;;;;;;;;;;;;;;;;;;;; Presumably we recursively handle this and thus eagerly generate code.
;;;;         (and (proper-list? form)
;;;;              (case (length form)
;;;;                ((2) (and (id=? (first form) 'em)
;;;;                          (macro-use (second form))))
;;;;                ((1) (macro-use form))
;;;;                (else #f))))

;;;;       (define (eager-macro-datum form)
;;;;         (or (quoted-datum form)
;;;;             (quasiquoted-datum form)
;;;;             (eager-macro-use form)
;;;;             (datum form)))

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

;;;;       (define (slot-or-eager-macro-datum form)
;;;;         (or (id=? form '<>)
;;;;             (eager-macro-datum form)))





       (define (quoted-pattern form)
         ;; Returns #f or the pattern without the quote.
         (and (quoted? form)
              (syntax-pattern (second form))
              (second form)))

       (define (quasiquoted-pattern form)
         ;; Returns #f or the pattern without the quasiquote.
         (and (quasiquoted? form)
              (syntax-pattern (second form))
              (second form)))

       (define (syntax-pattern form)
         ;; Returns #f or the pattern without any processing.
         (and (or (pattern-identifier form)
                  (underscore form)
                  (proper-list? form)
                  (dotted-list? form)
                  (vector? form)
                  (datum form))
              form))

;;;;       (define (eager-macro-rule form)
;;;;         (let*-values (((top-level-pattern rest*)
;;;;                        (car+cdr form))
;;;;                       ((binding-specs template.pair)
;;;;                        (split-at rest* (- (length rest*) 1)))
;;;;                       ((template)
;;;;                        (car template.pair)))
;;;;           (when (pair? binding-specs)
;;;;             (err "we do not yet handle pattern binding specs"
;;;;                  binding-specs))
;;;;           (set! code (cons `(,(rename 'syntax-rules) ()
;;;;                              ((¶) "FIXME"))
;;;;                            code))))
;;;;           (let loop ((patelt* top-level-pattern))
;;;;             (cond ((quoted-pattern pattern) =>
;;;;                    (lambda (pattrn) "FIXME"))
;;;;                   ((quasiquoted-pattern pattern) =>
;;;;                    (lambda (pattrn) "FIXME"))
;;;;                   ((syntax-pattern pattern) =>
;;;;                    (lambda (pattrn)
;;;;                      (set! code
;;;;                        (cons `(,(rename 'syntax-rules) ()
;;;;                                (,pattrn ,template))
;;;;                              code))))
;;;;                   (else
;;;;                    (err "expected an eager macro rule" form)))))

       (define (eager-macro-rule form)
         (let-values (((ident patelt* bindspec* templt)
                       (values (first form) (second form)
                               (third form) (fourth form))))
           (when (pair? bindspec*)
             (err "we do not yet handle pattern binding specs"
                  bindspec*))
           ;;;;;;;; FIXME: This is just a quick test.
           (set! code (cons `(,(rename 'syntax-rules) ()
                              ((_) ,templt))
                            code))))

       (let loop ((rule* rules))
         (when (pair? rule*)
           (eager-macro-rule (car rule*))
           (loop (cdr rule*)))
         (concatenate (reverse code)))
       ))))

;;; »,«

(define-syntax em-syntax-rules-1
  (lambda (stx)
    (syntax (syntax-rules ()
              ((_) "FIXME")))))

;;; »)

(define-syntax em-syntax-rules
  (syntax-rules (=>)

    ((¶ (literals ...)
        ((ident . patelt*) (element => var) ... template)
        ...)

     (em-syntax-rules-1
      #f (literals ...)
      (ident patelt* ((element var) ...) template)
      ...))

    ((¶ ellipsis (literals ...)
        ((ident . patelt*) (element => var) ... template)
        ...)

     (em-syntax-rules-1
      ellipsis (literals ...)
      (ident patelt* ((element var) ...) template)
      ...))

    ((¶ . anything)
     (syntax-err "expected an eager syntax rule" anything))))

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
