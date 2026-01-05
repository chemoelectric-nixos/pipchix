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

;;; m4_ifelse(scheme_standard,«r6rs»,«

(define-syntax syntax-error
  (syntax-rules ()
    ((¶ message . args)
     ;; R⁶RS provides only for two arguments after the message. So put
     ;; all the arguments into a list.
     (syntax-violation #f message args))))

;;; »)

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

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
            "g    " (random-partial-identifier) "    "
            (number->string i) "    ")))))) )

  (else)) ;; cond-expand

;;; m4_define(«syntax_rules_e_aux2»,«
#|
(er-macro-transformer
 (lambda (form rename compare)
   (let* ((_LET_ (rename 'let))
          (arg* (cdr form))
          (actual-parameters (list-copy (first arg*)))
          (actual-parameters
           (if (pair? actual-parameters)
             (let ((lst-pair (last-pair actual-parameters)))
               (if (null? (cdr lst-pair))
                 actual-parameters
                 (begin
                   ;; Turn a dotted list into a proper list.
                   (set-cdr! lst-pair (list (cdr lst-pair)))
                   actual-parameters)))))
          (receiver (second arg*))
          (n (length actual-parameters))
          (tmp* (list-tabulate n (lambda (i) (gensym))))
          (f (gensym))
          (lets-list
           (cons
            (list f receiver)
            (list-tabulate
             n (lambda (i)
                 (list (list-ref tmp* i)
                       (list-ref actual-parameters i)))))))
`(,_LET_ ,lets-list (,f . ,tmp*)) )))
|#
;;; »)

;;; »,«

;;; m4_define(«define_syntax_primitives»,«
(define (syntax->proper-list stx_)
  ;; Converts any dotted list to a proper list.
  (syntax-case stx_ ()
    (()       '())
    ((x . x*) (cons (syntax x)
                    (syntax->proper-list (syntax x*))))
    (x        (list (syntax x))))) ;; The conversion.

(define (syntax-first stx_)
  (syntax-case stx_ ()
    ((x . y) (syntax x))))

(define (syntax-second stx_)
  (syntax-case stx_ ()
    ((x y . z) (syntax y))))

(define (syntax-cdr stx_)
  (syntax-case stx_ ()
    ((x . y) (syntax y))))
;;; »)

;;; m4_define(«syntax_rules_e_aux2»,«
#|
(lambda (stx)
  define_syntax_primitives
  (let* ((form (syntax->proper-list stx))
         (arg* (syntax-cdr form))
         (actual-parameters
          (syntax->proper-list (first arg*)))
         (receiver (second arg*))
         (n (length actual-parameters))
         (tmp* (generate-temporaries actual-parameters))
         (f (car (generate-temporaries '(1))))
         (lets-list
          (cons
           (list f receiver)
           (list-tabulate
            n (lambda (i)
                (list (list-ref tmp* i)
                      (list-ref actual-parameters i)))))))
    (quasisyntax (let (unsyntax lets-list)
                   ((unsyntax f) . (unsyntax tmp*)))) ))
|#
;;; »)

;;; »)

#|
(define-syntax syntax-rules:e-aux1
  (syntax-rules ()

    ((¶ #f (literals ...) ((ident . pattern) receiver) ...)

     (syntax-rules (literals ...)
       ((ident . pattern)
        (let-syntax ((syntax-rules:e-aux2
                      syntax_rules_e_aux2
                      ))
          (syntax-rules:e-aux2 pattern receiver)))
       ...))

    ;; m4_ifelse(scheme_standard,«r6rs»,,«
    ((¶ alt-ellipsis (literals ...) ((ident . pattern) receiver) ...)

     (syntax-rules alt-ellipsis (literals ...)
       ((ident . pattern)
        (let-syntax ((syntax-rules:e-aux2
                      syntax_rules_e_aux2
                      ))
          (syntax-rules:e-aux2 pattern receiver)))
       ...))
    ;; »)

))


(define-syntax syntax-rules:e
  (syntax-rules ()

    ((syntax-rules:e (literals ...)
       (pattern receiver) ...)

     (syntax-rules:e-aux1
      #f (literals ...) (pattern receiver) ...))

    ;; m4_ifelse(scheme_standard,«r6rs»,,«
    ((syntax-rules:e alt-ellipsis (literals ...)
                     (pattern receiver) ...)

     (syntax-rules:e-aux1
      alt-ellipsis (literals ...) (pattern receiver) ...))
    ;; »)

))
|#

(define-syntax syntax-rules:e
  (syntax-rules ()
    ((¶ rule)
     (syntax-rules:e-rule rule))
    ((¶ (rule . rule*))
     (cons (syntax-rules:e-rule rule)
           (syntax-rules:e rule*)))))

(define-syntax syntax-rules:e-rule
  (syntax-rules (=>)
    ((¶ (pattern (=> failure) receiver))
     "FIXME")
    ((¶ (pattern receiver))
     "FIXME")
    ((¶ . anything)
     (syntax-error "expected a syntax-rules:e rule" anything))))
    

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'syntax-rules:e 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
