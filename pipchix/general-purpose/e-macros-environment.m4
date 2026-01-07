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

(define e-macros-eval eval)
(define e-macros-environment environment)

(define *e-macros-libraries*
  (make-parameter
   (vector 'default_environment)
   (lambda (obj)
     (unless (and (vector? obj)
                  (>= (vector-length obj) 1))
       SCHEME_ERROR("expected a vector of length >=1", obj))
     obj)))

(define (e-macros-libraries)
  (vector-ref (*e-macros-libraries*) 0))

(define-syntax with-e-macros-libraries
  (syntax-rules ()
    ((¶ lst body ...)
     (parameterize ((*e-macros-libraries* (vector lst)))
       (if #f #f) ;; The body defaults to ‘#<unspecified>’
       body ...))))

(define (set-e-macros-libraries! lst)
  (vector-set! (*e-macros-libraries*) 0 lst))

(define (e-macros-evaluate form)
  (e-macros-eval form (apply e-macros-environment
                             (e-macros-libraries))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
