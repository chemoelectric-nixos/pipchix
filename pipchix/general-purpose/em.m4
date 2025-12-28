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
           (if (pair? (cadr form))
             (values #f (cadr form) (cddr form))
             (values (cadr form) (caddr form) (cdddr form)))))
       (unless (pair? literals)
         (err "bad syntax near start of em-syntax-rules" form))
       'FIXME))))

;;; »,«

'FIXME

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
