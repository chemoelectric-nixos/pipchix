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

(define-syntax nix-cond
  ;;
  ;; Unlike Scheme’s cond, nix-cond cannot have empty branches, nor
  ;; procedural branches, nor =>
  ;;
  ;; nix-cond could thus safely be written with fewer parentheses than
  ;; cond. Nevertheless, for familiarity, syntax similar to that of
  ;; cond will be retained. The syntax is actually much simpler than
  ;; that of cond.
  ;;
  (syntax-rules ( else )

    ((nix-cond (test clause)
               (else else-clause))
     (nix-if test
       clause
       else-clause))
    
    ((nix-cond (test1 clause1)
               (test2 clause2)
               ...
               (else else-clause))
     (nix-if test1
       clause1
       (nix-cond (test2 clause2)
                 ...
                 (else else-clause))))

    ((nix-cond (test1 clause1)
               (test2 clause2)
               ...
               (else-clause))
     (nix-cond (test1 clause1)
               (test2 clause2)
               ...
               (else else-clause)))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; eval: (put 'nix-if 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
