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
m4_include(pipchix/pipchix-includes.m4)

(define-library (pipchix macros)

  (export m4_include(pipchix/macros.exports.m4))

  (import (scheme base))
  (import (pipchix abstract-syntax-tree))
  (import (pipchix nix-list))
  (import (pipchix expressions))

  (cond-expand
    (loko
     (import (rnrs syntax-case (6))))
    (else))
  
  (begin

    (cond-expand
      (loko
       (define-syntax ellipsis-branch
         (lambda (stx)
           (syntax-case stx ()
             ((_ id then-clause else-clause)
              (with-syntax ((ellps (syntax (... ...))))
                (syntax-case stx (ellps)
                  ((_ ellps then-clause else-clause)
                   (syntax then-clause))
                  ((_ other then-clause else-clause)
                   (syntax else-clause)))))))))
      (else
       (define-syntax ellipsis-branch
         (syntax-rules ::: ( ... )
           ((_ ... then-clause else-clause)
            then-clause)
           ((_ otherwise then-clause else-clause)
            else-clause)))))

    m4_include(pipchix/macros.m4)

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
