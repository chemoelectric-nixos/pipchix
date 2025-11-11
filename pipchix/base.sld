;;; Copyright © 2025 Barry Schwartz
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

(define-library (pipchix base)
  (export nix-set)
  (import (scheme base)
                                        ;(srfi 148) ;; Eager syntax-rules.
          (pipchix abstract-syntax-tree))
  (begin

    (define-syntax nix-set ;; Set attributes.
      (syntax-rules ( <--- )
        ((nix-set (attr-name ... <--- attr-value) ...)
         (let ((attrset (make-nix-attributeset-node)))
           (begin
             (let ((path-node (list->nix-attributepath-node
                               (list attr-name ...))))
               (if (nix-attributeset-node-contains? attrset path-node)
                   (error "duplicate Nix attribute" path-node)
                   (nix-attributeset-node-set!
                    attrset path-node (%%scheme->nix attr-value))))
             ...)
           attrset))))

    (define (%%scheme->nix value) ;; Convert Scheme values to Nix AST.
      (cond ((or (string? value)
                 (number? value)
                 (boolean? value)
                 (eq? '() value))
             (make-nix-data-node value))
            ((symbol? value)
             (make-nix-data-node (symbol->string value)))
            (else value)))

    ))
