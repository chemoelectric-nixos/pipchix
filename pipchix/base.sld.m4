;;;
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
;;;
m4_include(pipchix-includes.m4)

(define-library (pipchix base)

  (export nix-list)   ;; Nix list from elements.
  (export nix-set)    ;; Set attributes non-recursively.
  (export nix-setrec) ;; Set attributes recursively.

  (import (scheme base))
  (import (pipchix nix-list))
  (import (pipchix abstract-syntax-tree))

  (begin

    (define-syntax nix-set ;; Set attributes non-recursively.
      (syntax-rules ()
        ((nix-set entry ...)
         (%%nix-set #f entry ...))))

    (define-syntax nix-setrec ;; Set attributes recursively.
      (syntax-rules ()
        ((nix-setrec entry ...)
         (%%nix-set #t entry ...))))

    (define-syntax %%nix-set
      (syntax-rules ()
        ((%%nix-set recursive? entry ...)
         (let ((attrset (make-nix-attributeset-node recursive?)))
           (begin
             (%%nix-set-insert-entry attrset entry)
             ...)
           attrset))))

    (define-syntax %%nix-set-insert-entry
      (syntax-rules ( --> <-- inherit )
        ((%%nix-set-insert-entry        ; Binding by --> arrow.
          attrset (attr-name ... --> attr-value))
         (let* ((path-node (list->nix-attributepath-node
                            (list attr-name ...)))
                (binding (make-nix-attributebinding-node
                          path-node (scheme->nix attr-value))))
           (nix-attributeset-node-set! attrset binding)))
        ((%%nix-set-insert-entry        ; Binding by <-- arrow.
          attrset (attr-value <-- attr-name ...))
         (%%nix-set-insert-entry
          attrset (attr-name ... --> attr-value)))
        ((%%nix-set-insert-entry        ; ‘inherit ()’
          attrset (inherit () identifier ...))
         (let ((inherit-node (list->nix-inherit-node
                              (list (scheme->nix identifier) ...))))
           (nix-attributeset-node-set! attrset inherit-node)))
        ((%%nix-set-insert-entry        ; ‘inherit (attribute-set)’
          attrset (inherit (attrset2) identifier ...))
         (let* ((attrset-node (scheme->nix attrset2))
                (inherit-node (list->nix-inherit-node
                               (list (scheme->nix identifier) ...)
                               attrset-node)))
           (nix-attributeset-node-set! attrset inherit-node)))
        ((%%nix-set-insert-entry        ; (begin ...)
          attrset (begin entry ...))
         ;; Being able to group multiple entries into a single
         ;; s-expression is potentially useful, particularly with
         ;; advanced inclusion and macro systems.
         (begin
           (%%nix-set-insert-entry attrset entry)
           ...))))

    (define (%%->string obj)
      (cond ((string? obj) obj)
            (else (nix-node-data-ref obj))))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
