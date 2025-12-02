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

;;;
;;; USE ONLY TRAILING ELLIPSES.
;;;
;;; Do not require the extensions of SRFI-46 and R⁷RS. They seem not
;;; to be available in Gambit 4.9.7, despite the claim that it
;;; supports R⁷RS. (On the other hand, Gambit 4.9.7 also seems unable
;;; to export macros from libraries, so what difference does it make?
;;; But it would make a difference, for instance, if we provided
;;; Pipchix macros as ‘include’ files.)
;;;

(define-syntax %%nix-set%%add-binding
  (syntax-rules ()
    ((%%nix-set%%add-binding attrset key value)
     (let* ((path-node (list->nix-attributepath-node key))
            (binding (make-nix-attributebinding-node
                      path-node (scheme->nix value))))
       (nix-attributeset-node-set! attrset binding)))))

(define-syntax %%nix-set%%nix-attributebinding
  ;;
  ;; Avoid the need for a SRFI-46 ellipsis left of the ==>. Instead
  ;; recursively rewrite the syntax.
  ;;
  (syntax-rules ( ==> )
    ;;
    ((%%nix-set%%nix-attributebinding
      attrset (attr-name ...) (==> attr-value))
     (%%nix-set%%add-binding attrset (reverse (list attr-name ...))
                    attr-value))
    ;;
    ((%%nix-set%%nix-attributebinding
      attrset (attr-name1 ...) (attr-name2 unknown ...))
     (%%nix-set%%nix-attributebinding
      attrset (attr-name2 attr-name1 ...) (unknown ...)))))

(define-syntax %%nix-set%%nix-set-insert-entry
  (syntax-rules ( <== inherit begin )
    ((%%nix-set%%nix-set-insert-entry   ; Binding by <== arrow.
      attrset (attr-value <== attr-name ...))
     (%%nix-set%%add-binding attrset (list attr-name ...) attr-value))
    ;;
    ((%%nix-set%%nix-set-insert-entry   ; ‘inherit ()’
      attrset (inherit () identifier ...))
     (let ((inherit-node (list->nix-inherit-node
                          (list (scheme->nix identifier) ...))))
       (nix-attributeset-node-set! attrset inherit-node)))
    ;;
    ((%%nix-set%%nix-set-insert-entry   ; ‘inherit (attribute-set)’

      attrset (inherit (attrset2) identifier ...))
     (let* ((attrset-node (scheme->nix attrset2))
            (inherit-node (list->nix-inherit-node
                           (list (scheme->nix identifier) ...)
                           attrset-node)))
       (nix-attributeset-node-set! attrset inherit-node)))
    ;;
    ((%%nix-set%%nix-set-insert-entry   ; (begin ...)
      attrset (begin entry ...))
     ;; Being able to group multiple entries into a single
     ;; s-expression is potentially useful, particularly with
     ;; advanced inclusion and macro systems.
     (begin
       (%%nix-set%%nix-set-insert-entry attrset entry)
       ...))
    ;;
    ((%%nix-set%%nix-set-insert-entry   ; Binding by ==> arrow.
      attrset (attr-name unknown ...))
     (%%nix-set%%nix-attributebinding
      attrset (attr-name) (unknown ...)))))

(define-syntax %%nix-set%%nix-set
  (syntax-rules ()
    ((%%nix-set%%nix-set recursive? entry ...)
     (let ((attrset (make-nix-attributeset-node recursive?)))
       (begin
         (%%nix-set%%nix-set-insert-entry attrset entry)
         ...)
       attrset))))

(define-syntax nix-set ;; Set attributes.
  (syntax-rules ()
    ((nix-setrec entry ...)
     (%%nix-set%%nix-set #f entry ...))))

(define-syntax nix-setrec ;; Set attributes recursively.
  (syntax-rules ()
    ((nix-setrec entry ...)
     (%%nix-set%%nix-set #t entry ...))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
