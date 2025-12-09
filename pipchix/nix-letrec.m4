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
;;; THESE MACROS USE ONLY TRAILING ELLIPSES.
;;;
;;; Do not require the extensions of SRFI-46 and R⁷RS. They are not
;;; available in Gambit 4.9.7, which has only an old version of
;;; syntax-case/syntax-rules.
;;;

;;;;;;;;;;; FIXME: COMBINE THIS CODE WITH nix-set.m4 BY M4 MAGIC.
;;;;;;;;;;;        FOR INSTANCE, we can put the stuff in pipchix-includes.m4.

(define-syntax find==>%%for-nix-letrec%%
  (syntax-rules ( ==> )

    ((_ lrnode ((key ...) (==> value)))
     (let* ((path-node (list->nix-attributepath-node
                        (reverse (list key ...))))
            (binding (make-nix-attributebinding-node
                      path-node (scheme->nix value))))
       (nix-letrec-node-set! lrnode binding)))

    ((_ lrnode ((key* ...) (key unknown ...)))
     (find==>%%for-nix-letrec%%
      lrnode ((key key* ...) (unknown ...))))))

(define-syntax expand-binding%%for-nix-letrec%%
  (syntax-rules ( <== inherit inherit-from begin )

    ((_ lrnode (value <== key ...))
     (let* ((path-node (list->nix-attributepath-node
                        (list key ...)))
            (binding (make-nix-attributebinding-node
                      path-node (scheme->nix value))))
       (nix-letrec-node-set! lrnode binding)))

    ((_ lrnode (inherit identifier ...))
     (let ((inherit-node (list->nix-inherit-node
                          (list (scheme->nix identifier) ...))))
       (nix-letrec-node-set! lrnode inherit-node)))

    ((_ lrnode (inherit-from attrset identifier ...))
     (let* ((attrset-node (scheme->nix attrset))
            (inherit-node (list->nix-inherit-node
                           (list (scheme->nix identifier) ...)
                           attrset-node)))
       (nix-letrec-node-set! lrnode inherit-node)))

    ((_ lrnode (begin entry ...))
     ;; Being able to group multiple entries into a single
     ;; s-expression is potentially useful, particularly with
     ;; advanced inclusion and macro systems.
     (begin (expand-binding%%for-nix-letrec%% lrnode entry) ...))

    ((_ lrnode (begin))          ; This is a no-operation.
     #f)

    ((_ lrnode (key unknown ...)) ; Binding by ==> arrow.
     (find==>%%for-nix-letrec%% lrnode ((key) (unknown ...))))))

(define-syntax nix-letrec
  (syntax-rules ()

    ((_ () in-clause)
     (let ((lrnode (make-nix-letrec-node)))
       (set-nix-letrec-node-in-clause! lrnode in-clause)
       lrnode))

    ((_ (binding ...) in-clause)
     (let ((lrnode (make-nix-letrec-node)))
       (begin (expand-binding%%for-nix-letrec%% lrnode binding) ...)
       (set-nix-letrec-node-in-clause! lrnode in-clause)
       lrnode))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
