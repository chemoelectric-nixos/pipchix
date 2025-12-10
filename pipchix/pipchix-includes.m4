m4_divert(-1)
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

m4_changequote(«,»)
m4_changecom(«#|»,«|#»)

;;;m4_define(«define_string_reverse_concatenate»,«
(define (%%string-reverse-concatenate lst)
  ;; Concatenation without the possible limitations of using
  ;; ‘apply’.
  (let loop ((lst lst)
             (str ""))
    (if (pair? lst)
        (loop (cdr lst) (string-append (car lst) str))
        str)))
;;;»)

;;;m4_define(«define_nix_set_setrec_letrec»,«
m4_pushdef(«who»,«$1»)m4_dnl
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
;;; THESE MACROS USE ONLY TRAILING ELLIPSES. They avoid the extensions
;;; of SRFI-46 and R⁷RS, which may not be available in Gambit Scheme.
;;;

(define-syntax find==>%%for-«»who«»%%
  (syntax-rules ( ==> )

    ((_ node ((key ...) (==> value)))
     (let* ((path-node (list->nix-attributepath-node
                        (reverse (list key ...))))
            (binding (make-nix-attributebinding-node
                      path-node (scheme->nix value))))
       («»who«»-node-set! node binding)))

    ((_ node ((key* ...) (key unknown ...)))
     (find==>%%for-«»who«»%%
      node ((key key* ...) (unknown ...))))))

(define-syntax expand-binding%%for-«»who«»%%
  (syntax-rules ( <== inherit inherit-from begin )

    ((_ node (value <== key ...))
     (let* ((path-node (list->nix-attributepath-node
                        (list key ...)))
            (binding (make-nix-attributebinding-node
                      path-node (scheme->nix value))))
       («»who«»-node-set! node binding)))

    ((_ node (inherit identifier ...))
     (let ((inherit-node (list->nix-inherit-node
                          (list (scheme->nix identifier) ...))))
       («»who«»-node-set! node inherit-node)))

    ((_ node (inherit-from attrset identifier ...))
     (let* ((attrset-node (scheme->nix attrset))
            (inherit-node (list->nix-inherit-node
                           (list (scheme->nix identifier) ...)
                           attrset-node)))
       («»who«»-node-set! node inherit-node)))

    ((_ node (begin entry ...))
     ;; Being able to group multiple entries into a single
     ;; s-expression is potentially useful, particularly with
     ;; advanced inclusion and macro systems.
     (begin (expand-binding%%for-«»who«»%% node entry) ...))

    ((_ node (begin))          ; This is a no-operation.
     #f)

    ((_ node (key unknown ...)) ; Binding by ==> arrow.
     (find==>%%for-«»who«»%% node ((key) (unknown ...))))))

m4_ifelse(who,«nix-letrec»,«
(define-syntax «»who«»
  (syntax-rules ()

    ((_ () in-clause)
     (let ((node (make-«»who«»-node)))
       (set-«»who«»-node-in-clause! node in-clause)
       node))

    ((_ (binding ...) in-clause)
     (let ((node (make-«»who«»-node)))
       (begin (expand-binding%%for-«»who«»%% node binding) ...)
       (set-«»who«»-node-in-clause! node in-clause)
       node))))
»)

m4_ifelse(who,«nix-attributeset»,«
(define-syntax nix-set%%for-«»who«»%%
  (syntax-rules ()

    ((_ () recursive?)
     (let ((node (make-«»who«»-node recursive?)))
       node))

    ((_ recursive? binding ...)
     (let ((node (make-«»who«»-node recursive?)))
       (begin (expand-binding%%for-«»who«»%% node binding) ...)
       node))))

(define-syntax nix-set ;; Set attributes.
  (syntax-rules ()
    ((_ binding ...)
     (nix-set%%for-«»who«»%% #f binding ...))))

(define-syntax nix-setrec ;; Set attributes recursively.
  (syntax-rules ()
    ((_ binding ...)
     (nix-set%%for-«»who«»%% #t binding ...))))
»)

m4_popdef(«who»)m4_dnl
;;;»)

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:

m4_divert«»m4_dnl
