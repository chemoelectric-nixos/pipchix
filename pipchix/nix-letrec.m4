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

define_nix_set_setrec_letrec(«nix-letrec»)

(define-syntax nix-letrec
  (syntax-rules ()
    ((_ (binding ...) in-clause)
     (let ((node (make-nix-letrec-node)))
       (expand-%%nix-letrec%%-bindings node binding ...)
       (set-nix-letrec-node-in-clause! node in-clause)
       node))))

(define-syntax nix-let*
  (syntax-rules ( inherit inherit-from )

    ((_ () in-clause)
     (nix-letrec ()
       in-clause))

    ((_ ((inherit-from s a) binding ...) in-clause)
     (nix-letrec ((inherit-from s a))
       (nix-let* (binding ...)
         in-clause)))

    ((_ ((inherit-from s a b ...) binding ...) in-clause)
     (nix-letrec ((inherit-from s a))
       (nix-let* ((inherit-from s b ...) binding ...)
         in-clause)))

    ((_ ((inherit a b ...) binding ...) in-clause)
     (nix-let* ((inherit-from #f a b ...) binding ...)
       in-clause))

    ((_ (binding1 binding2 ...) in-clause)
     (nix-letrec (binding1)
       (nix-let* (binding2 ...)
         in-clause)))))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'nix-letrec 'scheme-indent-function 1)
;;; eval: (put 'nix-let* 'scheme-indent-function 1)
;;; eval: (put 'nix-let 'scheme-indent-function 1)
;;; end:
