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

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-syntax nix-letrec
  (syntax-rules ()
    ((_ (binding ...) in-clause)
     (let ((node (make-nix-letrec-node)))
       (expand-%%nix-letrec%%-bindings node binding ...)
       (set-nix-letrec-node-in-clause! node in-clause)
       node))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-syntax nix-let*
  (syntax-rules ( inherit inherit-from )

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

    ((_ (binding) in-clause)
     (nix-letrec (binding)
       in-clause))

    ((_ (binding1 binding2 ...) in-clause)
     (nix-letrec (binding1)
       (nix-let* (binding2 ...)
         in-clause)))

    ((_ () in-clause)
     (nix-letrec ()
       in-clause))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-syntax nix-let
  (syntax-rules ()
    ((_ (binding ...) in-clause)
     (let ((bdgs '()))
       (collect-%%nix-let%%-bindings! bdgs binding ...)
       (let ((node1 (make-nix-letrec-node))
             (node2 (make-nix-letrec-node)))
         (let loop ((p (reverse bdgs)))
           (when (pair? p)
             (let ((tmp (generate-identifier))
                   (attrpath (caar p))
                   (value (cadar p)))
               (insert-%%nix-letrec%%-binding
                node1 (list tmp) value)
               (insert-%%nix-letrec%%-binding
                node2 attrpath (make-nix-get-node
                                #f (list->nix-attributepath-node
                                    (list tmp))))
               (loop (cdr p)))))
         (set-nix-letrec-node-in-clause! node1 node2)
         (set-nix-letrec-node-in-clause! node2 in-clause)
         node1)))))

(define-syntax collect-%%nix-let%%-bindings!
  (syntax-rules ( inherit inherit-from <== ==> )

    ((_ bdgs (inherit-from s a) binding ...)
     (let ((value (expand-%%nix-letrec%%-get-node s a)))
       (set! bdgs `((,(list a) ,value) . ,bdgs))
       (collect-%%nix-let%%-bindings! bdgs binding ...)))

    ((_ bdgs (inherit-from s a b ...) binding ...)
     (let ((value (expand-%%nix-letrec%%-get-node s a)))
       (set! bdgs `((,(list a) ,value) . ,bdgs))
       (collect-%%nix-let%%-bindings!
        bdgs (inherit-from s b ...) binding ...)))

    ((_ bdgs (inherit a b ...) binding ...)
     (collect-%%nix-let%%-bindings!
      bdgs (inherit-from #f a b ...) binding ...))

    ((_ bdgs (value <== a b ...) binding ...)
     (begin
       (set! bdgs `((,(list a b ...) ,value) . ,bdgs))
       (collect-%%nix-let%%-bindings! bdgs binding ...)))

    ((_ bdgs (a b ... ==> value) binding ...)
     (begin
       (set! bdgs `((,(list a b ...) ,value) . ,bdgs))
       (collect-%%nix-let%%-bindings! bdgs binding ...)))

    ((_ bdgs) #t)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; eval: (put 'nix-letrec 'scheme-indent-function 1)
;;; eval: (put 'nix-let* 'scheme-indent-function 1)
;;; eval: (put 'nix-let 'scheme-indent-function 1)
;;; end:
