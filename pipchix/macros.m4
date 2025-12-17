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

;;;;;;;;;;;
;;;;;;;;;;; FIXME: implement nix-case once function calls are ready.
;;;;;;;;;;;

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

(define-syntax nix-with
  (syntax-rules ()

    ((_ (attrset) clause)
     (make-nix-with-node attrset clause))

    ((_ (attrset1 attrset2 . attrset*) clause)
     (nix-with (attrset1)
       (nix-with (attrset2 . attrset*) clause)))

    ((_ () clause) (scheme->nix clause))))

(define-syntax nix-lambda
  (syntax-rules ()

    ((_ () clause)
     ;; Let this be a synonym for a function that takes an empty set
     ;; as argument.
     (nix-lambda (()) clause))

    ((_ (arguments ...) clause)
     (collect-args (arguments ...) (scheme->nix clause) '()))))

(define-syntax collect-args
  (syntax-rules ()
    
    ((_ () clause args)
     (make-nix-lambda-node (reverse args) clause))

    ((_ (()) clause args)
     (collect-args () clause (cons '() args)))

    ((_ (() elem2 ...) clause args)
     (collect-args (elem2 ...) clause (cons '() args)))

    ((_ (((id1 val1) el2 ...) elem2 ...) clause args)
     (collect-attrs ((el2 ...) elem2 ...) clause
                    args (list (list (scheme->nix 'id1)
                                     (scheme->nix val1)))))

    ((_ ((id) elem2 ...) clause args)
     (let ((last-thing (if-syntax-match-ellipsis id
                         (nix-lambda-ellipsis-argument)
                         (scheme->nix 'id))))
       (collect-args (elem2 ...) clause
                     (cons (list last-thing) args))))

    ((_ ((id1 el2 ...) elem2 ...) clause args)
     (collect-attrs ((el2 ...) elem2 ...) clause
                    args (list (scheme->nix 'id1))))

    ((_ (ident1 elem2 ...) clause args)
     (collect-args (elem2 ...) clause
                   (cons (scheme->nix 'ident1) args)))))

(define-syntax collect-attrs
  (syntax-rules ()

    ((_ (() elem2 ...) clause args attrs)
     (collect-args (elem2 ...) clause
                   (cons (reverse attrs) args)))

    ((_ (((id1 val1) el2 ...) elem2 ...) clause args attrs)
     (collect-attrs ((el2 ...) elem2 ...) clause
                    args (cons (list (scheme->nix 'id1)
                                     (scheme->nix val1))
                               attrs)))

    ((_ ((id) elem2 ...) clause args attrs)
     (let ((last-thing (if-syntax-match-ellipsis id
                         (nix-lambda-ellipsis-argument)
                         (scheme->nix 'id))))
       (collect-args (elem2 ...) clause
                     (cons (reverse (cons last-thing attrs)) args))))
    
    ((_ ((id1 el2 ...) elem2 ...) clause args attrs)
     (collect-attrs ((el2 ...) elem2 ...) clause
                    args (cons (scheme->nix 'id1) attrs)))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if-syntax-match-ellipsis 'scheme-indent-function 1)
;;; eval: (put 'nix-if 'scheme-indent-function 1)
;;; eval: (put 'nix-with 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
