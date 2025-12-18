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

(define-syntax nix-lambda
  (syntax-rules ()

    ((_ () clause)
     ;; Let this be a synonym for a function that takes an empty set
     ;; as argument.
     (nix-lambda (()) clause))

    ((_ (arguments ...) clause)
     (collect-args (arguments ...) (scheme->nix clause) (list)))))

(define-syntax collect-args
  (syntax-rules ( list )
    
    ((_ () clause args)
     (make-nix-lambda-node args clause))

    ((_ (()) clause (list args ...))
     (collect-args () clause (list args ... (quote ()))))

    ((_ (() elem2 ...) clause (list args ...))
     (collect-args (elem2 ...) clause (list args ... (quote ()))))

    ((_ (((id1 val1) el2 ...) elem2 ...) clause (list args ...))
     (collect-attrs ((el2 ...) elem2 ...) clause
                    (list args ...)
                    (list (list (scheme->nix 'id1)
                                (scheme->nix val1)))))

    ((_ ((id) elem2 ...) clause (list args ...))
     (let ((last-thing (if-syntax-match-ellipsis id
                         (nix-lambda-ellipsis-argument)
                         (scheme->nix 'id))))
       (collect-args (elem2 ...) clause
                     (list args ... (list last-thing)))))

    ((_ ((id1 el2 ...) elem2 ...) clause (list args ...))
     (collect-attrs ((el2 ...) elem2 ...) clause
                    (list args ...) (list (scheme->nix 'id1))))

    ((_ (ident1 elem2 ...) clause (list args ...))
     (collect-args (elem2 ...) clause
                   (list args ... (scheme->nix 'ident1))))))

(define-syntax collect-attrs
  (syntax-rules ( list )

    ((_ (() elem2 ...) clause (list args ...) attrs)
     (collect-args (elem2 ...) clause (list args ... attrs)))

    ((_ (((id1 val1) el2 ...) elem2 ...) clause args (list attrs ...))
     (collect-attrs ((el2 ...) elem2 ...) clause
                    args (list attrs ... (list (scheme->nix 'id1)
                                               (scheme->nix val1)))))

    ((_ ((id) elem2 ...) clause (list args ...) (list attrs ...))
     (let ((last-thing (if-syntax-match-ellipsis id
                         (nix-lambda-ellipsis-argument)
                         (scheme->nix 'id))))
       (collect-args (elem2 ...) clause
                     (list args ... (list attrs ... last-thing)))))
    
    ((_ ((id1 el2 ...) elem2 ...) clause args (list attrs ...))
     (collect-attrs ((el2 ...) elem2 ...) clause
                    args (list attrs ... (scheme->nix 'id1))))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if-syntax-match-ellipsis 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
