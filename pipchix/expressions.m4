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

(define nix-get
  (case-lambda
    ((identifier)
     (make-nix-get-node #f identifier))
    ((attrset . attrpath)
     (make-nix-get-node attrset attrpath))))

(define © nix-get) ;; A synonym.

(define (nix-has? attrset attrpath)
  (make-nix-has?-node attrset attrpath))

(define nix//
  (case-lambda
    ((a b) (make-nix-//-node a b))
    ((a . args) (nix// a (apply nix// args)))))

(define (nix-not= a b) (make-nix-binaryoperator-node "!=" a b))
(define (nix= a b) (make-nix-binaryoperator-node "==" a b))
(define (nix< a b) (make-nix-binaryoperator-node "<" a b))
(define (nix<= a b) (make-nix-binaryoperator-node "<=" a b))
(define (nix> a b) (make-nix-binaryoperator-node ">" a b))
(define (nix>= a b) (make-nix-binaryoperator-node ">=" a b))

(define (nix-and a b) (make-nix-binaryoperator-node "&&" a b))
(define (nix-or a b) (make-nix-binaryoperator-node "||" a b))
(define (nix-> a b) (make-nix-binaryoperator-node "->" a b))
(define (nix-not a) (make-nix-unaryoperator-node "!" a))

(define nix++
  (case-lambda
    ((a b) (make-nix-binaryoperator-node "++" a b))
    ((a) (scheme->nix a))
    ((a . args) (nix++ (scheme->nix a) (apply nix++ args)))
    (() (nix-list)))) ;; An empty Nix list.

(define nix+
  (case-lambda
    ;;
    ;; Because nix+ operates on more than just numbers, the case of no
    ;; operands is not handled. In Scheme, by contrast, (+) returns 0.
    ;;
    ((a b) (make-nix-binaryoperator-node "+" a b))
    ((a . args) (let loop ((a (scheme->nix a))
                           (p args))
                  (if (pair? p)
                    (loop (nix+ a (car p)) (cdr p))
                    a)))))

(define nix-
  (case-lambda
    ((a b) (make-nix-binaryoperator-node "-" a b))
    ((a) (make-nix-unaryoperator-node "-" a))
    ((a . args) (let loop ((a (scheme->nix a))
                           (p args))
                  (if (pair? p)
                    (loop (nix- a (car p)) (cdr p))
                    a)))))

(define nix*
  (case-lambda
    ((a b) (make-nix-binaryoperator-node "*" a b))
    ((a . args) (let loop ((a (scheme->nix a))
                           (p args))
                  (if (pair? p)
                    (loop (nix* a (car p)) (cdr p))
                    a)))
    (() (scheme->nix 1))))

(define nix/
  (case-lambda
    ((a b) (make-nix-binaryoperator-node "/" a b))
    ((a) (nix/ (scheme->nix 1.0) a))
    ((a . args) (let loop ((a (scheme->nix a))
                           (p args))
                  (if (pair? p)
                    (loop (nix/ a (car p)) (cdr p))
                    a)))))

(define (nix-if if-clause then-clause else-clause)
  (make-nix-ifthenelse-node
   if-clause then-clause else-clause))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
