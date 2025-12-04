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
;;; This record interface requires only R⁷RS define-record-type or
;;; SRFI-9. See (pipchix abstract-syntax-tree) for an example of its
;;; use.
;;;

(define-syntax define-record-interface
  (syntax-rules ()
    ((_ type-name type-name? constructor predicate access)
     (begin
       (define-record-type type-name
         (make-type-name variety fields)
         type-name?
         (variety variety-ref)
         (fields access))
       (define (constructor variety)
         (lambda fields
           (make-type-name variety (list->vector fields))))
       (define predicate
         (case-lambda
           ((variety)
            ;; The default ‘eq?’ assumes the most common type for
            ;; ‘variety’ is a symbol.
            (lambda (obj)
              (and (type-name? obj)
                   (eq? (variety-ref obj) variety))))
           ((variety equiv?)
            (lambda (obj)
              (and (type-name? obj)
                   (equiv? (variety-ref obj) variety))))
           ((variety obj check?)
            (and (type-name? obj)
                 (check? (variety-ref obj) obj variety)))))))))

(define-syntax handle-record-interface-variety-rule%%%%
  (syntax-rules ( constructor>
                  predicate>
                  getter> setter> )
    ((_ variety constructor predicate access
        (constructor> name proc))
     (define name (proc (constructor 'variety))))
    ((_ variety constructor predicate access
        (constructor> name))
     (define name (constructor 'variety)))
    ((_ variety constructor predicate access
        (predicate> name))
     (define name (predicate 'variety)))
    ((_ variety constructor predicate access
        (getter> i name))
     (define name
       (lambda (obj)
         (vector-ref (access obj) (- i 1)))))
    ((_ variety constructor predicate access
        (setter> i name))
     (define name
       (lambda (obj value)
         (vector-set! (access obj) (- i 1) value))))))

(define-syntax define-record-interface-variety
  (syntax-rules ()
    ((_ variety constructor predicate access
        rule
        ...)
     (begin
       (handle-record-interface-variety-rule%%%%
        variety constructor predicate access
        rule)
       ...))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
