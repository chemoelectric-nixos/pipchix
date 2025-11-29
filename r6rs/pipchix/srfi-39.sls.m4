#!r6rs
;;;
;;; Copyright © 2025 Barry Schwartz. All Rights Reserved.
;;; Copyright © 2002 Marc Feeley. All Rights Reserved.
;;;
;;; This file is part of Pipchix.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
m4_include(pipchix/pipchix-includes.m4)

;;;
;;; Marc Feeley’s single-threaded reference implementation for SRFI-39
;;; (dynamically bound parameters), made into an R⁶RS library for
;;; internal use in Pipchix. Users might find it useful, although
;;; their implementation of Scheme is likely to provide (srfi :39).
;;;
;;; Pipchix is written by someone whose Scheme writing is mostly for
;;; R⁷RS. Thus SRFI implementations such as this one are included for
;;; compatibility with R⁷RS.
;;;

(library (pipchix srfi-39)

  (export make-parameter
          parameterize)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs mutable-pairs (6)))

  (define make-parameter
    (lambda (init . conv)
      (let ((converter
             (if (null? conv) (lambda (x) x) (car conv))))
        (let ((global-cell
               (cons #f (converter init))))
          (letrec ((parameter
                    (lambda new-val
                      (let ((cell (dynamic-lookup parameter global-cell)))
                        (cond ((null? new-val)
                               (cdr cell))
                              ((null? (cdr new-val))
                               (set-cdr! cell (converter (car new-val))))
                              (else ; this case is needed for parameterize
                               (converter (car new-val))))))))
            (set-car! global-cell parameter)
            parameter)))))

  (define-syntax parameterize
    (syntax-rules ()
      ((parameterize ((expr1 expr2) ...) body ...)
       (dynamic-bind (list expr1 ...)
                     (list expr2 ...)
                     (lambda () body ...)))))

  (define dynamic-bind
    (lambda (parameters values body)
      (let* ((old-local
              (dynamic-env-local-get))
             (new-cells
              (map (lambda (parameter value)
                     (cons parameter (parameter value #f)))
                   parameters
                   values))
             (new-local
              (append new-cells old-local)))
        (dynamic-wind
          (lambda () (dynamic-env-local-set! new-local))
          body
          (lambda () (dynamic-env-local-set! old-local))))))

  (define dynamic-lookup
    (lambda (parameter global-cell)
      (or (assq parameter (dynamic-env-local-get))
          global-cell)))

  (define dynamic-env-local '())

  (define dynamic-env-local-get
    (lambda () dynamic-env-local))

  (define dynamic-env-local-set!
    (lambda (new-env) (set! dynamic-env-local new-env)))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
