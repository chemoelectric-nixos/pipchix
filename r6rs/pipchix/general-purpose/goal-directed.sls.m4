#!r6rs
;;;
;;; Copyright © 2026 Barry Schwartz
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
m4_include(pipchix/pipchix-includes.m4)

;;;
;;; Support for backtracking algorithms, by ‘success’ and ‘failure’,
;;; roughly in the fashion of the Icon language.
;;;

(library (pipchix general-purpose goal-directed)

  (export m4_include(pipchix/general-purpose/goal-directed.exports.m4))

  (import basic_libraries
          (rnrs mutable-pairs (6))
          (rnrs mutable-strings (6))
          (pipchix general-purpose srfi-39)
          (pipchix general-purpose charset)
          (pipchix general-purpose ec)
          (pipchix general-purpose match)
          (pipchix general-purpose box)
          (pipchix general-purpose cut))

  (define (list-set! lst i x)
    (set-car! (list-tail lst i) x))

  m4_include(pipchix/general-purpose/goal-directed.m4)

  (define icon-string-copy!
    (case-lambda
      ((to at from) (icon-string-copy! to at from 1 0))
      ((to at from start) (icon-string-copy! to at from start 0))
      ((to at from start end)
       (let* ((m (string-length to))
              (j1 (icon->scheme-indexing m at))
              (n (string-length from)))
         (let-values (((i1 i2) (icon->scheme-indexing n start end)))
           ;; For simple portability, the substring to be copied is
           ;; first copied into a buffer. An optimized implementation
           ;; could avoid the buffer.
           (let* ((buffer (substring from i1 i2))
                  (i2-i1 (- i2 i1)))
             (do-ec (:range i 0 i2-i1)
                    (string-set! to (+ j1 i)
                                 (string-ref buffer i)))))))))

  (define icon-string-fill!
    (case-lambda
      ((string fill) (icon-string-fill! string fill 1 0))
      ((string fill start) (icon-string-fill! string fill start 0))
      ((string fill start end)
       (let* ((n (string-length string)))
         (let-values (((i1 i2) (icon->scheme-indexing n start end)))
           (do-ec (:range i i1 i2) (string-set! string i fill)))))))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
