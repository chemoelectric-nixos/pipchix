#!r6rs
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
m4_include(pipchix/pipchix-includes.m4)

(library (pipchix string-manipulation)

  (export m4_include(pipchix/string-manipulation.exports.m4))

  (import (rnrs base (6))
          (rnrs control (6))
          (rnrs lists (6))
          (rnrs mutable-strings (6))
          (rnrs unicode (6))
          (rnrs bytevectors (6))
          (pipchix general-purpose box)
          (pipchix general-purpose srfi-39))

  define_err_r6rs
  (define integer-division div-and-mod)

  (define (string-copy! to at from)
    ;; A limited version of string-copy! that assumes non-overlapping
    ;; regions.
    (let ((n (string-length from)))
      (let loop ((i at)
                 (j 0))
        (unless (= j n)
          (string-set! to i (string-ref from j))
          (loop (+ i 1) (+ j 1))))))

  m4_include(pipchix/string-manipulation.m4)

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
