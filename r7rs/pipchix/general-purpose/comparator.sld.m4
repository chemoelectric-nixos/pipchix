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

(define-library (pipchix general-purpose comparator)

  ;; SRFI-128 Comparators [ R⁷RS-large (scheme comparator) ]

  (export m4_include(pipchix/general-purpose/comparator.exports.m4))

  (import (scheme base))
  (cond-expand
    (loko
     (import (scheme case-lambda))
     (import (scheme char))
     (import (scheme inexact))
     (import (scheme complex))
     (import (pipchix general-purpose define-record-factory))
     ;;
     ;; Use the native R⁶RS hash functions.
     ;;
     (import (only (rnrs hashtables (6))
                   equal-hash
                   string-hash
                   string-ci-hash
                   symbol-hash)))
    (else
     (import (scheme comparator))))

  (begin

    (cond-expand
      (loko
       m4_include(pipchix/general-purpose/comparator.m4)
       m4_include(pipchix/general-purpose/comparator/128.body1.scm)
       m4_include(pipchix/general-purpose/comparator/128.body2.scm)
       )
      (else
       (if #f #f)
       ))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
