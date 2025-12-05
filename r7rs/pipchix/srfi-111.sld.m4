;;; Copyright © 2013 John Cowan. All Rights Reserved.

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;
;;; The SRFI-111 reference implementation for R⁷RS (where needed).
;;;

(define-library (pipchix srfi-111)

  (export box
          box?
          unbox
          set-box!)

  (cond-expand
    ((or chicken guile)
     (import (scheme base)
             (srfi 111))) ;; = (scheme box)
    ((or chibi gauche gerbil sagittarius)
     (import (scheme base)
             (scheme box)))
    (else
     (import (scheme base))))

  (begin

    (cond-expand
      ((or chibi chicken gauche gerbil guile sagittarius)
       #f)
      (else
       (define-record-type box-type
         (box value)
         box?
         (value unbox set-box!))))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
