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

(define-syntax :goal-directed
  (syntax-rules (index)
    ((¶ cc var (index i) arg)
     (:parallel cc (:goal-directed var arg)
                (:integers i)) )
    ((¶ cc var arg)
     (:do cc
          (let ())
          ((t arg))
          (not (failure-object? t))
          (let ((var t)))
          #t
          (arg) ))))

(define-syntax co-expression-ec
  (syntax-rules (nested)
    ((¶ (nested q1 ...) q etc1 etc ...)
     (co-expression-ec (nested q1 ... q) etc1 etc ...) )
    ((¶ q1 q2             etc1 etc ...)
     (co-expression-ec (nested q1 q2)    etc1 etc ...) )
    ((¶ expression)
     (co-expression-ec (nested) expression) )

    ((¶ qualifier expression)
     (make-co-expression
      (lambda ()
        (define (ec)
          (do-ec
            qualifier
            (suspend expression))
          (fail))
        (in-new-failure-context ec))))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
