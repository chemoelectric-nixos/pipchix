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
;;;-------------------------------------------------------------------

(define-syntax define-backtracking
  (syntax-rules ()
    ((¶ step fail)
     (begin

       ;; Create a return stack.
       (define continuations '())

       (define step
         (case-lambda

           ((before thunk after)
            (dynamic-wind

              ;; Run user-defined code.
              before

              (lambda ()
                (call/cc
                 (lambda (cc)
                   ;; Put a return address on the stack.
                   (set! continuations (cons cc continuations))
                   ;; Run user-defined code.
                   (thunk))))

              (lambda ()
                ;; Remove the obsolete return address.
                (set! continuations (cdr continuations))
                ;; Run user-defined code.
                (after))))

           ((thunk)
            ;; A step in which ‘before’ and ‘after’ are no-operations.
            (let ((nop (lambda () (if #f #f))))
              (step nop thunk nop)))))

       (define fail
         (let ((error-message "the backtracking stack is empty"))
           (case-lambda
             (()
              (unless (pair? continuations)
                SCHEME_ERROR(error-message, (list fail)))
              ;; Return to the return address, without arguments.
              ;; (This is merely a fast path that avoids
              ;; call-with-values.)
              ((car continuations)))
             (args
              (unless (pair? continuations)
                SCHEME_ERROR(error-message, (cons fail args)))
              ;; Return to the return address, with the given
              ;; arguments.
              (call-with-values
                  (lambda () (apply values args))
                (car continuations))))))
       ))))

;;;-------------------------------------------------------------------
m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
