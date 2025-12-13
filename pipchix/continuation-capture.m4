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
;;; The continuations interface of Marc Feeley’s paper, ‘A Better API
;;; for First-Class Continuations’,
;;; http://www.iro.umontreal.ca/~feeley/papers/FeeleySW01.pdf
;;; https://web.archive.org/web/20250701033312/http://www.iro.umontreal.ca/~feeley/papers/FeeleySW01.pdf
;;;
;;; See also SRFI-226: Control Features, which includes an interface
;;; derived from this one.
;;;
;;; CHICKEN includes its own implementation of this interface,
;;; (chicken continuation), with continuation?, continuation-capture,
;;; continuation-graft, and continuation-return. Our implementation is
;;; not compatible with it. There is also a CHICKEN egg,
;;; ‘continuations’, that reëxports (chicken continuation) with the
;;; procedure names changed. It includes some small corollary
;;; procedures that are duplicated here, but implemented with our
;;; stuff and sometimes with different names. I have implemented
;;; further corollaries.
;;;
;;; Obviously it would be better to have these operations as
;;; primitives rather than implementing them as wrappers around
;;; call/cc, but for Pipchix we do most often will not need great
;;; efficiency. (If we do, we can offload tasks to Ada programs!)
;;;

(define-record-type <continuation>
  (%%make-continuation proc)
  continuation?
  (proc %%continuation-proc))

(define (continuation-capture receiver)
  ((call/cc (lambda (proc)
              (receiver (%%make-continuation proc))))))

(define (continuation-graft cc thunk)
  ((%%continuation-proc cc) thunk))

(define (continuation-return cc . returned-values)
  (continuation-graft
   cc
   (lambda () (apply values returned-values))))

(define (continuation->procedure cc)
  ;; A better name might be ‘wrap-continuation-in-procedure’, but a
  ;; primitive implementation might not use a wrapper.
  (lambda args
    (apply continuation-return cc args)))

(define (current-continuation)
  ;;
  ;; Returns a snapshot of the current continuation into itself. You
  ;; can use this thus:
  ;;
  ;;  (let ((cc (current-continuation)))
  ;;    (if (continuation? cc)
  ;;      (begin (do-stuff) (continuation-return cc result))
  ;;      (use-result cc)))
  ;;
  ;; This is like doing:
  ;;
  ;;  (let ((result (continuation-capture
  ;;                  (lambda (cc)
  ;;                    (do-stuff)
  ;;                    (continuation-return cc result)))))
  ;;    (use-result result))
  ;;
  (continuation-capture
   (lambda (cc) (continuation-return cc cc))))

;;;
;;; The following is called GOTO. As if tail calls were not GOTOs!
;;;
;;; It is often claimed GOTO is dangerous. This is true! But thisv
;;; author believes it is mainly because strictly structured
;;; programming in procedural languages makes it easy to monitor the
;;; McCabe complexity by eye. There is a simple procedure for this,
;;; which any kind of GOTO disrupts. GOTOs thus destroy the
;;; programmer’s ability to monitor complexity. The programmer
;;; therefore loses track of when to split a procedure into parts. The
;;; result is code whose procedures are too large, even if by just a
;;; little.
;;;
;;; Programmers will endlessly make excuses for this behavior: for
;;; their production of procedures that contain GOTO (by other names)
;;; and which are too large. The one thing they will NOT do is
;;; practice strictly structured programming when writing in
;;; procedural languages. They will NOT monitor complexity and will
;;; NOT split their procedures into small bits (or write macros that
;;; simplify expressions) as the procedures approach a McCabe
;;; complexity of ten.
;;;
;;; But here we are writing in Scheme. Different considerations apply
;;; than if we were writing in Ada or C. I do not have a general rule,
;;; as I do for ordinary procedural languages.
;;;
(define (goto-continuation cc)
  (continuation-return cc cc))

(define (continuation-capture-call-with-values
         continuation-receiver
         values-receiver)
  ;;
  ;; Use continuation-capture-call-with-values similarly to
  ;; call-with-values:
  ;;
  ;;   (continuation-capture-call-with-values
  ;;       (lambda (cc) ...)
  ;;     (lambda results ...))
  ;;
  (call-with-values
      (lambda () (continuation-capture continuation-receiver))
    values-receiver))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
