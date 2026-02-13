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

(define-library (pipchix general-purpose error-reporting)

  ;;
  ;; Unified R⁶RS and R⁷RS error reporting.
  ;;

  (export m4_include(pipchix/general-purpose/error-reporting.exports.m4))

  (import (rename (scheme base) (error r7rs-error))
          (pipchix general-purpose list))

  (begin

    (define (error . arg*)
      (cond ((and (pair? arg*)
                  (pair? (cdr arg*))
                  (eq? (first arg*) #f)
                  (string? (second arg*)))
             ;; R⁶RS syntax.
             (apply r7rs-error (cons* (second arg*) (drop arg* 2))))

            ((and (pair? arg*)
                  (pair? (cdr arg*))
                  (symbol? (first arg*))
                  (string? (second arg*)))
             ;; R⁶RS syntax.
             (apply r7rs-error (cons* (second arg*)
                                      (string-append
                                       "[who:\x2009;'"
                                       (symbol->string (first arg*))
                                       "]")
                                      (drop arg* 2))))

            ((and (pair? arg*)
                  (pair? (cdr arg*))
                  (string? (first arg*))
                  (string? (second arg*)))
             ;; R⁶RS syntax. If (first arg*) is a string the syntax
             ;; may actually, however, be R⁷RS misinterpreted. It is
             ;; better to use a symbol for ‘who’.
             (apply r7rs-error (cons* (second arg*)
                                      (string-append
                                       "[who:\x2009;\x201C;"
                                       (first arg*)
                                       "\x201D;]")
                                      (drop arg* 2))))

            ;; Assume R⁷RS syntax.
            (else (apply r7rs-error arg*))))

    (define (syntax-violation who message . arg*)
      (let ((message (string-append "[syntax\xA0;violation:\x2009;"
                                    message "]")))
        (apply error (cons* who message arg*))))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
