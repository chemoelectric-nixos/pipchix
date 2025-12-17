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

(define-library (pipchix if-syntax-match-ellipsis)

  (export if-syntax-match-ellipsis)

  (import (scheme base))
  (import (scheme cxr))

  (cond-expand
    (chicken-5
     (import (only (chicken base)
                   gensym)
             (chicken syntax)))
    (loko
     (import (rnrs syntax-case (6))))
    (else))

  (begin

    (cond-expand
      (chicken-5
       ;; CHICKEN 5 is really R⁵RS, and does not have R⁷RS
       ;; syntax-rules. Use er-macro-transformer.
       (define-syntax if-syntax-match-ellipsis
         (er-macro-transformer
          (lambda (form rename compare)
            (let* ((ellipsis (rename '...))
                   (ellipsis=? (lambda (id)
                                 (and (symbol? id)
                                      (compare id ellipsis)))))
              (unless (= (length form) 4)
                (error
                 "expected (if-syntax-match-ellipsis form then-clause else-clause)"
                 form))
              (if (ellipsis=? (cadr form))
                (caddr form)
                (cadddr form)))))))
      (loko
       ;; Loko seems to have broken R⁷RS syntax-rules. So instead use
       ;; R⁶RS syntax-case
       (define-syntax if-syntax-match-ellipsis
         (lambda (stx)
           (define ellipsis (string->symbol "..."))
           (syntax-case stx ()
             ((_ id then-clause else-clause)
              (syntax-case stx ()
                ((_ id then-clause else-clause)
                 (if (free-identifier=?
                      (syntax id)
                      (datum->syntax (syntax id) ellipsis))
                   (syntax then-clause)
                   (syntax else-clause)))))))))
       (else
        (define-syntax if-syntax-match-ellipsis
          (syntax-rules ::: ( ... )
            ((_ ... then-clause else-clause)
             then-clause)
            ((_ otherwise then-clause else-clause)
             else-clause)))))

      ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
