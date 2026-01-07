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

(define-library (pipchix general-purpose define-record-factory)

  (export m4_include(pipchix/general-purpose/define-record-factory.exports.m4))

  (import (scheme base))
  (import (scheme case-lambda))
  (cond-expand
    (chicken-5
     (import (only (chicken base) gensym)
             (chicken syntax)))
    (else))

  (begin

    (cond-expand
      (chicken-5
       ;; Use er-macro-transformer. (There is complicated coördination
       ;; of symbols between different definitions, so it might be
       ;; unsurprising that syntax-rules implementations for R⁵RS have
       ;; trouble.)
       m4_define(«implementation_of_define_record_factory»,
                 «er-macro-transformer»)
       m4_include(pipchix/general-purpose/define-record-factory.m4)
       m4_undefine(«implementation_of_define_record_factory»))
      (else
       m4_define(«implementation_of_define_record_factory»,«srfi-9»)
       m4_include(pipchix/general-purpose/define-record-factory.m4)
       m4_undefine(«implementation_of_define_record_factory»)))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
