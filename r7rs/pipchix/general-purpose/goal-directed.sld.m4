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

(define-library (pipchix general-purpose goal-directed)

  ;; m4_define(«rnrs_number»,«7»)

  (export m4_include(pipchix/general-purpose/goal-directed.exports.m4))

  (import basic_libraries
          (pipchix general-purpose match)
          (pipchix general-purpose box)
          (pipchix general-purpose ec)
          (pipchix general-purpose cut))
  (cond-expand
    ((or chibi gauche sagittarius)
     (import (scheme charset)))
    (else))

  (begin

    (cond-expand
      ((or chibi gauche sagittarius)
       ;; m4_pushdef(«support_scheme_charset»,«yes»)
       m4_include(pipchix/general-purpose/goal-directed.m4)
       ;; m4_popdef(«support_scheme_charset»)
       )
      (else
       m4_include(pipchix/general-purpose/goal-directed.m4)
       ))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
