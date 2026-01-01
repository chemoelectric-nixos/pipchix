;;;
;;; Copyright © 2025, 2026 Barry Schwartz
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

(define-library (pipchix general-purpose em-syntax-rules)

  (export m4_include(pipchix/general-purpose/em-syntax-rules.exports.m4))
  ;; m4_ifelse(CHICKEN_5,«yes»,,«
  (cond-expand
    (gauche (export :info-alist))
    (else))
  ;; »)
 
  (import (scheme base))
  (import (scheme write)) ;; For debugging.
  (import (scheme eval))
  (import (pipchix general-purpose list))

  (cond-expand
    (chicken-5 (import (only (chicken syntax) er-macro-transformer)))
    (chibi (import (only (chibi) er-macro-transformer)))
    (gauche (import (only (r7rs aux) er-macro-transformer))
            (import (only (r7rs aux) :info-alist)))
    (sagittarius (import (only (sagittarius) er-macro-transformer)))
    ((or loko guile) (import (rnrs syntax-case (6))))
    (else))

  (begin

    define_err_r7rs
    define_syntax_err_r7rs

    (cond-expand
      (chicken-5
       ;; m4_pushdef(«general_macros»,«er-macro-transformer»)
       ;; m4_pushdef(«syntax_rules»,«r5rs»)
       ;; m4_pushdef(«scheme_standard»,«r5rs»)
       (import-for-syntax (scheme base))
       (import-for-syntax (srfi 1))
       m4_include(pipchix/general-purpose/em-syntax-rules.m4)
       ;; m4_popdef(«general_macros»,«syntax_rules»,«scheme_standard»)
       )
      ((or loko guile)
       ;; m4_pushdef(«general_macros»,«syntax-case»)
       ;; m4_pushdef(«syntax_rules»,«unknown»)
       ;; m4_pushdef(«scheme_standard»,«r7rs»)
       m4_include(pipchix/general-purpose/em-syntax-rules.m4)
       ;; m4_popdef(«general_macros»,«syntax_rules»,«scheme_standard»)
       )
      (else
       ;; m4_pushdef(«general_macros»,«er-macro-transformer»)
       ;; m4_pushdef(«syntax_rules»,«r7rs»)
       ;; m4_pushdef(«scheme_standard»,«r7rs»)
       m4_include(pipchix/general-purpose/em-syntax-rules.m4)
       ;; m4_popdef(«general_macros»,«syntax_rules»,«scheme_standard»)
       ))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
