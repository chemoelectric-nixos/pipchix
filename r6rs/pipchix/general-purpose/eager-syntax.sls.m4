#!r6rs
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

(library (pipchix general-purpose eager-syntax)

  (export m4_include(pipchix/general-purpose/eager-syntax.exports.m4))

  (import (for (except (rnrs (6))
                       fold-right
                       member
                       assoc
                       map
                       remove)
            ;; See
            ;; https://racket.discourse.group/t/error-cond-unbound-identifier-also-no-app-syntax-transformer-is-bound/3081/3
            ;; for why I added (meta -1). It is for Racket.
            run expand (meta -1))
          (for (pipchix general-purpose list) run expand)
          (for (pipchix general-purpose match) run expand))

  ;; m4_define(«scheme_standard»,«r6rs»)
  ;; m4_define(«general_macros»,«syntax-case»)

  m4_include(pipchix/general-purpose/eager-syntax.m4)

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
