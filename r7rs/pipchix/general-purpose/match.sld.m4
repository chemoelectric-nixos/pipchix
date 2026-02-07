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

(define-library (pipchix general-purpose match)

  (export m4_include(pipchix/general-purpose/match.exports.m4))
  (cond-expand
    (gauche (export :info-alist))
    (else))

  (import basic_libraries)

  (cond-expand
    (chicken-5 (import (only (chicken syntax)
                             er-macro-transformer))
               (import (rename (chicken keyword)
                               (keyword?
                                chicken-scheme-keyword?))))
    (chibi (import (scheme file))
           (import (only (chibi)
                         er-macro-transformer
                         identifier?))
           (import (pipchix general-purpose continuation-capture)))
    (gauche (import (only (r7rs aux)
                          :info-alist
                          er-macro-transformer
                          is-a?)
                    (only (gauche record)
                          rtd-accessor
                          rtd-mutator)))
    (sagittarius (import (only (sagittarius)
                               er-macro-transformer)
                         (only (sagittarius clos)
                               is-a?
                               slot-ref-using-class
                               slot-set-using-class!)
                         (rename (only (sagittarius clos)
                                       (slot-ref orig-slot-ref)
                                       (slot-set! orig-slot-set!)))))
    (loko (import (rnrs syntax-case (6))))
    (else))

  (begin

    (cond-expand
      (loko
       ;; m4_pushdef(«general_macros»,«syntax-case»)
       m4_include(pipchix/general-purpose/match.m4)
       ;; m4_popdef(«general_macros»)
       )
      (else
       ;; m4_pushdef(«general_macros»,«er-macro-transformer»)
       m4_include(pipchix/general-purpose/match.m4)
       ;; m4_popdef(«general_macros»)
       ))

    (cond-expand
      (gauche
       (define (slot-ref rec v name)
         ((rtd-accessor rec name) v))
       (define (slot-set! rec v name value)
         ((rtd-mutator rec name) v value)))

      (sagittarius
       (define slot-ref
         (case-lambda
           ((rec v name)
            (slot-ref-using-class rec v name))
           ((obj name)
            (orig-slot-ref obj name))))
       (define slot-set!
         (case-lambda
           ((rec v name value)
            (slot-set-using-class! rec v name value))
           ((obj name value)
            (orig-slot-set! obj name value)))))

      (else))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
