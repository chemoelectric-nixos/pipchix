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
  ;; m4_ifelse(CHICKEN_5,«yes»,«
  (export match-set!-ineffectively
          match-bound-identifier-memv
          match-bound-identifier=?
          match-check-identifier
          match-check-ellipsis
          match-check-underscore
          match-cons
          match-rewrite2
          match-rewrite
          match-letrec-two-step
          match-letrec-two
          match-letrec-one
          match-named-let
          match-let/aux
          match-extract-quasiquote-vars-step
          match-extract-quasiquote-vars
          match-extract-vars-step
          match-extract-vars
          match-record-named-refs
          match-record-refs
          match-vector-tail-two
          match-vector-tail
          match-gen-vector-ellipsis
          match-vector-step
          match-vector-two
          match-vector
          match-gen-search
          match-verify-no-ellipsis
          match-gen-ellipsis/range
          match-gen-ellipsis/qq
          match-gen-ellipsis
          match-gen-or-step
          match-gen-or
          match-drop-first-arg
          match-tuck-ids
          match-drop-ids
          match-quasiquote-step
          match-quasiquote
          match-two
          match-one
          match-next
          match-syntax-error
          chicken-scheme-keyword?)
  ;; »)
  ;; m4_ifelse(CHICKEN_5,«yes»,,«
  (cond-expand
    (gauche (export :info-alist))
    (else))
  ;; »)

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
    ((or loko guile) (import (rnrs syntax-case (6))))
    (else))

  (begin

    (cond-expand
      (chicken-5
       ;; m4_pushdef(«general_macros»,«er-macro-transformer»)
       (import-for-syntax (scheme base))
       (import-for-syntax (srfi 1))
       m4_include(pipchix/general-purpose/match.m4)
       ;; m4_popdef(«general_macros»)
       )
      ((or loko guile)
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
