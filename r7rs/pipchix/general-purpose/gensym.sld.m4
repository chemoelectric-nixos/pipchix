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

(define-library (pipchix general-purpose gensym)

  (export gensym generate-temporaries)
  ;; m4_ifelse(CHICKEN_5,«yes»,,«
  (cond-expand
    (gauche (export :info-alist))
    (else))
  ;; »)

  (import basic_libraries)
  ;; m4_ifelse(CHICKEN_5,«yes»,«
  (import (only (chicken base) gensym))
  ;; »,«
  (cond-expand
    (chibi (import (scheme file))
           (import (pipchix general-purpose base64))
           (import (pipchix general-purpose continuation-capture)))
    (gauche (import (only (r7rs aux) :info-alist gensym)))
    (sagittarius (import (only (sagittarius) gensym)))
    ((or loko guile) (import (rnrs syntax-case (6))))
    (else))
  ;; »)

  (begin

    ;; m4_ifelse(CHICKEN_5,«yes»,«
    (define (generate-temporaries lst)
      (map (lambda (x) (gensym)) lst))
    ;; »,«
    (cond-expand
      ((or loko guile)
       (define gensym
         (case-lambda
           (() (car (generate-temporaries '(1))))
           ((ignored) (gensym)))))

      (chibi
       (begin

         (define random-number-generator
           ;; A linear congruential generator.
           (let (
                 ;; The multiplier lcg_a comes from Steele, Guy; Vigna,
                 ;; Sebastiano (28 September 2021). ‘Computationally easy,
                 ;; spectrally good multipliers for congruential pseudorandom
                 ;; number generators’. arXiv:2001.05304v3 [cs.DS]

                 (lcg_a #xF1357AEA2E62A9C5)

                 ;; The value of lcg_c is not critical, but should be odd.

                 (lcg_c 1)

                 ;; A Pell-Lucas number. See https://oeis.org/A001333
                 (seed 367296043199))

             (lambda ()
               ;; Usually I would use only the high 48 bits of the seed for the
               ;; random number, but in this case it does not matter very much.
               ;; I will just update the seed (by arithmetic modulo 2**64) and
               ;; use the result as the random value.
               (let* ((x (* lcg_a seed))
                      (x (+ x lcg_c))
                      (x (truncate-remainder x 18446744073709551616)))
                 (set! seed x)
                 x))))

         (define (random-partial-identifier--general)
           ;; Use a random number in base64.
           (uinteger64->base64 (random-number-generator)))

         (define (random-partial-identifier--linux)
           ;; Use a UUID.
           (with-input-from-file "/proc/sys/kernel/random/uuid"
             (lambda () (read-string 36))))

         (define (random-partial-identifier)
           ;;
           ;; Choose an identifier generator and install it as a new
           ;; ‘random-partial-identifier’ procedure.
           ;;
           (continuation-capture
            (lambda (cc)
              (with-exception-handler
                  (lambda (c)
                    (set! random-partial-identifier
                      random-partial-identifier--general)
                    (continuation-return cc
                      (random-partial-identifier--general)))
                (lambda ()
                  (set! random-partial-identifier
                    random-partial-identifier--linux)
                  (continuation-return cc
                    (random-partial-identifier--linux)))))))

         (define gensym
           (let ((i -1)
                 (nonbreaking-space (string #\x00A0)))
             (case-lambda
               (() (gensym ""))
               ((ignored)
                (set! i (+ i 1))
                (string->symbol (string-append
                                 "g"
                                 nonbreaking-space
                                 (random-partial-identifier)
                                 nonbreaking-space
                                 (number->string i))))))) ))

      (else))

    (cond-expand
      ((or loko guile))
      (else
       (define (generate-temporaries lst)
         (map (lambda (x) (gensym)) lst))))
    ;; »)

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
