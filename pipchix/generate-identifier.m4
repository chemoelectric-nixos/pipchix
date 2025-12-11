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

(define (sanitary? c)
  ;; Leave out ' characters. They are legal in Nix identifiers, but
  ;; are not copacetic with Scheme, where they are shorthand for the
  ;; (quote ...) form.
  (and (char<? c #\x7F)
       (or (char-alphabetic? c)
           (char-numeric? c)
           (char=? c #\-)
           (char=? c #\_))))

(define (sanitize-identifier id)
  (let* ((lst (string->list id))
         (lst (map (lambda (c) (if (sanitary? c) c #\_))
                   lst))
         (id (list->string lst)))
    id))

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

        (seed 602214076))               ; Avogadro’s number (scaled).
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

(define (uinteger64->bytevector u)
  (let ((bv (make-bytevector 8)))
    (let loop ((i 0)
               (q u))
      (unless (= i 8)
        (let-values (((q r) (truncate/ q 256)))
          (bytevector-u8-set! bv i r)
          (loop (+ i 1) q))))
    bv))

(define (uinteger64->base64 u)
  (bytevector->base64 (uinteger64->bytevector u)))

(define (random-partial-identifier--general)
  (uinteger64->base64 (random-number-generator)))

(define (random-partial-identifier--linux)
  (with-input-from-file "/proc/sys/kernel/random/uuid"
    (lambda () (read-string 36))))

(define (random-partial-identifier)
  ;;
  ;; Choose an identifier generator and install it as a new
  ;; ‘random-partial-identifier’ procedure.
  ;;
  (let ((cc (current-continuation)))
    (if (continuation? cc)
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
              (random-partial-identifier--linux))))
        cc)))

(define (full-identifier i extra)
  (let ((extra (if (symbol? extra) (symbol->string extra) extra)))
    (sanitize-identifier
     (string-append "g" (if (string=? extra "")
                            extra
                            (string-append "-" extra "-"))
                    (random-partial-identifier)
                    (number->string i)))))

(define generate-identifier
  ;;
  ;; Generate a Nix-compatible identifier that is difficult to
  ;; accidentally duplicate, that will be unique to a run of Pipchix,
  ;; and ideally which will be unique for all time.
  ;;
  ;; This is a cheap substitute for gensym.
  ;;
  ;; One can ensure uniqueness in a single-threaded run by using a
  ;; counter along with some large random string.
  ;;
  ;; Probable uniqueness for all time is done by using a UUID for the
  ;; random string. On Linux a UUID can be gotten by reading a line
  ;; from /proc/sys/kernel/random/uuid.
  ;;
  (let ((i 0))
    (case-lambda
      (() (generate-identifier ""))
      ((extra)
       (set! i (+ i 1))
       (full-identifier i extra)))))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'continuation-return 'scheme-indent-function 1)
;;; end:
