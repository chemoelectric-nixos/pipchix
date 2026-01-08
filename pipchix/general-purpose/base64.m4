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

(define bytevector->base64
  ;; Return the BASE64 representation of a bytevector.
  (let* ((digits
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (look-up (lambda (i) (string-ref digits i))))
    (lambda (bv)
      (let* ((n (bytevector-length bv))
             (bvref (lambda (bv j)
                      (if (<= n j) 0 (bytevector-u8-ref bv j)))))
        (let-values (((quot rem) (truncate/ n 3)))
          (let* ((m^ (if (zero? rem) quot (+ quot 1)))
                 (m (* 4 m^))
                 (str (make-string m)))
            (let loop ((i 0))
              (unless (= i m^)
                (let* ((k (* i 4))
                       (j (* i 3))
                       (b0 (bvref bv j))
                       (b1 (bvref bv (+ j 1)))
                       (b2 (bvref bv (+ j 2))))
                  (let-values
                      (((b0x b0y) (truncate/ b0 4))
                       ((b1x b1y) (truncate/ b1 16))
                       ((b2x b2y) (truncate/ b2 64)))
                    (let ((u0 b0x)
                          (u1 (+ b1x (* 16 b0y)))
                          (u2 (+ b2x (* 4 b1y)))
                          (u3 b2y))
                      (string-set! str k (look-up u0))
                      (string-set! str (+ k 1) (look-up u1))
                      (string-set! str (+ k 2) (look-up u2))
                      (string-set! str (+ k 3) (look-up u3))
                      (loop (+ i 1)))))))
            (unless (zero? rem)
              (let ((q (* 4 quot)))
                (let loop ((i (+ q 1 rem)))
                  (unless (= i (+ q 4))
                    (string-set! str i #\=)
                    (loop (+ i 1))))))
            str))))))

(define (uinteger64->bytevector u)
  (unless (and (integer? u) (exact? u)
               (<= 0 u) (<= u 18446744073709551615))
    SCHEME_ERROR("expected a 64-bit non-negative exact integer", u))
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

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
