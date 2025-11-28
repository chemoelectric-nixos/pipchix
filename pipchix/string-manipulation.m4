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

(define bytevector->base64
  (let* ((digits
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (look-up (lambda (i) (string-ref digits i))))
    (lambda (bv)
      (let* ((n (bytevector-length bv))
             (bvref (lambda (bv j)
                      (if (<= n j) 0 (bytevector-u8-ref bv j)))))
        (let-values (((quot rem) (integer-division n 3)))
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
                      (((b0x b0y) (integer-division b0 4))
                       ((b1x b1y) (integer-division b1 16))
                       ((b2x b2y) (integer-division b2 64)))
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

(define (base64-utf8 str)
  (bytevector->base64 (string->utf8 str)))

(define (bytevector->printf-utf8 bv)
  (define (fragment u)
    (cond ((= u #x5C) (string-append "\\" "\\"))  ;; #\\
          ((= u #x25) (string-append "\\" "045")) ;; #\%
          ((= u #x22) (string-append "\\" "042")) ;; #\"
          ((= u #x27) (string-append "\\" "047")) ;; #\'
          ((= u #x07) (string-append "\\" "a"))
          ((= u #x08) (string-append "\\" "b"))
          ((= u #x09) (string-append "\\" "t"))
          ((= u #x0A) (string-append "\\" "n"))
          ((= u #x0B) (string-append "\\" "v"))
          ((= u #x0C) (string-append "\\" "f"))
          ((= u #x0D) (string-append "\\" "r"))
          ((<= #x7F u) (string-append
                        "\\" (number->string u 8)))
          ((<= u #x07) (string-append
                        "\\" "00" (number->string u 8)))
          ((<= u #x1F) (string-append
                        "\\" "0" (number->string u 8)))
          (else (string (integer->char u)))))
  (define (fragment-length u)
    (cond ((= u #x5C) 2)
          ((= u #x25) 4)
          ((= u #x22) 4)
          ((= u #x27) 4)
          ((= u #x07) 2)
          ((= u #x08) 2)
          ((= u #x09) 2)
          ((= u #x0A) 2)
          ((= u #x0B) 2)
          ((= u #x0C) 2)
          ((= u #x0D) 2)
          ((<= #x7F u) 4)
          ((<= u #x07) 4)
          ((<= u #x1F) 4)
          (else 1)))
  (define (find-string-length bv)
    (let ((m (bytevector-length bv)))
      (let loop ((i 0)
                 (n 0))
        (cond ((= i m) n)
              (else
               (loop (+ i 1)
                     (+ n (fragment-length
                           (bytevector-u8-ref bv i)))))))))
  (let* ((m (bytevector-length bv))
         (n (find-string-length bv))
         (str (make-string n)))
    (let loop ((i 0)
               (j 0))
      (unless (= i m)
        (let ((frag (fragment (bytevector-u8-ref bv i))))
          (string-copy! str j frag)
          (loop (+ i 1) (+ j (string-length frag))))))
    str))

(define (printf-utf8 str)
  (bytevector->printf-utf8 (string->utf8 str)))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
