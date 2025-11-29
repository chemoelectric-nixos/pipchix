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

(define %%shell-print-handlers
  (list (cons 'base64 (lambda (str)
                        (string-append
                         "if :; then printf '"
                         (base64-utf8 str)
                         "' | base64 -d -; fi")))
        (cons 'escaped (lambda (str)
                         (string-append
                          "printf '"
                          (escaped-utf8 str)
                          "'")))))

(define (add-shell-print-format! format handler)
  ;; There is nothing here to ensure the handler is actually a
  ;; handler. We assume the programmer knows what they are doing.
  (unless (symbol? format)
    (error "not a symbol" format))
  (unless (procedure? handler)
    (error "not a procedure" handler))
  (%%add-shell-print-format! format handler))

(define (%%add-shell-print-format! format handler)
  (set! %%shell-print-handlers
    (cons (cons format handler) %%shell-print-handlers)))

(define current-shell-print-format
  ;; 'base64 is much more compact than 'escaped, but would be slower.
  (make-parameter 'escaped
                  (lambda (format)
                    (unless (symbol? format)
                      (error "not a symbol" format))
                    (unless (assq format %%shell-print-handlers)
                      (error "not an available handler" format))
                    format)))

(define (with-shell-print-format format thunk)
  (parameterize ((current-shell-print-format format))
    (thunk)))

(define shell-print
  (case-lambda
    ((str) (shell-print (current-shell-print-format) str))
    ((format str)
     (unless (symbol? format)
       (error "not a symbol" format))
     (unless (assq format %%shell-print-handlers)
       (error "not an available handler" format))
     (let ((handler-pair (assq format %%shell-print-handlers)))
       (let ((handler (cdr handler-pair)))
         (handler str))))))

(define bytevector->base64
  ;; Return the BASE64 representation of a bytevector.
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
  ;; Return the BASE64 representation of the UTF-8 encoding of a
  ;; string.
  (bytevector->base64 (string->utf8 str)))

(define (bytevector->escaped-8bit bv)
  ;; Assume a bytevector holds an 8-bit encoded string. Return a
  ;; string suitable for printing with printf(1) or echo(1).
  ;;
  ;; This implemementation uses only backslashes and ASCII
  ;; alphanumerics.
  (define (fragment u)
    (cond ((%%is-alphanumeric-ascii? u) (string (integer->char u)))
          ((and (<= #o007 u) (<= u #o015)) (control-fragment u))
          (else (string-append "\\" (%%three-digit-octal u)))))
  (define (control-fragment u)
    (case u
      ((#o007) "\\a")
      ((#o010) "\\b")
      ((#o011) "\\t")
      ((#o012) "\\n")
      ((#o013) "\\v")
      ((#o014) "\\f")
      ((#o015) "\\r")
      (else (error "internal error in bytevector->escaped-8bit" u))))
  (define (fragment-length u)
    (cond ((%%is-alphanumeric-ascii? u) 1)
          ((and (<= #o007 u) (<= u #o015)) 2)
          (else 4)))
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

(define (escaped-utf8 str)
  ;; Encode the string in UTF-8. Return a string representing that
  ;; encoding, and suitable for printing with printf(1) or echo(1).
  (bytevector->escaped-8bit (string->utf8 str)))

(define (%%is-alphanumeric-ascii? u)
  (or (and (<= #o101 u) (<= u #o132))   ;; A-Z
      (and (<= #o141 u) (<= u #o172))   ;; a-z
      (and (<= #o060 u) (<= u #o071)))) ;; 0-9

(define (%%three-digit-octal u)
  (let ((s (number->string u 8)))
    (cond ((< u #o010) (string-append "00" s))
          ((< u #o100) (string-append "0" s))
          (else s))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
