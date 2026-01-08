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
    SCHEME_ERROR("not a symbol", format))
  (unless (procedure? handler)
    SCHEME_ERROR("not a procedure", handler))
  (%%add-shell-print-format! format handler))

(define (%%add-shell-print-format! format handler)
  (set! %%shell-print-handlers
    (cons (cons format handler) %%shell-print-handlers)))

(define %%current-shell-print-format
  ;; 'base64 is much more compact than 'escaped, but would be slower.
  (make-parameter (box 'escaped)
                  (lambda (boxed-format)
                    (let ((format (unbox boxed-format)))
                      (unless (symbol? format)
                        SCHEME_ERROR("not a symbol", format))
                      (unless (assq format %%shell-print-handlers)
                        SCHEME_ERROR("not an available handler",
                                     format))
                      boxed-format))))

(define (current-shell-print-format)
  (unbox (%%current-shell-print-format)))

(define (with-shell-print-format format thunk)
  (parameterize ((%%current-shell-print-format (box format)))
    (thunk)))

(define (set-shell-print-format! format)
  (set-box! (%%current-shell-print-format) format))

(define shell-print
  (case-lambda
    ((str) (shell-print (current-shell-print-format) str))
    ((format str)
     (cond
      ((not format)
       ;; (shell-print #f str) is equivalent to (shell-print str)
       (shell-print str))
      (else
       (unless (symbol? format)
         SCHEME_ERROR("not a symbol", format))
       (unless (assq format %%shell-print-handlers)
         SCHEME_ERROR("not an available handler", format))
       (let ((handler-pair (assq format %%shell-print-handlers)))
         (let ((handler (cdr handler-pair)))
           (handler str))))))))

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
      (else
       SCHEME_ERROR("internal error in bytevector->escaped-8bit",
                    u))))
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
