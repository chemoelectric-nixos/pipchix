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

(define (random-identifier)
  ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME: For Linux,
  ;; this is good. For non-Linux, we need something else.
  (with-input-from-file "/proc/sys/kernel/random/uuid"
    (lambda () (read-string 36))))

(define (full-identifier i extra)
  (let ((extra (if (symbol? extra) (symbol->string extra) extra)))
    (sanitize-identifier
     (string-append "g" (if (string=? extra "")
                            extra
                            (string-append "-" extra "-"))
                    (random-identifier)
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
;;; end:
