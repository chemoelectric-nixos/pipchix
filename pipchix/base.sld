;;; Copyright © 2025 Barry Schwartz
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

(define-library (pipchix base)
  (export make-nix-attributeset ;; FIXME: THIS STUFF SHOULD GO IN A LOW-LEVEL STUFF LIBRARY.
          make-recursive-nix-attributeset
          nix-attributeset?
          nix-attributeset-recursive?
          nix-attributeset-ref)
  (import (scheme base)
          (scheme case-lambda)
          (scheme hash-table)           ; SRFI-125
          (srfi 148)) ; Eager syntax-rules. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WHEN WILL THIS BE NEEDED?
  (begin

    (define-record-type <nix-attributeset>
      (%%make-nix-attributeset table recursive?)
      nix-attributeset?
      (table %%nix-attributeset-table)
      (recursive? nix-attributeset-recursive?))

    (define make-nix-attributeset
      (let ((table (make-hash-table equal?)))
        (case-lambda
          ((recursive?)
           (%%make-nix-attributeset table recursive?))
          (()
           (%%make-nix-attributeset table #f)))))

    (define (make-recursive-nix-attributeset)
      (make-nix-attributeset #t))

    (define (nix-attributeset-ref key)
      (hash-table-ref key))
    
    ))
