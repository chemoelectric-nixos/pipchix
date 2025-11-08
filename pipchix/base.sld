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
  (export )
  (import (scheme base)
          (pipchix abstract-syntax-tree)
          (srfi 148)) ; Eager syntax-rules. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WHEN WILL THIS BE NEEDED?
  (begin

    (define lst
      (list (make-nix-data-node "${foobar}")
            (make-nix-data-node "Emojipedia — 😃 Home of Emoji Meanings 💁👌🎍😍")
            (make-nix-data-node #t)
            (make-nix-data-node #f)
            (make-nix-data-node '())
            (make-nix-data-node 1234)
            (make-nix-data-node -1234)
            (make-nix-data-node 123.4)))
    (define ast1 (list->nix-list-node lst))
    (display-nix-abstract-syntax-tree ast1)

    ))
