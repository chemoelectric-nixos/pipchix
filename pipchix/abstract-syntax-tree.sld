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

(define-library (pipchix abstract-syntax-tree)

  (export make-nix-data-node
          nix-data-node?
          nix-data-node-ref

          make-nix-path-node
          nix-path-node?
          nix-path-node-ref

          make-nix-attributeset-node
          make-recursive-nix-attributeset-node
          nix-attributeset-node?
          nix-attributeset-node-recursive?
          nix-attributeset-node-set!
          nix-attributeset-node-ref

          list->nix-list-node
          nix-list-node->list
          nix-list-node?

          display-nix-abstract-syntax-tree)

  (import (scheme base)
          (scheme bitwise)
          (scheme case-lambda)
          (scheme char)
          (scheme hash-table)           ; SRFI-125
          (scheme write)
          (only (scheme list)
                reverse!)
          (only (srfi 13)
                string-concatenate))

  (begin

    (define-record-type <nix-data-node> ; Boolean, number, string, null.
      (make-nix-data-node data) ; (A null list represents a Nix null.)
      nix-data-node?
      (data nix-data-node-ref))

    (define-record-type <nix-path-node>
      (make-nix-path-node path)
      nix-path-node?
      (path nix-path-node-ref))

    (define-record-type <nix-attributeset-node>
      (%%make-nix-attributeset-node table recursive?)
      nix-attributeset-node?
      (table %%nix-attributeset-node-table)
      (recursive? nix-attributeset-node-recursive?))

    (define make-nix-attributeset-node
      (let ((table (make-hash-table equal?)))
        (case-lambda
          ((recursive?)
           (%%make-nix-attributeset-node table recursive?))
          (()
           (%%make-nix-attributeset-node table #f)))))

    (define (make-recursive-nix-attributeset-node)
      (make-nix-attributeset-node #t))

    (define (nix-attributeset-node-set! attrset key value)
      (let ((table (%%nix-attributeset-node-table attrset)))
        (hash-table-set! table key value)))

    (define (nix-attributeset-node-ref attrset key)
      (let ((table (%%nix-attributeset-node-table attrset)))
        (hash-table-ref table key)))
    
    (define-record-type <nix-list-node>
      (list->nix-list-node lst)
      nix-list-node?
      (lst nix-list-node->list))

    (define display-nix-abstract-syntax-tree
      (case-lambda
        ((ast) (display-nix-abstract-syntax-tree
                ast (current-output-port)))
        ((ast port)
         (cond
          ((nix-data-node? ast) (%%display-nix-data-node ast port))
          ((nix-list-node? ast) (%%display-nix-list-node ast port))
          (else (error "not an abstract syntax tree" ast))))))

    (define (%%display-nix-data-node ast port)
      (let ((data (nix-data-node-ref ast)))
        (cond
         ((null? data)
          (display "null\n" port))
         ((eq? #f data)
          (display "false\n" port))
         ((eq? #t data)
          (display "true\n" port))
         ((number? data)
          (display "(" port)
          (display data port)
          (display ")\n" port))
         ((string? data)
          (display "(builtins.fromJSON ''\"" port)
          (display (%%json-escape-string data) port)
          (display "\"'')\n" port))
         (else (error "bad <nix-data-node>" ast)))))

    (define (%%json-escape-string str)
      (let ((lst '()))
        (string-for-each
         (lambda (c)
           (let ((i (char->integer c)))
             (if (or (char=? c #\$)
                     (char=? c #\\)
                     (char=? c #\")
                     (char=? c #\')
                     (< i 32)
                     (> i 126))
                 (set! lst (cons (%%utf16-escape i) lst))
                 (set! lst (cons (string c) lst)))))
         str)
        (string-concatenate (reverse! lst))))

    (define (%%utf16-escape i)
      (if (<= i #xFFFF)
          (let* ((hex (string-upcase (number->string i 16)))
                 (str (string-copy "\\u0000"))
                 (n (string-length hex)))
            (string-copy! str (- 6 n) hex)
            str)
          (let* ((i (- i #x10000))      ; Make a surrogate pair.
                 (hi (+ #xDB00
                        (arithmetic-shift
                         (bitwise-and i #b11111111110000000000)
                         -10)))
                 (lo (+ #xDC00 (bitwise-and i #b1111111111))))
            (string-append (%%utf16-escape hi)
                           (%%utf16-escape lo)))))

    (define (%%display-nix-list-node ast port)
      (let ((lst (nix-list-node->list ast))
            (display-elem (lambda (elem)
                            (display-nix-abstract-syntax-tree
                             elem port))))
        (display "[\n" port)
        (for-each display-elem lst)
        (display "]\n" port)))

    ))
