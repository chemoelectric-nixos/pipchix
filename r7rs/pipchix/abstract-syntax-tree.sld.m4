;;;
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
;;;
m4_include(pipchix-includes.m4)

(define-library (pipchix abstract-syntax-tree)

  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme char))
  (import (scheme write))

  (cond-expand
    (chicken-5
     (import (utf8))
     (import (utf8-srfi-13)))
    (else))

  (cond-expand
    ((and guile r7rs) ;; ‘guile --r7rs’
     (import (only (srfi 60) bitwise-and arithmetic-shift)))
    (else ;; (srfi 151) = (scheme bitwise)
     (import (only (srfi 151) bitwise-and arithmetic-shift))))

  (export make-nix-embedded-node)
  (export nix-embedded-node?)

  (export make-nix-data-node)
  (export nix-data-node?)

  (export make-nix-path-node)
  (export nix-path-node?)

  (export make-nix-attributeset-node)
  (export nix-attributeset-node?)
  (export nix-attributeset-node-recursive?)
  (export nix-attributeset-node-set!)

  (export list->nix-attributepath-node)
  (export nix-attributepath-node?)

  (export make-nix-attributebinding-node)
  (export nix-attributebinding-node?)

  (export list->nix-inherit-node)
  (export nix-inherit-node?)

  (export list->nix-list-node)
  (export nix-list-node?)

  (export scheme->nix)

  (export output-nix-abstract-syntax-tree)

  (begin

    m4_string_reverse_concatenate

    (define-record-type <nix-embedded-node> ; Embedded Nix code.
      (make-nix-embedded-node code)
      nix-embedded-node?
      (code nix-embedded-node-ref))

    (define-record-type <nix-data-node> ; Boolean, number, string, null.
      (make-nix-data-node data) ; (A null list represents a Nix null.)
      nix-data-node?
      (data nix-data-node-ref))

    (define-record-type <nix-path-node>
      (make-nix-path-node path)
      nix-path-node?
      (path nix-path-node-ref))

    (define-record-type <nix-attributeset-node>
      (%%make-nix-attributeset-node recursive? bag)
      nix-attributeset-node?
      (recursive? nix-attributeset-node-recursive?)
      (bag nix-attributeset-node-bag
           set-nix-attributeset-node-bag!))

    (define (make-nix-attributeset-node recursive?)
      (%%make-nix-attributeset-node recursive? '()))

    (define (nix-attributeset-node-set! attrset elem)
      (let ((bag (nix-attributeset-node-bag attrset)))
        (set-nix-attributeset-node-bag! attrset (cons elem bag))))

    (define (nix-attributeset-node-for-each proc attrset)
      (let ((bag (nix-attributeset-node-bag attrset)))
        (for-each proc (reverse bag))))

    (define-record-type <nix-attributepath-node>
      (%%list->nix-attributepath-node names)
      nix-attributepath-node?
      (names nix-attributepath-node->list))

    (define list->nix-attributepath-node
      ;; Let symbols be used as if they were strings.
      (let ((proc
             (lambda (s) (if (symbol? s) (symbol->string s) s))))
        (lambda (names)
          (%%list->nix-attributepath-node (map proc names)))))

    (define-record-type <nix-attributebinding-node>
      (make-nix-attributebinding-node key value)
      nix-attributebinding-node?
      (key nix-attributebinding-node-key)
      (value nix-attributebinding-node-value))

    (define-record-type <nix-inherit-node>
      (%%list->nix-inherit-node lst attrset)
      nix-inherit-node?
      (lst nix-inherit-node->list)
      (attrset nix-inherit-node-attributeset))

    (define list->nix-inherit-node
      (case-lambda
        ((lst)
         (%%list->nix-inherit-node lst #f))
        ((lst attrset)
         (%%list->nix-inherit-node lst attrset))))

    (define-record-type <nix-list-node>
      (list->nix-list-node lst)
      nix-list-node?
      (lst nix-list-node->list))

    (define (scheme->nix value)
      ;; Convert Scheme values to Nix AST.
      (cond ((or (string? value)
                 (number? value)
                 (boolean? value)
                 (eq? '() value))
             (make-nix-data-node value))
            ((symbol? value)
             (make-nix-data-node (symbol->string value)))
            ((list? value)
             (list->nix-list-node (map scheme->nix value)))
            (else
             ;; FIXME FIXME FIXME: Recognize Nix AST nodes and let
             ;; them through. Otherwise treat as an error.
             value)))

    (define output-nix-abstract-syntax-tree
      (let ((outp-default
             (lambda (str)
               (write-string str (current-output-port)))))
        (case-lambda
          ((ast)
           (output-nix-abstract-syntax-tree ast outp-default))
          ((ast outp)
           (cond
            ((nix-embedded-node? ast)
             (%%output-nix-embedded-node ast outp))
            ((nix-data-node? ast)
             (%%output-nix-data-node ast outp))
            ((nix-path-node? ast)
             (%%output-nix-path-node ast outp))
            ((nix-attributeset-node? ast)
             (%%output-nix-attributeset-node ast outp))
            ((nix-attributepath-node? ast)
             (%%output-nix-attributepath-node ast outp))
            ((nix-attributebinding-node? ast)
             (%%output-nix-attributebinding-node ast outp))
            ((nix-inherit-node? ast)
             (%%output-nix-inherit-node ast outp))
            ((nix-list-node? ast)
             (%%output-nix-list-node ast outp))
            (else (error "not an abstract syntax tree" ast)))))))

    (define (%%output-nix-embedded-node ast outp)
      (let ((code (nix-embedded-node-ref ast)))
        (outp code)))

    (define (%%output-nix-data-node ast outp)
      (let ((data (nix-data-node-ref ast)))
        (cond ((null? data)
               (outp "(builtins.null)\n"))
              ((eq? #f data)
               (outp "(builtins.false)\n"))
              ((eq? #t data)
               (outp "(builtins.true)\n"))
              ((number? data)
               (outp "(")
               (outp (number->string data))
               (outp ")\n"))
              ((string? data)
               (%%output-string data outp)
               (outp "\n"))
              (else (error "bad <nix-data-node>" ast)))))

    (define (%%output-string str outp)
      (cond ((%%string-needs-escaping? str)
             (outp "(builtins.fromJSON ''\"")
             (outp (%%json-escape-string str))
             (outp "\"'')"))
            (else
             (outp "(\"")
             (outp str)
             (outp "\")"))))

    (define (%%json-escape-string str)
      (let ((lst '()))
        (string-for-each
         (lambda (c)
           (if (%%char-needs-escaping? c)
               (let ((i (char->integer c)))
                 (set! lst (cons (%%utf16-escape i) lst)))
               (set! lst (cons (string c) lst))))
         str)
        (%%string-reverse-concatenate lst)))

    (define (%%string-needs-escaping? str)
      (%%string-contains? %%char-needs-escaping? str))

    (define (%%char-needs-escaping? c)
      (or (char=? c #\$)
          (char=? c #\\)
          (char=? c #\")
          (char=? c #\')
          (let ((i (char->integer c)))
            (or (< i 32)
                (> i 126)))))

    (define (%%utf16-escape i)
      (if (<= i #xFFFF)
          (let* ((hex (string-upcase (number->string i 16)))
                 (str (string-copy "\\u0000"))
                 (n (string-length hex)))
            (string-copy! str (- 6 n) hex)
            str)
          (let* ((i (- i #x10000))      ; Make a surrogate pair.
                 (hi (+ #xDB00
                        (bitwise-and (arithmetic-shift i -10)
                                     #b1111111111)))
                 (lo (+ #xDC00 (bitwise-and i #b1111111111))))
            (string-append (%%utf16-escape hi)
                           (%%utf16-escape lo)))))


    (define (%%output-nix-path-node ast outp)
      (let ((path (nix-path-node-ref ast)))
        (outp "( ")
        (unless (%%string-contains-slash? path) (outp "./"))
        (outp path)
        (outp " )\n")))

    (define (%%output-nix-attributeset-node ast outp)
      (let ((recursive? (nix-attributeset-node-recursive? ast)))
        (outp "(")
        (when recursive? (outp "rec"))
        (outp "{\n")
        (nix-attributeset-node-for-each
         output-nix-abstract-syntax-tree ast)
        (outp "})\n")))

    (define (%%output-nix-attributebinding-node ast outp)
      (let ((key (nix-attributebinding-node-key ast))
            (value (nix-attributebinding-node-value ast)))
        (output-nix-abstract-syntax-tree key outp)
        (outp "=\n")
        (output-nix-abstract-syntax-tree value outp)
        (outp ";\n")))

    (define (%%output-nix-attributepath-node ast outp)
      (let loop ((lst (nix-attributepath-node->list ast))
                 (needs-separator? #f))
        (when (pair? lst)
          (let ((name (car lst)))
            (when needs-separator? (outp ".\n"))
            (cond ((not (string? name))
                   (outp "\"${")
                   (output-nix-abstract-syntax-tree name outp)
                   (outp "}\"\n"))
                  ((%%string-is-nix-identifier? name)
                   (outp name)
                   (outp "\n"))
                  ((%%string-needs-escaping? name)
                   (outp "\"${builtins.fromJSON ''\"")
                   (outp (%%json-escape-string name))
                   (outp "\"''}\"\n"))
                  (else
                   (outp "\"")
                   (outp name)
                   (outp "\"\n")))
            (loop (cdr lst) #t)))))

    (define (%%output-nix-inherit-node ast outp)
      (let ((attrset (nix-inherit-node-attributeset ast))
            (lst (nix-inherit-node->list ast)))
        (outp "inherit\n")
        (when attrset
          (outp "(\n")
          (cond ((nix-attributeset-node? attrset)
                 (output-nix-abstract-syntax-tree attrset outp))
                (else
                 (%%output-nix-identifier attrset outp)))
          (outp ")\n"))
        (let loop ((lst lst))
          (when (pair? lst)
            (%%output-nix-identifier (car lst) outp)
            (loop (cdr lst))))
        (outp ";\n")))

    (define (%%output-nix-identifier ast outp)
      (let ((data (nix-data-node-ref ast)))
        (cond ((symbol? data)
               (%%output-nix-identifier
                (make-nix-data-node (symbol->string data)) outp))
              ((not (string? data))
               (error "not a symbol or string" data))
              ((not (%%string-is-nix-identifier? data))
               (error "not a Nix identifier" data))
              (else (outp data) (outp "\n")))))

    (define (%%output-nix-list-node ast outp)
      (let ((lst (nix-list-node->list ast))
            (output-elem (lambda (elem)
                           (output-nix-abstract-syntax-tree
                            elem outp))))
        (outp "[\n")
        (for-each output-elem lst)
        (outp "]\n")))

    (define (%%string-contains-slash? str)
      (%%string-contains? (lambda (c) (char=? c #\/))
                          str))

    (define (%%string-contains? pred str)
      (let ((n (string-length str)))
        (let loop ((i 0))
          (cond ((= i n) #f)
                ((pred (string-ref str i)) #t)
                (else (loop (+ i 1)))))))

    (define (%%string-is-nix-identifier? str)
      (let ((n (string-length str)))
        (if (zero? n)
            #f
            (if (not (%%char-is-nix-identifier-start?
                      (string-ref str 0)))
                #f
                (let loop ((i 1))
                  (cond ((= i n) #t)
                        ((%%char-is-nix-identifier-rest?
                          (string-ref str i))
                         (loop (+ i 1)))
                        (else #f)))))))

    (define (%%char-is-nix-identifier-start? c)
      (or (char=? c #\_)
          (and (<= (char->integer c) #x7F)
               (char-alphabetic? c))))

    (define (%%char-is-nix-identifier-rest? c)
      (or (char=? c #\_)
          (char=? c #\')
          (char=? c #\-)
          (and (<= (char->integer c) #x7F)
               (or (char-alphabetic? c)
                   (char-numeric? c)))))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
