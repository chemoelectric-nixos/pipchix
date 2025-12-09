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

m4_string_reverse_concatenate

(define the-nix-null%% '#(nix-null))    ; An arbitrary unique object.

(define nix-node-predicates '())

(define (register-nix-node-predicate pred)
  (unless (procedure? pred)
    (error "not a procedure" pred))
  (set! nix-node-predicates
    (cons pred nix-node-predicates))
  pred)

(define-record-factory <nix-embedded-node>
  (constructor> make-nix-embedded-node)
  (predicate> nix-embedded-node? register-nix-node-predicate)
  (getter> 1 nix-embedded-node-ref))

(define-record-factory <nix-data-node>
  ;; A node whose content is boolean, number, string, or
  ;; the-nix-null%%.
  (constructor> make-nix-data-node%%)
  (constructor>
   make-nix-data-node
   (lambda (construct)
     (lambda (data)
       (cond ((nix-null? data)
              ;; This is a convenience. It lets you treat nix-null the
              ;; same as a boolean, number, or string.
              (construct the-nix-null%%))
             ((or (eq? #f data)
                  (eq? #t data)
                  (eq? the-nix-null%% data)
                  (string? data)
                  (number? data))
              (construct data))
             (else (error "incorrect type" data))))))
  (predicate> nix-data-node? register-nix-node-predicate)
  (getter> 1 nix-data-node-ref))

(define nix-false (make-nix-data-node%% #f))
(define nix-true (make-nix-data-node%% #t))
(define nix-null (make-nix-data-node%% the-nix-null%%))

(define (nix-false? obj)
  (and (nix-data-node? obj)
       (eq? #f (nix-data-node-ref obj))))

(define (nix-true? obj)
  (and (nix-data-node? obj)
       (eq? #t (nix-data-node-ref obj))))

(define (nix-boolean? obj)
  (and (nix-data-node? obj)
       (let ((b (nix-data-node-ref obj)))
         (or (eq? b #f) (eq? b #t)))))

(define (nix-null? obj)
  (and (nix-data-node? obj)
       (eq? the-nix-null%% (nix-data-node-ref obj))))

(define-record-factory <nix-path-node>
  (constructor> make-nix-path-node)
  (predicate> nix-path-node? register-nix-node-predicate)
  (getter> 1 nix-path-node-ref))

(define-record-factory <nix-attributeset-or-letrec-node>
  (constructor>
   make-nix-attributeset-node
   (lambda (construct)
     (lambda (recursive?)
       (let ((kind (if recursive? 'nix-setrec 'nix-set)))
         (construct kind '())))))
  (constructor>
   make-nix-letrec-node
   (let ((filler
          ;; Fill the in-clause with something that cannot be
          ;; converted to a Nix object.
          (lambda () #f)))
     (lambda (construct)
       (lambda ()
         (construct 'nix-letrec '() filler)))))
  (predicate>
   nix-attributeset-node?
   (lambda (pred)
     (define-syntax kind%%%
       (syntax-rules ()
         ((_ obj)
          (nix-attributeset-or-letrec-node-kind%%% obj))))
     (let ((new-pred
            (lambda (obj)
               (and (pred obj)
                    (let ((kind (kind%%% obj)))
                      (or (eq? kind 'nix-set)
                          (eq? kind 'nix-setrec)))))))
       (register-nix-node-predicate new-pred))))
  (predicate>
   nix-letrec-node?
   (lambda (pred)
     (define-syntax kind%%%
       (syntax-rules ()
         ((_ obj)
          (nix-attributeset-or-letrec-node-kind%%% obj))))
     (let* ((new-pred
             (lambda (obj)
               (and (pred obj)
                    (eq? (kind%%% obj) 'nix-letrec)))))
       (register-nix-node-predicate new-pred))))
  (getter> 1 nix-attributeset-or-letrec-node-kind%%%)
  (getter> 1 nix-attributeset-node-recursive?
           (lambda (getter)
             (lambda (obj)
               (unless (nix-attributeset-node? obj)
                 (error "not a <nix-attributeset-node>" obj))
               (let ((kind (getter obj)))
                 (eq? kind 'nix-setrec)))))
  (getter> 1 nix-letrec-node-recursive?
           (lambda (getter)
             (lambda (obj)
               (unless (nix-letrec-node? obj)
                 (error "not a <nix-letrec-node>" obj))
               ;; This should always be true, because Nix has only the
               ;; recursive ‘let’.
               (let ((kind (getter obj)))
                 (eq? kind 'nix-letrec)))))
  (getter> 2 nix-attributeset-node-bag)
  (getter> 2 nix-letrec-node-bag)
  (setter> 2 set-nix-attributeset-node-bag!)
  (setter> 2 set-nix-letrec-node-bag!)
  (getter> 3 nix-letrec-node-in-clause)
  (setter> 3 set-nix-letrec-node-in-clause!))

(define (nix-attributeset-node-set! attrset elem)
  (let ((bag (nix-attributeset-node-bag attrset)))
    (set-nix-attributeset-node-bag! attrset (cons elem bag))))

(define (nix-attributeset-node-for-each proc attrset)
  (let ((bag (nix-attributeset-node-bag attrset)))
    (for-each proc (reverse bag))))

(define nix-letrec-node-set! nix-attributeset-node-set!)
(define nix-letrec-node-for-each nix-attributeset-node-for-each)

(define-record-factory <nix-attributepath-node>
  (constructor>
   list->nix-attributepath-node
   (let ((sym->str
          ;; Let symbols be used as if they were strings.
          (lambda (s) (if (symbol? s) (symbol->string s) s))))
     (lambda (construct)
       (lambda (names)
         (construct (map sym->str names))))))
  (predicate> nix-attributepath-node? register-nix-node-predicate)
  (getter> 1 nix-attributepath-node->list))

(define-record-factory <nix-attributebinding-node>
  (constructor> make-nix-attributebinding-node)
  (predicate> nix-attributebinding-node? register-nix-node-predicate)
  (getter> 1 nix-attributebinding-node-key)
  (getter> 2 nix-attributebinding-node-value))

(define-record-factory <nix-inherit-node>
  (constructor>
   list->nix-inherit-node
   (lambda (construct)
     (case-lambda
       ((lst) (construct lst #f))
       ((lst attrset) (construct lst attrset)))))
  (predicate> nix-inherit-node? register-nix-node-predicate)
  (getter> 1 nix-inherit-node->list)
  (getter> 2 nix-inherit-node-attributeset))

(define-record-factory <nix-list-node>
  (constructor> list->nix-list-node)
  (predicate> nix-list-node? register-nix-node-predicate)
  (getter> 1 nix-list-node->list))

(define (nix-abstract-syntax-tree? obj)
  (let loop ((p nix-node-predicates))
    (and (pair? p)
         (or ((car p) obj)
             (loop (cdr p))))))

(define (scheme->nix value)
  ;; Convert Scheme values to Nix AST.
  (cond ((or (string? value)
             (number? value)
             (boolean? value)
             (eq? the-nix-null%% value))
         (make-nix-data-node value))
        ((symbol? value)
         (make-nix-data-node (symbol->string value)))
        ((list? value)
         (list->nix-list-node (map scheme->nix value)))
        ((nix-abstract-syntax-tree? value)
         value)
        (else (error "incorrect type" value))))

(define output-nix-abstract-syntax-tree
  (let ((outp-default
         (lambda (str)
           (display str (current-output-port)))))
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
        ((nix-letrec-node? ast)
         (%%output-nix-letrec-node ast outp))
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
    (cond ((eq? the-nix-null%% data)
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
         (outp "(\"")
         (outp (%%escaped-string str))
         (outp "\")"))
        (else
         (outp "(\"")
         (outp str)
         (outp "\")"))))

(define (%%escaped-string str)
  (let ((lst '()))
    (string-for-each
     (lambda (c)
       (cond ((char=? c #\newline)
              (set! lst (cons "\\n" lst)))
             ((char=? c #\return)
              (set! lst (cons "\\r" lst)))
             ((char=? c #\tab)
              (set! lst (cons "\\t" lst)))
             ((char=? c #\$)
              (set! lst (cons "\\$" lst)))
             ((char=? c #\\)
              (set! lst (cons "\\\\" lst)))
             ((char=? c #\")
              (set! lst (cons "\\\"" lst)))
             ((char=? c #\')
              (set! lst (cons "\\'" lst)))
             (else
              ;; BEWARE: Nix cannot handle many strings that are legal
              ;;         in Scheme. THERE IS NO ATTEMPT HERE TO CHECK
              ;;         FOR LEGALITY IN NIX.
              (set! lst (cons (string c) lst)))))
     str)
    (%%string-reverse-concatenate lst)))

(define (%%string-needs-escaping? str)
  (%%string-contains? %%char-needs-escaping? str))

(define (%%char-needs-escaping? c)
  (or (char=? c #\$)
      (char=? c #\\)
      (char=? c #\")
      (char=? c #\')
      (char=? c #\newline)
      (char=? c #\return)
      (char=? c #\tab)))

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

(define (%%output-nix-letrec-node ast outp)
    (outp "(let\n")
    (nix-letrec-node-for-each
     output-nix-abstract-syntax-tree ast)
    (outp "in(\n")
    (output-nix-abstract-syntax-tree
     (nix-letrec-node-in-clause ast))
    (outp "))\n"))

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
               (outp (%%escaped-string name))
               (outp "\n"))
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
          (else
           (outp data)
           (outp "\n")))))

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

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
