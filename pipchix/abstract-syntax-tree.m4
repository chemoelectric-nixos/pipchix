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

define_string_reverse_concatenate

(define the-nix-null%% '#(nix-null))    ; An arbitrary unique object.
(define the-...-%% '#(the-...))     ; Another arbitrary unique object.

(define nix-node-predicates '())

(define (register-nix-node-predicate pred)
  (unless (procedure? pred)
    (err "not a procedure" pred))
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
             (else (err "incorrect type" data))))))
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
                 (err "not a <nix-attributeset-node>" obj))
               (let ((kind (getter obj)))
                 (eq? kind 'nix-setrec)))))
  (getter> 1 nix-letrec-node-recursive?
           (lambda (getter)
             (lambda (obj)
               (unless (nix-letrec-node? obj)
                 (err "not a <nix-letrec-node>" obj))
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

(define-record-factory <nix-list-node>
  (constructor> list->nix-list-node)
  (predicate> nix-list-node? register-nix-node-predicate)
  (getter> 1 nix-list-node->list))

(define-record-factory <nix-get-node>
  (constructor>
   make-nix-get-node
   (lambda (construct)
     (lambda (attrset attrpath)
       (let ((attrset (and attrset (scheme->nix attrset)))
             (attrpath (cond
                        ((nix-attributepath-node? attrpath)
                         attrpath)
                        ((pair? attrpath)
                         (list->nix-attributepath-node attrpath))
                        (else
                         (list->nix-attributepath-node
                          (list attrpath))))))
         (when attrset
           (%%check-attrset attrset))
         (construct attrset attrpath)))))
  (predicate> nix-get-node? register-nix-node-predicate)
  (getter> 1 nix-get-node-attributeset)
  (getter> 2 nix-get-node-attributepath))

(define-record-factory <nix-has?-node>
  (constructor>
   make-nix-has?-node
   (lambda (construct)
     (lambda (attrset attrpath)
       (let ((attrset (and attrset (scheme->nix attrset)))
             (attrpath (cond
                        ((nix-attributepath-node? attrpath)
                         attrpath)
                        ((pair? attrpath)
                         (list->nix-attributepath-node attrpath))
                        (else
                         (list->nix-attributepath-node
                          (list attrpath))))))
         (%%check-attrset attrset)
         (construct attrset attrpath)))))         
  (predicate> nix-has?-node? register-nix-node-predicate)
  (getter> 1 nix-has?-node-attributeset)
  (getter> 2 nix-has?-node-attributepath))

(define-record-factory <nix-//-node>
  (constructor>
   make-nix-//-node
   (lambda (construct)
     (lambda (attrset1 attrset2)
       (let ((attrset1 (and attrset1 (scheme->nix attrset1)))
             (attrset2 (and attrset2 (scheme->nix attrset2))))
         (unless (nix-//-node? attrset1)
           (%%check-attrset attrset1))
         (unless (nix-//-node? attrset2)
           (%%check-attrset attrset2))
         (construct attrset1 attrset2)))))
  (predicate> nix-//-node? register-nix-node-predicate)
  (getter> 1 nix-//-node-attributeset1)
  (getter> 2 nix-//-node-attributeset2))

(define-record-factory <nix-with-node>
  (constructor>
   make-nix-with-node
   (lambda (construct)
     (lambda (attrset clause)
       (let ((attrset (and attrset (scheme->nix attrset))))
         (%%check-attrset attrset)
         (construct attrset (scheme->nix clause))))))
  (predicate> nix-with-node? register-nix-node-predicate)
  (getter> 1 nix-with-node-attributeset)
  (getter> 2 nix-with-node-clause))

(define-record-factory <nix-unaryoperator-node>
  (constructor>
   make-nix-unaryoperator-node
   (lambda (construct)
     (lambda (op arg)
       (construct (if (symbol? op) (symbol->string op) op)
                  (scheme->nix arg)))))
  (predicate> nix-unaryoperator-node? register-nix-node-predicate)
  (getter> 1 nix-unaryoperator-node-op)
  (getter> 2 nix-unaryoperator-node-arg))

(define-record-factory <nix-binaryoperator-node>
  (constructor>
   make-nix-binaryoperator-node
   (lambda (construct)
     (lambda (op arg1 arg2)
       (construct (if (symbol? op) (symbol->string op) op)
                  (scheme->nix arg1)
                  (scheme->nix arg2)))))
  (predicate> nix-binaryoperator-node? register-nix-node-predicate)
  (getter> 1 nix-binaryoperator-node-op)
  (getter> 2 nix-binaryoperator-node-arg1)
  (getter> 3 nix-binaryoperator-node-arg2))

(define-record-factory <nix-ifthenelse-node>
  (constructor>
   make-nix-ifthenelse-node
   (lambda (construct)
     (lambda (if-clause then-clause else-clause)
       (construct (scheme->nix if-clause)
                  (scheme->nix then-clause)
                  (scheme->nix else-clause)))))
  (predicate> nix-ifthenelse-node? register-nix-node-predicate)
  (getter> 1 nix-ifthenelse-node-if-clause)
  (getter> 2 nix-ifthenelse-node-then-clause)
  (getter> 3 nix-ifthenelse-node-else-clause))

(define-record-factory <nix-lambda-node>
  (constructor> make-nix-lambda-node)
  (predicate> nix-lambda-node? register-nix-node-predicate)
  (getter> 1 nix-lambda-node-args)
  (getter> 2 nix-lambda-node-body))

(define (nix-lambda-ellipsis-argument)
  the-...-%%)

(define (nix-lambda-ellipsis-argument? obj)
  (eq? the-...-%% obj))

(define (nix-abstract-syntax-tree? obj)
  (let loop ((p nix-node-predicates))
    (and (pair? p)
         (or ((car p) obj)
             (loop (cdr p))))))

;;; A synonym.
(define nix-ast? nix-abstract-syntax-tree?)

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
        (else (err "incorrect type" value))))

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
        ((nix-list-node? ast)
         (%%output-nix-list-node ast outp))
        ((nix-get-node? ast)
         (%%output-nix-get-node ast outp))
        ((nix-has?-node? ast)
         (%%output-nix-has?-node ast outp))
        ((nix-//-node? ast)
         (%%output-nix-//-node ast outp))
        ((nix-with-node? ast)
         (%%output-nix-with-node ast outp))
        ((nix-unaryoperator-node? ast)
         (%%output-nix-unaryoperator-node ast outp))
        ((nix-binaryoperator-node? ast)
         (%%output-nix-binaryoperator-node ast outp))
        ((nix-ifthenelse-node? ast)
         (%%output-nix-ifthenelse-node ast outp))
        ((nix-lambda-node? ast)
         (%%output-nix-lambda-node ast outp))
        (else (err "not an abstract syntax tree" ast)))))))

;;; A synonym.
(define output-nix-ast output-nix-abstract-syntax-tree)

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
          (else (err "bad <nix-data-node>" ast)))))

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

(define (%%output-nix-identifier ast outp)
  (let ((data (nix-data-node-ref ast)))
    (cond ((symbol? data)
           (%%output-nix-identifier
            (make-nix-data-node (symbol->string data)) outp))
          ((not (string? data))
           (err "not a symbol or string" data))
          ((not (%%string-is-nix-identifier? data))
           (err "not a Nix identifier" data))
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

(define (%%output-nix-get-node ast outp)
  (let ((attrset (nix-get-node-attributeset ast))
        (attrpath (nix-get-node-attributepath ast)))
    (when attrset
      (%%output-attrset attrset outp)
      (outp ".\n"))
    (output-nix-abstract-syntax-tree attrpath outp)))

(define (%%output-nix-has?-node ast outp)
  (let ((attrset (nix-has?-node-attributeset ast))
        (attrpath (nix-has?-node-attributepath ast)))
    (outp "(\n")
    (%%output-attrset attrset outp)
    (outp "?\n")
    (output-nix-abstract-syntax-tree attrpath outp)
    (outp ")\n")))

(define (%%output-nix-//-node ast outp)
  (let ((attrset1 (nix-//-node-attributeset1 ast))
        (attrset2 (nix-//-node-attributeset2 ast)))
    (outp "(\n")
    (%%output-attrset attrset1 outp)
    (outp "//\n")
    (%%output-attrset attrset2 outp)
    (outp ")\n")))

(define (%%output-nix-with-node ast outp)
  (let ((attrset (nix-with-node-attributeset ast))
        (clause (nix-with-node-clause ast)))
    (outp "(with\n")
    (%%output-attrset attrset outp)
    (outp ";\n")
    (output-nix-abstract-syntax-tree clause outp)
    (outp ")\n")))

(define (%%output-nix-unaryoperator-node ast outp)
  (outp "(\n")
  (outp (nix-unaryoperator-node-op ast))
  (outp "\n")
  (output-nix-abstract-syntax-tree
   (nix-unaryoperator-node-arg ast) outp)
  (outp ")\n"))

(define (%%output-nix-binaryoperator-node ast outp)
  (outp "(\n")
  (output-nix-abstract-syntax-tree
   (nix-binaryoperator-node-arg1 ast) outp)
  (outp (nix-binaryoperator-node-op ast))
  (outp "\n")
  (output-nix-abstract-syntax-tree
   (nix-binaryoperator-node-arg2 ast) outp)
  (outp ")\n"))

(define (%%output-nix-ifthenelse-node ast outp)
  (let ((if-clause (nix-ifthenelse-node-if-clause ast))
        (then-clause (nix-ifthenelse-node-then-clause ast))
        (else-clause (nix-ifthenelse-node-else-clause ast)))
    (outp "(if\n")
    (output-nix-abstract-syntax-tree if-clause outp)
    (outp "then\n")
    (output-nix-abstract-syntax-tree then-clause outp)
    (outp "else\n")
    (output-nix-abstract-syntax-tree else-clause outp)
    (outp ")\n")))

(define (%%output-nix-lambda-node ast outp)
  (define (attrset-arg lst outp)
    (let loop ((p lst))
      (when (pair? p)
        (let ((carp (car p)))
          (cond
           ((pair? carp)
            ;; There is a default value.
            (unless (= (length carp) 2)
              (err "expected an argument with default value" carp))
            (%%output-nix-identifier (car carp) outp)
            (outp "?\n")
            (output-nix-abstract-syntax-tree
             (scheme->nix (cadr carp)) outp))
           ((%%nix-identifier? carp)
            (%%output-nix-identifier carp outp))
           (else
            (err "malformed nix-lambda arguments" p)))
          (outp ",\n")
          (if (nix-lambda-ellipsis-argument? (cdr p))
            (outp "...\n") ;; An improper list.
            (loop (cdr p)))))))
  (let ((args (nix-lambda-node-args ast))
        (body (nix-lambda-node-body ast)))
    (outp "(\n")
    (let loop ((p args))
      (when (pair? p)
        (let ((carp (car p)))
          (cond ((pair? carp)
                 (outp "{\n")
                 (attrset-arg carp outp)
                 (outp "}:\n"))
                ((%%nix-identifier? carp)
                 (%%output-nix-identifier carp outp)
                 (outp ":\n"))
                (else
                 (err "malformed nix-lambda arguments" p))))
        (loop (cdr p))))
    (output-nix-abstract-syntax-tree (scheme->nix body) outp)
    (outp ")\n")))

(define (%%string-contains-slash? str)
  (%%string-contains? (lambda (c) (char=? c #\/))
                      str))

(define (%%string-contains? pred str)
  (let ((n (string-length str)))
    (let loop ((i 0))
      (cond ((= i n) #f)
            ((pred (string-ref str i)) #t)
            (else (loop (+ i 1)))))))

(define (%%nix-identifier? obj)
  (and (nix-data-node? obj)
       (let ((data (nix-data-node-ref obj)))
         (or (and (string? data)
                  (%%string-is-nix-identifier? data))
             (and (symbol? data)
                  (%%string-is-nix-identifier?
                   (symbol->string data)))))))

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

(define (%%output-attrset attrset outp)
  (if (%%nix-identifier? attrset)
    (%%output-nix-identifier attrset outp)
    (output-nix-abstract-syntax-tree attrset outp)))

(define (%%check-attrset attrset)
  (unless (or (%%nix-identifier? attrset)
              (and (nix-get-node? attrset)
                   (not (nix-get-node-attributeset attrset)))
              (nix-attributeset-node? attrset))
    (err "expected an identifier or attribute set"
         attrset)))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
