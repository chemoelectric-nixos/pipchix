;;;
;;; Copyright © 2026 Barry Schwartz
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
;;;-------------------------------------------------------------------

;;; WARNING: This variable is not thread-safe.
(define failures (vector '()))

;;; WARNING: This variable is not thread-safe.
(define failure-stack%% (vector failures))

(define-syntax change-failure-stack!
  (syntax-rules ()
    ((¶ pointer)
     (vector-set! failure-stack%% 0 pointer))))

(define-syntax failure-stack
  (syntax-rules ()
    ((¶) (vector-ref failure-stack%% 0))))

(define-syntax failure-set!
  (syntax-rules ()
    ((¶ stk)
     (vector-set! (failure-stack) 0 stk))))

(define-syntax failure-ref
  (syntax-rules ()
    ((¶) (vector-ref (failure-stack) 0))))

(define-syntax push-failure!
  (syntax-rules ()
    ((¶ failure)
     (let ((stk (failure-ref)))
       (failure-set! (cons failure stk))))))

(define-syntax drop-failure!
  (syntax-rules ()
    ((¶)
     (let ((stk (failure-ref)))
       (when (pair? stk)
         (failure-set! (cdr stk)))))))

(define (attempt-dynamic-wind thunk)
  (dynamic-wind (lambda () (if #f #f))
                thunk
                (lambda () (drop-failure!))))

(define (in-new-failure-context thunk)
  (let ((new-failure-stack (vector '()))
        (saved-failure-stack '<undefined>))
    (dynamic-wind ;; Stack-switching for backtracks.
      (lambda ()
        (set! saved-failure-stack (failure-stack))
        (change-failure-stack! new-failure-stack))
      thunk
      (lambda ()
        (change-failure-stack! saved-failure-stack)
        (set! saved-failure-stack '<undefined>)))))

(define *failure* '#("the\xA0;failure\xA0;object"))

(define (failure-object)
  ;; Return a failure object.
  *failure*)

(define (failure-object? obj)
  ;; Test if an object is a failure object.
  (eq? obj *failure*))

(define (fail)
  (let ((stk (failure-ref)))
    (if (pair? stk)
      ((car stk))
      *failure*)))

(define-syntax attempt-not
  ;;
  ;; Reverse success and failure with each other. Returns nothing
  ;; useful.
  ;;
  (syntax-rules ()
    ((¶ form)
     (if (call/cc
          (lambda (leave)
            (attempt-dynamic-wind
             (lambda ()
               (call/cc
                (lambda (failure)
                  (push-failure! failure)
                  form
                  (leave #t)))
               (leave #f)))))
       (fail)
       (if #f #f)))))

(define-syntax attempt-or
  ;;
  ;; On success, returns the values returned by the successful form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (call/cc
      (lambda (leave)
        (begin
          (call/cc
           (lambda (next)
             (attempt-dynamic-wind
              (lambda ()
                (call/cc
                 (lambda (failure)
                   (push-failure! failure)
                   (call-with-values
                       (lambda () form1)
                     leave)
                   (next)))))))
          ...)
        (fail))))))

(define-syntax attempt-and
  ;;
  ;; On success, returns the values returned by the last form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (let-values ((val* (values)))
       (if (call/cc
            (lambda (leave)
              (begin
                (call/cc
                 (lambda (next)
                   (attempt-dynamic-wind
                    (lambda ()
                      (call/cc
                       (lambda (failure)
                         (push-failure! failure)
                         (let-values ((v* form1))
                           (set! val* v*)
                           (next))))
                      (leave #f)))))
                ...)
              (leave #t)))
         (apply values val*)
         (fail))))))

(define-syntax attempt-while
  ;;
  ;; Returns the values returned by the last successful form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (let-values ((val* (values)))
       (call/cc
        (lambda (leave)
          (begin
            (call/cc
             (lambda (next)
               (attempt-dynamic-wind
                (lambda ()
                  (call/cc
                   (lambda (failure)
                     (push-failure! failure)
                     (let-values ((v* form1))
                       (set! val* v*)
                       (next))))
                  (leave)))))
            ...)))
       (apply values val*)))))

(define-syntax attempt-every
  ;;
  ;; Returns the values returned by the last successful form.
  ;;
  (syntax-rules ()
    ((¶ form1 ...)
     (let-values ((val* (values)))
       (begin
         (call/cc
          (lambda (next)
            (attempt-dynamic-wind
             (lambda ()
               (call/cc
                (lambda (failure)
                  (push-failure! failure)
                  (let-values ((v* form1))
                    (set! val* v*)
                    (next))))))))
         ...)
       (apply values val*)))))

(define-syntax attempt-or-ec
  ;;
  ;; On success, returns the values returned by ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (call/cc
      (lambda (leave)
        (do-ec
          qualifier1 ...
          (call/cc
           (lambda (next)
             (attempt-dynamic-wind
              (lambda ()
                (call/cc
                 (lambda (failure)
                   (push-failure! failure)
                   (call-with-values
                       (lambda () command)
                     leave)
                   (next))))))))
        (fail))))))

(define-syntax attempt-and-ec
  ;;
  ;; On success, returns the values returned by the last call to
  ;; ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (let-values ((val* (values)))
       (if (call/cc
            (lambda (leave)
              (do-ec
                qualifier1 ...
                (call/cc
                 (lambda (next)
                   (attempt-dynamic-wind
                    (lambda ()
                      (call/cc
                       (lambda (failure)
                         (push-failure! failure)
                         (let-values ((v* command))
                           (set! val* v*)
                           (next))))
                      (leave #f))))))
              (leave #t)))
         (apply values val*)
         (fail))))))

(define-syntax attempt-while-ec
  ;;
  ;; Returns the values returned by the last successful call to
  ;; ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (let-values ((val* (values)))
       (call/cc
        (lambda (leave)
          (do-ec
            qualifier1 ...
            (call/cc
             (lambda (next)
               (attempt-dynamic-wind
                (lambda ()
                  (call/cc
                   (lambda (failure)
                     (push-failure! failure)
                     (let-values ((v* command))
                       (set! val* v*)
                       (next))))
                  (leave))))))))
       (apply values val*)))))

(define-syntax attempt-every-ec
  ;;
  ;; Returns the values returned by the last successful call to
  ;; ‘command’.
  ;;
  (syntax-rules ()
    ((¶ qualifier1 ... command)
     (let-values ((val* (values)))
       (do-ec
         qualifier1 ...
         (call/cc
          (lambda (next)
            (attempt-dynamic-wind
             (lambda ()
               (call/cc
                (lambda (failure)
                  (push-failure! failure)
                  (let-values ((v* command))
                    (set! val* v*)
                    (next)))))))))
       (apply values val*)))))

(define-syntax attempt
  ;;
  ;; Try a predicate or fail. On success, return the result of the
  ;; predicate.
  ;;
  (syntax-rules ()
    ((¶ predicate obj ...)
     (or (predicate obj ...) (fail)))))

(define attempt-null (cut attempt null? <>))
(define attempt-pair (cut attempt pair? <>))

(define attempt-zero (cut attempt zero? <>))
(define attempt-nonzero
  (let ((nonzero? (lambda (x) (not (zero? x)))))
    (cut attempt nonzero? <>)))
(define attempt-positive (cut attempt positive? <>))
(define attempt-nonpositive
  (let ((nonpositive? (lambda (x) (not (positive? x)))))
    (cut attempt nonpositive? <>)))
(define attempt-negative (cut attempt negative? <>))
(define attempt-nonnegative
  (let ((nonnegative? (lambda (x) (not (negative? x)))))
    (cut attempt nonnegative? <>)))
(define attempt-odd (cut attempt odd? <>))
(define attempt-even (cut attempt even? <>))

(define (attempt-char-ascii obj) (attempt (cut char<=? <> #\x7F) obj))
(define attempt-char-alphabetic (cut attempt char-alphabetic? <>))
(define attempt-char-numeric (cut attempt char-numeric? <>))
(define attempt-char-whitespace (cut attempt char-whitespace? <>))
(define attempt-char-upper-case (cut attempt char-upper-case? <>))
(define attempt-char-lower-case (cut attempt char-lower-case? <>))

(define-syntax general-reversible-set!
  (syntax-rules ()
    ((¶ getter! setter! () body ...)
     (begin (if #f #f) body ...))
    ((¶ getter! setter! ((obj value) ...) body ...)
     (let*-values
         ((previous-values (values (getter! obj) ...))
          ((successful? val* results)
           (call/cc
            (lambda (leave)
              (attempt-dynamic-wind
               (lambda ()
                 (call/cc
                  (lambda (failure)
                    (push-failure! failure)
                    (setter! obj value) ...
                    (let-values ((val* (begin (if #f #f) body ...)))
                      (leave #t val* (list (getter! obj) ...)))))
                 (let ((p previous-values))
                   (begin
                     (setter! obj (car p))
                     (set! p (cdr p)))
                   ...
                   (leave #f #f previous-values))))))))
       (let ((p results))
         (begin
           (setter! obj (car p))
           (set! p (cdr p)))
         ...)
       (if successful?
         (apply values val*)
         (fail))))))

(define-syntax reversible-set!
  (syntax-rules ()
    ((¶ ((obj value) ...) body ...)
     (let-syntax ((ref (syntax-rules () ((µ t) t))))
       (general-reversible-set! ref set!
                                ((obj value) ...)
                                body ...)))))

(define-syntax reversible-box-set!
  (syntax-rules ()
    ((¶ ((obj value) ...) body ...)
     (let-syntax ((ref (syntax-rules () ((µ t) t))))
       (general-reversible-set! unbox set-box!
                                ((obj value) ...)
                                body ...)))))

(define-syntax reversible-vector-set!
  (syntax-rules ()
    ((¶ (((obj i) value) ...) body ...)
     (let-syntax
         ((getter (syntax-rules ()
                    ((µ (t j)) (vector-ref t j))))
          (setter! (syntax-rules ()
                     ((µ (t j) v) (vector-set! t j v)))))
       (general-reversible-set! getter setter!
                                (((obj i) value) ...)
                                body ...)))))

(define-syntax reversible-bytevector-u8-set!
  (syntax-rules ()
    ((¶ (((obj i) value) ...) body ...)
     (let-syntax
         ((getter (syntax-rules ()
                    ((µ (t j)) (bytevector-u8-ref t j))))
          (setter! (syntax-rules ()
                     ((µ (t j) v) (bytevector-u8-set! t j v)))))
       (general-reversible-set! getter setter!
                                (((obj i) value) ...)
                                body ...)))))

(define-syntax reversible-list-set!
  (syntax-rules ()
    ((¶ (((obj i) value) ...) body ...)
     (let-syntax
         ((getter (syntax-rules ()
                    ((µ (t j)) (list-ref t j))))
          (setter! (syntax-rules ()
                     ((µ (t j) v) (list-set! t j v)))))
       (general-reversible-set! getter setter!
                                (((obj i) value) ...)
                                body ...)))))

;;;-------------------------------------------------------------------
;;;
;;; Co-expressions.
;;;

(define *suspend*
  (make-parameter
   (lambda v* (apply values v*))))

(define (suspend . v*)
  (apply (*suspend*) v*))

(define (make-co-expression thunk)
  (let ((just-apply-values #f))
    (letrec
        ((resume-coexpr
          (lambda (ε . ξ*)

            ;; ε is the point in the program where the co-expression
            ;; was called.
            (let ((caller-of-coexpr ε))

              (define (suspension-procedure . v*)
                (call/cc
                 (lambda (cc)
                   ;; Resume at the point following the ‘suspend’.
                   (set! resume-coexpr cc)
                   ;; Perform the suspension.
                   (set! caller-of-coexpr (apply caller-of-coexpr v*))
                   ;; The ξ* are the values that were passed to this
                   ;; call of the co-expression.
                   (apply values ξ*))))

              (call/cc
               (lambda (leave)
                 (attempt-dynamic-wind
                  (lambda ()
                    (parameterize ((*suspend* suspension-procedure))
                      (call/cc
                       (lambda (failure)
                         (push-failure! failure)
                         (thunk)))
                      (set! just-apply-values #t)
                      (leave (failure-object)))))))))))

      (lambda ξ*
        (if just-apply-values
          (apply values ξ*)
          (call-with-values
              (lambda ()
                (in-new-failure-context
                 (lambda ()
                   (call/cc (lambda (ε)
                              (apply resume-coexpr (cons ε ξ*)))))))
            (lambda arg*
              (if (and (pair? arg*)
                       (null? (cdr arg*))
                       (failure-object? (car arg*)))
                (fail)
                (apply values arg*)))) )))))

;;;-------------------------------------------------------------------
;;;
;;; String scanning.
;;;

(define icon->scheme-indexing
  ;;
  ;; Convert Icon-style indices to Scheme indices. (Conversion of
  ;; Scheme indices to Icon-style indices can be done by adding 1.)
  ;;
  (case-lambda
    ((n i)
     (cond ((zero? i) n)
           ((negative? i)
            (if (< i (- n))
              (fail)
              (+ n i)))
           (else
            (if (< (+ n 1) i)
              (fail)
              (- i 1)))))
    ((n i j)
     (let ((i (icon->scheme-indexing n i))
           (j (icon->scheme-indexing n j)))
       (if (< j i)
         (values j i)
         (values i j))))))

(define *string-subject* (make-parameter (box "")))
(define *string-position* (make-parameter (box 1)))

(define &string-subject ;; Analogous to the Icon &subject keyword.
  (case-lambda
    (() (unbox (*string-subject*)))
    ((str)
     (unless (string? str)
       (error "expected a string" str))
     (set-box! (*string-subject*) str)
     (set-box! (*string-position*) 1))))

(define &string-position ;; Analogous to the Icon &pos keyword.
  (case-lambda
    (() (unbox (*string-position*)))
    ((i)
     (unless (integer? i)
       (error "expected an integer" i))
     (set-box! (*string-position*) i))))

(define-syntax string-scan
  (syntax-rules ()
    ((¶ str body ...)
     (parameterize ((*string-subject* (box str))
                    (*string-position* (box 1)))
       (if #f #f)
       body ...))))

(define (make-char-predicate obj)
  (lambda (c)
    (match obj
      ((? procedure? ob) (ob c))
      ((? char? ob)      (char=? ob c))
      ((? string? ob)    (any?-ec (:string ch ob) (char=? ch c)))
      ;; m4_ifelse(support_scheme_charset,«yes»,«
      ((? char-set? ob)  (char-set-contains? ob c))
      ;; »)
      (_ (error "unexpected argument type" obj)))))

(define string-any
  ;;
  ;; Analogous to Icon’s ‘any’ function. Examples:
  ;;
  ;;     (string-any #\s "string any")
  ;;     (string-any (cut string-ci=? #\s <>) "STRING ANY")
  ;;     (string-any char-alphabetic? "string any")
  ;;     (string-any "sSŝŜśŚ" "string any")
  ;;
  ;;     (string-any #\s)
  ;;     (string-any (cut string-ci=? #\s <>))
  ;;     (string-any char-alphabetic?)
  ;;     (string-any "sSŝŜśŚ")
  ;;
  ;; Also, with some R⁷RS Scheme implementations, you can use a
  ;; (scheme charset) = (srfi 14) character set.
  ;;
  (case-lambda
    ((c) ((string-any-%aux% make-char-predicate) c))
    ((c s) ((string-any-%aux% make-char-predicate) c s))
    ((c s i1) ((string-any-%aux% make-char-predicate) c s i1))
    ((c s i1 i2) ((string-any-%aux% make-char-predicate) c s i1 i2))))

(define (string-any-%aux% make-predicate)
  (define compare
    (case-lambda
      ((c) (compare c (&string-subject) (&string-position) 0))
      ((c s) (compare c s 1 0))
      ((c s i1) (compare c s i1 0))
      ((c s i1 i2)
       (let ((n (string-length s)))
         (let-values (((i1 i2) (icon->scheme-indexing n i1 i2)))
           (let ((n12 (- i2 i1)))
             (if (< n12 1)
               (fail)
               (let ((pred? (make-predicate c)))
                 (if (pred? (string-ref s i1))
                   (+ i1 2)
                   (fail))))))))))
  compare)

(define string-many
  ;;
  ;; Analogous to Icon’s ‘many’ function. Examples:
  ;;
  ;;     (string-many #\s "string many")
  ;;     (string-many (cut string-ci=? #\s <>) "STRING MANY")
  ;;     (string-many char-alphabetic? "string many")
  ;;     (string-many "rst" "string many")
  ;;
  ;;     (string-many #\s)
  ;;     (string-many (cut string-ci=? #\s <>))
  ;;     (string-many char-alphabetic?)
  ;;     (string-many "rst")
  ;;
  ;; Also, with some R⁷RS Scheme implementations, you can use a
  ;; (scheme charset) = (srfi 14) character set.
  ;;
  (case-lambda
    ((c) ((string-many-%aux% make-char-predicate) c))
    ((c s) ((string-many-%aux% make-char-predicate) c s))
    ((c s i1) ((string-many-%aux% make-char-predicate) c s i1))
    ((c s i1 i2) ((string-many-%aux% make-char-predicate)
                  c s i1 i2))))

(define (string-many-%aux% make-predicate)
  (define compare
    (case-lambda
      ((c) (compare c (&string-subject) (&string-position) 0))
      ((c s) (compare c s 1 0))
      ((c s i1) (compare c s i1 0))
      ((c s i1 i2)
       (let ((n (string-length s)))
         (let-values (((i1 i2) (icon->scheme-indexing n i1 i2)))
           (let ((n12 (- i2 i1)))
             (if (< n12 1)
               (fail)
               (let ((pred? (make-predicate c)))
                 (if (not (pred? (string-ref s i1)))
                   (fail)
                   (let loop ((i (+ i1 1)))
                     (cond ((= i i2) (+ i2 1))
                           ((pred? (string-ref s i)) (loop (+ i 1)))
                           (else (+ i 1)))))))))))))
  compare)

(define string-match
  ;;
  ;; Analogous to Icon’s ‘match’ function. Examples:
  ;;
  ;;     (string-match "string" "string match")
  ;;     ((string-match string-ci=?) "string" "STRING MATCH") 
  ;;
  (case-lambda
    ((arg) (if (procedure? arg)
             (string-match-%aux% arg)
             ((string-match-%aux% string=?) arg)))
    ((s1 s2) ((string-match-%aux% string=?) s1 s2))
    ((s1 s2 i1) ((string-match-%aux% string=?) s1 s2 i1))
    ((s1 s2 i1 i2) ((string-match-%aux% string=?) s1 s2 i1 i2))))

(define (string-match-%aux% predicate)
  (define compare
    (case-lambda
      ((s1) (compare s1 (&string-subject) (&string-position) 0))
      ((s1 s2) (compare s1 s2 1 0))
      ((s1 s2 i1) (compare s1 s2 i1 0))
      ((s1 s2 i1 i2)
       (let ((m (string-length s1))
             (n (string-length s2)))
         (let-values (((i1 i2) (icon->scheme-indexing n i1 i2)))
           (let ((n12 (- i2 i1)))
             (if (< n12 m)
               (fail)
               (let ((i1+m (+ i1 m)))
                 (if (predicate s1 (substring s2 i1 i1+m))
                   (+ i1+m 1)
                   (fail))))))))))
  compare)

(define string-find
  ;;
  ;; Analogous to Icon’s ‘find’ function. Returns a co-expression.
  ;; Examples:
  ;;
  ;;     (string-find "string" "string find")
  ;;     ((string-find string-ci=?) "string" "STRING FIND")
  ;;
  ;; There is no effort to be fast. Favored instead is generality of
  ;; the matching predicate.
  ;;
  (case-lambda
    ((arg) (if (procedure? arg)
             (string-find-%aux% arg)
             ((string-find-%aux% string=?) arg)))
    ((s1 s2) ((string-find-%aux% string=?) s1 s2))
    ((s1 s2 i1) ((string-find-%aux% string=?) s1 s2 i1))
    ((s1 s2 i1 i2) ((string-find-%aux% string=?) s1 s2 i1 i2))))

(define (string-find-%aux% predicate)
  (define compare
    (case-lambda
      ((s1) (compare s1 (&string-subject) (&string-position) 0))
      ((s1 s2) (compare s1 s2 1 0))
      ((s1 s2 i1) (compare s1 s2 i1 0))
      ((s1 s2 i1 i2)
       (let ((m (string-length s1))
             (n (string-length s2)))
         (let-values (((i1 i2) (icon->scheme-indexing n i1 i2)))
           (let ((len (- i2 i1 m)))
             (if (not (positive? len))
               (fail)
               (make-co-expression
                (lambda ()
                  (do-ec
                    (:let end (+ i1 len))
                    (:range i i1 end)
                    (when (predicate s1 (substring s2 i (+ i m)))
                      (suspend (+ i 1))))
                  (fail))))))))))
  compare)

(define string-tab
  ;;
  ;; Analogous to Icon’s ‘tab’ function. Example:
  ;;
  ;;     (string-tab (0) (lambda (s) (display s)(newline)))
  ;;
  (case-lambda
    ((i receiver)
     (let* ((s (&string-subject))
            (n (string-length s))
            (j (&string-position)))
       (attempt-and
         (let-values (((i% j%) (icon->scheme-indexing n j i)))
           (let ((substr (substring s i% j%)))
             (reversible-box-set! (((*string-position*) i))
               (receiver substr)))))))
    ((i)
     ;; Call without a receiver.
     (set-box! (*string-position*) i))))

(define string-move
  ;;
  ;; Analogous to Icon’s ‘move’ function. Example:
  ;;
  ;;     (string-move (3) (lambda (s) (display s)(newline)))
  ;;
  (case-lambda
    ((i receiver)
     (string-tab (+ (&string-position) i) receiver))
    ((i)
     ;; Call without a receiver.
     (string-tab (+ (&string-position) i)))))

;;;-------------------------------------------------------------------
m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'if 'scheme-indent-function 1)
;;; eval: (put 'set-box! 'scheme-indent-function 1)
;;; eval: (put 'reversible-box-set! 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
