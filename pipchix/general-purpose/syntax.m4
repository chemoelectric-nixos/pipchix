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
;;;
;;; Adaptations from SRFI-148 are copyrighted and licensed as follows:
;;;
;;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;;-------------------------------------------------------------------

(define-syntax delete-duplicate-identifiers
  ;;
  ;; Deletes duplicate identifiers from a syntactic list. For example:
  ;;
  ;;     (delete-duplicate-identifiers
  ;;      (syx-identity)
  ;;      if-bound-identifier=
  ;;      (a a a a b c d e c b d e))    -->    (a b c d e)
  ;;
  ;; The rightmost items are those retained, in order.
  ;;
  ;; The macro is written in continuation-passing-macro style.
  ;;
  (syntax-rules ()
    ((¶ k* if-ident= lst)
     (delete-duplicate-identifiers-aux k* if-ident= lst ()))))

(define-syntax delete-duplicate-identifiers-aux
  (syntax-rules ()
    ((µ (k ...) if-ident= () result*)
     (k ... result*))
    ((µ k* if-ident= (item1 ... itemN) result*)
     (if-identifier-in-list
      if-ident= itemN (item1 ...)
      (delete-duplicate-identifiers-aux
       k* if-ident= (item1 ...) result*)
      (delete-duplicate-identifiers-aux
       k* if-ident= (item1 ...) (itemN . result*))))))

(define-syntax syx-proper-list-length
  ;;
  ;; Calculates the length of a syntactic proper list. This macro is
  ;; in continuation-passing-macro style.
  ;;
  (syntax-rules ()
    ((¶ k* (obj1 ...))
     (syx-proper-list-length-aux k* (obj1 ...) (+)))))

(define-syntax syx-proper-list-length-aux
  (syntax-rules ()
    ((¶ (k ...) () n)
     (k ... n))
    ((¶ k* (obj1 obj2 ...) (plus ...))
     (syx-proper-list-length-aux k* (obj2 ...)
                                 (plus ... 1)))))

(define-syntax syx-identity
  (syntax-rules ()
    ((ι τ) τ)))

(define-syntax syx-values1
  (syntax-rules ()
    ((ι τ1) (τ1))))

(define-syntax syx-values2
  (syntax-rules ()
    ((ι τ1 τ2) (τ1 τ2))))

(define-syntax syx-values3
  (syntax-rules ()
    ((ι τ1 τ2 τ3) (τ1 τ2 τ3))))

;;;-------------------------------------------------------------------
;;;
;;; Matchers for syntactic objects.
;;;

;;; m4_ifelse(general_macros,«er-macro-transformer»,«

(define-syntax if-identifier
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (if (symbol? (first arg*))
         (second arg*)
         (third arg*))))))

(define-syntax if-free-identifier=
  ;; Based on code in the example implementation of SRFI-148 by Marc
  ;; Nieper-Wißkirchen.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (let ((id1 (first arg*))
             (id2 (second arg*)))
         (if (compare id1 id2)
           (third arg*)
           (fourth arg*)))))))

(define-syntax if-bound-identifier=
  ;; Based on code in the example implementation of SRFI-148 by Marc
  ;; Nieper-Wißkirchen.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (let ((id1 (first arg*))
             (id2 (second arg*)))
         (if (eq? id1 id2)
           (third arg*)
           (fourth arg*)))))))

(define-syntax if-...
  ;; Based on code in the example implementation of SRFI-148 by Marc
  ;; Nieper-Wißkirchen.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (let ((id (first arg*)))
         (if (compare id (rename '...))
           (second arg*)
           (third arg*)))))))

;;; »)

;;; m4_ifelse(general_macros,«syntax-case»,«

(define-syntax if-identifier
  (lambda (stx)
    (syntax-case stx ()
      ((¶ obj kt kf)
       (if (identifier? (syntax obj))
         (syntax kt)
         (syntax kf))))))

(define-syntax if-free-identifier=
  (lambda (stx)
    (syntax-case stx ()
      ((¶ id1 id2 kt kf)
       (let ((s1 (syntax id1))
             (s2 (syntax id2)))
         (if (and (identifier? s1)
                  (identifier? s2)
                  (free-identifier=? s1 s2))
           (syntax kt)
           (syntax kf)))))))

(define-syntax if-bound-identifier=
  (lambda (stx)
    (syntax-case stx ()
      ((¶ id1 id2 kt kf)
       (let ((s1 (syntax id1))
             (s2 (syntax id2)))
         (if (and (identifier? s1)
                  (identifier? s2)
                  (bound-identifier=? s1 s2))
           (syntax kt)
           (syntax kf)))))))

(define-syntax if-...
  (lambda (stx)
    (syntax-case stx ()
      ((¶ id kt kf)
       (let ((s (syntax id)))
         (if (and (identifier? s)
                  (free-identifier=? s (syntax (... ...))))
           (syntax kt)
           (syntax kf)))))))

;;; »)

(define-syntax if-identifier-in-list
  ;;
  ;; For example:
  ;;
  ;;    (define-syntax if-literal
  ;;      (syntax-rules ()
  ;;        ((¶ ident literal*
  ;;            continuation-if-true
  ;;            continuation-if-false)
  ;;         (if-identifier-in-list if-bound-identifier=
  ;;                                ident literal*
  ;;                                continuation-if-true
  ;;                                continuation-if-false)
  ;;
  (syntax-rules ()
    ((¶ if-ident= id% lst kt% kf%)
     (let ()
       (define-syntax loop
         (syntax-rules ()
           ((µ f id () kt kf)
            f)
           ((µ f id (item . item*) kt kf)
            (loop (if-ident= id item kt f)
                  id item* kt kf))))
       (loop kf% id% lst kt% kf%)))))

;;;; (define-syntax if-unbound-or-equiv-variable
;;;;   ;;
;;;;   ;; True if obj and var are equivalent bound identifiers.
;;;;   ;;
;;;;   ;; True if obj is a value and var is unbound. Then var is bound to
;;;;   ;; the value of obj.
;;;;   ;;
;;;;   ;; True if obj is a value var is bound and ‘equiv?’ to the value of
;;;;   ;; obj.
;;;;   ;;
;;;;   ;; Otherwise false.
;;;;   ;;
;;;;   ;; (In some Scheme implementations, unbound variables do not
;;;;   ;; necessarily work as one would hope. Sagittarius is an example.
;;;;   ;; Compilation may fail with an ‘unbound identifier’ error, even
;;;;   ;; though the code works in other Scheme implementations. Chibi may
;;;;   ;; issue an ‘undefined variable’ warning.)
;;;;   ;;
;;;;   (syntax-rules ()
;;;;     ((¶ equiv? obj var kt kf)
;;;;      (if-bound-identifier=
;;;;       obj var
;;;;       kt
;;;;       (let ((o obj))
;;;;         (call/cc
;;;;          (lambda (cc)
;;;;            (with-exception-handler
;;;;                (lambda (exc)
;;;;                  (let ((var o))
;;;;                    (cc kt)))
;;;;              (lambda ()
;;;;                (let ((v var))
;;;;                  (if (equiv? o v)
;;;;                    kt
;;;;                    kf)))))))))))

(define-syntax msyx-proper-list
  ;;
  ;; Try to match a syntactic proper list. For example:
  ;;
  ;;    (define-syntax success1
  ;;      (syntax-rules ()
  ;;        ((¶ a b)
  ;;         (begin
  ;;           (display 'a)
  ;;           (display " ")
  ;;           (display 'b)
  ;;           (newline)))))
  ;;
  ;;    (define-syntax failure1
  ;;      (syntax-rules ()
  ;;        ((¶ a)
  ;;         (begin
  ;;           (display 'a)
  ;;           (newline)))))
  ;;
  ;;    (syx-split
  ;;     (1 2 (3 => 4) 5 6)
  ;;     (msyx-proper-list)
  ;;     success1
  ;;     failure1)
  ;;
  ;; This will print:
  ;;
  ;;    (1 2) ((3 => 4) 5 6)
  ;;
  (syntax-rules ()
    ((¶ (item ...) kt kf) kt)
    ((¶ xxxxxxxxxx kt kf) kf)))

(define-syntax msyx-list
  ;;
  ;; Try to match a syntactic proper or dotted list. For example:
  ;;
  ;;    (define-syntax success1
  ;;      (syntax-rules ()
  ;;        ((¶ a b)
  ;;         (begin
  ;;           (display 'a)
  ;;           (display " ")
  ;;           (display 'b)
  ;;           (newline)))))
  ;;
  ;;    (define-syntax failure1
  ;;      (syntax-rules ()
  ;;        ((¶ a)
  ;;         (begin
  ;;           (display 'a)
  ;;           (newline)))))
  ;;
  ;;    (syx-split
  ;;     (1 2 (3 => 4 . 5) 6)
  ;;     (msyx-list)
  ;;     success1
  ;;     failure1)
  ;;
  ;; This will print:
  ;;
  ;;    (1 2) ((3 => 4 . 5) 6)
  ;;
  ;; Note that dotted list SYNTAX includes the non-empty proper lists.
  ;; Unfortunately, there is a great difference between dotted SYNTAX
  ;; and the ‘proper’ versus ‘dotted’ distinction defined by SRFI-1.
  ;; (Indeed, in SRFI-1 ‘dotted list’ includes everything that
  ;; ordinarily is called ‘not a list’.)
  ;;
  (syntax-rules ()
    ((¶ (item1 ... itemN . tail) kt kf) kt)
    ((¶ (item1 ...)              kt kf) kt)
    ((¶ xxxxxxxxxxxxxxxxxxxxxxxx kt kf) kf)))

(define-syntax msyx-null-list
  ;;
  ;; Try to match a syntactic null list.
  ;;
  ;;    (define-syntax success1
  ;;      (syntax-rules ()
  ;;        ((¶ a b)
  ;;         (begin
  ;;           (display 'a)
  ;;           (display " ")
  ;;           (display 'b)
  ;;           (newline)))))
  ;;
  ;;    (define-syntax failure1
  ;;      (syntax-rules ()
  ;;        ((¶ a)
  ;;         (begin
  ;;           (display 'a)
  ;;           (newline)))))
  ;;
  ;;    (syx-split
  ;;     (1 2 (3 => 4 . 5) () 6 7)
  ;;     (msyx-null-list)
  ;;     success1
  ;;     failure1)
  ;;
  ;; This will print:
  ;;
  ;;    (1 2 (3 => 4 . 5)) (() 6 7)
  ;;
  (syntax-rules ()
    ((¶ () kt kf) kt)
    ((¶ xx kt kf) kf)))

(define-syntax msyx-vector
  ;;
  ;; Try to match a syntactic vector. For example:
  ;;
  ;;    (define-syntax success1
  ;;      (syntax-rules ()
  ;;        ((¶ a b)
  ;;         (begin
  ;;           (display 'a)
  ;;           (display " ")
  ;;           (display 'b)
  ;;           (newline)))))
  ;;
  ;;    (define-syntax failure1
  ;;      (syntax-rules ()
  ;;        ((¶ a)
  ;;         (begin
  ;;           (display 'a)
  ;;           (newline)))))
  ;;
  ;;    (syx-split
  ;;     (1 2 #(3 => 4) 5 6)
  ;;     (msyx-vector)
  ;;     success1
  ;;     failure1)
  ;;
  ;; This will print:
  ;;
  ;;    (1 2) (#(3 => 4) 5 6)
  ;;
  (syntax-rules ()
    ((¶ #(item ...) kt kf) kt)
    ((¶ xxxxxxxxxxx kt kf) kf)))

(define-syntax msyx-literal
  ;;
  ;; Try to match a literal. For example:
  ;;
  ;;    (msyx-literal => =>
  ;;            (display "=>\n")
  ;;            (display "not =>\n"))
  ;;
  (syntax-rules ()
    ((¶ obj literal kt kf)
     (if-bound-identifier= obj literal kt kf))))

;;; m4_ifelse(general_macros,«er-macro-transformer»,«
;;;m4_define(«define_scheme_type_syntax»,«
(define-syntax $1
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (call/cc
        (lambda (cc)
          (with-exception-handler
              (lambda (exc)
                (cc (third arg*))) ;; Failure.
            (lambda ()
              (let ((x (first arg*)))
                ;; FIXME: How many of the following checks are actually
                ;; necessary? And are any others needed?
                (if (and (not (null? x))
                         (not (pair? x))
                         (not (symbol? x))
                         ($2 x))
                  (second arg*)
                  (third arg*)))))))))))
;;; »)
;;; »)
;;; m4_ifelse(general_macros,«syntax-case»,«
;;;m4_define(«define_scheme_type_syntax»,«
(define-syntax $1
  (lambda (stx)
    (syntax-case stx ()
      ((¶ (item ... itemN . tail) kt kf)
       (syntax kf))
      ((¶ (item ...) kt kf)
       (syntax kf))
      ((¶ obj kt kf)
       (call/cc
        (lambda (cc)
          (with-exception-handler
              (lambda (exc)
                (cc (syntax kf)))
            (lambda ()
              (if (and (not (identifier? (syntax obj)))
                       ($2 (syntax->datum (syntax obj))))
                (syntax kt)
                (syntax kf))))))))))
;;; »)
;;; »)

define_scheme_type_syntax(msyx-number,number?)
define_scheme_type_syntax(msyx-complex,complex?)
define_scheme_type_syntax(msyx-real,real?)
define_scheme_type_syntax(msyx-rational,rational?)
define_scheme_type_syntax(msyx-integer,integer?)
define_scheme_type_syntax(msyx-exact-integer,exact-integer?)
define_scheme_type_syntax(msyx-exact,exact?)
define_scheme_type_syntax(msyx-inexact,inexact?)
define_scheme_type_syntax(msyx-finite,finite?)
define_scheme_type_syntax(msyx-infinite,infinite?)
define_scheme_type_syntax(msyx-nan,nan?)
define_scheme_type_syntax(msyx-zero,zero?)
define_scheme_type_syntax(msyx-positive,positive?)
define_scheme_type_syntax(msyx-negative,negative?)
define_scheme_type_syntax(msyx-odd,odd?)
define_scheme_type_syntax(msyx-even,even?)

define_scheme_type_syntax(msyx-string,string?)

define_scheme_type_syntax(msyx-char,char?)
define_scheme_type_syntax(msyx-char-alphabetic,char-alphabetic?)
define_scheme_type_syntax(msyx-char-numeric,char-numeric?)
define_scheme_type_syntax(msyx-char-whitespace,char-whitespace?)
define_scheme_type_syntax(msyx-char-upper-case,char-upper-case?)
define_scheme_type_syntax(msyx-char-lower-case,char-lower-case?)

define_scheme_type_syntax(msyx-boolean,boolean?)

(define-syntax msyx-false
  (syntax-rules ()
    ((¶ #f success failure) success)
    ((¶ xx success failure) failure)))

(define-syntax msyx-true
  (syntax-rules ()
    ((¶ #t success failure) success)
    ((¶ xx success failure) failure)))

(define-syntax msyx-car
  (syntax-rules ()
    ((¶ (Car . Cdr) (match1 arg1 ...) success failure)
     (match1 Car arg1 ... success failure))
    ((¶ xxxxxxxxxxx (match1 arg1 ...) success failure)
     failure)))

(define-syntax msyx-cdr
  (syntax-rules ()
    ((¶ (Car . Cdr) (match1 arg1 ...) success failure)
     (match1 Cdr arg1 ... success failure))
    ((¶ xxxxxxxxxxx (match1 arg1 ...) success failure)
     failure)))

(define-syntax msyx-quote
  (syntax-rules (quote)
    ((¶ (quote b) success failure)
     success)
    ((¶ xxxxx success failure)
     failure)))

(define-syntax msyx-quasiquote
  (syntax-rules (quasiquote)
    ((¶ (quasiquote b) success failure)
     success)
    ((¶ xxxxxxxxxxxxxx success failure)
     failure)))

(define-syntax msyx-unquote
  (syntax-rules (unquote)
    ((¶ (unquote b) success failure)
     success)
    ((¶ xxxxxxxxxxx success failure)
     failure)))

(define-syntax msyx-unquote-splicing
  (syntax-rules (unquote-splicing)
    ((¶ (unquote-splicing b) success failure)
     success)
    ((¶ xxxxxxxxxxxxxxxxxxxx success failure)
     failure)))

(define-syntax msyx-not
  ;;
  ;; Reverse the sense of a syntax matcher. Example:
  ;;
  ;;     (syx-split
  ;;      (+nan.0 +nan.0 +nan.0 (1) (2) 1 3.0 3 (4) (5))
  ;;      (msyx-not (msyx-nan))
  ;;      success1
  ;;      failure1)
  ;;
  (syntax-rules ()
    ((¶ obj (match1 arg1 ...) success failure)
     (match1 obj arg1 ... failure success))))

(define-syntax msyx-or
  ;;
  ;; Disjunction of syntax matchers. Example:
  ;;
  ;;     (syx-split ((1) (2) (3) (4) 1 2.1 3 4)
  ;;                   (msyx-or
  ;;                    (msyx-literal =>)
  ;;                    (msyx-nan)
  ;;                    (msyx-real)
  ;;                    (msyx-infinite))
  ;;                   success1
  ;;                   failure1)
  ;;
  ;; Which gives the split:
  ;;
  ;;     ((1) (2) (3) (4)) (1 2.1 3 4)
  ;;
  (syntax-rules ()
    ((¶ obj success failure)
     failure)
    ((¶ obj (match1 a1 ...) (match2 a2 ...) ... success failure)
     (match1 obj a1 ...
             success
             (msyx-or obj (match2 a2 ...) ... success failure)))))

(define-syntax msyx-and
  ;;
  ;; Conjunction of syntax matchers. Example:
  ;;
  ;;    (syx-split ((1) (2) (3) (4) 1 2.1 3 4)
  ;;                  (msyx-and
  ;;                   (msyx-finite)
  ;;                   (msyx-rational)
  ;;                   (msyx-not
  ;;                   (msyx-integer)))
  ;;                  success1
  ;;                  failure1)
  ;;
  ;; Which gives the split:
  ;;
  ;;    ((1) (2) (3) (4) 1) (2.1 3 4)
  ;;
  (syntax-rules ()
    ((¶ obj success failure)
     success)
    ((¶ obj (match1 a1 ...) (match2 a2 ...) ... success failure)
     (match1 obj a1 ...
             (msyx-and obj (match2 a2 ...) ... success failure)
             failure))))

(define-syntax msyx-xor
  ;;
  ;; Succeed if one, but only one, of a set of matchers succeeds. For
  ;; example:
  ;;
  ;;    (syx-split ((1) (2) (3) (4) 1 "string" #\x2A01 2.1 3 4)
  ;;                  (msyx-xor
  ;;                   (msyx-real)
  ;;                   (msyx-char)
  ;;                   (msyx-string)
  ;;                   (msyx-integer))
  ;;                  success1
  ;;                  failure1)
  ;;
  ;; This gives the split:
  ;;
  ;;    ((1) (2) (3) (4) 1) (string ⨁ 2.1 3 4)
  ;;
  (syntax-rules ()
    ((¶ obj (match1 a1 ...) (match2 a2 ...) ... success failure)
     (msyx-xor-aux obj "f" (match1 a1 ...) (match2 a2 ...) ...
                   success failure))))

(define-syntax msyx-xor-aux
  (syntax-rules ()
    ((¶ obj "t" success failure)
     success)
    ((¶ obj "f" success failure)
     failure)
    ((¶ obj "t" (match1 a1 ...) (match2 a2 ...) ... success failure)
     (match1 obj a1 ...
             failure
             (msyx-xor-aux obj "t" (match2 a2 ...) ...
                           success failure)))
    ((¶ obj "f" (match1 a1 ...) (match2 a2 ...) ... success failure)
     (match1 obj a1 ...
             (msyx-xor-aux obj "t" (match2 a2 ...) ...
                           success failure)
             (msyx-xor-aux obj "f" (match2 a2 ...) ...
                           success failure)))))

;;;-------------------------------------------------------------------
;;;
;;; Scanners for syntactic objects.
;;;

(define-syntax syx-split-at-last-pair
  ;;
  ;; Split syntax at the last pair of a possibly dotted list.
  ;;
  (syntax-rules ()
    ((¶ (item1 ... itemN . tail) succeed fail)
     (succeed (item1 ...) (itemN . tail)))
    ((¶ anything-else succeed fail)
     (fail anything-else))))

(define-syntax syx-split
  ;;
  ;; Split syntax according to a predicate. For example:
  ;;
  ;;    (define-syntax success-branch
  ;;      (syntax-rules ()
  ;;        ((¶ a b)
  ;;         (begin
  ;;           (display "success: ")
  ;;           (display 'a)
  ;;           (display " ")
  ;;           (display 'b)
  ;;           (newline)))))
  ;;
  ;;    (define-syntax failure-branch
  ;;      (syntax-rules ()
  ;;        ((¶ a)
  ;;         (begin
  ;;           (display "failure: ")
  ;;           (display 'a)
  ;;           (display " ")
  ;;           (newline)))))
  ;;
  ;;    (syx-split (a b c => 4 5 6)
  ;;                  (msyx-literal =>)
  ;;                  success-branch
  ;;                  failure-branch)
  ;;
  ;; Running the foregoing prints
  ;;
  ;;    success: (a b c) (=> 4 5 6)
  ;;
  (syntax-rules ()
    ((¶ (item ...) (predicate arg2 ...) succeed fail)
     (syx-split () (item ...) (predicate arg2 ...)
                (syx-values3 succeed)
                (syx-values2 fail)))

    ((¶ #(item ...) (predicate arg2 ...) succeed fail)
     (syx-split #() #(item ...) (predicate arg2 ...)
                succeed fail))

    ((¶ (item1 ... itemN . tail) (predicate arg2 ...) succeed fail)
     (syx-split () (item1 ... itemN . tail) (predicate arg2 ...)
                succeed fail))

    ;; The rules that follow are for partially split stages.

    ((¶ #(left1 ...) #(item ...) (pred arg2 ...) succeed fail)
     ;; Partially split vector.
     (syx-split (left1 ...) (item ...) (pred arg2 ...)
                (syx-split-s2 succeed)
                (syx-split-f2 fail)))

    ((¶ (left1 ...) () (pred arg2 ...) (succeed ks) (fail kf))
     ;; The split attempt has failed.
     (fail kf (left1 ...)))

    ((¶ (left1 ...) (right1 right2 ...) (pred arg2 ...)
        (succeed ks) (fail kf))
     ;; Try the next entry.
     (pred right1 arg2 ... 
           (succeed ks (left1 ...) (right1 right2 ...))
           (syx-split (left1 ... right1) (right2 ...)
                      (pred arg2 ...)
                      (succeed ks) (fail kf))))

    ((¶ (left1 ...) (item1 ... itemN . tail) (pred arg2 ...)
        succeed fail)
     ;; Partially split dotted list.
     (syx-split (left1 ...) (item1 ... ) (pred arg2 ...)
                (syx-split-s3 #(succeed (itemN . tail)))
                (syx-split-f3 #(succeed
                                fail (itemN . tail)
                                (pred arg2 ...)))))))

(define-syntax syx-split-s2
  (syntax-rules ()
    ((¶ k (a ...) (b ...))
     (k #(a ...) #(b ...)))))

(define-syntax syx-split-f2
  (syntax-rules ()
    ((¶ k (a ...))
     (k #(a ...)))))

(define-syntax syx-split-s3
  (syntax-rules ()
    ((¶ #(k (itemN . tail)) (a ...) (b ...))
     (k (a ...) (b ... itemN . tail)))))

(define-syntax syx-split-f3
  (syntax-rules ()
    ((¶ #(ks kf (itemN . tail) (pred arg2 ...)) (a ...))
     (pred itemN arg2 ...
           (ks (a ...) (itemN . tail))
           (kf (a ... itemN . tail))))))

;;;-------------------------------------------------------------------
;;;
;;; Support for matchers for runtime objects.
;;;

(define the-default-initialization-object
  ;; U+00A0 is the NO-BREAK SPACE.
  '#("the\xA0;default\xA0;initialization\xA0;object"))

(define (default-initialization)
  the-default-initialization-object)

(define (has-default-initialization? obj)
  (eq? obj the-default-initialization-object))

(define-syntax if-default-initialization-or-equiv-object
  ;;
  ;; True if obj1 and obj2 are equivalent bound identifiers.
  ;;
  ;; True if obj1 is a value and obj2 is set to the
  ;; ‘default-initialization’ object. Then obj2 is bound to the value
  ;; of obj.
  ;;
  ;; True if obj1 is a value obj2 is bound and ‘equiv?’ to the value
  ;; of obj1.
  ;;
  ;; Otherwise false.
  ;;
  (syntax-rules ()
    ((¶ obj1 obj2 getter! setter! equiv? kt kf)
     (if-bound-identifier=
      obj1 obj2
      kt
      (let ((o1 obj1)
            (o2 (getter! obj2)))
        (cond ((has-default-initialization? o2)
               (setter! obj2 o1)
               kt)
              ((equiv? o1 o2)
               kt)
              (else
               kf)))))))

(define-syntax if-default-initialization-or-equiv-variable
  ;;
  ;; if-default-initialization-or-equiv-object for the case where the
  ;; object is a variable, such as created by ‘define’ or ‘let’, or by
  ;; ‘lambda’ as one of its arguments.
  ;;
  (syntax-rules ()
    ((¶ obj var equiv? kt kf)
     (let-syntax ((identity
                   (syntax-rules ()
                     ((¶ x) x))))
       (if-default-initialization-or-equiv-object
        obj var identity set! equiv? kt kf)))))

(define-syntax extract-identifiers-from-proper-list
  ;;
  ;; Extract identifiers from list (item ...) that are not already
  ;; collected in (previous ...) and which are not literals in
  ;; (literal ...). Respective equivalence matchers are used.
  ;;
  ;; The macro is written in continuation-passing-macro style.
  ;;
  (syntax-rules ()
    ((¶ k* if-id=2 (previous ...) (item ...))
     (extract-identifiers-from-proper-list-aux
      k* if-free-identifier= ()
      if-id=2 (item ...) (previous ...)))

    ((¶ k* if-id=1 (literal ...)
        if-id=2 (previous ...) (item ...))
     (extract-identifiers-from-proper-list-aux
      k* if-id=1 (literal ...)
      if-id=2 (item ...) (previous ...)))))

(define-syntax extract-identifiers-from-proper-list-aux
  (syntax-rules ()
    ((¶ (k ...) if-id=1 (literal ...)
        if-id=2 () result*)
     (k ... result*))
    ((¶ k* if-id=1 (literal ...)
        if-id=2 (item ... itemN) result*)
     (if-identifier-in-list
      if-id=1 itemN (literal ...)
      (extract-identifiers-from-proper-list-aux
       k* if-id=1 (literal ...)
       if-id=2  (item ...) result*)
      (if-identifier-in-list
       if-id=2 itemN result*
       (extract-identifiers-from-proper-list-aux
        k* if-id=1 (literal ...)
        if-id=2 (item ...) result*)
       (if-identifier
        itemN
        (extract-identifiers-from-proper-list-aux
         k* if-id=1 (literal ...)
         if-id=2 (item ...) (itemN . result*))
        (extract-identifiers-from-proper-list-aux
         k* if-id=1 (literal ...)
         if-id=2 (item ...) result*)))))))

(define-syntax make-identifiers-environment
  ;;
  ;;     (make-identifiers-environment
  ;;      (new ...) (old ...) (body ...))
  ;;
  ;; creates an environment in which ‘new’ variables not in ‘old’
  ;; variables are initialized to (default-initialization).
  ;;
  (syntax-rules ()

    ((¶ () () (body ...))
     (let () (if #f #f) body ...))

    ((¶ (new1 ...) () (body ...))
     (let ((dflt (default-initialization)))
       (let ((new1 dflt) ...)
         (if #f #f) body ...)))
    
    ((¶ new* old* (body ...))
     (make-identifiers-environment-aux new* old* () (body ...)))))

(define-syntax make-identifiers-environment-aux
  (syntax-rules ()

    ((¶ () old* (var ...) (body ...))
     (let ((dflt (default-initialization)))
       (let ((var dflt) ...)
         (if #f #f) body ...)))

    ((¶ (new . new*) old* (var ...) (body ...))
     (if-identifier-in-list
      if-bound-identifier= new old*
      (make-identifiers-environment-aux new* old* (new var ...)
                                        (body ...))
      (make-identifiers-environment-aux new* old* (var ...)
                                        (body ...))))))

;;;-------------------------------------------------------------------
;;;
;;; Matchers for runtime objects.
;;;

(define-syntax match-proper-list
  ;;
  ;; Try to match a proper list runtime object. Example:
  ;;
  ;;    (match-proper-list (list 1 2 3) (a 2 c)
  ;;      (begin (display "yes: ")
  ;;             (display (list a 2 c))
  ;;             (newline))
  ;;      (display "no\n"))
  ;;
  ;; To import nonlocal variables:
  ;;
  ;;    (define-values (b c) (list 2 3))
  ;;    (match-proper-list (list 1 2 3) (a b c) (:nonlocal b c)
  ;;      (begin (display "yes: ")
  ;;             (display (list a b c))
  ;;             (newline))
  ;;      (display "no\n"))
  ;;
  (syntax-rules (:nonlocal)
    ((¶ obj (var1 ...) kt kf)
     (match-proper-list obj (var1 ...) (:nonlocal) kt kf))
    ((¶ obj (var1 ...) (:nonlocal previous ...) kt kf)
     (cond
       ((not (proper-list? obj))
        kf)
       ((not (= (length obj)
                (syx-proper-list-length (syx-identity)
                                        (var1 ...))))
        kf)
       (else
        ;; Evaluate the list here. Otherwise, variable names in it may
        ;; be misinterpreted as identifiers, rather than as values.
        (let ((lst obj))
          (extract-identifiers-from-proper-list
           (match-proper-list-aux (previous ...) lst (var1 ...)
                                  kt kf)
           if-bound-identifier= (previous ...) (var1 ...))))))))

(define-syntax match-proper-list-aux
  (syntax-rules ()
    ((¶ (previous ...) lst (var1 ...) kt kf (new ...))
     (make-identifiers-environment
      (new ...)
      (previous ...)
      ((call/cc
        (lambda (cc)
          (let ((identity (lambda x* (apply values x*)))
                (p lst))
            (let ((elem (car p)))
              (if-identifier
               var1
               (if-default-initialization-or-equiv-variable
                elem var1 equal? identity (cc kf))
               (unless (equal? elem var1) (cc kf)))
              (set! p (cdr p)))
            ...
            (cc kt)))))))))

;;;-------------------------------------------------------------------

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
