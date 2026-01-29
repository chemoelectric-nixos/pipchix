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
;;; Adaptations from SRFI-147 are copyrighted and licensed as follows:
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

(define (thrush-maker custom)
  ;;
  ;; Creates ‘point-free’ combinators known to Racket programmers.
  ;;
  (lambda (proc . proc*)
    (lambda val*
      (let ((p proc*))
        (define loop
          (lambda val*
            (if (pair? p)
              (let ((proc (car p)))
                (set! p (cdr p))
                (custom loop proc val*))
              (apply values val*))))
        (custom loop proc val*)))))

(define thrush
  ;;
  ;; A ‘point-free’ combinator known to Racket programmers.
  ;;
  ;; (thrush f g h) returns a procedure whose data flow is
  ;; input-values => f => g => h => output-values
  ;;
  ;; One may wish to use this along with SRFI-26 (pipchix
  ;; general-purpose cut).
  ;;
  (thrush-maker
   (lambda (loop proc val*)
     (call-with-values
         (lambda () (apply proc val*))
       loop))))

(define λ~> thrush)      ;; A synonym known to Racket programmers.
(define lambda~> thrush) ;; A synonym without Greek script.

(define (thrush+ val . proc*)
  ;;
  ;; A combinator known to Racket programmers. It is also a
  ;; generalization of the CHICKEN Scheme ‘clojurian’ combinators.
  ;;
  ;; One may wish to use this along with SRFI-26 (pipchix
  ;; general-purpose cut).
  ;;
  (if (pair? proc*)
    ((apply thrush proc*) val)
    val))

(define ~> thrush+) ;; A synonym known to Racket programmers.

(define (thrush* . val*)
  ;;
  ;; Another Racket combinator. One can write
  ;;
  ;;   ((thrush* 1 2 3) proc1 proc2 proc3)
  ;;
  ;; instead of
  ;;
  ;;   ((thrush proc1 proc2 proc3) 1 2 3)
  ;;
  ;; or
  ;;
  (lambda proc*
    (call-with-values
        (lambda () (apply values val*))
      (apply thrush proc*))))

(define ~>* thrush*) ;; A synonym known to Racket programmers.

(define thrush-and
  ;;
  ;; Short-circuiting thrush. Stops and returns #f as soon as any
  ;; procedure returns the single value #f.
  ;;
  (let ((short-circuit? (lambda (val*)
                          (and (pair? val*)
                               (null? (cdr val*))
                               (not (car val*))))))
    (thrush-maker
     (lambda (loop proc val*)
       (and (not (short-circuit? val*))
            (call-with-values
                (lambda () (apply proc val*))
              loop))))))

(define λand~> thrush-and)       ;; Racket’s synonym.
(define lambda-and~> thrush-and) ;; A synonym without Greek script.

(define (thrush+-and val . proc*)
  ;;
  ;; Short-circuiting thrush+
  ;;
  (if (pair? proc*)
    ((apply thrush-and proc*) val)
    val))

(define and~> thrush+-and) ;; A synonym known to Racket programmers.

(define (thrush*-and . val*)
  ;;
  ;; Short-circuiting thrush*
  ;;
  (lambda proc*
    (call-with-values
        (lambda () (apply values val*))
      (apply thrush-and proc*))))

(define and~>* thrush*-and) ;; A synonym known to Racket programmers.

;;;-------------------------------------------------------------------

(define-syntax thrush-syntax
  ;;
  ;; A syntactic equivalent of the ‘thrush’ combinator:
  ;;
  ;;   (let-syntax ((macro (thrush-syntax f1 f2 f3 ...)))
  ;;     (macro val1 val2 val3 ...))
  ;;
  (syntax-rules ()
    ((¶ . f*)
     (syntax-rules ()
       ((µ . val*)
        (thrush-syntax-aux f* val*))))))

(define-syntax thrush-syntax-aux
  (syntax-rules ()
    ((¶ (f) val*)
     (f . val*))
    ((¶ (f . f*) val*)
     (thrush-syntax-aux f* ((f . val*))))))

(define-syntax λσ~> ;; A synonym for thrush-syntax
  (syntax-rules ()
    ((¶ . f*)
     (thrush-syntax . f*))))

(define-syntax thrush+-syntax
  ;;
  ;; A syntactic equivalent of the ‘thrush+’ combinator:
  ;;
  ;;   (thrush+-syntax val f1 f2 f3 ...)
  ;;
  (syntax-rules ()
    ((¶ val . f*)
     (let-syntax ((macro (thrush-syntax . f*)))
       (macro val)))))

(define-syntax σ~> ;; A synonym for thrush+-syntax
  (syntax-rules ()
    ((¶ val . f*)
     (thrush+-syntax val . f*))))

(define-syntax thrush*-syntax
  ;;
  ;; A syntactic equivalent of the ‘thrush*’ combinator:
  ;;
  ;;   (let-syntax ((macro (thrush*-syntax val1 val2 val3 ...)))
  ;;     (macro f1 f2 f3 ...))
  ;;
  (syntax-rules ()
    ((¶ . val*)
     (syntax-rules ()
       ((µ f . f*)
        (thrush-syntax-aux (f . f*) val*))))))

(define-syntax σ~>* ;; A synonym for thrush*-syntax
  (syntax-rules ()
    ((¶ . val*)
     (thrush*-syntax . val*))))

;;;-------------------------------------------------------------------

(define (join proc . proc*)
  ;;
  ;; Join one-to-one procedures in parallel to make an n-to-n
  ;; procedure. This combinator may be familiar to Racket programmers.
  ;;
  (lambda (x . x*)
    (apply values
           (cons (proc x)
                 (let recurs ((proc* proc*)
                              (x* x*))
                   (if (pair? proc*)
                     (cons ((car proc*) (car x*))
                           (recurs (cdr proc*) (cdr x*)))
                     '()))))))

(define (wind-pre f g . g*)
  ;;
  ;; This combinator may be familiar to Racket programmers. Values
  ;; first go through a ‘join’ of the g and then are passed to f.
  ;;
  (let ((proc (apply join (cons g g*))))
    (wind-inner-outer proc f)))

(define (wind-post f g . g*)
  ;;
  ;; This combinator may be familiar to Racket programmers. The output
  ;; values of f are passed through the ‘join’ of the g.
  ;;
  (let ((proc (apply join (cons g g*))))
    (wind-inner-outer f proc)))

(define (wind f g . g*)
  ;;
  ;; This combinator may be familiar to Racket programmers. It
  ;; combines ‘wind-pre’ and ‘wind-post’. Values go through the ‘join’
  ;; of the g, then through f, then they go through the ‘join’ of the
  ;; h.
  ;;
  (let ((pre (apply wind-pre (cons* f g g*))))
    (lambda (h . h*)
      (apply wind-post (cons* pre h h*)))))

(define (join* proc)
  ;;
  ;; Apply a procedure to multiple values. This combinator may be
  ;; familiar to Racket programmers.
  ;;
  (lambda (x . x*)
    (apply values
           (cons (proc x)
                 (let recurs ((x* x*))
                   (if (pair? x*)
                     (cons (proc (car x*))
                           (recurs (cdr x*)))
                     '()))))))

(define (wind-pre* f g)
  ;;
  ;; wind-pre for join*
  ;;
  (let ((proc (join* g)))
    (wind-inner-outer proc f)))

(define (wind-post* f g)
  ;;
  ;; wind-post for join*
  ;;
  (let ((proc (join* g)))
    (wind-inner-outer f proc)))

(define (wind* f g h)
  ;;
  ;; wind for join*
  ;;
  (let ((pre (wind-pre* f g)))
    (wind-post* pre h)))

(define (wind-inner-outer inner outer)
  (lambda (x . x*)
    (call-with-values
        (lambda () (apply inner (cons x x*)))
      outer)))

;;;-------------------------------------------------------------------

(define (cps proc)
  ;;
  ;; Return a continuation-passing style variant of a procedure:
  ;;
  ;;     ((cps proc) k . arg*) --> (k (proc . arg*))
  ;;
  (lambda (k . arg*)
    (k (apply proc arg*))))

(define uncps
  ;;
  ;; Return an ordinary variant of a continuation-passing style
  ;; procedure:
  ;;
  ;;     ((uncps proc) . arg*) --> (proc identity . arg*)
  ;;
  (let ((identity (lambda x* (apply values x*))))
    (lambda (proc)
      (lambda arg*
        (apply proc (cons identity arg*))))))

(define (λcps~> proc . proc*)
  ;;
  ;; (λcps~> f g h ...) links a chain of continuation-passing style
  ;; procedures.
  ;;
  ;; λcps~> is proof that continuation-passing style and the thrush
  ;; combinator are equivalent to each other.
  ;;
  (let ((f* (map uncps (cons proc proc*))))
    (cps (apply thrush f*))))

(define lambda-cps~> λcps~>) ;; a synonym.

(define (cps~>* k . val*)
  ;;
  ;; A continuation-passing style equivalent of the thrush*
  ;; combinator:
  ;;
  ;;    ((cps~>* k val1 val2 ...) proc1 proc2 ...)
  ;;
  (let ((local~>* (apply thrush* val*)))
    (lambda proc*
      (let ((f* (map uncps proc*)))
        (k (apply local~>* f*))))))

;;;-------------------------------------------------------------------

(define-syntax define-cps-syntax
  ;;
  ;; Define a continuation-passing style macro. Start from an ordinary
  ;; macro or procedure f:
  ;;
  ;;     (define-cps-syntax cps-macro f) ==>
  ;;
  ;;           (cps-macro k . arg*) --> (k (f . arg*))
  ;;
  (syntax-rules ()
    ((¶ name f)
     (define-syntax name
       (cps-syntax f)))))

(define-syntax define-cpsσ ;; A synonym for define-cps-syntax
  (syntax-rules ()
    ((¶ name f)
     (define-cps-syntax name f))))

(define-syntax define-uncps-syntax
  ;;
  ;; Define an ordinary macro from a continuation-passing style macro
  ;; or procedure f.
  ;;
  ;;     (define-uncps-syntax uncps-macro f) ==>
  ;;
  ;;           (uncps-macro . arg*) --> (f identity . arg*))
  ;;
  (syntax-rules ()
    ((¶ name f)
     (define-syntax name
       (uncps-syntax f)))))

(define-syntax define-uncpsσ ;; A synonym for define-uncps-syntax
  (syntax-rules ()
    ((¶ name f)
     (define-uncps-syntax name f))))

(define-syntax cps-syntax
  ;;
  ;; Create a macro that is in functional continuation-passing style.
  ;; Start from an ordinary macro or procedure f:
  ;;
  ;;     (let-syntax ((cps-macro (cps-syntax f)))
  ;;       (cps-macro k . arg*))      -->     (k (f . arg*))
  ;;
  ;; This is likely not to work if f is a macro, however, if the macro
  ;; is not already expanded and the call to cps-syntax is within the
  ;; macro definition.
  ;;
  (syntax-rules ()
    ((¶ f)
     (syntax-rules ()
       ((µ k . t*)
        (k (f . t*)))))))

(define-syntax cpsσ ;; A synonym for cps-syntax
  (syntax-rules ()
    ((¶ f)
     (cps-syntax f))))

(define-syntax uncps-syntax
  ;;
  ;; Create an ordinary macro from a macro that is in functional
  ;; continuation-passing style.
  ;;
  ;;     (let-syntax ((uncps-syntax-fact (uncps-syntax cps-macro)))
  ;;       (uncps-macro . args*))
  ;;
  (syntax-rules ()
    ((¶ f)
     (syntax-rules ()
       ((µ . t*)
        (let-syntax ((identity (syntax-rules () ((ι τ) τ))))
          (f identity . t*)))))))

(define-syntax uncpsσ ;; A synonym for uncps-syntax
  (syntax-rules ()
    ((¶ f)
     (uncps-syntax f))))

(define-syntax lambda-cps-syntax~>
  ;;
  ;; A syntactic analog of λcps~>
  ;;
  ;; The f* may be both macros and procedures.
  ;;
  (syntax-rules ()
    ((¶ . f*)
     (syntax-rules ()
       ((µ k . val*)
        (lambda-cps-syntax~>-aux f* k val*))))))

(define-syntax lambda-cps-syntax~>-aux
  ;;
  ;; This implementation ignores the usual presentation of
  ;; continuation-passing style. Instead we use equivalence of CPS to
  ;; the thrush combinator. The result, you may notice, is essentially
  ;; the same as the usual presentation that is full of nested
  ;; lambdas. One merely need merge the identity procedures more fully
  ;; with the procedures they wrap (or use identity macros, if
  ;; possible).
  ;;
  ;; This result was not by design. It is one of those things that
  ;; works out when we ignore the complicated stuff too often
  ;; presented to us, and instead seek for ourselves a much simpler
  ;; explanation. I was surprised and very pleased to see such
  ;; simplicity, though it should have been obvious if facts had been
  ;; presented properly. Both the thrush and continuation-passing
  ;; style are merely lining up procedures and passing the output
  ;; value of one as the input to the next. The big nests of lambdas,
  ;; you can see here, arise naturally as consequence of treating an
  ;; identity procedure (or macro) as a kind of ‘standard
  ;; continuation’ that couples procedures.
  ;;
  ;; A corollary is that ‘concatenative’ languages such as Forth also
  ;; are equivalent to continuation-passing style. Indeed, the
  ;; foregoing discussion shows that direct-threaded Forth code
  ;; requires no call stack. It is obvious that tail calls can be
  ;; optimized, and such tail-call optimization is easily done. But
  ;; also, where necessary, continuations can be passed on an operand
  ;; stack and used as ‘gotos’.
  ;;
  ;; Imagine, for instance, a ‘word’ comprising several calls to other
  ;; ‘words’ in sequence. The final call is optimized to a jump. But
  ;; what of the others? Normally they would be compiled to ‘call’
  ;; instructions. But what if, instead, a return address is pushed to
  ;; an operand stack and a ‘jump’ instruction is compiled? Then the
  ;; pushed return address is a continuation, and the code is now in
  ;; continuation-passing style.
  ;;
  ;; But suppose we view the ordinary Forth call stack as being a
  ;; second operand stack, specially for continuations. Then the Forth
  ;; code was in continuation-passing style all along! Except now we
  ;; start to think of doing more interesting things with the call
  ;; stack than we might have thought of before.
  ;;
  ;; This is all that continuation-passing style is. There is no great
  ;; mystery or ugliness to it. Continuation-passing in Scheme is
  ;; explicit passing of return addresses as if they were ordinary
  ;; procedures. This works because Scheme requires tail calls be
  ;; proper.
  ;;
  ;; Now suppose we put an identity procedure at the front of each
  ;; continuation. Except perhaps for execution speed, etc., this
  ;; changes nothing. Then we regroup. We move each of the identity
  ;; procedures to the back of the calling procedure. We have just
  ;; turned continuation-passing style into the thrush combinator.
  ;; Indeed, I think this is a theoretical definition of the thrush
  ;; combinator.
  ;;
  ;; Therefore, where it is more convenient, we can implement
  ;; continuation-passing as the thrush combinator, and vice versa.
  ;;
  ;; And so on.
  ;;
  (syntax-rules ()
    ((¶ () k val*)
     (k . val*))
    ((¶ (f . f*) k val*)
     (lambda-cps-syntax~>-aux
      f* k ((f (lambda v* (apply values v*)) . val*))))))

(define-syntax λcpsσ~> ;; A synonym for lambda-cps-syntax~>
  (syntax-rules ()
    ((¶ k . f*)
     (lambda-cps-syntax~> k . f*))))

(define-syntax cps-syntax~>*
  ;;
  ;; A syntactic analog of cps~>*
  ;;
  (syntax-rules ()
    ((¶ k . val*)
     (syntax-rules ()
       ((µ f . f*)
        (lambda-cps-syntax~>-aux (f . f*) k val*))))))

(define-syntax cpsσ~>* ;; A synonym for cps-syntax~>*
  (syntax-rules ()
    ((¶ k . val*)
     (cps-syntax~>* k . val*))))

;;;-------------------------------------------------------------------

(define-syntax expand-thrush*-syntax
  (syntax-rules (thrush*-syntax σ~>*)

    ((¶ ((thrush*-syntax a ...) b ...))
     (let-syntax ((tmp (thrush*-syntax a ...)))
       (tmp b ...)))

    ((¶ ((σ~>* a ...) b ...))
     (let-syntax ((tmp (σ~>* a ...)))
       (tmp b ...)))

    ((¶ (other b ...))
     (other b ...))

    ))

(define-syntax expandσ~>*
  (syntax-rules ()
    ((¶ (a ...))
     (expand-thrush*-syntax (a ...)))))

(define-syntax expand-cps-syntax~>*
  (syntax-rules (lambda-cps-syntax~> λcpsσ~>)

    ((¶ ((cps-syntax~>* k a ...) b ...))
     (let-syntax ((tmp (cps-syntax~>* k a ...)))
       (expand-cps-syntax~>*-aux (tmp) (b ...))))

    ((¶ ((cpsσ~>* k a ...) b ...))
     (let-syntax ((tmp (cpsσ~>* k a ...)))
       (expand-cps-syntax~>*-aux (tmp) (b ...))))

    ((¶ (other b ...))
     (expand-cps-syntax~>*-aux (other) (b ...)))
    ))

(define-syntax expand-cps-syntax~>*-aux
  (syntax-rules (cps-syntax cpsσ lambda-cps-syntax~> λcpsσ~>)

    ((¶ (op a ...) ())
     (op a ...))

    ((¶ (op a ...) ((cps-syntax obj) b ...))
     (let-syntax ((tmp (cps-syntax obj)))
       (expand-cps-syntax~>*-aux
        (op a ... tmp) (b ...))))

    ((¶ (op a ...) ((cpsσ obj) b ...))
     (let-syntax ((tmp (cpsσ obj)))
       (expand-cps-syntax~>*-aux
        (op a ... tmp) (b ...))))

    ((¶ (op a ...) ((lambda-cps-syntax~> b ...) c ...))
     (expand-cps-syntax~>*-aux
      (op a ...) (b ... c ...)))

    ((¶ (op a ...) ((λcpsσ~> b ...) c ...))
     (expand-cps-syntax~>*-aux
      (op a ...) (b ... c ...)))

    ((¶ (op a ...) (other b ...))
     (expand-cps-syntax~>*-aux (op a ... other) (b ...)))
    ))

(define-syntax expand-cpsσ~>*
  (syntax-rules ()
    ((¶ (a ...))
     (expand-cps-syntax~>* (a ...)))))

(define-syntax bind*
  (syntax-rules (=> thrush*-syntax σ~>* cps-syntax~>* cpsσ~>*)

    ((¶ () body ...)
     (begin (if #f #f) body ...))

    ((¶ ((((thrush*-syntax a ...) b ...) => name ...)
         binding ...)
        body ...)
     (let-values (((name ...) (expand-thrush*-syntax
                               ((thrush*-syntax a ...) b ...))))
       (bind* (binding ...) body ...)))

    ((¶ ((((σ~>* a ...) b ...) => name ...) binding ...)
        body ...)
     (let-values (((name ...) (expandσ~>* ((σ~>* a ...) b ...))))
       (bind* (binding ...) body ...)))

    ((¶ ((((cps-syntax~>* a ...) b ...) => name ...) binding ...)
        body ...)
     (let-values
         (((name ...)
           (call/cc
            (lambda (k1)
              (expand-cps-syntax~>*
               ((cps-syntax~>* k1 a ...) b ...))))))
       (bind* (binding ...) body ...)))

    ((¶ ((((cpsσ~>* a ...) b ...) => name ...) binding ...)
        body ...)
     (let-values
         (((name ...)
           (call/cc
            (lambda (k1)
              (expand-cpsσ~>* ((cpsσ~>* k1 a ...) b ...))))))
       (bind* (binding ...) body ...)))

    ((¶ ((other => name ...) binding ...) body ...)
     (let-values (((name ...) (other)))
       (bind* (binding ...) body ...)))

    ))

;;;-------------------------------------------------------------------
;;;
;;; Branching for continuation-passing style.
;;;

(define-syntax lambda-if
  (syntax-rules ()
    ((¶ kt kf)
     (lambda (x) (if x kt kf)))))

(define-syntax λif
  (syntax-rules ()
    ((¶ kt kf)
     (lambda-if kt kf))))

;;;-------------------------------------------------------------------

(define-syntax alternatives
  ;;
  ;; An analog of Icon’s ‘|’ and ‘every’ operators.
  ;;
  ;; For example,
  ;;
  ;;     (define write-args
  ;;       (lambda args
  ;;         (write args)
  ;;         (newline)))
  ;;
  ;;     (alternatives
  ;;      (1 2 3 4 5)
  ;;      (λcps~> (cps +) (cps write-args))
  ;;      (λcps~> (cps -) (cps write-args))
  ;;      (λcps~> (cps *) (cps write-args))
  ;;      (λcps~> (cps /) (cps write-args)))
  ;;
  ;; with output
  ;;
  ;;     (15)
  ;;     (-13)
  ;;     (120)
  ;;     (1/120)
  ;;
  (syntax-rules ()
    ((¶ arg* f1 ...)
     (let ()
       (define-syntax call-f
         (syntax-rules ()
           ((µ f . a*)
            (call/cc
             (lambda (k^)
               (f k^ . a*))))))
       (if #f #f)
       (call-f f1 . arg*)
       ...))))

(define-cps-syntax cps-alternatives alternatives)

;;;-------------------------------------------------------------------
;;;
;;; Alternative names for let-syntax, letrec-syntax, etc. These
;;; bindings exist so code written in combinator style can have a
;;; consistent look to it. They are not recommended as general
;;; replacements for the more wordy originals.
;;;

(define-syntax letσ ;; A synonym for let-syntax
  (syntax-rules ()
    ((¶ ((keyword transformer-spec) ...) body1 body2 ...)
     (let-syntax ((keyword transformer-spec) ...)
       body1 body2 ...))))

(define-syntax letrecσ ;; A synonym for letrec-syntax
  (syntax-rules ()
    ((¶ ((keyword transformer-spec) ...) body1 body2 ...)
     (letrec-syntax ((keyword transformer-spec) ...)
       body1 body2 ...))))

(define-syntax defineσ ;; A synonym for define-syntax
  (syntax-rules ()
    ((¶ keyword transformer-spec)
     (define-syntax keyword transformer-spec))))

(define-syntax σrules ;; A synonym for syntax-rules
  (syntax-rules-original ()
                         ((¶ (literal ...) rule ...)
                          (syntax-rules-original (literal ...) rule ...))
;;; m4_ifelse(RNRS_NUMBER,«7»,«
                         ((¶ ellipsis (literal ...) rule ...)
                          (syntax-rules-original ellipsis (literal ...) rule ...))
;;; »)
                         ))

;;;-------------------------------------------------------------------
;;;
;;; Composing matchers.
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
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (let ((id1 (first arg*))
             (id2 (second arg*)))
         (if (compare id1 id2)
           (third arg*)
           (fourth arg*)))))))

(define-syntax if-bound-identifier=
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((arg* (cdr form)))
       (let ((id1 (first arg*))
             (id2 (second arg*)))
         (if (eq? id1 id2)
           (third arg*)
           (fourth arg*)))))))

(define-syntax if-...
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

(define-syntax syntax-identity
  (syntax-rules ()
    ((ι τ) τ)))

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

(define-syntax if-unbound-or-equiv-variable
  ;;
  ;; True if obj and var are equivalent bound identifiers.
  ;;
  ;; True if obj is a value and var is unbound. Then var is bound to
  ;; the value of obj.
  ;;
  ;; True if obj is a value var is bound and ‘equiv?’ to the value of
  ;; obj.
  ;;
  ;; Otherwise false.
  ;;
  ;; (In some Scheme implementations, unbound variables do not
  ;; necessarily work as one would hope. Sagittarius is an example.
  ;; Compilation may fail with an ‘unbound identifier’ error, even
  ;; though the code works in other Scheme implementations. Chibi may
  ;; issue an ‘undefined variable’ warning.)
  ;;
  (syntax-rules ()
    ((¶ equiv? obj var kt kf)
     (if-bound-identifier=
      obj var
      kt
      (let ((o obj))
        (call/cc
         (lambda (cc)
           (with-exception-handler
               (lambda (exc)
                 (let ((var o))
                   (cc kt)))
             (lambda ()
               (let ((v var))
                 (if (equiv? o v)
                   kt
                   kf)))))))))))

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
    ((¶ getter! setter! equiv? obj1 obj2 kt kf)
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
    ((¶ equiv? obj var kt kf)
     (let-syntax ((identity
                   (syntax-rules ()
                     ((¶ x) x))))
       (if-default-initialization-or-equiv-object
        identity set! equiv? obj var kt kf)))))

(define-syntax delete-duplicate-identifiers
  ;;
  ;; Deletes duplicate identifiers from a syntactic list. For example:
  ;;
  ;;     (delete-duplicate-identifiers
  ;;      (syntax-identity)
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
  ;;      (old ...) (new ...) (body ...))
  ;;
  ;; creates an environment in which ‘new’ variables to the left of
  ;; any ‘old’ variables are initialized to (default-initialization).
  ;;
  (syntax-rules ()

    ((¶ () () (body ...))
     (let () (if #f #f) body ...))

    ((¶ () (new1 ...) (body ...))
     (let ((dflt (default-initialization)))
       (let ((new1 dflt) ...)
         (if #f #f) body ...)))

    ((¶ (old1 ... oldN) (new1 ... newN) (body ...))
     (make-identifiers-environment-aux
      (old1 ... oldN) (new1 ... newN) (body ...)))))

(define-syntax make-identifiers-environment-aux
  (syntax-rules ()

    ((¶ () (new1 ...) (body ...))
     (let ((dflt (default-initialization)))
       (let ((new1 dflt) ...)
         (if #f #f) body ...)))

    ((¶ (old1 ... oldN) (new1 ... newN) (body ...))
     (make-identifiers-environment-aux
      (old1 ...) (new1 ...) (body ...)))))

(define-syntax syntax-proper-list-length
  ;;
  ;; Calculates the length of a syntactic proper list. This macro is
  ;; in continuation-passing-macro style.
  ;;
  (syntax-rules ()
    ((¶ k* (obj1 ...))
     (syntax-proper-list-length-aux k* (obj1 ...) (+)))))

(define-syntax syntax-proper-list-length-aux
  (syntax-rules ()
    ((¶ (k ...) () n)
     (k ... n))
    ((¶ k* (obj1 obj2 ...) (plus ...))
     (syntax-proper-list-length-aux k* (obj2 ...)
                                    (plus ... 1)))))

(define-syntax match-proper-list
  ;;
  ;; Try to match a proper list. Example:
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
                (syntax-proper-list-length (syntax-identity)
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
      (previous ...)
      (new ...)
      ((call/cc
        (lambda (cc)
          (let ((identity (lambda x* (apply values x*)))
                (p lst))
            (let ((elem (car p)))
              (if-identifier
               var1
               (if-default-initialization-or-equiv-variable
                equal? elem var1 identity (cc kf))
               (unless (equal? elem var1) (cc kf)))
              (set! p (cdr p)))
            ...
            (cc kt)))))))))

(define-syntax match-literal
  ;;
  ;; Try to match a literal. For example:
  ;;
  ;;    (match-literal => =>
  ;;            (display "=>\n")
  ;;            (display "not =>\n"))
  ;;
  (syntax-rules ()
    ((¶ obj literal kt kf)
     (if-bound-identifier= obj literal kt kf))))

(define-syntax split-syntax
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
  ;;    (split-syntax (a b c => 4 5 6)
  ;;                  (match-literal =>)
  ;;                  success-branch
  ;;                  failure-branch)
  ;;
  ;; Running the foregoing prints
  ;;
  ;;    success: (a b c) (=> 4 5 6)
  ;;
  (syntax-rules ()
    ((¶ (item ...) (predicate arg2 ...) succeed fail)
     (split-syntax-aux (predicate arg2 ...) () (item ...)
                       (split-syntax-s1 succeed)
                       (split-syntax-f1 fail)))
    ((¶ #(item ...) (predicate arg2 ...) succeed fail)
     (split-syntax-aux (predicate arg2 ...) () (item ...)
                       (split-syntax-s2 succeed)
                       (split-syntax-f2 fail)))))

(define-syntax split-syntax-s1
  (syntax-rules ()
    ((¶ k (a ...) (b ...))
     (k (a ...) (b ...)))))

(define-syntax split-syntax-f1
  (syntax-rules ()
    ((¶ k (a ...))
    (k (a ...)))))

(define-syntax split-syntax-s2
  (syntax-rules ()
    ((¶ k (a ...) (b ...))
     (k #(a ...) #(b ...)))))

(define-syntax split-syntax-f2
  (syntax-rules ()
    ((¶ k (a ...))
    (k #(a ...)))))

(define-syntax split-syntax-aux
  (syntax-rules ()

    ((¶ (pred arg2 ...) (left1 ...) () (succeed ks*) (fail kf*))
     (fail kf* (left1 ...)))

    ((¶ (pred arg2 ...) (left1 ...) (right1 right2 ...)
        (succeed ks*) (fail kf*))
     (pred right1 arg2 ... 
           (succeed ks* (left1 ...) (right1 right2 ...))
           (split-syntax-aux (pred arg2 ...)
                             (left1 ... right1) (right2 ...)
                             (succeed ks*) (fail kf*))))))

;;;-------------------------------------------------------------------

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
