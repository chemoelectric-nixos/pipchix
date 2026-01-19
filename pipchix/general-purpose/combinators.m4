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

(define-syntax λΣ~> ;; A synonym for thrush-syntax
  (syntax-rules ()
    ((¶ . f*)
     (thrush-syntax . f*))))

(define-syntax lambda-Σ~> ;; A synonym for thrush-syntax
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

(define-syntax Σ~> ;; A synonym for thrush+-syntax
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

(define-syntax Σ~>* ;; A synonym for thrush*-syntax
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

(define-syntax define-cpsΣ ;; A synonym for define-cps-syntax
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

(define-syntax define-uncpsΣ ;; A synonym for define-uncps-syntax
  (syntax-rules ()
    ((¶ name f)
     (define-uncps-syntax name f))))

(define-syntax cps-syntax
  ;;
  ;; Create a continuation-passing style macro. Start from an ordinary
  ;; macro or procedure f:
  ;;
  ;;     (let-syntax ((cps-macro (cps-syntax f)))
  ;;       (cps-macro k . arg*))      -->     (k (f . arg*))
  ;;
  (syntax-rules ()
    ((¶ f)
     (syntax-rules ()
       ((µ k . t*)
        (k (f . t*)))))))

(define-syntax cpsΣ ;; A synonym for cps-syntax
  (syntax-rules ()
    ((¶ f)
     (cps-syntax f))))

(define-syntax uncps-syntax
  ;;
  ;; Create an ordinary macro from a continuation-passing style macro.
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

(define-syntax uncpsΣ ;; A synonym for uncps-syntax
  (syntax-rules ()
    ((¶ f)
     (uncps-syntax f))))

(define-syntax lambda-cps-syntax~>
  ;;
  ;; A syntactic analog of λcps~>
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
      f* k ((f (lambda (v) v) . val*))))))

(define-syntax lambda-cpsΣ~> ;; A synonym for lambda-cps-syntax~>
  (syntax-rules ()
    ((¶ k . f*)
     (lambda-cps-syntax~> k . f*))))

(define-syntax λcpsΣ~> ;; A synonym for lambda-cps-syntax~>
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

(define-syntax cpsΣ~>* ;; A synonym for cps-syntax~>*
  (syntax-rules ()
    ((¶ k . val*)
     (cps-syntax~>* k . val*))))

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
;;;
;;; Alternative names for let-syntax, letrec-syntax, etc. These
;;; bindings exist so code written in combinator style can have a
;;; consistent look to it. They are not recommended as general
;;; replacements for the more wordy originals.
;;;

(define-syntax letΣ ;; A synonym for let-syntax
  (syntax-rules ()
    ((¶ ((keyword transformer-spec) ...) body1 body2 ...)
     (let-syntax ((keyword transformer-spec) ...)
       body1 body2 ...))))

(define-syntax letrecΣ ;; A synonym for letrec-syntax
  (syntax-rules ()
    ((¶ ((keyword transformer-spec) ...) body1 body2 ...)
     (letrec-syntax ((keyword transformer-spec) ...)
       body1 body2 ...))))

(define-syntax defineΣ ;; A synonym for define-syntax
  (syntax-rules ()
    ((¶ keyword transformer-spec)
     (define-syntax keyword transformer-spec))))

(define-syntax Σrules ;; A synonym for syntax-rules
  (syntax-rules-original ()
    ((¶ (literal ...) rule ...)
     (syntax-rules-original (literal ...) rule ...))
;;; m4_ifelse(RNRS_NUMBER,«7»,«
    ((¶ ellipsis (literal ...) rule ...)
     (syntax-rules-original ellipsis (literal ...) rule ...))
;;; »)
    ))

;;;-------------------------------------------------------------------

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
