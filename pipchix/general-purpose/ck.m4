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
;;;-------------------------------------------------------------------
;;;
;;; Faster ck-macros, that can handle diverse Scheme types.
;;;
;;; These are implemented with er-macro-transformer or syntax-case,
;;; instead of in syntax-rules (with some exceptions, such as the ck
;;; abstract machine itself). The ck-macros themselves are for use
;;; with syntax-rules, and can be thought of as primitives.
;;;
;;; However, one should keep in mind that evaluation usually is done
;;; with ‘eval’ rather than by the syntax-rules mechanism. The
;;; evaluation is done during macro expansion (NOT during definition),
;;; in an environment passed around as part of the state variable ‘s’
;;; of the ck-machine.
;;;
;;;-------------------------------------------------------------------

;;; CK ABSTRACT MACHINE

;;; Based on John Croisant’s public domain version for CHICKEN Scheme,
;;; which in turn is based on "Applicative syntax-rules: macros that
;;; compose better" by Oleg Kiselyov
;;; http://okmij.org/ftp/Scheme/macros.html#ck-macros

;;; The `ck' macro implements the CK abstract machine. It does
;;; focusing and refocusing, relying on user-defined CK-macros for
;;; (primitive) reductions.
;;;
;;; The argument `s' is a stack (list) representing pending operations
;;; (CK-macro applications). [In our case it also includes the
;;; environment for ‘eval’.] Each stack frame is a list of the form:
;;;
;;;    ((op va ...) ea ...)
;;;
;;; `op' is the name of a CK-macro that does the reduction. [It may
;;; also be a vector #("prim" proc) representing a procedure.] Zero or
;;; more `va' must all be values (i.e. quoted). Zero or more `ea' are
;;; arbitrary expressions (could be values, or more operations to
;;; apply).
;;;
;;; `ck' is a highly recursive macro with various patterns. To aid
;;; comprehension, the patterns are labeled as steps 0 through 6,
;;; reflecting the overall progression of the recursion. Some steps
;;; repeat or jump around to other steps.
;;;
;;; "arg" is an internal implementation detail, and should not be used
;;; by the caller. It acts as a "mode flag" while the macro is
;;; processing arguments, to avoid conflicting syntax patterns.
;;;

;;; m4_ifelse(scheme_standard,«r6rs»,«
;;; m4_define(«default_environment»,«(environment '(rnrs (6)) '(pipchix general-purpose ck) '(pipchix essentials))»)
;;; »,«
;;; m4_define(«default_environment»,«(environment '(scheme base) '(scheme write) '(pipchix general-purpose list) '(pipchix general-purpose ck) '(pipchix essentials))»)
;;; »)

;;; m4_ifelse (general_macros,«er-macro-transformer»,«

;;;;;;;;;;; FIXME;;;;;;;;;;; FIXME;;;;;;;;;;; FIXME;;;;;;;;;;; FIXME;;;;;;;;;;; FIXME;;;;;;;;;;; FIXME;;;;;;;;;;; FIXME

;; ck is a ck-machine. However the state s is not just a stack. It is
;; also an environment.
(define-syntax ck
  (syntax-rules (quote quasiquote)
    ((ck (s ...) x ...)
     ;; The machine is missing its environment field. Give it one.
     (ck #((s ...) #f) x ...))

    ;; Step 1 is listed last to make the macro work.
    ;; Otherwise its pattern would interfere with steps 4 and 6.

    ;; 0. If the top-level value is quasiquoted, transform directly
    ;; into a call to `c-quasiquote', skipping steps 1-5.
    ((ck s `v)
     (c-quasiquote s 'v))

    ;; 2A. If the next unprocessed arg is quasiquoted, transform
    ;; directly into a call to `c-quasiquote', skipping steps 3-5.
    ;; Otherwise go to step 2B.
    ((ck #(stk env) "arg" (op ...) `v ea1 ...)
     (c-quasiquote #((((op ...) ea1 ...) . stk) env) 'v))

    ;; 2B. If the next unprocessed arg is already a value, move it
    ;; inside and repeat this step. Otherwise go to step 3. (This is
    ;; an optimization to avoid needlessly doing steps 3 and 4.)
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))

    ;; 3. Focus on next arg (`ea'), and push everything else onto the
    ;; stack. If `ea' is a value, go to step 4. Otherwise, go to 5.
    ((ck #(stk env) "arg" (op ...) ea ea1 ...)
     (ck #((((op ...) ea1 ...) . stk) env) ea))

    ;; 4. If the focused arg is a value and there is at least one
    ;; operation on the stack, pop the top operation off the stack,
    ;; move the value inside, and process the remaining unprocessed
    ;; args (`ea ...'), if any. If there are remaining args, go back
    ;; to step 2. If there are no remaining args, go to step 5.
    ((ck #((((op ...) ea ...) . stk) env) 'v)
     (ck #(stk env) "arg" (op ... 'v) ea ...))

    ;; 5. Currently focused on an operation, either from step 1 or 4.
    ;; Either way, it is time to expand the operation.
    ;; The operation will expand into a call to `(ck s x)', where
    ;; `x' is either an operation or a value.
    ;; If `x' is an operation, go back to step 1.
    ;; If `x' is a value, but there are pending operations on the
    ;; stack, go back to step 4.
    ;; Otherwise, if `x' is a value and there are no pending
    ;; operations, go to step 6.
    ((ck #(stk env) "arg" (op va ...))
     (op #(stk env) va ...))

    ;; 6. The stack is empty (no pending operations) and there is an
    ;; ordinary value, so yield the unquoted value as the final
    ;; result.
    ((ck #(() env) 'v)
     v)

    ;; 1. Move the operation's args outside, if it has any. If the
    ;; operation has no args, go to step 5. Otherwise, go to step 2.
    ;; (Use this opportunity to evaluate primitive ck-macros.)
    ((ck #(stk env) (#("prim" code) ea ...))
     (ck #(stk env) "arg"
         ((eval code (or env »default_environment«))) ea ...))
    ((ck #(stk env) (op ea ...))
     (ck #(stk env) "arg" (op) ea ...))

    )) ;; ck

(define-syntax c-quasiquote
  (syntax-rules (quote unquote unquote-splicing)
    ((c-quasiquote s '#(v ...))
     (ck s (c-list->vector (c-quasiquote '(v ...)))))
    ((c-quasiquote s ',v)
     (ck s v))
    ((c-quasiquote s '(,@v . vs))
     (ck s (c-append v (c-quasiquote 'vs))))
    ((c-quasiquote s '(v . vs))
     (ck s (c-cons (c-quasiquote 'v) (c-quasiquote 'vs))))
    ((c-quasiquote s 'v)
     (ck s 'v))
    )) ;; c-quasiquote

;;; »,«

#|
;; ck is a ck-machine. However the state s is not just a stack. It is
;; also an environment.
(define-syntax ck
  (lambda (stx)
    (syntax-case stx (quote quasiquote)
      ((ck (s ...) x ...)
       ;; The machine is missing its environment field. Give it one.
       (syntax (ck #((s ...) #f) x ...)))

      ;; Step 1 is listed last to make the macro work.
      ;; Otherwise its pattern would interfere with steps 4 and 6.

      ;; 0. If the top-level value is quasiquoted, transform directly
      ;; into a call to `c-quasiquote', skipping steps 1-5.
      ((ck s `v)
       (syntax (c-quasiquote s 'v)))

      ;; 2A. If the next unprocessed arg is quasiquoted, transform
      ;; directly into a call to `c-quasiquote', skipping steps 3-5.
      ;; Otherwise go to step 2B.
      ((ck #(stk env) "arg" (op ...) `v ea1 ...)
       (syntax (c-quasiquote #((((op ...) ea1 ...) . stk) env) 'v)))

      ;; 2B. If the next unprocessed arg is already a value, move it
      ;; inside and repeat this step. Otherwise go to step 3. (This is
      ;; an optimization to avoid needlessly doing steps 3 and 4.)
      ((ck s "arg" (op ...) 'v ea1 ...)
       (syntax (ck s "arg" (op ... 'v) ea1 ...)))

      ;; 3. Focus on next arg (`ea'), and push everything else onto the
      ;; stack. If `ea' is a value, go to step 4. Otherwise, go to 5.
      ((ck #(stk env) "arg" (op ...) ea ea1 ...)
       (syntax (ck #((((op ...) ea1 ...) . stk) env) ea)))

      ;; 4. If the focused arg is a value and there is at least one
      ;; operation on the stack, pop the top operation off the stack,
      ;; move the value inside, and process the remaining unprocessed
      ;; args (`ea ...'), if any. If there are remaining args, go back
      ;; to step 2. If there are no remaining args, go to step 5.
      ((ck #((((op ...) ea ...) . stk) env) 'v)
       (syntax (ck #(stk env) "arg" (op ... 'v) ea ...)))

      ;; 5. Currently focused on an operation, either from step 1 or 4.
      ;; Either way, it is time to expand the operation.
      ;; The operation will expand into a call to `(ck s x)', where
      ;; `x' is either an operation or a value.
      ;; If `x' is an operation, go back to step 1.
      ;; If `x' is a value, but there are pending operations on the
      ;; stack, go back to step 4.
      ;; Otherwise, if `x' is a value and there are no pending
      ;; operations, go to step 6.
      ((ck #(stk env) "arg" (op va ...))
       (syntax (op #(stk env) va ...)))

      ;; 6. The stack is empty (no pending operations) and there is an
      ;; ordinary value, so yield the unquoted value as the final
      ;; result.
      ((ck #(() env) 'v)
;;;;;       (syntax v))
       (let* ((environ (or (syntax->datum (syntax env))
                           »default_environment«))
              (val (eval (syntax->datum (syntax 'v)) environ)))
         (datum->syntax (syntax ck) val)))

      ;; 1. Move the operation's args outside, if it has any. If the
      ;; operation has no args, go to step 5. Otherwise, go to step 2.
      ;; (Use this opportunity to evaluate primitive ck-macros.)
      ((ck #(stk env) (#("prim" code) ea ...))
       (let* ((environ (or (syntax->datum (syntax env))
                           »default_environment«))
              (scheme-code (syntax->datum (syntax code)))
              (operation (eval scheme-code environ)))
         (with-syntax ((op (datum->syntax (syntax ck) operation)))
           (syntax (ck #(stk env) "arg" (op) ea ...)))))
      ((ck #(stk env) (op ea ...))
       (syntax (ck #(stk env) "arg" (op) ea ...)))

))) ;; ck
|#

;;; »)

(define-syntax c-quasiquote
  (syntax-rules (quote unquote unquote-splicing)
    ((c-quasiquote s '#(v ...))
     (ck s (c-list->vector (c-quasiquote '(v ...)))))
    ((c-quasiquote s ',v)
     (ck s v))
    ((c-quasiquote s '(,@v . vs))
     (ck s (c-append v (c-quasiquote 'vs))))
    ((c-quasiquote s '(v . vs))
     (ck s (c-cons (c-quasiquote 'v) (c-quasiquote 'vs))))
    ((c-quasiquote s 'v)
     (ck s 'v))
    )) ;; c-quasiquote

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-syntax c-quote
  ;; Add an extra level of quote to an argument.
  (syntax-rules (quote)
    ((¶ s x)
     (ck s 'x))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-syntax define-ck-primitive
  (syntax-rules ()
    ((define-ck-primitive NAME PROC)
;;; m4_ifelse(general_macros,«er-macro-transformer»,«
     (define-syntax NAME
       (er-macro-transformer
        (lambda (form rename compare)
          (let* ((ck (rename 'ck))
                 (s (cadr form))
                 (args (cddr form))
                 ;; One must ignore the ‘s’ argument:
                 (f '(lambda x* (apply PROC (cdr x*)))))
            `(,ck ,s (#("prim" ,f) . ,args))))))
;;; »,«
     (define-syntax NAME
       (lambda (stx)
         (syntax-case stx ()
           ((NAME s . args)
            (quasisyntax
             ;; One must ignore the ‘s’ argument:
             (ck s (#("prim" '(lambda x* (apply PROC (cdr x*))))
                    (unsyntax-splicing (syntax args)))))))))
;;; »)
     )))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-ck-primitive c-cons cons)
(define-ck-primitive c-xcons xcons)
(define-ck-primitive c-cons* cons*)

(define-ck-primitive c-append append)
(define-ck-primitive c-append-reverse append-reverse)

(define-ck-primitive c-car car)
(define-ck-primitive c-cdr cdr)
(define-ck-primitive c-caar caar)
(define-ck-primitive c-cadr cadr)
(define-ck-primitive c-cdar cdar)
(define-ck-primitive c-cddr cddr)
(define-ck-primitive c-caaaar caaaar)
(define-ck-primitive c-caaar caaar)
(define-ck-primitive c-caaddr caaddr)
(define-ck-primitive c-cadaar cadaar)
(define-ck-primitive c-cadar cadar)
(define-ck-primitive c-cadddr cadddr)
(define-ck-primitive c-cdaaar cdaaar)
(define-ck-primitive c-cdaar cdaar)
(define-ck-primitive c-cdaddr cdaddr)
(define-ck-primitive c-cddaar cddaar)
(define-ck-primitive c-cddar cddar)
(define-ck-primitive c-cddddr cddddr)
(define-ck-primitive c-caaadr caaadr)
(define-ck-primitive c-caadar caadar)
(define-ck-primitive c-caadr caadr)
(define-ck-primitive c-cadadr cadadr)
(define-ck-primitive c-caddar caddar)
(define-ck-primitive c-caddr caddr)
(define-ck-primitive c-cdaadr cdaadr)
(define-ck-primitive c-cdadar cdadar)
(define-ck-primitive c-cdadr cdadr)
(define-ck-primitive c-cddadr cddadr)
(define-ck-primitive c-cdddar cdddar)
(define-ck-primitive c-cdddr cdddr)

(define-ck-primitive c-first first)
(define-ck-primitive c-second second)
(define-ck-primitive c-third third)
(define-ck-primitive c-fourth fourth)
(define-ck-primitive c-fifth fifth)
(define-ck-primitive c-sixth sixth)
(define-ck-primitive c-seventh seventh)
(define-ck-primitive c-eighth eighth)
(define-ck-primitive c-ninth ninth)
(define-ck-primitive c-tenth tenth)

(define-ck-primitive c-list->vector list->vector)

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -













;;;;;  
;;;;;  #|
;;;;;  (define-syntax stx
;;;;;    (syntax-rules ()
;;;;;      ((¶ x)
;;;;;       (stx-ck () x))))
;;;;;  
;;;;;  ;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;;;  ;;;
;;;;;  ;;; Some simple ck-macros based on those in John Croisant’s CHICKEN
;;;;;  ;;; egg.
;;;;;  ;;;
;;;;;  
;;;;;  (define-syntax stx-quote
;;;;;    ;; Add an extra level of quote to an argument.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s x)
;;;;;       (stx-ck s 'x))))
;;;;;  
;;;;;  (define-syntax stx-dequote
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s 'x)
;;;;;       (stx-ck s x))))
;;;;;  
;;;;;  (define-syntax stx-identity
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s x)
;;;;;       (stx-ck s x))))
;;;;;  
;;;;;  (define-syntax stx-constantly
;;;;;    ;; Return the first argument constantly.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s X Y ...)
;;;;;       (stx-ck s X))))
;;;;;  
;;;;;  (define-syntax stx-true
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s X ...)
;;;;;       (stx-ck s '#t))))
;;;;;  
;;;;;  (define-syntax stx-false
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s X ...)
;;;;;       (stx-ck s '#f))))
;;;;;  
;;;;;  (define-syntax stx-if
;;;;;    ;; An ‘if’ that evaluates both branches.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s '#f 'pass 'fail)
;;;;;       (stx-ck s 'fail))
;;;;;      ((¶ s otherwise 'pass 'fail)
;;;;;       (stx-ck s 'pass))))
;;;;;  
;;;;;  (define-syntax stx-if*
;;;;;    ;; An ‘if’ that evaluates only the branch taken. The branches must
;;;;;    ;; have an extra level of quoting.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s '#f 'pass 'fail)
;;;;;       (stx-ck s fail))
;;;;;      ((¶ s otherwise 'pass 'fail)
;;;;;       (stx-ck s pass))))
;;;;;  
;;;;;  (define-syntax stx-or
;;;;;    ;; ‘or’ that evaluates eagerly.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s)
;;;;;       (stx-ck s '#f))
;;;;;      ((¶ s 'h)
;;;;;       (stx-ck s 'h))
;;;;;      ;; TODO: Can this be optimized to avoid expanding 'h twice?
;;;;;      ((¶ s 'h . t)
;;;;;       (stx-ck s (stx-if 'h 'h (stx-or . t))))))
;;;;;  
;;;;;  (define-syntax stx-or*
;;;;;    ;; Short-circuiting ‘or’. The arguments must have an extra level of
;;;;;    ;; quoting.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s)
;;;;;       (stx-ck s '#f))
;;;;;      ((¶ s 'h)
;;;;;       (stx-ck s h))
;;;;;      ;; TODO: Can this be optimized to avoid expanding 'h twice?
;;;;;      ((¶ s 'h . t)
;;;;;       (stx-ck s (stx-if* h 'h '(stx-or* . t))))))
;;;;;  
;;;;;  (define-syntax stx-and
;;;;;    ;; ‘and’ that evaluates eagerly.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s)
;;;;;       (stx-ck s '#t))
;;;;;      ((¶ s 'h)
;;;;;       (stx-ck s 'h))
;;;;;      ((¶ s 'h . t)
;;;;;       (stx-ck s (stx-if 'h (stx-and . t) '#f)))))
;;;;;  
;;;;;  (define-syntax stx-and*
;;;;;    ;; Short-circuiting ‘and’. The arguments must have an extra level of
;;;;;    ;; quoting.
;;;;;    (syntax-rules ( quote )
;;;;;      ((¶ s)
;;;;;       (stx-ck s '#t))
;;;;;      ((¶ s 'h)
;;;;;       (stx-ck s h))
;;;;;      ((¶ s 'h . t)
;;;;;  (stx-ck s (stx-if* h '(stx-and* . t) ''#f)))))
;;;;;  
;;;;;  ;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;;;  
;;;;;  ;;;m4_divert(-1)
;;;;;  
;;;;;  m4_define(«stx_output»,«$1»)
;;;;;  m4_dnl          «(if (or (boolean? $1) (number? $1)) `,$1 $1)»)   FIXME   FIXME   FIXME   FIXME   FIXME   FIXME   FIXME   FIXME
;;;;;  
;;;;;  
;;;;;  ;;;m4_define(«one_argument_procedure»,«
;;;;;  m4_pushdef(«NAME»,«$1»)
;;;;;  m4_pushdef(«MACR»,m4_ifelse($2,«»,«stx-$1»,«$1»))
;;;;;  m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;;;  ;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;;;  (define-syntax MACR
;;;;;    (er-macro-transformer
;;;;;     (lambda (form rename compare)
;;;;;       (let ((env eval_environment)
;;;;;             (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;             (s (cadr form))
;;;;;             (args (cddr form))
;;;;;             (f (if (symbol? PROC) (rename PROC) PROC))
;;;;;             (x (Evaluate (car args)))
;;;;;             (y (f x))
;;;;;             (retval `(,(rename stx-ck) ,s ',y)))
;;;;;         stx_output(retval)))))
;;;;;  ;;;»,«
;;;;;  (define-syntax MACR
;;;;;    (lambda (stx)
;;;;;      (syntax-case stx ()
;;;;;        ((¶ s x)
;;;;;         (let* ((env eval_environment)
;;;;;                (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;                (f PROC)
;;;;;                (t (Evaluate (syntax->datum (syntax x))))
;;;;;                (y (datum->syntax (syntax ¶) (f t))))
;;;;;           stx_output((quasisyntax (stx-ck s '(unsyntax y)))))))))
;;;;;  ;;;»)
;;;;;  m4_popdef(«NAME»,«MACR»,«PROC»)
;;;;;  ;;;»)
;;;;;  
;;;;;  ;;;m4_define(«two_argument_procedure»,«
;;;;;  m4_pushdef(«NAME»,«$1»)
;;;;;  m4_pushdef(«MACR»,m4_ifelse($2,«»,«stx-$1»,«$1»))
;;;;;  m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;;;  ;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;;;  (define-syntax MACR
;;;;;    (er-macro-transformer
;;;;;     (lambda (form rename compare)
;;;;;       (let* ((env eval_environment)
;;;;;              (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;              (s (cadr form))
;;;;;              (args (cddr form))
;;;;;              (f (if (symbol? PROC) (rename PROC) PROC))
;;;;;              (x (Evaluate (car args)))
;;;;;              (y (Evaluate (cadr args)))
;;;;;              (z (f x y))
;;;;;              (retval `(,(rename stx-ck) ,s ',z)))
;;;;;         stx_output(retval)))))
;;;;;  ;;;»,«
;;;;;  (define-syntax MACR
;;;;;    (lambda (stx)
;;;;;      (syntax-case stx ()
;;;;;        ((¶ s x y)
;;;;;         (let* ((env eval_environment)
;;;;;                (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;                (f PROC)
;;;;;                (u (Evaluate (syntax->datum (syntax x))))
;;;;;                (v (Evaluate (syntax->datum (syntax y))))
;;;;;                (z (datum->syntax (syntax ¶) (f u v))))
;;;;;           stx_output((quasisyntax (stx-ck s '(unsyntax z)))))))))
;;;;;  ;;;»)
;;;;;  m4_popdef(«NAME»,«MACR»,«PROC»)
;;;;;  ;;;»)
;;;;;  
;;;;;  ;;;m4_define(«general_arguments_procedure»,«
;;;;;  m4_pushdef(«NAME»,«$1»)
;;;;;  m4_pushdef(«MACR»,m4_ifelse($2,«»,«stx-$1»,«$1»))
;;;;;  m4_pushdef(«PROC»,m4_ifelse($2,«»,«$1»,«$2»))
;;;;;  ;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;;;  (define-syntax MACR
;;;;;    (er-macro-transformer
;;;;;     (lambda (form rename compare)
;;;;;       (let* ((env eval_environment)
;;;;;              (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;              (s (cadr form))
;;;;;              (args (cddr form))
;;;;;              (f (if (symbol? PROC) (rename PROC) PROC))
;;;;;              (x* (map Evaluate args))
;;;;;              (z (apply f x*))
;;;;;              (retval `(,(rename stx-ck) ,s ',z)))
;;;;;         stx_output(retval)))))
;;;;;  ;;;»,«
;;;;;  (define-syntax MACR
;;;;;    (lambda (stx)
;;;;;      (syntax-case stx ()
;;;;;        ((¶ s . args)
;;;;;         (let* ((env eval_environment)
;;;;;                (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;                (f PROC)
;;;;;                (x* (map Evaluate (syntax->datum (syntax args))))
;;;;;                (y (apply f x*))
;;;;;                (z (datum->syntax (syntax ¶) y)))
;;;;;           stx_output((quasisyntax (stx-ck s '(unsyntax z)))))))))
;;;;;  ;;;»)
;;;;;  m4_popdef(«NAME»,«MACR»,«PROC»)
;;;;;  ;;;»)
;;;;;  
;;;;;  ;;;m4_divert
;;;;;  
;;;;;  ;;; m4_ifelse(scheme_standard,«r7rs»,«
;;;;;  one_argument_procedure(exact-integer?)
;;;;;  ;;; »,«
;;;;;  (define-syntax stx-exact-integer?
;;;;;    (syntax-rules ()
;;;;;      ((¶ s x)
;;;;;       (stx-ck s (stx-and* (stx-quote (stx-integer? x))
;;;;;                           (stx-quote (stx-exact? x)))))))
;;;;;  ;;; »)
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  
;;;;;  (define-syntax stx-eval
;;;;;    (syntax-rules ()
;;;;;      ((¶ s x)
;;;;;       (stx-ck s '(eval x eval_environment)))
;;;;;      ((¶ s x env)
;;;;;       (stx-ck s '(eval x env)))))
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  
;;;;;  two_argument_procedure(equal?)
;;;;;  two_argument_procedure(eqv?)
;;;;;  two_argument_procedure(eq?)
;;;;;  
;;;;;  one_argument_procedure(symbol->string)
;;;;;  one_argument_procedure(string->symbol)
;;;;;  
;;;;;  general_arguments_procedure(number->string)
;;;;;  general_arguments_procedure(string->number)
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  
;;;;;  one_argument_procedure(number?)
;;;;;  one_argument_procedure(exact?)
;;;;;  one_argument_procedure(inexact?)
;;;;;  one_argument_procedure(integer?)
;;;;;  one_argument_procedure(rational?)
;;;;;  one_argument_procedure(real?)
;;;;;  one_argument_procedure(complex?)
;;;;;  one_argument_procedure(finite?)
;;;;;  one_argument_procedure(infinite?)
;;;;;  one_argument_procedure(nan?)
;;;;;  one_argument_procedure(boolean?)
;;;;;  one_argument_procedure(symbol?)
;;;;;  one_argument_procedure(string?)
;;;;;  one_argument_procedure(char?)
;;;;;  one_argument_procedure(vector?)
;;;;;  one_argument_procedure(bytevector?)
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  
;;;;;  one_argument_procedure(list?)
;;;;;  one_argument_procedure(proper-list?)
;;;;;  one_argument_procedure(circular-list?)
;;;;;  one_argument_procedure(dotted-list?)
;;;;;  one_argument_procedure(pair?)
;;;;;  one_argument_procedure(null?)
;;;;;  one_argument_procedure(null-list?)
;;;;;  one_argument_procedure(not-pair?)
;;;;;  
;;;;;  one_argument_procedure(car)
;;;;;  one_argument_procedure(cdr)
;;;;;  one_argument_procedure(caar)
;;;;;  one_argument_procedure(cadr)
;;;;;  one_argument_procedure(cdar)
;;;;;  one_argument_procedure(cddr)
;;;;;  one_argument_procedure(caaaar)
;;;;;  one_argument_procedure(caaar)
;;;;;  one_argument_procedure(caaddr)
;;;;;  one_argument_procedure(cadaar)
;;;;;  one_argument_procedure(cadar)
;;;;;  one_argument_procedure(cadddr)
;;;;;  one_argument_procedure(cdaaar)
;;;;;  one_argument_procedure(cdaar)
;;;;;  one_argument_procedure(cdaddr)
;;;;;  one_argument_procedure(cddaar)
;;;;;  one_argument_procedure(cddar)
;;;;;  one_argument_procedure(cddddr)
;;;;;  one_argument_procedure(caaadr)
;;;;;  one_argument_procedure(caadar)
;;;;;  one_argument_procedure(caadr)
;;;;;  one_argument_procedure(cadadr)
;;;;;  one_argument_procedure(caddar)
;;;;;  one_argument_procedure(caddr)
;;;;;  one_argument_procedure(cdaadr)
;;;;;  one_argument_procedure(cdadar)
;;;;;  one_argument_procedure(cdadr)
;;;;;  one_argument_procedure(cddadr)
;;;;;  one_argument_procedure(cdddar)
;;;;;  one_argument_procedure(cdddr)
;;;;;  
;;;;;  one_argument_procedure(first)
;;;;;  one_argument_procedure(second)
;;;;;  one_argument_procedure(third)
;;;;;  one_argument_procedure(fourth)
;;;;;  one_argument_procedure(fifth)
;;;;;  one_argument_procedure(sixth)
;;;;;  one_argument_procedure(seventh)
;;;;;  one_argument_procedure(eighth)
;;;;;  one_argument_procedure(ninth)
;;;;;  one_argument_procedure(tenth)
;;;;;  
;;;;;  two_argument_procedure(list-ref)
;;;;;  one_argument_procedure(length)
;;;;;  one_argument_procedure(length+)
;;;;;  
;;;;;  one_argument_procedure(last)
;;;;;  one_argument_procedure(last-pair)
;;;;;  one_argument_procedure(reverse)
;;;;;  two_argument_procedure(take)
;;;;;  two_argument_procedure(drop)
;;;;;  two_argument_procedure(take-right)
;;;;;  two_argument_procedure(drop-right)
;;;;;  
;;;;;  general_arguments_procedure(append)
;;;;;  two_argument_procedure(append-reverse)
;;;;;  one_argument_procedure(concatenate)
;;;;;  general_arguments_procedure(zip)
;;;;;  two_argument_procedure(cons)
;;;;;  two_argument_procedure(xcons)
;;;;;  general_arguments_procedure(cons*)
;;;;;  general_arguments_procedure(list)
;;;;;  one_argument_procedure(list-copy)
;;;;;  general_arguments_procedure(iota)
;;;;;  general_arguments_procedure(circular-list)
;;;;;  general_arguments_procedure(alist-cons)
;;;;;  
;;;;;  one_argument_procedure(list->vector)
;;;;;  general_arguments_procedure(vector->list)
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  
;;;;;  one_argument_procedure(not)
;;;;;  
;;;;;  general_arguments_procedure(+)
;;;;;  general_arguments_procedure(-)
;;;;;  general_arguments_procedure(*)
;;;;;  general_arguments_procedure(/)
;;;;;  one_argument_procedure(abs)
;;;;;  two_argument_procedure(quotient)
;;;;;  two_argument_procedure(remainder)
;;;;;  two_argument_procedure(ceiling-quotient)
;;;;;  two_argument_procedure(ceiling-remainder)
;;;;;  two_argument_procedure(floor-quotient)
;;;;;  two_argument_procedure(floor-remainder)
;;;;;  two_argument_procedure(truncate-quotient)
;;;;;  two_argument_procedure(truncate-remainder)
;;;;;  two_argument_procedure(round-quotient)
;;;;;  two_argument_procedure(round-remainder)
;;;;;  two_argument_procedure(euclidean-quotient)
;;;;;  two_argument_procedure(euclidean-remainder)
;;;;;  two_argument_procedure(balanced-quotient)
;;;;;  two_argument_procedure(balanced-remainder)
;;;;;  
;;;;;  general_arguments_procedure(=)
;;;;;  general_arguments_procedure(<)
;;;;;  general_arguments_procedure(>)
;;;;;  general_arguments_procedure(<=)
;;;;;  general_arguments_procedure(>=)
;;;;;  
;;;;;  one_argument_procedure(zero?)
;;;;;  one_argument_procedure(positive?)
;;;;;  one_argument_procedure(negative?)
;;;;;  one_argument_procedure(odd?)
;;;;;  one_argument_procedure(even?)
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  ;;;
;;;;;  ;;; User-defined fast ck-macros.
;;;;;  ;;;
;;;;;  
;;;;;  (define-syntax define-stx-macro
;;;;;    (syntax-rules ()
;;;;;      ((¶ NAME PROC)
;;;;;       (define-stx-primitive NAME PROC eval_environment))
;;;;;      ((¶ NAME PROC ENV)
;;;;;       (begin
;;;;;  ;;;m4_ifelse(general_macros,«er-macro-transformer»,«
;;;;;         (define-syntax NAME
;;;;;           (er-macro-transformer
;;;;;            (lambda (form rename compare)
;;;;;              (let* ((env ENV)
;;;;;                     (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;                     (stx-ck (rename 'stx-ck))
;;;;;                     (s (cadr form))
;;;;;                     (args (cddr form))
;;;;;                     (f (Evaluate PROC))
;;;;;                     (x* (map Evaluate args))
;;;;;                     (z (apply f x*))
;;;;;                     (retval `(,stx-ck ,s ',z)))
;;;;;                stx_output(retval)))))
;;;;;  ;;;»,«
;;;;;         (define-syntax NAME
;;;;;           (lambda (stx)
;;;;;             (syntax-case stx ()
;;;;;               ((¶ s . args)
;;;;;                (let* ((env ENV)
;;;;;                       (Evaluate (lambda (ϑ) (eval ϑ env)))
;;;;;                       (f (Evaluate PROC))
;;;;;                       (x* (map Evaluate
;;;;;                                (syntax->datum (syntax args))))
;;;;;                       (y (apply f x*))
;;;;;                       (z (datum->syntax (syntax ¶) y)))
;;;;;                  stx_output((quasisyntax
;;;;;                              (stx-ck s '(unsyntax z)))))))))
;;;;;  ;;;»)
;;;;;         ))))
;;;;;  
;;;;;  ;;;-------------------------------------------------------------------
;;;;;  
;;;;;  |#

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; eval: (put 'with-syntax 'scheme-indent-function 1)
;;; eval: (put 'stx-satisfied? 'scheme-indent-function 1)
;;; eval: (put 'stx-integer? 'scheme-indent-function 1)
;;; eval: (put 'stx-exact? 'scheme-indent-function 1)
;;; eval: (put 'stx-list? 'scheme-indent-function 1)
;;; end:
m4_divert«»m4_dnl
