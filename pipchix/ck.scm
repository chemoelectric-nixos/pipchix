
;;; ck-macros
;;; Composable Scheme macros based on the CK abstract machine.
;;; Version 0.3.1 (2024-01-27)
;;;
;;; Based on "Applicative syntax-rules: macros that compose better"
;;; by Oleg Kiselyov
;;; http://okmij.org/ftp/Scheme/macros.html#ck-macros
;;;
;;; `ck' and several CK-macros by Oleg Kiselyov.
;;; Packaged for CHICKEN Scheme by John Croisant.
;;; Based on CK.scm version 1.1 (April 2011).
;;; Documentation and many additional CK-macros by John Croisant.
;;;
;;; If you create a useful or interesting general-purpose CK-macro, or
;;; an improvement to an existing CK-macro, please email
;;; john (at) croisant (dot) net, so I can add it to the library.
;;;
;;; All source code (including contributions) is released by its
;;; authors to the public domain. For more information, please refer
;;; to <http://unlicense.org>
;;;
;;; Credit Oleg Kiselyov:
;;;
;;;   ck            c-quote        c-cons         c-sym-eq?
;;;   c-map1        c-append-map1  c-alist-delete
;;;   c-u+          c-u*
;;;
;;; Credit John Croisant:
;;;
;;;   c-eval        c-call         c-apply
;;;   c-compose     c-rcompose
;;;   c-flip        c-branch
;;;   c-identity    c-constantly
;;;   c-make-rules  c-make-next
;;;   c-not         c-true         c-false
;;;   c-if          c-if*
;;;   c-or          c-or*          c-and          c-and*
;;;   c-null?       c-pair?        c-not-pair?    c-vector?      c-boolean?
;;;   c-sym-eq? (converted to CK-macro)
;;;   c-sym-equal?  c-compare?
;;;   c-cons*       c-xcons
;;;   c-list        c-car          c-cdr
;;;   c-first       c-second       c-third        c-fourth       c-fifth
;;;   c-sixth       c-seventh      c-eighth       c-ninth        c-tenth
;;;   c-last        c-last-pair
;;;   c-drop1       c-drop2        c-drop3        c-drop4        c-drop5
;;;   c-take1       c-take2        c-take3        c-take4        c-take5
;;;   c-reverse     c-prefix       c-suffix
;;;   c-map2        c-map3         c-map4         c-map5
;;;   c-fold1       c-unfold
;;;   c-filter      c-remove
;;;   c-find        c-find-tail
;;;   c-member
;;;   c-any1        c-every1
;;;   c-assoc       c-alist-delete
;;;   c-vector           c-list->vector      c-vector->list
;;;   c-vector-reverse   c-vector-prefix     c-vector-suffix     c-vector-append
;;;   c-vector-map1
;;;   c-u=          c-u<           c-u<=          c-u>           c-u>=
;;;   c-uzero?      c-ueven?       c-uodd?
;;;   c-u* (improved)
;;;   c-u-          c-u/
;;;   c-ufactorial
;;;   c-udrop       c-utake
;;;   c-dadd1       c-dsub1
;;;   c-du          c-ud



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CK ABSTRACT MACHINE

;;; The `ck' macro implements the CK abstract machine. It does
;;; focusing and refocusing, relying on user-defined CK-macros for
;;; (primitive) reductions.
;;;
;;; The argument `s' is a stack (list) representing pending operations
;;; (CK-macro applications). Each stack frame is a list of the form:
;;;
;;;    ((op va ...) ea ...)
;;;
;;; `op' is the name of a CK-macro that does the reduction.
;;; Zero or more `va' must all be values (i.e. quoted).
;;; Zero or more `ea' are arbitrary expressions (could be values, or
;;; more operations to apply).
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
(define-syntax ck
  (syntax-rules (quote quasiquote)
    ;; Step 1 is listed last to make the macro work.
    ;; Otherwise its pattern would interfere with steps 4 and 6.

    ;; 0. If the top-level value is quasiquoted, transform directly
    ;; into a call to `c-quasiquote', skipping steps 1-5.
    ((ck s `v)
     (c-quasiquote s 'v))

    ;; 2A. If the next unprocessed arg is quasiquoted, transform
    ;; directly into a call to `c-quasiquote', skipping steps 3-5.
    ;; Otherwise go to step 2B.
    ((ck s "arg" (op ...) `v ea1 ...)
     (c-quasiquote (((op ...) ea1 ...) . s) 'v))

    ;; 2B. If the next unprocessed arg is already a value, move it
    ;; inside and repeat this step. Otherwise go to step 3. (This is
    ;; an optimization to avoid needlessly doing steps 3 and 4.)
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))

    ;; 3. Focus on next arg (`ea'), and push everything else onto the
    ;; stack. If `ea' is a value, go to step 4. Otherwise, go to 5.
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))

    ;; 4. If the focused arg is a value and there is at least one
    ;; operation on the stack, pop the top operation off the stack,
    ;; move the value inside, and process the remaining unprocessed
    ;; args (`ea ...'), if any. If there are remaining args, go back
    ;; to step 2. If there are no remaining args, go to step 5.
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))

    ;; 5. Currently focused on an operation, either from step 1 or 4.
    ;; Either way, it is time to expand the operation.
    ;; The operation will expand into a call to `(ck s x)', where
    ;; `x' is either an operation or a value.
    ;; If `x' is an operation, go back to step 1.
    ;; If `x' is a value, but there are pending operations on the
    ;; stack, go back to step 4.
    ;; Otherwise, if `x' is a value and there are no pending
    ;; operations, go to step 6.
    ((ck s "arg" (op va ...))
     (op s va ...))

    ;; 6. The stack is empty (no pending operations) and there is a
    ;; value, so yield the unquoted value as the final result.
    ((ck () 'v)
     v)

    ;; 1. Move the operation's args outside, if it has any. If the
    ;; operation has no args, go to step 5. Otherwise, go to step 2.
    ((ck s (op ea ...))
     (ck s "arg" (op) ea ...))))


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
     (ck s 'v))))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; GENERAL

;;; (c-quote X)  ->  'X
;;;
;;; Adds an extra level of quotation to the argument. This is useful
;;; for macros that should expand to a quoted value, and for quoting
;;; operations to use with macros like `c-map1' or `c-find'.
(define-syntax c-quote
  (syntax-rules (quote)
    ((c-quote s 'x)
     (ck s ''x))))


;;; (c-eval '(OP ...))  ->  result
;;;
;;; Takes a quoted operation and unquotes it, allowing the CK machine
;;; to expand it. Analogous to `eval'.
(define-syntax c-eval
  (syntax-rules (quote)
    ((c-eval s '(op ...))
     (ck s (op ...)))))


;;; ;;; (c-call '(OP ...) X ...)  ->  result
;;; ;;;
;;; ;;; Like `c-eval', but adds the given arguments on to the end of the
;;; ;;; operation. Analogous to a lambda call in normal Scheme code.
;;; (define-syntax c-call
;;;   (syntax-rules (quote)
;;;     ((c-call s '(op ...) arg ...)
;;;      (ck s (op ... arg ...)))))
;;; 
;;; 
;;; ;;; (c-apply '(OP ...) X ... '(Y ...))  ->  result
;;; ;;;
;;; ;;; Like `c-call', but the final argument is a list of more arguments.
;;; ;;; The arguments inside the final list should NOT have extra quotes.
;;; ;;; Analogous to `apply'.
;;; (define-syntax c-apply
;;;   (syntax-rules (quote)
;;;     ;; 0. No individual args. Enter "quote" mode, go to 4.
;;;     ((c-apply s '(op ...) '(y ...))
;;;      (c-apply s '(op ...) "quote" '() '(y ...)))
;;; 
;;;     ;; 1. Found at least one individual arg. Enter "join" mode.
;;;     ((c-apply s '(op ...) 'x . more)
;;;      (c-apply s '(op ...) "join" '(x) . more))
;;;     ;; 3. In "join" mode, found final list. Enter "quote" mode, go to 4.
;;;     ((c-apply s '(op ...) "join" '(x ...) '(y ...))
;;;      (c-apply s '(op ...) "quote" '() '(x ... y ...)))
;;;     ;; 2. In "join" mode, collect another individual arg.
;;;     ((c-apply s '(op ...) "join" '(x ...) 'x2 . more)
;;;      (c-apply s '(op ...) "join" '(x ... x2) . more))
;;; 
;;;     ;; 4. In "quote" mode, quote another arg.
;;;     ((c-apply s '(op ...) "quote" '(q ...) '(y . ys))
;;;      (c-apply s '(op ...) "quote" '(q ... 'y) 'ys))
;;;     ;; 5. In "quote" mode, no more args to quote. Done.
;;;     ((c-apply s '(op ...) "quote" '(q ...) '())
;;;      (ck s (op ... q ...)))))
;;; 
;;; 
;;; ;;; (c-compose '((OP-N ...) ... (OP-1 ...)) X ...)  ->  result
;;; ;;;
;;; ;;; Compose one or more CK-macros and apply them to the arguments.
;;; ;;; Calls the right-most OP with arguments X ..., then calls the
;;; ;;; next-right-most OP with that result, and so on:
;;; ;;;
;;; ;;;   (OP-N ... (OP-2 ... (OP-1 ... X ...)))
;;; ;;;
;;; ;;; OP-1 must accept all the Xs as arguments, and the other OPs must
;;; ;;; each accept one argument (the result of the previous operation).
;;; (define-syntax c-compose
;;;   (syntax-rules (quote)
;;;     ((c-compose s '(op ...) x ...)
;;;      (ck s (c-rcompose (c-reverse '(op ...)) x ...)))))
;;; 
;;; 
;;; ;;; (c-rcompose '((OP-1 ...) ... (OP-N ...)) X ...)  ->  result
;;; ;;;
;;; ;;; Like `c-compose', but the operations are called in the reverse
;;; ;;; order (left to right). This is more efficient than `c-compose'.
;;; (define-syntax c-rcompose
;;;   (syntax-rules (quote)
;;;     ((c-rcompose s '() x)
;;;      (ck s x))
;;;     ((c-rcompose s '((op ...) ops ...) x ...)
;;;      (c-rcompose s '(ops ...) (op ... x ...)))))
;;; 
;;; 
;;; ;;; (c-flip '(OP ...) X Y)  ->  (OP ... Y X)
;;; ;;;
;;; ;;; Call (OP ...) with the arguments X and Y, but in reverse order.
;;; (define-syntax c-flip
;;;   (syntax-rules (quote)
;;;     ((c-flip s '(op ...) x y)
;;;      (ck s (op ... y x)))))
;;; 
;;; 
;;; ;;; (c-branch '((OP ...) ...) X ...)  ->  '(result ...)
;;; ;;;
;;; ;;; Yield a list of the results of calling each (OP ... X ...).
;;; (define-syntax c-branch
;;;   (syntax-rules (quote)
;;;     ((c-branch s '() X ...)
;;;      (ck s '()))
;;;     ((c-branch s '((OP ...) MORE ...) X ...)
;;;      (ck s (c-cons (OP ... X ...)
;;;                    (c-branch '(MORE ...) X ...))))))
;;; 
;;; 
;;; ;;; (c-identity X)  ->  X
;;; ;;;
;;; ;;; Simply yields the value as given. Sometimes useful for
;;; ;;; higher-order macros like `c-filter'.
;;; (define-syntax c-identity
;;;   (syntax-rules (quote)
;;;     ((c-identity s 'v)
;;;      (ck s 'v))))
;;; 
;;; 
;;; ;;; (c-constantly X Y ...)  ->  X
;;; ;;;
;;; ;;; Always yields X, regardless of the other arguments.
;;; ;;; May be useful for some higher-order macros.
;;; (define-syntax c-constantly
;;;   (syntax-rules (quote)
;;;     ((c-constantly s X Y ...)
;;;      (ck s X))))
;;; 
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; CK-MACRO BUILDERS
;;; 
;;; ;;; (c-make-rules '(L ...) '(P X) ...)  ->  '(syntax-rules ...)
;;; ;;;
;;; ;;; Given a list of zero or more literal symbols, and one or more
;;; ;;; pattern/expression lists, yields a syntax-rules form for a
;;; ;;; CK-macro with the following behavior:
;;; ;;;
;;; ;;;   Given argument(s) that match any pattern P, yields the value of
;;; ;;;   the associated expression X, which may use identifiers from P.
;;; ;;;   Fails if no pattern matches.
;;; ;;;
;;; ;;; Each pattern P is a list of zero or more sub-patterns, which will
;;; ;;; be matched (as with syntax-rules) against the already-evaluated
;;; ;;; and quoted arguments given to the new CK-macro. Alternatively, P
;;; ;;; may be an identifier which will capture all arguments as a list.
;;; ;;;
;;; ;;; Each expression X is a single CK-macro expression or quoted value.
;;; ;;; Identifiers from the pattern can be used in the expression, as
;;; ;;; with syntax-rules.
;;; ;;;
;;; ;;; Symbols in the literals list (L ...) will be treated as literal
;;; ;;; identifiers in patterns. Additionally, "quote" is always treated
;;; ;;; as a literal identifier.
;;; ;;;
;;; ;;; Caveats:
;;; ;;; - Using "..." in a pattern or expression may not work portably
;;; ;;;   with all Scheme systems.
;;; ;;; - Symbols begining with "%ck:" are reserved for internal use and
;;; ;;;   must not appear in any L, P, or X.
;;; ;;;
;;; (define-syntax c-make-rules
;;;   (syntax-rules (quote)
;;;     ((c-make-rules s '(LIT ...) '(P X) ...)
;;;      (ck s (c-cons* 'syntax-rules '(quote LIT ...)
;;;                     (c-map2 '(c-list)
;;;                             (c-map1 '(c-cons* '%ck:_ '%ck:s)
;;;                                     '(P ...))
;;;                             (c-map1 '(c-list 'ck '%ck:s)
;;;                                     '(X ...))))))))
;;; 
;;; 
;;; ;;; (c-make-next '(X1 X2 ...))  ->  '(syntax-rules ...)
;;; ;;;
;;; ;;; Given a list of items, yields a syntax-rules form for a CK-macro
;;; ;;; with the following behavior:
;;; ;;;
;;; ;;;   Given an item in the list, yields the item following it.
;;; ;;;   Yields '#f if given the final item or an item not in the list.
;;; ;;;
;;; ;;; E.g. with a list of increasing integers, the CK-macro behaves
;;; ;;; like c-dadd1. With decreasing integers, it behaves like c-dsub1.
;;; ;;;
;;; ;;; The list must have at least two items, with no duplicates. The
;;; ;;; items should be literal constants: booleans, numbers, strings,
;;; ;;; characters; or (possibly nested) pairs, lists, or vectors of those
;;; ;;; things. Symbols are allowed but the result may not be what you
;;; ;;; expect because they are treated as identifiers in the patterns.
;;; ;;;
;;; ;;; Be advised that compilation can be very slow if there are many
;;; ;;; items because it generates a syntax-rules with many branches.
;;; (define-syntax c-make-next
;;;   (syntax-rules (quote)
;;;     ((c-make-next %s '(X1 X2 MORE ...))
;;;      (ck %s
;;;        (c-apply '(c-make-rules '())
;;;                 (c-suffix
;;;                  (c-map2 '(c-list)
;;;                          (c-map1 '(c-list) '('X1 'X2 'MORE ...))
;;;                          '('X2 'MORE ...))
;;;                  '((otherwise) '#f)))))))
;;; 
;;; 
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; BOOLEAN LOGIC
;;; 
;;; ;;; (c-not X)  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields '#t if the argument is '#f, otherwise yields '#t.
;;; ;;; Analogous to `not'.
;;; (define-syntax c-not
;;;   (syntax-rules (quote)
;;;     ((c-not s '#f)
;;;      (ck s '#t))
;;;     ((c-not s otherwise)
;;;      (ck s '#f))))
;;; 
;;; 
;;; ;;; (c-true X ...)  ->  '#t
;;; ;;;
;;; ;;; Always yields '#t, regardless of its arguments.
;;; ;;; May be useful for some higher-order macros.
;;; ;;; Equivalent to (c-constantly '#t X ...).
;;; (define-syntax c-true
;;;   (syntax-rules (quote)
;;;     ((c-true s X ...)
;;;      (ck s '#t))))
;;; 
;;; 
;;; ;;; (c-false X ...)  ->  '#f
;;; ;;;
;;; ;;; Always yields '#f, regardless of its arguments.
;;; ;;; May be useful for some higher-order macros.
;;; ;;; Equivalent to (c-constantly '#f X ...).
;;; (define-syntax c-false
;;;   (syntax-rules (quote)
;;;     ((c-false s X ...)
;;;      (ck s '#f))))
;;; 
;;; 
;;; ;;; (c-if TEST PASS FAIL)  ->  PASS or FAIL
;;; ;;;
;;; ;;; Conditional branching. If TEST is '#f, this yields FAIL. Otherwise
;;; ;;; it yields PASS.
;;; ;;;
;;; ;;; Due to the way the CK machine works, both branches will be
;;; ;;; expanded, then the unneeded branch will be discarded. This is
;;; ;;; analogous to (lambda (test pass fail) (if test pass fail)).
;;; ;;;
;;; ;;; If you only want the needed branch to be expanded (e.g. because
;;; ;;; the branches are complex and slow to expand, or because it would
;;; ;;; be an error to expand the unneeded branch), use `c-if*' instead.
;;; (define-syntax c-if
;;;   (syntax-rules (quote)
;;;     ((c-if s '#f 'pass 'fail)           ; If #f, expand to fail.
;;;      (ck s 'fail))
;;;     ((c-if s otherwise 'pass 'fail)     ; Else, expand to pass.
;;;      (ck s 'pass))))
;;; 
;;; 
;;; ;;; (c-if* TEST 'PASS 'FAIL)  ->  PASS or FAIL
;;; ;;;
;;; ;;; Similar to `c-if', except that the branches must have an extra
;;; ;;; level of quoting, and only one branch will be expanded. This is
;;; ;;; more similar to how `if' behaves, but it is a bit awkward to use.
;;; ;;; Analogous to
;;; ;;; (lambda (test pass fail) (if test (eval pass) (eval fail)))
;;; (define-syntax c-if*
;;;   (syntax-rules (quote)
;;;     ((c-if* s '#f 'pass 'fail)
;;;      (ck s fail))
;;;     ((c-if* s otherwise 'pass 'fail)
;;;      (ck s pass))))
;;; 
;;; 
;;; ;;; (c-or X ...)  ->  item or '#f
;;; ;;;
;;; ;;; Yields the first argument that is not '#f. Yields '#f if all
;;; ;;; arguments are '#f, or if there are no arguments.
;;; ;;;
;;; ;;; All arguments will be expanded, but at most one will be kept.
;;; ;;; In other words, there is no short circuiting. If you only want to
;;; ;;; expand the arguments as needed, use `c-or*' instead.
;;; (define-syntax c-or
;;;   (syntax-rules (quote)
;;;     ((c-or s)
;;;      (ck s '#f))
;;;     ((c-or s 'h)
;;;      (ck s 'h))
;;;     ;; TODO: Can this be optimized to avoid expanding 'h twice?
;;;     ((c-or s 'h . t)
;;;      (ck s (c-if 'h 'h (c-or . t))))))
;;; 
;;; 
;;; ;;; (c-or* 'X ...)  ->  item or '#f
;;; ;;;
;;; ;;; Similar to `c-or', but all arguments must have an extra level of
;;; ;;; quoting, and the arguments will be expanded one at a time until a
;;; ;;; non-'#f value is found. This is more similar to how `or' behaves,
;;; ;;; but it is a bit awkward to use.
;;; (define-syntax c-or*
;;;   (syntax-rules (quote)
;;;     ((c-or* s)
;;;      (ck s '#f))
;;;     ((c-or* s 'h)
;;;      (ck s h))
;;;     ;; TODO: Can this be optimized to avoid expanding 'h twice?
;;;     ((c-or* s 'h . t)
;;;      (ck s (c-if* h 'h '(c-or* . t))))))
;;; 
;;; 
;;; ;;; (c-and X ...)  ->  item or '#f
;;; ;;;
;;; ;;; If all arguments are not '#f, yields the last argument. If any of
;;; ;;; the arguments are '#f, yields '#f. If there are no arguments,
;;; ;;; yields '#t.
;;; ;;;
;;; ;;; All arguments will be expanded, but at most one will be kept.
;;; ;;; In other words, there is no short circuiting. If you only want to
;;; ;;; expand the arguments as needed, use `c-and*' instead.
;;; (define-syntax c-and
;;;   (syntax-rules (quote)
;;;     ((c-and s)
;;;      (ck s '#t))
;;;     ((c-and s 'h)
;;;      (ck s 'h))
;;;     ((c-and s 'h . t)
;;;      (ck s (c-if 'h (c-and . t) '#f)))))
;;; 
;;; 
;;; ;;; (c-and* 'X ...)  ->  item or '#f
;;; ;;;
;;; ;;; Similar to `c-and', but all the arguments must have an extra level
;;; ;;; of quoting, and the arguments will be expanded one at a time until
;;; ;;; a '#f value is found. This is more similar to how `and' behaves,
;;; ;;; but it is a bit awkward to use.
;;; (define-syntax c-and*
;;;   (syntax-rules (quote)
;;;     ((c-and* s)
;;;      (ck s '#t))
;;;     ((c-and* s 'h)
;;;      (ck s h))
;;;     ((c-and* s 'h . t)
;;;      (ck s (c-if* h '(c-and* . t) ''#f)))))
;;; 
;;; 
;;; ;;; (c-null? X)  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields '#t if the argument is the empty list, '().
;;; ;;; Otherwise yields '#f. Analogous to `null?'.
;;; (define-syntax c-null?
;;;   (syntax-rules (quote)
;;;     ((c-null? s '())
;;;      (ck s '#t))
;;;     ((c-null? s otherwise)
;;;      (ck s '#f))))
;;; 
;;; 
;;; ;;; (c-pair? X)  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields '#t if the argument is a dotted pair or a non-empty list.
;;; ;;; Otherwise yields '#f.
;;; ;;; Analogous to `pair?'.
;;; (define-syntax c-pair?
;;;   (syntax-rules (quote)
;;;     ((c-pair? s '(h . t))
;;;      (ck s '#t))
;;;     ((c-pair? s otherwise)
;;;      (ck s '#f))))
;;; 
;;; 
;;; ;;; (c-not-pair? X)  ->  '#t or '#f
;;; ;;;
;;; ;;; Opposite of `c-pair?'. Analogous to `not-pair?' from SRFI 1.
;;; (define-syntax c-not-pair?
;;;   (syntax-rules (quote)
;;;     ((c-not-pair? s x)
;;;      (ck s (c-not (c-pair? x))))))
;;; 
;;; 
;;; ;;; (c-vector? X)  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields '#t if the argument is a vector.
;;; ;;; Otherwise yields '#f. Analogous to `vector?'.
;;; (define-syntax c-vector?
;;;   (syntax-rules (quote)
;;;     ((c-vector? s '#(x ...))
;;;      (ck s '#t))
;;;     ((c-vector? s otherwise)
;;;      (ck s '#f))))
;;; 
;;; 
;;; ;;; (c-boolean? X)  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields '#t if the argument is either '#t or '#f.
;;; ;;; Otherwise yields '#f. Analogous to `boolean?'.
;;; (define-syntax c-boolean?
;;;   (syntax-rules (quote)
;;;     ((c-boolean? s '#t)
;;;      (ck s '#t))
;;;     ((c-boolean? s '#f)
;;;      (ck s '#t))
;;;     ((c-boolean? s otherwise)
;;;      (ck s '#f))))
;;; 
;;; 
;;; ;;; (c-sym-eq? X Y)  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields '#t if X and Y are the same symbol, otherwise yields '#f.
;;; ;;; X should be a symbol. Y can be any value. Some Scheme impls allow
;;; ;;; X to be other types, but it is only portable if X is a symbol.
;;; ;;; Based on `symbol-eq?' from the paper.
;;; ;;;
;;; ;;; NOTE: You should not use c-sym-eq?, or any macro that uses it
;;; ;;; directly or indirectly, in any code that expands into a definition
;;; ;;; (of a variable, procedure, etc.), because that definition will not
;;; ;;; be visible from outside. This is because definitions that occur
;;; ;;; within the body of let-syntax are internal, not global.
;;; (define-syntax c-sym-eq?
;;;   (syntax-rules (quote)
;;;     ((c-sym-eq? s 'x 'y)
;;;      (let-syntax ((check (syntax-rules (x quote)
;;;                            ((check x)
;;;                             (ck s '#t))
;;;                            ((check otherwise)
;;;                             (ck s '#f)))))
;;;        (check y)))))
;;; 
;;; 
;;; ;;; (c-sym-equal? X Y)  ->  '#t or '#f
;;; ;;;
;;; ;;; Similar to `c-sym-eq?', but recursively compares pairs, lists, and
;;; ;;; vectors. Roughly analogous to `equal?', except it only works
;;; ;;; (portably) with symbols, pairs, lists, vectors, and nested
;;; ;;; combinations of those things.
;;; ;;;
;;; ;;; Same as (c-compare? '(c-sym-eq?) X Y).
;;; ;;; See the notes for c-sym-eq? for pitfalls.
;;; (define-syntax c-sym-equal?
;;;   (syntax-rules (quote)
;;;     ((c-sym-equal? s 'a 'b)
;;;      (c-compare? s '(c-sym-eq?) 'a 'b))))
;;; 
;;; 
;;; ;;; (c-compare? '(OP ...) X Y)  ->  value or '#f
;;; ;;;
;;; ;;; Recursively compares atoms, pairs, lists, or vectors, using OP as
;;; ;;; the predicate to compare corresponding atoms. OP will be called
;;; ;;; with two arguments: an atom of X, and the corresponding atom of Y.
;;; ;;; I.e. the Nth atom of X will be compared with the Nth atom of Y,
;;; ;;; descending recursively into nested structures. If X and Y are
;;; ;;; themselves atoms, they are compared directly.
;;; ;;;
;;; ;;; Yields '#f if X and Y have dissimilar structures (length, nesting,
;;; ;;; type), or if OP yields '#f for any corresponding atoms of X and Y.
;;; ;;; Otherwise yields '#t.
;;; ;;;
;;; ;;; Similar to `c-sym-equal?' but with a custom predicate for
;;; ;;; comparing atoms.
;;; (define-syntax c-compare?
;;;   (syntax-rules (quote)
;;;     ;; List versus vector, and vice versa
;;;     ((c-compare? s '(pred? ...) '(A ...) '#(B ...))
;;;      (ck s '#f))
;;;     ((c-compare? s '(pred? ...) '#(A ...) '(B ...))
;;;      (ck s '#f))
;;; 
;;;     ;; Lists and pairs
;;;     ((c-compare? s '(pred? ...) '() '())
;;;      (ck s '#t))
;;;     ((c-compare? s '(pred? ...) '(a . A) '())
;;;      (ck s '#f))
;;;     ((c-compare? s '(pred? ...) '() '(b . B))
;;;      (ck s '#f))
;;;     ((c-compare? s '(pred? ...) '(a . A) '(b . B))
;;;      (ck s (c-and* '(c-compare? '(pred? ...) 'a 'b)
;;;                    '(c-compare? '(pred? ...) 'A 'B))))
;;; 
;;;     ;; Vectors
;;;     ((c-compare? s '(pred? ...) '#() '#())
;;;      (ck s '#t))
;;;     ((c-compare? s '(pred? ...) '#(a A ...) '#())
;;;      (ck s '#f))
;;;     ((c-compare? s '(pred? ...) '#() '#(b B ...))
;;;      (ck s '#f))
;;;     ((c-compare? s '(pred? ...) '#(a A ...) '#(b B ...))
;;;      (ck s (c-and* '(c-compare? '(pred? ...) 'a 'b)
;;;                    '(c-compare? '(pred? ...) '#(A ...) '#(B ...)))))
;;; 
;;;     ;; Atom
;;;     ((c-compare? s '(pred? ...) 'a 'b)
;;;      (ck s (c-and (pred? ... 'a 'b) '#t)))))
;;; 
;;; 
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; LIST PROCESSING

;;; (c-cons X Y)  ->  '(X . Y)
;;;
;;; Yields a pair with the two given arguments. Analogous to `cons'.
(define-syntax c-cons
  (syntax-rules (quote)
    ((c-cons s 'h 't)
     (ck s '(h . t)))))


;;; ;;; (c-cons* X ... Y Z)  ->  '(X ... Y . Z)
;;; ;;;
;;; ;;; Yields a list from consing all the arguments together in a chain.
;;; ;;; If the final argument Z is a list, the result will be a proper
;;; ;;; list. Otherwise it will be a "dotted list".
;;; ;;; Analogous to `cons*' from SRFI 1.
;;; (define-syntax c-cons*
;;;   (syntax-rules (quote)
;;;     ((c-cons* s 'x 'z)
;;;      (ck s '(x . z)))
;;;     ((c-cons* s 'x 'y . more)
;;;      (ck s (c-cons 'x (c-cons* 'y . more))))))
;;; 
;;; 
;;; ;;; (c-xcons X Y)  ->  '(Y . X)
;;; ;;;
;;; ;;; Like c-cons, but but exchanges the order of arguments.
;;; ;;; Analogous to `xcons' from SRFI 1.
;;; (define-syntax c-xcons
;;;   (syntax-rules (quote)
;;;     ((c-xcons s 't 'h)
;;;      (ck s '(h . t)))))
;;; 
;;; 
;;; ;;; (c-list X ...)  ->  '(X ...)
;;; ;;;
;;; ;;; Yields a list with the given arguments. Analogous to `list'.
;;; (define-syntax c-list
;;;   (syntax-rules (quote)
;;;     ((c-list s 'x ...)
;;;      (ck s '(x ...)))))
;;; 
;;; 
;;; ;;; (c-car P)  ->  item
;;; ;;;
;;; ;;; Yields the head of the given pair. Analogous to `car'.
;;; (define-syntax c-car
;;;   (syntax-rules (quote)
;;;     ((c-car s '(h . t))
;;;      (ck s 'h))))
;;; 
;;; ;;; (c-cdr P)  ->  tail
;;; ;;;
;;; ;;; Yields the tail of the given pair. Analogous to `cdr'.
;;; (define-syntax c-cdr
;;;   (syntax-rules (quote)
;;;     ((c-cdr s '(h . t))
;;;      (ck s 't))))
;;; 
;;; 
;;; ;;; (c-first  L)  ->  item
;;; ;;; (c-second L)  ->  item
;;; ;;; ...
;;; ;;; (c-tenth  L)  ->  item
;;; ;;;
;;; ;;; Yields an item of the given list. Fails if the list is too short.
;;; ;;; Analogous to `first' ... `tenth' from SRFI 1.
;;; (define-syntax c-first
;;;   (syntax-rules (quote)
;;;     ((c-first s '(a . t))
;;;      (ck s 'a))))
;;; 
;;; (define-syntax c-second
;;;   (syntax-rules (quote)
;;;     ((c-second s '(a b . t))
;;;      (ck s 'b))))
;;; 
;;; (define-syntax c-third
;;;   (syntax-rules (quote)
;;;     ((c-third s '(a b c . t))
;;;      (ck s 'c))))
;;; 
;;; (define-syntax c-fourth
;;;   (syntax-rules (quote)
;;;     ((c-fourth s '(a b c d . t))
;;;      (ck s 'd))))
;;; 
;;; (define-syntax c-fifth
;;;   (syntax-rules (quote)
;;;     ((c-fifth s '(a b c d e . t))
;;;      (ck s 'e))))
;;; 
;;; (define-syntax c-sixth
;;;   (syntax-rules (quote)
;;;     ((c-sixth s '(a b c d e f . t))
;;;      (ck s 'f))))
;;; 
;;; (define-syntax c-seventh
;;;   (syntax-rules (quote)
;;;     ((c-seventh s '(a b c d e f g . t))
;;;      (ck s 'g))))
;;; 
;;; (define-syntax c-eighth
;;;   (syntax-rules (quote)
;;;     ((c-eighth s '(a b c d e f g h . t))
;;;      (ck s 'h))))
;;; 
;;; (define-syntax c-ninth
;;;   (syntax-rules (quote)
;;;     ((c-ninth s '(a b c d e f g h i . t))
;;;      (ck s 'i))))
;;; 
;;; (define-syntax c-tenth
;;;   (syntax-rules (quote)
;;;     ((c-tenth s '(a b c d e f g h i j . t))
;;;      (ck s 'j))))
;;; 
;;; 
;;; ;;; (c-last L)  ->  item
;;; ;;; Yields the last value of the given list. Fails if the list is
;;; ;;; empty or is not a proper list. Analogous to `last' from SRFI 1.
;;; (define-syntax c-last
;;;   (syntax-rules (quote)
;;;     ((c-last s '(h))
;;;      (ck s 'h))
;;;     ((c-last s '(h . t))
;;;      (c-last s 't))))
;;; 
;;; ;;; (c-last-pair L)  ->  pair
;;; ;;; Yields the last pair of the given list. Fails if the list is
;;; ;;; empty. Analogous to `last-pair' from SRFI 1.
;;; (define-syntax c-last-pair
;;;   (syntax-rules (quote)
;;;     ((c-last-pair s '(x h . t))
;;;      (c-last-pair s '(h . t)))
;;;     ((c-last-pair s '(h . t))
;;;      (ck s '(h . t)))))
;;; 
;;; 
;;; ;;; (c-drop1 L)  ->  list
;;; ;;; (c-drop2 L)  ->  list
;;; ;;; ...
;;; ;;; (c-drop5 L)  ->  list
;;; ;;;
;;; ;;; Drops a predefined number of items from the front of the list.
;;; ;;; Fails if the list is too short.
;;; ;;; Analogous to `(drop L N)' from SRFI 1.
;;; (define-syntax c-drop1
;;;   (syntax-rules (quote)
;;;     ((c-drop1 arg ...)
;;;      (c-cdr arg ...))))
;;; 
;;; (define-syntax c-drop2
;;;   (syntax-rules (quote)
;;;     ((c-drop2 s '(a b . t))
;;;      (ck s 't))))
;;; 
;;; (define-syntax c-drop3
;;;   (syntax-rules (quote)
;;;     ((c-drop3 s '(a b c . t))
;;;      (ck s 't))))
;;; 
;;; (define-syntax c-drop4
;;;   (syntax-rules (quote)
;;;     ((c-drop4 s '(a b c d . t))
;;;      (ck s 't))))
;;; 
;;; (define-syntax c-drop5
;;;   (syntax-rules (quote)
;;;     ((c-drop5 s '(a b c d e . t))
;;;      (ck s 't))))
;;; 
;;; 
;;; ;;; (c-take1 L)  ->  list
;;; ;;; (c-take2 L)  ->  list
;;; ;;; ...
;;; ;;; (c-take5 L)  ->  list
;;; ;;;
;;; ;;; Takes a predefined number of items from the front of the list.
;;; ;;; Fails if the list is too short.
;;; ;;; Analogous to `(take L N)' from SRFI 1.
;;; (define-syntax c-take1
;;;   (syntax-rules (quote)
;;;     ((c-take2 s '(a . t))
;;;      (ck s '(a)))))
;;; 
;;; (define-syntax c-take2
;;;   (syntax-rules (quote)
;;;     ((c-take2 s '(a b . t))
;;;      (ck s '(a b)))))
;;; 
;;; (define-syntax c-take3
;;;   (syntax-rules (quote)
;;;     ((c-take3 s '(a b c . t))
;;;      (ck s '(a b c)))))
;;; 
;;; (define-syntax c-take4
;;;   (syntax-rules (quote)
;;;     ((c-take4 s '(a b c d . t))
;;;      (ck s '(a b c d)))))
;;; 
;;; (define-syntax c-take5
;;;   (syntax-rules (quote)
;;;     ((c-take5 s '(a b c d e . t))
;;;      (ck s '(a b c d e)))))
;;; 
;;; 
;;; ;;; (c-reverse L)  ->  list
;;; ;;;
;;; ;;; Yields the given list in reverse order. Fails if the list is not a
;;; ;;; proper list. Analogous to `reverse'.
;;; (define-syntax c-reverse
;;;   (syntax-rules (quote)
;;;     ((c-reverse s 'lst)                 ; Add accum arg if missing.
;;;      (c-reverse s 'lst '()))
;;;     ((c-reverse s '() 'accum)           ; Empty list, so yield accum.
;;;      (ck s 'accum))
;;;     ((c-reverse s '(h . t) 'accum)      ; Move h onto accum and recur.
;;;      (c-reverse s 't '(h . accum)))))
;;; 
;;; 
;;; ;;; (c-prefix L X ...)  ->  list
;;; ;;;
;;; ;;; Yields the given list with the extra arguments added to the front.
;;; ;;; (c-prefix '(3 4) '1 '2) is equivalent to (c-append '(1 2) '(3 4)).
;;; (define-syntax c-prefix
;;;   (syntax-rules (quote)
;;;     ((c-prefix s '(y ...) 'x ...)
;;;      (ck s '(x ... y ...)))))
;;; 
;;; 
;;; ;;; (c-suffix L X ...)  ->  list
;;; ;;;
;;; ;;; Yields the given list with the extra arguments added to the end.
;;; ;;; (c-suffix '(1 2) '3 '4) is equivalent to (c-append '(1 2) '(3 4)).
;;; (define-syntax c-suffix
;;;   (syntax-rules (quote)
;;;     ((c-suffix s '(x ...) 'y ...)
;;;      (ck s '(x ... y ...)))))


;;; (c-append L ...)  ->  list
;;;
;;; Yields a list containing the elements of the given lists.
;;; Analogous to `append'.
(define-syntax c-append
  (syntax-rules (quote)
    ((c-append s)
     (ck s '()))
    ((c-append s '(a ...))
     (ck s '(a ...)))
    ((c-append s '(a ...) '(b ...))
     (ck s '(a ... b ...)))
    ((c-append s '(a ...) '(b ...) . more)
     (ck s (c-append '(a ... b ...) . more)))))


;;; ;;; (c-append-map1 '(OP ...) L)  ->  list
;;; ;;;
;;; ;;; Yields a list by calling the quoted operation on each item in the
;;; ;;; list, then appending the results. The operation must be a CK-macro
;;; ;;; that yields a list. Analogous to `append-map' from SFRI-1, but
;;; ;;; only accepts one list. This was named `c-concatMap' in the paper.
;;; (define-syntax c-append-map1
;;;   (syntax-rules (quote)
;;;     ((c-append-map1 s 'op '())
;;;      (ck s '()))
;;;     ((c-append-map1 s '(op ...) '(h . t))
;;;      (ck s (c-append (op ... 'h) (c-append-map1 '(op ...) 't))))))
;;; 
;;; 
;;; ;;; (c-map1 '(OP ...) L)  ->  list
;;; ;;;
;;; ;;; Yields a list by calling the quoted operation on each item in the
;;; ;;; given list. Analogous to `map', but only accepts one list.
;;; (define-syntax c-map1
;;;   (syntax-rules (quote)
;;;     ((c-map1 s '(op ...) '(h . t))
;;;      (ck s (c-cons (op ... 'h) (c-map1 '(op ...) 't))))
;;;     ((c-map1 s 'op '())
;;;      (ck s '()))))
;;; 
;;; 
;;; ;;; (c-map2 '(OP ...) L1 L2)  ->  list
;;; ;;;
;;; ;;; Like `c-map1' but accepts exactly two lists. OP must accept two
;;; ;;; extra arguments. If the lists are different lengths, terminates
;;; ;;; when the shortest list runs out. Analogous to `map' from SRFI 1,
;;; ;;; but accepts exactly two lists.
;;; (define-syntax c-map2
;;;   (syntax-rules (quote)
;;;     ((c-map2 s '(op ...) '(h1 . t1) '(h2 . t2))
;;;      (ck s (c-cons (op ... 'h1 'h2)
;;;                    (c-map2 '(op ...) 't1 't2))))
;;;     ((c-map2 s 'op '() '(x2 ...))
;;;      (ck s '()))
;;;     ((c-map2 s 'op '(x1 ...) '())
;;;      (ck s '()))))
;;; 
;;; 
;;; ;;; (c-map3 '(OP ...) L1 L2 L3)  ->  list
;;; ;;;
;;; ;;; Like `c-map2' but accepts exactly three lists.
;;; ;;; OP must accept three extra arguments.
;;; (define-syntax c-map3
;;;   (syntax-rules (quote)
;;;     ((c-map3 s '(op ...) '(h1 . t1) '(h2 . t2) '(h3 . t3))
;;;      (ck s (c-cons (op ... 'h1 'h2 'h3)
;;;                    (c-map3 '(op ...) 't1 't2 't3))))
;;;     ((c-map3 s 'op '() '(x2 ...) '(x3 ...))
;;;      (ck s '()))
;;;     ((c-map3 s 'op '(x1 ...) '() '(x3 ...))
;;;      (ck s '()))
;;;     ((c-map3 s 'op '(x1 ...) '(x2 ...) '())
;;;      (ck s '()))))
;;; 
;;; 
;;; ;;; (c-map4 '(OP ...) L1 L2 L3 L4)  ->  list
;;; ;;;
;;; ;;; Like `c-map2' but accepts exactly four lists.
;;; ;;; OP must accept four extra arguments.
;;; (define-syntax c-map4
;;;   (syntax-rules (quote)
;;;     ((c-map4 s '(op ...) '(h1 . t1) '(h2 . t2) '(h3 . t3) '(h4 . t4))
;;;      (ck s (c-cons (op ... 'h1 'h2 'h3 'h4)
;;;                    (c-map4 '(op ...) 't1 't2 't3 't4))))
;;;     ((c-map4 s 'op '() '(x2 ...) '(x3 ...) '(x4 ...))
;;;      (ck s '()))
;;;     ((c-map4 s 'op '(x1 ...) '() '(x3 ...) '(x4 ...))
;;;      (ck s '()))
;;;     ((c-map4 s 'op '(x1 ...) '(x2 ...) '() '(x4 ...))
;;;      (ck s '()))
;;;     ((c-map4 s 'op '(x1 ...) '(x2 ...) '(x3 ...) '())
;;;      (ck s '()))))
;;; 
;;; 
;;; ;;; (c-map5 '(OP ...) L1 L2 L3 L4 L5)  ->  list
;;; ;;;
;;; ;;; Like `c-map2' but accepts exactly five lists.
;;; ;;; OP must accept five extra arguments.
;;; (define-syntax c-map5
;;;   (syntax-rules (quote)
;;;     ((c-map5 s '(op ...) '(h1 . t1) '(h2 . t2) '(h3 . t3) '(h4 . t4) '(h5 . t5))
;;;      (ck s (c-cons (op ... 'h1 'h2 'h3 'h4 'h5)
;;;                    (c-map5 '(op ...) 't1 't2 't3 't4 't5))))
;;;     ((c-map5 s 'op '() '(x2 ...) '(x3 ...) '(x4 ...) '(x5 ...))
;;;      (ck s '()))
;;;     ((c-map5 s 'op '(x1 ...) '() '(x3 ...) '(x4 ...) '(x5 ...))
;;;      (ck s '()))
;;;     ((c-map5 s 'op '(x1 ...) '(x2 ...) '() '(x4 ...) '(x5 ...))
;;;      (ck s '()))
;;;     ((c-map5 s 'op '(x1 ...) '(x2 ...) '(x3 ...) '() '(x5 ...))
;;;      (ck s '()))
;;;     ((c-map5 s 'op '(x1 ...) '(x2 ...) '(x3 ...) '(x4 ...) '())
;;;      (ck s '()))))
;;; 
;;; 
;;; ;;; (c-fold1 '(OP ...) INIT L)  ->  result
;;; ;;;
;;; ;;; Yield a value by repeatedly calling the quoted operation with
;;; ;;; each item from the list plus the previous result.
;;; ;;; The operation is first called with two arguments, the first item
;;; ;;; of the list and the initial value. Then, the operation is called
;;; ;;; with the next item of the list and the previous result. If list is
;;; ;;; empty, yields the initial value. Analogous to `fold' from SRFI 1,
;;; ;;; but only accepts one list.
;;; (define-syntax c-fold1
;;;   (syntax-rules (quote)
;;;     ((c-fold1 s 'op 'v '())
;;;      (ck s 'v))
;;;     ((c-fold1 s '(op ...) 'v '(h . t))
;;;      (ck s (c-fold1 '(op ...) (op ... 'h 'v) 't)))))
;;; 
;;; 
;;; ;;; (c-unfold '(P ...) '(F ...) '(G ...) SEED)  ->  list
;;; ;;; (c-unfold '(P ...) '(F ...) '(G ...) SEED '(TAIL-GEN ...))  ->  list
;;; ;;;
;;; ;;; Generate a list by recursively "unfolding" from a seed.
;;; ;;; Analogous to "unfold" from SRFI 1.
;;; ;;;
;;; ;;; Takes several operations which are called with the seed:
;;; ;;;
;;; ;;; * (P ... SEED) should yield '#t if it is time to stop generating.
;;; ;;; * (F ... SEED) should yield an item to be appended to the list.
;;; ;;; * (G ... SEED) should yield the next SEED.
;;; ;;; * (TAIL-GEN ... SEED) should yield the tail of the list.
;;; ;;;   Called when generation stops, i.e. when (P ... SEED) yields '#t.
;;; ;;;   If omitted, '(TAIL-GEN ...) defaults to '(c-constantly '()).
;;; ;;;
;;; (define-syntax c-unfold
;;;   (syntax-rules (quote)
;;;     ((c-unfold s '(P ...) '(F ...) '(G ...) SEED '(TAIL-GEN ...))
;;;      (ck s (c-if* (P ... SEED)
;;;                   '(TAIL-GEN ... SEED)
;;;                   '(c-cons (F ... SEED)
;;;                            (c-unfold '(P ...) '(F ...) '(G ...)
;;;                                      (G ... SEED) '(TAIL-GEN ...))))))
;;;     ;; Default TAIL-GEN.
;;;     ((c-unfold s '(P ...) '(F ...) '(G ...) SEED)
;;;      (c-unfold s '(P ...) '(F ...) '(G ...) SEED '(c-constantly '())))))
;;; 
;;; 
;;; ;;; (c-filter '(OP ...) L)  ->  list
;;; ;;;
;;; ;;; Yields a list by calling the quoted operation on each item in the
;;; ;;; given list, and discarding any item for which the test yields '#f.
;;; ;;; Analogous to `filter' from SRFI 1.
;;; (define-syntax c-filter
;;;   (syntax-rules (quote)
;;;     ((c-filter s 'pred 'lst)            ; Add accum arg if missing.
;;;      (c-filter s 'pred 'lst '()))
;;;     ((c-filter s 'pred '() 'accum)      ; No more items in list,
;;;      (ck s (c-reverse 'accum)))         ; so reverse and yield accum.
;;;     ((c-filter s '(pred ...) '(h . t) 'accum)
;;;      ;; Check the first remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so move h onto accum and recur.
;;;                   '(c-filter '(pred ...) 't '(h . accum))
;;;                   ;; Failed, so recur without h.
;;;                   '(c-filter '(pred ...) 't 'accum))))))
;;; 
;;; 
;;; ;;; (c-remove '(OP ...) L)  ->  list
;;; ;;;
;;; ;;; Opposite of `c-filter'. Discards items that pass the test, keeps
;;; ;;; items that fail the test. Analogous to `remove' from SRFI 1.
;;; (define-syntax c-remove
;;;   (syntax-rules (quote)
;;;     ((c-remove s 'pred 'lst)            ; Add accum arg if missing.
;;;      (c-remove s 'pred 'lst '()))
;;;     ((c-remove s 'pred '() 'accum)      ; No more items in list,
;;;      (ck s (c-reverse 'accum)))         ; so reverse and yield accum.
;;;     ((c-remove s '(pred ...) '(h . t) 'accum)
;;;      ;; Check the first remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so recur without h.
;;;                   '(c-remove '(pred ...) 't 'accum)
;;;                   ;; Failed, so move h onto accum and recur.
;;;                   '(c-remove '(pred ...) 't '(h . accum)))))))
;;; 
;;; 
;;; ;;; (c-find '(OP ...) L)  ->  item or '#f
;;; ;;;
;;; ;;; Yields the first item in the given list that passes the predicate
;;; ;;; operation (i.e. the predicate yields a non-'#f value). Yields '#f
;;; ;;; if no items pass the predicate. Analogous to `find' from SRFI 1.
;;; (define-syntax c-find
;;;   (syntax-rules (quote)
;;;     ((c-find s 'pred '())               ; No more items in list,
;;;      (ck s '#f))                        ; so yield #f.
;;;     ((c-find s '(pred ...) '(h . t))
;;;      ;; Check the first remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so yield h.
;;;                   ''h
;;;                   ;; Failed, so recur without h.
;;;                   '(c-find '(pred ...) 't))))))
;;; 
;;; 
;;; ;;; (c-find-tail '(OP ...) L)  ->  pair or '#f
;;; ;;;
;;; ;;; Yields the first pair in the list for which the head of the pair
;;; ;;; passes the predicate operation (i.e. the predicate yields a
;;; ;;; non-'#f value). Yields '#f if no items pass the predicate.
;;; ;;; Analogous to `find-tail' from SRFI 1.
;;; (define-syntax c-find-tail
;;;   (syntax-rules (quote)
;;;     ((c-find-tail s 'pred '())          ; No more items in list,
;;;      (ck s '#f))                        ; so yield #f.
;;;     ((c-find-tail s '(pred ...) '(h . t))
;;;      ;; Check the first remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so yield the list.
;;;                   ''(h . t)
;;;                   ;; Failed, so recur without h.
;;;                   '(c-find-tail '(pred ...) 't))))))
;;; 
;;; 
;;; ;;; (c-member X L)  ->  '#t or '#f
;;; ;;; (c-member X L '(OP ...))  ->  '#t or '#f
;;; ;;;
;;; ;;; Yields the first pair in the list for which the head of the pair
;;; ;;; is X, or '#f if the list does not contain X. Uses '(OP ...) for
;;; ;;; comparison, or '(c-sym-equal?) if '(OP ...) is omitted.
;;; ;;; See the notes for c-sym-eq? for pitfalls.
;;; ;;;
;;; ;;; Same as (c-find-tail '(OP ... X) L). Analogous to `member' from
;;; ;;; SRFI 1 except for the default comparison predicate.
;;; (define-syntax c-member
;;;   (syntax-rules (quote)
;;;     ((c-member s 'x 'lst)               ; Fill in optional op
;;;      (c-member s 'x 'lst '(c-sym-equal?)))
;;;     ((c-member s 'x 'lst '(op ...))
;;;      (ck s (c-find-tail '(op ... 'x) 'lst)))))
;;; 
;;; 
;;; ;;; (c-any1 '(OP ...) L)  ->  result or '#f
;;; ;;;
;;; ;;; Calls the operation on each value in the given list until it finds
;;; ;;; a result that is not '#f, then yields that result. Yields '#f if
;;; ;;; the predicate yields '#f for all items in the list, or if the list
;;; ;;; is empty. Analogous to `any' from SRFI 1, but only accepts one
;;; ;;; list.
;;; (define-syntax c-any1
;;;   (syntax-rules (quote)
;;;     ((c-any1 s 'pred '())               ; No more items in list,
;;;      (ck s '#f))                        ; so yield #f.
;;;     ((c-any1 s '(pred ...) '(h . t))
;;;      ;; Check the first remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so yield result of applying pred.
;;;                   ;; TODO: Can it be optimized to not do pred twice?
;;;                   '(pred ... 'h)
;;;                   ;; Failed, so recur without h.
;;;                   '(c-any1 '(pred ...) 't))))))
;;; 
;;; 
;;; ;;; (c-every1 '(OP ...) L)  ->  result or '#f
;;; ;;;
;;; ;;; Calls the operation on each value in the given list until it finds
;;; ;;; a result that is '#f, then yields '#f. If the predicate yields a
;;; ;;; non-'#f value for every item in the list, this yields the result
;;; ;;; of calling the predicate on the last item. Yields '#t if the list
;;; ;;; is empty. Analogous to `every' from SRFI 1, but only accepts one
;;; ;;; list.
;;; (define-syntax c-every1
;;;   (syntax-rules (quote)
;;;     ((c-every1 s 'pred '())             ; Empty list,
;;;      (ck s '#t))                        ; so yield #t.
;;;     ((c-every1 s '(pred ...) '(h))
;;;      ;; Check the only remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so yield result of applying pred.
;;;                   ;; TODO: Can it be optimized to not do pred twice?
;;;                   '(pred ... 'h)
;;;                   ;; Failed, so yield #f.
;;;                   ''#f)))
;;;     ((c-every1 s '(pred ...) '(h . t))
;;;      ;; Check the first remaining item, h.
;;;      (ck s (c-if* (pred ... 'h)
;;;                   ;; Passed, so recur without h.
;;;                   '(c-every1 '(pred ...) 't)
;;;                   ;; Failed, so yierd #f.
;;;                   ''#f)))))
;;; 
;;; 
;;; ;;; (c-assoc KEY ALIST)  ->  pair or '#f
;;; ;;; (c-assoc KEY ALIST '(OP ...))  ->  pair or '#f
;;; ;;;
;;; ;;; Yields the first pair in ALIST whose car matches KEY, or '#f if no
;;; ;;; match is found.
;;; ;;; Uses '(OP ...) for comparison, or '(c-sym-equal?) if '(OP ...)
;;; ;;; is omitted. See the notes for c-sym-eq? for pitfalls.
;;; ;;; Analogous to `assoc' from SRFI 1.
;;; (define-syntax c-assoc
;;;   (syntax-rules (quote)
;;;     ((c-assoc s 'key 'alist)            ; Fill in omitted op
;;;      (c-assoc s 'key 'alist '(c-sym-equal?)))
;;;     ((c-assoc s 'key '() '(op ...))
;;;      (ck s '#f))
;;;     ((c-assoc s 'key '((h . e) . t) '(op ...))
;;;      (ck s (c-if* (op ... 'key 'h)
;;;                   ''(h . e)
;;;                   '(c-assoc 'key 't))))))
;;; 
;;; 
;;; ;;; (c-alist-delete KEY ALIST)  ->  list
;;; ;;; (c-alist-delete KEY ALIST '(OP ...))  ->  list
;;; ;;;
;;; ;;; Removes all pairs in ALIST whose car matches KEY. Uses '(OP ...)
;;; ;;; for comparison, or '(c-sym-equal?) if '(OP ...) is omitted.
;;; ;;; See the notes for c-sym-eq? for pitfalls.
;;; ;;; Analogous to `alist-delete' from SRFI 1.
;;; ;;; Based on `c-delete-assoc' from the paper.
;;; (define-syntax c-alist-delete
;;;   (syntax-rules (quote)
;;;     ((c-alist-delete s 'key 'alist)
;;;      (c-alist-delete s 'key 'alist '(c-sym-equal?)))
;;;     ((c-alist-delete s 'key '() '(op ...))
;;;      (ck s '()))
;;;     ((c-alist-delete s 'key '((h . e) . t) '(op ...))
;;;      (ck s (c-if* (op ... 'key 'h)
;;;                   '(c-alist-delete 'key 't)
;;;                   '(c-cons '(h . e)
;;;                            (c-alist-delete 'key 't '(op ...))))))))
;;; 
;;; 
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; VECTOR PROCESSING
;;; 
;;; ;;; (c-vector X ...)  ->  '#(X ...)
;;; ;;;
;;; ;;; Yields a vector with the given arguments. Analogous to `vector'.
;;; (define-syntax c-vector
;;;   (syntax-rules (quote)
;;;     ((c-vector s 'x ...)
;;;      (ck s '#(x ...)))))


;;; (c-list->vector L)  ->  vector
;;;
;;; Yields a vector containing the same elements as the given list.
;;; Analogous to `list->vector' from SRFI 43.
(define-syntax c-list->vector
  (syntax-rules (quote)
    ((c-list->vector s '(x ...))
     (ck s '#(x ...)))))


;;; ;;; (c-vector->list V)  ->  list
;;; ;;;
;;; ;;; Yields a list containing the same elements as the given vector.
;;; ;;; Analogous to `vector->list' from SRFI 43.
;;; (define-syntax c-vector->list
;;;   (syntax-rules (quote)
;;;     ((c-vector->list s '#(x ...))
;;;      (ck s '(x ...)))))
;;; 
;;; 
;;; ;;; (c-vector-reverse V)  ->  vector
;;; ;;;
;;; ;;; Yields a vector with the same elements in the reverse order.
;;; ;;; Similar to `vector-reverse-copy' from SRFI 43, but does not take a
;;; ;;; start or end argument.
;;; (define-syntax c-vector-reverse
;;;   (syntax-rules (quote)
;;;     ((c-vector-reverse s '#(x ...))
;;;      (ck s (c-list->vector (c-reverse '(x ...)))))))
;;; 
;;; 
;;; ;;; (c-vector-prefix V X ...)  ->  vector
;;; ;;;
;;; ;;; Yields the given vector with the extra arguments added to the front.
;;; ;;; (c-vector-prefix '#(3 4) '1 '2) is equivalent to
;;; ;;; (c-vector-append '#(1 2) '#(3 4)).
;;; (define-syntax c-vector-prefix
;;;   (syntax-rules (quote)
;;;     ((c-vector-prefix s '#(a ...) 'x ...)
;;;      (ck s '#(x ... a ...)))))
;;; 
;;; 
;;; ;;; (c-vector-suffix V X ...)  ->  vector
;;; ;;;
;;; ;;; Yields the given vector with the extra arguments added to the end.
;;; ;;; (c-vector-suffix '#(1 2) '3 '4) is equivalent to
;;; ;;; (c-vector-append '#(1 2) '#(3 4)).
;;; (define-syntax c-vector-suffix
;;;   (syntax-rules (quote)
;;;     ((c-vector-suffix s '#(a ...) 'x ...)
;;;      (ck s '#(a ... x ...)))))
;;; 
;;; 
;;; ;;; (c-vector-append V ...)  ->  vector
;;; ;;;
;;; ;;; Yields a vector containing the elements of the given vectors.
;;; ;;; Analogous to `vector-append' from SRFI 43.
;;; (define-syntax c-vector-append
;;;   (syntax-rules (quote)
;;;     ((c-vector-append s)
;;;      (ck s '#()))
;;;     ((c-vector-append s '#(a ...))
;;;      (ck s '#(a ...)))
;;;     ((c-vector-append s '#(a ...) '#(b ...))
;;;      (ck s '#(a ... b ...)))
;;;     ((c-vector-append s '#(a ...) '#(b ...) . more)
;;;      (ck s (c-vector-append '#(a ... b ...) . more)))))
;;; 
;;; 
;;; ;;; (c-vector-map1 '(OP ...) V)  ->  vector
;;; ;;;
;;; ;;; Yields a vector by calling the quoted operation on each item in
;;; ;;; the given vector. Analogous to `vector-map' from SRFI 43, but
;;; ;;; only accepts one vector.
;;; (define-syntax c-vector-map1
;;;   (syntax-rules (quote)
;;;     ((c-vector-map1 s 'op '#())
;;;      (ck s '#()))
;;;     ((c-vector-map1 s '(op ...) '#(x ...))
;;;      (ck s (c-list->vector (c-map1 '(op ...) '(x ...)))))))
;;; 
;;; 
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; UNARY MATH
;;; ;;;
;;; ;;; Unary numbers encode non-negative integers as a list of a certain
;;; ;;; length. For example, the list '(a b c d e) means the number 5, and
;;; ;;; the list '() means the number 0. The contents of the list do not
;;; ;;; matter, only the length. Negative numbers and non-integral numbers
;;; ;;; cannot be represented in unary.
;;; ;;;
;;; ;;; Unary math is pretty slow, but it is interesting, portable, and
;;; ;;; maybe even useful in some cases.
;;; 
;;; 
;;; ;;; (c-u= U1 U2)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary equality. Yields '#t if the two lists have the same lengths,
;;; ;;; otherwise yields '#f.
;;; (define-syntax c-u=
;;;   (syntax-rules (quote)
;;;     ((c-u= s '() '())                   ; Both lists are empty,
;;;      (ck s '#t))                        ; so they are same length.
;;;     ((c-u= s '(xh . xt) '())            ; Only one list is empty,
;;;      (ck s '#f))                        ; so not same length.
;;;     ((c-u= s '() '(yh . yt))            ; Only one list is empty,
;;;      (ck s '#f))                        ; so not same length.
;;;     ((c-u= s '(xh . xt) '(yh . yt))     ; Neither list is empty,
;;;      (c-u= s 'xt 'yt))))                ; so recur with the tails.
;;; 
;;; 
;;; ;;; (c-u< U1 U2)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary less-than. Yields '#t if the first list is shorter than the
;;; ;;; second list, otherwise yields '#f.
;;; (define-syntax c-u<
;;;   (syntax-rules (quote)
;;;     ((c-u< s '() '())                   ; Both lists are empty,
;;;      (ck s '#f))                        ; so first is not less.
;;;     ((c-u< s '(xh . xt) '())            ; Only second is empty,
;;;      (ck s '#f))                        ; so first is not less.
;;;     ((c-u< s '() '(yh . yt))            ; Only first list is empty,
;;;      (ck s '#t))                        ; so first is less.
;;;     ((c-u< s '(xh . xt) '(yh . yt))     ; Neither list is empty,
;;;      (c-u< s 'xt 'yt))))                ; so recur with the tails.
;;; 
;;; 
;;; ;;; (c-u<= U1 U2)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary less-than-or-equals. Yields '#t if first list is the same
;;; ;;; length or shorter than the second list, otherwise yields '#f.
;;; (define-syntax c-u<=
;;;   (syntax-rules (quote)
;;;     ((c-u<= s '() '())                  ; Both lists are empty,
;;;      (ck s '#t))                        ; so they are equal.
;;;     ((c-u<= s '(xh . xt) '())           ; Only second is empty,
;;;      (ck s '#f))                        ; so first is greater.
;;;     ((c-u<= s '() '(yh . yt))           ; Only first list is empty,
;;;      (ck s '#t))                        ; so first is less.
;;;     ((c-u<= s '(xh . xt) '(yh . yt))    ; Neither list is empty,
;;;      (c-u<= s 'xt 'yt))))               ; so recur with the tails.
;;; 
;;; 
;;; ;;; (c-u> U1 U2)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary greater-than. Yields '#t if the first list is longer than
;;; ;;; the second list, otherwise yields '#f.
;;; (define-syntax c-u>
;;;   (syntax-rules (quote)
;;;     ((c-u> s 'x 'y)
;;;      (ck s (c-not (c-u<= 'x 'y))))))
;;; 
;;; 
;;; ;;; (c-u>= U1 U2)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary greater-than-or-equals. Yields '#t if first list is same
;;; ;;; length or longer than the second list, otherwise yields '#f.
;;; (define-syntax c-u>=
;;;   (syntax-rules (quote)
;;;     ((c-u>= s 'x 'y)
;;;      (ck s (c-not (c-u< 'x 'y))))))
;;; 
;;; 
;;; ;;; (c-uzero? U)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary "zero?". Yields '#t if the list is empty, otherwise yields
;;; ;;; '#f. Same as `c-null?'.
;;; (define-syntax c-uzero?
;;;   (syntax-rules (quote)
;;;     ((c-uzero? s 'x)
;;;      (c-null? s 'x))))
;;; 
;;; 
;;; ;;; (c-ueven? U)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary "even?". Yields '#t if the given list's length is even (i.e.
;;; ;;; a multiple of 2), otherwise yields '#f.
;;; (define-syntax c-ueven?
;;;   (syntax-rules (quote)
;;;     ((c-ueven? s '())                   ; No items remaining,
;;;      (ck s '#t))                        ; so it is even.
;;;     ((c-ueven? s '(x))                  ; One item remaining,
;;;      (ck s '#f))                        ; so it is odd.
;;;     ((c-ueven? s '(x y . more))         ; Two or more items remaining,
;;;      (c-ueven? s 'more))))              ; so drop first two and recur.
;;; 
;;; 
;;; ;;; (c-uodd? U)  ->  '#t or '#f
;;; ;;;
;;; ;;; Unary "odd?". Yields '#t if the given list's length is odd length
;;; ;;; (i.e. not a multiple of 2), otherwise yields '#f.
;;; (define-syntax c-uodd?
;;;   (syntax-rules (quote)
;;;     ((c-uodd? s 'lst)
;;;      (ck s (c-not (c-ueven? 'lst))))))
;;; 
;;; 
;;; ;;; (c-u+ U1 U2)  ->  list
;;; ;;;
;;; ;;; Unary addition. Same as `c-append'.
;;; ;;; This was named `c-add' in the paper.
;;; (define-syntax c-u+
;;;   (syntax-rules (quote)
;;;     ((c-u+ . args)
;;;      (c-append . args))))
;;; 
;;; 
;;; ;;; (c-u- U1 U2)  ->  list
;;; ;;;
;;; ;;; Unary subtraction. Drops an element from the front of the first
;;; ;;; list for each element in second list, then yields the remaining
;;; ;;; list. Negative numbers cannot be represented in unary, so this
;;; ;;; yields '() if the second list is equal or longer than the first.
;;; (define-syntax c-u-
;;;   (syntax-rules (quote)
;;;     ((c-u- s '() 'y)                    ; Cannot go negative.
;;;      (ck s '()))
;;;     ((c-u- s 'x '())                    ; x - 0 = x
;;;      (ck s 'x))
;;;     ((c-u- s '(xh . xt) '(yh . yt))     ; Drop one from each list.
;;;      (c-u- s 'xt 'yt))))
;;; 
;;; 
;;; ;;; (c-u* U1 U2)  ->  list
;;; ;;;
;;; ;;; Unary multiplication. Yields a list containing the contents of the
;;; ;;; first list, repeated once for every item in the second list. Based
;;; ;;; on `c-mul' from the paper, except the symbol 'u has no special
;;; ;;; significance, and result is made from duplicating the first list.
;;; (define-syntax c-u*
;;;   (syntax-rules (quote)
;;;     ((c-u* s '() '(y ...))              ; 0 * y = 0
;;;      (ck s '()))
;;;     ((c-u* s '(x ...) '())              ; x * 0 = 0
;;;      (ck s '()))
;;;     ((c-u* s '(x ...) '(yh . yt))       ; x * y = x + x * (y-1)
;;;      (ck s (c-u+ '(x ...) (c-u* '(x ...) 'yt))))))
;;; 
;;; 
;;; ;;; (c-u/ U1 U2)  ->  list
;;; ;;;
;;; ;;; Unary division. Yields a list of two unary numbers, representing
;;; ;;; the quotient and the remainder of the division. Given the second
;;; ;;; list has length N, the quotient will contain every Nth item from
;;; ;;; the first list, and the remainder will contain the tail of the
;;; ;;; first list. Division by zero (empty list) is a syntax error.
;;; (define-syntax c-u/
;;;   (syntax-rules (quote)
;;;     ((c-u/ s '(x ...) '(yh . yt))       ; Add q (quotient accum).
;;;      (c-u/ s '(x ...) '(yh . yt) '()))
;;;     ((c-u/ s '() '(yh . yt) 'q)         ; x is 0,
;;;      (ck s '(q ())))                    ; so yield q and 0 remainder
;;;     ((c-u/ s '(xh . xt) '(yh . yt) 'q)
;;;      ;; If x < y (i.e. x is shorter than y)
;;;      (ck s (c-if (c-u< '(xh . xt) '(yh . yt))
;;;                  ;; Yield q (the quotient) and x (the remainder)
;;;                  '(q (xh . xt))
;;;                  ;; Else subtract y from x, inc quo, and recur.
;;;                  (c-u/ (c-u- '(xh . xt) '(yh . yt))
;;;                        '(yh . yt)
;;;                        '(xh . q)))))))
;;; 
;;; 
;;; ;;; (c-ufactorial U)  ->  list
;;; ;;;
;;; ;;; Unary factorial.
;;; ;;; If the given list has length zero, yields the list '(u).
;;; ;;; If the given list has length one, yields the given list.
;;; ;;; Otherwise, yields a list containing items of the given list
;;; ;;; repeated (U-1)! times, where U is the length of the given list.
;;; ;;; This was named `c-fact' in the original source.
;;; (define-syntax c-ufactorial
;;;   (syntax-rules (quote)
;;;     ((c-fact s '())
;;;      (ck s '(u)))
;;;     ((c-fact s '(x))
;;;      (ck s '(x)))
;;;     ((c-fact s '(h . t))
;;;      (ck s (c-u* '(h . t) (c-ufactorial 't))))))
;;; 
;;; 
;;; ;;; (c-udrop L U)  ->  list
;;; ;;;
;;; ;;; Drops up to U items from the front of the given list, where U is a
;;; ;;; unary number. Analogous to `drop' from SRFI 1, but uses unary
;;; ;;; numbers, and yields empty list if the list is too short.
;;; (define-syntax c-udrop
;;;   (syntax-rules (quote)
;;;     ((c-udrop . args)
;;;      (c-u- . args))))
;;; 
;;; 
;;; ;;; (c-utake L U)  ->  list
;;; ;;;
;;; ;;; Yields a list containing up to U items from the front of the given
;;; ;;; list, where U is a unary number. Analogous to `take' from SRFI 1,
;;; ;;; but uses unary numbers, and yields entire list if it is too short.
;;; (define-syntax c-utake
;;;   (syntax-rules (quote)
;;;     ((c-utake s '(h ...) '(u ...))      ; Add a (accum) if missing.
;;;      (c-utake s '(h ...) '(u ...) '()))
;;;     ((c-utake s '() '(u ...) 'a)        ; Empty list,
;;;      (ck s (c-reverse 'a)))             ; so reverse and yield a.
;;;     ((c-utake s '(h ...) '() 'a)        ; Empty u,
;;;      (ck s (c-reverse 'a)))             ; so reverse and yield a.
;;;     ((c-utake s '(h . t) '(u . ut) 'a)  ; Move h onto a,
;;;      (c-utake s 't 'ut '(h . a)))))     ; decrement u, and recur.
;;; 
;;; 
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; DECIMAL INTEGERS
;;; 
;;; 
;;; ;;; (c-dadd1 N)  ->  'N+1 or '#f
;;; ;;;
;;; ;;; Decimal add1. If 0 <= N <= 15, yields N + 1. Otherwise '#f.
;;; ;;; (= 8 (ck () (c-dadd1 '7)))
;;; ;;;
;;; ;;; The domain could be extended indefinitely, but library compilation
;;; ;;; time increases rapidly because it generates a syntax-rules with
;;; ;;; many patterns. It is included as a proof of concept and example of
;;; ;;; a complex CK-macro built with CK-macros.
;;; (ck ()
;;;   (c-list 'define-syntax 'c-dadd1
;;;           (c-make-next
;;;            '(00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16))))
;;; 
;;; 
;;; ;;; (c-dsub1 N)  ->  'N-1 or '#f
;;; ;;;
;;; ;;; Decimal sub1. If 1 <= N <= 16, yields N - 1. Otherwise '#f.
;;; ;;; See also c-dadd1.
;;; ;;; (= 8 (ck () (c-dsub1 '9)))
;;; (ck ()
;;;   (c-list 'define-syntax 'c-dsub1
;;;           (c-make-next
;;;            '(16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00))))
;;; 
;;; 
;;; ;;; (c-du N)  ->  list or '#f
;;; ;;; (c-du N '(SUB1 ...))  ->  list or '#f
;;; ;;;
;;; ;;; Convert from decimal to unary. If N is in the domain of SUB1,
;;; ;;; yields a list of that many elements, specifically the integers
;;; ;;; (N-1, ..., 0). Otherwise, yields '#f.
;;; ;;;
;;; ;;; SUB1 is any operation that given N yields N-1, where 1 <= N.
;;; ;;; Defaults to c-dsub1, which supports 1 <= N <= 16.
;;; ;;; You can use c-make-next to make an operation with a larger domain.
;;; (define-syntax c-du
;;;   (syntax-rules (quote)
;;;     ;; Default SUB1
;;;     ((c-du s 'n)
;;;      (c-du s 'n '(c-dsub1)))
;;;     ((c-du s '0 '(SUB1 ...))
;;;      (ck s '()))
;;;     ((c-du s 'n '(SUB1 ...))
;;;      (ck s (c-if* (SUB1 ... 'n)
;;;                   '(c-cons (SUB1 ... 'n) (c-du (SUB1 ... 'n)))
;;;                   ''#f)))))
;;; 
;;; 
;;; ;;; (c-ud U)  ->  integer or '#f
;;; ;;; (c-ud U '(ADD1 ...))  ->  integer or '#f
;;; ;;;
;;; ;;; Convert from unary to decimal. Given a list of items, yields the
;;; ;;; list's length as a decimal integer.
;;; ;;; If the list length is too large for ADD1, yields '#f.
;;; ;;;
;;; ;;; ADD1 is any operation that given N yields N+1, where 0 <= N.
;;; ;;; Defaults to c-dadd1, which supports 0 <= N <= 15.
;;; ;;; You can use c-make-next to make an operation with a larger domain.
;;; (define-syntax c-ud
;;;   (syntax-rules (quote)
;;;     ;; Default ADD1
;;;     ((c-ud s '(xs ...))
;;;      (c-ud s '(xs ...) '(c-dadd1)))
;;;     ((c-ud s '() '(ADD1 ...))
;;;      (ck s '0))
;;;     ((c-ud s '(xs ...) '(ADD1 ...))
;;;      (c-ud s '(xs ...) '(ADD1 ...) '"accum:" '0))
;;;     ((c-ud s '() '(ADD1 ...) '"accum:" 'n)
;;;      (ck s 'n))
;;;     ((c-ud s '(x xs ...) '(ADD1 ...) '"accum:" 'n)
;;;      (ck s (c-if* (ADD1 ... 'n)
;;;                   '(c-ud '(xs ...) '(ADD1 ...) '"accum:" (ADD1 ... 'n))
;;;                   ''#f)))))
