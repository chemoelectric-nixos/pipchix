m4_divert(-1)
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

;;; m4_changequote(«,»)
;;; m4_changecom(«#|»,«|#»)
;;; m4_define(«semicolon»,«;»)
;;; m4_define(«backslash»,«\»)

;;;
;;; A forloop implementation from the GNU M4 manual:
;;;
;;; m4_define(«m4_forloop», «m4_ifelse(m4_eval(«($2) <= ($3)»), «1», «m4_pushdef(«$1»)_$0(«$1», m4_eval(«$2»), m4_eval(«$3»), «$4»)m4_popdef(«$1»)»)»)
;;; m4_define(«_m4_forloop», «m4_define(«$1», «$2»)$4«»m4_ifelse(«$2», «$3», «», «$0(«$1», m4_incr(«$2»), «$3», «$4»)»)»)
;;;

;;; m4_define(«default_environment_r6rs»,«((except (rnrs (6)) fold-right member assoc map remove) (pipchix general-purpose list) (pipchix general-purpose box) (pipchix general-purpose division))»)
;;; m4_define(«default_environment_r7rs»,«((scheme base) (scheme complex) (scheme inexact) (scheme write) (pipchix general-purpose list) (pipchix general-purpose box) (pipchix general-purpose division))»)

;;; m4_ifelse(RNRS_NUMBER,«6»,«
;;; m4_define(«SCHEME_ERROR»,«(error #f $1 $2 $3 $4 $5 $6 $7 $8 $9)»)
;;; m4_define(«SLOW_SYNTAX_ERROR»,«(syntax-violation #f $1 (list $2 $3 $4 $5 $6 $7 $8 $9))»)
;;; m4_define(«FAST_SYNTAX_ERROR»,«(syntax-violation #f $1 (list $2 $3 $4 $5 $6 $7 $8 $9))»)
;;; »)
;;; m4_ifelse(RNRS_NUMBER,«7»,«
;;; m4_define(«SCHEME_ERROR»,«(error $1 $2 $3 $4 $5 $6 $7 $8 $9)»)
;;; m4_define(«SLOW_SYNTAX_ERROR»,«(error $1 $2 $3 $4 $5 $6 $7 $8 $9)»)
;;; m4_define(«FAST_SYNTAX_ERROR»,«(syntax-error $1 $2 $3 $4 $5 $6 $7 $8 $9)»)
;;; »)

;;;
;;; See
;;; https://racket.discourse.group/t/error-cond-unbound-identifier-also-no-app-syntax-transformer-is-bound/3081/3
;;; for why I add (meta -1) below. It is for Racket and has been
;;; needed in a few cases.
;;;
;;; m4_ifelse(RNRS_NUMBER,«6»,«
;;; m4_define(«basic_libraries»,«
(for (except (rnrs (6)) fold-right member assoc map remove) run expand (meta -1))
(for (pipchix general-purpose list) run expand)»)»)

;;; m4_ifelse(RNRS_NUMBER,«7»,«
;;; m4_define(«basic_libraries»,«
(scheme base)
(scheme case-lambda)
(scheme char)
(scheme complex)
(scheme file)
(scheme inexact)
(scheme lazy)
(scheme load)
(scheme process-context)
(scheme read)
(scheme write)
(pipchix general-purpose list)»)»)

;;; m4_define(«define_string_reverse_concatenate»,«
(define (%%string-reverse-concatenate lst)
  ;; Concatenation without the possible limitations of using
  ;; ‘apply’.
  (let loop ((lst lst)
             (str ""))
    (if (pair? lst)
        (loop (cdr lst) (string-append (car lst) str))
        str)))
;;; »)

;;; m4_define(«define_nix_set_setrec_letrec»,«
;;; m4_pushdef(«who»,«$1»)
(define-syntax expand-%%who%%-bindings
  (syntax-rules ( inherit inherit-from <== ==> )

    ((_ node (inherit-from s a) binding ...)
     (begin
       (insert-%%who%%-binding
        node (list a) (expand-%%who%%-get-node s a))
       (expand-%%who%%-bindings node binding ...)))

    ((_ node (inherit-from s a b ...) binding ...)
     (begin
       (insert-%%who%%-binding
        node (list a) (expand-%%who%%-get-node s a))
       (expand-%%who%%-bindings
        node (inherit-from s b ...) binding ...)))
    
    ((_ node (inherit a b ...) binding ...)
     (expand-%%who%%-bindings
      node (inherit-from #f a b ...) binding ...))

    ((_ node (value <== a b ...) binding ...)
     (begin
       (insert-%%who%%-binding
        node (list a b ...) value)
       (expand-%%who%%-bindings node binding ...)))

    ((_ node (a b ... ==> value) binding ...)
     (begin
       (insert-%%who%%-binding
        node (list a b ...) value)
       (expand-%%who%%-bindings node binding ...)))

    ((_ node) #t)))

(define-syntax insert-%%who%%-binding
  (syntax-rules ()
    ((_ node attrpath value)
     (let* ((path-node (list->nix-attributepath-node attrpath))
            (binding (make-nix-attributebinding-node
                      path-node (scheme->nix value))))
       (who-node-set! node binding)))))

(define-syntax expand-%%who%%-get-node
  (syntax-rules ()
    ((_ attrset identifier)
     (make-nix-get-node
      (and attrset (scheme->nix attrset))
      (list->nix-attributepath-node (list identifier))))))
;;; m4_popdef(«who»)
;;;»)

;;; m4_define(«define_ellipsis_test_r6rs»,«
;;; m4_pushdef(«NAME»,m4_ifelse($1,«»,«if-...»,«$1»))
(define-syntax NAME
  (lambda (stx)
    (syntax-case stx ()
      ((¶ ident if-true if-false)
       (if (free-identifier=? (syntax ident)
                              (syntax (... ...)))
         (syntax if-true)
         (syntax if-false))))))
;;; m4_popdef(«NAME»)
;;; »)

;;; m4_define(«define_ellipsis_test_r7rs»,«
;;; m4_pushdef(«NAME»,m4_ifelse($1,«»,«if-...»,«$1»))
(cond-expand
  (loko
   (define-syntax NAME
     (lambda (stx)
       (syntax-case stx ()
         ((_ ident if-true if-false)
          (if (free-identifier=? (syntax ident)
                                 (syntax (... ...)))
            (syntax if-true)
            (syntax if-false)))))))
  (else
   (define-syntax NAME
     (syntax-rules ::: ( ... )
       ((_ ... if-true if-false)
        if-true)
       ((_ xxx if-true if-false)
        if-false)))))
;;; m4_popdef(«NAME»)
;;; »)

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
