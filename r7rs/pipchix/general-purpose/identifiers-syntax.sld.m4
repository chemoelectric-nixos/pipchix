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
;;;
;;; The syntax-rules and er-macro-transformer implementations are
;;; based closely on work by Marc Nieper-Wißkirchen for SRFI-148.
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

(define-library (pipchix general-purpose identifiers-syntax)

  (export stx-free-identifier=?
          stx-bound-identifier=?
          stx-ellipsis=?)

  (import (scheme base))
  (import (scheme cxr))

  (cond-expand
    (chicken-5
     (import (chicken syntax)))
    (loko
     (import (rnrs syntax-case (6))))
    (else))

  (begin

    (cond-expand

      (chicken-5

       (define-syntax stx-free-identifier=?
         (er-macro-transformer
          (lambda (form rename compare)
            (unless (= (length form) 5)
              (error "malformed stx-free-identifier=?" form))
            (let ((args (cdr form)))
              (if (compare (car args) (cadr args))
                (caddr args)
                (cadddr args))))))

       (define-syntax stx-bound-identifier=?
         (er-macro-transformer
          (lambda (form rename compare)
            (unless (= (length form) 5)
              (error "malformed stx-bound-identifier=?" form))
            (let ((args (cdr form)))
              (if (eq? (car args) (cadr args))
                (caddr args)
                (cadddr args))))))

       (define-syntax stx-ellipsis=?
         (er-macro-transformer
          (lambda (form rename compare)
            (unless (= (length form) 4)
              (error "malformed stx-ellipsis=?" form))
            (let ((args (cdr form))
                  (ellipsis (rename '...)))
              (let ((ellipsis=?
                     (lambda (x) (and (symbol? x)
                                      (compare x ellipsis)))))
                (if (ellipsis=? (car args))
                  (cadr args)
                  (caddr args))))))))

      (loko

       (define-syntax stx-free-identifier=?
         (lambda (stx)
           (syntax-case stx ()
             ((_ ident1 ident2 if-true if-false)
              (syntax-case stx ()
                ((_ ident1 ident2 if-true if-false)
                 (if (free-identifier=? (syntax ident1)
                                        (syntax ident2))
                   (syntax if-true)
                   (syntax if-false))))))))

       (define-syntax stx-bound-identifier=?
         (lambda (stx)
           (syntax-case stx ()
             ((_ ident1 ident2 if-true if-false)
              (syntax-case stx ()
                ((_ ident1 ident2 if-true if-false)
                 (if (bound-identifier=? (syntax ident1)
                                         (syntax ident2))
                   (syntax if-true)
                   (syntax if-false))))))))

       (define-syntax stx-ellipsis=?
         (lambda (stx)
           (syntax-case stx ()
             ((_ ident if-true if-false)
              (if (free-identifier=? (syntax ident)
                                     (syntax (... ...)))
                (syntax if-true)
                (syntax if-false)))))))

      (else

       (define-syntax stx-free-identifier=?
         (syntax-rules ()
           ((_ ident1 ident2 if-true if-false)
            (begin
              (define-syntax match?
                (syntax-rules :::1 ()
                  ((_ if-tr if-fls)
                   (begin
                     (define-syntax test
                       (syntax-rules :::2 (ident1)
                         ((_ ident1 if-t if-f)
                          if-t)
                         ((_ xxxxxx if-t if-f)
                          if-f)))
                     (test ident2 if-tr if-fls)))))
              (match? if-true if-false)))))

       (define-syntax stx-bound-identifier=?
         (syntax-rules ()
           ((_ ident var if-true if-false)
            (begin
              (define-syntax match?
                (syntax-rules :::1 ()                                 
                  ((_ if-tr if-fls)
                   (begin
                     (define-syntax ident
                       (syntax-rules :::2 ()
                         ((_ if-t if-f)
                          if-f)))
                     (define-syntax okay
                       (syntax-rules ()
                         ((_ if-t if-f)
                          if-t)))
                     (define-syntax test
                       (syntax-rules :::2 ()
                         ((_ var if-t if-f)
                          (ident if-t if-f))
                         ((_ xxx if-t if-f)
                          (ident if-t if-f))))
                     (test okay if-tr if-fls)))))
              (match? if-true if-false)))))

       (define-syntax stx-ellipsis=?
         (syntax-rules ::: ( ... )
           ((_ ... if-true if-false)
            if-true)
           ((_ xxx if-true if-false)
            if-false)))))
    
    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
