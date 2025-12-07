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

;;;
;;; This record interface requires R⁷RS-small or SRFI-9 records, and
;;; also er-macro-transformer and gensym. See (pipchix
;;; abstract-syntax-tree) for an example of its use.
;;;

(define-syntax define-record-factory
  (er-macro-transformer
   (lambda (form rename compare)

     (define rn rename)

     (define (caddr x) (cadr (cdr x)))

     (define one-arg?
       (lambda (rule*)
         (and (pair? rule*)
              (list? (car rule*))
              (= (length (car rule*)) 2))))

     (define two-arg?
       (lambda (rule*)
         (and (pair? rule*)
              (list? (car rule*))
              (= (length (car rule*)) 3))))

     (define arg1 (lambda (rule*) (cadr (car rule*))))
     (define arg2 (lambda (rule*) (caddr (car rule*))))

     (define (rule-match? symb)
       (lambda (rule*)
         (and (symbol? (caar rule*))
              (compare (caar rule*) (rn symb)))))

     (define constructor>? (rule-match? 'constructor>))
     (define predicate>? (rule-match? 'predicate>))
     (define getter>? (rule-match? 'getter>))
     (define setter>? (rule-match? 'setter>))

     (define define-record-type% (rn 'define-record-type))
     (define define% (rn 'define))
     (define lambda% (rn 'lambda))
     (define begin% (rn 'begin))
     (define vector-ref% (rn 'vector-ref))
     (define vector-set!% (rn 'vector-set!))
     (define list->vector% (rn 'list->vector))
     (define minus% (rn '-))

     (define (error-in-rule rule*)
       ;; FIXME: Do a better analysis.
       (error "syntax error in rule" (car rule*)))

     (let ((n (length form)))
       (cond
        ((= n 1) (error "record type designation is missing" form))
        (else
         (let* ((designation (cadr form))
                (original-constructor% (gensym))
                (constructor% (gensym))
                (original-predicate% (gensym))
                (predicate% (gensym))
                (fields% (gensym))
                (access% (gensym)))
           (let loop ((code '())
                      (rule* (cddr form)))
             (cond
              ((one-arg? rule*)
               (cond
                ((constructor>? rule*)
                 (let* ((name (arg1 rule*))
                        (definition
                          `(,define% ,name ,constructor%)))
                   (loop (cons definition code) (cdr rule*))))
                ((predicate>? rule*)
                 (let* ((name (arg1 rule*))
                        (definition
                          `(,define% ,name ,predicate%)))
                   (loop (cons definition code) (cdr rule*))))
                (else (error-in-rule rule*))))
              ((two-arg? rule*)
               (cond
                ((constructor>? rule*)
                 (let* ((name (arg1 rule*))
                        (proc (arg2 rule*))
                        (definition
                          `(,define% ,name (,proc ,constructor%))))
                   (loop (cons definition code) (cdr rule*))))
                ((predicate>? rule*)
                 (let* ((name (arg1 rule*))
                        (proc (arg2 rule*))
                        (definition
                          `(,define% ,name (,proc ,predicate%))))
                   (loop (cons definition code) (cdr rule*))))
                ((getter>? rule*)
                 (let* ((i (arg1 rule*))
                        (name (arg2 rule*))
                        (definition
                          `(,define% ,name
                             (,lambda%
                              (obj)
                              (,vector-ref% (,access% obj)
                                            (,minus% ,i 1))))))
                   (loop (cons definition code) (cdr rule*))))
                ((setter>? rule*)
                 (let* ((i (arg1 rule*))
                        (name (arg2 rule*))
                        (definition
                          `(,define% ,name
                             (,lambda%
                              (obj value)
                              (,vector-set!% (,access% obj)
                                             (,minus% ,i 1)
                                             value)))))
                   (loop (cons definition code) (cdr rule*))))
                (else (error-in-rule rule*))))
              (else
               (let ((heading
                      `(,begin%
                        (,define-record-type% ,designation
                          (,original-constructor% ,fields%)
                          ,original-predicate%
                          (,fields% ,access%))
                        (,define% (,constructor% . obj)
                          (,original-constructor%
                           (,list->vector% obj)))
                        (,define% (,predicate% obj)
                          (,original-predicate% obj)))))
                 (append heading (reverse code)))))))))))))

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
