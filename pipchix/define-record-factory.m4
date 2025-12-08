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

m4_ifelse(implementation_of_define_record_factory,«syntax-rules»,«

;;;
;;; This record interface requires only R⁷RS-small or SRFI-9 records,
;;; and R⁷RS-small or R⁶RS syntax-rules. See (pipchix
;;; abstract-syntax-tree) for an example of its use.
;;;

(define-syntax define-record-factory
  (syntax-rules ()
    ((_ designation rule ...)
     (begin
       (define-record-type designation
         (make-it fields)
         predicate
         (fields access))
       (begin
         (define-syntax record-rule
           (syntax-rules ( constructor>
                           predicate>
                           getter> setter> )

             ((_ constructor predicate access (constructor> name proc))
              (define name (proc constructor)))

             ((_ constructor predicate access (constructor> name))
              (define name constructor))

             ((_ constructor predicate access (predicate> name proc))
              (define name (proc predicate)))

             ((_ constructor predicate access (predicate> name))
              (define name predicate))

             ((_ constructor predicate access (getter> i name proc))
              (define name
                (proc
                 (lambda (obj)
                   (vector-ref (access obj) (- i 1))))))

             ((_ constructor predicate access (getter> i name))
              (define name
                (lambda (obj)
                  (vector-ref (access obj) (- i 1)))))

             ((_ constructor predicate access (setter> i name proc))
               (define name
                 (proc
                  (lambda (obj value)
                    (vector-set! (access obj) (- i 1) value)))))

             ((_ constructor predicate access (setter> i name))
              (define name
                (lambda (obj value)
                  (vector-set! (access obj) (- i 1) value))))))

         (record-rule
          (lambda fields (make-it (list->vector fields)))
          predicate access rule)
         ...)))))

»)

m4_ifelse(implementation_of_define_record_factory,«er-macro-transformer»,«

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
     (define (cadddr x) (cadr (cddr x)))

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

     (define three-arg?
       (lambda (rule*)
         (and (pair? rule*)
              (list? (car rule*))
              (= (length (car rule*)) 4))))

     (define arg1 (lambda (rule*) (cadr (car rule*))))
     (define arg2 (lambda (rule*) (caddr (car rule*))))
     (define arg3 (lambda (rule*) (cadddr (car rule*))))

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
       ;; FIXME: Do a more precise analysis. (It is better to analyze
       ;; here than to force the code below to follow a particular
       ;; order, just to get the best error messages.)
       (error "syntax error in rule" (car rule*)))

     (let ((n (length form)))
       (cond
        ((= n 1)
         (error "record type designation is missing" form))
        ((not (symbol? (cadr form)))
         (error "record type designation is not a symbol"
                (cadr form)))
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
              ((three-arg? rule*)
               (cond
                ((getter>? rule*)
                 (let* ((i (arg1 rule*))
                        (name (arg2 rule*))
                        (proc (arg3 rule*))
                        (definition
                          `(,define% ,name
                             (,proc
                              (,lambda%
                               (obj)
                               (,vector-ref% (,access% obj)
                                             (,minus% ,i 1)))))))
                   (loop (cons definition code) (cdr rule*))))
                ((setter>? rule*)
                 (let* ((i (arg1 rule*))
                        (name (arg2 rule*))
                        (proc (arg3 rule*))
                        (definition
                          `(,define% ,name
                             (,proc
                              (,lambda%
                               (obj value)
                               (,vector-set!% (,access% obj)
                                              (,minus% ,i 1)
                                              value))))))
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

»)

m4_ifelse(implementation_of_define_record_factory,«gambit-syntax-case»,«

(define-syntax define-record-factory
  (lambda (stx)

    (define (insert-identifiers . x*)
      ;; Create a list of gensym-generated identifiers within the
      ;; syntactic context. (This is not the same datum->syntax as
      ;; that of R⁶RS, and this gensym is not the same as in the
      ;; (chezscheme) library of Chez Scheme.)
      (datum->syntax stx (map gensym x*)))

    (syntax-case stx ()
      ((_ designation rule ...)
       (syntax-case
           (insert-identifiers
            'original-constructor%
            'constructor%
            'original-predicate%
            'predicate%
            'fields%
            'access%) ()
         ((original-constructor%
           constructor%
           original-predicate%
           predicate%
           fields%
           access%)
          (syntax ;; Avoid #' because many readers cannot handle it.
           (begin
             (define-record-type designation
               (original-constructor% fields%)
               original-predicate%
               (fields% access%))
             (define (constructor% . obj)
               (original-constructor% (list->vector obj)))
             (define (predicate% obj)
               (original-predicate% obj))
             (define-syntax record-rule
               (syntax-rules ( constructor>
                               predicate>
                               getter> setter> )

                 ((_ constructor predicate access (constructor> name proc))
                  (define name (proc constructor)))

                 ((_ constructor predicate access (constructor> name))
                  (define name constructor))

                 ((_ constructor predicate access (predicate> name proc))
                  (define name (proc predicate)))

                 ((_ constructor predicate access (predicate> name))
                  (define name predicate))

                 ((_ constructor predicate access (getter> i name proc))
                  (define name
                    (proc
                     (lambda (obj)
                       (vector-ref (access obj) (- i 1))))))

                 ((_ constructor predicate access (getter> i name))
                  (define name
                    (lambda (obj)
                      (vector-ref (access obj) (- i 1)))))

                 ((_ constructor predicate access (setter> i name proc))
                  (define name
                    (proc
                     (lambda (obj value)
                       (vector-set! (access obj) (- i 1) value)))))

                 ((_ constructor predicate access (setter> i name))
                  (define name
                    (lambda (obj value)
                      (vector-set! (access obj) (- i 1) value))))))

             (record-rule constructor% predicate% access%
                          rule)
             ...))))))))

»)

m4_divert(-1)
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
m4_divert«»m4_dnl
