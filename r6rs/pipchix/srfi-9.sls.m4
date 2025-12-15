#!r6rs
;;;
;;; Copyright © 2025 Barry Schwartz. All Rights Reserved.
;;; Copyright © 1999 Richard Kelsey. All Rights Reserved.
;;;
;;; This file is part of Pipchix.
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
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
m4_include(pipchix/pipchix-includes.m4)

;;;
;;; Richard Kelsey’s reference implementation for SRFI-9, modified for
;;; R⁶RS. The modifications are by Barry Schwartz. The original
;;; low-level implementation of records is replaced with vectors
;;; wrapped in R⁶RS records. Other changes also have been made.
;;;

(library (pipchix srfi-9)

  (export (rename (srfi-9:record? record?)
                  (srfi-9:make-record make-record)
                  (srfi-9:record-ref record-ref)
                  (srfi-9:record-set! record-set!)
                  (srfi-9:make-record-type make-record-type)
                  (srfi-9:record-constructor record-constructor)
                  (srfi-9:record-predicate record-predicate)
                  (srfi-9:record-accessor record-accessor)
                  (srfi-9:record-modifier record-modifier)
                  (srfi-9:define-record-type define-record-type)))

  (import (rnrs base (6))
          (rnrs records syntactic (6)))

  ;; The following implements a record abstraction that is identical
  ;; to vectors, except that they are not vectors (vector? returns
  ;; false when given a record and record? returns false when given a
  ;; vector). The following procedures are provided:
  ;;
  ;;   (record? <value>)                      -> <boolean>
  ;;   (make-record <size>)                   -> <record>
  ;;   (record-ref <record> <index>)          -> <value>
  ;;   (record-set! <record> <index> <value>) -> <unspecific>
  ;;
  ;; These can implemented in R⁶RS Scheme as R⁶RS records.

  (define-record-type <srfi-9:record>
    (fields srfi-9-fields))

  (define (srfi-9:make-record size)
    (make-<srfi-9:record> (make-vector size)))

  (define srfi-9:record? <srfi-9:record>?)

  (define (srfi-9:record-ref record index)
    (vector-ref (<srfi-9:record>-srfi-9-fields record)
                index))

  (define (srfi-9:record-set! record index value)
    (vector-set! (<srfi-9:record>-srfi-9-fields record)
                 index value))

  ;; We define the following procedures:
  ;; 
  ;; (make-record-type <type-name> <field-names>)     -> <record-type>
  ;; (record-constructor <record-type> <field-names>) -> <constructor>
  ;; (record-predicate <record-type>)                 -> <predicate>
  ;; (record-accessor <record-type <field-name>)      -> <accessor>
  ;; (record-modifier <record-type <field-name>)      -> <modifier>
  ;;
  ;;   where
  ;;
  ;; (<constructor> <initial-value> ...)              -> <record>
  ;; (<predicate> <value>)                            -> <boolean>
  ;; (<accessor> <record>)                            -> <value>
  ;; (<modifier> <record> <value>)                    -> <unspecific>

  ;; Record types are implemented using vector-like records.
  ;; The first slot of each record contains the record's type,
  ;; which is itself a record.
  
  (define (srfi-9:record-type record)
    (srfi-9:record-ref record 0))

  ;; Record types are themselves records, so we first define
  ;; the type for them. Except for problems with
  ;; circularities, this could be defined as:
  ;;
  ;;  (define-record-type <srfi-9:record-type>
  ;;    (make-record-type name field-tags)
  ;;    record-type?
  ;;    (name record-type-name)
  ;;    (field-tags record-type-field-tags))
  ;;
  ;; As it is, we need to define everything by hand.

  (define <srfi-9:record-type>
    (let ((new (srfi-9:make-record 3)))
      (srfi-9:record-set! new 0 new) ; Its type is itself.
      (srfi-9:record-set! new 1 '<srfi-9:record-type>)
      (srfi-9:record-set! new 2 '(name field-tags))
      new))

  ;; Now that <srfi-9:record-type> exists we can define a
  ;; procedure for making more record types.

  (define (srfi-9:make-record-type name field-tags)
    (let ((new (srfi-9:make-record 3)))
      (srfi-9:record-set! new 0 <srfi-9:record-type>)
      (srfi-9:record-set! new 1 name)
      (srfi-9:record-set! new 2 field-tags)
      new))

  ;; Accessors for record types.

  (define (srfi-9:record-type-name record-type)
    (srfi-9:record-ref record-type 1))

  (define (srfi-9:record-type-field-tags record-type)
    (srfi-9:record-ref record-type 2))

  ;; A utility for getting the offset of a field within a
  ;; record.

  (define (srfi-9:field-index type tag)
    (let loop ((i 1)
               (tags (srfi-9:record-type-field-tags type)))
      (cond ((null? tags)
             (error #f "record type has no such field" type tag))
            ((eq? tag (car tags))
             i)
            (else
             (loop (+ i 1) (cdr tags))))))

  ;; Now we are ready to define record-constructor and the
  ;; rest of the procedures used by the macro expansion of
  ;; define-record-type.

  (define (srfi-9:record-constructor type tags)
    (let ((size (length (srfi-9:record-type-field-tags type)))
          (arg-count (length tags))
          (indexes (map (lambda (tag)
                          (srfi-9:field-index type tag))
                        tags)))
      (lambda args
        (if (= (length args) arg-count)
            (let ((new (srfi-9:make-record (+ size 1))))
              (srfi-9:record-set! new 0 type)
              (for-each (lambda (arg i)
			  (srfi-9:record-set! new i arg))
                        args indexes)
              new)
            (error #f "wrong number of arguments to constructor"
                   type args)))))

  (define (srfi-9:record-predicate type)
    (lambda (thing)
      (and (srfi-9:record? thing)
           (eq? (srfi-9:record-type thing) type))))

  (define (srfi-9:record-accessor type tag)
    (let ((index (srfi-9:field-index type tag)))
      (lambda (thing)
        (if (and (srfi-9:record? thing)
                 (eq? (srfi-9:record-type thing) type))
            (srfi-9:record-ref thing index)
            (error #f "accessor applied to bad value"
                   type tag thing)))))
  
  (define (srfi-9:record-modifier type tag)
    (let ((index (srfi-9:field-index type tag)))
      (lambda (thing value)
        (if (and (srfi-9:record? thing)
                 (eq? (srfi-9:record-type thing) type))
            (srfi-9:record-set! thing index value)
            (error #f "modifier applied to bad value"
                   type tag thing)))))

  ;; Definition of define-record-type

  (define-syntax srfi-9:define-record-type
    (syntax-rules ()
      ((X type
          (constructor constructor-tag ...)
          predicate
          (field-tag accessor . more) ...)
       (let-syntax
           ((srfi-9:define-record-field^
             (syntax-rules ()
               ((X type^ field-tag^ accessor^)
                (define accessor^
                  (srfi-9:record-accessor type^ 'field-tag^)))
               ((X type^ field-tag^ accessor^ modifier^)
                (begin
                  (define accessor^
                    (srfi-9:record-accessor type^ 'field-tag^))
                  (define modifier^
                    (srfi-9:record-modifier type^ 'field-tag^)))))))
         (define type
           (srfi-9:make-record-type
            'type '(field-tag ...)))
         (define constructor
           (srfi-9:record-constructor
            type '(constructor-tag ...)))
         (define predicate
           (srfi-9:record-predicate type))
         (srfi-9:define-record-field^
          type field-tag accessor . more)
         ...))))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
