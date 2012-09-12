;;; Streams

;;; Copyright (c) 1999 Emergent Technologies, Inc.

;;; Permission to copy this software, to redistribute it, and to use it
;;; for any purpose is granted, subject to the following restrictions and
;;; understandings.

;;; 1. Any copy made of this software must include this copyright notice
;;; in full.

;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to Emergent Technologies, Inc. any improvements or extensions
;;; that they make, so that these may be included in future releases; and
;;; (b) to inform Emergent Technologies, Inc. of noteworthy uses of this
;;; software.

;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.

;;; 4. Emergent Technologies, Inc. has made no warrantee or representation
;;; that the operation of this software will be error-free, and Emergent
;;; Technologies, Inc. is under no obligation to provide any services, by
;;; way of maintenance, update, or otherwise.

;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name Emergent Technologies nor of any
;;; adaptation thereof in any advertising, promotional, or sales
;;; literature without prior written consent from Emergent Technologies,
;;; Inc. in each case.

(require-library "synrule.ss")

;; Streams
(define (stream/make-empty)
  '())

(define (stream/empty? stream)
  (null? stream))

(define (stream/car stream)
  (car stream))

(define (stream/cdr stream)
  (force (cdr stream)))

(define-syntax stream/cons
  (syntax-rules ()
   ((stream/cons car cdr) (cons car (delay cdr)))))

(define (list->stream list)
  (if (null? list)
      (stream/make-empty)
      (stream/cons (car list) (list->stream (cdr list)))))

(define (stream->list stream)
  (if (stream/empty? stream)
      '()
      (cons (stream/car stream) (stream->list (stream/cdr stream)))))

(define (stream/map stream f)
  (if (stream/empty? stream)
      stream
      (stream/cons (f (stream/car stream))
                   (stream/map (stream/cdr stream) f))))

(define (stream/for-each stream f)
  (if (not (stream/empty? stream))
      (begin (f (stream/car stream))
             (stream/for-each (stream/cdr stream) f))))

(define (stream/filter stream predicate)
  (if (stream/empty? stream) 
      stream
      (let ((this-element (stream/car stream)))
        (if (predicate this-element)
            (stream/cons this-element (stream/filter (stream/cdr stream) predicate))
            (stream/filter (stream/cdr stream) predicate)))))

(define (stream/foldl f init stream)
  (if (stream/empty? stream)
      init
      (stream/foldl f 
                    (f init (stream/car stream))
                    (stream/cdr stream))))
