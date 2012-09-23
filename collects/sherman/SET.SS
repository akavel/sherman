;;; Sets

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

(module set mzscheme
 (provide
  set->list
  stream->set
  set/cardinality)

(require "stream.ss")

;; Sets
;; Sets are represented as lists compared with eq
(define (set/make-empty) '())
(define set/empty? null?)

(define (set/adjoin set new-element)
  (if (memq new-element set)
      set
      (cons new-element set)))

(define (set->list set) set)

(define (stream->set stream)
  (stream/foldl set/adjoin (set/make-empty) stream))

(define (set/cardinality set)
  (length set))
)
