;;; Blocks

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

(require-library "functios.ss")
(require-library "synrule.ss")
(require-library "stream.ss" "sherman")

;;; Blocks are fundamentally based on linked lists.  The data
;;; structure handed to the user contains a pointer to the first
;;; element and to the 'current' element.  (These pointers actually
;;; point to the cell *before* the current element.  This allows you
;;; to perform an insert at the 'current' location.)
;;; This will give you a O(1) insert, O(n) traversal.

(define (#%list->block list)
  (let ((tag (cons 'block list)))
    (cons tag tag)))

(define list->block #%list->block)

(define-syntax incf
  (syntax-rules ()
    ((incf var) (set! var (add1 var)))))

(define block?-count 0)
(define (block? object)
  (and (pair? object)
       (pair? (car object))
       (eq? (caar object) 'block)))

;;; Macros for performance.  

(define-syntax block/first
  (syntax-rules ()
    ((block/first block) (caddr block))))

(define-syntax block/second
  (syntax-rules ()
    ((block/first block) (cadddr block))))

(define-syntax block/third
  (syntax-rules ()
    ((block/first block) (car (cddddr block)))))

(define-syntax block/fourth
  (syntax-rules ()
    ((block/first block) (cadr (cddddr block)))))

(define-syntax block->list 
  (syntax-rules ()
    ((block->list block) (cddr block))))

(define-syntax block/rest 
  (syntax-rules ()
    ((block/rest block) (cons (car block) (cddr block)))))

(define-syntax block/empty?
  (syntax-rules ()
    ((block/empty? block) (null? (cddr block)))))

;;; Block functions

(define (block/clear! block)
  (set-cdr! (cdr block) '()))

(define (block/copy b)
  ;; First, copy the tail elements.
  (let ((tail-copy (append (cdr b) '()))) ;copy list fast
    (cons 
     ;; now, copy the head elements until reaching the tail
     (let loop ((scan (car b)))
       (if (eq? scan (cdr b))
           tail-copy
           (cons (car scan) (loop (cdr scan)))))
     tail-copy)))

(define (block/find block element)
  ;; We'd like to use member, but we can't 
  (let loop ((scan (cdr block)))
    (cond ((null? (cdr scan)) #f)
          ((equal? (cadr scan) element)
           (cons (car block) scan))
          (else (loop (cdr scan))))))

(define (block/head block)
  (cons (car block) (car block)))

(define (block/insert! dest object)
  (set-cdr! (cdr dest) (cons object (cddr dest)))
  ;; Bump past the new element
  ;; (set-cdr! dest (cddr dest))
  )

(define (append-head list tail nelements)
  (if (zero? nelements)
      tail
      (cons (car list) (append-head (cdr list) tail (sub1 nelements)))))

(define (block/insert-part! dest src count)
  (let* ((original-tail (cddr dest))
         (new-tail (append-head (block->list src) original-tail count)))
    (set-cdr! (cdr dest) new-tail)
    ;; Bump past the insert
    (let loop ((scan (cdr dest)))
      (if (eq? (cdr scan) original-tail)
          (set-cdr! dest scan)
          (loop (cdr scan))))
    dest))

(define (block/length block)
  (length (block->list block)))

(define (block/pick block n)
  ;; Stupid 1-based picking
  (list-ref (cdr block) n))

(define (block/select block element)
  (let ((found (member element (cddr block))))
    (and found
        (cadr found)
        )))

(define (block/set-first! block new-first)
  (set-car! (cddr block) new-first))

(define (block/set-n! dest new-stuff)
  (let loop ((dest (block->list dest))
             (src  (block->list new-stuff)))
    (if (not (null? src))
        (begin (set-car! dest (car src))
               (loop (cdr dest) (cdr src))))))

(define (block/skip block n)
  (if (>= n 0)
      (cons (car block) (list-tail (cdr block) n))
      (error "Backward skipping not implemented yet")))

(define (block/splice! dest source)
  (let* ((current-tail (block->list dest))
         (new-tail (append (block->list source) current-tail)))
    (set-cdr! (cdr dest) new-tail)
    ;; Bump past the insert
;    (let loop ((scan (cdr dest)))
;      (if (eq? (cdr scan) current-tail)
;          (set-cdr! dest scan)
;          (loop (cdr scan))))
    ))

(define (block->stream block)
  (list->stream (block->list block)))

(define (block/tail block)
  (cons (car block) (last-pair (cdr block))))

(define (block/tail? block)
  (null? (cddr block)))

