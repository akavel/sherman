;;; Sherman runtime system for MzScheme

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

(require-library "function.ss")

(require-library "block.ss" "sherman")

(define sherman-runtime-version .5)

(define (sherman-banner)
  (display "Sherman runtime version ")
  (display sherman-runtime-version)
  (newline)
  (display "Hosted on ")
  (display (substring (banner) 11 (string-length (banner))))
  )

(sherman-banner)

(define (change dest new-element)
  (if (block? new-element)
      ;what is this crap?
      (if (block/empty? dest)
	  (block/splice! dest new-element)
	  (block/set-n! dest new-element))
      (begin
	;;Modify the first, then skip to the next
	(block/set-first! dest new-element)
	(block/rest dest))))


(define (rebol-copy thing)
  (cond ((block? thing) (block/copy thing))
	(else (error "Can't copy this" thing))))

(define (find series element)
  (cond ((block? series) (block/find series element))
	(else (error "Can't find in this thing" series))))

(define (rebol-first thing)
  (cond ((string? thing) (string-ref thing 0))
	((block? thing) (block/first thing))
	(else (error "Can't take the first of this"))))

(define (form object)
  (let ((port (open-output-string)))
    (if (block? object)
	(begin (display "[" port)
	       (for-each (lambda (element)
			   (display element port) (display " " port))
			 (block->list object))
	       (display "]" port))
	(display object port))
    (get-output-string port)))

(define (head object)
  (cond ((string? object) object)
	((block? object) (block/head object))
	(else (error "Can't take the head of this" object))))

(define (make prototype initialization)
  (cond ((block? prototype) (if (block? initialization)
				(block/copy initialization)
				(error "Can't make  a block from this" initialization)))
	(else (error "Can't make this" prototype))))

(define (less-than-greater-than? a b)
  (not (equal? a b)))

(define (match pattern thing)
  ;; Wrong!
  (string=? pattern thing))

(define (now)
  (list->block (list 0 0 0 (current-seconds))))

(define (pick thing index)
  (cond ((block? thing) (block/pick thing index))
	(else (error "Can't pick this" thing))))

(define (prin thing) (display (form thing)))

(define (print thing)
  (prin thing)
  (display #\newline))

(define (rebol-random amount)
  ;; Stupid 1-based random
  (add1 (random amount)))

(define (subvector->list vector start end)
  (build-list (- end start)
	      (lambda (i)
		(vector-ref vector (+ start i)))))


(define sherman-namespace (make-namespace 'keywords))

(parameterize ((current-namespace sherman-namespace))
  (require-library "trace.ss")
  (map (lambda (spec)
         (if (list? spec)
             (global-defined-value (car spec) (cadr spec))
             (undefine spec)))
       `(
         (add             ,+)
         (block!          ,(list->block '(this is the prototype block)))
	 (change          ,change)
	 (clear           ,block/clear!)
         (copy            ,block/copy)
         (equal?          ,equal?)
         (exit            ,(lambda values
                            (error "Exit can only be called from within a function.
Use quit to leave the interpreter")))
	 (false           #f)
	 (find            ,block/find)
	 (first           ,rebol-first)
	 (form            ,form)
	 (fourth          ,(lambda (x) (block/fourth x)))
         (greater-than?   ,>)
	 (head            ,head)
	 (input           ,read-line)
         (insert          ,block/insert!)
	 (insert/part     ,block/insert-part!)
	 (length?         ,block/length)
         (less-than?      ,<)
	 (less-than-greater-than? ,less-than-greater-than?)
         (#%list->block   ,list->block)
	 (do-file         ,load)
         (make            ,make)
	 (match           ,match)
	 (next            ,(lambda (x) (block/rest x)))
         (newline         ,#\newline)
	 (now             ,now)
	 (on              #t)
	 (off             #f)
	 (pick            ,block/pick)
	 (prin            ,prin)
         (print           ,print)
         (quit            ,exit)
	 (random          ,rebol-random)
	 (select          ,block/select)
	 (skip            ,block/skip)
         (subtract        ,-)
	 (tail            ,block/tail)
	 (tail?           ,block/tail?)
         (time-apply      ,time-apply)
	 (true            #t)
         ))
)


