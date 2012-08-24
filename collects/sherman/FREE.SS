;;; Simple free variable analysis

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


;;; free-variable? <name> <body> 
;;; Returns #t iff <name> is freely referenced in <body>.  

;;; Works by performing a code walk looking for <name>.  Currently,
;;; this only handles the LET and LAMBDA binding constructs (and the
;;; keyword versions), because the compiler doesn't use any others.
;;; If that is changed, then new code must be added here.

(define (free-variable? name body)
  (cond ((eq? name body) #t)
	((pair? body) (let ((handler (assq (car body) free-variable-special-forms)))
			(if (eq? handler #f)
			    (any? (lambda (subform) (free-variable? name subform))
				  body)
			    (apply (cadr handler) name (cdr body)))))
	(else #f)))

(define (free-variable-test name)
  (lambda (subform) (free-variable? name subform)))

(define (free-variable/lambda name bindings . body)
  (if (memq name bindings)
      #f
      (any? (lambda (subform) (free-variable? name subform))
	    body)))

(define (let->named-let name-or-bindings bindings-or-first body receiver)
  (if (symbol? name-or-bindings)
      (receiver name-or-bindings bindings-or-first body)
      (receiver #f name-or-bindings (cons bindings-or-first body))))

(define (free-variable/let var name-or-bindings bindings-or-first . body)
  (let->named-let name-or-bindings bindings-or-first body
   (lambda (name bindings body)
     (or (any? (free-variable-test var)
	       (map cadr bindings))
	 (if (memq var (map car bindings))
	     #f
	     (any? (free-variable-test var)
		   body))))))

(define free-variable-special-forms '())

(define (any? predicate list)
  (and (pair? list)
       (or (predicate (car list))
	   (any? predicate (cdr list)))))

(set! free-variable-special-forms
      `((LAMBDA   ,free-variable/lambda)
        (#%LAMBDA ,free-variable/lambda)
	(LET      ,free-variable/let)
        (#%LET    ,free-variable/let)
	))