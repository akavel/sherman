#| -*-Scheme-*-

$Id: tables.scm,v 4.2 1993/01/02 07:33:38 cph Exp $

Copyright (c) 1987, 1993 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: Tables

;; mzscheme doesn't have these
;; (declare (usual-integrations)
;;            (integrate-external "object"))


(module tables mzscheme
 (provide
  operations/make
  operations/bind
  operations/lookup)

;; Compatability
(define cons* list*)

;;;; Operations

(define (operations/make)
  (cons '() '()))

(define (operations/lookup operations variable if-found if-not)
  

  (let ((entry (assq variable (car operations))))

    (if entry
        (if (cdr entry)
            (if-found (cadr entry) (cddr entry))
            (if-not))
        (let ((entry (assq variable (cdr operations))))
          (if entry
              (if-found (cadr entry) (cddr entry))
              (if-not))))))

(define (operations/bind operations operation variable value)
  (cons (cons (cons* variable operation value)
              (car operations))
        (cdr operations)))

(define (operations/bind-global operations operation variable value)
  (cons (car operations)
        (cons (cons* variable operation value)
              (cdr operations))))

(define (operations/map-external operations procedure)
  (let loop ((elements (car operations)))
    (cond ((null? elements)
           '())
          ((cdar elements)
           (cons (procedure (cadar elements) (caar elements) (cddar elements))
                 (loop (cdr elements))))
          (else
           (loop (cdr elements))))))
)
