;; A compiler for a REBOL-like language

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

(require-library "compat.ss")
(require-library "compile.ss")
(require-library "file.ss")
(require-library "pretty.ss")
(require-library "synrule.ss")

(require-library "free.ss"   "sherman")
(require-library "block.ss"  "sherman")
(require-library "set.ss"    "sherman")
(require-library "tables.ss" "sherman")


;; Compatability
(define -1+ sub1)

(define cons* list*)

(define (complement f)
  (lambda (x) (not (f x))))

(define (identity-procedure x) x)

(define (subvector->list vector start end)
  (build-list (- end start) (lambda (i) (vector-ref vector (+ start i)))))

;; Various source file elements
(define (declaration? thing)
  (eq? thing 'declare))

(define (func? thing)
  (eq? thing 'func))

(define (if? thing)
  (eq? thing 'if))

(define (repeat? thing)
  (eq? thing 'repeat))

(define (definition/base-name definition)
  (let ((pname (symbol->string definition)))
    (string->symbol (substring pname 0 (-1+ (string-length pname))))))

(define (definition? thing)
  (and (symbol? thing)
       (let ((pname (symbol->string thing)))
         (char=? (string-ref pname (-1+ (string-length pname)))
                 #\:))))

(define (fetch/base-name definition)
  (let ((pname (symbol->string definition)))
    (string->symbol (substring pname 1 (string-length pname)))))

(define (fetch? thing)
  (and (symbol? thing)
       (let ((pname (symbol->string thing)))
         (char=? (string-ref pname 0)
                 #\:))))

(define (self-evaluating? thing)
  (or (null? thing)
      (void? thing)
      (eq? thing #t)
      (eq? thing #f)
      (string? thing)
      (number? thing)
      ))


;; Various output file elements
;; This is the compile back end.
;; This section is specific to mzscheme
(define (maybe-bind-break inner)
  (if (free-variable? 'break inner)
      (let ((break (gensym)))
        `(#%CALL-WITH-CURRENT-CONTINUATION
           (#%LAMBDA (,break)
              (#%LET ((BREAK (#%LAMBDA () (,break #f))))
                     ,inner))))
      inner))

(define (sequence? thing)
  (and (pair? thing)
       (or (eq? (car thing) 'BEGIN)
           (eq? (car thing) '#%BEGIN))))

(define sequence-elements cdr)

(define (arity-case/make f cases)
  `(#%CASE (#%ARITY ,f)
     ,@cases))

(define (assignment/make name value)
  `(#%SET! ,name ,value))

(define (combination/make operator operands)
  `(,operator ,@operands))

(define (conditional/make predicate consequence alternative)
  `(#%IF ,predicate
       ,consequence
       ,alternative))

(define (dangling-else/make)
  `(#%ERROR "Danging else."))

(define (for/make name start end step body)
  (let ((loop  (gensym))
        (stemp (gensym))
        (steptemp (gensym))
        (etemp (gensym))
        )
    (maybe-bind-break
     `(#%LET ((,stemp ,start)
              (,etemp ,end)
              (,steptemp ,step)
              )
           (#%LET ,loop ((,name ,stemp))
              (#%IF (#%<= ,name ,etemp)
                    (#%BEGIN ,body
                             (,loop (#%+ ,name ,steptemp)))))))))

(define (foreach/make name series body)
  (let ((stemp (gensym))
        (break (gensym))
        (loop  (gensym))
        (body (if (sequence? body)
                  (sequence-elements body)
                  (list body))))
    (maybe-bind-break
           `(#%LET ,loop ((,stemp ,series))
                   (#%IF (TAIL? ,stemp)
                         #f
                         (#%LET ((,name (FIRST ,stemp)))
                                (#%BEGIN ,@body
                                         (,loop (NEXT ,stemp)))))))))

(define (lambda/make args body)
  ;; support use of RETURN
  ;; Check to see if we actually have
  ;; a free reference to RETURN within the body, 
  ;; because there is no point in capturing the continuation if 
  ;; aren't planning on using it.  ~jrm
  (let ((body (if (sequence? body)
                  (sequence-elements body)
                  (list body))))
    `(#%LAMBDA ,args
               ,@(if (free-variable? 'return body)
                   `((#%CALL-WITH-CURRENT-CONTINUATION
                      (#%LAMBDA (RETURN)
                               ,@body)))
                   body))))

(define (literal/make text)
  (if (symbol? text)
      `(#%QUOTE, text)
      text))

(define (literal-block/make text body)
  ;; Discard the body for now, yuck.
  `(#%list->block (list ,@text)))

(define (procedure-test/make thing)
  `(#%PROCEDURE? ,thing))

(define (repeat/make name limit body)
  (let ((temp (gensym))
        (loop (gensym)))
    (maybe-bind-break
    `(#%LET ,loop ((,name 1)
                   (,temp ,limit))
          (#%if (#%not (#%> ,name ,temp))
                (#%BEGIN ,body
                         (,loop (#%+ ,name 1) ,temp)))))))

(define (too-few-arguments thing)
  `(#%ERROR "Too few arguments." (#%QUOTE ,thing)))

(define (flatten-sequence elements)
  (apply append (map (lambda (element)
                 (if (and (list? element)
                          (eq? (car element) '#%BEGIN))
                     (cdr element)
                     (list element)))
               elements)))

(define (sequence/make elements)
  (let ((contents (flatten-sequence
                   (stream->list
                    (stream/filter (list->stream elements)
                                   (complement self-evaluating?))))))
    (if (null? contents)
        'NONE
        `(#%BEGIN ,@contents))))

(define (top-level-block/make definitions body)
  `(#%BEGIN (#%DEFINE-VALUES ,(set->list definitions)
            (#%VALUES ,@(build-list (set/cardinality definitions)
                                  (lambda (n) #f))))
          ,body))

(define (until/make body)
  (let ((loop (gensym))
        (result (gensym)))
    (maybe-bind-break
    `(#%LET ,loop ((,result #f))
        (#%IF (#%NOT ,result)
              (,loop ,body)
              ,result)))))

(define (use/make names body)
  `(#%LET ,(map (lambda (name) `(,name #f)) names) ,body))

(define (while/make test body)
  (let ((loop (gensym))
        (break (gensym)))
    (maybe-bind-break `(#%LET ,loop ()
            (#%IF ,test
                (#%BEGIN ,body (,loop)))))))

(define (conditional/make test consequence alternative)
  (if (eq? alternative #f)
      `(#%IF ,test ,consequence)
      `(#%IF ,test ,consequence ,alternative)))

;;; End of back end stuff


(define (crsf infile outfile)
  (let ((input (load infile)))
    (if (and (pair? input)
             (eq? (car input) 'sherman)
             (block? (cadr input)))
        (let ((result (compile-top-level (cadr input))))
          (with-output-to-file outfile
            (lambda ()
              (stream/for-each (list->stream (cdr result))
                               (lambda (element)
                                 (newline)
                                 (pretty-print element))))
            'replace))
        (error "File is not a sherman file." infile))))

(define (scan-definitions block)
  (stream->set
   (stream/map
    (stream/filter (block->stream block)
                   definition?)
    definition/base-name)))

(define (compile-top-level block)
  (top-level-block/make (scan-definitions block) 
                        (compile-sequence block (operations/make) (environment/make))))


;;; Environment hacking
;; Stolen from MIT Scheme
(define (environment/make) '())

(define *unknown-value "Unknown Value")

(define (environment/bind environment variable value)
  (cons (cons variable value) environment))

(define (environment/bind-multiple environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-unknown if-not)
  (let ((association (assq variable environment)))
    (if association
        (if (eq? (cdr association) *unknown-value)
            (if-unknown)
            (if-found (cdr association)))
        (if-not))))

;;; Declaration hacking
;;; A lot of this is lifted from MIT Scheme
(define known-declarations '())

(define (define-declaration operation parser)
  (let ((entry (assq operation known-declarations)))
    (if entry
        (set-cdr! entry parser)
        (set! known-declarations
              (cons (cons operation parser)
                    known-declarations)))))

(define-struct decl
  (
  ;; OPERATION is the name of the operation that is to be performed by
  ;; this declaration.
  operation

  ;; The variable that this declaration affects.
  variable

  ;; The value associated with this declaration.  The meaning of this
  ;; field depends on OPERATION.
  value))

(define (declarations/bind operations declaration-set)
  (let loop
      ((operations operations)
       (declarations declaration-set))
    (if (null? declarations)
        operations
        (loop (let ((declaration (car declarations)))
                (operations/bind operations
                                 (decl-operation declaration)
                                 (decl-variable declaration)
                                 (decl-value declaration)))
              (cdr declarations)))))

;; This table transates infix operators into their prefix form.

(define *infix-operators*
  '((= equal?)
    (< <)
    (> >)
    (+ +)
    (- -)
    (* *)
    (** expt)
    (<> less-than-greater-than?) 
    (and and)
    (or or)
    ))

;; This table declares all the built-in forms

(define standard-declarations
  '((type block! block)
    (type break  (procedure 0))
    (type change (procedure 2))
    (type clear  (procedure 1))
    (type copy   (procedure 1))
    (type false  boolean)
    (type find   (procedure 2))
    (type first  (procedure 1))
    (type form   (procedure 1))
    (type fourth (procedure 1))
    (type insert (procedure 2))
    (type insert/part (procedure 3))
    (type head   (procedure 1))
    (type input  (procedure 0))
    (type length? (procedure 1))
    ;; Note, make function! is dealt with specially
    (type make   (procedure 2))
    (type match  (procedure 2))
    (type max    (procedure 2))
    (type not    (procedure 1))
    (type now    (procedure 0))
    (type next   (procedure 1))
    (type newline character)
    (type on     boolean)
    (type pick   (procedure 2))
    (type prin   (procedure 1))
    (type print  (procedure 1))
    (type random (procedure 1))
    (type return (procedure 1))
    (type skip   (procedure 2))
    (type select (procedure 2))
    (type tail   (procedure 1))
    (type tail?  (procedure 1))
    (type true   boolean)
    (type zero?  (procedure 1))
    ))
  

(define (parse-declaration declaration)
  (let ((operation (block/first declaration)))
    (cond ((eq? operation 'ARITY) 
           (list
            (make-decl 'TYPE
                       (block/second declaration)
                       `(PROCEDURE ,(block/third declaration)))))
          ((eq? operation 'standard-definitions)
           (map (lambda (spec)
                  (apply make-decl spec)) standard-declarations))
          ((eq? operation 'type)
           (list
            (make-decl 'TYPE
                       (block/second declaration)
                       (block/third declaration))))
          (else (error "Unrecognized declaration." operation)))))

;;; The heart of the compiler

;;; We handle infix operators in this cheesy way:
;;; When we are about to return a value, we peek ahead in the
;;; instruction stream to find out if there is an infix operator
;;; if so, we greedily convert it to the equivalent prefix operator.

;; This should be left associative, but it isn't.
(define (infix-wrapper ctenv if-more if-done)
  (lambda (value new-operations rest)
    (let* ((peek (block/first rest))
           (entry (assq peek *infix-operators*))
           (infix-more
             (lambda (rhvalue new-operations rest)
               (if-more (combination/make (cadr entry) (list value rhvalue)) new-operations rest)))
           (infix-done
             (lambda (rhvalue)
               (if-done (combination/make (cadr entry) (list value rhvalue))))))


      (if (eq? entry #f)
          (if-more value new-operations rest)
          (compile-one (block/rest rest) new-operations ctenv
                       (infix-wrapper ctenv infix-more infix-done)
                       infix-done)))))


(define (compile-sequence expr operations ctenv)
  (let ((combine-more     
         (lambda (value new-operations rest)
           (sequence/make (list value (compile-sequence rest new-operations ctenv))))))
    (compile-one expr operations ctenv
                 (infix-wrapper ctenv combine-more identity-procedure)
                 identity-procedure)))

(define (compile-one block operations ctenv if-more if-done)
(display ".") (flush-output)

  (if (block/empty? block)
      (if-done 'none)

  (let ((d (block/first block)))
    (cond ((self-evaluating? d) 
           (compile-self-evaluating d
                                    (block/rest block) 
                                    operations 
                                    ctenv 
                                    ;; NO!
                                    ;; We want infix to associate to the left.
                                    ;;(infix-wrapper ctenv if-more if-done)
                                    if-more
                                    if-done))

          ((block? d)
           (compile-block d (block/rest block)
                          operations
                          ctenv if-more if-done))

          ;; Parenthesized subexpressions
          ((list? d)
           (let ((value (compile-sequence (list->block d) operations ctenv)))
             (if (not (block/empty? (block/rest block)))
                 ((infix-wrapper ctenv if-more if-done) value operations
                                                        (block/rest block))
                 (if-done value))))

          ((definition? d)
           (compile-definition d
                               (block/rest block)
                               operations
                               ctenv
                               (infix-wrapper ctenv if-more if-done)
                               if-done))

          ((fetch? d)
           (compile-fetch d
                          (block/rest block)
                          operations
                          ctenv
                          (infix-wrapper ctenv if-more if-done)
                          if-done))

          ;; DECLARE is a reserved word.
          ((declaration? d)
           (process-declaration (block/second  block)
                                (block/rest (block/rest block))
                                operations
                                ctenv
                                if-more if-done))

          ;; Kludge, finesse the multiple arity by checking for this special case. 
          ((and (eq? d 'make)
                (not (block/empty? (block/rest block)))
                (eq? (block/second block) 'function!))
           (compile-func d
                         (block/third block)
                         (block/fourth block)
                         (block/rest (block/rest (block/rest (block/rest block))))
                         operations
                         ctenv
                         if-more
                         if-done))

          ;; FUNC is a reserved word.
          ((func? d)
           (compile-func d 
                         (block/second block) 
                         (block/third block)
                         (block/rest (block/rest (block/rest block)))
                         operations
                         ctenv
                         if-more
                         if-done))

          ;; IF is a reserved word
          ((if? d)
           (compile-if (block/rest block) operations ctenv 
                       (infix-wrapper ctenv if-more if-done)
                       if-done))

          ((symbol? d) (check-function d 
                                       (block/rest block) 
                                       operations 
                                       ctenv
                                       if-more if-done))
          (else (error "Unrecognized element" d))))))

(define (compile-self-evaluating thing tail operations ctenv if-more if-done)
  (if (block/empty? tail)
      (if-done thing)
      (if-more thing operations tail)))

(define (compile-block b tail operations ctenv if-more if-done)

  (define (descend-block b)
    (literal-block/make 
     (map (lambda (block-element)
            (if (block? block-element)
                (descend-block block-element)
                (literal/make block-element)))
          (block->list b))
     (compile-sequence b operations ctenv)))

  (let ((cb (descend-block b)))
    (if (block/empty? tail)
        (if-done cb)
        (if-more cb operations tail))))

(define (process-declaration declaration tail operations ctenv if-more if-done)
  (if (block/empty? tail)
      (if-done #f)
      (if-more #f
               (declarations/bind operations
                                  (parse-declaration declaration))
               tail)))

(define (compile-fetch f tail operations ctenv if-more if-done)
  (if (block/empty? tail)
      (if-done (fetch/base-name f))
      (if-more (fetch/base-name f) operations tail)))

(define special-forms '())

(define (check-function f tail operations ctenv if-more if-done)

  (define (generate-non-procedure)
    (if (block/empty? tail)
        (if-done f)
        (if-more f operations tail)))

  (define (generate-call-0)                ;zero arguments
    (if (block/empty? tail)
        (if-done (combination/make f '()))
        (if-more (combination/make f '()) operations tail)))

  (define (generate-call-1)                ;one argument
    (let ((one-arg-more       
           (lambda (arg0 ctenv expr)
             (if-more (combination/make f (list arg0)) operations expr)))
          (one-arg-done
           (lambda (arg0)
             (if-done (combination/make f (list arg0))))))
      (compile-one tail operations ctenv
                   (infix-wrapper ctenv one-arg-more one-arg-done)
                   one-arg-done)))

  (define (generate-call-2)                ;two arguments
    (let ((first-arg-more
           (lambda (arg0 ctenv expr)
             (let ((second-arg-more
                    (lambda (arg1 ctenv expr)
                      (if-more (combination/make f (list arg0 arg1)) operations expr)))
                   (second-arg-done
                    (lambda (arg1)
                      (if-done (combination/make f (list arg0 arg1))))))
             (compile-one expr operations ctenv
                          (infix-wrapper ctenv second-arg-more second-arg-done)
                          second-arg-done))))
          (first-arg-done
           (lambda (arg0)
             (too-few-arguments f))))
      (compile-one tail operations ctenv
                   (infix-wrapper ctenv first-arg-more first-arg-done)
                   first-arg-done)))

  (define (generate-call-3)                ;three arguments
    (let ((first-arg-more
           (lambda (arg0 ctenv expr)
             (let ((second-arg-more
                    (lambda (arg1 ctenv expr)
                      (let ((third-arg-more
                             (lambda (arg2 ctenv expr)
                               (if-more (combination/make f (list arg0 arg1 arg2))
                                        operations expr)))
                            (third-arg-done
                             (lambda (arg2)
                               (if-done (combination/make f (list arg0 arg1 arg2))))))
                        (compile-one expr operations ctenv
                                     (infix-wrapper ctenv third-arg-more third-arg-done)
                                     third-arg-done))))
                   (second-arg-done
                    (lambda (arg1)
                      (too-few-arguments f))))
               (compile-one expr operations ctenv
                            (infix-wrapper ctenv second-arg-more second-arg-done)
                            second-arg-done))))
          (first-arg-done
           (lambda (arg0)
             (too-few-arguments f))))
      (compile-one tail operations ctenv
                   (infix-wrapper ctenv first-arg-more first-arg-done)
                   first-arg-done)))

  (define (generate-call-4)                ;four arguments
    (let ((first-arg-more
           (lambda (arg0 ctenv expr)
             (let ((second-arg-more
                    (lambda (arg1 ctenv expr)
                      (let ((third-arg-more
                             (lambda (arg2 ctenv expr)
                               (let ((fourth-arg-more
                                      (lambda (arg3 ctenv expr)
                                        (if-more 
                                         (combination/make f (list arg0 arg1 arg2 arg3))
                                         operations expr)))
                                     (fourth-arg-done
                                      (lambda (arg3)
                                        (if-done
                                         (combination/make f (list arg0 arg1 arg2 arg3))))))
                                 (compile-one expr operations ctenv
                                              (infix-wrapper ctenv fourth-arg-more fourth-arg-done)
                                              fourth-arg-done))))
                            (third-arg-done
                             (lambda (arg2)
                               (too-few-arguments f))))
                        (compile-one expr operations ctenv
                                     (infix-wrapper ctenv third-arg-more third-arg-done)
                                     third-arg-done))))
                   (second-arg-done
                    (lambda (arg1)
                      (too-few-arguments f))))
               (compile-one expr operations ctenv
                            (infix-wrapper ctenv second-arg-more second-arg-done)
                            second-arg-done))))
          (first-arg-done
           (lambda (arg0)
             (too-few-arguments f))))
      (compile-one tail operations ctenv
                   (infix-wrapper ctenv first-arg-more first-arg-done)
                   first-arg-done)))


  (define (generate-procedure-call)
    (arity-case/make f
        `(((0) ,(generate-call-0))

          ;;These should really be abstracted out.

          ,@(if (and (not (block/empty? tail))
                     (not (definition? (block/first tail)))
                     (not (assq (block/first tail) *infix-operators*)))
                `(((1) ,(generate-call-1)))
                '())
          ,@(if (and (not (block/empty? tail))
                     (not (definition? (block/first tail)))
                     (not (assq (block/first tail) *infix-operators*))
                     (not (block/empty? (block/rest tail)))
                     (not (definition? (block/second tail)))
                     (not (assq (block/second tail) *infix-operators*))
                     )
                `(((2) ,(generate-call-2)))
                '())
          ,@(if (and (not (block/empty? tail))
                     (not (definition? (block/first tail)))
                     (not (assq (block/first tail) *infix-operators*))
                     (not (block/empty? (block/rest tail)))
                     (not (definition? (block/second tail)))
                     (not (assq (block/second tail) *infix-operators*))
                     (not (block/empty? (block/rest (block/rest tail))))
                     (not (definition? (block/third tail)))
                     (not (assq (block/third tail) *infix-operators*))
                     )
                `(((3) ,(generate-call-3)))
                '())
          ,@(if (and (not (block/empty? tail))
                     (not (definition? (block/first tail)))
                     (not (assq (block/first tail) *infix-operators*))
                     (not (block/empty? (block/rest tail)))
                     (not (definition? (block/second tail)))
                     (not (assq (block/second tail) *infix-operators*))
                     (not (block/empty? (block/rest (block/rest tail))))
                     (not (definition? (block/third tail)))
                     (not (assq (block/third tail) *infix-operators*))
                     (not (block/empty? (block/rest (block/rest (block/rest tail)))))
                     (not (definition? (block/fourth tail)))
                     (not (assq (block/fourth tail) *infix-operators*))
                     )
                `(((4) ,(generate-call-4)))
                '())
          (ELSE ,(too-few-arguments f)))))

  (let ((special-compiler (assq f special-forms)))
    (if (eq? special-compiler #f)
        (operations/lookup operations f
          (lambda (operation info) 
            (case operation
              ((TYPE) (cond ((eq? info 'procedure) (generate-procedure-call))
                            ((and (pair? info)
                                  (eq? (car info) 'PROCEDURE))
                             (cond ((null? (cdr info)) (generate-procedure-call))
                                   ((= (cadr info) 0) (generate-call-0))
                                   ((= (cadr info) 1) (generate-call-1))
                                   ((= (cadr info) 2) (generate-call-2))
                                   ((= (cadr info) 3) (generate-call-3))
                                   ((= (cadr info) 4) (generate-call-4))
                                   (else (error "Can't handle this arity yet."))))
                            (else (generate-non-procedure))))
              (else "Unknown operation" operation)))

          (lambda ()
            (conditional/make
             (procedure-test/make f)
             (generate-procedure-call)
             (generate-non-procedure))))
        ((cdr special-compiler) tail operations ctenv if-more if-done))))

(define (compile-else tail operations ctenv if-more if-done)
  (if-done (dangling-else/make)))

(define (compile-for tail operations ctenv if-more if-done)
    (let* ((name (block/first tail))
          (first-arg-more
           (lambda (arg0 ctenv expr)
             (let ((second-arg-more
                    (lambda (arg1 ctenv expr)
                      (let ((third-arg-more
                             (lambda (arg2 ctenv expr)

                               (let ((body (block/first expr)))
                                 (cond ((not (block? body))
                                        `(#%ERROR "Missing block in FOR"))
                                       ((block/empty? (block/rest expr))
                                        (if-done (for/make name arg0 arg1 arg2
                                                           (compile-sequence body operations ctenv))))
                                       (else
                                        (if-more (for/make name arg0 arg1 arg2
                                                           (compile-sequence body operations ctenv)) operations (block/rest expr)))))))
                            (third-arg-done
                             (lambda (arg2)
                               (too-few-arguments 'for))))
                        (compile-one expr operations ctenv
                                     (infix-wrapper ctenv third-arg-more third-arg-done)
                                     third-arg-done))))
                   (second-arg-done
                    (lambda (arg1)
                      (too-few-arguments 'for))))
               (compile-one expr operations ctenv
                            (infix-wrapper ctenv second-arg-more second-arg-done)
                            second-arg-done))))
          (first-arg-done
           (lambda (arg0)
             (too-few-arguments 'for))))
      (compile-one (block/rest tail) operations ctenv
                   (infix-wrapper ctenv first-arg-more first-arg-done)
                   first-arg-done)))




(define (compile-foreach tail operations ctenv if-more if-done)
  (let* ((name (block/first tail))

         (foreach-more (lambda (value ctenv* rest)
                         (if (block? (block/first rest))
                             (let ((result 
                                    (foreach/make name value
                                                  (compile-sequence (block/first rest) operations ctenv*))))
                               (if (block/empty? (block/rest rest))
                                   (if-done result)
                                   (if-more result ctenv* (block/rest rest))))
                             (too-few-arguments 'foreach))))
         (foreach-done
          (lambda (done)
            (too-few-arguments 'foreach))))
    (compile-one (block/rest tail) operations ctenv
                 (infix-wrapper ctenv foreach-more foreach-done)
                 foreach-done)))

(define (compile-loop tail operations ctenv if-more if-done)
  (let* ((name (gensym))
         (repeat-more
          (lambda (value ctenv* rest)
            (if (block? (block/first rest))
                (let ((result
                       (repeat/make name value
                                    (compile-sequence (block/first rest) operations ctenv*))))
                  (if (block/empty? (block/rest rest))
                      (if-done result)
                      (if-more result ctenv* (block/rest rest))))
                (too-few-arguments 'loop))))
         (repeat-done
          (lambda (value)
            (too-few-arguments 'loop))))

    (compile-one tail operations ctenv
                 (infix-wrapper ctenv repeat-more repeat-done)
                 repeat-done)
      ))


(define (compile-repeat tail operations ctenv if-more if-done)
  (let* ((name (block/first tail))
         (repeat-more
          (lambda (value ctenv* rest)
            (if (block? (block/first rest))
                (let ((result
                       (repeat/make name value
                                    (compile-sequence (block/first rest) operations ctenv*))))
                  (if (block/empty? (block/rest rest))
                      (if-done result)
                      (if-more result ctenv* (block/rest rest))))
                (too-few-arguments 'repeat))))
         (repeat-done
          (lambda (value)
            (too-few-arguments 'repeat))))

    (compile-one (block/rest tail) operations ctenv
                 (infix-wrapper ctenv repeat-more repeat-done)
                 repeat-done)
      ))

(define (compile-until tail operations ctenv if-more if-done)
  (let* ((body (block/first tail))
         (cbody (until/make (compile-sequence body operations ctenv))))
    
    (if (block/empty? (block/rest tail))
        (if-done cbody)
        (if-more cbody ctenv (block/rest tail)))))

(define (compile-use tail operations ctenv if-more if-done)
  (let ((names (block/first tail))
        (body (block/second tail))
        (tail* (block/rest (block/rest tail))))

    (let ((cbody (use/make (block->list names) 
                           (compile-sequence body operations ctenv))))

      (if (block/empty? tail*)
          (if-done cbody)
          ((infix-wrapper ctenv if-more if-done) cbody operations tail*))))) 

(define (compile-while tail operations ctenv if-more if-done)
  (let ((test (block/first tail))
        (body (block/second tail)))
    (if (not (block? test)) (error "bogus test in while")) 
    (if (not (block? body)) (error "bogus body in while")) 
    (let ((ctest (compile-sequence test operations ctenv))
          (cbody (compile-sequence body operations ctenv)))
      (if (block/empty? (block/rest (block/rest tail)))
          (if-done (while/make ctest cbody))
          ((infix-wrapper ctenv if-more if-done) 
           (while/make ctest cbody) operations (block/rest (block/rest tail)))))))


(set! special-forms 
      `((ELSE    . ,compile-else)
        (FOR     . ,compile-for)
        (FOREACH . ,compile-foreach)
        (LOOP    . ,compile-loop)
        (REPEAT  . ,compile-repeat)
        (UNTIL   . ,compile-until)
        (USE     . ,compile-use)
        (WHILE   . ,compile-while)
      ))


(define (compile-definition d tail operations ctenv if-more if-done)
  ;; definitions should be scanned out, so this is becomes an
  ;; assignment
  (define (generate-assignment value)
    (assignment/make
     (definition/base-name d)
     value))

  (compile-one tail operations ctenv
               (lambda (value ctenv* rest)
                 (if-more (generate-assignment value)
                          operations
                          rest))
               (lambda (value)
                 (if-done (generate-assignment value)))))

(define (compile-func d args body tail operations ctenv if-more if-done)

  (define (generate-lambda args body)
    (lambda/make (block->list args)
                 (compile-sequence body operations ctenv)))


  (if (block/empty? tail)
      (if-done (generate-lambda args body))
      (if-more (generate-lambda args body) operations tail)))

(define (compile-if tail operations ctenv if-more if-done)
  (let ((after-conditional-more
         (lambda (value ctenv* rest)
           (if (not (block? (block/first rest)))
               (if-done `(#%ERROR "Missing block in IF"))

           (let ((if-true (compile-sequence (block/first rest) operations ctenv)))
             (cond ((block/empty? (block/rest rest))
                    (if-done (conditional/make value if-true #f)))
                   ((eq? (block/second rest) 'else)
                    (let ((if-false (compile-sequence (block/third rest) operations ctenv))
                          (ttail (block/rest (block/rest (block/rest rest)))))
                      (if (block/empty? ttail)
                          (if-done (conditional/make value if-true if-false))
                          (if-more (conditional/make value if-true if-false)
                                   operations ttail))))
                   (else
                    (if-more (conditional/make value if-true #f)
                             operations
                             (block/rest rest))))))))
        (after-conditional-done
         (lambda (value)
           `(#%ERROR "Missing block in IF"))))

    (compile-one tail operations ctenv
                 (infix-wrapper ctenv after-conditional-more after-conditional-done)
                 after-conditional-done)

))


;;; Top level functions

(define source-file-extension "r")
(define parsed-file-extension "rs")
(define translated-file-extension "ss")        ;mzscheme specific
(define compiled-file-extension "zo")   ;mzscheme specific

(define (extension-checker extension)
  (lambda (file) (string-ci=? (filename-extension file) extension)))

(define source-file?     (extension-checker source-file-extension))
(define parsed-file?     (extension-checker parsed-file-extension))
(define translated-file? (extension-checker translated-file-extension))

(define (file-new-extension source extension)
  (let* ((len (string-length source))
         (base (let loop ((p (sub1 len)))
                      (cond ((negative? p) source)
                            ((char=? (string-ref source p) #\.)
                             (substring source 0 p))
                            (else (loop (sub1 p)))))))
    (string-append base "." extension)))


;;; Runtime support

(define (translate-file translator source dest)
  (if (or (not (file-exists? dest))
          (> (file-or-directory-modify-seconds source)
             (file-or-directory-modify-seconds dest)))
      (translator source dest)))

(define (parse-file source dest)
  (system (string-append "r2s <" source " >" dest)))

(define (sherman source)
  (cond ((source-file? source) 
         (let ((parsed (file-new-extension source parsed-file-extension)))
           (translate-file parse-file source parsed)
           (sherman parsed)))
        ((parsed-file? source)
         (let ((compiled (file-new-extension source translated-file-extension)))
           (translate-file crsf source compiled)
           (sherman compiled)))
        ((translated-file? source)
         (let ((compiled (file-new-extension source compiled-file-extension)))
           (translate-file compile-file source compiled)
           (sherman compiled)))
        (else #t)))
