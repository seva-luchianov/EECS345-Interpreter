;;;; *********************************************************************************************************
;;;; * Group 20
;;;; * EECS 345 Spring 2020
;;;; * Interpreter, Part 4 (Based on part 2 solutions)
;;;; *********************************************************************************************************

; Requires the new parser which includes functions.
#lang racket
(require "classParser.rkt")

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; Just to see what test the parser returns
(define show
  (lambda ()
    (parser "t.txt")))

(define test
  (lambda ()
    (interpret "t.txt")))

; The main function. Parser to get the parse tree and interprets it with a new environment. The returned value is in the environment.
; Must be invoked with the filename and the class that has the main method in it.
(define interpret
  (lambda (file class)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-top-level-statement-list (add-invoke-main (validate-top-level (parser file)) class) (newenvironment) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; Top-level must be classes only
(define validate-top-level
  (lambda (statement-list)
    (cond
      ((null? statement-list) '())
      ((eq? (statement-type (car statement-list)) 'class) (cons (car statement-list) (validate-top-level (cdr statement-list))))
      (else (myerror "Cannot have anything except class defined at top-level:" (statement-type (car statement-list)))))))

; Need to invoke main function after everything gets parsed
(define add-invoke-main
  (lambda (statement-list class)
    (append statement-list (cons 'funcall (cons (cons 'dot (cons (cons 'new (cons class '())) '(main))) '())))))

; This does not handle non-class statement at the top level scope on purpose.
; The validate-top-level function asserts that the parsed list is valid at the top level.
; Afterwards, we add an invoke main statement to the end of the list and that will get executed correctly here.
(define interpret-top-level-statement-list
  (lambda (statement-list environment return break continue throw)
    (cond
      ((null? statement-list) environment)
      ((eq? 'funcall (statement-type statement-list)) (interpret-statement statement-list environment return break continue throw '()))
      (else
       (interpret-top-level-statement-list (cdr statement-list)
                                           ; handle interpret-class here. otherwise just invoke interpret-statement
                                           ((if (eq? 'class (statement-type (car statement-list))) interpret-class interpret-statement) ;statement
                                            (car statement-list) environment return break continue throw)
                                            return break continue throw)))))

(define interpret-class
  (lambda (statement environment return break continue throw)
    (cond
      ; Class names must be unique because we do not support sub-classes
      ((exists? (class-name statement) environment) (myerror "Class already defined:" (class-name statement)))
      ; Class does not extend another class, so create it with a fresh environment
      ((null? (class-parent statement)) (insert (class-name statement) (handle-class-environment (newenvironment) (class-body statement) return break continue throw) environment))
      ; Otherwise, lookup the parent class and use the environment of the parent class when initializing the new class.
      (else (insert (class-name statement) (cons (get-stored-class-environment (lookup (class-parent-name statement))) (class-body statement)) environment)))))

;Builds class environment
(define handle-class-environment
  (lambda (environment body return break continue throw)
    (interpret-statement-list body environment return break continue throw '())))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw instance)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw instance) return break continue throw instance))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  ; We might need to pass around an evironment and a top-level-environment to help with scope issues. We have boxes so it shouldnt be a problem with references.
  (lambda (statement environment return break continue throw instance)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return break continue throw instance))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw))
      ;((eq? 'static-var (statement-type statement)) (interpret-static-declare statement environment return break continue throw))
      ((eq? 'dot (statement-type statement)) (interpret-dot statement environment return break continue throw instance))
      ;((eq? 'new (statement-type statement)) (interpret-new statement environment return break continue throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return break continue throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return break continue throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment return break continue throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ;((eq? 'constructor (statement-type statement)) (interpret-constructor statement environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment return break continue throw))
      ((eq? 'static-function (statement-type statement)) (interpret-function statement environment return break continue throw))
      ;((eq? 'abstract-function (statement-type statement)) (interpret-abstract-function statement environment return break continue throw))
      ((eq? 'funcall (statement-type statement))
       (if (eq? (caddr (cadr statement)) 'main) ;changed from get-function-name to operand2, add-invoke-main sets up the main function call to be using the dot operator
           ; TODO: does this logic make sense anymore with the new scoping?
           (invoke-function statement environment return break continue throw)
           (begin
             (invoke-function statement environment return break continue throw)
             environment)))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return break continue throw instance)
    (return (eval-expression (get-expr statement) environment return break continue throw instance))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment return break continue throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment return break continue throw '()) environment)
        (insert (get-declare-var statement) 'novalue environment))))
#|
(define interpret-static-declare
  (lambda (statement environment return break continue throw)
    #|mystery code|#))|#

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment return break continue throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment return break continue throw '()) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment return break continue throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment return break continue throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw instance)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))
                                         instance))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment return break continue throw)
    (throw (eval-expression (get-expr statement) environment return break continue throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; we are essentially defining a variable that maps to an expression
(define interpret-function
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? (get-function-name statement) 'main)
       (insert-main (cons (get-function-body statement) (cons (get-function-variables statement) '())) environment))
      (else ; environment mapping is [func-name: (func-body func-vars)]
       (insert (get-function-name statement) (cons (get-function-body statement) (cons (get-function-variables statement) '())) environment)))))

;Interprets dot operator
;Handles calls for new class instance by passing to interpret-new, otherwise checks if class instance exists, calls interpret-statement on the 
(define interpret-dot ;operand1 = class, operand2 = variable
  (lambda (statement environment return break continue throw instance)
    (cond
      ((eq? (operand1 statement) 'this) (lookup (operand2 statement) (lookup instance environment)))
      (else (lookup (operand2 statement) (lookup (operand1 statement) environment))))))
#|    (cond
      ((eq? (operator (operand1 statement)) 'new) (interpret-dot (cons 'dot (cons (interpret-new (operator (operand1 statement))) (cons (operand2 statement) '())))))
      ((exists? (operand1 statement)) (interpret-statement (operand2 statement) (lookup (operand1 statement) environment) return break continue throw))
      (else (myerror "Undeclared class instance: " (operand1 statement))))))
|#

;interprets new statements: returns the environment of the class
;TODO: Should handle static/non-static variables (thinking we could have a function to go through and make new boxes in the returned environment for non-static variables, but just return the same box for static variables
(define interpret-new
  (lambda (expr environment return break continue throw)
    (cond
      ((exists? (operand1 expr) environment) (lookup (operand1 expr) environment))
      (else (myerror "Trying to use undeclared class: " (operand1 expr))))))
      

#|
(define interpret-static-function
  (lambda (statement environment return break continue throw)
    ))


(define interpret-abstract-function
  (lambda (statement environment return break continue throw)
    #|mystery code|#))

(define interpret-constructor
  (lambda (statement environment return break continue throw)
    #|mystery code|#))
|#

;TEMP
(define extract-dot-statement cadr)
(define get-instance
  (lambda statement
    (dot-handle-new (operand1 (extract-dot-statement (car statement))))))
         
;Runs and evaluates functions invoked in the code
(define invoke-function
  (lambda (statement environment return break continue throw)
    (cond
      ;((is-dot? statement) (invoke-function statement environment return break continue throw))
      ((exists? (get-function-name-dot statement) (get-dot-environment statement environment))
            (call/cc
             (lambda (new-return)
               (begin
                 (pop-frame (interpret-block
                             (cons 'begin (get-function-body-from-environment (lookup (extract-dot-statement statement) environment)))
                             (assign-function-input-variables
                              (get-function-variables-from-environment (lookup (extract-dot-statement statement) environment))
                              (get-function-param-values (get-function-variables-for-assign statement) environment return break continue throw)
                              (push-frame (pop-frames-to-function-scope (extract-dot-statement statement) environment))
                              new-return break continue (lambda (v env) (throw v environment)))
                             new-return break continue (lambda (v env) (throw v environment))
                             (get-instance statement)))
                 environment))))
            (else (myerror "error: function not defined:" (get-function-name-dot statement))))))

;Used to make sure that functions are being evaluated in the proper scope
; TODO: this is no longer the correct functionality. Now, this function needs to pass in the environment of the class that the function is defined in.
; - new A().whatever is easy, just use the newly created A() class's environment. But what to do for static functions?
(define pop-frames-to-function-scope
  (lambda (function-name environment)
    (cond
      ((null? environment) (myerror "error: function not defined in lookup:" function-name))
      ((exists-in-list? (operand2 function-name) (variables (topframe (get-dot-environment function-name environment)))) environment)
      (else (pop-frames-to-function-scope function-name (pop-frame environment))))))

; assign function input variables once function is invoked
(define assign-function-input-variables
  (lambda (variable-names variable-values environment return break continue throw)
    (cond
      ((and (null? variable-names) (null? variable-values)) environment)
      ((null? variable-names) (myerror "error: too many variables passed into function"))
      ((null? variable-values) (myerror "error: not enough variables passed into function"))
      (else ((if (exists? (car variable-names) environment) update insert)
             (car variable-names)
             (eval-expression (car variable-values) environment return break continue throw '())
             (assign-function-input-variables (cdr variable-names) (cdr variable-values) environment return break continue throw))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment return break continue throw instance)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment return break continue throw instance)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment return break continue throw instance)
    (cond
      ((eq? 'dot (operator expr)) (interpret-dot expr environment return break continue throw instance))
      ((eq? 'new (operator expr)) (interpret-new expr environment return break continue throw))
      ((eq? 'funcall (operator expr)) (invoke-function expr environment return break continue throw))
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment return break continue throw instance)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment return break continue throw instance)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment return break continue throw instance) environment return break continue throw instance)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment return break continue throw instance)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment return break continue throw instance))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment return break continue throw instance)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

(define get-function-name operand1)
(define get-function-name-dot
  (lambda l
    (operand2 (operand1 (car l)))))
(define get-function-variables operand2)
(define get-function-body operand3)

(define class-name cadr)
(define class-parent caddr)
(define class-parent-name
  (lambda (statement)
    (cadr (class-parent statement))))
(define class-body cadddr)

(define get-stored-class-environment car)
(define get-stored-class-body cadr)

(define topframe car)
(define remainingframes cdr)

(define get-function-body-from-environment car)
(define get-function-variables-from-environment cadr)
(define get-function-variables-for-assign cddr)

;Helper to identify dot functions
(define is-dot?
  (lambda statement
    (if (list? (cadr (car statement))) #t
        #f)))
;------------------------
; Class Environment/State Helper Functions
;------------------------

;(define 

;------------------------
; Environment/State Helper Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; does a variable exist in the environment?
;TODO: Update to check all 
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))
      

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

;gets the environment used for a dot operation
;TODO: handle new operator
(define get-dot-environment
  (lambda (statement environment)
    (cond
      ((eq? 'dot (car statement))
      (if (pair? (operand1 statement)) (lookup (dot-handle-new (operand1 statement)) environment)
          (lookup-instance statement environment)))
      (else (get-dot-environment (cadr statement) environment)))))

(define dot-handle-new
  (lambda (statement)
    (cond
      ((not (pair? statement)) statement)
      ((eq? 'new (car statement)) (operand1 statement))
      ;((not (list? statement)) (lookup statement environment))
      (else '())))) ;TODO: fix to handle instances of classes, else

(define lookup-instance
  (lambda (statement environment)
    ;(lookup (lookup (operand1 statement) environment) environment)))
    (lookup (operand1 statement) environment)))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (cond
      ((list? var) (dot-lookup-variable var environment))
      (else (lookup-variable var environment)))))

;Helper function to look for variables in specific classes
;var comes in as (dot a x) where a is the class (either as a variable or with New) x is variable to lookup
(define dot-lookup-variable
  (lambda (var environment)
    (cond
      ((list? (operand1 var)) (lookup (operand2 var) (lookup (dot-handle-new (operand1 var)) environment)))
      (else (lookup (operand2 var) (lookup-instance var environment))))))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      ((box? (get-value (indexof var (variables frame)) (store frame))) (language->scheme (unbox (get-value (indexof var (variables frame)) (store frame)))))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Maybe get the top-level environment here?
(define insert-main
  (lambda (main-body environment)
    (if (exists-in-list? 'main (variables (car environment)))
        (myerror "error: can only declare one main function")
        (cons (add-to-frame 'main main-body (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons(scheme->language (box val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    ;(update-in-frame-store var val (variables frame) (store frame))))
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (begin
                                       (set-box! (car vallist) (scheme->language val))
                                       (car vallist))
                                     (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.
(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

;Does what it says on the tin (gets the param values when functions called)
(define get-function-param-values
  (lambda (value-lis environment return break continue throw)
    (cond
      [(null? value-lis) '()]
      [(or (eq? (car value-lis) 'true) (eq? (car value-lis) 'false)) (cons (car value-lis) (get-function-param-values (cdr value-lis) environment return break continue throw))]
      [(and (not (number? (car value-lis))) (not (list? (car value-lis)))) (cons (lookup (car value-lis) environment) (get-function-param-values (cdr value-lis) environment return break continue throw))]
      [(number? (car value-lis)) (cons (car value-lis) (get-function-param-values (cdr value-lis) environment return break continue throw))]
      [(list? (car value-lis)) (cons (eval-expression (car value-lis) environment return break continue throw) (get-function-param-values (cdr value-lis) environment return break continue throw))])))
          

