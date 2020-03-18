;;;; *********************************************************************************************************
;;;; * Group 16
;;;; * EECS 345 Spring 2020
;;;; * Interpreter, Part 1
;;;; *********************************************************************************************************

#lang racket

;add layer
(define addLayer
  (lambda (state)
    (append (initState) state)))

; Load the parser
(require "simpleParser.rkt")

; Left operand of an expression (0 X 0)
(define leftoperand cadr)

; Operator of an expression (X 0 0)
(define operator car)

; Right operand of an expression (0 0 X)
(define rightoperand caddr)

; The else statement within an if statement (0 0 0 X)
(define thirdOperand
  (lambda (expression)
    (car (cdr (cdr (cdr expression))))))

; Returns the initial state
(define initState
  (lambda ()
    '(())))

; get_return_val. Takes an expression and changes #t to 'true and #f to 'false while leaving any other type
; of statement the untouched.
; Param: expression - the expression that will be changed if it is equal to #f or #t.
; Return: 'true is the statement was #t, 'false is the statement was #f, and the original expression
; if it was neither.
(define get_return_val
  (lambda (expression)
    (cond
      [(eq? expression #t) 'true]
      [(eq? expression #f) 'false]
      [else expression])))

; (interpret "t.txt")
;((var x) (= x 0) (try ((= x (+ x 10))) (catch (e) ((= x (+ x 100)))) (finally ((= x (+ x 1000))))) (return x))

; The interpret main function. It calls the parser and uses that output to calculate the end state of the
; input file
; Param: filename - the name of the text file containing the code to be parsed
; Return: The return value of the function from the state that it calculated
(define interpret
  (lambda (filename)
    (get_return_val
     (call/cc
      (lambda (return-cont)
        (Mstate
         (parser filename)
         (initState)
         ; use default configureation but set return to jump here
         (setReturnCont (generateContinuations) return-cont)
         ))))))

; Mstate. Obtains the state of an expression given a state. The original state is set to only contain return
; without a value
; Param: expression - the expression of which to generate the new state from
; Param: state - the old state used to obtain the new state with the given expression
; Return: The state of the code after the expression
(define Mstate
  (lambda (expression state continuations)
    (cond
      [(null? expression) state]
      [(number? state) state]
      [(list? (car expression)) (Mstate (cdr expression) (Mstate (car expression) state continuations) continuations)]
      [else
       (cond
         [(isValueOp expression) Mvalue(expression state continuations)]
         [(isBoolOp expression) Mboolean(expression state continuations)]
         [(eq? (operator expression) 'return) ((getReturnCont continuations) (Mvalue (cadr expression) state continuations))]
         [(eq? (operator expression) 'break) ((getBreakCont continuations) (popLayer state))]
         [(eq? (operator expression) 'continue) ((getContinueCont continuations) (popLayer state))]
         [(eq? (operator expression) 'throw) ((getThrowCont continuations) (Mvalue (cadr expression) state continuations))]
         [(eq? (operator expression) 'var) (declare expression state continuations)]
         [(eq? (operator expression) '=) (assign (cdr expression) state continuations)]
         [(eq? (operator expression) 'while) (call/cc (lambda (break-cont) (whileStatement
                                                                            (leftoperand expression)
                                                                            (rightoperand expression)
                                                                            state
                                                                            (setBreakCont continuations break-cont))))]
         [(eq? (operator expression) 'begin) (popLayer (Mstate (cdr expression) (addLayer state) continuations))]
         [(eq? (operator expression) 'try) (tryStatement (cdr expression) (addLayer state) continuations)]
         [(eq? (operator expression) 'if) (if (doesNotHaveElseStatement expression)
                                              (ifStatement
                                               (leftoperand expression)
                                               (rightoperand expression)
                                               state
                                               continuations)
                                              (ifElseStatement
                                               (leftoperand expression)
                                               (rightoperand expression)
                                               (thirdOperand expression)
                                               state
                                               continuations))]
         [else state])])))

; continuation helpers
; used so that the function signature for Mstate and all helpers just takes in a single continuations parameter
(define generateContinuations
  (lambda () 
    ; return-cont (Should never be invalid so this will always be override right now)
    (cons (lambda (v) ('error "Invalid return statement"))
          ; break-cont
          (cons (lambda (v) ('error "Invalid break statement"))
                ; continue-cont
                (cons (lambda (v) ('error "Invalid continue statement"))
                      ; throw-cont
                      (cons (lambda (v) ('error v))
                            ; default finally-cont is null
                            (cons null '())))))))

(define pushFinallyContinuation
  (lambda (continuations finally-cont)
    (setReturnCont
     (setBreakCont
      (setContinueCont
       (setThrowCont
        continuations
        (lambda (v) ((getThrowCont continuations) (finally-cont v))))
       (lambda (v) ((getContinueCont continuations) (finally-cont v))))
      (lambda (v) ((getBreakCont continuations) (finally-cont v))))
     (lambda (v) ((getReturnCont continuations) (finally-cont v))))))

(define getReturnCont car)
(define getBreakCont cadr)
(define getContinueCont caddr)
(define getThrowCont cadddr)

(define replace-atom
  (lambda (lis index a)
    (cond
      [(null? lis) '()]
      [(zero? index) (cons a (cdr lis))]
      [else (cons (car lis) (replace-atom (cdr lis) (- index 1) a))])))

(define setReturnCont
  (lambda (continuations cont)
    (replace-atom continuations 0 cont)))

(define setBreakCont
  (lambda (continuations cont)
    (replace-atom continuations 1 cont)))

(define setContinueCont
  (lambda (continuations cont)
    (replace-atom continuations 2 cont)))

(define setThrowCont
  (lambda (continuations cont)
    (replace-atom continuations 3 cont)))

; M_value. Obtains the value of an numerical expression. Can do the following operators: +,-,*,/,%,(negation),
; as well as return the values of declared and assigned variables
; Param: expression- a numeric expression that you wish to obtain the value of
; Param: state - the state of the code before the expression.
; Return: the value of the expression
(define Mvalue
  (lambda (expression state continuations)
    (cond
      [(null? expression) ((getThrowCont continuations) "parser should have caught this")]
      [(number? expression) expression]
      [(and (not (number? expression)) (not (list? expression))) (getLookupValue expression state continuations)]
      [(and (eq? '- (operator expression)) (null? (cdr (cdr expression))))
       (* -1 (Mvalue(leftoperand expression) state continuations))]
      [(eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state continuations)
                                         (Mvalue (rightoperand expression) state continuations))]
      [(eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state continuations)
                                         (Mvalue (rightoperand expression) state continuations))]
      [(eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state continuations)
                                         (Mvalue (rightoperand expression) state continuations))]
      [(eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state continuations)
                                                (Mvalue (rightoperand expression) state continuations))]
      [(eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state continuations)
                                                 (Mvalue (rightoperand expression) state continuations))]
      [else ((getThrowCont continuations) "The operator is not known")])))

; Mboolean. Takes a boolean expression and returns the boolean value of that expression.
; Param: expression - The boolean expression of which you want to obtain the boolean value of
; Param: state - the state of the code before the boolean expression is evaluated.
(define Mboolean
  (lambda (expression state continuations)
    (cond
      [(null? expression) '()]
      [(eq? expression 'true) #t]
      [(eq? expression 'false) #f]
      [(and (not (list? expression)) (not (isValueOp expression))) (getLookupValue expression state continuations)]
      [(eq? (operator expression) '==) (= (Mvalue (leftoperand expression) state continuations)
                                          (Mvalue (rightoperand expression) state continuations))]
      [(eq? (operator expression) '<) (< (Mvalue (leftoperand expression) state continuations)
                                         (Mvalue (rightoperand expression) state continuations))]
      [(eq? (operator expression) '>) (> (Mvalue (leftoperand expression) state continuations)
                                         (Mvalue (rightoperand expression) state continuations))]
      [(eq? (operator expression) '!=) (not (= (Mvalue (leftoperand expression) state continuations)
                                               (Mvalue (rightoperand expression) state continuations)))]
      [(eq? (operator expression) '<=) (<= (Mvalue (leftoperand expression) state continuations)
                                           (Mvalue (rightoperand expression) state continuations))]
      [(eq? (operator expression) '>=) (>= (Mvalue (leftoperand expression) state continuations)
                                           (Mvalue (rightoperand expression) state continuations))]
      [(eq? (operator expression) '&&) (and (Mboolean (leftoperand expression) state continuations)
                                            (Mboolean (rightoperand expression) state continuations))]
      [(eq? (operator expression) '||) (or (Mboolean (leftoperand expression) state continuations)
                                           (Mboolean (rightoperand expression) state continuations))]
      [(eq? (operator expression) '!) (not (Mboolean (leftoperand expression) state continuations))]
      [else ((getThrowCont continuations) "The operator is not known")])))

; lookup. Looks for a variable within a state and returns its value if it is found.
; Param: a - the atom representing the variable that you wish to look up the value of.
; Param: state- the state for which you will be looking through to obtain the value of the variable a.
; Return: the value of the variable if it is present within the state or an error if it is not.
(define lookup
  (lambda (a state)
    (cond
      [(number? a) a]
      [(null? state) '()]
      [(and (eq? a (car (car state))) (not (null? (cdr (car state))))) (car (cdr (car state)))]
      [else (lookup a (cdr state))])))

(define LayerLookup
  (lambda (a state)
    (cond
      [(null? state) '()]
      [(number? state) state]
      [else (cons (lookup a (car state)) (LayerLookup a (cdr state)))])))

(define getLookupValue
  (lambda (a state continuations)
    (cond
      [(null? (flatten (LayerLookup a state))) ((getThrowCont continuations) "variable for lookup not assigned or declared in any layer")]
      [else (car (flatten (LayerLookup a state)))])))

; declare. Takes an expression and a state and declares a new varible within the state.
; Param: expression - the expression containing the variable which is being declared.
; Param: state - the state of the code before the variable declaration.
; Return: the state of the code after the variable declaration. The new state should include the variable
; and its value if it was an assignment on the same line. Otherwise if the variable was already declared,
; this returns an error.
(define declare
  (lambda (expression state continuations)
    (cond
      [(and (not (null? (cdr (cdr expression)))) (not (findfirst* (leftoperand expression) state)))
       (declareandassign (cdr expression) state continuations)]
      [(not (findfirst* (leftoperand expression) state)) (cons (cons (cons (leftoperand expression) '()) (car state)) '())]
      [else ((getThrowCont continuations) "This variable has already been declared")])))

; declareandassign. Takes an expression and a state and declares a variable and assigns a value to
; it based on the expression.
; Param: expression - an expression containing both the variable name and the value or expression which
; becomes a value to assign to the variable within the state
; Param: state - the state of the code before the declaration and assigning of the variable.
; Return the state of the code after the declaration and assignment. 
(define declareandassign
  (lambda (expression state continuations)
    (cond
      [(and (not (findfirst* (operator expression) state)) (isValueOp (leftoperand expression)))
       (append (cons (cons (cons (car expression) (cons (Mvalue (leftoperand expression) state continuations) '())) (car state)) '()) (cdr state))]
      [(and (not (findfirst* (operator expression) state)) (isBoolOp (leftoperand expression)))
       (append (cons (cons (cons (car expression) (cons (Mboolean (leftoperand expression) state) '())) (car state)) '()) (cdr state))]
      [(findfirst* (leftoperand expression) state) (declareandassign (cons (operator expression) (cons (Mvalue (leftoperand expression) state continuations) '())) state continuations)]
      [else ((getThrowCont continuations) "The parser should have caught this")])))

; assign. Takes an assignment expression and a state and assigns a value to a variable within the state.
; Param: assignment - the expression that contains the variable name to be assigned and the value to
; assign to it.
; Param: state - the state before the assignment has occured.
; Return: the state after the variable has been assigned a value based on the assignment expression
(define assign
  (lambda (assignment state continuations)
    (cond
      [(null? assignment) state]
      [(findfirst* (operator assignment) state) (addWithLayers (operator assignment)
                                                               (Mvalue (leftoperand assignment) state continuations)
                                                               state
                                                               continuations)]
      [else ((getThrowCont continuations) "The variable has not yet been declared")])))

; add. Adds a value to the state at a certain point corresponding to the variable name it is being added to.
; Param: var - the variable name of which to add the value to.
; Param; value - the value to be added to the variable name within the state.
; Param: state - the state before assignment of the variable to the value.
; Return: the state after the value has been added to the given variable.
(define add
  (lambda (var value state)
    (cond
      [(or (or (null? var) (null? value)) (null? state)) state]
      [(null? (car state)) state]
      [(eq? var (car (car state))) (cons (append (cons (car (car state)) '()) (cons value '()))
                                         (add var value (cdr state)))]
      [else (cons (car state) (add var value (cdr state)))])))

(define addWithLayers
  (lambda (var value state continuations)
    (cond
      [(null? state) '()]
      [(cons (add var value (car state)) (addWithLayers var value (cdr state) continuations))]
      [else ((getThrowCont continuations) "Why")])))
      
; findfirst*. Finds the first instance of an atom within a list which could be containing lists.
; Returns a boolean value as to whether it found the atom or not.
; Param: a - the atom which you are searching for from within the list.
; Param: lis - the list which you are searching in to see if the atom a is present.
; Return: a boolean indicating whether the atom a is within the list or not.
(define findfirst*
  (lambda (a lis)
    (cond
      [(null? (flatten lis)) #f]
      [(eq? a (car (flatten lis))) #t]
      [else (findfirst* a (cdr (flatten lis)))])))

; isValueOp. Returns a boolean indicating whether an expression is a value operation or a numeric expression.
; Param: expression - the expression of which you want to find if it is numeric or not.
; Return: a boolean indicatiing whether an expression is a numberic expression.
(define isValueOp
  (lambda (expression)
    (cond
      [(number? expression) #t]
      [(and (not (number? expression)) (not (list? expression))) #f]
      [(eq? '+ (operator expression)) #t]
      [(eq? '- (operator expression)) #t]
      [(eq? '* (operator expression)) #t]
      [(eq? '/ (operator expression)) #t]
      [(eq? '% (operator expression)) #t]
      [else #f])))

; isBoolOp. Returns a boolean indicating whether an expression is a boolean operation or not.
; Param: expression - the expression of which you want to find if it is a boolean operation.
; Return: a boolean indicating whether the expression is a boolean operation.
(define isBoolOp
  (lambda (expression)
    (cond
      [(number? expression) #f]
      [(eq? 'true expression) #t]
      [(eq? 'false expression) #t]
      [(not (list? expression)) #f]
      [(eq? '== (operator expression)) #t]
      [(eq? '&& (operator expression)) #t]
      [(eq? '|| (operator expression)) #t]
      [(eq? '! (operator expression)) #t]
      [(eq? '< (operator expression)) #t]
      [(eq? '> (operator expression)) #t]
      [(eq? '<= (operator expression)) #t]
      [(eq? '>= (operator expression)) #t]
      [(eq? '!= (operator expression)) #t]
      [else #f])))

; doesNotHaveElseStatement. Checks to see whether an if expression has an else statement.
; Param: expression - the if expression to check for an else statement.
(define doesNotHaveElseStatement
  (lambda (expression)
    (null? (cdr (cdr (cdr expression))))))

; ifElseStatement. Handles if-else statements within the code. Checks the condition and decides
; which path to take.
; Param: condition - the condition statement which is checked at the start of the if statement.
; Param statement1 - the statement that is executed if the condition is true.
; Param statemnet2 - the else statement that is executed if the condition is false.
; Param state -  the state of the code before the if-else statement.
; Return: the state of the code after the if-else statement is executed.
(define ifElseStatement
  (lambda (condition statement1 statement2 state continuations)
    (cond
      [(Mboolean condition state continuations) (Mstate statement1 state continuations)]
      [else (Mstate statement2 state continuations)])))

; ifStatement. Handles if statements within the code without an else statement. Checks
; the condition and evaluates the state after the statement if the condition is true.
; Param: condition - the condition to check at the start of the if statement to determine
; if the statment1 will be executed.
; Param: statement1 - the expression that its state is evaluated if the condition is deemed
; to be true.
; Param state - the state of the code before the if statement.
; Return: the state of the code after the if statement is executed.
(define ifStatement
  (lambda (condition statement state continuations)
    (cond
      [(Mboolean condition state continuations) (Mstate statement state continuations)]
      [else state])))

; whileStatement. Handles while loops within the code. Checks a condition and decides whether to execute the
; statement based on the value of that condition.
; Param: condition = the condition that is checked at the start of each iteration of the while loop.
; Determines if the loop body will be executed.
; Param: statement - the body of the loop that will only run if the while condition is met
; Param: state - the state of the code before the while loop is executed.
; Return: the state of the code after the while loop is executed.
(define whileStatement
  (lambda (condition statement state continuations)
    (cond
     [(Mboolean condition state continuations) (whileStatement
                                                condition
                                                statement
                                                (call/cc (lambda (continue-cont) (Mstate statement state (setContinueCont continuations continue-cont))))
                                                continuations)]
     [else state])))

; tryStatement
(define tryStatement
  (lambda (expression-parts state continuations)
    (call/cc
     (lambda (finally-cont)
       (Mstate
        (car expression-parts)
        state
        (continuations))))))

(define create-finally
  (lambda (finally-expression state continuations)
    (if (null? finally-expression)
        (lambda (e) e)
        (lambda (e) (Mstate (cons 'begin (cadr finally-expression)) state continuations)))))

(define buildThrowBody
  (lambda (parsed-throw-body e)
    (cons (append (cons 'var (cadr parsed-throw-body)) (cons e '())) (caddr parsed-throw-body))))

(define generateDefaultThrowCont
  (lambda ()
    (lambda (e)
      (error 'uncaught-exception "oof"))))

;remove all but global layer
(define popLayer
  (lambda (state)
    (cond
      [(null? state) '()]
      [(number? state) state]
      [(null? (cdr state)) state]
      [else (cdr state)])))