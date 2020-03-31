;;;; *********************************************************************************************************
;;;; * Group 6
;;;; * EECS 345 Spring 2020
;;;; * Interpreter, Part 2
;;;; *********************************************************************************************************

#lang racket

; _______________ -= Initializing Functions =- _______________________________________________________________________________________________________

; Load the parser
(require "simpleParser.rkt")

; The interpret main function. It calls the parser and uses that output to calculate the end state of the
; input file
; Param: filename - the name of the text file containing the code to be parsed
; Return: The return value of the function from the state that it calculated
(define interpret
  (lambda (filename)
    (get_return_val
     (call/cc
      (lambda (return-cont)
        (call/cc
           (Mstate
            (parser filename)
            (initState)
            ; Use default configuration but set return to jump here
            (setReturnCont (generateContinuations) return-cont))))))))

; used so that the function signature for Mstate and all helpers just takes in a single continuations parameter
(define generateContinuations
  (lambda ()
    ; Return-cont
    (cons null
          ; Break-cont
          (cons null
                ; Continue-cont
                (cons null
                      ; Throw-cont
                      (cons null '()))))))

; Returns the initial state
(define initState
  (lambda ()
    '(())))

; _______________ -= Mstate Function =- ______________________________________________________________________________________________________________

; Mstate. Obtains the state of an expression given a state. The original state is set to only contain return
; without a value
; Param: expression - the expression of which to generate the new state from
; Param: state - the old state used to obtain the new state with the given expression
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: The state of the code after the expression
(define Mstate
  (lambda (expression state continuations)
    (cond
      ; No return statemnet found
      [(and (null? expression) (null? (unsafe_popLayer state))) ((getReturnCont continuations)
                                                           (string-append "Error: No return statement. Final program state: "
                                                                          (string-join (map ~a state) " ")))]
      [(null? expression) state]
      [(number? state) state]
      [(list? (firststatement expression))
       (Mstate (secondstatement expression) (Mstate (firststatement expression) state continuations) continuations)]
      [else
       (cond
         [(isValueOp expression) (Mvalue expression state continuations)]
         [(isBoolOp expression) (Mboolean expression state continuations)]
         [(eq? (operator expression) 'return)
          (cond
            [(isBoolOp (leftoperand expression)) ((getReturnCont continuations)
                                                  (Mboolean (leftoperand expression) state continuations))]
            [(null? (rightoperand-listform expression)) ((getReturnCont continuations)
                                             (Mvalue (leftoperand expression) state continuations))]
            [else ((getReturnCont continuations) (Mvalue (leftoperand expression) state continuations))])]
         [(and (eq? (operator expression) 'break) (eq? (getBreakCont continuations) null))
          ((getReturnCont continuations) "Error: Break in an invalid location.")]
         [(eq? (operator expression) 'break) ((getBreakCont continuations) state)]
         [(and (eq? (operator expression) 'continue) (eq? (getContinueCont continuations) null))
          ((getReturnCont continuations) "Error: Continue in an invalid location.")]
         [(eq? (operator expression) 'continue) ((getContinueCont continuations) state)]
         [(eq? (operator expression) 'var) (declare expression state continuations)]
         [(eq? (operator expression) '=) (assign (secondstatement expression) state continuations)]
         [(eq? (operator expression) 'while) (call/cc (lambda (break-cont) (whileStatement
                                                                            (leftoperand expression)
                                                                            (rightoperand expression)
                                                                            state
                                                                            (setBreakCont continuations break-cont))))]
         [(eq? (operator expression) 'begin) (with-handlers ([exn:fail? (lambda (exn)
                                                                          ((getReturnCont continuations)
                                                                           "Error: Continue/Break/Throw in an invalid location."))])
                                               (popLayer
                                                (Mstate (secondstatement expression)
                                                        (addLayer state)
                                                        (setBreakCont (setContinueCont
                                                                       (setThrowCont continuations
                                                                                     (lambda (state3)
                                                                                       ((getThrowCont continuations)
                                                                                        (popLayer state3))))
                                                                       (lambda (state3)
                                                                         ((getContinueCont
                                                                           (setThrowCont continuations
                                                                                         (lambda (state3)
                                                                                           ((getThrowCont continuations)
                                                                                            (popLayer state3)))))
                                                                          (popLayer state3))))
                                                                      (lambda (state2)
                                                                        ((getBreakCont
                                                                          (setContinueCont
                                                                           (setThrowCont continuations
                                                                                         (lambda (state3)
                                                                                           ((getThrowCont continuations)
                                                                                            (popLayer state3))))
                                                                           (lambda (state3)
                                                                             ((getContinueCont
                                                                               (setThrowCont continuations
                                                                                             (lambda (state3)
                                                                                               ((getThrowCont continuations)
                                                                                                (popLayer state3)))))
                                                                              (popLayer state3)))))
                                                                         (popLayer state2)))))))]
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

         [(and (eq? (operator expression) 'throw) (null? (getThrowCont continuations)))
          ((getReturnCont continuations) "Error: Throw in an invalid location.")]
         [(eq? (operator expression) 'try) (tryStatement (secondstatement expression) state continuations)]
         [(eq? (operator expression) 'throw) ((getThrowCont continuations)
                                              (cons state (cons (Mvalue (leftoperand expression) state continuations) '(%throw%))))]
         [else state])])))

; _______________ -= Try-Catch_Finally Functions =- __________________________________________________________________________________________________

; tryStatement. Handles try-catch-finally blocks. Takes try, catch, and finally expressions
; and returns the state after their execution.
; Param: expression - the try-catch-finally expression of which to generate the new state from
; Param: state - the old state used to obtain the new state with the given expression
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: The state of the code after the try-catch-finally expression
(define tryStatement
  (lambda (expression state continuations)
    (cond
      [(findfirst* '%throw% (call/cc (lambda (v) (Mstate (cons 'begin (try-expression expression)) state (setThrowCont continuations v)))))
       (finallyStatement (finally-expression expression)
                         (catchStatement (catch-expression expression)
                                         (firststatement (call/cc (lambda (v) (Mstate
                                                                               (cons 'begin (try-expression expression))
                                                                               state
                                                                               (setThrowCont continuations v)))))
                                         continuations
                                         (secondstatement (call/cc (lambda (v2) (Mstate
                                                                                 (cons 'begin (try-expression expression))
                                                                                 state
                                                                                 (setThrowCont continuations v2))))))
                         continuations)]
      [else (finallyStatement (finally-expression expression)
                              (call/cc (lambda (v)
                                         (Mstate (cons 'begin (try-expression expression))
                                                 state
                                                 (setThrowCont continuations v))))
                              continuations)])))

; catchStatement. Handles catch blocks. Takes a catch expresion and
; returns the state after its execution
; Param: catch-expression - the catch expression of which to generate a new state from
; Param: state - the old state used to obtain the new state with the given expression
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Param throw-value - the value of the item throw. Will be '() if nothing was thrown
; Return: The state of the code after the catch expression
(define catchStatement
  (lambda (catch-expression state continuations throw-value)
    (cond
      ; No catch statement
      [(null? catch-expression) state]
      ; No throw found
      [(null? throw-value) (Mstate (cons 'begin (rightoperand catch-expression))
                                   (Mstate (cons 'var (cons (catchvar catch-expression) '())) state continuations) continuations)]
      [(Mstate (cons 'begin (rightoperand catch-expression))
               (Mstate (cons 'var (cons (catchvar catch-expression) (cons (throw-v throw-value) '()))) state continuations) continuations)])))

; finallyStatement. Handles finally blocks. Takes a finally expresion and
; returns the state after its execution
; Param: finally-expression - the finally expression of which to generate a new state from
; Param: state - the old state used to obtain the new state with the given expression
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: The state of the code after the catch expression
(define finallyStatement
  (lambda (finally-expression state continuations)
    (cond
      [(null? finally-expression) state]
      [(Mstate (cons 'begin (leftoperand finally-expression)) state continuations)])))
               
; _______________ -= Continuation-Helper Functions =- ________________________________________________________________________________________________

; Replaces an item in a list with a desired item
; Param: lis - the original list in which to replace an item in
; Param: index - the index in the original list of the item you want to replace
; Param: a - the item to be put in place of the removed item of the original list
; Return: The new list with the desired item replaced
(define replace-atom
  (lambda (lis index a)
    (cond
      [(null? lis) '()]
      [(zero? index) (cons a (secondstatement lis))]
      [else (cons (firststatement lis) (replace-atom (secondstatement lis) (- index 1) a))])))

; Sets the return continuation
; Param: continuations - the original continuation list to update the return continuation of
; Param: cont - the new return continuation to insert
; Return: the continuation list with the new return continuation
(define setReturnCont
  (lambda (continuations cont)
    (replace-atom continuations 0 cont)))

; Sets the break continuation
; Param: continuations - the original continuation list to update the break continuation of
; Param: cont - the new break continuation to insert
; Return: the continuation list with the new break continuation
(define setBreakCont
  (lambda (continuations cont)
    (replace-atom continuations 1 cont)))

; Sets the continue continuation
; Param: continuations - the original continuation list to update the continue continuation of
; Param: cont - the new continue continuation to insert
; Return: the continuation list with the new continue continuation
(define setContinueCont
  (lambda (continuations cont)
    (replace-atom continuations 2 cont)))

; Sets the throw continuation
; Param: continuations - the original continuation list to update the throw continuation of
; Param: cont - the new throw continuation to insert
; Return: the continuation list with the new throw continuation
(define setThrowCont
  (lambda (continuations cont)
    (replace-atom continuations 3 cont)))

; _______________ -= Value/Boolean Functions =- ______________________________________________________________________________________________________

; M_value. Obtains the value of an numerical expression. Can do the following operators: +,-,*,/,%,(negation),
; as well as return the values of declared and assigned variables
; Param: expression- a numeric expression that you wish to obtain the value of
; Param: state - the state of the code before the expression.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the value of the expression
(define Mvalue
  (lambda (expression state continuations)
    (cond
      [(null? expression) ((getReturnCont continuations) "Error: Parser should have caught this.")]
      [(number? expression) expression]
      [(and (not (number? expression)) (not (list? expression))) (getLookupValue expression state continuations)]
      [(and (eq? '- (operator expression)) (null? (rightoperand-listform expression)))
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
      [else ((getReturnCont continuations) "Error: The operator is not known.")])))

; Mboolean. Takes a boolean expression and returns the boolean value of that expression.
; Param: expression - The boolean expression of which you want to obtain the boolean value of
; Param: state - the state of the code before the boolean expression is evaluated.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the boolean value of the expression
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
      [else ((getReturnCont continuations) "Error: The operator is not known.")])))

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

; _______________ -= State-Lookup Functions =- _______________________________________________________________________________________________________

; lookup. Looks for a variable within a state and returns its value if it is found.
; Param: a - the atom representing the variable that you wish to look up the value of.
; Param: state - the state for which you will be looking through to obtain the value of the variable a.
; Return: the value of the variable.
(define lookup
  (lambda (a state)
    (cond
      [(number? a) a]
      [(null? state) '()]
      [(and (eq? a (var_name state)) (not (null? (var_value-list state)))) (var_value state)]
      [else (lookup a (restof_state state))])))

; LayerLookup. Looks for a variable within a layered state and returns its value if it is found.
; Param: a - the atom representing the variable that you wish to look up the value of.
; Param: state - the layered state for which you will be looking through to obtain the value of the variable a.
; Return: the value of the variable.
(define LayerLookup
  (lambda (a state)
    (cond
      [(null? state) '()]
      [(number? state) state]
      [else (cons (lookup a (firstof_state state)) (LayerLookup a (restof_state state)))])))

; getLookupValue. Looks for a variable within a layered state and returns its value if it is found.
; Param: a - the atom representing the variable that you wish to look up the value of.
; Param: state - the layered state for which you will be looking through to obtain the value of the variable a.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the value of the variable or an error if the variable is not within the layered state.
(define getLookupValue
  (lambda (a state continuations)
    (cond
      [(null? (flatten (LayerLookup a state))) ((getReturnCont continuations)
                                                "Error: The variable for lookup not assigned or declared in any layer.")]
      [else (firstof_state (flatten (LayerLookup a state)))])))

; _______________ -= Declare/Assign Functions =- _____________________________________________________________________________________________________

; declare. Takes an expression and a state and declares a new varible within the state.
; Param: expression - the expression containing the variable which is being declared.
; Param: state - the state of the code before the variable declaration.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the state of the code after the variable declaration. The new state should include the variable
; and its value if it was an assignment on the same line. Otherwise if the variable was already declared,
; this returns an error.
(define declare
  (lambda (expression state continuations)
    (cond
      [(and (not (null? (rightoperand-listform expression))) (not (findfirst* (leftoperand expression) state)))
       (declareandassign (secondstatement expression) state continuations)]
      [(not (findfirst* (leftoperand expression) state))
       (append (cons (cons (cons (leftoperand expression) '()) (firstof_state state)) '()) (restof_state state))]
      [else ((getReturnCont continuations) "Error: This variable has already been declared.")])))

; declareandassign. Takes an expression and a state and declares a variable and assigns a value to
; it based on the expression.
; Param: expression - an expression containing both the variable name and the value or expression which
; becomes a value to assign to the variable within the state
; Param: state - the state of the code before the declaration and assigning of the variable.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return the state of the code after the declaration and assignment. 
(define declareandassign
  (lambda (expression state continuations)
    (cond
      [(and (not (findfirst* (operator expression) state)) (isValueOp (leftoperand expression)))
       (append (cons (cons (cons (operator expression) (cons (Mvalue (leftoperand expression) state continuations) '()))
                           (firstof_state state)) '()) (restof_state state))]
      [(and (not (findfirst* (operator expression) state)) (isBoolOp (leftoperand expression)))
       (append (cons (cons (cons (operator expression) (cons (Mboolean (leftoperand expression) state continuations) '()))
                           (firstof_state state)) '()) (restof_state state))]
      [(findfirst* (leftoperand expression) state)
       (declareandassign (cons (operator expression) (cons (Mvalue (leftoperand expression) state continuations) '())) state continuations)]
      [else ((getReturnCont continuations) "Error: The parser should have caught this.")])))

; assign. Takes an assignment expression and a state and assigns a value to a variable within the state.
; Param: assignment - the expression that contains the variable name to be assigned and the value to
; assign to it.
; Param: state - the state before the assignment has occured.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the state after the variable has been assigned a value based on the assignment expression
(define assign
  (lambda (assignment state continuations)
    (cond
      [(null? assignment) state]
      [(findfirst* (operator assignment) state) (addWithLayers (operator assignment)
                                                               (Mvalue (leftoperand assignment) state continuations)
                                                               state
                                                               continuations)]
      [else ((getReturnCont continuations) "Error: The variable has not yet been declared.")])))

; add. Adds a value to the state at a certain point corresponding to the variable name it is being added to.
; Param: var - the variable name of which to add the value to.
; Param; value - the value to be added to the variable name within the state.
; Param: state - the state before assignment of the variable to the value.
; Return: the state after the value has been added to the given variable.
(define add
  (lambda (var value state)
    (cond
      [(or (or (null? var) (null? value)) (null? state)) state]
      [(null? (firstof_state state)) state]
      [(eq? var (var_name state)) (cons (append (cons (var_name state) '()) (cons value '()))
                                         (add var value (restof_state state)))]
      [else (cons (firstof_state state) (add var value (restof_state state)))])))

; addWithLayers. Adds a value to the layered state at a certain point corresponding to the variable name it is being added to.
; Param: var - the variable name of which to add the value to.
; Param; value - the value to be added to the variable name within the state.
; Param: state - the layered state before assignment of the variable to the value.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the state after the value has been added to the given variable.
(define addWithLayers
  (lambda (var value state continuations)
    (cond
      [(null? state) '()]
      [(cons (add var value (firstof_state state)) (addWithLayers var value (restof_state state) continuations))]
      [else ((getReturnCont continuations) "Error: The parser should have caught this.")])))
      
; findfirst*. Finds the first instance of an atom within a list which could be containing lists.
; Returns a boolean value as to whether it found the atom or not.
; Param: a - the atom which you are searching for from within the list.
; Param: lis - the list which you are searching in to see if the atom a is present.
; Return: a boolean indicating whether the atom a is within the list or not.
(define findfirst*
  (lambda (a lis)
    (cond
      [(null? (flatten lis)) #f]
      [(eq? a (firststatement (flatten lis))) #t]
      [else (findfirst* a (secondstatement (flatten lis)))])))

; _______________ -= If-Statement Functions =- _______________________________________________________________________________________________________

; ifElseStatement. Handles if-else statements within the code. Checks the condition and decides
; which path to take.
; Param: condition - the condition statement which is checked at the start of the if statement.
; Param statement1 - the statement that is executed if the condition is true.
; Param statemnet2 - the else statement that is executed if the condition is false.
; Param state -  the state of the code before the if-else statement.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
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
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the state of the code after the if statement is executed.
(define ifStatement
  (lambda (condition statement state continuations)
    (cond
      [(Mboolean condition state continuations) (Mstate statement state continuations)]
      [else state])))

; doesNotHaveElseStatement. Checks to see whether an if expression has an else statement.
; Param: expression - the if expression to check for an else statement.
; Return: true of false depending on if the expression has an else statement
(define doesNotHaveElseStatement
  (lambda (expression)
    (null? (cdr (cdr (cdr expression))))))

; _______________ -= While-Statement Functions =- ____________________________________________________________________________________________________

; whileStatement. Handles while loops within the code. Checks a condition and decides whether to execute the
; statement based on the value of that condition.
; Param: condition = the condition that is checked at the start of each iteration of the while loop.
; Determines if the loop body will be executed.
; Param: statement - the body of the loop that will only run if the while condition is met
; Param: state - the state of the code before the while loop is executed.
; Param: continuations - the continuations for break, continue, throw, and return in a list form
; Return: the state of the code after the while loop is executed.
(define whileStatement
  (lambda (condition statement state continuations)
    (cond
     [(Mboolean condition state continuations) (whileStatement
                                                condition
                                                statement
                                                (call/cc (lambda (continue-cont) (Mstate statement
                                                                                         state
                                                                                         (setContinueCont continuations continue-cont))))
                                                continuations)]
     [else state])))

; _______________ -= Popping/Adding Layer Functions =- _______________________________________________________________________________________________

; popLayer. Removes the newest top layer of the state. Will never remove the lowest layer for code safety
; Param: state - the layered state to which you are popping a layer off of
; Return: the state after the layer is popped off
(define popLayer
  (lambda (state)
    (cond
      [(null? state) '()]
      [(number? state) state]
      [(findfirst* '%throw% state) (cons (popLayer (firstof_state state)) (restof_state state))]
      [(null? (restof_state state)) state]
      [else (restof_state state)])))

; addLayer. Adds a layer to the top of the layered state
; Param: state - the layered state to which you are adding a layer to
; Return: the state after the layer is added
(define addLayer
  (lambda (state)
    (append (initState) state)))

; unsafe_popLayer. Removes the newest top layer of the state. Will remove the lowest layer if only one layer
; Param: state - the layered state to which you are popping a layer off of
; Return: the state after the layer is popped off
(define unsafe_popLayer
  (lambda (state)
    (cond
      [(null? state) '()]
      [(number? state) state]
      [(null? (restof_state state)) '()]
      [else (restof_state state)])))

; _______________ -= Absstraction Helpers =- _________________________________________________________________________________________________________


; the variable values in a list within the state for the lookup functions
(define var_value-list (lambda (state) (cdr (car state))))

; the variable value in a list within the state for the lookup functions
(define var_value (lambda (state) (car (cdr (car state)))))

; the variable name in a list within the state for the lookup functions
(define var_name (lambda (state) (car (car state))))

; the rest of the state disregarding the first item
(define restof_state cdr)

; the first item of the state disregarding the rest
(define firstof_state car)

; Gets the return continuation from the continuations list
(define getReturnCont car)

; Gets the break continuation from the continuations list
(define getBreakCont cadr)

; Gets the continue continuation from the continuations list
(define getContinueCont caddr)

; Gets the throw continuation from the continuations list
(define getThrowCont cadddr)

; The caught variable name within the catch statement
(define catchvar caadr)

; The throw value after being caught
(define throw-v car)

; The statements produced by the parser for a finally block
(define finally-expression caddr)

; The statements produced by the parser for a catch block
(define catch-expression cadr)

; The statements produced by the parser for a try block
(define try-expression car)

; Left operand of an expression (0 X 0)
(define leftoperand cadr)

; Operator of an expression (X 0 0)
(define operator car)

; Right operand of an expression (0 0 X)
(define rightoperand caddr)

; Used to check if right operand exists (0 0 X) --> '(X)
(define rightoperand-listform cddr)

; The else statement within an if statement (0 0 0 X)
(define thirdOperand
  (lambda (expression)
    (car (cdr (cdr (cdr expression))))))

; First statment to evaluate
(define firststatement car)

; Second statemnet to evaluate
(define secondstatement cdr)
