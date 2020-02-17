#lang racket

; (load "lex.scm")
(require "simpleParser.rkt")

(define leftoperand cadr)
(define operator car)
(define rightoperand caddr)
(define else
  (lambda (expression)
    (car (cdr (cdr (cdr expression))))))
    

;not important
;      [(eq? (car (car (parser filename))) 'var) (declare (car (parser filename)) '((return)))]
     ; [(eq? (car (car (parser filename))) 'return) (add


;input (parser filename)
;Main
(define main
  (lambda (filename)
      (Mstate (parser filename) '((return)))))

;Mstate
(define Mstate
  (lambda (expression state)
    (cond
      [(null? expression) state]
      [(list? (car expression)) (Mstate (cdr expression) (Mstate (car expression) state))]
      [else
       (cond
         [(isValueOp expression) Mvalue(expression state)]
         [(eq? (operator expression) 'var) (declare expression state)]
         [(eq? (operator expression) '=) (assign (cdr expression) state)]
         [(and (eq? (operator expression) 'if) (null? (cdr (cdr (cdr expression))))) (ifStatement (car (cdr expression)) (rightoperand expression) state)]
         [(eq? (operator expression) 'if) (ifelseStatement (car (cdr expression)) (rightoperand expression) (else expression) state)]
         [(eq? (operator expression) 'while) (whileStatement (car (cdr expression)) (rightoperand expression) state)]
         [(eq? (operator expression) 'return)
          (cond
            ((null? (cdr (cdr expression))) (Mvalue (car (cdr expression)) state))
            (else (Mvalue (cdr expression) state)))]
         [else state])])))

;help for sublists
(define hasSublist?
  (lambda (lis)
    (cond
      [(null? lis) #f]
      [(list? (car lis)) #t]
      [else (hasSublist? (cdr lis))])))


; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
; This is an example of using abstraction to make the code easier to read and maintain
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((and (not (number? expression)) (not (list? expression))) (lookup expression state))
      [(and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (* -1 (Mvalue(leftoperand expression) state))]
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'badop "The operator is not known")))))

(define isValueOp
  (lambda (expression)
    (cond
      [(eq? '+ (operator expression)) #t]
      [(eq? '- (operator expression)) #t]
      [(eq? '* (operator expression)) #t]
      [(eq? '/ (operator expression)) #t]
      [(eq? '% (operator expression)) #t]
      [else #f])))

(define isBoolOp
  (lambda (expression)
    (cond
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

; M_boolean
; takes a boolean expression and returns the boolean value of that expression
(define Mboolean
  (lambda (expression state)
    (cond
      [(null? expression) '()]
      [(or (not (number? (leftoperand expression))) (not (number? (rightoperand expression)))) (Mboolean (cons (operator expression) (cons (lookup (leftoperand expression) state) (cons (lookup (rightoperand expression) state) '()))) state)]
      [(eq? (operator expression) '==) (= (leftoperand expression) (rightoperand expression))]
      [(eq? (operator expression) '<) (< (leftoperand expression) (rightoperand expression))]
      [(eq? (operator expression) '>) (> (leftoperand expression) (rightoperand expression))]
      [(eq? (operator expression) '!=) (not (= (leftoperand expression) (rightoperand expression)))]
      [(eq? (operator expression) '<=) (<= (leftoperand expression) (rightoperand expression))]
      [(eq? (operator expression) '>=) (>= (leftoperand expression) (rightoperand expression))]
      [(eq? (operator expression) '&&) (and (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state))]
      [(eq? (operator expression) '||) (or (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state))]
      [(eq? (operator expression) '!) (not (Mboolean (leftoperand expression) state))]
      [else (error 'badop "the operator is not known")])))

;lookup
(define lookup
  (lambda (a state)
    (cond
      [(number? a) a]
      [(null? state) (error 'notdeclared "the variable that you are trying to use is not yet declared or assigned")]
      [(and (eq? a (car (car state))) (not (null? (cdr (car state))))) (car (cdr (car state)))]
      [else (lookup a (cdr state))])))

;> (declare '(var x) '()) --> '((x))
;declare
(define declare
  (lambda (expression state)
    (cond
      [(and (not (null? (cdr (cdr expression)))) (not (findfirst* (leftoperand expression) state))) (declareandassign (cdr expression) state)]
      [(not (findfirst* (leftoperand expression) state)) (cons (cons (leftoperand expression) '()) state)]
      [else (error 'alreadydelcared "this variable has alreaday been declared")])))

;declare and assign
(define declareandassign
  (lambda (expression state)
    (cond
      [(not (findfirst* (operator expression) state)) (cons (cons (car expression) (cons (Mvalue (leftoperand expression) state) '())) state)]
      [#t (display expression)])))

;> (assign '(x 4) '((x))) --> '((x 4))
;assign
(define assign
  (lambda (assignment state)
    (cond
      [(null? assignment) state]
      [(findfirst* (operator assignment) state) (add (operator assignment) (Mvalue (leftoperand assignment) state) state)]
      [else (error 'notdeclared "The variable has not yet been declared")])))

;add
(define add
  (lambda (var value state)
    (cond
      [(or (or (null? var) (null? value)) (null? state)) state]
      [(eq? var (car (car state))) (cons (append (cons (car (car state)) '()) (cons value '())) (add var value (cdr state)))]
      [else (cons (car state) (add var value (cdr state)))])))
      
  
       
;findfirst* 
(define findfirst*
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? a (car (flatten lis))) #t]
      [else (findfirst* a (cdr (flatten lis)))])))

;ifelseStatement
(define ifelseStatement
  (lambda (condition statement1 statement2 state)
    (cond
      [(Mboolean condition state) (Mstate statement1 state)]
      [else (Mstate statement2 state)])))

;ifStatement
(define ifStatement
  (lambda (condition statement1 state)
    (cond
      [(Mboolean condition state) (Mstate statement1 state)]
      [else state])))

(define whileStatement
  (lambda (condition statement1 state)
    (cond
     [(Mboolean condition state) (Mstate (whileStatement condition statement1 (Mstate statement1 state)) state)]
     [else state])))

;(Mvalue_default
 ;(lambda (expression state)
  ; (cond
   ;  [(= 3 (length expression))
    ;  ((get_function_for_2_inputs (operator expression))
     ;                            (Mvalue (leftoperand expression) state)
      ;                           (Mvalue (rightoperand expression) state))]
     ;[(= 2 (length expression))
      ;((get_function_for_2_inputs (operator expression))
       ;                          (Mvalue (leftoperand expression) state))]

; Pass in the operator atom, and get back the actual function that should be performed
; The operator must take 2 inputs, otherwise you will get an error
;(define get_function_for_2_inputs
 ; (lambda (operator)
  ;  (cond
   ;   ; Math bois
    ;  ((eq? operator '+) +)
     ; ((eq? operator '-) -)
      ;((eq? operator '/) quotient)
;      ((eq? operator '*) *)
 ;     ((eq? operator '%) remainder)
  ;    ; Logic bois
   ;   ((eq? operator '<) <)
    ;  ((eq? operator '>) >)
;     ; ((eq? operator '<=) <=)
 ;     ((eq? operator '>=) >=)
  ;    ((eq? operator '==) ==)
   ;   ((eq? operator '!=) !=)
    ;  ((eq? operator '&&) (lambda (x y) (and x y)))
 ;     ((eq? operator '||) (lambda (x y) (or x y)))
  ;    ; Might being doing a goof like (+ x) or something
   ;   (else (error "Unknown operator for 2 inputs")))))

(define get_function_for_1_input
  (lambda (operator)
    (cond
      ((eq? operator '!) not)
      ; This is ok because this is how negative numbers are parsed
      ((eq? operator '-) (lambda (x) (- 0 x)))
      (else (error "Unknown operator for 1 input")))))

; We are supposed to return true / false rather than #t and #f
; Pass-through if not a bool
(define get_return_val
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

;(define !=
 ; (lambda (x y)
 ; ;  (not (== x y))))

;return sublist
(define firstSublist
  (lambda (lis)
    (cond 
      ((null? lis) '())
      ((list? (car lis)) (cons (car lis) (cdr lis)))
      (else (firstSublist (cdr lis))))))