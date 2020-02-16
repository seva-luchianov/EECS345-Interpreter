#lang racket

; (load "lex.scm")
(require "simpleParser.rkt")

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
         [(eq? (operator expression) 'return) (assign (cons (operator expression) (cons (lookup (leftoperand expression) state) '())) state)]
         [else state])])))

;help for sublists
(define hasSublist?
  (lambda (lis)
    (cond
      [(null? lis) #f]
      [(list? (car lis)) #t]
      [else (hasSublist? (cdr lis))])))


; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
;
; This is an example of using abstraction to make the code easier to read and maintain
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((and (not (number? expression)) (not (list? expression))) (lookup expression state))
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
      [(not (findfirst* (leftoperand expression) state)) (cons (cons (leftoperand expression) '()) state)]
      [else (error 'alreadydelcared "this variable has alreaday been declared")])))

;> (assign '(x 4) '((x))) --> '((x 4))
;assign
(define assign
  (lambda (assignment state)
    (cond
      [(null? assignment) state]
      [(findfirst* (operator assignment) state) (add (operator assignment) (leftoperand assignment) state)]
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
      

(define leftoperand
  (lambda (expression)
    (cadr expression)))

(define operator car)
(define rightoperand caddr)