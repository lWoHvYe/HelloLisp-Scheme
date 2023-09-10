;    理解派生表达式  Derived Expressions

    (define (eval exp env) 
        (cond ((self-evaluating? exp) exp) 
              ((variable? exp) (lookup-variable-value exp env)) 
              ((quoted? exp) (text-of-quotation exp)) 
              ((assignment? exp) (eval-assignment exp env)) 
              ((definition? exp) (eval-definition exp env)) 
              ((if? exp) (eval-if exp env)) 
              ((lambda? exp) 
               (make-procedure (lambda-parameters exp) 
                               (lambda-body exp) 
                               env)) 
              ((begin? exp) 
               (eval-sequence (begin-actions exp) env)) 
              ((cond? exp) (eval (cond->if exp) env)) 
              ((application? exp) 
               (apply (eval (operator exp) env) 
                      (list-of-values (operands exp) env))) 
              (else 
               (error "Unknown expression type -- EVAL" exp)))) 

    (define (apply procedure arguments)
        (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
              ((compound-procedure? procedure) 
                (eval-sequence 
                    (procedure-body procedure)
                    (extend-environment (procedure-environment procedure) arguments (procedure-environment procedure))))
               (else (error "Unknown procedure type -- APPLY" procedure))))


; 丘奇-图灵等价性论题 : 该论题的核心思想是，丘奇提出的λ演算（Lambda Calculus）和图灵机（Turing Machine）这两种不同的计算模型具有等价性，
;    即它们能够互相模拟和表达相同的计算能力。
