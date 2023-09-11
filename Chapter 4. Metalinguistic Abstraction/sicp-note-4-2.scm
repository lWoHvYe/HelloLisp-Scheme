(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

 (define prev-eval eval) 
  
 (define (eval expr env) 
     (if (quoted? expr) 
         (text-of-quotation expr env) 
         (prev-eval expr env))) 
  
 (define (quoted? expr) (tagged-list? expr 'quote)) 
  
 (define (text-of-quotation expr env)  
         (let ((text (cadr expr))) 
                 (if (pair? text) 
                         (evaln (make-list text) env) 
                         text))) 
 (define (make-list expr) 
         (if (null? expr) 
                 (list 'quote '()) 
                 (list 'cons 
                           (list 'quote (car expr)) 
                           (make-list (cdr expr))))) 