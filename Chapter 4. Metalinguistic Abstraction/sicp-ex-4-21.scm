 ;; a，这个还是比较难理解的 
  (lambda (n)  
    ((lambda (fib) 
       (fib fib n)) ; 自递归的函数调用
     (lambda (f n) 
       (cond ((= n 0) 0) 
             ((= n 1) 1) 
             (else (+ (f f (- n 2)) (f f (- n 1)))))))) 
  

   (define (make-lambda-combination lambda-dec expressions) 
            (cons lambda-dec expressions))