; 这一小节，自定义语法并实现了一个数据库
(define (execute exp)
    (apply (eval (predicate exp) user-initial-environment)
           (args exp)))