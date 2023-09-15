; amb求值器，表达式有歧义性地返回n个表达式之一的值

(define (amb items)
  (define (try-next choice rest)
    (if (null? choice)
        '()
        (let ((next (car choice))
              (others (cdr choice)))
          (let ((result (amb rest)))
            (if (not (null? result))
                (cons next result)
                (try-next others rest))))))
  (try-next items '()))

 (define (amb-choices exp)
    (cdr exp))

 (define (ambeval exp env succeed fail)
    ((analyze exp) env succeed fail))

;   ramb，以random的方式检索
 (load "evaluator-amb.scm") 
  
 (define (shuffled s) 
     (define (swap s p q) 
         (let ((ps (list-starting-from s p)) 
               (qs (list-starting-from s q))) 
             (let ((pv (car ps))) 
                 (set-car! ps (car qs)) 
                 (set-car! qs pv))))  
     (define (iter rest) 
         (if (null? rest) 
             s 
             (let ((n (random (length rest)))) 
                 (swap rest 0 n) 
                 (iter (cdr rest))))) 
     (iter s)) 
              
  
 (define (analyze-amb exp) 
     (let ((cprocs (map analyze (amb-choices exp))))  ; can't shuffle here 
         (lambda (env succeed fail) 
             ; achieve random order by shuffling choices at RUNTIME 
             (define shuffled-cprocs (shuffled cprocs)) 
             (define (try-next choices) 
                 (if (null? choices) 
                     (fail) 
                     ((car choices) env 
                                    succeed 
                                    (lambda () 
                                     (try-next (cdr choices)))))) 
             (try-next shuffled-cprocs)))) 