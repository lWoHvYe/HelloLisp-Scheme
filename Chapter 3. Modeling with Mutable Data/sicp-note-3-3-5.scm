 (define (averager a b c) 
   (let ((u (make-connector)) 
         (v (make-connector))) 
     (adder a b u) 
     (multiplier c v u) 
     (constant 2 v) 
     'ok)) 

 ;; Squarer Constraint 
  
 (define (squarer a b) 
   (define (process-new-value) 
     (if (has-value? b) 
         (if (< (get-value b) 0) 
             (error "square less than 0 -- SQUARER" (get-value b)) 
             (set-value! a 
                         (sqrt (get-value b)) 
                         me)) 
         (if (has-value? a) 
             (set-value! b 
                         (square (get-value a)) 
                         me)))) 
   (define (process-forget-value) 
     (forget-value! a me) 
     (forget-value! b me) 
     (process-new-value)) 
   (define (me request) 
     (cond ((eq? request 'I-have-a-value) 
            (process-new-value)) 
           ((eq? request 'I-lost-my-value) 
            (process-forget-value)) 
           (else 
            (error "Unknown request -- SQUARER" request)))) 
   (connect a me) 
   (connect b me) 
   me) 

;; ex-3.37                                                                                                                                                                                                          
  
 (define (c+ x y) 
   (let ((z (make-connector))) 
     (adder x y z) 
     z)) 
  
 (define (c- x y) 
   (let ((z (make-connector))) 
     (adder z y x) 
     z)) 
  
 (define (c* x y) 
   (let ((z (make-connector))) 
     (multiplier x y z) 
     z)) 
  
 (define (c/ x y) 
   (let ((z (make-connector))) 
     (multiplier z y x) 
     z)) 
  
 (define (cv x) 
   (let ((z (make-connector))) 
     (constant x z) 
     z)) 
  
 (define (celsius-fahrenheit-converter x) 
   (c+ (c* (c/ (cv 9) (cv 5)) 
           x) 
       (cv 32))) 