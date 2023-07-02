 (define (square guess) 
   (* guess guess)) 
  
 ;not used 
 ;(define (average x guess) 
 ;  (/ (+ x guess) 2)) 
  
 ;improve square root 
 ;(define (improve guess x) 
 ;  (average guess (/ x guess))) 
  
 ;cube root improve formula used as is 
 (define (improve guess x) 
   (/ (+ (/ x (square guess)) (* 2 guess)) 3)) 
  
 ;original test 
 ;(define (good-enough? guess x) 
 ;  (< (abs (- (square guess) x)) 0.001)) 
  
 ;iterates until guess and next guess are equal, 
 ;automatically produces answer to limit of system precision 
 (define (good-enough? guess x) 
   (= (improve guess x) guess)) 
  
 (define (3rt-iter guess x) 
   (if (good-enough? guess x) 
       guess 
       (3rt-iter (improve guess x) x))) 
  
 ;<<<expression entry point>>> 
 ;change initial guess to 1.1 to prevent an anomalous result for 
 ;cube root of -2 
 (define (3root x) 
   (3rt-iter 1.1 x)) 

(display (3root 1)) (newline)
(display (3root -8)) (newline)
(display (3root 1e-30)) (newline)
(display (3root 1e60))