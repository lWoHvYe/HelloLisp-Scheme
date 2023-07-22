 ;; Supporting functions: 
  
 (define nil '()) 
  
 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 
  
 (define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) #t) 
           ((= 0 (remainder x divisor)) #f) 
           (else (test (+ divisor 1))))) 
   (test 2)) 
  
 (define (prime-sum? pair) 
   (prime? (+ (car pair) (cadr pair)))) 
  
 (define (make-sum-pair pair) 
   (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))) 
  
 ;; ------------ 
  
 ;; The answer ... it's just the top of page 123, pulled into a new 
 ;; function (with flatmap): 
 (define (unique-pairs n) 
   (flatmap (lambda (i)  
              (map (lambda (j) (list i j)) 
                   (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n))) 
  
 ;; Test: 
 (display (unique-pairs 5))(newline)
  
  
 (define (prime-sum-pairs n) 
   (map make-sum-pair 
        (filter prime-sum? (unique-pairs n)))) 
  
 ;; Test: 
 (display (prime-sum-pairs 6))(newline) 
 ;; => ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11)) 

 ;; We need to make triples (i j k).  The following will do:  
 (define (unique-triples n) 
   (flatmap (lambda (i) 
              (flatmap (lambda (j) 
                         (map (lambda (k) (list i j k)) 
                              (enumerate-interval 1 (- j 1)))) 
                       (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n))) 
  