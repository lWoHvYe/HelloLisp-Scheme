 (define (tan-cf x k) 
   (cont-frac (lambda (i) 
                (if (= i 1) x (- (* x x)))) 
              (lambda (i) 
                (- (* i 2) 1)) 
              k)) 

 (define (cont-frac n d k) 
   (define (rec x) 
     (if (> x k) 
         0 
         (/ (n x) (+ (d x) (rec (+ x 1)))))) 
   (rec 1))

(display (tan 1)) (newline)

(display (tan-cf 1 10)) (newline)

 (define (tan-cf x k) 
   (let ((a (- (* x x)))) 
     (cont-frac (lambda (i) (if (= i 1) x a)) 
              (lambda (i) (- (* i 2) 1)) 
              k))) 

(display (tan-cf 1 10)) (newline)

 (define(tan-cf x k) 
   (let ((Xs (- (* x x)))) 
     (- (/ (cont-frac (lambda (i) Xs) 
                      (lambda (i) (- (* i 2) 1)) 
                      k) 
           x)) 
   ))

(display (tan-cf 1 10)) (newline)
