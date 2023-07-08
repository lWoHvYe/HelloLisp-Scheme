;  这题还是不懂，算出的数也是不理解
 ; Average damping of y ↦ x/y^{n − 1} must be repeated ⌊log_2(n)⌋ times to compute nth roots of x as a fixed-point search. 
  (define (nth-root x n) 
   (define (fixed-point f first-guess) 
     (define (try guess) 
       (let ((next (f guess))) 
         (if (close-enough? guess next) 
           guess 
           (try next)))) 
     (define (close-enough? x y) 
       (< (abs (- x y)) 0.00001)) 
     (try first-guess)) 
   (define (average-damp f) 
     (lambda (x) (/ (+ x (f x)) 2))) 
   (define (repeated f n) 
     (define (compose f g) 
       (lambda (x) (f (g x)))) 
     (if (= n 1) 
       f 
       (compose f (repeated f (- n 1))))) 
   (fixed-point 
     ((repeated average-damp (floor (log n 2))) 
       (lambda (y) (/ x (expt y (- n 1))))) 
     1.0)) 
  
;  (define (log2 x) (/ (log x) (log 2))) 
;  (define (nth-root x n) 
;    (fixed-point ((repeated average-damp (floor (log2 n)))  
;                  (lambda (y) (/ x (pow y (- n 1))))) 
;                 1.0)) 
; (define (average-damp f)
;     (lambda (x) (average x (f x))))

; (define (average x guess) 
;   (/ (+ x guess) 2)) 

; (define (compose f g) (lambda (x) (f (g x))))

;  (define (repeated f n) 
;    (cond ((= n 0) identity) 
;          ((even? n) (repeated (compose f f) (/ n 2))) 
;          (else (compose f (repeated f (- n 1)))))) 

; (define (identity x) x) 

; (define tolerance 0.00001)

; (define (fixed-point f first-guess)
;     (define (close-enough? v1 v2)
;         (< (abs (- v1 v2)) tolerance))
;     (define (try guess)
;         (let ((next (f guess)))
;             (if (close-enough? guess next)
;                 next
;                 (try next))))
;     (try first-guess))

; (define (close-enough? x y)
;     (< (abs (- x y)) 0.001))

; (define (pow base exponent)
;   (if (= exponent 0)
;       1
;       (* base (pow base (- exponent 1)))))

 (display (nth-root 2.0 2)) 
 (newline) 
 (display (nth-root 2.0 3)) 
 (newline) 
 (display (nth-root 2.0 4)) 
 (newline) 
 (display (nth-root 4.0 4))
 (newline)
 (display (nth-root 5.0 4))
 (newline)
 (display (nth-root 6.0 4))
 (newline)
 (display (nth-root 2.0 5)) 
 (newline) 
 (display (nth-root 2.0 6)) 
 (newline) 
 (display (nth-root 2.0 7)) 
 (newline) 
 (display (nth-root 2.0 8)) 
 (newline) 
 