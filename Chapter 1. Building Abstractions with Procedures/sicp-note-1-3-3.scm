(define (search f neg-point pos-point)
    (let ((mid-point (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            mid-point
            (let ((test-value (f mid-point)))
                (cond ((positive? test-value) 
                         (search f neg-point mid-point))
                       ((negative? test-value) 
                         (search f mid-point pos-point))
                        (else mid-point))))))

(define (average x guess) 
  (/ (+ x guess) 2)) 

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
                (search f a b))
              ((and (positive? a-value) (negative? b-value))
                (search f b a))
              (else (error "Values are not of opposite sign" a b)))))

(display (half-interval-method sin 3.0 4.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(display (fixed-point cos 1.0)) (newline)

(display (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)) (newline)

(define (sqrt x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (print-line value) 
    (display value) 
    (newline)) 
    
(display (/ (+ 1 (sqrt 5)) 2)) (newline)

(print-line (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

