; 牛顿法求导数，不动点
(define (derive g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.000001)

(define (cube x)
    (* x x x))

(display ((derive cube) 5)) (newline)

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((derive g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

(define (sqrt x)
    (newtons-method (lambda (y) (- (square y) x)) 1.0))

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

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))

(define (square x) 
  (* x x))

(display (sqrt 5)) (newline)

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))

(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (average x guess) 
  (/ (+ x guess) 2)) 

(display (sqrt 5)) (newline)

(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))
    
(display (sqrt 5)) (newline)