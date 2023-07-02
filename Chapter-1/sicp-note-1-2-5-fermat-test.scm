(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((or (= n 0) (= n 1)) #f)
          ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

 (define (square guess) 
   (* guess guess))

(display (fast-prime? 27 10))

(newline)

(display (fast-prime? 0 10))
(display (fast-prime? 1 10))
(display (fast-prime? 2 10))
; error, expect false, return true
(display (fast-prime? 561 1000)) 