;;;; Under Creative Commons Attribution-ShareAlike 4.0 
 ;;;; International. See 
 ;;;; <https://creativecommons.org/licenses/by-sa/4.0/>. 
  
;  (define-module (net ricketyspace sicp one twentyeight) 
;    #:use-module (srfi srfi-1) 
;    #:export (miller-rabin-test prime? run-tests)) 
  
 (define (sqmod x m) 
    ; "Return x^2 if `x^2 mod m` is not equal to `1 mod m` and x != m - 1 and x != 1; 0 otherwise." 
   (let ((square (* x x))) 
     (cond ((and  (= (remainder square m) 1) ; 1 mod m = 1 
                  (not (= x (1- m))) ; (1- m) equal to (- m 1) means (m - 1)
                  (not (= x 1))) 
            0) 
           (else square)))) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) (remainder (sqmod (expmod base (/ exp 2) m) m) m)) 
         (else (remainder (* base (expmod base (- exp 1) m)) m)))) 
  
; (fold (lambda (a p) (and (pass? a) p)) #t (iota (1- n) 1)) performs a fold operation on a list generated by (iota (1- n) 1). 
; The fold operation checks if the pass? function returns #t for all elements in the list.
; (iota (1- n) 1) generates a list of integers from 1 to (1- n) (i.e., n - 1).
; (lambda (a p) (and (pass? a) p)) is the folding function that takes an element a and an accumulator p. 
;  It checks if (pass? a) is #t and combines it with the accumulator p using the logical and operation.
; #t is the initial value of the accumulator.
; In summary, the miller-rabin-test function iterates over a range of values from 1 to (n - 1) and checks if (expmod a (1- n) n) is equal to 1 for each value. 
;  If the condition holds true for all values, the function returns #t, indicating that n is likely to be a prime number.
 (define (miller-rabin-test n) 
   (define (pass? a) 
     (= (expmod a (1- n) n) 1)) 
   (fold (lambda (a p) (and (pass? a) p)) #t (iota (1- n) 1))) 
  
 (define (prime? n) 
   (if (miller-rabin-test n) #t #f)) 
  
  
 ;;; Tests 
  
 (define (carmichael-numbers-pass?) 
   "Return #t if the sample carmichael numbers are detected as non-prime." 
   (let ((numbers '(561 1105 1729 2465 2821 6601))) 
     (cons "carmichael-numbers-pass?" 
           (fold (lambda (n p) (and (not (prime? n)) p)) #t numbers)))) 
  
 (define (prime-numbers-pass?) 
   "Return #t if the sample prime numbers are detected as prime" 
   (let ((numbers '(311 641 829 599 809 127 419 13 431 883))) 
     (cons "prime-numbers-pass?" 
           (fold (lambda (n p) (and (prime? n) p)) #t numbers)))) 
  
 (define (even-numbers-pass?) 
   "Return #t if the sample even numbers are detected as non-prime" 
   (let ((numbers '(302 640 828 594 804 128 414 12 436 888))) 
     (cons "even-numbers-pass?" 
           (fold (lambda (n p) (and (not (prime? n)) p)) #t numbers)))) 
  
 (define (run-tests) 
   (map (lambda (test) (test)) (list carmichael-numbers-pass? 
                                     prime-numbers-pass? 
                                     even-numbers-pass?))) 
  
 ;;; Guile REPL 
 ;;; 
 ;;; scheme@(guile-user)> ,use (net ricketyspace sicp one twentyeight) 
 ;;; scheme@(guile-user)> (run-tests) 
 ;;; $18 = (("carmichael-numbers-pass?" . #t)  
 ;;;        ("prime-numbers-pass?" . #t)  
 ;;;        ("prime-numbers-pass?" . #t)) 
  