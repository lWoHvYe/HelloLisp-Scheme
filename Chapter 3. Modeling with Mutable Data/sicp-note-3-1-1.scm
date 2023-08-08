(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

(display (withdraw 25))(newline)
(display (withdraw 25))(newline)
(display (withdraw 60))(newline)
(display (withdraw 15))(newline)

(define new-withdraw 
    (let ((balance 100))
        (lambda (amount) 
           (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))))

(display (new-withdraw 25))(newline)
(display (new-withdraw 25))(newline)
(display (new-withdraw 60))(newline)
(display (new-withdraw 15))(newline)

(define (make-withdraw balance)
    (lambda (amount) 
       (if (>= balance amount)
        (begin  (set! balance (- balance amount)) balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))


(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))
            
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)        
    
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknow request -- Make-Account" m))))
    dispatch)

 (define (make-account balance password) 
   (define (withdraw amount) 
     (if (>= balance amount) (begin (set! balance (- balance amount)) balance) 
         "Insufficient funds")) 
   (define (deposit amount) 
     (set! balance (+ balance amount)) balance) 
   (define (dispatch pass m) 
     (cond ((not (eq? pass password)) (lambda (x) "Incorrect password")) 
           ((eq? m 'withdraw) withdraw) 
           ((eq? m 'deposit) deposit) 
           (else (error "Unknown request -- MAKE-ACCOUNT" m)))) 
   dispatch) 