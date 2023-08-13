 (define (and-gate a1 a2 output) 
   (define (and-action-procedure) 
         (let ((new-value  
                 (logic-and (get-singal a1) (get-signal a2)))) 
           (after-delay and-gate-delay 
                        (lambda () 
                          (set-signal! output new-value))))) 
   (add-action! a1 and-action-procedure) 
   (add-action! a2 and-action-procedure)
   'ok)
 (define (logic-and s1 s2) 
   (if (and (= s1 1) (= s2 1)) 
         1 
         0))

 (define (or-gate a1 a2 output) 
   (define (or-action-procedure) 
         (let ((new-value  
                 (logic-or (get-singal a1) (get-signal a2)))) 
           (after-delay or-gate-delay 
                        (lambda () 
                          (set-signal! output new-value))))) 
   (add-action! a1 or-action-procedure) 
   (add-action! a2 or-action-procedure)
   'ok) 
 (define (logic-or s1 s2) 
   (if (or (= s1 1) (= s2 1)) 
         1 
         0))

 (define (inverter input output) 
   (define (invert-input) 
         (let ((new-value  
                 (logic-not (get-singal input)))) 
           (after-delay inverter-delay 
                        (lambda () 
                          (set-signal! output new-value))))) 
   (add-action! input invert-input) 
   'ok)
 (define (logic-not s) 
   (cond ((= s 0) 1)
         ((= s 1) 0)
         (else (error "Invalid signal" s))))


 (define (ripple-carry-adder a-list b-list s-list c) 
     (if (null? a-list) 
         (begin (set-signal! c 0) c) 
         (begin 
             (full-adder 
                 (car a-list) 
                 (car b-list) 
                 (ripple-carry-adder 
                     (cdr a-list) 
                     (cdr b-list) 
                     (cdr s-list) 
                     (make-wire)) 
                 (car s-list) 
                 c) 
             c))) 