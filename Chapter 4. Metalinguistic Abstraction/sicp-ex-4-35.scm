 (define (an-interger-between low high)  
    (require (<= low high))  
    (amb low (an-interger-between (+ low 1) high))) 

 (define (an-interger-starting-from n)
    (amb n (an-interger-starting-from (+ n 1))))