 (define (horner-eval x coefficient-sequence) 
   (accumulate (lambda (this-coeff higher-terms) 
                 (+ (* higher-terms x) this-coeff)) 
               0 
               coefficient-sequence)) 
 (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op  initial (cdr sequence)))))

 (display (horner-eval 2 (list 1 3 0 5 0 1)))
 (newline)
 (display (horner-eval 2 (list 2 3 0 5 0 1))) ; Expect 1 greater than above. 
 (newline)
 (display (horner-eval 2 (list 2 3 0 5 0 2))) ; Expect 2⁵ = 32 greater than above. 
 (newline)

; ex 2.35
; The map call in the function would flatten the tree into a list of the top level nodes with each one containing the count of its children nodes, 
;  if the node in it self is not a tree then 1 would be returned from the iterator passed to the map call.
 (define (count-leaves t) 
   (accumulate + 0 
     (map 
       (lambda (t) 
         (cond ((null? t) 0) 
               ((pair? t) (count-leaves t)) 
               (else 1))) 
       t))) 

(display (count-leaves (cons (list 1 2 3) (list 4 5 6))))(newline)

; ex 2.36
 (define (accumulate-n op init seqs) 
   (if (null? (car seqs)) 
       nil 
       (cons (accumulate op   init (map (lambda (s) (car s)) seqs)) 
             (accumulate-n op init (map (lambda (s) (cdr s)) seqs))))) 

; ex 2.38
 (define (fold-right op initial sequence) 
   (accumulate op initial sequence)) 
  
  
; Because of this tail-recursive nature, Scheme (and other tail-call-optimized languages) can perform an optimization known as "tail call optimization" (TCO) or "tail call elimination," where the recursive call does not consume additional stack space. 
;  Instead, it reuses the current function call's stack frame, making it more memory-efficient and avoiding stack overflow errors for deep recursive calls.
 (define (fold-left op initial sequence) 
   (define (iter result rest) 
     (if (null? rest) 
         result 
         (iter (op result (car rest)) 
               (cdr rest)))) 
   (iter initial sequence)) 