(define (filtered-accumulator filter combiner null-value term a next b) 
   (if (> a b) null-value 
     (combiner (if (filter a) (term a) 
                 null-value) 
               (filtered-accumulator filter combiner null-value term (next a) next b)))) 
               
 ; so the accumulate can be: 
 (define (accumulate combiner null-value term a next b) 
   (filtered-accumulate (lambda (x) true) 
                        combiner null-value term a next b)) 