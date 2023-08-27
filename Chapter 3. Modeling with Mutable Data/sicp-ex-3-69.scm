 (define first-of-integer-pair 
   (stream-map car (pairs integers integers))) 
  
 (define (triples s t u) 
   (let ((pairs-tu (pairs t u))) ;; compute pairs only *once* 
     (define (rec si i ptu top-i) 
       (cons-stream 
        (cons (stream-car si) (stream-car ptu)) 
        (if (= i (stream-car top-i)) 
            (rec s 1 (stream-cdr ptu) (stream-cdr top-i)) 
            ;; restart s cycle with next ptu 
            (rec (stream-cdr si) (1+ i) ptu top-i)))) 
     (rec s 1 pairs-tu first-of-integer-pair))) 
  
 ;; example: pythagorean triples 
  
 (define triples-integers 
   (triples integers integers integers)) 
  
 (define (pythagorean? a b c) 
   (= (square c) 
      (+ (square a) (square b)))) 
  
 (define pythagorean-triples 
   (stream-filter 
    (lambda (triple) 
      (apply pythagorean? triple)) 
    triples-integers)) 
  
 (display (stream-ref pythagorean-triples 0)) ; (3 4 5) 
 (stream-ref pythagorean-triples 1) ; (6 8 10) 
 (stream-ref pythagorean-triples 2) ; (5 12 13) 
 (stream-ref pythagorean-triples 3) ; (9 12 15) 
 (stream-ref pythagorean-triples 4) ; (8 15 17) 