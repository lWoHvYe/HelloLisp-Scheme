
 ;; (paint einstein) 
  
 ;; use zorro instead of wave 
 (define wave mark-of-zorro) 
  
 (define wave2 (beside wave (flip-vert wave))) 
 (define wave4 (below wave2 wave2)) 
  
 (define (flipped-pairs painter) 
   (let ((painter2 (beside painter (flip-vert painter)))) 
     (below painter2 painter2))) 
  
 ;; cannot redefine procedures 
 (define wave4a (flipped-pairs wave)) 
  
 (define (right-split painter n) 
   (if (= n 0) 
       painter 
       (let ((smaller (right-split painter (- n 1)))) 
         (beside painter (below smaller smaller))))) 
  
 ;; up-split also here. 
 (define (up-split painter n) 
   (if (= n 0) 
       painter 
       (let ((smaller (up-split painter (- n 1)))) 
         (below painter (beside smaller smaller))))) 
  
 (define (corner-split painter n) 
   (if (= n 0) 
       painter 
       (let ((up (up-split painter (- n 1))) 
             (right (right-split painter (- n 1)))) 
         (let ((top-left (beside up up)) 
               (bottom-right (below right right)) 
               (corner (corner-split painter (- n 1)))) 
           (beside (below painter top-left) 
                   (below bottom-right corner)))))) 
  
 (define (square-limit painter n) 
   (let ((quarter (corner-split painter n))) 
     (let ((half (beside (flip-horiz quarter) quarter))) 
       (below (flip-vert half) half)))) 

(define (square-of-four tl tr bl br)
    (lambda (painter) 
        (let ((top (beside (tl painter) (tr painter)))
              (bottom (beside (bl painter) (br painter))))
        (below bottom top))))