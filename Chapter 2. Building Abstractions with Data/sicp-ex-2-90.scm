 ; install different term-list representations correspondingly: 
  
 ; sparse as in the text 
 (define (install-dense-term-list) 
      
     ;inner 
     (define (adjoin-term term term-list) 
         (if (=zero? (coeff term)) 
             term-list  
             (cons (term term-list))))  
  
     (define (first-term term-list) (car term-list)) 
     (define (rest-terms term-list) (cdr term-list)) 
  
     ;interface 
    ;  在Scheme中，'sparse 和 '(sparse) 都表示符号（Symbol）'sparse'。它们是等价的
     (define (tag term-list) (attach-tag 'sparse term-list)) 
  
     (put 'adjoin-term 'sparse adjoin-term)  
  
     (put 'first-term '(sparse)  
         (lambda (term-list) (first-term term-list))) 
  
     (put 'rest-term '(sparse)  
         (lambda (term-list) (tag (rest-terms term-list)))) 
     'done) 
  
 ; dense from ex_2.89 
 (define (install-dense-term-list) 
      
     ;inner 
     (define (adjoin-term term term-list)  
     (cond ((=zero? (coeff term)) term-list)  
             ((=equ? (order term) (length term-list)) (cons (coeff term) term-list))  
             (else (adjoin-term term (cons 0 term-list)))))  
  
     (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list))) 
     (define (rest-terms term-list) (cdr term-list)) 
  
     ;interface 
     (define (tag term-list) (attach-tag 'dense term-list)) 
  
     (put 'adjoin-term 'dense adjoin-term)  
  
     (put 'first-term '(dense)  
         (lambda (term-list) (first-term term-list))) 
  
     (put 'rest-term '(dense)  
         (lambda (term-list) (tag (rest-terms term-list)))) 
     'done) 
  
 ; since term representation is all the same,  
 ; we only registered the term-list type into the table. 
 (define (adjoin-term term term-list)  
     ((get 'adjoin-term (type-tag term-list)) term term-list)) 
  
 (define (first-term term-list) (apply-generic 'first-term term-list)) 
 (define (rest-term term-list) (apply-generic 'rest-term term-list)) 

; 2-91
  (define (div-terms L1 L2) 
   (if (empty-termlist? L1) 
       (list (the-empty-termlist) (the-empty-termlist)) 
       (let ((t1 (first-term L1)) 
             (t2 (first-term L2))) 
         (if (> (order t2) (order t1)) 
             (list (the-empty-termlist) L1) 
             (let ((new-c (div (coeff t1) (coeff t2))) 
                   (new-o (- (order t1) (order t2)))) 
               (let ((rest-of-result 
                      (div-terms (sub-terms L1 
                                            (mul-term-by-all-terms 
                                             (make-term new-o new-c) 
                                             L2)) 
                                 L2))) 
                 (list (adjoin-term (make-term new-o new-c) 
                                    (first rest-of-result)) 
                       (second rest-of-result)))))))) 
  
 (define (div-poly p1 p2) 
   (if (same-variable? (variable p1) (variable p2)) 
       (map (lambda (termlist) (make-poly (variable p1) termlist)) 
            (div-terms (term-list p1) (term-list p2))) 
       (error "Polys not in same var -- DIV-POLY" 
              (list p1 p2)))) 