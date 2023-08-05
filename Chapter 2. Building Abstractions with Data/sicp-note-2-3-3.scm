
(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; add
(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

; 交集
(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
; 并集
 (define (union-set s1 s2) 
   (cond ((and (null? s1) (not (null? s2))) 
          s2) 
         ((and (not (null? s1)) (null? s2)) 
          s1) 
         ((element-of-set? (car s1) s2) 
          (union-set (cdr s1) s2)) 
         (else (cons (car s1) 
                     (union-set (cdr s1) s2))))) 

 (define (remove-set-element x set) 
   (define (remove-set-element-iter acc rest) 
     (cond ((null? rest) acc) 
           ((equal? x (car rest)) (append acc (cdr rest))) 
           (else (remove-set-element-iter (adjoin-set (car rest) acc) (cdr rest))))) 
   (remove-set-element-iter '() set)) 

;    --------------------------------

 (define (union-set set1 set2) 
   (define (union-list set1 set2) 
     (cond ((null? set1) set2) 
           ((null? set2) set1) 
           (else  (let ((x1 (car set1)) (x2 (car set2))) 
                    (cond ((equal? x1 x2) 
                           (cons x1 (union-list (cdr set1) (cdr set2)))) 
                          ((< x1 x2) 
                           (cons x1 (union-list (cdr set1) set2))) 
                          ((< x2 x1) 
                           (cons x2 (union-list set1 (cdr set2))))))))) 
   (list->tree (union-list (tree->list set1) (tree->list set2)))) 
  
 (define (intersection-set set1 set2) 
   (define (intersection-list set1 set2) 
     (cond ((null? set1) '()) 
           ((null? set2) '()) 
           (else (let ((x1 (car set1)) (x2 (car set2))) 
                   (cond ((equal? x1 x2) 
                          (cons x1 (intersection-list (cdr set1) (cdr set2)))) 
                         ((< x1 x2) 
                          (intersection-list (cdr set1) set2)) 
                         ((< x2 x1) 
                          (intersection-list set1 (cdr set2)))))))) 
   (list->tree (intersection-list (tree->list set1) (tree->list set2)))) 

;    ---------------------
 ;; returns false if key not in records, else key 
 (define (lookup given-key set-of-records)  
   (if (null? set-of-records) #f 
       (let ((parent (entry set-of-records))) 
         (cond ((eq? parent '()) #f) 
               ((= given-key parent) parent) 
               (else 
                (lookup given-key 
                        (if (< given-key parent) 
                            (left-branch set-of-records) 
                            (right-branch set-of-records)))))))) 