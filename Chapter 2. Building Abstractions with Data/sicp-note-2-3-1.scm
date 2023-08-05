(display (cdr '((x1 x2) (y1 y2))))(newline)
(display (car '((x1 x2) (y1 y2))))(newline)
(display (cadr '((x1 x2) (y1 y2))))(newline) ; It is equivalent to (car (cdr <list>))
(display (cddr '((x1 x2) (y1 y2))))(newline)
(display (caar '((x1 x2) (y1 y2))))(newline)

; (display (cdr ((x1 x2) (y1 y2))))(newline) exception variable y2 is not bound

 ;; 2.54 
 (define (equal? list1 list2) 
   (cond ((and (not (pair? list1)) (not (pair? list2))) 
          (eq? list1 list2)) 
         ((and (pair? list1) (pair? list2)) 
          (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2)))) 
         (else #f))) 

(display (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)))(newline)
(display (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)))(newline)

 ;;; the above solution works, but a good rule of thumb is that any time you have  
 ;;; the pattern "if <a> then <true> else <false>", you can replace the whole 
 ;;; thing by <a>, so andras' solution becomes: 
  
 (define (equal? a b) 
     (or 
      (eq? a b) 
      (and  
       (or  
        (and  
         (pair? a)  
         (pair? b))  
        (and  
         (null? a)  
         (null? b))) 
       (and  
        (equal? (car a) (car b))  
        (equal? (cdr a) (cdr b)))))) 

(display (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)))(newline)
(display (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)))(newline)