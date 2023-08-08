   
 (define (contains-cycle? lst) 
   (let ((encountered (list))) 
     (define (loop lst) 
       (if (not (pair? lst)) 
           false 
           (if (memq lst encountered) 
               true 
               (begin (set! encountered (cons lst encountered)) 
                      (or (loop (car lst)) 
                          (loop (cdr lst))))))) 
     (loop lst))) 
 
 (define (has-loop? lis) 
   (define (iter searchlist seen) 
     (cond ((not (pair? searchlist)) #f) 
           ((memq searchlist seen) #t) 
           (else (or (iter (car searchlist) (cons searchlist seen)) 
                     (iter (cdr searchlist) (cons searchlist seen)))))) 
   (iter lis '())) 
  