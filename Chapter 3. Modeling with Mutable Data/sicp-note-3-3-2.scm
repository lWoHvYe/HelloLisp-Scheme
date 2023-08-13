(define (print-queue queue) (car queue)) 

 ;; One level of abstraction: select and to modify the front and rear pointers of a queue: 
 (define (make-queue) 
   (let ((front-ptr '()) 
         (rear-ptr '())) 
     (define (set-front-ptr! item) 
       (set! front-ptr item)) 
     (define (set-rear-ptr! item) 
       (set! rear-ptr item)) 
     (define (dispatch m) 
       (cond ((eq? m 'front-ptr) front-ptr) 
             ((eq? m 'rear-ptr) rear-ptr) 
             ((eq? m 'set-front-ptr!) set-front-ptr!) 
             ((eq? m 'set-rear-ptr!) set-rear-ptr!) 
             (else 
              (error "Undefined operation: QUEUE" m)))) 
     dispatch)) 
  
 (define (front-ptr q) (q 'front-ptr)) 
 (define (rear-ptr q) (q 'rear-ptr)) 
 (define (set-front-ptr! q item) 
   ((q 'set-front-ptr!) item)) 
 (define (set-rear-ptr! q item) 
   ((q 'set-rear-ptr!) item)) 
  
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 ;; Next level abstraction: Queue operations. No change. 
 (define (empty-queue? queue) 
   (null? (front-ptr queue))) 
 (define (front-queue queue) 
   (if (empty-queue? queue) 
       (error "FRONT called with an empty queue" queue) 
       (car (front-ptr queue)))) 
 (define (insert-queue! queue item) 
   (let ((new-pair (cons item '()))) 
     (cond ((empty-queue? queue) 
            (set-front-ptr! queue new-pair) 
            (set-rear-ptr! queue new-pair) 
            queue) 
           (else 
            (set-cdr! (rear-ptr queue) new-pair) 
            (set-rear-ptr! queue new-pair) 
            queue)))) 
 (define (delete-queue! queue) 
   (cond ((empty-queue? queue) 
          (error "DELETE! called with an empty queue" queue)) 
         (else (set-front-ptr! queue (cdr (front-ptr queue))) 
               queue))) 
  
 ;; TEST 
 (define q (make-queue)) 
 (insert-queue! q 'a) (front-ptr q);a 
 (display (front-queue q))(newline)
 (front-queue q) ;a 
 (insert-queue! q 'b)(front-ptr q) ;a b 
 (delete-queue! q) (front-ptr q);b 
 (insert-queue! q 'c) (front-ptr q);b c 
 (insert-queue! q 'd) (front-ptr q);b c d 
 (delete-queue! q) (front-ptr q);c d 
 (display (front-queue q))(newline)