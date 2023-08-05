 ;; Testing: 
 (define (ensure b err-msg) 
   (if (not b) (error err-msg))) 
  
 (define (ensure-all list-of-tests-and-messages) 
   (cond ((null? list-of-tests-and-messages) true) 
         (else 
          (ensure (car list-of-tests-and-messages) 
                  (cadr list-of-tests-and-messages)) 
          (ensure-all (cddr list-of-tests-and-messages))))) 
  

  
 ;; -------------------------- 

 (define true #t)
 (define false #f)

 (define (make-vect x y) (cons x y)) 
 (define (xcor-vect vec) (car vec)) 
 (define (ycor-vect vec) (cdr vec)) 
  
 (define (eq-vect? v1 v2) 
   (and (= (xcor-vect v1) (xcor-vect v2)) 
        (= (ycor-vect v1) (ycor-vect v2)))) 
  
 (define (add-vect v1 v2) 
   (make-vect (+ (xcor-vect v1) (xcor-vect v2)) 
              (+ (ycor-vect v1) (ycor-vect v2)))) 
 (define (sub-vect v1 v2) 
   (make-vect (- (xcor-vect v1) (xcor-vect v2)) 
              (- (ycor-vect v1) (ycor-vect v2)))) 
 (define (scale-vect s vec) 
   (make-vect (* s (xcor-vect vec)) 
              (* s (ycor-vect vec)))) 

 ;; Tests: 
 (define v2-3 (make-vect 2 3)) 
 (define v5-8 (make-vect 5 8)) 
  
 ;; Simple TDD to ensure everything works.  Yes, pretty keen of me. 
 (ensure-all 
  (list (= (xcor-vect (make-vect 3 4)) 3) "x" 
  (= (ycor-vect (make-vect 3 4)) 4) "y" 
  (eq-vect? (make-vect 7 11) (add-vect v5-8 v2-3)) "add" 
  (eq-vect? (make-vect 3 5) (sub-vect v5-8 v2-3)) "sub" 
  (eq-vect? (make-vect 10 16) (scale-vect 2 v5-8)) "scale" 
  )) 
  

(define (make-segment start-point end-point)
  (list (make-vect 0 start-point)
        (make-vect 0 end-point)))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cadr seg))

(define (frame-coord-map frame)
    (lambda (v) 
       (add-vect (origin-frame frame)
                 (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                           (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame) 
      (let ((m (frame-coord-map frame)))
        (let ((new-origin (m origin)))
            (painter (make-frame new-origin
                                 (sub-vect (m corner1) new-origin)
                                 (sub-vect (m corner2) new-origin)))))))