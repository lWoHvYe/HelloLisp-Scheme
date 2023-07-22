(define nil '())
(define list-1 (cons 1 (cons 2 (cons 3 (cons 4 nil))))) ; (car . cdr)
(define list-2 (list 1 2 3 4 5)) ; 与上面的等价。表

(display list-1)(newline)
(display list-2)(newline)

; 传统的递归方式
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))


(display (length list-1))(newline)
; 尾递归，类似于迭代的方式
(define (length items)
    (define (length-iter a count)
        (if (null? a)
            count
            (length-iter (cdr a) (+ 1 count))))
    (length-iter items 0))

(display (length list-2))(newline)

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(display (append list-1 list-2)) (newline)

; reverse

 (define nil '()) 
  
 (define (reverse items) 
   (define (iter items result) 
     (if (null? items) 
         result 
         (iter (cdr items) (cons (car items) result)))) 
  
   (iter items nil)) 
  
  
 ;; Usage 
 (display (reverse (list 1 2 3 4))) (newline)
  
 (define (reverse items) 
   (if (null? (cdr items)) 
       items 
       (append (reverse (cdr items)) 
               (cons (car items) nil)))) 
  
 (display (reverse (list 1 2 3 4))) (newline)

 ; 带点尾部记法
; (define (f x y . z) <body>) 在调用时，z是一个表

;  随着课程的推进，对lisp的 syntax有了跟多了解，比之前单看doc更深了一步
; map是一个高阶过程，有一个过程参数和一个表参数，返回将这一过程应用于表中各个元素得到的结果形成的表
; (define (map proc items)
    ; (if (null? items)
        ; nil
        ; (cons (proc (car items))
            ;   (map proc (cdr items)))))

(display (map abs (list 1 -1 2.5 -4.8)))(newline)

; for-each is a standard procedure that is provided by most Scheme implementations as part of the language specification. 
; (define (for-each proc items) 
;    (let ((items-cdr (cdr items))) 
    ;  (proc (car items)) 
    ;  (if (not (null? items-cdr)) 
        ;  (for-each proc items-cdr) 
        ;  #t)))

(define (print-squared x)
  (display (* x x))
  (newline))

(for-each print-squared '(1 2 3 4 5))
