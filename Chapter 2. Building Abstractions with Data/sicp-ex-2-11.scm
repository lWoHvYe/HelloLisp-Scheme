 (define (make-interval a b) 
         (if (< a b) 
                 (cons a b) 
                 (cons b a))) 
  
 (define (lower-bound interval) (car interval)) 
 (define (upper-bound interval) (cdr interval)) 
  
 (define (mul-interval x y) 
         (define (opposite-pair? a b) 
                 (if (positive? a) 
                         (negative? b) 
                         (positive? b))) 
  
         (define (positive-pair? a b) 
                 (if (opposite-pair? a b) 
                         #f 
                         (positive? a))) 
  
         (define (negative-pair? a b) 
                 (if (opposite-pair? a b) 
                         #f 
                         (negative? a))) 
         (let        ((x0 (lower-bound x)) 
                      (x1 (upper-bound x)) 
                      (y0 (lower-bound y)) 
                      (y1 (upper-bound y))) 
                 (cond   ((negative-pair? x0 x1) 
                                 (cond   ((opposite-pair? y0 y1) 
                                                 (make-interval (* x0 y0) (* x0 y1))) 
                                         ((negative-pair? y0 y1) 
                                                 (make-interval (* x1 y1) (* x0 y0))) 
                                         (else 
                                                 (make-interval (* x1 y0) (* x0 y1))))) 
                         ((positive-pair? x0 x1) 
                                 (cond   ((opposite-pair? y0 y1) 
                                                 (make-interval (* x1 y0) (* x1 y1))) 
                                         ((negative-pair? y0 y1) 
                                                 (make-interval (* x1 y0) (* x0 y1))) 
                                         (else 
                                                 (make-interval (* x0 y0) (* x1 y1))))) 
                         (else 
                                 (cond   ((positive-pair? y0 y1) 
                                                 (make-interval (* x0 y1) (* x1 y1))) 
                                         ((negative-pair? y0 y1) 
                                                 (make-interval (* x1 y0) (* x0 y0))) 
                                         (else    
                                                 (make-interval 
                                                         ((lambda (a b) (if (< a b) a b)) (* x0 y1) (* x1 y0)) 
                                                         ((lambda (a b) (if (> a b) a b)) (* x0 y0) (* x1 y1)))))))))  
;  这部分当前不太懂，尤其是cons部分
; 这段代码看起来是生成一个包含多个区间的列表。
; 代码中首先定义了一个空的列表 `test-list` 作为结果列表。然后定义了 `test-data`，它是一个由两个列表组成的对，表示区间的上界和下界。
; 接下来使用 `map` 函数对 `test-data` 进行映射操作。内部的 `map` 函数将每个下界 `x` 和对应的上界列表进行映射操作，通过 `make-interval` 创建一个区间对象。这样可以生成多个区间，每个区间由一个下界和上界组成。
; 然后使用 `for-each` 函数对生成的区间列表进行迭代操作。迭代过程中，通过 `(lambda (x) (set! test-list (append test-list x)))` 将每个区间添加到 `test-list` 列表中。
; 最后，返回一个由 `test-list` 重复两次组成的对，即 `(cons test-list test-list)`。
; 整体来说，这段代码的目的是生成一个包含多个区间的列表，并将生成的区间添加到结果列表中。
 (define (generate-intervals) 
         (define test-list '()) 
         (define test-data 
                 (cons (list 0 1 2 3 4 5 -6 -7 -8 -9 -10) 
                       (list 5 4 3 2 1 0 -1 -2 -3 -4 -5))) 
         (for-each 
                 (lambda (x) (set! test-list (append test-list x))) 
                 (map    (lambda (x)     (map    (lambda (y) (make-interval x y)) 
                                                 (cdr test-data))) 
                         (car test-data))) 
         (cons test-list test-list)) 
  
 (define test-intervals 
         (generate-intervals)) 
  
 (define (test f g) 
         (define (interval-equals a b) 
                 (and (= (lower-bound a) (lower-bound b)) (= (upper-bound a) (upper-bound b)))) 
         (for-each (lambda (x) 
                         (for-each (lambda (y) 
                                         (cond   ((interval-equals (f x y) (g x y)) #t) 
                                                 (else 
                                                         (newline) 
                                                         (display x) (display y) 
                                                         (newline) 
                                                         (display (f x y)) (display (g x y)) 
                                                         (newline)))) 
                                   (cdr test-intervals))) 
                   (car test-intervals))) 
  
 (define (old-mul-interval x y) 
         (let            ((p1 (* (lower-bound x) (lower-bound y))) 
                          (p2 (* (lower-bound x) (upper-bound y))) 
                          (p3 (* (upper-bound x) (lower-bound y))) 
                          (p4 (* (upper-bound x) (upper-bound y)))) 
                 (make-interval 
                         (min p1 p2 p3 p4) 
                         (max p1 p2 p3 p4)))) 
  
 (test old-mul-interval mul-interval) 


;  ----
; No scheme, just math. If Ca is center of a, and Ta is tolerance of a, a is the interval
;   a = [Ca*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]
; and b is
;   b = [Cb*(1 - 0.5*Tb), Cb*(1 + 0.5*Tb)]
; If the endpoints are positive, a*b has the endpoints (after simplifying):
; 
;   a*b = [Ca*Cb*(1 - 0.5*(Ta + Tb) + 0.25*Ta*Tb),
;          Ca*Cb*(1 + 0.5*(Ta + Tb) + 0.25*Ta*Tb)]
; Ta*Tb will be a wee number, so it can be ignored. So, it appears that for small tolerances, 
;  the tolerance of the product will be approximately the sum of the component tolerances.