; 递归
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))
; 迭代
(define (itersum term a next b) 
  (define (iter a result) 
          (if (> a b) 
              result 
              (iter (next a) (+ result (term a))))) 
  (iter a 0)) 

; 莱布尼茨计算PI
(define (pi-sum a  b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))
(display (* 8 (pi-sum 1 1e4))) ; 1e8: 3.1415926335897932, 1e9: 3.141592651589793

(newline)

; 求函数f的定积分
(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x)
    (* x x x))

(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))
(newline)

(define (round-to-next-even x) 
   (+ x (remainder x 2))) 

(define (inc n) (+ n 1)) 

; The integral of cube between 0 and 1 computed using the composite Simpson’s rule is exact 
;  because the error of that method is proportional to the fourth derivative of the function to integrate, so for cube the error is zero. 
; The integral of cube between 0 and 1 computed using the composite midpoint rule is not exact 
;  because the error of that method is proportional to the second derivate of the function to integrate, so for cube the error is nonzero.
(define (simpson-integral f a b n) 
   (define fixed-n (round-to-next-even n)) 
   (define h (/ (- b a) fixed-n)) 
   (define (simpson-term k) 
     (define y (f (+ a (* k h)))) 
     (if (or (= k 0) (= k fixed-n)) 
         (* 1 y) 
         (if (even? k) 
             (* 2 y) 
             (* 4 y)))) 
   (* (/ h 3) (itersum simpson-term 0 inc fixed-n))) 

; 下面这个做了拆分，将 y0 + 4y1 + 2y2 + 4y3 ... + yn 拆成， (y0 + 4y1 + y2) (y2 + 4y3 + y4) ...
; 然后y0 = f(a), y1 = f(a+h), y2 = f(a+2h),处理是0,2,4...这样进行的，所以next是+2h
; (define (simpson-integral f a b n) 
;    (define (term x) 
;      (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h))))) 
;    (define (next x) 
;      (+ x (* 2 h))) 
;    (define h (/ (- b a) n)) 
;    (* (/ h 3) (sum term a next (- b (* 2 h))))) 
  
  
 (display (simpson-integral cube 0 1 99)) (newline) 
 (display (simpson-integral cube 0 1 1000)) (newline)  


  (define (product term a next b) 
     (if (> a b) 1 
        (* (term a) (product term (next a) next b)))) 

  (define (product term a next b) 
   (define (iter a res) 
     (if (> a b) res 
         (iter (next a) (* (term a) res)))) 
   (iter a 1)) 