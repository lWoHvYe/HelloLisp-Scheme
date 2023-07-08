 (define (e-euler k) 
   (+ 2.0 (cont-frac (lambda (i) 1) 
                     (lambda (i) 
                       (if (= (remainder i 3) 2) 
                           (/ (+ i 1) 1.5) 
                           1)) 
                     k))) 

 (define (cont-frac n d k) 
   (define (rec x) 
     (if (> x k) 
         0 
         (/ (n x) (+ (d x) (rec (+ x 1)))))) 
   (rec 1))

(display (e-euler 20)) (newline)

; from ChatGPT stands for "Chat Generative Pre-trained Transformer."
; it provide incorrect result....
