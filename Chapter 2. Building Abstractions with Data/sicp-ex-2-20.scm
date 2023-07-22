;; ex 2.20, dotted-tail notation. 
  
 (define (same-parity first . rest) 
   (define (congruent-to-first-mod-2? a) 
     (= (remainder a 2) (remainder first 2))) 
  
   (define (select-same-parity items) 
     (if (null? items)  
         items 
         (let ((curr (car items)) 
               (select-rest (select-same-parity (cdr items)))) 
           (if (congruent-to-first-mod-2? curr) 
               (cons curr select-rest) 
               select-rest)))) 
  
   (cons first (select-same-parity rest))) 
  
 ;; an alternative implementation by andras: 
 (define (same-parity a . l) 
         (define (sp-builder result tail) 
         (if (null? tail) 
         result 
         (if (even? (+ a (car tail))) 
         ;;test for same parity 
         ;;if the current beginning of the rest (car tail) is the same parity as "a", then it is appended to the result, else the result is left untouched 
         (sp-builder (append result (list (car tail))) (cdr tail)) 
         (sp-builder result (cdr tail))))) 
         (sp-builder (list a) l)) 
  
 ;; Usage: 
 (display (same-parity 1 2 3 4 5 6 7))(newline)
 ;; (1 3 5 7) 
  
 (display (same-parity 2 3 4 5 6 7 8))(newline)
 ;; (2 4 6 8) 