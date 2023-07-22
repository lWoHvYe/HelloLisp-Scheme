 ;; Supporting functions: 
  
 (define nil '()) 
  
 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 

 ;; ------------
  
; 过程queen-cols返回在棋盘的前k列中放皇后的所有格局的序列
; rest-of-queens是在前k-1列放置k-1个皇后的一种方式，new-row是在第k列放置所考虑的行编号
(define (queens board-size)
    (define (queen-cols k)
        ; The base case of the queen-cols function. If k is 0, it means all queens have been placed successfully, and it returns a list containing the empty board (empty-board).
        (if (= k 0) 
            (list empty-board)
            (filter
             (lambda (positions) (safe? k positions))
             ; It flattens the resulting list of possible combination of queen positions into a single list.
             (flatmap (lambda (rest-of-queens)
             ; This generates all possible combinations of queen positions for the current row k.
               (map (lambda (new-row) (adjoin-position new-row k rest-of-queens)) 
               (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size))

 (define empty-board nil) 
  
 (define (adjoin-position new-row k rest-of-queens) 
   (cons (list new-row k) rest-of-queens)) 
; It checks whether there is a queen in column k and returns the row number where that queen is located.
 (define (queen-in-k k positions) 
   (cond ((null? positions) nil) 
         ((= (cadar positions) k) 
          (car positions)) 
         (else (queen-in-k k (cdr positions))))) 
;  It is used to remove the queen in column k from the list of queens to check for conflicts in the safe? function.
 (define (queens-not-k k positions) 
   (cond ((null? positions) nil) 
         ((= (cadar positions) k) 
          (cdr positions)) 
         (else (cons (car positions) 
                     (queens-not-k k (cdr positions)))))) 
;  The safe? function first finds the position of the queen in column k (using queen-in-k), and then it removes that queen from the list of queens to check for conflicts with the rest of the queens (using queens-not-k). 
;   After that, it checks if there are any conflicts with the other queens in the same row, column, or diagonal as the position of interest.
 (define (safe? k positions) 
   (let ((queen-k (queen-in-k k positions)) 
         (o-queens (queens-not-k k positions))) 
     (null? (filter (lambda (o-q) 
                      (or (= (car o-q) (car queen-k)) 
                          (= (- (car o-q) (cadr o-q)) 
                             (- (car queen-k) (cadr queen-k))) 
                          (= (+ (car o-q) (cadr o-q)) 
                             (+ (car queen-k) (cadr queen-k))))) 
                    o-queens)))) 

(display (length (queens 8)))(newline)