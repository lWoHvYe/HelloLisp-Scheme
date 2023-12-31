(define (make-dataset)              
     (let ((dataset '()))             
         (define (adjoin! datum) 
             (if (not (is-in-dataset? datum)) 
                 (set! dataset (cons datum dataset))))                 
         (define (print) 
             (display dataset) 
             (newline)) 
         (define (is-in-dataset? datum) ; private helper function 
             (cond 
                 ((symbol? datum) (memq datum dataset)) 
                 ((list? datum) (member datum dataset)) 
                 (else (error "Unknown data type -- IS-IN-dataset?" datum)))) 
         (define (dispatch message) 
             (cond 
                 ((eq? message 'adjoin!) adjoin!) 
                 ((eq? message 'print) (print)) 
                 (else (error "Unknown operation -- DATASET" message)))) 
         dispatch)) 
  
 (define (adjoin-to-dataset! datum dataset) 
     ((dataset 'adjoin!) datum)) 
  
 (define (print-dataset dataset) 
     (dataset 'print)) 
      
  
      
 ; the rest of this solution "overrides" existing functions in ch5-regsim.scm 
     ; that is, they add a bit of functionality, then call the existing functions. 
     ; this way, readers (the author included) don't have to compare code 
         ; just to figure out what was added/changed. 
          
      
  
 ; implemented as a "facade" in front of the old machine 
 (define (make-new-machine-5.12)              
   (let ((machine-regsim (make-new-machine-regsim)) ; "base object" or "delegate"  
         (dataset-table                                           
             (list  
                 (list 'assign (make-dataset)) 
                 (list 'branch (make-dataset)) 
                 (list 'goto (make-dataset)) 
                 (list 'perform (make-dataset)) 
                 (list 'restore (make-dataset)) 
                 (list 'save (make-dataset)) 
                 (list 'test (make-dataset)) 
  
                 (list 'goto-registers (make-dataset)) 
                 (list 'save-registers (make-dataset)) 
                 (list 'restore-registers (make-dataset)))) 
                
         ; register names are determined by the user, so these should be stored separately 
             ; sure, it'd take one sick cookie to name a register 'goto', 
             ; but a register named 'test' is not inconceivable. 
             ; also, a user could technically manipulate pc and flag directly 
         (assign-dataset-table 
             (list 
                 (list 'pc (make-dataset)) 
                 (list 'flag (make-dataset))))) 
          
     ; "public procedures" 
     (define (allocate-register-5.12 name)         
       (set! assign-dataset-table 
             (cons  ; no duplicate checking - original regsim will crash on that anyway 
               (list name (make-dataset)) 
               assign-dataset-table))                 
       ((machine-regsim 'allocate-register) name)) 
        
     (define (lookup-dataset name) 
         (lookup-dataset-in-table name dataset-table)) 
          
     (define (lookup-assign-dataset name) 
         (lookup-dataset-in-table name assign-dataset-table))    
  
     (define (print-all-datasets) 
         (print-dataset-table dataset-table "Instructions and registers used") 
         (print-dataset-table assign-dataset-table "Assignments"))       
        
     ; "private procedures" (cannot be invoked from outside the object)  
     (define (lookup-dataset-in-table name table) 
         (let ((val (assoc name table))) 
             (if val 
                 (cadr val) 
                 (error "dataset not found -- GET-DATASET-FROM-TABLE" name table))))       
          
     (define (print-dataset-table table title) 
         (newline) 
         (display title) 
         (newline) 
         (for-each  
             (lambda (table-entry)  
                 (display (car table-entry)) 
                 (display ": ") 
                 (print-dataset (cadr table-entry))) 
             table)) 
        
     ; expose public API 
     (define (dispatch message) 
       (cond                
         ; one override 
         ((eq? message 'allocate-register) allocate-register-5.12)               
  
         ; new messages 
         ((eq? message 'print-all-datasets) (print-all-datasets)) 
         ((eq? message 'lookup-dataset) lookup-dataset) 
         ((eq? message 'lookup-assign-dataset) lookup-assign-dataset) 
  
         ; punt everything else to "base class" / delegate - INCLUDING error handling 
         (else (machine-regsim message))))                          
     dispatch))    
      
  
      
 (define (make-execution-procedure-5.12 inst labels machine pc flag stack ops)   
     (let ((dataset ((machine 'lookup-dataset) (car inst)))) 
         (adjoin-to-dataset! (cdr inst) dataset))     
     (make-execution-procedure-regsim inst labels machine pc flag stack ops))     
  
 (define (make-goto-5.12 inst machine labels pc)  
     (let ((dest (goto-dest inst)))  ; duplicated 2 lines of supporting logic 
         (if (register-exp? dest) 
             (let ((dataset ((machine 'lookup-dataset) 'goto-registers))) 
                 (adjoin-to-dataset! (register-exp-reg dest) dataset))))                  
     (make-goto-regsim inst machine labels pc)) ; punt to ch5-regsim.scm 
                         
 (define (make-save-5.12 inst machine stack pc)         
     (let ((dataset ((machine 'lookup-dataset) 'save-registers))) 
         (adjoin-to-dataset! (stack-inst-reg-name inst) dataset)) 
     (make-save-regsim inst machine stack pc)) 
  
 (define (make-restore-5.12 inst machine stack pc)                        
     (let ((dataset ((machine 'lookup-dataset) 'restore-registers))) 
         (adjoin-to-dataset! (stack-inst-reg-name inst) dataset)) 
     (make-restore-regsim inst machine stack pc))       
  
 (define (make-assign-5.12 inst machine labels operations pc)     
     (let ((dataset ((machine 'lookup-assign-dataset) (assign-reg-name inst)))) 
         (adjoin-to-dataset! (assign-value-exp inst) dataset)) 
     (make-assign-regsim inst machine labels operations pc))         
  
  
     
        
 ; ------------------------------------------------------------------------- 
        
 ; example usage. 
        
 (load "ch5-regsim.scm") 
  
 ; make the overrides official. 
 (define make-new-machine-regsim make-new-machine)  
 (define make-new-machine make-new-machine-5.12)  
  
 (define make-goto-regsim make-goto)  
 (define make-goto make-goto-5.12) 
  
 (define make-save-regsim make-save)  
 (define make-save make-save-5.12) 
  
 (define make-restore-regsim make-restore)  
 (define make-restore make-restore-5.12) 
  
 (define make-assign-regsim make-assign)  
 (define make-assign make-assign-5.12) 
  
 (define make-execution-procedure-regsim make-execution-procedure)  
 (define make-execution-procedure make-execution-procedure-5.12) 
  
  
 (define fib-machine (make-machine ;register-names ops controller-text 
     '(n val continue) 
     (list (list '< <) (list '- -) (list '+ +)) 
     '(  ; from ch5.scm 
            (assign continue (label fib-done)) 
          fib-loop 
            (test (op <) (reg n) (const 2)) 
            (branch (label immediate-answer)) 
            ;; set up to compute Fib(n-1) 
            (save continue) 
            (assign continue (label afterfib-n-1)) 
            (save n)                           ; save old value of n 
            (assign n (op -) (reg n) (const 1)); clobber n to n-1 
            (goto (label fib-loop))            ; perform recursive call 
          afterfib-n-1                         ; upon return, val contains Fib(n-1) 
            (restore n) 
            (restore continue) 
            ;; set up to compute Fib(n-2) 
            (assign n (op -) (reg n) (const 2)) 
            (save continue) 
            (assign continue (label afterfib-n-2)) 
            (save val)                         ; save Fib(n-1) 
            (goto (label fib-loop)) 
          afterfib-n-2                         ; upon return, val contains Fib(n-2) 
            (assign n (reg val))               ; n now contains Fib(n-2) 
            (restore val)                      ; val now contains Fib(n-1) 
            (restore continue) 
            (assign val                        ; Fib(n-1)+Fib(n-2) 
                    (op +) (reg val) (reg n))  
            (goto (reg continue))              ; return to caller, answer is in val 
          immediate-answer 
            (assign val (reg n))               ; base case: Fib(n)=n 
            (goto (reg continue)) 
          fib-done))) 
  
 (fib-machine 'print-all-datasets) 

Rptx


; added ((machine 'gather-info) controller-text) in `make-machine
; added ((eq? message 'gather-info) (lambda (controller-text) 
; (gather-info controller-text))
; added ((eq? message 'get-info) (lambda (type) (assoc type information)))
; to `make-new-machine
; use:
; ((<machine> 'get-info) <info-type>)
; for example:
; ((recursive-fib-machine 'get-info) 'entry-points)
; ((<machine> 'get-info) <reg-name>))

; to maintain abstraction. 
(define (get-info machine info)
  ((machine 'get-info) info))

(define (gather-info controller-text)
  (define (gather inst-type insts)
    (if debug
     (format #t "\n ~a \n" insts))      
    (define (gather-iter gathered left lst)
      (if debug
       (format #t "gather-iter: \n
                  gatehered: ~a \n
                  lst: ~a \n" gathered lst))
      (cond ((null? lst) (list (cons inst-type gathered)
                              left))
            ((not (pair? (car lst)))
             (gather-iter gathered left (cdr lst)))
            ((eq? inst-type
                  (caar lst))
             (if (member (car lst)
                         gathered)
                 (gather-iter gathered left (cdr lst))
                 (gather-iter (cons (car lst) gathered)
                              left (cdr lst))))
            (else
             (gather-iter gathered (cons (car lst) left)
                          (cdr lst)))))
    (gather-iter '() '() insts))
  (define (gather-entry-points gotos)
    (define (gather-iter gathered lst)
      (if (null? lst)
          (list 'entry-points gathered)
          (let ((dest (goto-dest (car lst))))
            (if (register-exp? dest)
                (gather-iter (cons (register-exp-reg dest)
                                   gathered)
                             (cdr lst))
                (gather-iter gathered (cdr lst))))))
    (list (gather-iter '() gotos))
  (define (gather-saved-reg saved)
    (list (cons 'stacked-ref (map (lambda (x)
                                    (stack-inst-reg-name x)) saved))))
  (define (gather-sources assigns)
  (define (sources-iter reg gathered left lst)
    (cond ((null? lst)
           (list (list reg gathered) left))
          ((eq? reg (assign-reg-name (car lst)))
           (sources-iter reg
                         (cons (cddr (car lst))
                               gathered)
                         left
                         (cdr lst)))
          (else
           (sources-iter reg gathered
                         (cons (car lst)
                               left)
                         (cdr lst)))))
  (define (sources-loop insts)
    (if (null? insts)
        '()
        (let* ((reg (assign-reg-name (car insts)))
               (srcs (sources-iter reg '() '()
                                   insts)))
          (cons (car srcs)
                (sources-loop (cadr srcs))))))
  (sources-loop assigns))
  (let* ((assigns
          (gather 'assign controller-text))
         (tests
          (gather 'test (cadr assigns)))
         (branches
          (gather 'branch (cadr tests)))
         (gotos
          (gather 'goto (cadr branches)))
         (saves
          (gather 'save (cadr gotos)))
         (restores
          (gather 'restore (cadr saves)))
         (performs
          (gather 'perform (cadr restores)))
         (entry-points
          (gather-entry-points (cdar gotos)))
         (stacked
          (gather-saved-reg (cdar saves)))
         (sources
          (gather-sources (cdar assigns)))
         (registers
          (list
           (cons 'registers (map car sources)))))
    (append
     (map (lambda (x)
            (car x))
          (list assigns tests branches gotos saves
                restores performs))
     entry-points stacked sources registers)))