 (define (deriv exp var) 
   (cond ((number? exp) 0) 
         ((variable? exp) (if (same-variable? exp var) 1 0)) 
         (else ((get 'deriv (operator exp)) (operands exp) 
                                            var)))) 
  
 ;; A. Above we have exported the type dispatch from the cond block internal to the function to the assumed operation table. 
 ;; We do not dispatch to the operation table for numbers or variables because both are untagged. I suppose we could tag them but that would be a drag/why? 
  
 ;; B. 
  
 (define (install-sum-package) 
   (define (addend s) 
     (cadr s)) 
   (define (augend s) 
     (caddr s)) 
   (define (deriv-sum s var) 
     (make-sum (deriv (addend s) var);; deriv must be defined before this 
               (deriv (augend s) var))) 
   (define (make-sum x y) 
     (list '+ x y)) 
  
   ;; interface to the rest of the system 
   (put 'deriv  '(+) deriv-sum) 
   (put 'make-sum '+ make-sum) 
   'done) ;; I guess this is just here for a print out message... 
  
 (define (make-sum add aug) 
   ((get 'make-sum '+) add aug)) 
  
  
 (define (install-product-package) 
   (define (multiplier p) 
     (cadr p)) 
   (define (multiplicand p) 
     (caddr p)) 
   (define (deriv-product p) 
     (make-sum 
      (make-product (deriv (multiplier p)) 
                    (multiplicand p)) 
      (make-product (multiplier p) 
                    (deriv (multiplicand p))))) 
   (define (make-product x y) 
     (list '* x y)) 
  
   ;; interface to the rest of the system 
   (put 'deriv '(*) deriv-product) 
   (put 'make-product '* make-product) 
   'done) 
  
 ;; C. Going to assume that exponent is not a func(var) as I do not feel like dealing with the chain rule... like I really do not feel like dealing with that 
 (define (make-exponent-package) 
   (define (base expression) 
     (cadr expression)) 
   (define (power expression) 
     (caddr expression)) 
   (define (deriv-exponent expression) 
     (cond 
       ((= (power expression) 0) 0) 
       ((= (power expression) 1) (base expression)) 
       (else 
        (make-product 
         (power expression) 
         (make-exponent (base expression) 
                        (- (power expression) 1)))))) 
   (define (make-exponent base power) 
     (list 'expt base power)) 
  
   ;; interface to the rest of the system 
   (put 'deriv '(expt) deriv-exponent) 
   (put 'make-exponent 'expt make-exponent) 
   'done) 
  
 ;; D. Assuming that derivative system means the deriv generic function 
  
 (define (deriv exp var) 
   (cond ((number? exp) 0) 
         ((variable? exp) (if (same-variable? exp var) 1 0)) 
         ;; line below swaps the position of the first and second arguments in the get func 
         (else ((get (operator exp) 'deriv) (operands exp) 
                                            var)))) 
 ;; We would also have to swap the order of arguments to the put calls in the package installation functions 