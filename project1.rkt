#lang racket
(define (rotate-list-left x)
  (cond ((empty? x) x) ;if empty return list
        ((= (count-list-elements x) 1) x) ;if one element return list
        (else (append (cdr x)(list(car x))) 
              )
        )
  )

(define (rotate-list-left-n x n)
  (cond ((= n 0) x) ;if n reaches zero return list
        (else (rotate-list-left-n (rotate-list-left x) (- n 1)) ;recursively rotate list left
              )
        )
  )

(define (count-list-elements x)
  (cond ((empty? x) 0) ;if empty return 0 elements
        (else (+ 1 (count-list-elements(cdr x))) ;else recursively add 1 to number
              )
        )
  )

(define (list-element-n x n)
  (cond ((= n 0) (car x)) ;if n is zero, return the car
        (else (list-element-n (cdr x)(- n 1)) ;recursively find nth element
              )
        )
  )

(define (list-minus-element-n x n)
  (cond ((= n 0) (cdr x)) ;if n is zero return the cdr
        (else (cons (car x)(list-minus-element-n (cdr x)(- n 1))) ;else recursively find nth element and remove
              )
        )
  )

(define (rotate-list-right x)
  (define n(count-list-elements x)) 
  (cond ((empty? x) x)
        (else (cons (list-element-n x (- n 1))(list-minus-element-n x (- n 1)))
              )
        )
  )

(define (reverse-list x)
  (cond ((empty? x) x)
        ((= (count-list-elements x) 1) x)
        (else (append (reverse-list (cdr x))(list(car x)))
              )
        )
  )

(define (cons-to-all a x)
  (cond ((empty? x) x)
        (else (map (lambda (y)(cons 'a y)) x))
        )
  )

(define (ph-1 x n) ;helper function 1
  (cons-to-all (list-element-n x n)(permute (list-minus-element-n x n)))
  )

(define (ph-2 x n)
  (cond ((= n 0)(ph-1 x 0)) ;helper function 2
        (else (append(ph-1 x n)(ph-2 x (- n 1))))
        )
  )


(define (permute x)
  (cond ((empty? x) x)
        ((= (count-list-elements x) 1) (list x))
        ((= (count-list-elements x) 2) (append(list x)(list(reverse-list x))))
        (else (ph-2 x (- (count-list-elements x) 1)))
        )
  )
                                      
  
  

(define (test x) ;ignore this is a test
  (define n(count-list-elements x))
    (list-element-n x n))
    

         
  
    
  
