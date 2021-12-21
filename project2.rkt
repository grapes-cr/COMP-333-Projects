#lang racket

(struct bst-node (value left right count) #:mutable #:transparent)

(define (add-value-to-bst tree v) ;calls helper function and outputs new updated tree
  (add-value-to-bst-subtree tree tree v)
  (display "Added value to BST. New tree: ") tree)

(define (add-value-to-bst-subtree tree subtree v) ;compares value with nodes in tree and places in proper node
  (cond ((and (< v (bst-node-value subtree)) (empty?(bst-node-left subtree)))
        (set-bst-node-left! subtree (bst-node v empty empty 1)))
        ((and(< v (bst-node-value subtree)) (not(empty?(bst-node-left subtree))))
        (add-value-to-bst-subtree tree (bst-node-left subtree) v)) 
        ((and(> v (bst-node-value subtree)) (empty?(bst-node-right subtree)))
        (set-bst-node-right! subtree (bst-node v empty empty 1)))
        ((and(> v (bst-node-value subtree)) (not(empty?(bst-node-right subtree))))
        (add-value-to-bst-subtree tree (bst-node-right subtree) v))
        ((= v (bst-node-value subtree))
        (set-bst-node-count! subtree (+ (bst-node-count subtree) 1)))))

(define (get-bst-value-list-inorder n) ;gets all of the values in the bst and appends them together
  (cond ((empty? n) (void))
        ((and (empty?(bst-node-left n)) (empty?(bst-node-right n))) (list (bst-node-value n)))
        ((and (empty?(bst-node-left n)) (not(empty?(bst-node-right n)))) (cons (bst-node-value n) (get-bst-value-list-inorder (bst-node-right n))))
        ((and (empty?(bst-node-right n)) (not(empty?(bst-node-left n)))) (append (get-bst-value-list-inorder (bst-node-left n)) (list (bst-node-value n)))) 
        ((and (not(empty?(bst-node-left n))) (not(empty?(bst-node-right n)))) (append (get-bst-value-list-inorder (bst-node-left n)) (append (list(bst-node-value n)) (get-bst-value-list-inorder (bst-node-right n)))))))

(define(get-random-in-range rng) ;gets a random number in range
  (inexact->exact (remainder (floor(* (random) (expt 2 31))) rng)))

(define (get-random-list-in-range rng count) ;gets random number in range count times
  (cond ((= 1 count) (list (get-random-in-range rng)))
        (else (cons (get-random-in-range rng) (get-random-list-in-range rng (- count 1))))))

(define (contains? x n) ;recursively checks if a list has a value
  (cond ((empty? x) #f)
        ((= (car x) n) #t)
        (else (contains? (cdr x) n))))

(define (contains-duplicates? x) ;recursively checks if there are duplicates in a list
  (cond ((or (empty? x) (empty? (cdr x))) #f)
        ((contains? (cdr x) (car x)) #t)
        (else (contains-duplicates? (cdr x)))))

(define (add-random-to-list-unique x rng) ;calls helper function to add randoms in range to a list
  (add-random-to-list-unique-1 x (cons (get-random-in-range rng) x) rng))

(define (add-random-to-list-unique-1 x y rng) ;helper function for above function
  (cond ((not(contains-duplicates? y)) y)
        (else (add-random-to-list-unique-1 x (cons (get-random-in-range rng) (cdr y)) rng))))

(define (get-random-list-in-range-unique rng count) ;call the two above functions to create a random list
  (cond ((= count 1) (add-random-to-list-unique empty rng))
        (else (append (add-random-to-list-unique (get-random-list-in-range-unique rng (- count 1)) rng)))))
  
(define (is-sorted? x) ;checks in values are sorted in a list
  (cond ((or (empty? x) (empty? (cdr x))) #t)
        (else (and (< (car x) (car(cdr x))) (is-sorted? (cdr x))))))

(define (find-path v n) ;calls helper function to find a path to a value
  (find-path-1 v n empty))

(define (find-path-1 v n p) ;creates a path to a specified bst node
  (cond ((empty? n) p)
        ((= v (bst-node-value n)) (cons n p))
        ((< v (bst-node-value n)) (cons n (find-path-1 v (bst-node-left n) p)))
        ((> v (bst-node-value n)) (cons n (find-path-1 v (bst-node-right n) p)))))

(define (bst-contains? v n) ;checks if bst contains a value
  (cond ((= v (bst-node-value (get-tail(find-path v n)))) #t)
        (else #f)))

(define (get-tail n) ;helper function to get to the tail of a tree (returns the tail node)
  (cond ((or(empty? n) (empty? (cdr n))) (car n)) ;if empty or singleton
        (else (cdr (get-tail n))))) ;recursive call on cdr to eventually get to tail

(define (get-path-values p) ;puts the path values in a list
  (cond ((empty? p) p)
        (else (cons (bst-node-value (car p)) (get-path-values (cdr p))))))

(define (get-child-count n) ;counts the amount of children a node has
  (cond ((empty? n) 0)
        ((and (empty? (bst-node-left n)) (empty? (bst-node-right n))) 0)
        ((and (not(empty? (bst-node-left n))) (empty? (bst-node-right n))) 1)
        ((and (empty? (bst-node-left n)) (not(empty? (bst-node-right n)))) 1)
        ((and (not(empty? (bst-node-left n))) (not(empty? (bst-node-right n)))) 2)))

(define (find-inorder-prev n) ;finds inorder pred 
  (cond ((null? (bst-node-right (bst-node-left n))) (list (bst-node-value n) n))
        (else (find-path-inorder-prev (bst-node-right (bst-node-left n)) (bst-node-left n)))))

(define (find-path-inorder-prev n p) ;helper function to find the path to the inorder pred
  (cond ((null? (bst-node-right n)) (cons n (car p)))
        (else (find-path-inorder-prev (bst-node-right n) (append n p)))))

(define (delete-value-from-bst v tree) ;delete value from bst tree
  (delete-node-from-bst (reverse (find-path v tree)) tree))

(define (set-bst-node n1 n2)
  (set-bst-node-value! n1 (bst-node-value n2))
  (set-bst-node-left! n1 (bst-node-left n2))
  (set-bst-node-right! n1 (bst-node-right n2))
  (set-bst-node-count! n1 (bst-node-count n2))) ;helper function to set a node to another

(define (delete-node-from-bst n1 tree) 
  (cond ((empty? n1) tree)
        ((> (bst-node-count (car n1)) 0) (set-bst-node-count! (car n1) (- (bst-node-count (car n1)) 1)) tree)
        ((= (get-child-count (car n1)) 0) ;child count 0
         (cond ((equal? (car n1) tree) empty)
               ((equal? (car n1) (bst-node-left (car (cdr n1)))) (set-bst-node-left! (car (cdr n1)) empty) tree)
         (else (set-bst-node-right! (car (cdr n1)) empty) tree)))
        ((= (get-child-count (car n1)) 1) ;child count 1
         (cond ((and(equal? (car n1) tree) (empty? (bst-node-right (car (cdr n1))))) (set-bst-node tree (car n1)) tree) ;case 1.1
               ((and(equal? (car n1) tree) (empty? (bst-node-left (car n1)))) (set-bst-node n1 (car (cdr n1))) tree) ;case 1.2
               ((and(equal? (car n1) (bst-node-left (car (cdr n1)))) (empty? (bst-node-right (car (cdr n1))))) (set-bst-node-left! (car n1) (car (cdr n1))) tree) ;case 1.3
               ((and(equal? (car n1) (bst-node-left (car (cdr n1)))) (empty? (bst-node-left (car n1)))) (set-bst-node-right! (car n1) (car (cdr n1))) tree) ;case 1.4
               ((and(equal? (car n1) (bst-node-right (car (cdr n1)))) (empty? (bst-node-right (car (cdr n1))))) (set-bst-node-left! (car n1) (car (cdr n1))) tree) ;case 1.5
               ((and(equal? (car n1) (bst-node-right (car (cdr n1)))) (empty? (bst-node-left (car n1)))) (set-bst-node-right! (car n1) (car (cdr n1))) tree))) ;case 1.6
        ((= (get-child-count (car n1)) 2) ;child count 2
         (cond ((= (get-child-count (find-inorder-prev n1)) 0)
                (cond ((equal? (find-inorder-prev n1) (bst-node-left (car (cdr n1))))
                       (set-bst-node n1 (find-inorder-prev n1)) (set-bst-node (find-inorder-prev n1) empty) tree)) ;case 2.1
                (cond ((equal? (find-inorder-prev n1) (bst-node-right (car (cdr n1))))
                       (set-bst-node n1 (find-inorder-prev n1)) (set-bst-node (find-inorder-prev n1) empty) tree)) ;case 2.2
               ((= (get-child-count (find-inorder-prev n1)) 1)
                (cond ((equal? (find-inorder-prev n1) (bst-node-left (car (cdr n1))))
                       (set-bst-node n1 (find-inorder-prev n1)) (set-bst-node (find-inorder-prev n1) empty) tree)) ;case 2.3
                (cond ((equal? (find-inorder-prev n1) (bst-node-right (car (cdr n1))))
                       (set-bst-node n1 (find-inorder-prev n1)) (set-bst-node (find-inorder-prev n1) empty) tree)))))))) ;case 2.4
               
                
;(define tree(bst-node 2 empty empty 0))
;(add-value-to-bst tree 3)
;(add-value-to-bst tree 1)
;(add-value-to-bst tree 4)
;(get-bst-value-list-inorder tree)