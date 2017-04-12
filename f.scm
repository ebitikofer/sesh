
;;;eric bitikofer
;CS 3200 - HW 5
;Scheme functions to do each of the following
; 1. extract-lists - extracts all list elements from a list
; 2. extract-lists - extracts all list elements from a list
; 3. extract-lists - extracts all list elements from a list
; 4. extract-lists - extracts all list elements from a list
; 5. extract-lists - extracts all list elements from a list
; 6. extract-lists - extracts all list elements from a list
;Write comments describing each function.

;extract-lists - a function that extracts all list elements from a list
(define (extract-lists lst) ;define
  (cond ;condition check
    ((null? lst) '()) ;if the list is null print an empty list
    ((list? (car lst)) (cons (car lst) (extract-lists (cdr lst)))) ;if the first element is a list print it and move to the rest of the list
    (else (extract-lists (cdr lst))) ;else move to the rest of the list
  )
)

;count-evens - a function that counts all even numbers inside a list.
(define (count-evens lst) ;define
  (evener lst 0) ;initial call
)

(define (evener lst cntr) ;define helper
  (cond ;condition check
    ((null? lst) cntr) ;if the list is null return counter
    ((even? (car lst)) (evener (cdr lst) (+ cntr 1))) ;if the firat element of the list is even increment the counter and move to the rest of the list
    (else (evener (cdr lst) cntr)) ;else move to the rest of the list
  )
)

;create-pairs - a function that creates a list of pairs from its two list parameters.
(define (create-pairs lst1 lst2)
  (cond ;condition check
     ((null? lst1) '()) ;if list 1 is null print null
     ((null? lst2) '()) ;if list 2 is null print null
     ((not (equal? (length lst1) (length lst2))) '()) ;if the list lengths are not equal print null
     (else (cons (list (car lst1) (car lst2)) (create-pairs (cdr lst1) (cdr lst2)))) ;else print the first pair and move to the rest of the list
  )
)

;product - a function that takes two lists as parameters and returns all possible pairs of the of the two sets.
;(define (product lst1 lst2)
  ;if lst1 = null
    ;return
  ;else
;) ;analyze

;remove-duplicates - a function that removes all duplicates from a list
;(define (remove-duplicates lst) ;define
  ;(cond ;condition check
    ;((null? lst) '()) ;if the list is null print null
    ;((not (member? (car lst) (cdr lst))) (cons (car lst) (remove-duplicates (cdr lst)))) ;if the first element is not a member of the rest of the list print it and move to the rest of the list
    ;(else (remove-duplicates (cdr lst))) ;else move to the rest of the list
  ;)
;)

;(define (member el lst)
;  (memberator el lst #f)
;)

;(define (memberator el lst ans)
;  (cond
;    ((null? lst) ans) ;if the list is null print null
;
;  )
;)

;fibonacci - a more efficient Fibonacci numbers function than the one discussed in class.
(define (fibonacci n) ;define
  (fibber 1 0 n) ;initial call
)

(define (fibber a b cntr) ;define helper
  (if (= cntr 0) b (fibber (+ a b) a (- cntr 1))) ;if counter is zero b else move to next value in sequence
)

  ;(load "f.scm")

  ;Example:
    ;(create-pairs '(1 2 3) '(a b c)) ;returns ((1 a) (2 b) (3 c))
    ;(create-pairs '() '()) ;returns '()
    ;(create-pairs '(1 2) '(a b c)) ;returns '()
  ;Example:
    ;(product '(1 2) '(a b)) ;returns ((1 a) (1 b) (2 a) (2 b))
  ;Examples:
    ;(remove-duplicates '(1 2 2 1 1 1 3 3 3 4 2 5))
    ;returns (1 2 3 4 5)
    ;(remove-duplicates '(h h h e h h l l l h o e e o o))
    ;returns (h e l o)
