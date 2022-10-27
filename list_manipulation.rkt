#lang racket

; Types
; 1) atomic types = booleans and number
; 2) lists = everything is a list, even null

; Working with lists :
; 1) (cons A B) -> creates a cons cell (A . B) (similar to std::pair) from these we can create lists
; 2) (car X)    -> return first element from cons cell
; 3) (cdr X)    -> return second element from cons cell

; Predicates
; (null? expr) - true if expr == null else false
; (list? expr) - true if exptr is a lists (even null) else false
; (pair? expr) - true if expr is cons-cell (meaning not-null list)

; IF-statements
; (if expr body-true body-false)
; (cond (expr-1 body-1) ... (expr-n body-n)) - equivalent to switch or if-elseif-else constructs.


;Logical operators
; (not expr)
; (and expr1 expr2)
; (or expr1 expr2)




;Checks if expression is an atom
(define (atom? x)
  (and (not(null? x))
       (not(pair? x))
  )
)

;Increment number by one - typesafe
(define (inc x)
  ( if
       (atom? x)
       (+ x 1)
       (error "Not atomic")
  )
)


;Decrement number by one - typesafe
(define (dec x)
  (if
   (atom? x)
   (- x 1)
   (error "Not atomic")
  )
)


;Checks if argument is zero
(define (isZero x)
   (= x 0)
   )

;Add function
(define (add x y)
  (+ x y)
)

;Multiply
(define (mtply x y)
  (* x y)
)

;========================================================================


; Length of a list

(define (lstlen x)
  (if (null? x) ;base case
      0
      (if (atom? (car x)) ;if it is not null check if it is atom - number
  (+ 1 (lstlen (cdr x))) ; if it is add +1 and recurse deeper
  (if (= (lstlen (car x)) 0) ; if it is not check if pair.first is a list with a len of 0 - we have an empty list and we treat it like a atom
      (+ 1 (lstlen (cdr x))) ; if it is empty list
      (+ (lstlen (car x)) (lstlen (cdr x))) ; not an empty list
      )
    )
   )
 )

; Faster
(define (lstlen-f x)
  (if (null? x) ;base case
      0
      (if (atom? (car x)) ;if it is not null check if it is atom - number
          (+ 1 (lstlen (cdr x))) ; if it is add +1 and recurse deeper
          (+ (plus-one-if-zero (lstlen (car x))) (lstlen (cdr x))
      )
    )
   )
 )



(define (plus-one-if-zero x)
  (if (= x 0)
      1
      x))


; ==================================================================================================

; my-sum returns a sum of elements in a list

(define (my-sum x)
  (if (null? x) ;base case
      0
      (if (atom? (car x)) ;if it is not null check if it is atom - number
          (+ (car x) (my-sum (cdr x))) ; if it is add +1 and recurse deeper
          (+ (plus-one-if-zero (my-sum (car x))) (my-sum (cdr x))
      )
    )
   )
 )

;my-nth - return n-th element from the lists
(define (my-nth list pos)
  (if (isZero pos)
      (car list)
      (my-nth (cdr list) (dec pos))
      )
  )

;my-maxL - returns biggest element from the list

(define (my-maxL x)
  (if (null? (cdr x))
      (car x)
      (max (my-maxL (cdr x)) (car x))
      )
  )

;my-minL - returns smallest element from the list
(define (my-minL x)
  (if (null? (cdr x))
      (car x)
      (min (my-minL (cdr x)) (car x))
           )
      )

;returns true if element is in the list otherwise false
(define (my-member x target)
  (if (null? x)
      #false
      (if (= (car x) target)
          #true
          (my-member (cdr x) target)
          )
      )
  )

;return list from 1 to n
(define (my-range n)
  (my-range_in n 1)
  )
  
(define (my-range_in n current) 
  (if (= current n)
      (cons n null)
      (cons current (my-range_in n (+ current 1)))
      )
  )


;appends element to the end of the list
(define (my-append x list)
  (if (null? list)
      (cons x null)
      (cons (car list) (my-append x (cdr list)))
      )
  )

;prepends element to the start of the list
(define (my-prepend x list)
  (cons x list)
  )

;prepends list1 to list2
(define (my-prependL l1 l2)
  (if (null? (cdr l1))
      (cons (car l1) l2)
      (cons (car l1) (my-prependL (cdr l1) l2))
      )
  )

;insert element into specified position in the list
(define (my-insert e n list)
  (if (isZero n)
      (cons e (cdr list))
      (cons (car list) (my-insert e (dec n) (cdr list)))
      )
  )

;creates a list containg element e n times
(define (my-repeat n e)
  (if (isZero n)
      null
      (my-prepend e (my-repeat (dec n) e))
      )
  )

;reverses given list
(define (my-reverse x)
  (if (null? x)
      null
      (my-append (car x) (my-reverse (cdr x)))
      )
  )
     
;deletes first occurence of e in the list
(define (my-remove-f e list)
  (if (= (car list) e)
      (cdr list)
      (cons (car list) (my-remove-f e (cdr list)))
      )
  )

;deletes all occurences of e in the list
(define (my-remove-all e list)
  (if (null? list)
      null
      (if (= (car list) e)
          (my-remove-all e (cdr list))       
          (cons (car list) (my-remove-all e (cdr list)))
      )
  )
  )
;delete last occurence of e in the list
(define (my-remove-l e list)
  (my-reverse (my-remove-f e (my-reverse list)))
  )


;inserts element between all values inside a list
(define (my-insert-between e list)
  (if (null? (cdr list))
      list
      (cons (car list) (cons e (my-insert-between e (cdr list))))
      )
  )

;duplicates every element in the list with a value of 'e'
(define (my-duplicate e list)
  (if (null? (cdr list))
      list
      (if (= e (car list))
          (cons (car list) (cons e (my-duplicate e (cdr list))))
          (cons (car list) (my-duplicate e (cdr list)))
          )
      )
  )
          
;===============================================================================================

;returns true if element is inside a list, list can have more lists in it
(define (my-memberS s list)
  (if (null? list)
      #false
      (if (list? (car list))
                 (if(my-memberS s (car list))
                    #true
                    (my-memberS s (cdr list)))
                 (if (= (car list) s)
                     #true
                     (my-memberS s (cdr list))
                     )
                 )
      )           
)

(define (my-sumS list)
  (if (null? list)
      0
      (if (list? (car list))
          (+ (my-sumS (car list)) (my-sumS (cdr list)))
          (+ (car list) (my-sumS (cdr list)))
          )
      )
  )




 


