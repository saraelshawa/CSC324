#| Exercise 2 - Higher-order functions (due Sept 29, 11:50pm)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Racket libraries, unless explicitly told to.
- You may not use mutation or any iterative constructs (for/*).
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

Implement the functions below in Racket without using explicit recursion.
Try using higher-order list functions when you can.
|#
#lang racket

(provide cartesian-product function-sort)

#|
(cartesian-product table1 table2)
  table1: list of lists [K1, K2, ..., Km]
  table2: list of lists [L1, L2, ..., Ln]

  Returns a list of the contatenation of all possible pairs of lists, in the 
  order [K1 + L1, K1 + L2, ..., K1 + Ln, K2 + L1, ..., K2 + Ln, ..., Km + Ln]

  If at least one of 'table1' and 'table2' is empty, their Caretesian product
  does not contain any lists.

> (cartesian-product '((1 4) (2 10)) '((3 4 5) (2)))
'((1 4 3 4 5) (1 4 2) (2 10 3 4 5) (2 10 2))
|#
(define (cartesian-product table1 table2)
(if (or (equal? table1 '()) (equal? table2 '()))
      '()
  (foldr (lambda (x y)
           (if (equal? y table2)
               (append (helper x table2))
          (append (helper x table2) y))
         )table2 table1)))

(define (helper sublist l2)
  (foldr (lambda (x y)
           (if (equal? y sublist)
               (list (append y x))
           (cons (append sublist x) y))
           )sublist l2))


#|
(function-sort functions arg)
  functions: a list (f1, f2, ... fn) of unary functions
             which map numbers to numbers
  arg: a number

  Returns a list of the same functions, sorted in increasing order of their 
  output when given 'arg' as input.

  For simplicity, you may assume that the functions in 'functions' will always
  output different numbers when given 'arg'.

> (define fs 
    (function-sort (list (lambda (x) (+ x 3)) 
                         (lambda (x) (- 100 x))
                         (lambda (x) (* x 2)))
                   5))
> ((first fs) 6)   ; (first fs) is (lambda (x) (+ x 3))
9
> ((second fs) 6)  ; (second fs) is (lambda (x) (* x 2))
12
> ((third fs) 6)   ; (third fs) is (lambda (x) (- 100 x))
94

Hint:
- the built-in sort is actually a higher-order function, with an optional
function parameter #:key
|#
;used https://docs.racket-lang.org/reference/pairs.html
(define (function-sort functions arg)
    (sort functions (lambda (firstfunction secondfunction)
          (< (firstfunction arg) (secondfunction arg)))))