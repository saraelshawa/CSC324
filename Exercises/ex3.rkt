#| Exercise 3 - Practice with macros (due October 7, 11:50pm)

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

Implement each of the following two macros.
|#
#lang racket

(provide flipped-and summation)

#|
(flipped-and <arg1> <arg2> ... <argn>)
  Takes 0 or more arguments.
  Each argument is an expression that evaluates to a boolean value.

  Returns the AND of all the arguments (i.e., #t if all evaluate to true,
  and #f otherwise). The difference between 'flipped-and' and 'and' 
  is that this one evaluates arguments *right-to-left*, and short-circuits.

  When given 0 arguments, returns #t.

> (flipped-and (/ 1 0) #f)
#f
> (flipped-and)
#t
|#

(define-syntax flipped-and
  (syntax-rules ()
    [(flipped-and) #t]
    [(flipped-and <argument>) <argument>]
    [(flipped-and <argument> ... <argn>)
    (if <argn> (flipped-and <argument> ...)
        <argn>)
     ]))

#| Design of summation.

 Review how we made a list comprehension in lecture.
 Decide what expression you would write to compute: 

> (summation i = 1 to 10 : (sqr i))
385

 Describe the general transformation of the components by fixing the following
  definition of the transformation. |#


(define-syntax summation
  (syntax-rules (= to :)
    [(summation <identifier> = <begin> to <end> : <operation>)
     (let ([<identifier> <begin>])
       (cond
         [(equal? <begin> <end>)
          <operation>]
         [else
          (summation-helper (range <begin> (+ <end> 1)) (lambda (<identifier>) <operation>))]
    ))]))

(define (summation-helper list function)
  (cond
    [(equal? '() list) 0]
    [(equal? '() (rest list)) (function (first list))]
    [else (+ (function (first list)) (summation-helper (rest list) function))]
    ))

(define-syntax my-syntax
  (syntax-rules ()
    [(my-syntax <variable> <expression>)
     (lambda (<variable>) <expression>)]))
