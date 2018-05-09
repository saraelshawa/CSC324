#| Exercise 5 - Extending Objects (due Oct 27, 11:50pm)

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

In this exercise, you have just one task: extend our class system to support
a trait-like structure.
|#
#lang racket

(provide distance-trait)

#| Some background.

Recall that we saw how to dynamically "add" methods to objects.
Of course, that approach is rather ad hoc: a programmer needs to explicitly
define every method they want to add, and use the same boilerplate for each
one. Moreover, this process must be repeated for each object we want to extend.

In this exercise, you will learn how to use higher-order functions to abstract
away this boilerplate. Concretely, implement the following function:

(distance-trait obj)
  obj: an "object" that has a method named "distance".
       The "distance" method must have exactly one parameter and return
       a number, but there is no other restriction on what it must do.
       'obj' may *not* have methods named "distance-to-self" or "closer".

  Returns a new object with the same properties as 'obj', plus two new methods:
  - distance-to-self: takes no arguments. Returns the distance from
                      the calling object to itself.
  - closer: takes two arguments. Computes the distance from the calling object
            to each one and returns the one which has a smaller distance to
            the calling object.
            If both are the same distance away, return the first argument.
            Note: you shouldn't assume the two arguments themselves have a
            "distance" method. That's not required for implementing this
            behaviour.

Please see the sample tests for examples of the expected behaviour.
You do *not* need to perform any error checking.

Side note: many programming languaes support the use of traits and mixins
to extend the behaviour of objects. They differ from the approach here,
however, because they tend to be used to augment classes themselves, not
individual objects. But don't worry, we'll get there. ;)
|#
(define (distance-trait obj)
  (lambda (msg)
    (cond [(equal? msg "closer")
           (lambda (x y)
             (if (> ((obj "distance") x)
                     ((obj "distance") y))
                 y
                 x))]
      [(equal? msg "distance-to-self")
           (lambda () ((obj "distance") obj))]
          [else (obj msg)])))
