#| Assignment 1 - Racket Query Language Tests (due Oct 6, 11:50pm on Markus)

***Write the names, CDF accounts and student id for each of your group members below.***
***Max group size is two students.***
Sara El-Shawa, shawasar, 1001538375
Nadir Yudoputra, yudoputr, 1001929559
|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)



; This imports your file; do not change this line!
(require "database.rkt")

; Test helpers - use these instead of the built-in syntactic forms.
(define-syntax If
  (syntax-rules ()
    ((If a b c)
     (if a b c))))
; Please do define And, Or as syntactic forms
; You may use the class code for this.


(define-syntax And
  (syntax-rules ()
    ((And p q)
     (if p q #f))))

(define-syntax Or
  (syntax-rules ()
    ((Or p q)
     (if p #t q))))


; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

(define Empty
  '(("Attribute 1" "Attribute 2")
    ))

(define Duplicate
  '(("Name" "Course" "Age")
    ("Marina" "CSC324" 23)
    ("Marina" "CSC324" 23)
    ))

(define 1Attribute
  '(("Class")
    ("CSC345")
    ("CSC324")
    ))

(define Profile 
  '(("Name" "Nationality" "Age" "Male?")
    ("Shayan" "Pakistani" 21 #t)
    ("Jai" "Trinidadian"  24 #t)
    ("Shamama" "Pakistani" 15 #f)
    ("Hamza" "Pakistani" 12 #f)
    ("Safwan" "Pakistani" 21 #t)
    ("Edmund" "Vietnamese" 21 #t)
    ("Arman" "Filipino" 21 #t)
    ("Carlito" "Filipino" 22 #t)
    ("Nadir" "Indonesian" 21 #t)
    ("Radhika" "Indian" 21 #f)
    ("Sara" "Jordanian" 21 #f)
    ("Sasha" "Russian" 21 #f)
    ("Brandon" "Mexican" 21 #t)
    ))

(define Major 
  '(("Name" "Major")
    ("Shayan" "CS")
    ("Jai" "CS"  )
    ("Shamama" "CS" )
    ("Hamza" "Failing" )
    ("Safwan" "CS" )
    ("Edmund" "CS" )
    ("Arman" "Math" )
    ("Carlito" "Math")
    ("Nadir" "CS")
    ("Radhika" "CS")
    ("Sara" "Biology")
    ("Sasha" "CCIT")
    ("Brandon" "CS")
    ))

(define GPA 
  '(("Name" "GPA")
    ("Shayan" 1.2)
    ("Jai" 3.5  )
    ("Shamama" 4.3 )
    ("Hamza" -5.2 )
    ("Safwan" 3.4 )
    ("Edmund" 0.12 )
    ("Arman" 0.5 )
    ("Carlito" 3.4)
    ("Nadir" 54.2)
    ("Radhika" 200)
    ("Sara" 3.6)
    ("Brandon" 3.3)
    ))

#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
         FROM '(("C" "A" "B" "D")
                (1 "Hi" 5 #t)
                (2 "Bye" 5 #f)
                (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

; Select from multiple tables with one empty table.
(test (SELECT * FROM [Major "P"] ['() "E"])
      '()
      )

; Select from multiple tables with one table with only attributes and no instances.
(test (SELECT * FROM [Person "P"] [Profile "P"] [Empty "E"])
      '() 
      )

; Select from multiple empty tables
(test (SELECT * FROM [1Attribute "A"] [Empty "E"] [Empty "E"])
      '() 
      )

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
         FROM ['(("Age" "A" "Name" "D")
                 (1 "Hi" 5 #t)
                 (2 "Bye" 5 #f)
                 (3 "Hi" 10 #t))
               "T1"]
         [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))


; ---- WHERE ----
; Attribute as condition, select all
(test (SELECT *
              FROM Person
              WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
              FROM Person
              WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
              FROM Person
              WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
              FROM Teaching
              WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
              FROM Person
              WHERE #t)
      Person)

; Constant false compound condition
(test (SELECT *
              FROM Person
              WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))


; Simple condition on joined tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))

; Compound condition on three joined tables
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
              FROM [Person "P"] [Teaching "T"] [Person "P1"]
              WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))

; Negation of a Condition
(test (SELECT *
              FROM GPA
              WHERE (not (equal? "Name" "Radhika")))
      '(("Name" "GPA")
        ("Shayan" 1.2)
        ("Jai" 3.5)
        ("Shamama" 4.3)
        ("Hamza" -5.2)
        ("Safwan" 3.4)
        ("Edmund" 0.12)
        ("Arman" 0.5)
        ("Carlito" 3.4)
        ("Nadir" 54.2)
        ("Sara" 3.6)
        ("Brandon" 3.3)))

; Compound condition with a negation
(test (SELECT '("P.Name" "Major")
              FROM [Profile "P"] [Major "M"]
              WHERE (And (equal? "P.Name" "M.Name") (not (equal? "Major" "CS"))))
      '(("P.Name" "Major")
        ("Hamza" "Failing")
        ("Arman" "Math")
        ("Carlito" "Math")
        ("Sara" "Biology")
        ("Sasha" "CCIT"))      )



; ---- ORDER BY ----
; Order by attribute
(test (SELECT *
              FROM Person
              ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
              FROM Person
              ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; Order by attribute on non-decreasing order by changing the sign of integer attributes.
(test (SELECT '("Name" "Age")
              FROM Profile
              ORDER BY (* -1 "Age"))
      '(("Name" "Age")
        ("Hamza" 12)
        ("Shamama" 15)
        ("Shayan" 21)
        ("Safwan" 21)
        ("Edmund" 21)
        ("Arman" 21)
        ("Nadir" 21)
        ("Radhika" 21)
        ("Sara" 21)
        ("Sasha" 21)
        ("Brandon" 21)
        ("Carlito" 22)
        ("Jai" 24)))

              
; Order by a function of an attribute
(test (SELECT *
              FROM Person
              ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

; Order with duplicate
(test (SELECT *
              FROM Teaching
              ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order with only duplicates
(test (SELECT *
              FROM Duplicate
              ORDER BY "Name")
      (SELECT *
              FROM Duplicate
              ORDER BY "Age")
 )

; Order with compound order expression function consisting of an attribute and an integer attribute
(test (SELECT *
              FROM Profile
              ORDER BY (+ (string-length "Name") "Age"))
      '(("Name" "Nationality" "Age" "Male?")
        ("Carlito" "Filipino" 22 #t)
        ("Radhika" "Indian" 21 #f)
        ("Brandon" "Mexican" 21 #t)
        ("Shayan" "Pakistani" 21 #t)
        ("Jai" "Trinidadian" 24 #t)
        ("Safwan" "Pakistani" 21 #t)
        ("Edmund" "Vietnamese" 21 #t)
        ("Arman" "Filipino" 21 #t)
        ("Nadir" "Indonesian" 21 #t)
        ("Sasha" "Russian" 21 #f)
        ("Sara" "Jordanian" 21 #f)
        ("Shamama" "Pakistani" 15 #f)
        ("Hamza" "Pakistani" 12 #f)))

; Order alphabetically
(test (SELECT *
              FROM Profile
              ORDER BY "Name")
      '(("Name" "Nationality" "Age" "Male?")
        ("Shayan" "Pakistani" 21 #t)
        ("Shamama" "Pakistani" 15 #f)
        ("Sasha" "Russian" 21 #f)
        ("Sara" "Jordanian" 21 #f)
        ("Safwan" "Pakistani" 21 #t)
        ("Radhika" "Indian" 21 #f)
        ("Nadir" "Indonesian" 21 #t)
        ("Jai" "Trinidadian" 24 #t)
        ("Hamza" "Pakistani" 12 #f)
        ("Edmund" "Vietnamese" 21 #t)
        ("Carlito" "Filipino" 22 #t)
        ("Brandon" "Mexican" 21 #t)
        ("Arman" "Filipino" 21 #t)))

; Order alphabetically based on two concatenated attributes
(test (SELECT *
              FROM Profile
              ORDER BY (string-append "Name" "Nationality"))
      '(("Name" "Nationality" "Age" "Male?")
        ("Shayan" "Pakistani" 21 #t)
        ("Shamama" "Pakistani" 15 #f)
        ("Sasha" "Russian" 21 #f)
        ("Sara" "Jordanian" 21 #f)
        ("Safwan" "Pakistani" 21 #t)
        ("Radhika" "Indian" 21 #f)
        ("Nadir" "Indonesian" 21 #t)
        ("Jai" "Trinidadian" 24 #t)
        ("Hamza" "Pakistani" 12 #f)
        ("Edmund" "Vietnamese" 21 #t)
        ("Carlito" "Filipino" 22 #t)
        ("Brandon" "Mexican" 21 #t)
        ("Arman" "Filipino" 21 #t)))

; Order on a literal table
(test (SELECT *
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; Order on two tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))


; ---- ORDER BY and WHERE ----
; Use attributes, select all 
(test
 (SELECT * 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))

 
; ---- Nested queries ----
(test
 (SELECT * 
         FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
         FROM [(SELECT '("Name") FROM Person) "Person"]
         [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                            (equal? "Course" "CSC108")))
          "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested query containing a literal
(test
 (SELECT *
         FROM [(SELECT '("A") 
                       FROM '(("A" "B") 
                              (1)
                              (10)))
               "Table1"]
         [(SELECT *
                  FROM '(("C" "A")
                         ("Hi" "Bye")
                         ("Dog" "Cat")
                         ("Red" "Blue")))
          "Table2"]
         WHERE (And (equal? (string-length "Table2.A") 3) (< 0  "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))