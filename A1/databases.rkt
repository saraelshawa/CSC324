#| Assignment 1 - Racket Query Language  (due Oct 6, 11:50pm, on Markus)

***Write the names, CDF accounts and student IDs for each of your group members below.***
***Max group size is two students.***
Sara El-Shawa
Nadir Yudoputra
|#

#lang racket

(provide identify-indices extract-column extract-columns extract-attributes join-attributes rename-attributes check-attribute-uniqueness append-attributes join-tuples helper cartesian-product replace-attr filter-table filter-tuples replace-conditions order-tuples SELECT compare)

(define-syntax And
  (syntax-rules ()
    ((And p q)
     (if p q #f))))

(define-syntax Or
  (syntax-rules ()
    ((Or p q)
     (if p #t q))))

(define-syntax If
  (syntax-rules ()
    ((If a b c)
     (if a b c))))

(define ns (make-base-namespace))



;Implementing SELECT functionality

;Extract the attributes and their conrresponding instances in a table. The Highest order function in for the SELECT statement.
(define (extract-attributes attributes table)
  (cond
    [(empty? table) '()]
    [(equal? attributes *) table]
    [else (cons attributes (extract-columns (identify-indices attributes (first table)) (rest table)))]))


;Convert the list of target attributes to their respective indices in which they appear in the table's attributes.
(define (identify-indices selected-attributes attributes)
  (cond
    [(empty? selected-attributes) '()]
    [else (cons (index-of attributes (first selected-attributes)) (identify-indices (rest selected-attributes) attributes))]))

;Extracts the target attributes from the intances of table. Higher order function of the function below
(define (extract-columns indices table)
  (cond
    [(empty? table) '()]
    [else (cons (extract-column indices (first table)) (extract-columns indices (rest table)))]))


;Extracts the target attributes from a single tuple.
(define (extract-column indices tuple)
  (cond
    [(empty? indices) '()]
    [else (cons (list-ref tuple (first indices))  (extract-column (rest indices) tuple))]))

;Implementing FROM functionality

;Returns a list of all the tables contained in 'pairs'.
(define (extract-tables pairs)
  (cond
    [(empty? pairs) '()]
    [else (cons (first (first pairs)) (extract-tables (rest pairs)))]))

;Performs a cartesian join on the list of tables given by 'pairs'. Highest order function of the FROM statement.
(define (join-tables pairs)
  (cond
    [(empty? pairs) '()]
    ;[(string? (first (first pairs))) pairs]
    [(check-empty-tables pairs) '()]
    [else (cons (join-attributes pairs (append-attributes (extract-tables pairs)))  (join-tuples (extract-tables pairs)))]))

;Checks if any of the tables given in 'pairs' are empty
(define (check-empty-tables pairs)
  (cond
    [(empty? (first (first pairs))) #t]
    [(empty? (rest (first (first pairs)))) #t]
    [(empty? (rest pairs)) #f]
    [else (check-empty-tables (rest pairs))]))

;Joins the attributes of all the tables given in 'pairs'. Higher order function of the two functions below.
(define (join-attributes pairs all-attributes)
  (cond
    [(empty? pairs) '()]
    [else (append  (rename-attributes (first (first (first pairs))) all-attributes (second (first pairs))) (join-attributes (rest pairs) all-attributes))]))

;Renames the overalpping attributes in the list of tables.
(define (rename-attributes attributes all-attributes suffix)
  (cond
    [(empty? attributes) '()]
    [(check-attribute-uniqueness (first attributes) all-attributes 0) (cons (first attributes) (rename-attributes (rest attributes) all-attributes suffix))]
    [else (cons (string-append suffix "." (first attributes)) (rename-attributes (rest attributes) all-attributes suffix))]))

;Checks if an attribute is unique amongs all the tables in the list.
(define (check-attribute-uniqueness attribute all-attributes count) 
  (cond
    [(equal? 2 count) #f]
    [(empty? all-attributes) #t]
    [(equal? attribute (first all-attributes)) (check-attribute-uniqueness attribute (rest all-attributes) (+ count 1))]
    [else (check-attribute-uniqueness attribute (rest all-attributes) count)]))

;Combines the attributes of each table in tables without checking uniqueness.
(define (append-attributes tables)
  (cond
    [(empty? tables) '()]
    [else (append (first (first tables)) (append-attributes (rest tables)))]))

;Performs cartesian product on all the instances of the tables in the list. Higher order function of the two functions below.
(define (join-tuples tables)
  (cond
    [(empty? (rest tables)) (rest (first tables))]
    [(empty? (rest (rest tables))) (cartesian-product (rest (first tables)) (rest (second tables)))]
    [else (cartesian-product (rest (first tables)) (join-tuples (rest tables)))]))

;Performs cartesian product on the instances of two tables.
(define (cartesian-product table1 table2)
  (cond [(empty? table1) '()]
        [else (append (helper (first table1) table2) (cartesian-product (rest table1) table2))]))

;Appends a single tuple instance of a table to all the tuple instances of another table.
(define (helper element lst)
  (cond [(empty? lst) '()]
        [else (cons (append element (first lst)) (helper element (rest lst))) ]))



;Implementing WHERE functionality

;Returns a lambda function which, takes a tuple instance, a target attribute, and the attributes for that tuple instance and returns 'target' if 'target'
;is not an attribute in 'attributes', otherwise, it returns the corresponding attribute in 'tuple'.
(define (replace-attr target attributes)
  (lambda (tuple)
    (cond
      [(false? (member target attributes)) target]
      [else (list-ref tuple (index-of attributes target))])))

;Deletes tuple instances in 'table' that do not evaluate true when checked against 'conditions'. Highest order function of the WHERE functionality.
(define (filter-table conditions table)
  (cons (first table)(filter-tuples conditions (first table) (rest table))))

;Evaluates non-nested racket non-nested expressions.
(define (my-eval condition)
  (cond
    [(equal? (first condition) 'And) (And (second condition) (third condition))]
    [(equal? (first condition) 'Or) (Or (second condition) (third condition))]
    [else (eval condition ns)]))

;Filters the tuple instances of 'tuples' of a table by checking each tuple instance against 'conditions'
(define (filter-tuples conditions attributes tuples)
  (cond
    [(empty? tuples) '()]
    [(replace-conditions (first tuples) attributes conditions) (cons (first tuples) (filter-tuples conditions attributes (rest tuples)))]
    [else (append '() (filter-tuples conditions attributes (rest tuples)))]))


;Given a condition, a tuple and a list of attributes for that tuple, replace the attribute variables in 'conditions' with the corresponding value in 'tuple'.
(define (replace-conditions tuple attributes conditions)
  (cond
    [(false? (list? conditions))
     ((replace-attr conditions attributes ) tuple)]
    [(equal? (length conditions) 2)
     (cond
       [(list? (second conditions))
        (my-eval (append (list (first conditions)) (list (replace-conditions tuple attributes (second conditions)))))]
       [else
        (my-eval (append (list (first conditions)) (list ((replace-attr (second conditions) attributes ) tuple)) ))])]
    [(equal? (length conditions) 3)
     (cond
       [(and (list? (second conditions)) (list? (third conditions)))
        (my-eval (append (list (first conditions)) (list (replace-conditions tuple attributes (second conditions))) (list (replace-conditions tuple attributes (third conditions)))))]
       [(list? (second conditions))
        (my-eval (append (list (first conditions)) (list (replace-conditions tuple attributes (second conditions))) (list ((replace-attr (third conditions) attributes ) tuple))))]
       [(list? (third conditions))
        (my-eval (append (list (first conditions)) (list ((replace-attr (second conditions) attributes ) tuple)) (list (replace-conditions tuple attributes (third conditions)))))]
       [else
        (my-eval (append (list (first conditions)) (list ((replace-attr (second conditions) attributes ) tuple)) (list ((replace-attr (third conditions) attributes ) tuple))))]
       )]))

;Implement ORDER BY functionality

;Using sort to order the tuples based 'order-expr'.
(define (order-tuples attributes order-expr tuples)
  (sort tuples compare #:key (lambda (tuple) (eval (replace-conditions tuple attributes order-expr) ns))))

;Determine whether the operands are numbers or strings in order to determine the proper operator to be used to compare two tuple instances.
(define (compare operand1 operand2)
  (cond
    [(and (string? operand1) (string? operand2))  (string>? operand1 operand2)]
    [else (> operand1 operand2)]))

;Implementing Macros
(define-syntax SELECT
  (syntax-rules (SELECT FROM ORDER BY WHERE)
    [(SELECT <attributes> FROM <table>)
     (extract-attributes <attributes> <table>)]
    [(SELECT <attributes> FROM [table1 name1] ...)
     (let ([<table> (list (list table1 name1) ... )])
           (cond
             [(check-empty-tables <table>) '()]
             [else (extract-attributes <attributes> (join-tables <table>))]))]
    [(SELECT <attributes> FROM <table> WHERE <conditions>)
     (extract-attributes <attributes> (filter-table '<conditions>  <table>))]
    [(SELECT <attributes> FROM [table1 name1] ... WHERE <conditions>)
     (let ([<table> (list (list table1 name1) ... )])
           (cond
             [(check-empty-tables <table>) '()]
             [else (extract-attributes <attributes> (filter-table '<conditions> (join-tables <table>)))]))]
    [(SELECT <attributes> FROM <table> ORDER BY <order-expr>)
     (extract-attributes <attributes> (append (list (first <table>)) (order-tuples (first <table>) '<order-expr> (rest <table>))))]
    [(SELECT <attributes> FROM [table1 name1] ... ORDER BY <order-expr>)
     (let ([<table> (list (list table1 name1) ... )])
           (cond
             [(check-empty-tables <table>) '()]
             [else (extract-attributes <attributes> (append (list (first (join-tables <table>))) (order-tuples (first (join-tables <table>)) '<order-expr> (rest (join-tables <table>)))))]))]
    [(SELECT <attributes> FROM <table> WHERE <conditions> ORDER BY <order-expr>)
     (extract-attributes <attributes> (append (filter-table '<conditions> (append (list (first <table>)) (order-tuples (first <table>) '<order-expr> (rest <table>))))))]
    [(SELECT <attributes> FROM [table1 name1] ... WHERE <conditions> ORDER BY <order-expr>)
     (let ([<table> (list (list table1 name1) ... )])
       (cond
             [(check-empty-tables <table>) '()]
             [else (extract-attributes <attributes> (append (filter-table '<conditions> (append (list (first (join-tables <table>))) (order-tuples (first (join-tables <table>)) '<order-expr> (rest (join-tables <table>)))))))]))]
    ))


