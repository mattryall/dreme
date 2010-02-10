(
(andSyntax
    (and #t #t #t)
    #t)
(orSyntax
    (or #f #f (or #f (or) #t))
    #t)
(orReturnsArgument
    (let ((x 3))
        (or #f #f x))
    3)
(beginSyntax
    (begin
        (define a 1)
        (set! a (+ a 1))
        a)
    2)
(orIsNotHygienic
    (begin
        (define t 3)
        (or #f t))
    #f) ; should be 3, but isn't because 't' is used in the macro definition
)