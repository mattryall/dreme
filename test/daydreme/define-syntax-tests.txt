(
(andSyntax
    (and #t #t #t)
    #t)
(orSyntax
    (or #f #f (or #f (or) #t))
    #t)
(beginSyntax
    (begin
        (define a 1)
        (set! a (+ a 1))
        a)
    2)
)