(
(defineWorks
    ((lambda () (define x 5) (+ 6 x)))
    11)
(defineProcedure
    (begin
        (define double-any
            (lambda (f x) (f x x)))
        (double-any + 10))
    20)
(defineString
    (begin
        (define sandwich "vegemite-and-cheese")    ; yum!
        sandwich)
    "vegemite-and-cheese")
(defineIsTopLevel
    (begin
        ((lambda (x y) (define y x) 5) 10 20)
        y)
    10)
)