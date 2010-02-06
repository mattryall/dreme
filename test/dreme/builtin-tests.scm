(
(testList
    (list 1 (+ 2 3) 4)
    (1 5 4))
(testCons
    (cons 1 2)
    (1 . 2))
(testQuote
    (quote (a b c))
    (a b c))
(testConsQuote
    (cons (quote (a b)) (quote (c d e)))
    ((a b) c d e))
(testCdrSingleItemList
    (cdr (quote (a)))
    ())
(testCdrList
    (cdr (quote (a b c)))
    (b c))
(testCdrPair
    (cdr (cons 3 4))
    4)
(testEqv
    (eqv? (cdr (quote (a))) (quote ()))
    #t)
(testLength
    (length (quote (1 2 3 4)))
    4)
)