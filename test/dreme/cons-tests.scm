(
(testCons
    (quote (a . c))
    (a . c))
(testImproperList
    (quote (a b . c))
    (a b . c))
(testImproperListCar
    (car (quote (a b . c)))
    a)
(testImproperListCdr
    (cdr (quote (a b . c)))
    (b . c))
;(testProperListInDotNotation
;    (quote (a . (b . (c . ()))))
;    (a b c))
)