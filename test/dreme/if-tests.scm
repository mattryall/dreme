(
(ifTrueValue
    (if #t #t #f)
    #t)
(ifFalseValue
    (if #f #t #f)
    #f)
(ifOnlyTrueValue
    (if #t #t)
    #t)
(ifZeroValue
    (if 0 #t #f)
    #t)
(ifNonZeroValue
    (if 1 #t #f)
    #t)
(greaterThanTrue
    (> 3 2)
    #t)
(greaterThanFalse
    (> 2 3)
    #f)
(greaterThanEqualArgs
    (> 2 2)
    #f)
(greaterThanManyArgsTrue
    (> 3 2 0)
    #t)
(greaterThanManyArgsFalse
    (> 3 2 5)
    #f)
(lessThanTrue
    (< -1 2)
    #t)
(lessThanFalse
    (< 2 -3)
    #f)
(lessThanEqualArgs
    (< 2 2)
    #f)
(lessThanManyArgsTrue
    (< 1 2 5)
    #t)
(lessThanManyArgsFalse
    (< 2 3 2)
    #f)
(isEqual
    (= 1 1)
    #t)
(isNotEqual
    (= 1 2)
    #f)
(isEqualDifference
    (= (- 2 1) 1)
    #t)
(isEqualSum
    (= (+ 2 1) 3)
    #t)
(elseNotProvided
    (if #f (+ 2 4))
    #<unspecified>)
(conditionEvaluatedOnceOnly
    (begin
        (define x 0)
        (let ((inc (lambda () (set! x (+ x 1)) #t)))
            (if (inc) #t #f)
            x))
    1) ; (inc) should only be evaluated once
)

