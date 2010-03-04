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
(testCar
    (car '(a b))
    a)
(testCarCdr
    (car (cdr '(a b)))
    b)
(testCdrSingleItemList
    (cdr (quote (a)))
    ())
(testCdrList
    (cdr (quote (a b c)))
    (b c))
(testCdrPair
    (cdr (cons 3 4))
    4)
(testCadr
    (cadr (quote (1 ((2 3) 4))))
    ((2 3) 4))
(testCaadr
    (caadr (quote (1 ((2 3) 4))))
    (2 3))
(testCaaadr
    (caaadr (quote (1 ((2 3) 4))))
    2)
(testCdaadr
    (cdaadr (quote (1 ((2 3) 4))))
    (3))
(testEq
    (list
        (eq? (quote a) (quote a)) ; #t
        (eq? (list (quote a)) (list (quote a))) ; #f
        (eq? (quote ()) (quote ())) ; #t
        (eq? car car) ; #t
        (let ((x (quote (a))))
          (eq? x x)) ; #t
        (let ((p (lambda (x) x)))
          (eq? p p))) ; #t
    (#t #f #t #t #t #t))
(testEqv
    (eqv? (cdr (quote (a))) (quote ()))
    #t)
(testLength
    (length (quote (1 2 3 4)))
    4)
(testCond
    ((lambda (x)
        (cond
	        ((>= x 5) "large")
	        ((>= x 3) "medium")
	        ((>= x 0) "small")
	        (else "negative")))
	    4)
	"medium")
(testCondSecondForm
    (cond
        ((quote (1 2 3)) => car)
        ((quote (4 5 6)) => cdr)
        (else #f))
    1)
(testMemberFalse
    (member 6 (quote (3 4 5 10)))
    #f)
(testMember
    (member 5 (quote (3 4 5 10)))
    (5 10))
(testUnforcedPromise
    (begin
        (define x 5)
        (define my-promise (delay (set! x 66)))
        x)
    5)
(testForcedPromise
    (begin
        (define x 5)
        (define my-promise (delay (set! x 66)))
        (force my-promise)
        x)
    66)
(testDoubleForcedPromise
    (begin
        (define x 5)
        (define my-promise (delay (set! x (+ x 10))))
        (force my-promise)
        (force my-promise)
        x)
    15)
(testList?
    (list
        (list? (quote (a)))
        (list? (quote (a b c)))
        (list? (quote a))
        (list? 2.3)
        (list? (quote (a . b)))
        (list? (quote (a . ()))))
    (#t #t #f #f #f #t))
(testPair?
    (list
        (pair? 1)
        (pair? (cons 1 2))
        (pair? (list 1 2))
        (pair? (quote (1 2)))
        (pair? (quote ())))
    (#f #t #t #t #f))
(testNull?
    (list
        (null? 1)
        (null? (quote (1 2)))
        (null? (quote ()))
        (null? (cdr (list 1))))
    (#f #f #t #t))
)
