(
(testSimpleQuote
    (quote '(a b c))
    (quote (a b c)))
(testConsQuote
    (cons 'a 'b)
    (a . b))
(testNestedQuote
    (list 'a 'b '(c 'd))
    (a b (c 'd)))
(testQuasiQuote
    (let ((x 10))
        `(a ,x b))
    (a 10 b))
(testQuasiQuoteComplex
    (let ((x +) (y 20))
        `(a ,(x y y) b c (b ,y)))
    (a 40 b c (b 20)))
(testQuasiQuoteDeeplyNested
    (let ((b 10))
        `(a (lambda () (+ ,b a)) c))
    (a (lambda () (+ 10 a)) c))
)