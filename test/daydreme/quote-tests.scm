(
(testSimpleQuote
    (quote '(a b c))
    (quote (a b c)))
(testSimpleQuote
    (cons 'a 'b)
    (a . b))
(testNestedQuote
    (list 'a 'b '(c 'd))
    (a b (c 'd)))
;(testQuasiQuote
;    (let ((x 10))
;        `(a ,x b))
;    (a 10 b))
)