(
(testMemberNotFound
    (member (quote d) (quote (a b c)))
    #f)
(testMemberFirst
    (member (quote a) (quote (a b c)))
    (a b c))
(testMemberMiddle
    (member (quote b) (quote (a b c)))
    (b c))
(testSimpleExample
    (+ 2
       (call/cc
         (lambda (k)
           (* 5 (k 4)))))
    6)
)