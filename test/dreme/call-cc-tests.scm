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
(testMutatingContinuation
    (begin
        (define x #f)
        (define count 0)
        (let ((fn            ; just using a function so the stack has something on it
            (lambda ()
                (call/cc (lambda (k) (set! x k)))
                (set! count (+ count 1))
                (if (= count 1) (x #f) count))))
            (fn)))
    2)
)