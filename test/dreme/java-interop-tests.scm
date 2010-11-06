(
(testConstructionAndMethodInvocation
    (begin
        (define a (new java.util.ArrayList))
        (. a add "test")
        (. a get (new java.lang.Integer "0")))
    "test")
(testStringConversion
    (. (. "foo" "concat" "bar") concat "baz")
    "foobarbaz")
(testIntegerConversion
    (+ (new java.lang.Integer "10") (new java.lang.Integer "20"))
    30)
)