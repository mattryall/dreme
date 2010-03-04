(
(testSimpleDo
    (let ((x (quote (1 3 5 7 9))))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x) sum)))
    25)

    ((letrec
        ((loop (lambda (x sum)
            (if (null? x)
                (begin (if #f #f) sum)
                (begin (loop
                    (do "step" x (cdr x) (+ sum (car x)))
                    (do "step" sum (+ sum (car x)))))))))
        (loop x 0.0)))
)