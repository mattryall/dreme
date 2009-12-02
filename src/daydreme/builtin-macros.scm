;(define-syntax begin
;    (syntax-rules ()
;        ((_ e1 e2 ...)
;            ((lambda () e1 e2 ...)))))
;(define-syntax and
;    (syntax-rules ()
;        ((_) #t)
;        ((_ e) e)
;        ((_ e1 e2 e3 ...)
;            (if e1 (and e2 e3 ...) #f))))
;(define-syntax or
;    (syntax-rules ()
;        ((_) #f)
;        ((_ e) e)
;        ((_ e1 e2 e3 ...)
;            (let ((t e1))
;                (if t t (or e2 e3 ...))))))
(define list (lambda ls ls))
(define not (lambda (x) (if x #f #t)))
(define null? (lambda (obj) (if (eqv? obj (quote ())) #t #f)))
(define length
    (lambda (ls)
        (letrec ((loop (lambda (ls n)
            (if (null? ls)
                n
                (loop (cdr ls) (+ n 1))))))
            (loop ls 0))))
;(define call/cc call-with-current-continuation)

; needs call/cc
;(define member
;    (lambda (x ls)
;        (call/cc
;            (lambda (break)
;                (letrec ((loop (lambda (ls)
;                    (if (null? ls)
;                        #f
;                        (if (eqv? x (car ls))
;                            (break ls)
;                            (loop (cdr ls)))))))
;                    (loop ls))))))
