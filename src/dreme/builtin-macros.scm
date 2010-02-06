(define-syntax begin
    (syntax-rules ()
        ((_ e1 e2 ...)
            ((lambda () e1 e2 ...)))))
(define-syntax and
    (syntax-rules ()
        ((_) #t)
        ((_ e) e)
        ((_ e1 e2 e3 ...)
            (if e1 (and e2 e3 ...) #f))))
(define-syntax or
    (syntax-rules ()
        ((_) #f)
        ((_ e) e)
        ((_ e1 e2 e3 ...)
            (let ((t e1))
                (if t t (or e2 e3 ...))))))
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

(define-syntax let
  (syntax-rules ()
    ((_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...))))

(define-syntax let*
  (syntax-rules ()
    ((_ () e1 e2 ...)
     (let () e1 e2 ...))
    ((_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
     (let ((x1 v1))
       (let* ((x2 v2) ...) e1 e2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((_ () body1 body2 ...)
     (let () body1 body2 ...))
    ((_ ((var init) ...) body1 body2 ...)
     (letrec-helper
       (var ...)
       ()
       ((var init) ...)
       body1 body2 ...))))
(define-syntax letrec-helper
  (syntax-rules ()
    ((_
       ()
       (temp ...)
       ((var init) ...)
       body1 body2 ...)
     (let ((var #<unspecified>) ...)
       (let ((temp init) ...)
         (set! var temp)
         ...)
       (let () body1 body2 ...)))
    ((_
       (x y ...)
       (temp ...)
       ((var init) ...)
       body1 body2 ...)
     (letrec-helper
       (y ...)
       (newtemp temp ...)
       ((var init) ...)
       body1 body2 ...))))

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
