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

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

; there's probably a shorter way to do this, but I'm too lazy to work it out :)
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

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
