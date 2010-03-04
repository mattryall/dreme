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
(define (not x) (if x #f #t))
(define (length ls)
    (letrec ((loop (lambda (ls n)
        (if (null? ls)
            n
            (loop (cdr ls) (+ n 1))))))
        (loop ls 0)))

(define (list? x)
    (letrec ((race
              (lambda (h t)
                (if (pair? h)
                    (let ((h (cdr h)))
                      (if (pair? h)
                          (and (not (eq? h t))
                               (race (cdr h) (cdr t)))
                          (null? h)))
                    (null? h)))))
      (race x x)))

(define-syntax rec
  (syntax-rules ()
    ((_ x e) (letrec ((x e)) x))))

(define-syntax let
  (syntax-rules ()
    ((_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...))
    ((_ f ((x e) ...) b1 b2 ...)
     ((rec f (lambda (x ...) b1 b2 ...)) e ...))))

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

(define-syntax quasiquote
    (syntax-rules (unquote)
        ((_ ((unquote e1) e2 ...)) ; handle unquote
            (cons e1 (quasiquote (e2 ...))))
        ((_ ((e1 e2 ...) e3 ...))  ; interpolate nested lists
            (cons (quasiquote (e1 e2 ...)) (quasiquote (e3 ...))))
        ((_ (e1 e2 ...))           ; normal processing
            (cons (quote e1) (quasiquote (e2 ...))))
        ((_ ())                    ; end condition - empty list
            (quote ()))))

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

(define call/cc call-with-current-continuation)

; needs call/cc
(define (member x ls)
    (call/cc
        (lambda (break)
            (letrec ((loop (lambda (ls)
                (if (null? ls)
                    #f
                    (if (eqv? x (car ls))
                        (break ls)
                        (loop (cdr ls)))))))
                (loop ls)))))

(define append
    (lambda args
        (let f ((ls (quote ())) (args args))
            (if (null? args)
                ls
                (let g ((ls ls))
                    (if (null? ls)
                        (f (car args) (cdr args))
                        (cons (car ls) (g (cdr ls)))))))))

(define (reverse lis)
   (if (null? lis)
       '()
       (append (reverse (cdr lis))
               (list (car lis)))))

(define newline
    (lambda args (display "\n"))) ; just ignore the port for the moment

; Delayed evaluation (RV5R)
(define-syntax delay
    (syntax-rules ()
	((_ exp) (make-promise (lambda () exp)))))

(define (make-promise p)
	(let ((val #f) (set? #f))
	    (lambda ()
		(if (not set?)
		    (let ((x (p)))
			(if (not set?)
			    (begin (set! val x)
				   (set! set? #t)))))
		val)))

(define (force promise) (promise))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))
