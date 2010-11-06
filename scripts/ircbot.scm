(define args (command-line))
(define host (car args))
(define port (cadr args))
(define nick (caddr args))

(define-syntax format-values
    (syntax-rules ()
        ((_ arg)
            (string-append (symbol->string (quote arg)) ": " arg))
        ((_ arg ...)
            (string-join (list (format-values arg) ...) ", "))))

;(display (format-values host port nick))
(define undef (if #f #f))

(define (send-commands port commands)
  (if (null? commands) undef
    (begin
      (write-string port (string-join (car commands) " "))
      (send-commands port (cdr commands)))))

; open-tcp-stream-socket host-name service [buffer-size [line-translation]]
(define irc-connection (open-tcp-stream-socket host port))

(begin
    (send-commands irc-connection
       `(("NICK" ,nick)
         ("USER" ,nick "0" "*" ":Oysta Moysta")
         ("JOIN" "#test")
         ("PRIVMSG" "#test" ":Hi, folks!")))
    (letrec ((read-loop (lambda () 
                                 (let ((string-in (read-string irc-connection)))
                                  (if string-in
                                    (display string-in)))
                                 (read-loop)))) (read-loop)))

(sleep 10000)

;(set-current-input-port! irc-connection)
;(set-current-output-port! irc-connection)
