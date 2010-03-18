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

(display (format-values host port nick))
(define undef (if #f #f))

(define (send-commands connection-out connection-in commands)
  (if (null? commands) undef
    (begin
      (. connection-out println (string-join (car commands) " "))
      (. connection-out flush)
      (send-commands connection-out connection-in (cdr commands)))))

; open-tcp-stream-socket host-name service [buffer-size [line-translation]]
(define irc-connection (open-tcp-stream-socket host port))

(let ((out (new java.io.PrintWriter (. irc-connection getOutputStream)))
      (in (new java.io.BufferedReader (new java.io.InputStreamReader (. irc-connection getInputStream)))))
  (begin
    (send-commands out in
       `(("NICK" ,nick)
         ("USER" ,nick "0" "*" ":Oysta Moysta")
         ("JOIN" "#test")
         ("PRIVMSG" "#test" ":Hi, folks!")))
    (display (read-string in))))

(sleep 10000)

;(set-current-input-port! irc-connection)
;(set-current-output-port! irc-connection)
