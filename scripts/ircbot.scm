(define args (command-line))
(define host (car args))
(define port (cadr args))
(define nick (caddr args))

(display (string-append "host: " host ", port: " port ", nick: " nick))

; open-tcp-stream-socket host-name service [buffer-size [line-translation]]
(define irc-connection (open-tcp-stream-socket host port))

(let ((out (new java.io.PrintWriter (. irc-connection getOutputStream))))
    (. out println (string-append "USER " nick))
    (. out flush))

;(set-current-input-port! irc-connection)
;(set-current-output-port! irc-connection)