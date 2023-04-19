(import (chezscheme)
        (letloop match)
        (letloop blake3))


(define call-with-input-from-string
  (lambda (string proc)
    (define port (open-input-string string))
    (call-with-values (lambda () (proc port))
      (lambda args
        (close-port port)
        (apply values args)))))

(define read*
  (lambda (string)
    (call-with-input-from-string string read)))

(define pk
  (lambda args
    (when #t #;(getenv "LETLOOP_DEBUG")
      (display ";; " (current-error-port))
      (write args (current-error-port))
      (newline (current-error-port)))
    (car (reverse args))))

(define run
  (lambda (command input)
    (define p (process command))
    (define out (car p))
    (define in (cadr p))
    (write input in)
    (close-port in)
    (read out)))

(define readline
  (lambda ()
    (list->string
     (let loop ()
       (let ((object (read-char)))
         (if (eof-object? object)
             '()
             (if (char=? #\newline object)
                 '()
                 (cons object (loop)))))))))

(define secret
  (begin 
    (format #t "Please, input the secret: ")
    (let* ((secret (readline))
           (secret* (blake3 (string->utf8 secret))))
      secret*)))

(match (pk (cdr (command-line)))
  ((,target ,filepath)
   (let ((response (run
                    (format #f "curl --silent -X PUT ~a --data-binary @-"
                            target)
                    (list 'deploy secret (call-with-input-file filepath read)))))
     (case (pk (car response))
       ((ok) (format #t "Good! What is done, is not to be done!\n"))
       ((error)
        (format #t "There is an error in the application, please proof read: ~a"
                filepath)
        (exit 3))
       (else (exit 255))))))
