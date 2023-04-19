(library (example)
  (export main)
  (import (petit-cloud base))

  (define counter
    (lambda ()
      (guard (ex (else (raise ex) 0))
        (call-with-database-transaction
         (lambda (tx)
           (let* ((counter (database-query tx (bytevector 42)))
                  (counter (+ (or (and counter
                                       (byter-unpack counter))
                                  0)
                              1)))
             (database-set! tx (bytevector 42) (byter-pack counter))
             counter))))))
  
  (define main
    (lambda (write method path query fragment headers body)
      #;(pk 'main method path)
      (write 200
             (list (cons 'content-type "text/plain"))
             (string->utf8
              (string-append "hello world "
                             (number->string (counter))
                             " from example application\n"))))))
