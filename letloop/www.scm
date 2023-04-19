(library (letloop www)
  (export www-form-urlencoded-read
          www-host-read
          www-query-read
          www-uri-read
          ~check-letloop-www-000
          ~check-letloop-www-001
          )
  (import (chezscheme) (letloop match))

  (define pk
    (lambda args
      (display ";; ")
      (write args)
      (newline)
      (car (reverse args))))

  (define string->list*
    (lambda (x)
      (if x (string->list x) '())))
  
  (define percent-decode
    (lambda (string)
      (let loop ((chars (string->list* string))
                 (out '()))
        (match chars
          (() (list->string (reverse out)))
          ((#\+ ,rest ...)
           (loop rest (cons #\space out)))
          ((#\% ,a ,b ,rest ...)
           (loop rest (cons
                       (integer->char
                        (string->number
                         (list->string (list a b))
                         16))
                       out)))
          ((,char . ,rest) (loop rest (cons char out)))))))

  (define www-form-urlencoded-read
    ;; content-type: application/x-www-form-urlencoded
    (lambda (string)

      (define form-item-split
        (lambda (string)
          (let loop ((chars (string->list* string))
                     (out '()))
            (match chars
              (() (list (string->symbol (list->string (reverse out)))))
              ((#\= . ,rest) (cons (string->symbol (percent-decode (list->string (reverse out))))
                                   (percent-decode (list->string rest))))
              ((,char . ,rest) (loop rest (cons char out)))))))

      (let loop ((chars (string->list* string))
                 (out '(())))
        (match chars
          (() (reverse (cons (form-item-split (list->string (reverse (car out)))) (cdr out))))
          ((#\& . ,rest) (loop (cdr chars)
                               (cons* (list)
                                      (form-item-split (list->string (reverse (car out))))
                                      (cdr out))))
          ((#\; . ,rest) (loop (cdr chars)
                               (cons* (list)
                                      (form-item-split (list->string (reverse (car out))))
                                      (cdr out))))
          ((,char . ,rest) (loop (cdr chars) (cons (cons char (car out)) (cdr out))))))))

  (define www-request-line-uri-split
    (lambda (string)
      (let loop ((chars (string->list* string))
                 (out '()))
        (match chars
          (() (reverse out))
          ((#\/ . ,rest) (loop rest (cons* (list)
                                           (percent-decode
                                            (list->string
                                             (reverse (car out))))
                                           (cdr out))))
          ((,char . ,rest) (loop rest (cons (cons char (car out))
                                            (cdr out))))))))

  (define www-query-read www-form-urlencoded-read)

  (define string-find
    (lambda (string char)
      (let loop ((chars (string->list* string))
                 (index 0))
        (if (null? chars)
            #f
            (if (char=? char (car chars))
                index
                (loop (cdr chars) (fx+ index 1)))))))

  (define www-uri-read
    (lambda (string)

      (define path-split
        (lambda (string)
          (when (and (not (string=? string "")) (char=? #\/ (string-ref string 0)))
            (set! string (substring string 1 (string-length string))))

          (when (and (not (string=? string "")) (char=? #\/ (string-ref string (fx- (string-length string) 1))))
            (set! string (substring string 0 (fx- (string-length string) 1))))

          (if (string=? "" string)
              '()
              (let loop ((chars (string->list* string))
                         (out '(())))
                (match chars
                  (() (reverse (cons (percent-decode (list->string (reverse (car out))))
                                     (cdr out))))
                  ((#\/ . ,rest) (loop rest (cons* '()
                                                   (percent-decode (list->string (reverse (car out))))
                                                   (cdr out))))
                  ((,char . ,rest) (loop rest (cons (cons char (car out))
                                                    (cdr out)))))))))

      (define path #f)
      (define query #f)
      (define fragment #f)

      (let ((index (string-find string #\#)))
        (when index
          (set! fragment (substring string (fx+ index 1) (string-length string)))
          (set! string (substring string 0 index))))

      (let ((index (string-find string #\?)))
        (when index
          (set! query (substring string (fx+ index 1) (string-length string)))
          (set! string (substring string 0 index))))

      (set! path string)

      (values (and path (path-split path)) (and query (www-query-read query)) fragment)))

  (define www-host-read
    (lambda (string)
      (define port #f)
      
      (define index (string-find string #\:))

      (when (and index (not (= index (string-length string))))
        (set! port (string->number
                    (substring string (+ index 1)
                               (string-length string)))))
      
      (when index
        (set! string (substring string 0 index)))
      
      (let loop ((chars (string->list* string))
                 (out '(())))
        (match chars
          (()  (cons (reverse (cons (list->string
                                     (reverse (car out)))
                                    (cdr out)))
                     port))
          ((#\. . ,rest) (loop rest
                               (cons* '()
                                      (list->string (reverse (car out)))
                                      (cdr out))))
          ((,char . ,rest) (loop rest (cons (cons char (car out))
                                            (cdr out))))))))

  (define ~check-letloop-www-000
    (lambda ()
      (call-with-values (lambda ()
                          (www-uri-read
                           "/foo/b%33r/baz/?q=peace&s=1&now#README"))
        (lambda uri
          (assert (equal? uri  '(("foo" "b3r" "baz")
                                 ((q . "peace")
                                  (s . "1")
                                  (now))
                                 "README")))))))

  (define ~check-letloop-www-001
    (lambda ()
      (assert (equal? (cons '("foo" "bar" "baz" "qux" "example") 9999)
                      (www-host-read "foo.bar.baz.qux.example:9999")))))
  

  )
