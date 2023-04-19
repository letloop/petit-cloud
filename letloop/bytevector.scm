(library (letloop bytevector)
  (export bytevector-little-endian->integer
          integer->bytevector-little-endian
          bytevector-random
          bytevector-append
          subbytevector)
  (import (chezscheme))

  (define pk
    (lambda args
      (write args)(newline)
      (car (reverse args))))

  (define subbytevector
    (case-lambda
     ((bv start end)
      (when (not (bytevector? bv))
        (void)
        (pk bv (vector? bv)))
      (assert (bytevector? bv))
      (unless (<= 0 start end (bytevector-length bv))
        (error 'subbytevector "Invalid indices" bv start end))
      (if (and (fxzero? start)
               (fx=? end (bytevector-length bv)))
          bv
          (let ((ret (make-bytevector (fx- end start))))
            (bytevector-copy! bv start
                              ret 0 (fx- end start))
            ret)))
     ((bv start)
      (subbytevector bv start (bytevector-length bv)))))
  
  (define (bytevector-append . bvs)
    (let* ((total (apply fx+ (map bytevector-length bvs)))
           (out (make-bytevector total)))
      (let loop ((bvs bvs)
                 (index 0))
        (unless (null? bvs)
          (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
          (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
      out))

  (define bytevector-random
    (lambda (length)
      (define out (make-bytevector length))
      (let loop ((index length))
        (unless (fxzero? index)
          (let ((index (fx- index 1)))
            (bytevector-u8-set! out index (random 256))
            (loop index))))
      out))

  (define bytevector-little-endian->integer
    (lambda (bytevector)

      (define bits-shift
        (lambda (padding index)
          (fx* (fx- (fx- length padding) (fx- index padding) 1)
               8)))

      (define length (bytevector-length bytevector))

      (let loop ((index 0)
                 (padding? #t)
                 (padding 0)
                 (out 0))
        (if (fx=? index length)
            out
            (let ((byte (bytevector-u8-ref bytevector index)))
              (if (and padding? (fxzero? byte))
                  (loop (fx+ index 1)
                        #t
                        (fx+ index 1)
                        out)
                  (loop (fx+ index 1)
                        #f
                        padding
                        (+ out
                           (ash byte
                                (bits-shift padding index))))))))))

  (define integer->bytevector-little-endian
    (lambda (integer n)

      (define make-padding
        (lambda (n)
          (map (lambda _ 0) (iota n))))

      (assert (integer? integer))

      (let loop ((input (reverse
                         (string->list
                          (format #f "~x" integer))))
                 (length 0)
                 (out '()))
        (cond
         ((null? input)
          (when (fx>? length n)
            (error 'letloop "integer is too large for n" '(letloop bytevector) integer->bytevector-little-endian integer n))
          (u8-list->bytevector
           (append (make-padding (fx- n length))
                   out)))
         ((null? (cdr input))
          (loop (cdr input)
                (fx+ length 1)
                (cons (string->number
                       (list->string
                        (list (car input)))
                       16)
                      out)))
         (else (loop (cddr input)
                     (fx+ length 1)
                     (cons (string->number
                            (list->string
                             (list (cadr input) (car input)))
                            16)
                           out)))))))
  )
