(library (petit-cloud v0)
  (export define - * / + < <= = => > >= abs acos and angle append
          apply asin assoc assq assv atan begin boolean? boolean=? bytevector
          bytevector? bytevector-copy bytevector-length bytevector-u8-ref caaaar
          caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar
          cadddr caddr cadr call/cc call-with-current-continuation
          call-with-values car case case-lambda cdaaar cdaadr cdaar cdadar
          cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr
          ceiling char? char<? char<=? char=? char>? char>=? char->integer
          char-alphabetic? char-ci<? char-ci<=? char-ci=? char-ci>? char-ci>=?
          char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
          char-upper-case? char-whitespace? complex? cond cons cos denominator
          do dynamic-wind else eof-object eof-object? eq? equal? eqv? error
          even? exact exact? exact-integer-sqrt exp expt finite? floor for-each
          gcd guard if imag-part inexact inexact? infinite? integer?
          integer->char lambda lcm length let let* let*-values letrec letrec*
          let-values list list? list->string list-copy list-ref list-tail log
          magnitude make-bytevector make-list make-parameter make-polar
          make-rectangular make-string map max member memq memv min modulo nan?
          negative? not null? number? number->string numerator odd? or pair?
          parameterize positive? procedure? quasiquote quote quotient raise
          raise-continuable rational? rationalize real? real-part remainder
          reverse round sin sqrt string string? string<? string<=? string=?
          string>? string>=? string->list string->number string->utf8
          string-append string-ci<? string-ci<=? string-ci=? string-ci>?
          string-ci>=? string-copy string-downcase string-foldcase
          string-for-each string-length string-ref string-upcase substring
          symbol? symbol=? symbol->string tan truncate unless unquote
          unquote-splicing utf8->string values when with-exception-handler zero?

          ;; www
          www-form-urlencoded-read

          ;; TODO: enable foundationdb
          
          ;; call-with-transaction
          ;; db-set! db-bytes db-clear! db-query

          ;; bytevector         
          subbytevector
          bytevector-append

          ;; byter
          byter-next-prefix
          byter-pack
          byter-unpack

          ;; html
          html-read
          html-write

          ;; json
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write
          jsonify

          ;; debug
          pk)

  (import (chezscheme)
          (letloop bytevector)
          (letloop foundationdb entangle)
          (letloop www)
          (letloop html)
          (letloop json))

  (define pk
    (lambda args
      (when (getenv "LETLOOP_DEBUG")
        (display ";; " (current-error-port))
        (write args (current-error-port))
        (newline (current-error-port)))
      (car (reverse args)))))

  
