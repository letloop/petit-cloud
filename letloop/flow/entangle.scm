(library (letloop flow entangle)

  (export make-entangle
          entangle-abort
          entangle-run
          entangle-sleep
          entangle-spawn
          entangle-spawn-threadsafe
          entangle-stop
          #;entangle-tcp-connect
          entangle-tcp-serve
          )

  (import (chezscheme)
          (letloop r999)
          (letloop epoll)
          (letloop cffi))

  ;;
  ;; inspired from https://stackoverflow.com/a/51777980/140837
  ;;
  ;; single thread, single event-loop
  ;;

  (define libloop (load-shared-object #f))

  (define pk
    (lambda args
      (display ";; ")(write args)(newline)
      (flush-output-port)
      (car (reverse args))))

  (define entangle-current (make-parameter #f))

  (define prompt-current #f)

  (define EWOULDBLOCK 11)

  (define prompt-singleton '(prompt-singleton))

  (define-record-type* <entangle>
    (make-entangle-base running epoll events debug thunks others readable writable)
    entangle?
    (running entangle-running? entangle-running!)
    (epoll entangle-epoll)
    (events entangle-events)
    (debug entangle-debug)
    (thunks entangle-thunks entangle-thunks!)
    (others entangle-others entangle-others!)
    (readable entangle-readable)
    (writable entangle-writable))

  (define (call-with-prompt thunk handler)
    (call-with-values (lambda ()
                        (call/1cc
                         (lambda (k)
                           ;; XXX: The continuation K also called
                           ;; prompt-current may be called in THUNK during
                           ;; the extent of this lambda.
                           (set! prompt-current k)
                           (thunk))))
      (lambda out
        (cond
         ((and (pair? out) (eq? (car out) prompt-singleton))
          (apply handler (cdr out)))
         (else (apply values out))))))

  (define (entangle-abort . args)
    (call/cc
     (lambda (k)
       ;; XXX: Capture the continuation and call it later, hence
       ;; call/cc instead of call/1cc.
       (let ((prompt prompt-current))
         (set! prompt-current #f)
         (apply prompt (cons prompt-singleton (cons k args)))))))

  (define make-event cons)
  (define event-continuation car)
  (define event-mode cdr)

  (define entangle-apply
    (lambda (entangle thunk)
      (call-with-prompt
          (lambda ()
            (guard (ex (else (condition-message ex)
                             (condition-irritants ex)))
              (thunk)))
        (lambda (k handler)
          (handler k)))))

  (define hashtable-empty?
    (lambda (h)
      (fx=? (hashtable-size h) 0)))

  (define entangle-run-once
    (lambda (entangle)
      (for-each (lambda (thunk)
                  (entangle-apply entangle thunk))
                (entangle-thunks entangle))
      (entangle-thunks! entangle '())
      (unless (hashtable-empty? (entangle-events entangle))
        ;; Wait for ONE event... TODO: do more.
        (let* ((event (make-epoll-event))
               ;; TODO: increase max events from 1 to 1024, instead of -1
               ;; adjust timeout to the next scheduled event
               (count (epoll-wait (entangle-epoll (entangle-current)) event 1 -1)))
          (if (zero? count)
              (begin
                (foreign-free (ftype-pointer-address event)))
              (let* ((mode (if (epoll-event-in? event) 'read 'write))
                     (k (hashtable-ref (entangle-events entangle)
                                       (cons (epoll-event-fd event) mode)
                                       #f)))
                (foreign-free (ftype-pointer-address event))
                (when k
                  (hashtable-delete! (entangle-events entangle)
                                     (cons (epoll-event-fd event)
                                           mode))
                  ;; remove the associated event mode from epoll instance
                  (entangle-apply entangle k))))))))

  (define entangle-stats
    (lambda ()
      (define entangle (entangle-current))
      (list 'running (entangle-running? entangle)
            'events (hashtable-size (entangle-events entangle))
            'events (call-with-values (lambda ()
                                        (hashtable-entries (entangle-events entangle)))
                      (lambda args args))
            'debug (call-with-values
                       (lambda ()
                         (hashtable-entries (entangle-debug entangle)))
                      (lambda args args))
            'todos (length (entangle-thunks entangle))
            'others (length (unbox (entangle-others entangle))))))

  (define entangle-watcher
    (lambda ()
      (entangle-read (entangle-readable (entangle-current)))
      (let ((thunks
             (let* ((box (entangle-others (entangle-current)))
                    (old (unbox box))
                    (new '()))
               (let loop ()
                 (unless (box-cas! box old new)
                   (loop)))
               old)))
        (entangle-thunks! (entangle-current)
                          (append thunks
                                  (entangle-thunks
                                   (entangle-current)))))
      (entangle-watcher)))

  (define entangle-stop
    (lambda ()
      (entangle-running! (entangle-current) #f)))

  (define entangle-run
    (lambda ()
      (entangle-spawn entangle-watcher)
      (guard (ex
              (else (pk 'ooops2
                        (condition-message ex)
                        (condition-irritants ex))))

        (let loop ()
          (when (entangle-running? (entangle-current))
            (guard (ex (else (pk 'ooops
                                 (condition-message ex)
                                 (condition-irritants ex))))
              (entangle-run-once (entangle-current))))
          (loop)))))

  (define entangle-spawn
    (lambda (thunk)
      (entangle-thunks! (entangle-current)
                        (cons thunk (entangle-thunks (entangle-current))))))

  (define entangle-sleep)

  (define entangle-spawn-threadsafe
    (lambda (thunk)
      (let* ((box (entangle-others (entangle-current)))
             (old (unbox box))
             (new (cons thunk old)))
        (let loop ()
          (unless (box-cas! box old new)
            (loop))))
      (entangle-write (entangle-writable (entangle-current))
                      (bytevector 20 06))))

  (define fcntl!
    (let ((func (foreign-procedure "fcntl" (int int int) int)))
      (lambda (fd command value)
        (func fd command value))))

  (define fcntl
    (let ((func (foreign-procedure "fcntl" (int int) int)))
      (lambda (fd)
        (func fd F_GETFL))))

  (define-ftype <pipe>
    (array 2 int))

  (define F_GETFL 3)
  (define F_SETFL 4)
  (define O_NONBLOCK 2048)

  (define entangle-nonblock!
    (lambda (fd)
      (fcntl! fd F_SETFL
              (fxlogior O_NONBLOCK
                        (fcntl fd)))))

  (define make-pipe
    (let ((func (foreign-procedure "pipe" (void* int) int)))
      (lambda ()
        (define pointer (foreign-alloc (ftype-sizeof <pipe>)))
        (call/errno* (lambda () (func pointer 0))
          (lambda (out errno)
            (when (fx=? out -1)
              (error '(letloop entangle) (strerror errno) errno))))
        (let  ((pipe (make-ftype-pointer <pipe> pointer)))
          (values (ftype-ref <pipe> (0) pipe) ;; readable
                  (ftype-ref <pipe> (1) pipe)))))) ;; writable

  (define make-entangle
    (lambda ()
      (unless (entangle-current)
        (register-signal-handler 10
                                 (lambda (oof)
                                   (pk (entangle-stats))))

        (call-with-values make-pipe
          (lambda (readable writable)
            (entangle-nonblock! readable)
            (entangle-nonblock! writable)
            (let ((epoll (epoll-create1 0))
                  (events (make-hashtable equal-hash equal?)))
              (entangle-current (make-entangle-base #t
                                                    epoll
                                                    events
                                                    (make-hashtable equal-hash equal?)
                                                    '()
                                                    (box '())
                                                    readable
                                                    writable))))))
      (entangle-current)))

  (define entangle-socket
    (let ((entangle-socket
           (foreign-procedure "socket" (int int int) int)))
      (lambda (domain type protocol)
        (define socket (entangle-socket domain type protocol))
        (entangle-nonblock! socket)
        socket)))

  (define entangle-accept-base
    (let ((entangle-accept (foreign-procedure "accept4" (int void* void* int) int)))
      (lambda (fd)
        (entangle-accept fd 0 0 2048))))

  (define entangle-update-epoll
    (lambda (fd mode)
      (if (hashtable-ref (entangle-events (entangle-current))
                         (cons fd (if (eq? mode 'read) 'write 'read))
                         #f)
          (epoll-ctl (entangle-epoll (entangle-current))
                     3
                     fd
                     (make-epoll-event-out fd)))
          (epoll-ctl (entangle-epoll (entangle-current))
                     2
                     fd
                     (make-epoll-event-out fd))))

  (define entangle-accept
    (lambda (fd)

      (define accept-handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'read)
                          k)
          (hashtable-set! (entangle-debug (entangle-current))
                          (cons fd 'read)
                          'accept)
          (epoll-ctl (entangle-epoll (entangle-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call/errno* (lambda () (entangle-accept-base fd))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort accept-handler)
                      (entangle-update-epoll fd 'read)
                      (loop))
                    #f)
                out))))))

  (define entangle-close
    (let ((entangle-close (foreign-procedure "close" (int) int)))
      (lambda (fd)
        (entangle-close fd))))

  ;; taken from https://github.com/ecraven/chez-scheme-libraries/
  (define (setsockopt socket level optname optval)
    (define f (foreign-procedure "setsockopt" (int int int void* int) int))

    (define (bool opt-int)
      (let ((i (foreign-alloc (ftype-sizeof int)))
            (size (ftype-sizeof int)))
        (foreign-set! 'int i 0 (if optval 1 0))
        (call/errno* (lambda () (f socket level opt-int i size))
          (lambda (out errno)
            (if (zero? out)
                #t
                (error 'setsockopt "Error on setsockopt" errno))))))

    (case optname
      ;; based on /usr/include/asm-generic/socket.h
      ((socket-option/debug) (bool 1))
      ((socket-option/reuseaddr) (bool 2))
      ((socket-option/dontroute) (bool 5))
      ((socket-option/broadcast) (bool 6))
      ;;((socket-option/sndbuf) (int 7))
      ;;((socket-option/rcvbuf) (int 8))
      ((socket-option/keepalive) (bool 9))
      ((socket-option/oobinline) (bool 10))
      ((socket-option/reuseport) (bool 15))
      ;;((socket-option/rcvlowat) (int 18))
      ;;((socket-option/sndlowat) (int 19))
      (else (error 'setsockopt "Unknown socket option" socket level optname optval))))

  (define entangle-bind
    (let ((entangle-bind
           (foreign-procedure "bind" (int void* size_t) int)))
      (lambda (fd ip port)
        (setsockopt fd 1 'socket-option/reuseaddr #t)
        (setsockopt fd 1 'socket-option/reuseport #t)
        (call-with-sockaddr-in
         ip port
         (lambda (address)
           (entangle-bind fd
                          address
                          (ftype-sizeof %sockaddr-in)))))))

  (define-ftype %sockaddr-in
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (address (endian big unsigned-32))
            (padding (array 8 char))))

  (define string->ipv4
    (lambda (string)

      (define (ipv4 one two three four)
        (+ (* one 256 256 256)
           (* two 256 256)
           (* three 256)
           four))

      (define make-char-predicate
        (lambda (char)
          (lambda (other)
            (char=? char other))))

      ;; taken from https://cookbook.scheme.org/split-string/
      (define (string-split char-delimiter? string)
        (define (maybe-add a b parts)
          (if (= a b) parts (cons (substring string a b) parts)))
        (let ((n (string-length string)))
          (let loop ((a 0) (b 0) (parts '()))
            (if (< b n)
                (if (not (char-delimiter? (string-ref string b)))
                    (loop a (+ b 1) parts)
                    (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
                (reverse (maybe-add a b parts))))))

      (apply ipv4 (map string->number
                       (string-split (make-char-predicate #\.)
                                     string)))))

  (define (call-with-sockaddr-in address port proc)
    (let* ((ptr (foreign-alloc (ftype-sizeof %sockaddr-in)))
           (res (make-ftype-pointer %sockaddr-in ptr)))
      ;; create socket FAMILY=inet
      (ftype-set! %sockaddr-in (family) res 2)
      (ftype-set! %sockaddr-in (port) res port)
      (ftype-set! %sockaddr-in (address) res (string->ipv4 address))
      (call-with-values (lambda () (proc (ftype-pointer-address res)))
        (lambda args
          (foreign-free ptr)
          (apply values args)))))

  (define entangle-listen
    (let ((entangle-listen (foreign-procedure "listen" (int int) int)))
      (lambda (fd backlog)
        (entangle-listen fd backlog))))

  (define entangle-read-base
    (let ((entangle-read-foreign
           (foreign-procedure "read" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
          (entangle-read-foreign fd
                                 (bytevector-pointer bytevector)
                                 (bytevector-length bytevector))))))

  (define entangle-read
    (lambda (fd)
      (define bv (make-bytevector 1024))

      (define handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call/errno* (lambda () (entangle-read-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort handler)
                      (loop))
                    #f)
                (if (fx=? out 0)
                    #f
                    (subbytevector bv 0 out))))))))

  (define entangle-write-base
    (let ((entangle-write
           (foreign-procedure "write" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
          (entangle-write fd
                          (bytevector-pointer bytevector)
                          (bytevector-length bytevector))))))

  (define entangle-write
    (lambda (fd bv)
      (define handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'write)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     3 ;; EPOLL_CTL_MOD
                     fd
                     (make-epoll-event-out fd))))

      (let loop ()
        (call/errno* (lambda () (entangle-write-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort handler)
                      (loop))
                    #f)
                ;; TODO: continue writing until everything is written
                #t))))))

  (define subbytevector
    (case-lambda
     ((bv start end)
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

  (define entangle-tcp-serve
    (lambda (ip port)

      (define SOCKET-DOMAIN=AF-INET 2)
      (define SOCKET-TYPE=STREAM 1)
      (define fd (entangle-socket SOCKET-DOMAIN=AF-INET SOCKET-TYPE=STREAM 0))

      (define close (lambda () (entangle-close fd)))

      (define read
        (lambda ()
          (let ((client (entangle-accept fd)))
            (if client
                (values (lambda () (entangle-read client))
                        (lambda (bv) (entangle-write client bv))
                        (lambda () (entangle-close client)))
                (begin
                  (values #f #f #f))))))

      (entangle-bind fd ip port)
      (entangle-listen fd 128)

      (values read close)))
  )
