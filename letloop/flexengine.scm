(library (letloop flexengine)
  (export make-flexengine)
  (import (chezscheme))

  (define make-flexengine
    (lambda (x max-duration)

      (define $oops
        (lambda (who message . args)
          (error who message args)))

      (define *exit*)
      (define *keybd* values)
      (define *timer*)

      (define cleanup
        (lambda (who)
          (keyboard-interrupt-handler *keybd*)
          (timer-interrupt-handler *timer*)
          (set! *keybd* values)
          (set! *exit* values)
          (set! *timer* values)))

      (define setup
        (lambda (exit)
          (set! *keybd* (keyboard-interrupt-handler))
          (keyboard-interrupt-handler (exception *keybd*))
          (set! *timer* (timer-interrupt-handler))
          (timer-interrupt-handler pause)
          (set! *exit* exit)))

      (define pause
        ;; disable engine and return the continuation
        (lambda ()
          (let ([exit *exit*])
            (cleanup 'engine-pause)
            (set-timer (call/cc (lambda (k) (exit (lambda () k))))))))

      (define return
        ;; disable engine and return list (ticks value ...)
        (lambda (args)
          (let ([n (set-timer 0)])
            (let ([exit *exit*])
              (cleanup 'engine-return)
              (exit (lambda () (cons n args)))))))

      (define exception
        ;; disable engine while calling the handler
        (lambda (handler)
          (lambda args
            (let ([ticks (set-timer 0)])
              (let ([exit *exit*])
                (cleanup 'engine-exception)
                (apply handler args)
                (setup exit)
                (if (= ticks 0) (pause) (set-timer ticks)))))))

      (define run-engine
        ;; run a continuation as an engine
        (lambda (k ticks)
          ((call/cc
            (lambda (exit)
              (set-timer 0)
              (setup exit)
              (k ticks))))))

      (define make
        ;; create an engine from a procedure or continuation
        (lambda (k)
          (lambda (ticks complete expire)
            (unless (and (fixnum? ticks) (not (negative? ticks)))
              ($oops 'engine "invalid ticks ~s" ticks))
            (unless (procedure? complete)
              ($oops 'engine "~s is not a procedure" complete))
            (unless (procedure? expire)
              ($oops 'engine "~s is not a procedure" expire))
            (if (= ticks 0)
                (expire (make k))
                (let ([x (run-engine k ticks)])
                  (if (procedure? x)
                      (expire (make x))
                      (apply complete x)))))))


      (define engine-return (lambda args (return args)))

      (define engine-pause (lambda () (pause)))

      (define start 0)
      (define total (make-time 'time-duration 0 0))

      (define wrap
        (lambda (thunk)
          (lambda ()
            (dynamic-wind
                (lambda () (set! start (current-time 'time-monotonic)))
                thunk
                (lambda ()
                  (set! total
                        (add-duration total
                                      (time-difference (current-time 'time-monotonic)
                                                       start)))
                  (when (time<=? (make-time 'time-duration 0 max-duration) total)
                    (raise (make-condition 'engine-timeout))))))))

      (define thunk (wrap x))

      (make (lambda (ticks)
              (with-exception-handler
               (lambda (c)
                 (let ([ticks (set-timer 0)])
                   (let ([exit *exit*])
                     (cleanup 'raise)
                     (exit (lambda () (raise c))))))
               (lambda ()
                 (set-timer ticks)
                 (call-with-values thunk (lambda args (return args))))))))))
