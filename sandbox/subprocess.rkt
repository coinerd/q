#lang racket

;; sandbox/subprocess.rkt — subprocess management under custodians
;;
;; Provides a reusable subprocess runner with resource limits and
;; custodian-based cleanup. Every subprocess runs under its own custodian
;; so that timeout or kill cleans up all associated resources.

(require racket/port
         racket/string
         "limits.rkt")

(provide (struct-out subprocess-result)
         run-subprocess
         kill-subprocess!
         default-timeout-seconds
         default-max-output-bytes)

;; --------------------------------------------------
;; Result struct
;; --------------------------------------------------

(struct subprocess-result
  (exit-code    ; integer
   stdout       ; string
   stderr       ; string
   timed-out?   ; boolean
   elapsed-ms)  ; number
  #:transparent)

;; --------------------------------------------------
;; Bounded port reader — reads incrementally with a byte budget
;; --------------------------------------------------

(define (read-port-bounded p max-bytes)
  (if (or (not p) (port-closed? p))
      ""
      (let loop ([acc (open-output-bytes)] [remaining max-bytes])
        (define buf-size (min 4096 remaining))
        (if (<= remaining 0)
            ;; Output hit the byte budget — drain rest and mark truncated
            (begin
              (with-handlers ([exn:fail? void])
                (copy-port p (open-output-bytes))) ; discard remainder
              (string-append
               (get-output-string acc)
               (format "\n[output truncated at ~a bytes]" max-bytes)))
            (let ([bs (read-bytes buf-size p)])
              (cond
                [(eof-object? bs) (get-output-string acc)]
                [else
                 (write-bytes bs acc)
                 (loop acc (- remaining (bytes-length bs)))]))))))

;; --------------------------------------------------
;; Kill helper
;; --------------------------------------------------

(define (kill-subprocess! custodian)
  (custodian-shutdown-all custodian))

;; --------------------------------------------------
;; Shell quoting
;; --------------------------------------------------

(define (shell-quote s)
  (format "'~a'" (string-replace s "'" "'\\''")))

;; --------------------------------------------------
;; Resolve command to executable path
;; --------------------------------------------------

(define (resolve-command cmd)
  (if (absolute-path? cmd)
      cmd
      (find-executable-path cmd)))

;; --------------------------------------------------
;; Main subprocess runner
;; --------------------------------------------------

(define (run-subprocess command
                         #:args [args '()]
                         #:limits [limits (default-exec-limits)]
                         #:timeout [timeout-secs #f]
                         #:directory [dir (current-directory)]
                         #:environment [env (current-environment-variables)]
                         #:encoding [encoding 'utf-8])
  (define effective-timeout (or timeout-secs
                                 (exec-limits-timeout-seconds limits)))
  (define max-output (exec-limits-max-output-bytes limits))

  (define cust (make-custodian (current-custodian)))
  (define start-ms (current-inexact-milliseconds))

  (with-handlers
      ([exn:fail?
        (lambda (e)
          (custodian-shutdown-all cust)
          (subprocess-result
           -1
           ""
           (format "Failed to execute: ~a" (exn-message e))
           #f
           (inexact->exact (round (- (current-inexact-milliseconds) start-ms)))))])

    (define cmd-path (resolve-command command))
    (unless cmd-path
      (error 'run-subprocess "command not found: ~a" command))

    ;; Build shell command line when args are provided
    (define cmd-string (if (path? cmd-path) (path->string cmd-path) cmd-path))

    (define-values (sp stdout-in stdin-out stderr-in)
      (parameterize ([current-custodian cust]
                     [current-directory dir]
                     [current-environment-variables env])
        (if (null? args)
            ;; No args: run command directly
            (subprocess #f #f #f cmd-path)
            ;; With args: use /bin/sh -c to handle them
            (let ([shell-cmd (format "exec ~a ~a"
                                     (shell-quote cmd-string)
                                     (string-join (map shell-quote args) " "))])
              (subprocess #f #f #f
                          "/bin/sh" "-c" shell-cmd)))))

    ;; Close stdin immediately
    (close-output-port stdin-out)

    ;; Wait with timeout
    (define evt-result (sync/timeout effective-timeout sp))

    (cond
      ;; Timeout
      [(not evt-result)
       ;; Kill the subprocess explicitly before shutting custodian
       (with-handlers ([exn:fail? void])
         (subprocess-kill sp))
       (define end-ms (current-inexact-milliseconds))
       (custodian-shutdown-all cust)
       (subprocess-result
        -1
        ""
        (format "Timed out after ~a seconds" effective-timeout)
        #t
        (inexact->exact (round (- end-ms start-ms))))]

      ;; Completed
      [else
       (define out-str (read-port-bounded stdout-in max-output))
       (define err-str (read-port-bounded stderr-in max-output))
       (define exit-code (subprocess-status sp))

       (with-handlers ([exn:fail? void])
         (close-input-port stdout-in))
       (with-handlers ([exn:fail? void])
         (close-input-port stderr-in))
       (custodian-shutdown-all cust)

       (define end-ms (current-inexact-milliseconds))
       (subprocess-result
        exit-code
        out-str
        err-str
        #f
        (inexact->exact (round (- end-ms start-ms))))])))
