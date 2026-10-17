#lang racket

;; @speed slow
;; @suite default
;; @boundary integration
;; @isolation temp-dir
;; @covers sandbox/subprocess.rkt

(require rackunit
         rackunit/text-ui
         "../sandbox/limits.rkt"
         "../sandbox/subprocess.rkt")

(define setsid-path (find-executable-path "setsid"))
(define linux? (directory-exists? "/proc/self"))

(define (dead? pid)
  ;; Reaping can race the observation; only a missing entry is evidence of
  ;; death, not an arbitrary read failure.
  (with-handlers ([exn:fail:filesystem:errno?
                   (lambda (e) (and (memv (car (exn:fail:filesystem:errno-errno e)) '(2 3)) #t))]
                  [exn:fail? (lambda (_) #f)])
    (regexp-match? #rx"\\) [ZX] " (file->string (format "/proc/~a/stat" pid)))))

(define (wait-until pred seconds)
  (define deadline (+ (current-inexact-monotonic-milliseconds) (* seconds 1000)))
  (let loop ()
    (cond [(pred) #t]
          [(>= (current-inexact-monotonic-milliseconds) deadline) #f]
          [else (sleep 0.01) (loop)])))

;; The command obeys TERM; its redirected descendant deliberately does not.
;; The descendant publishes readiness only AFTER installing its TERM handler.
(define fixture-script
  (string-append
   "trap 'exit 23' TERM\n"
   "echo $$ > \"$1/command.pid\"\n"
   "if IFS= read -r parent < /proc/$PPID/comm && [ \"$parent\" = sh ]; then echo $PPID > \"$1/owner.pid\"; fi\n"
   "/bin/sh -c 'trap \"\" TERM; echo $$ > \"$1/descendant.pid\"; while :; do sleep 1; done' child \"$1\" >/dev/null 2>&1 &\n"
   "while [ ! -s \"$1/descendant.pid\" ]; do sleep 0.01; done\n"
   "printf 'before-timeout\\n'; printf 'command-stderr\\n' >&2\n"
   ;; On the old direct-launch implementation PPID is the Racket test runner!
   ;; Fail safely there instead of stopping the entire regression harness.
   "if [ \"${2-}\" = stop-owner ]; then [ -s \"$1/owner.pid\" ] || exit 97; kill -STOP \"$PPID\"; fi\n"
   "if [ \"${2-}\" = kill-owner ]; then [ -s \"$1/owner.pid\" ] || exit 97; kill -KILL \"$PPID\"; fi\n"
   "if [ \"${2-}\" = finish-command ]; then exit 0; fi\n"
   "while :; do sleep 1; done\n"))

(define (with-fixture proc)
  (define root (make-temporary-file "subprocess-owner-~a" 'directory))
  (dynamic-wind
   void
   (lambda () (proc root))
   (lambda ()
     ;; Only failure cleanup; never signal a numeric group, even in tests.
     (for ([name '("command.pid" "descendant.pid" "owner.pid")])
       (define path (build-path root name))
       (when (file-exists? path)
         (define pid (string->number (string-trim (file->string path))))
         (when (and pid (not (dead? pid)))
           (system* (find-executable-path "kill") "-KILL" (number->string pid)))))
     (delete-directory/files root))))

(define (check-fixture-gone root)
  (for ([name '("command.pid" "descendant.pid" "owner.pid")])
    (define path (build-path root name))
    (check-true (file-exists? path) (format "fixture reached ~a" name))
    (when (file-exists? path)
      (define pid (string->number (string-trim (file->string path))))
      (check-true (dead? pid)
                  (format "~a (~a) must not survive cleanup" name pid)))))

(define ownership-tests
  (test-suite
   "Subprocess group ownership"
   (test-case "TERM-exiting command cannot release ownership before descendant cleanup"
     (when (and setsid-path linux?)
       (with-fixture
        (lambda (root)
          ;; Observe parent-side executions, not commands run inside the owner.
          ;; The former implementation launches shells to signal -PGID, including
          ;; an unconditional KILL AFTER its grace wait has reaped the leader.
          ;; No such numeric signalling helper may run, before OR after reap.
          (define executions '())
          (define guard
            (make-security-guard
             (current-security-guard)
             (lambda (_who path modes)
               (when (memq 'execute modes) (set! executions (cons path executions))))
             (lambda args (void))))
          (define result
            (parameterize ([current-security-guard guard])
              (run-subprocess "/bin/sh" #:args (list "-c" fixture-script "fixture" (path->string root))
                              #:process-group? #t #:timeout 2)))
          (check-true (subprocess-result-timed-out? result))
          (check-equal? (subprocess-result-exit-code result) -9)
          (check-equal? (subprocess-result-stdout result) "before-timeout\n")
          (check-regexp-match #rx"command-stderr" (subprocess-result-stderr result))
          (check-true (< (subprocess-result-elapsed-ms result) 8000))
          (check-true (pair? executions) "security guard observed process launch")
          (for ([path executions])
            (check-equal? (simplify-path path) (simplify-path setsid-path)
                          "only the owner is launched: no external -PGID signal helper"))
          (check-fixture-gone root)))))
   (test-case "stopped supervisor cannot strand its redirected TERM-ignoring descendant"
     (when (and setsid-path linux?)
       (with-fixture
        (lambda (root)
          (define result
            (run-subprocess "/bin/sh"
                            #:args (list "-c" fixture-script "fixture" (path->string root) "stop-owner")
                            #:process-group? #t #:timeout 2))
          (check-true (subprocess-result-timed-out? result))
          (check-equal? (subprocess-result-exit-code result) -9)
          (check-true (< (subprocess-result-elapsed-ms result) 8000))
          (check-fixture-gone root)))))
   (test-case "lost supervisor is not cleanup confirmation for a TERM-ignoring child"
     (when (and setsid-path linux?)
       (with-fixture
        (lambda (root)
          (define result
            (run-subprocess "/bin/sh"
                            #:args (list "-c" fixture-script "fixture" (path->string root) "kill-owner")
                            #:process-group? #t #:timeout 10))
          (check-false (subprocess-result-timed-out? result))
          (check-not-equal? (subprocess-result-exit-code result) 0)
          (check-not-equal? (subprocess-result-exit-code result) -1)
          (check-equal? (subprocess-result-stdout result) "before-timeout\n")
          (check-regexp-match #rx"command-stderr" (subprocess-result-stderr result))
          (check-true (< (subprocess-result-elapsed-ms result) 8000))
          ;; No test-side grace period: the old path returned after merely
          ;; closing the control pipe, while the watcher was still in TERM
          ;; grace and the redirected descendant was alive.
          (check-fixture-gone root)))))
   (test-case "unreadable confirmation fails honestly on completion, timeout and break"
     (when (and setsid-path linux?)
       (for ([mode '(completion timeout break)])
         (with-fixture
          (lambda (root)
            (define denied? #f)
            (define guard
              (make-security-guard
               (current-security-guard)
               (lambda (_who path modes)
                 ;; Do not interfere with spawn or the private handshake.
                 ;; Denial is NOT proof of watcher death, even if it really
                 ;; did die. Exercise group-scan refusal separately: a dead
                 ;; watcher without an observable group is also insufficient.
                 (when (and path (memq 'read modes)
                            (if (eq? mode 'timeout)
                                (equal? (path->string path) "/proc")
                                (regexp-match? #rx"^/proc/[0-9]+/stat$" (path->string path))))
                   (set! denied? #t)
                   (error 'confirmation-test "denied /proc observation")))
               (lambda args (void))))
            (define result-box (box #f))
            (define worker
              (thread
               (lambda ()
                 (with-handlers ([exn? (lambda (e) (set-box! result-box e))])
                   (set-box! result-box
                             (parameterize ([current-security-guard guard])
                               (run-subprocess
                                "/bin/sh"
                                #:args (list "-c" fixture-script "fixture" (path->string root)
                                             (if (eq? mode 'completion) "finish-command" ""))
                                #:process-group? #t #:timeout (if (eq? mode 'timeout) 1 600))))))))
            (dynamic-wind
             void
             (lambda ()
               (when (eq? mode 'break)
                 (check-true (wait-until (lambda () (file-exists? (build-path root "descendant.pid"))) 5))
                 (break-thread worker))
               (check-not-false (sync/timeout 6 worker) "failed confirmation is bounded")
               (check-true denied?)
               (define result (unbox result-box))
               (cond
                 [(eq? mode 'break)
                  (check-pred exn:fail? result "do not silently re-raise break after unconfirmed cleanup")
                  (when (exn? result)
                    (check-regexp-match #rx"cleanup unconfirmed" (exn-message result)))]
                 [else
                  (check-pred subprocess-result? result)
                  (when (subprocess-result? result)
                    (check-equal? (subprocess-result-exit-code result) -1)
                    (check-false (subprocess-result-timed-out? result))
                    (check-regexp-match #rx"cleanup unconfirmed" (subprocess-result-stderr result)))])
               (check-fixture-gone root))
             (lambda ()
               (unless (thread-dead? worker)
                 (break-thread worker)
                 (sync/timeout 5 worker)))))))))
   (test-case "outer cancellation cleans command and TERM-ignoring descendant"
     (when (and setsid-path linux?)
       (with-fixture
        (lambda (root)
          (define result (box #f))
          (define worker
            (thread
             (lambda ()
               (with-handlers ([exn:break? (lambda (e) (set-box! result e))])
                 (set-box! result
                           (run-subprocess "/bin/sh"
                                           #:args (list "-c" fixture-script "fixture" (path->string root))
                                           #:process-group? #t #:timeout 600))))))
          (dynamic-wind
           void
           (lambda ()
             (check-true (wait-until (lambda () (file-exists? (build-path root "descendant.pid"))) 5))
             (break-thread worker)
             (check-not-false (sync/timeout 5 worker) "cancellation is bounded")
             (check-pred exn:break? (unbox result))
             (check-fixture-gone root))
           (lambda ()
             (unless (thread-dead? worker)
               (break-thread worker)
               (sync/timeout 5 worker))))))))
   (test-case "group owner preserves actual command status, arguments and both streams"
     (when setsid-path
       (for ([code '(0 7 143)])
         (define result
           (run-subprocess "/bin/sh"
                           #:args (list "-c" "printf '%s' \"$1\"; printf 'err' >&2; exit \"$2\""
                                        "fixture" "literal '$value; spaces" (number->string code))
                           #:process-group? #t #:timeout 5))
         (check-false (subprocess-result-timed-out? result))
         (check-equal? (subprocess-result-exit-code result) code)
         (check-equal? (subprocess-result-stdout result) "literal '$value; spaces")
         (check-equal? (subprocess-result-stderr result) "err"))))
   (test-case "confirmed group cleanup preserves exact output budgets and truncation"
     (when (and setsid-path linux?)
       (for ([size '(512 513)])
         (define result
           (run-subprocess
            "/bin/sh"
            #:args (list "-c" "head -c \"$1\" /dev/zero; head -c \"$1\" /dev/zero >&2; exit 7"
                         "fixture" (number->string size))
            #:limits (exec-limits 5 512 536870912 10)
            #:process-group? #t))
         (check-equal? (subprocess-result-exit-code result) 7)
         (check-equal? (string-length (subprocess-result-stdout result)) 512)
         (check-equal? (string-length (subprocess-result-stderr result)) 512)
         (check-equal? (subprocess-result-truncated? result) (> size 512)))))
   (test-case "strict UTF-8 survives group status transport without accepting invalid bytes"
     (when setsid-path
       (define valid
         (run-subprocess "/bin/sh" #:args '("-c" "printf '\\357\\277\\275'")
                         #:encoding 'utf-8/strict #:process-group? #t #:timeout 5))
       (check-equal? (subprocess-result-exit-code valid) 0)
       (check-equal? (subprocess-result-stdout valid) "\uFFFD")
       (define invalid
         (run-subprocess "/bin/sh" #:args '("-c" "printf '\\377'")
                         #:encoding 'utf-8/strict #:process-group? #t #:timeout 5))
       (check-equal? (subprocess-result-exit-code invalid) -1)
       (check-regexp-match #rx"Failed to execute" (subprocess-result-stderr invalid))))))

(module+ test
  (define failures (run-tests ownership-tests))
  (unless (zero? failures) (error 'subprocess-ownership "~a failures" failures)))
(module+ main
  (exit (run-tests ownership-tests)))
