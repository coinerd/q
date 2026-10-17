#lang racket

;; @speed slow
;; @suite default
;; @boundary integration
;; @isolation temp-dir
;; @covers sandbox/subprocess.rkt extensions/gsd/verification-job.rkt

(require rackunit
         racket/runtime-path)

(define-runtime-path subprocess-module "../sandbox/subprocess.rkt")
(define-runtime-path verification-module "../extensions/gsd/verification-job.rkt")

(define (wait-until pred)
  (define deadline (+ (current-inexact-monotonic-milliseconds) 5000))
  (let loop ()
    (cond
      [(pred) #t]
      [(>= (current-inexact-monotonic-milliseconds) deadline)
       (error 'handshake-race "fixture did not reach scheduled boundary")]
      [else
       (sleep 0.001)
       (loop)])))

(define (dead? pid)
  (define path (format "/proc/~a/stat" pid))
  (or (not (file-exists? path)) (regexp-match? #rx"\\) [ZX] " (file->string path))))

;; A private setsid wrapper holds the REAL owner before it can publish. Load
;; both APIs in a fresh namespace so verification-job's cached setsid lookup
;; sees the wrapper too. No production test hook or timing-based race trigger.
(define (with-empty-read-then-exit api proc #:identity [identity-mode 'valid])
  (define real-setsid (find-executable-path "setsid"))
  (when (and real-setsid (directory-exists? "/proc/self"))
    (define root (make-temporary-file "owner-handshake-race-~a" 'directory))
    (define gate (build-path root "release"))
    (define pid-path (build-path root "owner.pid"))
    (define wrapper (build-path root "setsid"))
    (define parent-guard (current-security-guard))
    (define first-empty? #f)
    (define later-reads 0)
    (define owner-pid #f)
    (dynamic-wind
     void
     (lambda ()
       (unless (system* (find-executable-path "mkfifo") gate)
         (error 'handshake-race "mkfifo failed"))
       (display-to-file (string-append "#!/bin/sh\n"
                                       "echo $$ > \"$Q_HANDSHAKE_PID\"\n"
                                       "IFS= read -r go < \"$Q_HANDSHAKE_GATE\" || exit 99\n"
                                       "exec \"$Q_REAL_SETSID\" \"$@\"\n")
                        wrapper)
       (file-or-directory-permissions wrapper #o755)
       (define env (environment-variables-copy (current-environment-variables)))
       (environment-variables-set!
        env
        #"PATH"
        (string->bytes/utf-8
         (string-append (path->string root) ":" (or (getenv "PATH") "/usr/bin:/bin"))))
       (for ([key '(#"Q_HANDSHAKE_PID" #"Q_HANDSHAKE_GATE" #"Q_REAL_SETSID")]
             [value (list pid-path gate real-setsid)])
         (environment-variables-set! env key (path->bytes value)))
       (define guard
         (make-security-guard
          parent-guard
          (lambda (_who path modes)
            (when (and path
                       (memq 'read modes)
                       (regexp-match? #rx"(q-subprocess|gsd-verification)-watcher-"
                                      (path->string path)))
              (cond
                [first-empty? (set! later-reads (add1 later-reads))]
                [else
                 (parameterize ([current-security-guard parent-guard])
                   ;; Observe the actual empty pre-created handshake while the
                   ;; wrapper is blocked. Delay delivery of this failed first
                   ;; observation until publication AND owner exit. The guard
                   ;; exception transports the already-observed missing value
                   ;; through read-watcher-identity's normal #f error path.
                   (check-equal? (file->string path) "")
                   (set! first-empty? #t)
                   (wait-until (lambda () (file-exists? pid-path)))
                   (set! owner-pid (string->number (string-trim (file->string pid-path))))
                   (call-with-output-file gate (lambda (out) (displayln "go" out)) #:exists 'append)
                   (wait-until (lambda () (dead? owner-pid)))
                   (check-regexp-match #rx"^[0-9]+ \\(" (file->string path))
                   (unless (eq? identity-mode 'valid)
                     (display-to-file (if (eq? identity-mode 'missing) "" "invalid identity\n")
                                      path
                                      #:exists 'truncate)))
                 (error 'handshake-race "delayed empty first observation")]))
            (void))
          (lambda args (void))))
       (parameterize ([current-namespace (make-base-namespace)]
                      [current-environment-variables env])
         (define module (if (eq? api 'subprocess) subprocess-module verification-module))
         ;; Resolve modules before installing the scheduling guard.
         (dynamic-require module #f)
         (define (get name)
           (dynamic-require module name))
         (parameterize ([current-security-guard guard])
           (proc get)))
       (check-true first-empty? "the empty observation was forced")
       (when (eq? identity-mode 'valid)
         (check-true (> later-reads 0) "the published identity was reread"))
       (check-true (dead? owner-pid) "owner exited before the API returned"))
     (lambda ()
       ;; Never signal a saved process group, including on fixture failure.
       (when (and owner-pid (not (dead? owner-pid)))
         (system* (find-executable-path "kill") "-KILL" (number->string owner-pid)))
       (delete-directory/files root)))))

(module+ test
  (test-case "subprocess rereads a handshake published after its empty observation before owner exit"
    (with-empty-read-then-exit 'subprocess
                               (lambda (get)
                                 (define result
                                   ((get 'run-subprocess) "/bin/sh"
                                                          #:args
                                                          '("-c" "printf out; printf err >&2; exit 7")
                                                          #:process-group? #t
                                                          #:timeout 10))
                                 (check-equal? ((get 'subprocess-result-exit-code) result) 7)
                                 (check-false ((get 'subprocess-result-timed-out?) result))
                                 (check-equal? ((get 'subprocess-result-stdout) result) "out")
                                 (check-equal? ((get 'subprocess-result-stderr) result) "err"))))
  (test-case "verification job retains the late handshake and confirms cleanup before releasing ownership"
    (with-empty-read-then-exit
     'verification
     (lambda (get)
       (define reg ((get 'make-verification-registry)))
       (dynamic-wind
        void
        (lambda ()
          (define ident ((get 'verification-identity) "handshake" "W1" "fast" "/repo" "local"))
          (define start
            ((get 'verification-start!) reg
                                        ident
                                        "/bin/sh"
                                        '("-c" "printf out; printf err >&2; exit 7")))
          (define job ((get 'verification-wait) reg ((get 'start-result-job-id) start) 5000))
          (check-equal? ((get 'verification-job-state) job) 'failed)
          (check-equal? ((get 'verification-job-exit-code) job) 7)
          (check-true ((get 'verification-job-reaped?) job))
          (check-equal? ((get 'verification-job-stdout) job) "out")
          (check-equal? ((get 'verification-job-stderr) job) "err")
          (check-equal? ((get 'registry-active-count) reg) 0))
        (lambda () ((get 'verification-close-registry!) reg))))))
  (for ([identity-mode '(missing malformed)])
    (test-case (format "owner exit cannot replace a ~a handshake in either API" identity-mode)
      (with-empty-read-then-exit
       'subprocess
       (lambda (get)
         (define result
           ((get 'run-subprocess) "/bin/sh" #:args '("-c" "exit 0") #:process-group? #t #:timeout 10))
         (check-equal? ((get 'subprocess-result-exit-code) result) -1)
         (check-regexp-match #rx"identity unconfirmed" ((get 'subprocess-result-stderr) result)))
       #:identity identity-mode)
      (with-empty-read-then-exit
       'verification
       (lambda (get)
         (define reg ((get 'make-verification-registry)))
         (dynamic-wind
          void
          (lambda ()
            (define ident ((get 'verification-identity) "missing" "W1" "fast" "/repo" "local"))
            (define start ((get 'verification-start!) reg ident "/bin/sh" '("-c" "exit 0")))
            (define job ((get 'verification-wait) reg ((get 'start-result-job-id) start) 5000))
            ;; The truthful command status remains separate from ownership.
            (check-equal? ((get 'verification-job-exit-code) job) 0)
            (check-false ((get 'verification-job-reaped?) job))
            (check-equal? ((get 'registry-active-count) reg) 1)
            (define duplicate ((get 'verification-start!) reg ident "/bin/sh" '("-c" "exit 0")))
            (check-false ((get 'start-result-started?) duplicate)))
          (lambda () ((get 'verification-close-registry!) reg))))
       #:identity identity-mode))))
