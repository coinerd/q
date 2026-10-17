#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/author-session-recovery.rkt extensions/gsd/delivery-verifier.rkt sandbox/subprocess.rkt
(require rackunit
         "helpers/lost-author-fixture.rkt"
         "../extensions/gsd/author-session-recovery.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/system-adapters.rkt"
         "../sandbox/subprocess.rkt"
         "../util/shell-quote.rkt")

;; Trigger the outer deadline only AFTER the exact process/guard is reached.
;; No tiny wall-clock Verify budget: legitimate compilation has 180 seconds.
(define (at-process-deadline pid-file thunk)
  (define start (current-inexact-milliseconds))
  (parameterize ([current-gsd-author-recovery-timeout-sec 180]
                 [current-gsd-timeout-now-ms
                  (lambda () (if (file-exists? pid-file)
                                 (+ start 181000)
                                 (current-inexact-milliseconds)))])
    (thunk)))
(define (hang-command pid-file)
  (format "echo $$ > ~a; trap '' TERM; while :; do sleep 1; done" (shell-quote (path->string pid-file))))
(define (dead? pid)
  ;; Linux boundary, like the production verifier's incarnation checks.
  (define p (format "/proc/~a/stat" pid))
  (or (not (file-exists? p))
      (with-handlers ([exn:fail? (lambda (_) #t)])
        (regexp-match? #rx"\\) [ZX] " (file->string p)))))
(define (check-process-gone pid-file)
  (check-true (file-exists? pid-file) "deadline must exercise the actual child")
  (when (file-exists? pid-file)
    (define pid (string->number (string-trim (file->string pid-file))))
    (define until (+ (current-inexact-milliseconds) 2000))
    (let wait ()
      (when (and (not (dead? pid)) (< (current-inexact-milliseconds) until))
        (sleep 0.01) (wait)))
    (check-true (dead? pid) (format "owned process ~a must not survive recovery" pid))))
(define (with-git-wrapper root script proc)
  (define bin (build-path root "fake-bin"))
  (make-directory* bin)
  (define git (build-path bin "git"))
  (put! git (string-append "#!/bin/sh\n" script "\n"))
  (file-or-directory-permissions git #o700)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"PATH"
                             (bytes-append (path->bytes bin) #":" (or (environment-variables-ref env #"PATH") #"")))
  (parameterize ([current-environment-variables env]) (proc)))

(module+ test
  ;; Journal reservations currently restrict frozen FILES to ASCII. Do not
  ;; weaken that authority just to install a U+FFFD alias: exercise an actual
  ;; invalid-byte commit and require refusal at the decoding boundary itself.
  (test-case "actual invalid-byte committed pathname refuses before scope comparison"
    (with-lost-author
     (lambda (f)
       (define cwd (lost-author-cwd f))
       (define invalid (bytes->path #"tests/\377.rkt"))
       (commit! cwd invalid "#lang racket/base\n")
       (check-true (file-exists? (build-path cwd invalid)))
       (check-false (file-exists? (build-path cwd "tests/\uFFFD.rkt")))
       (define before (journal f))
       (define head (git-output cwd "rev-parse" "HEAD"))
       (define result
         (recover-lost-author-session! (lost-author-root f) (lost-author-plan f) 0
                                       #:session-live? (lambda () #f)))
       (check-pred string? result)
       (when (string? result)
         (check-regexp-match #rx"cannot inspect committed author scope" result))
       (check-equal? (journal f) before)
       (check-equal? (git-output cwd "rev-parse" "HEAD") head))))

  (for ([phase '(success later-timeout)])
    (test-case (format "redirected descendant of completed Verify is dead after recovery ~a" phase)
      (with-lost-author
       (lambda (f)
         (define root (lost-author-root f))
         (define pid-file (build-path root "descendant.pid"))
         (define hung (build-path root "later-hung.pid"))
         (define owned #f)
         (define before (journal f))
         (commit! (lost-author-cwd f) "tests/second.rkt"
                  (format "#lang racket/base\n(require racket/system)\n(module+ test (unless (system ~s) (error 'test \"spawn failed\")))\n"
                          (format "(sleep 60; echo late-mutation >> source.rkt) >/dev/null 2>&1 & echo $! > ~a; exit 0"
                                  (shell-quote (path->string pid-file)))))
         (define (recover)
           (parameterize ([current-gsd-git-runner
                           (lambda (repo args)
                             (set! owned (current-gsd-verification-registry))
                             (parameterize ([current-gsd-git-runner #f]) (run-git repo args)))])
             (recover-lost-author-session!
              root (lost-author-plan f) 0
              #:session-live?
              (lambda ()
                (when (and (eq? phase 'later-timeout)
                           (let ([pub (load-approved-publication root (lost-author-plan f) 0)])
                             (and pub (equal? (hash-ref pub 'status) "confirmed")
                                  (not (equal? (hash-ref pub 'head) (hash-ref (lost-author-old f) 'head))))))
                  (run-subprocess "/bin/sh" #:args (list "-c" (hang-command hung))
                                  #:timeout 600 #:process-group? #t))
                #f))))
         (dynamic-wind
          void
          (lambda ()
            (define result (if (eq? phase 'success) (recover)
                               (at-process-deadline hung recover)))
            (if (eq? phase 'success)
                (check-equal? result #t)
                (begin (check-regexp-match #rx"owner recovery deadline exceeded" result)
                       (check-equal? (journal f) before)
                       (check-process-gone hung)))
            (check-process-gone pid-file)
            (define jobs (verification-close-registry! owned))
            (check-true (pair? jobs))
            (for ([job (in-list jobs)])
              (check-equal? (verification-job-state job) 'completed)
              (check-equal? (verification-job-exit-code job) 0))
            (check-false (string-contains? (file->string (build-path (lost-author-cwd f) "source.rkt"))
                                           "late-mutation")))
          (lambda ()
            ;; RED runs must not leave their intentionally surviving descendant.
            (when (and (file-exists? pid-file)
                       (not (dead? (string->number (string-trim (file->string pid-file))))))
              (system* (find-executable-path "kill") "-KILL"
                       (string-trim (file->string pid-file))))))))))

  (test-case "strict Git facts reject invalid UTF-8, not literal U+FFFD"
    (define root (make-temporary-file "verify-git-encoding-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define valid
         (run-subprocess "/bin/sh" #:args '("-c" "printf '\\357\\277\\275'")
                         #:encoding 'utf-8/strict))
       (check-equal? (subprocess-result-exit-code valid) 0)
       (check-equal? (subprocess-result-stdout valid) "\uFFFD")
       (define invalid
         (run-subprocess "/bin/sh" #:args '("-c" "printf '\\377'")
                         #:encoding 'utf-8/strict))
       (check-not-equal? (subprocess-result-exit-code invalid) 0)
       (with-git-wrapper
        root "printf '\\377'"
        (lambda ()
          (check-not-equal? (car (run-git root '("rev-parse" "HEAD"))) 0)
          (check-false (execution-snapshot root))
          (check-false (committed-delivery-snapshot root))
          (check-false (delivery-git root "rev-parse" "HEAD")))))
     (lambda () (delete-directory/files root))))

  (test-case "real verifier Git and authentic proof snapshots refuse output overflow"
    (define root (make-temporary-file "verify-git-output-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (with-git-wrapper
        root "head -c 10485761 /dev/zero"
        (lambda ()
          (define r (run-git root '("rev-parse" "HEAD")))
          (check-false (car r) "truncated output must not be a successful Git fact")
          (check-true (<= (string-length (second r)) default-max-output-bytes))
          (check-false (execution-snapshot root) "proof snapshots use the bounded REAL Git path"))))
     (lambda () (delete-directory/files root))))

  (for ([phase '(declared-verify proof-snapshot readback-guard)])
    (test-case (format "outer recovery deadline cleans ~a processes, registry and threads" phase)
      (with-lost-author
       (lambda (f)
         (define root (lost-author-root f))
         (define pid-file (build-path root "hung.pid"))
         (define arm (build-path root "arm-git"))
         (define real-git (find-executable-path "git"))
         (define before (journal f))
         (define ambient (current-gsd-verification-registry))
         (define ambient-count (registry-active-count ambient))
         (define owned #f)
         (define cust #f)
         (case phase
           [(declared-verify)
            (commit! (lost-author-cwd f) "tests/second.rkt"
                     (format "#lang racket/base\n(require racket/system)\n(module+ test (system ~s))\n"
                             (hang-command pid-file)))]
           [(proof-snapshot)
            (commit! (lost-author-cwd f) "tests/second.rkt"
                     (format "#lang racket/base\n(require racket/file)\n(module+ test (display-to-file \"arm\" ~s))\n"
                             (path->string arm)))])
         (define (session-live?)
           (when (and (eq? phase 'readback-guard)
                      (let ([pub (load-approved-publication root (lost-author-plan f) 0)])
                        (and pub (equal? (hash-ref pub 'status) "confirmed")
                             (not (equal? (hash-ref pub 'head) (hash-ref (lost-author-old f) 'head))))))
             ;; Exercise the post-publication certification guard, not merely
             ;; the command's timeout. A real child must be unwound here too.
             (run-subprocess "/bin/sh" #:args (list "-c" (hang-command pid-file))
                             #:timeout 600 #:process-group? #t))
           #f)
         (define (recover)
           (parameterize ([current-gsd-git-runner
                           (lambda (repo args)
                             (set! owned (current-gsd-verification-registry))
                             (set! cust (current-custodian))
                             (parameterize ([current-gsd-git-runner #f]) (run-git repo args)))])
             (at-process-deadline
              pid-file
              (lambda ()
                (recover-lost-author-session! root (lost-author-plan f) 0 #:session-live? session-live?)))))
         (define result
           (if (eq? phase 'proof-snapshot)
               (with-git-wrapper
                root
                (format "if [ -f ~a ]; then ~a; fi\nexec ~a \"$@\""
                        (shell-quote (path->string arm)) (hang-command pid-file)
                        (shell-quote (path->string real-git)))
                recover)
               (recover)))
         (check-regexp-match #rx"owner recovery deadline exceeded" result)
         (check-equal? (journal f) before "timeout must not certify or consume author allowance")
         (check-process-gone pid-file)
         (check-not-false owned)
         (check-not-eq? owned ambient)
         (check-equal? (registry-active-count owned) 0)
         (define jobs (verification-close-registry! owned))
         (check-true (pair? jobs) "must have reached the actual owned verifier")
         (for ([job (in-list jobs)])
           (check-true (verification-job-reaped? job))
           (check-not-equal? (verification-job-state job) 'running)
           (check-true (dead? (verification-job-pid job))))
         (check-true (custodian-shut-down? cust) "no recovery reader or worker threads survive")
         (check-equal? (registry-active-count ambient) ambient-count)
         ;; Confirmed-publication replay discovery is bounded too. Interrupt a
         ;; real ancestry Git read; it must return no replay, not hang or leak.
         (when (eq? phase 'readback-guard)
           (delete-file pid-file)
           (check-false
            (with-git-wrapper
             root
             (format "if [ \"$3\" = merge-base ]; then ~a; fi\nexec ~a \"$@\""
                     (hang-command pid-file) (shell-quote (path->string real-git)))
             (lambda ()
               (at-process-deadline pid-file
                                    (lambda () (author-publication-recovery root (lost-author-plan f) 0))))))
           (check-process-gone pid-file)
           (check-equal? (journal f) before)))))))
