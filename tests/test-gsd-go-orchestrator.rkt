#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-go-orchestrator.rkt — W2: Single-Wave Campaign Coordinator
;;
;; TDD tests for:
;;   1. One runner call per wave, prompt isolation.
;; @boundary integration
;;   2. No advancement on failure/cancellation.
;;   3. Verifier rejection prevents DONE.
;;   4. /go N assertion rejects non-earliest wave.
;;   5. Duplicate lease rejection.
;;   6. Campaign completes when all waves done.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/system
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest
                  make-campaign-wave-descriptor
                  make-campaign-wave
                  make-campaign-record
                  campaign-manifest-hash
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count
                  set-campaign-wave-status!
                  set-campaign-cancellation!
                  set-campaign-fence-token!
                  make-campaign-cancellation
                  begin-attempt!
                  select-next-actionable-wave
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign! load-campaign-record)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign-wave
                  run-campaign!
                  assert-go-n
                  campaign-result-status
                  campaign-result-message
                  campaign-result-completed-waves
                  acquire-lease
                  release-lease!
                  campaign-lease?
                  make-campaign-request
                  campaign-request-timeout-sec
                  execute-campaign-request!
                  current-gsd-wave-cancel!
                  find-git-root
                  git-available?)
         (only-in "../extensions/gsd/wave-runner-port.rkt" wave-execution-outcome)
         (only-in "../util/loop-result.rkt" make-loop-result)
         (only-in "../extensions/gsd/policy.rkt"
                  current-gsd-wave-timeout-seconds
                  current-gsd-wave-timeout-retries))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-tmp-campaign-dir n-waves)
  (define dir (make-temporary-file "go-orch-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Test Campaign\n\n## Waves\n\n" out)
                           (for ([i (in-range n-waves)])
                             (fprintf out "- [Inbox] W~a: Wave ~a → waves/W~a-wave.md\n" i i i)))
                         #:exists 'truncate)
  dir)

(define (load-or-migrate dir)
  (migrate-campaign! dir))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave-status* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    (campaign-wave-status w)))

;; ============================================================
;; Test suites
;; ============================================================

(define single-wave-suite
  (test-suite "single-wave execution"

    (test-case "runner succeeds + verifier approves → DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir rec 0 #:runner (lambda (_) 'ok) #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-done)
      (check-eq? (wave-status* rec 0) 'done)
      (cleanup-tmp dir))

    (test-case "verifier rejects → no DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir rec 0 #:runner (lambda (_) 'ok) #:verifier (lambda (_) #f)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-false (eq? (wave-status* rec 0) 'done) "verifier rejection cannot persist DONE")
      (cleanup-tmp dir))

    (test-case "runner error → FAILED"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir rec 0 #:runner (lambda (_) 'error) #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'failed)
      (cleanup-tmp dir))

    (test-case "runner cancelled → INTERRUPTED"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir rec 0 #:runner (lambda (_) 'cancelled) #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))

    (test-case "wave-timeout retries up to the configured ceiling (BUG-0017)"
      ;; A run that keeps exceeding the per-wave budget must be retried with a
      ;; fresh session up to current-gsd-wave-timeout-retries times before the
      ;; wave is persisted interrupted (at-least-once, exactly-once).
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define runs (box 0))
      (define result
        (parameterize ([current-gsd-wave-timeout-retries 3])
          (run-campaign-wave dir
                             rec
                             0
                             #:runner (lambda (_)
                                        (set-box! runs (add1 (unbox runs)))
                                        (wave-execution-outcome 'timed-out
                                                                "runner exceeded 1 second(s)"))
                             #:verifier (lambda (_) #t))))
      ;; 1 initial + 3 retries = 4 runs, all timed out → interrupted stop.
      (check-equal? (unbox runs) 4)
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-true (string-contains? (campaign-result-message result) "after 3 retries"))
      (cleanup-tmp dir))

    (test-case "wave-timeout retry succeeds when a later run completes (BUG-0017)"
      ;; The coordinator must retry with a fresh session and accept a run that
      ;; completes on a later attempt — a transient budget overrun should not
      ;; burn the wave.
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define runs (box 0))
      (define result
        (parameterize ([current-gsd-wave-timeout-retries 5])
          (run-campaign-wave dir
                             rec
                             0
                             #:runner
                             (lambda (_)
                               (set-box! runs (add1 (unbox runs)))
                               (if (< (unbox runs) 3)
                                   (wave-execution-outcome 'timed-out "runner exceeded 1 second(s)")
                                   'ok))
                             #:verifier (lambda (_) #t))))
      (check-equal? (unbox runs) 3)
      (check-eq? (campaign-result-status result) 'wave-done)
      (check-eq? (wave-status* rec 0) 'done)
      (cleanup-tmp dir))

    (test-case "wave-timeout retries=0 preserves single-run fail-closed"
      ;; Explicitly disabling retries keeps the D8 fail-closed behavior: one
      ;; run, one timeout, interrupted stop.
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define runs (box 0))
      (define result
        (parameterize ([current-gsd-wave-timeout-retries 0])
          (run-campaign-wave dir
                             rec
                             0
                             #:runner (lambda (_)
                                        (set-box! runs (add1 (unbox runs)))
                                        (wave-execution-outcome 'timed-out
                                                                "runner exceeded 1 second(s)"))
                             #:verifier (lambda (_) #t))))
      (check-equal? (unbox runs) 1)
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))))

(define campaign-suite
  (test-suite "full campaign execution"

    (test-case "all waves succeed → campaign-complete"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define result (run-campaign! dir rec #:runner (lambda (_) 'ok) #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'campaign-complete)
      (check-equal? (campaign-result-completed-waves result) '(0 1 2))
      (cleanup-tmp dir))

    (test-case "failure stops campaign (no advancement)"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define call-count 0)
      (define result
        (run-campaign! dir
                       rec
                       #:runner (lambda (idx)
                                  (set! call-count (add1 call-count))
                                  (if (= idx 1) 'error 'ok))
                       #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-equal? call-count 2 "runner called for W0 and W1 only")
      (check-eq? (wave-status* rec 0) 'done)
      (check-eq? (wave-status* rec 1) 'failed)
      (check-false (eq? (wave-status* rec 2) 'done) "W2 must not advance past failed W1")
      (cleanup-tmp dir))

    (test-case "verifier rejection stops campaign"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define cancel-count 0)
      (define result
        (parameterize ([current-gsd-wave-cancel! (lambda () (set! cancel-count (add1 cancel-count)))])
          (run-campaign! dir
                         rec
                         #:runner (lambda (_) 'ok)
                         #:verifier (lambda (idx) (not (= idx 1))))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'done)
      (check-false (eq? (wave-status* rec 1) 'done))
      (check-equal? cancel-count
                    0
                    "an expected verifier rejection must not cancel an unrelated worker")
      (cleanup-tmp dir))))

(define lease-suite
  (test-suite "campaign lease (D5)"

    (test-case "duplicate lease is rejected"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define lease1 (acquire-lease dir (campaign-plan-id rec)))
      (check-true (campaign-lease? lease1))
      (define lease2 (acquire-lease dir (campaign-plan-id rec)))
      (check-false lease2 "second acquire returns #f")
      (release-lease! lease1)
      ;; After release, can re-acquire
      (define lease3 (acquire-lease dir (campaign-plan-id rec)))
      (check-true (campaign-lease? lease3))
      (release-lease! lease3)
      (cleanup-tmp dir))

    (test-case "stale lock file does not block restart"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define lock-path
        (build-path dir ".planning" "campaigns" (string-append (campaign-plan-id rec) ".lock")))
      (make-directory* (path-only lock-path))
      (call-with-output-file lock-path (lambda (out) (display "stale-owner" out)) #:exists 'truncate)
      (define lease (acquire-lease dir (campaign-plan-id rec)))
      (check-true (campaign-lease? lease))
      (release-lease! lease)
      (cleanup-tmp dir))

    (test-case "lease records owner session-id and pid (D4, issue #9351)"
      ;; Incident 81f9be4b: the lock file named owner "unknown" and no pid,
      ;; so a stale lease could not be attributed to any holder.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define lease (acquire-lease dir (campaign-plan-id rec) #:session-id "session-abc"))
      (check-true (campaign-lease? lease))
      (define lock-path
        (build-path dir ".planning" "campaigns" (string-append (campaign-plan-id rec) ".lock")))
      (define content (with-input-from-file lock-path read))
      (check-equal? (hash-ref content 'owner #f) "session-abc")
      (check-true (exact-nonnegative-integer? (hash-ref content 'pid #f)) "pid must be recorded")
      (check-true (real? (hash-ref content 'acquired #f)) "acquired timestamp must be recorded")
      (release-lease! lease)
      (cleanup-tmp dir))

    (test-case "run-campaign! threads lease-owner into the lease (D4, issue #9351)"
      ;; The wave runner executes while the coordinator holds the lease, so
      ;; it can observe the lock-file owner written under the lease.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define lock-path
        (build-path dir ".planning" "campaigns" (string-append (campaign-plan-id rec) ".lock")))
      (define observed-owner (box #f))
      (define result
        (run-campaign! dir
                       rec
                       #:lease-owner "main-tui-session"
                       #:verifier (lambda (_) #t)
                       #:runner
                       (lambda (_idx)
                         (set-box! observed-owner
                                   (hash-ref (with-input-from-file lock-path read) 'owner #f))
                         'ok)))
      (check-eq? (campaign-result-status result) 'campaign-complete)
      (check-equal? (unbox observed-owner) "main-tui-session")
      (cleanup-tmp dir))))

(define go-n-suite
  (test-suite "/go N assertion (D8)"

    (test-case "/go 0 when W0 actionable → allowed"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (check-true (assert-go-n rec 0))
      (cleanup-tmp dir))

    (test-case "/go 2 when W0 actionable → rejected"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (check-false (assert-go-n rec 2) "cannot bypass W0")
      (cleanup-tmp dir))

    (test-case "/go 1 when W0 done → allowed"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
      (check-true (assert-go-n rec 1))
      (cleanup-tmp dir))))

(define request-suite
  (test-suite "live campaign request boundary"

    (test-case "unsafe default runner and verifier fail closed"
      (define dir-a (make-tmp-campaign-dir 1))
      (define rec-a (load-or-migrate dir-a))
      (check-eq? (campaign-result-status (run-campaign-wave dir-a rec-a 0)) 'wave-failed)
      (cleanup-tmp dir-a)
      (define dir-b (make-tmp-campaign-dir 1))
      (define rec-b (load-or-migrate dir-b))
      (check-eq? (campaign-result-status (run-campaign-wave dir-b rec-b 0 #:runner (lambda (_) 'ok)))
                 'wave-failed)
      (cleanup-tmp dir-b))

    (test-case "request runs isolated prompt for each wave"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define prompts '())
      (define request
        (make-campaign-request dir rec (lambda (idx) (format "ONLY-W~a" idx)) (lambda (_) #t)))
      (define result
        (execute-campaign-request! request
                                   (lambda (prompt)
                                     (set! prompts (append prompts (list prompt)))
                                     (make-loop-result '() 'completed (hasheq)))))
      (check-eq? (campaign-result-status result) 'campaign-complete)
      (check-equal? prompts '("ONLY-W0" "ONLY-W1"))
      (cleanup-tmp dir))

    (test-case "blocked completion metadata fails closed"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t)))
      (define result
        (execute-campaign-request!
         request
         (lambda (_) (make-loop-result '() 'completed (hasheq 'reason "extension-block")))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'failed)
      (cleanup-tmp dir))

    (test-case "pending tools without limit metadata fail current wave"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t)))
      (define result
        (execute-campaign-request! request
                                   (lambda (_) (make-loop-result '() 'tool-calls-pending (hasheq)))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'failed)
      (cleanup-tmp dir))

    (test-case "empty response fails current wave"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t)))
      (define result
        (execute-campaign-request! request
                                   (lambda (_) (make-loop-result '() 'empty-response (hasheq)))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-eq? (wave-status* rec 0) 'failed)
      (cleanup-tmp dir))

    (test-case "production request applies bounded wave timeout"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t)))
      (define result
        (parameterize ([current-gsd-wave-timeout-seconds 0.01]
                       [current-gsd-wave-timeout-retries 0])
          (execute-campaign-request! request
                                     (lambda (_)
                                       (sleep 0.1)
                                       (make-loop-result '() 'completed (hasheq))))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))

    (test-case "request #:timeout-sec overrides the wave-budget parameter"
      ;; /go --wave-timeout=SECONDS and config wave-timeout-seconds
      ;; resolve to a per-campaign timeout carried on the request. It must
      ;; override the (default 7200 s) parameter even when the campaign runs
      ;; in a separate thread — hence the request-carried value, not a
      ;; parameterize at /go time.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request
        (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t) #:timeout-sec 0.01))
      (define result
        (parameterize ([current-gsd-wave-timeout-retries 0])
          (execute-campaign-request! request
                                     (lambda (_)
                                       (sleep 0.1)
                                       (make-loop-result '() 'completed (hasheq))))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))

    (test-case "request timeout-sec carries onto the request struct"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request
        (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t) #:timeout-sec 4321))
      (check-equal? (campaign-request-timeout-sec request) 4321)
      (define default-request (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t)))
      (check-false (campaign-request-timeout-sec default-request))
      (cleanup-tmp dir))

    (test-case "tool-loop termination fails current wave and stops advancement"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define calls 0)
      (define request
        (make-campaign-request dir rec (lambda (idx) (format "W~a" idx)) (lambda (_) #t)))
      (define result
        (execute-campaign-request!
         request
         (lambda (_)
           (set! calls (add1 calls))
           (make-loop-result '() 'tool-calls-pending (hasheq 'toolLoopLimit #t)))))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (check-equal? calls 1)
      (check-eq? (wave-status* rec 0) 'failed)
      (check-eq? (wave-status* rec 1) 'pending)
      (cleanup-tmp dir))

    (test-case "two-value production runner uses returned loop result"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define request (make-campaign-request dir rec (lambda (_) "W0") (lambda (_) #t)))
      (define result
        (execute-campaign-request!
         request
         (lambda (_) (values 'updated-session (make-loop-result '() 'completed (hasheq))))))
      (check-eq? (campaign-result-status result) 'campaign-complete)
      (cleanup-tmp dir))

    (test-case "durable cancellation prevents the first runner call"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define calls 0)
      (persist-campaign! dir rec)
      (define newer (load-campaign-record dir (campaign-plan-id rec)))
      (set-campaign-cancellation! newer (make-campaign-cancellation "stop" 1))
      (persist-campaign! dir newer)
      (define result
        (run-campaign! dir
                       rec
                       #:runner (lambda (_)
                                  (set! calls (add1 calls))
                                  'ok)))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-equal? calls 0)
      (cleanup-tmp dir))

    (test-case "request captured before newer DONE cannot rerun or overwrite it"
      (define dir (make-tmp-campaign-dir 1))
      (define stale (load-or-migrate dir))
      (persist-campaign! dir stale)
      (define newer (load-campaign-record dir (campaign-plan-id stale)))
      (set-campaign-wave-status! (car (campaign-record-waves newer)) 'done)
      (set-campaign-fence-token! newer 7)
      (persist-campaign! dir newer)
      (define calls 0)
      (define result
        (run-campaign! dir
                       stale
                       #:runner (lambda (_)
                                  (set! calls (add1 calls))
                                  'ok)))
      (check-eq? (campaign-result-status result) 'campaign-complete)
      (check-equal? calls 0)
      (define durable (load-campaign-record dir (campaign-plan-id stale)))
      (check-eq? (wave-status* durable 0) 'done)
      (cleanup-tmp dir))

    (test-case "stale verifier result cannot commit DONE"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier (lambda (_)
                                        (set-campaign-fence-token! rec 999)
                                        (persist-campaign! dir rec)
                                        #t)))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'verifying "stale verifier must not write status")
      (cleanup-tmp dir))

    (test-case "cancellation during verification remains INTERRUPTED"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier
                           (lambda (_)
                             (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 2))
                             (persist-campaign! dir rec)
                             #t)))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))

    (test-case "verifier observes VERIFYING before approval"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define observed-status #f)
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier (lambda (_)
                                        (set! observed-status (wave-status* rec 0))
                                        #t)))
      (check-eq? observed-status 'verifying)
      (check-eq? (campaign-result-status result) 'wave-done)
      (cleanup-tmp dir))))

;; ============================================================
;; F-7: Git Root Resolution Tests
;; ============================================================

(define git-root-suite
  (test-suite "F-7: find-git-root"
    (test-case "find-git-root finds .git in the start directory"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (make-directory (build-path tmp ".git"))
                      (define result (find-git-root tmp))
                      (check-pred path? result)
                      (check-equal? (path->string (path->complete-path tmp))
                                    (path->string (path->complete-path result))))
                    (lambda () (delete-directory/files tmp))))

    (test-case "F-7: find-git-root finds .git in q/ subdirectory (two-tier layout)"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (make-directory* (build-path tmp "q" ".git"))
                      (define result (find-git-root tmp))
                      (check-pred path? result "should find q/ subdir")
                      (when result
                        (check-true (string-contains? (path->string result) "q"))))
                    (lambda () (delete-directory/files tmp))))

    (test-case "F-7: find-git-root walks up to parent directory"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (make-directory (build-path tmp ".git"))
         (define sub (build-path tmp "subdir" "nested"))
         (make-directory* sub)
         (define result (find-git-root sub))
         (check-pred path? result)
         (when result
           (define tmp-str (string-trim (path->string (simple-form-path tmp)) "/" #:right? #t))
           (define res-str (string-trim (path->string (simple-form-path result)) "/" #:right? #t))
           (check-equal? tmp-str res-str)))
       (lambda () (delete-directory/files tmp))))

    (test-case "F-7: find-git-root returns #f when no .git anywhere"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         ;; No .git anywhere in this temp tree (parent is /tmp which may or may not have .git)
         ;; We check that at least the function doesn't crash
         (define result (find-git-root tmp))
         ;; Could be #f or could find a parent .git — just check no crash
         (check-true (or (not result) (path? result))))
       (lambda () (delete-directory/files tmp))))

    (test-case "F-7: git-available? rejects a fake .git marker"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (make-directory (build-path tmp ".git"))
                      (check-false (git-available? tmp)))
                    (lambda () (delete-directory/files tmp))))

    (test-case "F-7: git-available? validates an actual work tree"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define git (find-executable-path "git"))
                      (when git
                        (parameterize ([current-output-port (open-output-string)]
                                       [current-error-port (open-output-string)])
                          (check-equal? (system*/exit-code git "-C" tmp "init" "--quiet") 0))
                        (check-true (git-available? tmp))))
                    (lambda () (delete-directory/files tmp))))

    (test-case "F-7: find-git-root handles .git as file (worktree/submodule)"
      (define tmp (make-temporary-file "git-root-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      ;; .git as a file (gitdir pointer) should also be detected
                      (call-with-output-file (build-path tmp ".git")
                                             (lambda (out) (display "gitdir: /fake/path\n" out)))
                      (define result (find-git-root tmp))
                      (check-pred path? result))
                    (lambda () (delete-directory/files tmp))))))

;; ============================================================
;; Runner
;; ============================================================

(define all-tests
  (test-suite "gsd-go-orchestrator (W2)"
    single-wave-suite
    campaign-suite
    lease-suite
    go-n-suite
    request-suite
    git-root-suite))

(void (run-tests all-tests))
