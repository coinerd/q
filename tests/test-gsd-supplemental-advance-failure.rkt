#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 1200
;; BUG-0101/R101 supplemental advance failure + authority semantics.
;;
;; Red-first contract tests on real private journal fixtures inside a
;; temporary authority root. The authority/journal deliberately expose NO
;; test clock, so time is injected through the real operator-approved
;; expiry at seconds resolution (a witness author that sleeps until the
;; retained task's own expires-at passes DURING a real C1/probe wait) —
;; fail-closed boundary semantics (now == expires-at is already expired)
;; are pinned by repeated observation, never by a wall-clock race.
;;
;; Contract under test (security-contracts.md 3.1 + plan R101):
;;   - additive typed discovery: absent vs live vs retained-ineligible
;;     {expired|revoked|malformed|inaccessible|context-mismatch} (+ typed
;;     pending-wal views), read-only, deny-only, no store creation, no WAL
;;     reconcile, no capability; retained-but-ineligible is NEVER reported
;;     as vanished and an absent public pointer with a retained claim stays
;;     deny-only;
;;   - the four-value compatibility API keeps its historical behavior for
;;     every consumer;
;;   - advance phase-local handlers return IMMEDIATELY with the ORIGINAL
;;     cause (no unconditional loop recursion after failure, continuation
;;     only on explicit success); typed pauses raised inside a phase remain
;;     pauses; no rediscovery of live authority merely to report the
;;     original error; expiry/revocation during C1/probe waits produce zero
;;     GREEN/probe/publication/promotion and zero new executions;
;;   - secondary transition/CAS/storage failures are reported separately,
;;     never replacing the primary cause and never claiming blocked
;;     persisted when it was not.
(require rackunit
         json
         racket/file
         racket/path
         (only-in "test-gsd-supplemental-evidence-journal.rkt"
                  task
                  with-fixture
                  fixture-grant
                  fixture-root
                  base-binding
                  approve!
                  revoke-approval!
                  operation-path
                  step!)
         "helpers/supplemental-go-fixture.rkt"
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/campaign-result.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/events.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         (only-in "../extensions/gsd/review-proof.rkt" current-owned-proof-root)
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-publication.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-operator.rkt"
                  current-supplemental-operator-publisher)
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-verification-repair-retries)
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/composition-root.rkt")

;; ------------------------------------------------------------------
;; Shared helpers
;; ------------------------------------------------------------------
(define (put-text repo relative text)
  (define p (build-path repo relative))
  (make-parent-directory* p)
  (display-to-file text p #:exists 'truncate))

(define (fixture-grant-of fx)
  (hash-ref
   (load-supplemental-pointer (supplemental-fixture-root fx) (supplemental-fixture-plan fx) 1)
   'task-approval))

(define (task-expires-at fx)
  (hash-ref (hash-ref (load-supplemental-record
                       (hash-ref (load-supplemental-pointer (supplemental-fixture-root fx)
                                                            (supplemental-fixture-plan fx)
                                                            1)
                                 'task-proposal))
                      'payload)
            'expires-at))

;; The real interactive broker, driven exactly once per revocation.
(define (revoke-approval-now! fx)
  (define aref (fixture-grant-of fx))
  (define revocation
    (propose-retain! "revocation-proposal"
                     (hasheq 'reason "BUG-0101 fixture operator revocation")
                     (list aref)))
  (revoke! revocation (fixture-interactive-grant revocation)))

;; Simulated model author: writes ONLY the two approved files. The witness
;; may sleep until the retained task's expiry (+1s) so a real expiry passes
;; DURING the C1 subprocess wait, and may deliberately check the WRONG
;; answer to make C1 genuinely RED.
(define harness-text
  "#lang racket/base\n(require \"../../runtime/local.rkt\")\n(provide witness)\n(define (witness) (= answer 42))\n")
(define (witness-text #:red? [red? #f] #:sleep-until [sleep-until #f])
  (string-append
   "#lang racket\n"
   ";; @speed fast\n;; @suite extensions\n;; @boundary unit\n;; @timeout "
   (if sleep-until "360" "120")
   "\n"
   "(require rackunit \"helpers/witness.rkt\")\n(provide evidence-suite)\n"
   (if sleep-until
       (format
        "(define (wait-for-expiry)\n  (let loop ()\n    (unless (>= (current-seconds) ~a)\n      (sleep 1)\n      (loop))))\n"
        sleep-until)
       "")
   "(define evidence-suite\n  (test-suite \"witness\"\n    (test-case \"new regression\"\n"
   (if sleep-until "      (wait-for-expiry)\n" "")
   (if red? "      (check-equal? (witness) #f))))\n" "      (check-true (witness)))))\n")
   "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))

(define ((author-runner fx #:red? [red? #f] #:sleep-until [sleep-until #f]) _)
  (define repo (current-directory))
  (put-text repo "tests/helpers/witness.rkt" harness-text)
  (put-text repo
            "tests/test-witness.rkt"
            (witness-text #:red? red? #:sleep-until (and sleep-until (+ (task-expires-at fx) 1))))
  'ok)

(define (auto-approve-publisher event payload)
  (when (equal? event "supplemental-evidence.approval-requested")
    (thread
     (lambda ()
       (approval-decide! (hash-ref payload 'request-id) (hash-ref payload 'proposal-sha256) #t)))))

(define (with-bus handler thunk)
  (define old (unbox gsd-event-bus-box))
  (dynamic-wind (lambda () (set-gsd-event-bus! handler)) thunk (lambda () (set-gsd-event-bus! old))))

(define (author-run fx runner)
  (parameterize ([current-gsd-wave-verification-repair-retries 0])
    (run-campaign-wave (supplemental-fixture-root fx)
                       (load-campaign-record (supplemental-fixture-root fx)
                                             (supplemental-fixture-plan fx))
                       1
                       #:fence-token 8
                       #:timeout-sec 600
                       #:timeout-retries 0
                       #:no-change-retries 0
                       #:stall-soft-limit #f
                       #:stall-hard-limit #f
                       #:isolate? #f
                       #:advance-override? #t
                       #:runner runner
                       #:verifier (lambda (_) (error 'verifier "ordinary verify must never run")))))

(define (wave-attempt-count fx)
  (campaign-wave-attempt-count
   (car (campaign-record-waves (load-campaign-record (supplemental-fixture-root fx)
                                                     (supplemental-fixture-plan fx))))))

(define (delivery-journal-bytes fx)
  (file->bytes
   (delivery-journal-path (supplemental-fixture-root fx) (supplemental-fixture-plan fx) 1)))

(define (staged-path fx)
  (build-path (current-owned-proof-root)
              (supplemental-fixture-plan fx)
              "supplemental-staging"
              "staged.json"))

;; The GO fixture's private operation storage (the journal-test helper
;; operation-path/operation-path binds the other fixture type).
(define (go-operation-path fx [suffix ".json"])
  (build-path (supplemental-fixture-private fx)
              "supplemental-operations"
              (string-append (hash-ref (fixture-grant-of fx) 'sha256) suffix)))

;; Read-only peek of the raw sealed snapshot for post-denial state checks:
;; once the authority is expired/revoked, the journal's load refuses
;; fail-closed, so state observation goes through the file itself.
(define (peek-record fx)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file (go-operation-path fx) read-json)))

(define (with-auto-operator thunk)
  (parameterize ([current-supplemental-operator-publisher auto-approve-publisher])
    (thunk)))

(module+ test
  ;; ----------------------------------------------------------------
  ;; Part A — typed journal view (fast, real private storage only)
  ;; ----------------------------------------------------------------
  (test-case "typed view: absent/unlaunched, live with attempt identity"
    (with-fixture
     (lambda (fx)
       (define grant (fixture-grant fx))
       (define v (supplemental-operation-view grant))
       (check-equal? (hash-ref v 'status) 'absent)
       (check-equal? (hash-ref v 'cause) 'unlaunched)
       (check-equal? (hash-ref v 'grant-ref) grant)
       (check-false (hash-ref v 'operation #f))
       (check-true (exact-positive-integer? (hash-ref v 'expires-at #f)))
       (start-supplemental-operation! grant base-binding)
       (define live (supplemental-operation-view grant))
       (check-equal? (hash-ref live 'status) 'live)
       (check-false (hash-ref live 'cause #f))
       (check-equal? (hash-ref (hash-ref live 'operation) 'state) "proposal-approved")
       (step! fx
              "reserved"
              (hasheq 'allocation
                      (hasheq 'attempt-id "attempt-9" 'attempt-fence 3 'coordinator-fence 7)))
       (define bound (supplemental-operation-view grant))
       (check-equal? (hash-ref bound 'attempt-id) "attempt-9")
       (check-equal? (hash-ref bound 'attempt-fence) 3)
       (check-equal? (hash-ref bound 'coordinator-fence) 7))))

  (test-case "typed view: expired including the exact boundary, revoked, malformed, inaccessible"
    ;; Expiry is injected through the real operator-approved expiry at
    ;; seconds resolution. Observations spanning the boundary second and
    ;; later seconds must ALL be expired: now == expires-at is already
    ;; expired, exactly matching the journal's admission check.
    (with-fixture #:payload (hasheq 'task task 'expires-at (current-seconds))
                  (lambda (fx)
                    (define grant (fixture-grant fx))
                    ;; Observations spanning the boundary second and later seconds must
                    ;; ALL be expired: now == expires-at is already expired, exactly
                    ;; matching the journal's admission check.
                    (let loop ([n 0])
                      (define v (supplemental-operation-view grant))
                      (check-equal? (hash-ref v 'status) 'retained-ineligible)
                      (check-equal? (hash-ref v 'cause) 'expired)
                      (check-regexp-match #rx"expired" (hash-ref v 'message))
                      (when (< n 2)
                        (sleep 1)
                        (loop (add1 n))))))
    (with-fixture (lambda (fx)
                    (define grant (fixture-grant fx))
                    (revoke-approval! grant)
                    (define v (supplemental-operation-view grant))
                    (check-equal? (hash-ref v 'status) 'revoked-expired)
                    (check-equal? (hash-ref v 'cause) 'revoked)
                    (check-regexp-match #rx"revoked" (hash-ref v 'message))))
    (with-fixture (lambda (fx)
                    (define grant (fixture-grant fx))
                    (start-supplemental-operation! grant base-binding)
                    ;; Corrupt the sealed snapshot: a retained but malformed record.
                    (call-with-output-file (operation-path fx)
                                           (lambda (out) (display "{broken" out))
                                           #:exists 'append)
                    (define v (supplemental-operation-view grant))
                    (check-equal? (hash-ref v 'status) 'retained-ineligible)
                    (check-equal? (hash-ref v 'cause) 'malformed)))
    (with-fixture (lambda (fx)
                    (define grant (fixture-grant fx))
                    (start-supplemental-operation! grant base-binding)
                    ;; A permission-denied operations directory is inaccessible, not
                    ;; malformed: the record itself may be perfectly intact.
                    (define dir (build-path (fixture-root fx) "supplemental-operations"))
                    (file-or-directory-permissions dir #o000)
                    (define v (supplemental-operation-view grant))
                    (check-equal? (hash-ref v 'status) 'unreadable)
                    (check-equal? (hash-ref v 'cause) 'inaccessible)
                    (file-or-directory-permissions dir #o700))))

  (test-case "typed view: pending-wal is read-only for typed and early cuts"
    (with-fixture
     (lambda (fx)
       ;; Early ambiguous cut: the sealed intent awaits the journal's
       ;; fail-closed blocked settlement, and the view must NOT settle it.
       (define grant (fixture-grant fx))
       (define intent (operation-path fx ".intent.json"))
       (start-supplemental-operation! grant base-binding)
       (dynamic-wind
        (lambda ()
          (current-supplemental-journal-crash-cut (lambda (at)
                                                    (when (eq? at 'intent-written)
                                                      (error 'fixture "simulated crash")))))
        (lambda ()
          (check-exn
           exn:fail?
           (lambda ()
             (step! fx
                    "reserved"
                    (hasheq 'allocation
                            (hasheq 'attempt-id "a" 'attempt-fence 1 'coordinator-fence 1))))))
        (lambda () (current-supplemental-journal-crash-cut void)))
       (check-true (file-exists? intent))
       (define early (supplemental-operation-view grant))
       (check-equal? (hash-ref early 'status) 'pending-wal)
       (check-false (hash-ref early 'intent-reconcilable?))
       (check-equal? (hash-ref early 'intent-target) "reserved")
       (check-equal? (hash-ref (hash-ref early 'operation) 'state) "proposal-approved")
       (check-true (file-exists? intent) "the view must never reconcile the WAL")
       ;; The write-path load settles the early cut exactly once, blocked.
       (define settled (load-supplemental-operation grant))
       (check-equal? (hash-ref settled 'state) "blocked")
       (check-false (file-exists? intent))
       (define after (supplemental-operation-view grant))
       (check-equal? (hash-ref after 'status) 'live)
       (check-equal? (hash-ref (hash-ref after 'operation) 'state) "blocked")))
    (with-fixture
     (lambda (fx)
       ;; Typed recoverable cut: a late GREEN WAL cut derives the exact
       ;; recover-pending view read-only, with the sealed intent preserved.
       (define grant (fixture-grant fx))
       (define intent (operation-path fx ".intent.json"))
       (start-supplemental-operation! grant base-binding)
       (define kref
         (propose-retain! "candidate-proposal"
                          (hasheq 'kind "supplemental-evidence-candidate/v1" 'note "fixture")
                          '()))
       (define cap (approve! kref))
       (step! fx
              "reserved"
              (hasheq 'allocation (hasheq 'attempt-id "a" 'attempt-fence 1 'coordinator-fence 1)))
       (step! fx "launched" (hasheq 'dispatch-id "fixture-dispatch"))
       (step! fx "candidate-sealed" (hasheq 'candidate-ref kref))
       (step! fx "candidate-approved" (hasheq 'candidate-approval-ref cap))
       (step! fx "verifying" (hasheq 'verify-job (hasheq 'command-sha256 (make-string 64 #\0))))
       (dynamic-wind
        (lambda ()
          (current-supplemental-journal-crash-cut (lambda (at)
                                                    (when (eq? at 'intent-written)
                                                      (error 'fixture "simulated crash")))))
        (lambda ()
          (check-exn exn:fail?
                     (lambda ()
                       (step! fx
                              "green-captured"
                              (hasheq 'green-proof-ref
                                      (hasheq 'kind "green-proof" 'sha256 (make-string 64 #\1)))))))
        (lambda () (current-supplemental-journal-crash-cut void)))
       (check-true (file-exists? intent))
       (define before (file->bytes intent))
       (define typed (supplemental-operation-view grant))
       (check-equal? (hash-ref typed 'status) 'pending-wal)
       (check-true (hash-ref typed 'intent-reconcilable?))
       (check-equal? (hash-ref typed 'intent-target) "green-captured")
       (check-equal? (hash-ref (hash-ref typed 'operation) 'state) "recover-pending-verified")
       (check-equal? (file->bytes intent) before)
       ;; The journal's own read-only recovery returns the identical view.
       (define reloaded (load-supplemental-operation grant))
       (check-equal? (hash-ref reloaded 'state) "recover-pending-verified")
       (check-equal? (file->bytes intent) before))))

  ;; ----------------------------------------------------------------
  ;; Part B — controller-level typed discovery + four-value adapter
  ;; ----------------------------------------------------------------
  (test-case "controller discovery types the retained authority; adapter stays compatible"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define grant (fixture-grant-of fx))
       (define pointer-path* (supplemental-pointer-path root plan 1))
       (define pointer-bytes (file->bytes pointer-path*))
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
         ;; Live approved task, not yet launched: typed absent/unlaunched
         ;; with safe identity; the four-value adapter stays deny-only.
         (define d0 (supplemental-discover root plan 1))
         (check-equal? (hash-ref d0 'status) 'absent)
         (check-equal? (hash-ref d0 'cause) 'unlaunched)
         (check-equal? (hash-ref d0 'grant-ref) grant)
         (check-true (hash? (hash-ref d0 'task #f)))
         (check-equal? (call-with-values (lambda () (supplemental-active-operation root plan 1)) list)
                       (list #f #f #f #f))
         (check-true (supplemental-author-pending? root plan 1))
         (check-true (supplemental-pointer-retained? root plan 1))
         ;; Launched operation: typed live, adapter yields the four values.
         (start-supplemental-operation!
          grant
          (hasheq 'context (supplemental-fixture-task-context fx) 'coordinator-fence 8))
         (define d1 (supplemental-discover root plan 1))
         (check-equal? (hash-ref d1 'status) 'live)
         (check-false (hash-ref d1 'cause #f))
         (define-values (op g t tr) (supplemental-active-operation root plan 1))
         (check-equal? op (hash-ref d1 'operation))
         (check-equal? g grant)
         (check-equal? (hash-ref (load-supplemental-record tr) 'kind) "task-proposal")
         (check-true (supplemental-operation-live? root plan 1))
         ;; Context mismatch: the campaign's owned-proof evidence is gone,
         ;; so the context cannot be authenticated against the receipt —
         ;; typed, and never reported as vanished. (The receipt fields the
         ;; conjunction compares are integrity-bound INSIDE the journal, so
         ;; the honest tamper target is the evidence the receipt pins.)
         (define journal-before (file->bytes (delivery-journal-path root plan 1)))
         (define receipt (hash-ref (load-delivery-journal root plan 1) 'receipt))
         (define proof-path
           (build-path (current-owned-proof-root)
                       plan
                       (hash-ref (hash-ref receipt 'owned-proof) 'path)))
         (define proof-bytes (file->bytes proof-path))
         (define proof-mode (file-or-directory-permissions proof-path 'bits))
         (file-or-directory-permissions proof-path #o600)
         (call-with-output-file proof-path (lambda (out) (write-bytes #"x" out)) #:exists 'append)
         (file-or-directory-permissions proof-path proof-mode)
         (define d2 (supplemental-discover root plan 1))
         (check-equal? (hash-ref d2 'status) 'retained-ineligible)
         (check-equal? (hash-ref d2 'cause) 'context-mismatch)
         (check-not-false (hash-ref d2 'operation #f))
         (check-equal? (call-with-values (lambda () (supplemental-active-operation root plan 1)) list)
                       (list #f #f #f #f))
         (check-false (supplemental-operation-live? root plan 1))
         (file-or-directory-permissions proof-path #o600)
         (call-with-output-file proof-path
                                (lambda (out) (write-bytes proof-bytes out))
                                #:exists 'truncate)
         (file-or-directory-permissions proof-path proof-mode)
         (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-before)
         (check-equal? (hash-ref (supplemental-discover root plan 1) 'status) 'live)
         ;; Malformed public pointer: retained ownership, typed denial, and
         ;; the entry seam refuses the ordinary author fallback.
         (file-or-directory-permissions pointer-path* #o600)
         (call-with-output-file pointer-path*
                                (lambda (out) (display "{broken" out))
                                #:exists 'truncate)
         (define d3 (supplemental-discover root plan 1))
         (check-equal? (hash-ref d3 'status) 'retained-ineligible)
         (check-equal? (hash-ref d3 'cause) 'malformed)
         (check-equal? (call-with-values (lambda () (supplemental-active-operation root plan 1)) list)
                       (list #f #f #f #f))
         (check-true (supplemental-pointer-retained? root plan 1))
         (define outcome (supplemental-entry-outcome root plan 1))
         (check-equal? (campaign-result-status outcome) 'wave-blocked)
         (check-regexp-match #rx"retained supplemental task is not launchable"
                             (campaign-result-message outcome))
         ;; Restore, then remove the pointer entirely: typed absent, and the
         ;; entry seam reports no supplemental business.
         (call-with-output-file pointer-path*
                                (lambda (out) (write-bytes pointer-bytes out))
                                #:exists 'truncate)
         (delete-file pointer-path*)
         (define d4 (supplemental-discover root plan 1))
         (check-equal? (hash-ref d4 'status) 'absent)
         (check-equal? (hash-ref d4 'cause) 'absent)
         (check-false (supplemental-entry-outcome root plan 1)))))

    ;; ----------------------------------------------------------------
    ;; Part C — advance failure semantics on the real C1 ladder
    ;; ----------------------------------------------------------------
    (test-case "expiry during a successful C1 wait: no GREEN, no new executions, original cause"
      (call-with-supplemental-fixture
       #:expires-in-sec 120
       (lambda (fx)
         (define root (supplemental-fixture-root fx))
         (define plan (supplemental-fixture-plan fx))
         (define aref (fixture-grant-of fx))
         (define dispatches (box 0))
         (define started (box 0))
         (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
           (define journal-before (delivery-journal-bytes fx))
           (define result
             (with-auto-operator
              (lambda ()
                (with-bus (lambda (event _)
                            (when (eq? event 'gsd.verification.started)
                              (set-box! started (add1 (unbox started)))))
                          (lambda ()
                            (author-run fx
                                        (lambda (_)
                                          (set-box! dispatches (add1 (unbox dispatches)))
                                          ((author-runner fx #:sleep-until #t) _))))))))
           (check-equal? (campaign-result-status result)
                         'wave-failed
                         (campaign-result-message result))
           (define attempts-after-author (wave-attempt-count fx))
           ;; The ORIGINAL cause survives as the primary; the advance returned
           ;; immediately; the post-wait CAS refusal is only secondary.
           (check-regexp-match
            #rx"^supplemental C1 verify failed: .*unexpired operator expiry \\[blocked not persisted: .*unexpired operator expiry\\]$"
            (campaign-result-message result))
           (check-equal? (unbox dispatches) 1)
           (define started-after-c1 (unbox started))
           (check-true (>= started-after-c1 1) "the C1 ladder must have started")
           ;; Zero GREEN committed, zero publication/probe effects.
           (define op (peek-record fx))
           (check-equal? (hash-ref op 'state) "verifying")
           (check-false (hash-has-key? (hash-ref op 'facts) 'green-proof-ref))
           (define staged (load-supplemental-staged-evidence root plan 1))
           (check-false
            (and staged (hash-ref (supplemental-staged-evidence-record staged) 'red-proof-ref #f)))
           (check-equal? (delivery-journal-bytes fx) journal-before)
           (check-equal? (wave-attempt-count fx)
                         attempts-after-author
                         "a failed advance must never allocate a new attempt")
           ;; Retained-but-ineligible is never reported as vanished.
           (define disc (supplemental-discover root plan 1))
           (check-equal? (hash-ref disc 'status) 'retained-ineligible)
           (check-equal? (hash-ref disc 'cause) 'expired)
           (define-values (kind detail) (advance-supplemental-evidence! root plan 1))
           (check-equal? kind 'failed)
           (check-regexp-match #rx"retained supplemental operation is not eligible \\(expired\\)"
                               detail)
           (check-false (string-contains? detail "vanished"))
           (check-equal? (unbox dispatches) 1 "a resumed advance must never redispatch")
           (check-equal? (unbox started) started-after-c1 "a resumed advance must never relaunch C1")
           (check-equal? (hash-ref (peek-record fx) 'state) "verifying")))))

    (test-case "revocation during a successful C1 wait: no GREEN, no new executions"
      (call-with-supplemental-fixture
       (lambda (fx)
         (define root (supplemental-fixture-root fx))
         (define plan (supplemental-fixture-plan fx))
         (define aref (fixture-grant-of fx))
         (define dispatches (box 0))
         (define started (box 0))
         (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
           (define journal-before (delivery-journal-bytes fx))
           (define result
             (with-auto-operator (lambda ()
                                   (with-bus (lambda (event _)
                                               (when (eq? event 'gsd.verification.started)
                                                 (set-box! started (add1 (unbox started)))
                                                 ;; Revoke the task approval DURING the C1 wait.
                                                 (revoke-approval-now! fx)))
                                             (lambda ()
                                               (author-run fx
                                                           (lambda (_)
                                                             (set-box! dispatches
                                                                       (add1 (unbox dispatches)))
                                                             ((author-runner fx) _))))))))
           (check-equal? (campaign-result-status result)
                         'wave-failed
                         (campaign-result-message result))
           (check-regexp-match #rx"live authenticated task approval required"
                               (campaign-result-message result))
           (check-equal? (unbox dispatches) 1)
           (define started-after-c1 (unbox started))
           (check-true (>= started-after-c1 1) "the C1 ladder must have started")
           (define op (peek-record fx))
           (check-equal? (hash-ref op 'state) "verifying")
           (check-false (hash-has-key? (hash-ref op 'facts) 'green-proof-ref))
           (check-equal? (delivery-journal-bytes fx) journal-before)
           (define disc (supplemental-discover root plan 1))
           (check-equal? (hash-ref disc 'status) 'revoked-expired)
           (check-equal? (hash-ref disc 'cause) 'revoked)
           (define readiness (evidence-readiness root plan 1))
           (check-false (equal? (hash-ref readiness 'status) "ready"))
           (define-values (kind detail) (advance-supplemental-evidence! root plan 1))
           (check-equal? kind 'failed)
           (check-regexp-match #rx"not eligible \\(revoked\\)" detail)
           (check-false (string-contains? detail "vanished"))
           (check-equal? (unbox started) started-after-c1)))))

    (test-case "a phase that already advanced the operation reports stale CAS as secondary"
      (call-with-supplemental-fixture
       (lambda (fx)
         (define root (supplemental-fixture-root fx))
         (define plan (supplemental-fixture-plan fx))
         (define aref (fixture-grant-of fx))
         (define private (supplemental-fixture-private fx))
         (define started (box 0))
         (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
           (define result
             (with-auto-operator
              (lambda ()
                (with-bus (lambda (event _)
                            (when (eq? event 'gsd.verification.started)
                              (set-box! started (add1 (unbox started)))
                              ;; Cut the candidate record's read access AFTER the
                              ;; verifying transition: the phase fails with the storage
                              ;; error and its blocked transition is stale.
                              (define kref
                                (hash-ref (hash-ref (load-supplemental-operation aref) 'facts)
                                          'candidate-ref))
                              (define kpath
                                (build-path private (string-append (hash-ref kref 'sha256) ".json")))
                              (file-or-directory-permissions kpath #o000)))
                          (lambda () (author-run fx (lambda (_) ((author-runner fx) _))))))))
           (check-equal? (campaign-result-status result)
                         'wave-failed
                         (campaign-result-message result))
           (check-regexp-match
            #rx"^supplemental C1 verify failed: .*\\[blocked not persisted: .*stale operation revision or state\\]$"
            (campaign-result-message result))
           ;; The blocked transition did NOT persist: the operation stays at
           ;; the durable verifying revision, never falsely claimed blocked.
           (define op (load-supplemental-operation aref))
           (check-equal? (hash-ref op 'state) "verifying")
           (check-false (equal? (hash-ref op 'state) "blocked"))
           (define kref
             (hash-ref (hash-ref (load-supplemental-operation aref) 'facts) 'candidate-ref))
           (file-or-directory-permissions (build-path private
                                                      (string-append (hash-ref kref 'sha256) ".json"))
                                          #o600)
           (check-equal? (unbox started) 1)))))

    (test-case "a concurrent operation lock surfaces as secondary, never as the cause"
      (call-with-supplemental-fixture
       (lambda (fx)
         (define root (supplemental-fixture-root fx))
         (define aref (fixture-grant-of fx))
         (define started (box 0))
         (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
           ;; Hold the journal's operation file lock once the durable
           ;; verifying transition has committed, so the phase's blocked
           ;; transition hits the lock while the ORIGINAL RED failure is
           ;; detected after the capture.
           (define release (make-semaphore))
           (define lock-path (go-operation-path fx ".lock"))
           (define holder
             (thread
              (lambda ()
                (let loop ()
                  (define state
                    (with-handlers ([exn:fail? (lambda (_) #f)])
                      (hash-ref (call-with-input-file (go-operation-path fx) read-json) 'state #f)))
                  (cond
                    [(equal? state "verifying")
                     (let hold ()
                       (call-with-file-lock/timeout #f
                                                    'exclusive
                                                    (lambda () (semaphore-wait release))
                                                    (lambda ()
                                                      (sleep 0.05)
                                                      (hold))
                                                    #:lock-file lock-path))]
                    [else
                     (sleep 0.05)
                     (loop)])))))
           (define result
             (with-auto-operator
              (lambda ()
                (with-bus (lambda (event _)
                            (when (eq? event 'gsd.verification.started)
                              (set-box! started (add1 (unbox started)))))
                          (lambda ()
                            (author-run fx (lambda (_) ((author-runner fx #:red? #t) _))))))))
           (check-equal? (campaign-result-status result)
                         'wave-failed
                         (campaign-result-message result))
           (check-regexp-match
            #rx"^supplemental C1 verify failed: .*\\[blocked not persisted: .*operation lock busy\\]$"
            (campaign-result-message result))
           (semaphore-post release)
           (thread-wait holder)
           (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "verifying")
           (check-equal? (unbox started) 1)))))

    (test-case "a typed pause inside the probe phase remains a pause; a later probe never launches"
      (call-with-supplemental-fixture
       (lambda (fx)
         (define root (supplemental-fixture-root fx))
         (define plan (supplemental-fixture-plan fx))
         (define aref (fixture-grant-of fx))
         (define started (box 0))
         (define staged-bytes (box #f))
         (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
           ;; Escape the wave exactly after the durable green-captured commit,
           ;; then remove the staged wrapper: the green-captured phase must
           ;; PAUSE with its original typed message, never block.
           (let/ec interrupt
             (with-auto-operator
              (lambda ()
                (parameterize ([current-supplemental-journal-crash-cut
                                (lambda (at)
                                  (when (eq? at 'intent-cleared)
                                    (define state
                                      (with-handlers ([exn:fail? (lambda (_) #f)])
                                        (hash-ref (call-with-input-file (go-operation-path fx)
                                                                        read-json)
                                                  'state
                                                  #f)))
                                    (when (equal? state "green-captured")
                                      (set-box! staged-bytes (file->bytes (staged-path fx)))
                                      (delete-file (staged-path fx))
                                      (interrupt (void)))))])
                  (author-run fx (lambda (_) ((author-runner fx) _)))))))
           (check-not-false (unbox staged-bytes) "the green-captured cut was not reached")
           (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "green-captured")
           (check-false (file-exists? (staged-path fx)))
           (define-values (kind detail) (advance-supplemental-evidence! root plan 1))
           (check-equal? kind 'paused detail)
           (check-equal?
            detail
            "supplemental GREEN captured; staged publication wrapper absent; readiness NOT claimed")
           (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "green-captured")
           (check-equal? (unbox started) 0 "no probe ran against the deleted wrapper")
           ;; Restore the wrapper, then revoke the task approval: the typed
           ;; discovery gate refuses the advance BEFORE any phase runs — the
           ;; probe never launches, the staged probe admission stays
           ;; unconsumed, and the denial is typed, never vanished.
           (call-with-output-file (staged-path fx)
                                  (lambda (out) (write-bytes (unbox staged-bytes) out))
                                  #:exists 'truncate)
           (file-or-directory-permissions (staged-path fx) #o600)
           (revoke-approval-now! fx)
           (define journal-before (delivery-journal-bytes fx))
           (define attempts-before (wave-attempt-count fx))
           (define-values (kind2 detail2) (advance-supplemental-evidence! root plan 1))
           (check-equal? kind2 'failed detail2)
           (check-regexp-match #rx"retained supplemental operation is not eligible \\(revoked\\)"
                               detail2)
           (check-false (string-contains? detail2 "vanished"))
           (check-equal? (unbox started) 0 "the probe must never launch on dead authority")
           (check-equal? (delivery-journal-bytes fx) journal-before)
           (check-equal? (wave-attempt-count fx) attempts-before)
           (define readiness (evidence-readiness root plan 1))
           (check-false (equal? (hash-ref readiness 'status) "ready"))
           (define disc (supplemental-discover root plan 1))
           (check-equal? (hash-ref disc 'status) 'revoked-expired)
           (check-equal? (hash-ref disc 'cause) 'revoked)
           (check-false (hash-ref (supplemental-staged-evidence-record
                                   (load-supplemental-staged-evidence root plan 1))
                                  'probe-launch-consumed
                                  #f))))))))
