#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 2400
;; BUG-0101 R101 unit 2: the narrow, separately-reviewed terminal-ONLY
;; observation + usage writer (security-contracts.md 3.2/3.3 handshake, the
;; R101 design gate and the R1/R2 security conditions).
;;
;; Contract under test:
;;   - authentication is the CURRENT coordinator lease/fence (live campaign
;;     record fence token + current wave attempt) plus the exact durable
;;     operation identity (sha256 + bound allocation fact); an expired or
;;     revoked TASK approval never suffices alone and never blocks a
;;     terminal record; stale/unknown fences and an unavailable lease
;;     refuse fail-closed (explicitly pending, never invented authority);
;;   - idempotency: content-addressed key over identity+terminal-facts+
;;     declared-usage; identical replay is one durable observation; the
;;     same job evidence with different facts refuses loudly;
;;   - the observation is NOT a workflow transition: operation revision/
;;     state, WAL, launch permission, eligibility, receipts/proofs are
;;     untouched (full byte/record equality, a post-observation transition
;;     still succeeds, a pending intent is never settled);
;;   - usage accounting at most once per key onto a cumulative private
;;     ledger: known deltas add exactly once, unknown stays explicitly
;;     unknown/pending, prior known contributions are never reset;
;;   - write-cut crash safety: cuts before and after accounting recover on
;;     replay without double-charge or loss; concurrent writers serialize
;;     on the store lock;
;;   - controller integration: the unit-1 typed loss paths best-effort
;;     record captured C1 registry evidence with the CURRENT owner lease;
;;     an observation failure is a distinct secondary, never masking the
;;     primary typed failure and never blocking it.
(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/list
         racket/path
         racket/string
         (only-in "test-gsd-supplemental-evidence-journal.rkt"
                  task
                  with-fixture
                  fixture-grant
                  fixture-root
                  fixture-workspace
                  base-binding
                  operation-path
                  revoke-approval!
                  step!)
         "helpers/supplemental-go-fixture.rkt"
         "../runtime/approval/broker.rkt"
         (only-in "../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-json)
         (only-in "../extensions/gsd/supplemental-evidence-store.rkt" bytes-for read-sealed)
         (only-in "../util/json/checksum.rkt" sha256-string)
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/events.rkt"
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-verification-repair-retries)
         (only-in "../extensions/gsd/review-proof.rkt" current-owned-proof-root)
         (only-in "../extensions/gsd/supplemental-evidence-operator.rkt"
                  current-supplemental-operator-publisher)
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         (only-in "../extensions/gsd/delivery-finalize.rkt" run-coordinator-checkpoint))

;; ------------------------------------------------------------------
;; Writer-level fixture: real retained approval, a reserved operation with
;; its bound allocation, and a real campaign record providing the CURRENT
;; coordinator lease the writer authenticates against.
;; ------------------------------------------------------------------
(define (campaign-plan-id-64)
  (campaign-manifest-hash (make-campaign-manifest 1 "fixture" '() '() "constraints")))

(define (persist-campaign-fixture! workspace
                                   plan
                                   #:fence [fence 8]
                                   #:attempt-id [attempt-id "fixture-new-attempt"]
                                   #:attempt-fence [attempt-fence 4])
  (define rec
    (make-campaign-record plan
                          (make-campaign-manifest 1 "fixture" '() '() "constraints")
                          (list (make-campaign-wave 1
                                                    "Supplemental"
                                                    'in-progress
                                                    1
                                                    (campaign-attempt attempt-id attempt-fence 0)))
                          #f
                          fence
                          'test
                          0
                          0))
  (persist-campaign! workspace rec))

(define (with-obs-fixture proc
                          #:payload
                          [payload (hasheq 'task task 'expires-at (+ (current-seconds) 1800))]
                          #:campaign? [campaign? #t])
  (with-fixture #:payload payload
                (lambda (f)
                  (start-supplemental-operation! (fixture-grant f) base-binding)
                  (step! f 'reserved)
                  (define plan (campaign-plan-id-64))
                  (when campaign?
                    (persist-campaign-fixture! (fixture-workspace f) plan))
                  (proc f plan))))

(define (lease-of f plan)
  (supplemental-coordinator-lease (fixture-workspace f) plan 1))

(define (retained-sha-of f)
  (supplemental-retained-operation-sha (fixture-grant f)))

(define (terminal-evidence-fixture #:state [state 'failed]
                                   #:exit [exit 1]
                                   #:job [job "vj-1"]
                                   #:incarnation [incarnation "fixture-incarnation"]
                                   #:reaped [reaped #t])
  (hasheq
   'state
   state
   'exit-code
   exit
   'reaped?
   reaped
   'log-path
   "/tmp/supplemental-fixture-verify.log"
   'captured-at
   (current-seconds)
   'identities
   (hasheq 'registry-incarnation incarnation 'job-id job 'command-digest "fixture-command-digest")))

(define (observe! f
                  plan
                  #:terminal [terminal (terminal-evidence-fixture)]
                  #:usage [usage #f]
                  #:sha [sha (retained-sha-of f)]
                  #:lease [lease (lease-of f plan)])
  (record-supplemental-terminal-observation! (fixture-grant f)
                                             #:lease lease
                                             #:operation-sha256 sha
                                             #:attempt-id "fixture-new-attempt"
                                             #:attempt-fence 4
                                             #:coordinator-fence 8
                                             #:terminal terminal
                                             #:usage usage))

(define (observation-path f)
  (build-path (fixture-root f)
              "supplemental-terminal-observations"
              (string-append (hash-ref (fixture-grant f) 'sha256) ".json")))

(define (observation-entries record)
  (hash-ref record 'entries))
(define (first-entry record)
  (car (observation-entries record)))

;; ------------------------------------------------------------------
;; Controller fixtures (mirrors the unit-1 advance-failure suite)
;; ------------------------------------------------------------------
(define (put-text repo relative text)
  (define p (build-path repo relative))
  (make-parent-directory* p)
  (display-to-file text p #:exists 'truncate))

;; The scenario's wait lives ONLY in the approved literal helper body: the
;; shared static grammar admits exactly one literal single-assertion
;; test-case in the candidate test module (and no extra top-level defines
;; there), so an expiry-overshooting scenario waits inside (witness) itself.
;; Helper top-level forms stay in the approved require/provide/define set and
;; the helper never contains a check form; the manifest digest is computed
;; from these committed bytes at candidate seal time.
(define (harness-text #:sleep-until [sleep-until #f])
  (string-append
   "#lang racket/base\n"
   "(require \"../../runtime/local.rkt\")\n"
   "(provide witness)\n"
   (if sleep-until
       (format
        "(define (witness)\n  (let loop ()\n    (unless (>= (current-seconds) ~a)\n      (sleep 1)\n      (loop)))\n  (= answer 42))\n"
        sleep-until)
       "(define (witness) (= answer 42))\n")))
(define (witness-text #:red? [red? #f] #:sleep-until [sleep-until #f])
  ;; The evidence-suite candidate is a literal single assertion in every
  ;; scenario; only the helper body and the runner timeout differ.
  (string-append
   "#lang racket\n"
   ";; @speed fast\n;; @suite extensions\n;; @boundary unit\n;; @timeout "
   (if sleep-until "360" "120")
   "\n"
   "(require rackunit \"helpers/witness.rkt\")\n(provide evidence-suite)\n"
   "(define evidence-suite\n  (test-suite \"witness\"\n    (test-case \"new regression\"\n"
   (if red? "      (check-equal? (witness) #f))))\n" "      (check-true (witness)))))\n")
   "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))

(define ((author-runner fx #:red? [red? #f] #:sleep-until [sleep-until #f]) _)
  (define repo (current-directory))
  (put-text repo
            "tests/helpers/witness.rkt"
            (harness-text #:sleep-until (and sleep-until (+ (task-expires-at fx) 1))))
  (put-text repo "tests/test-witness.rkt" (witness-text #:red? red? #:sleep-until sleep-until))
  'ok)

(define (task-expires-at fx)
  (hash-ref (hash-ref (load-supplemental-record
                       (hash-ref (load-supplemental-pointer (supplemental-fixture-root fx)
                                                            (supplemental-fixture-plan fx)
                                                            1)
                                 'task-proposal))
                      'payload)
            'expires-at))

(define (auto-approve-publisher event payload)
  (when (equal? event "supplemental-evidence.approval-requested")
    (thread
     (lambda ()
       (approval-decide! (hash-ref payload 'request-id) (hash-ref payload 'proposal-sha256) #t)))))

(define (with-auto-operator thunk)
  (parameterize ([current-supplemental-operator-publisher auto-approve-publisher])
    (thunk)))

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

(define (fixture-grant-of fx)
  (hash-ref
   (load-supplemental-pointer (supplemental-fixture-root fx) (supplemental-fixture-plan fx) 1)
   'task-approval))

;; Read-only peek of the raw sealed snapshot for post-denial state checks.
(define (go-operation-path fx [suffix ".json"])
  (build-path (supplemental-fixture-private fx)
              "supplemental-operations"
              (string-append (hash-ref (fixture-grant-of fx) 'sha256) suffix)))
(define (peek-record fx)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file (go-operation-path fx) read-json)))

;; Amendment-3 terminalizer helpers: the exact lease-authenticated seal of a
;; spent non-terminal retained operation (mirrors observe! above).
(define (terminalize! f plan #:sha [sha (retained-sha-of f)] #:lease [lease (lease-of f plan)])
  (terminalize-expired-supplemental-operation! (fixture-grant f)
                                               #:lease lease
                                               #:operation-sha256 sha
                                               #:attempt-id "fixture-new-attempt"
                                               #:attempt-fence 4
                                               #:coordinator-fence 8))

;; The durable sealed snapshot, read back byte-exactly (the live journal
;; refuses expired grants, so the historical read is deliberate here).
(define (op-record f)
  (read-sealed (operation-path f)))

(define (expired-obs-fixture proc)
  (with-obs-fixture proc #:payload (hasheq 'task task 'expires-at (+ (current-seconds) 5))))

(define (write-fixture-pointer! f plan)
  (write-supplemental-pointer! (fixture-workspace f)
                               plan
                               1
                               (hash-ref (load-supplemental-record (fixture-grant f)) 'proposal)
                               (fixture-grant f)))

(define (steal-attempt! fx)
  ;; A successor coordinator attempt takes the campaign lease mid-C1.
  (define root (supplemental-fixture-root fx))
  (define plan (supplemental-fixture-plan fx))
  (define rec (load-campaign-record root plan))
  (define w (car (campaign-record-waves rec)))
  (define old (campaign-wave-current-attempt w))
  (set-campaign-fence-token! rec (add1 (campaign-fence-token rec)))
  (set-campaign-wave-current-attempt! w
                                      (campaign-attempt (string-append (campaign-attempt-id old)
                                                                       "-successor")
                                                        (add1 (campaign-attempt-fence-token old))
                                                        0))
  (persist-campaign! root rec))

;; ------------------------------------------------------------------
(module+ test
  ;; --------------------------------------------------------------
  ;; Part A — the narrow authenticated writer (real private storage)
  ;; --------------------------------------------------------------
  (test-case "happy observation persists and reads back with exact facts"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       (define recorded
         (observe! f
                   plan
                   #:usage (hasheq 'input-tokens 10 'output-tokens 5 'total-tokens 15 'cost-usd 25)))
       ;; Exact facts, read back through the reader.
       (check-equal? (supplemental-terminal-observation grant) recorded)
       (check-equal? (hash-ref recorded 'schema) "supplemental-terminal-observation/v1")
       (check-equal? (hash-ref recorded 'grant-ref) grant)
       (check-equal? (hash-ref recorded 'identity)
                     (hasheq 'attempt-id "fixture-new-attempt" 'attempt-fence 4 'coordinator-fence 8))
       (define entry (first-entry recorded))
       (check-equal? (length (observation-entries recorded)) 1)
       (check-equal? (hash-ref (hash-ref entry 'terminal) 'state) "failed")
       (check-equal? (hash-ref (hash-ref entry 'terminal) 'exit-code) 1)
       (check-true (hash-ref (hash-ref entry 'terminal) 'reaped?))
       (check-equal? (hash-ref (hash-ref entry 'terminal) 'identities)
                     (hasheq 'registry-incarnation
                             "fixture-incarnation"
                             'job-id
                             "vj-1"
                             'command-digest
                             "fixture-command-digest"))
       (check-equal? (hash-ref entry 'operation-sha256) (retained-sha-of f))
       (check-equal? (hash-ref (hash-ref recorded 'usage) 'coverage) "known")
       (check-equal? (hash-ref (hash-ref recorded 'usage) 'input-tokens) 10)
       (check-equal? (hash-ref (hash-ref recorded 'usage) 'total-tokens) 15)
       (check-equal? (hash-ref (hash-ref recorded 'usage) 'cost-usd) 25)
       (check-equal? (hash-ref recorded 'accounting-keys) (list (hash-ref entry 'key)))
       (check-true
        (= #o600 (bitwise-and #o7777 (file-or-directory-permissions (observation-path f) 'bits)))))))
  (test-case "the observation is not a workflow transition; eligibility is untouched"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       (define op-bytes-before (file->bytes (operation-path f)))
       (define operation-before (load-supplemental-operation grant))
       (define pre-sha (retained-sha-of f))
       (observe! f plan)
       (check-equal? (file->bytes (operation-path f)) op-bytes-before)
       (check-equal? (load-supplemental-operation grant) operation-before)
       (check-false (hash-ref (load-supplemental-operation grant) 'launch-consumed))
       (step! f 'launched (hasheq 'dispatch-id "post-observation-dispatch"))
       (check-equal? (hash-ref (load-supplemental-operation grant) 'state) "launched")
       ;; A stale digest (the pre-transition operation) refuses.
       (check-exn #rx"stale or unknown operation sha256" (lambda () (observe! f plan #:sha pre-sha)))
       ;; And the same job evidence under the NEW operation revision is
       ;; contradictory: it can never be re-recorded or re-charged.
       (check-exn #rx"contradictory supplemental terminal observation"
                  (lambda () (observe! f plan))))))

  (test-case "identical replay is idempotent: one record, accounting applied once"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       ;; Replay the SAME captured evidence, not a new wall-clock observation.
       (define terminal (terminal-evidence-fixture))
       (define first
         (observe! f
                   plan
                   #:terminal terminal
                   #:usage (hasheq 'input-tokens 7 'output-tokens 3 'total-tokens 10)))
       (define bytes (file->bytes (observation-path f)))
       (define second
         (observe! f
                   plan
                   #:terminal terminal
                   #:usage (hasheq 'input-tokens 7 'output-tokens 3 'total-tokens 10)))
       (check-equal? second first)
       (check-equal? (file->bytes (observation-path f)) bytes)
       (check-equal? (length (observation-entries second)) 1)
       (check-equal? (hash-ref (hash-ref second 'usage) 'input-tokens) 7 "no double-charge")
       (check-equal? (length (hash-ref second 'accounting-keys)) 1))))

  (test-case "contradictory duplicate evidence refuses loudly; the store stays untouched"
    (with-obs-fixture
     (lambda (f plan)
       (observe! f plan)
       (define bytes (file->bytes (observation-path f)))
       (check-exn #rx"contradictory supplemental terminal observation"
                  (lambda () (observe! f plan #:terminal (terminal-evidence-fixture #:exit 2))))
       (check-exn #rx"contradictory supplemental terminal observation"
                  (lambda ()
                    (observe! f plan #:terminal (terminal-evidence-fixture #:state 'cancelled))))
       (check-equal? (file->bytes (observation-path f)) bytes))))

  (test-case "the live task approval alone never authorizes: an absent lease refuses"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       ;; The approval is LIVE here, but there is no coordinator lease: the
       ;; shape-valid proof names a campaign record that does not exist.
       (define unavailable-lease
         (hasheq 'schema
                 "supplemental-coordinator-lease/v1"
                 'base
                 (string->immutable-string (path->string (fixture-workspace f)))
                 'plan
                 plan
                 'wave
                 1
                 'coordinator-fence
                 8
                 'attempt-id
                 "fixture-new-attempt"
                 'attempt-fence
                 4))
       (check-exn #rx"current coordinator lease unavailable"
                  (lambda () (observe! f plan #:lease unavailable-lease)))
       (check-exn #rx"invalid coordinator lease proof" (lambda () (observe! f plan #:lease #f)))
       (check-false (file-exists? (observation-path f)) "nothing is recorded without the lease"))
     #:campaign? #f))

  (test-case "an expired task approval neither authorizes alone nor blocks the lease record"
    (with-obs-fixture (lambda (f plan)
                        (define grant (fixture-grant f))
                        (sleep 6)
                        ;; The task grant is now DEAD: the live journal refuses it fail-closed.
                        (check-exn #rx"unexpired operator expiry"
                                   (lambda () (load-supplemental-operation grant)))
                        ;; But the CURRENT coordinator lease still authenticates the terminal
                        ;; observation of the already-retained operation.
                        (define recorded (observe! f plan))
                        (check-not-false recorded)
                        (check-equal? (supplemental-terminal-observation grant) recorded))
                      #:payload (hasheq 'task task 'expires-at (+ (current-seconds) 5))))

  (test-case "stale lease fences refuse after the campaign moved"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       (define lease (lease-of f plan))
       ;; The coordinator fence advanced after the proof was captured.
       (persist-campaign-fixture! (fixture-workspace f) plan #:fence 9)
       (check-exn #rx"stale coordinator lease fence" (lambda () (observe! f plan #:lease lease)))
       ;; A successor attempt took the wave.
       (persist-campaign-fixture! (fixture-workspace f)
                                  plan
                                  #:fence 8
                                  #:attempt-id "fixture-successor"
                                  #:attempt-fence 5)
       (check-exn #rx"stale coordinator lease attempt" (lambda () (observe! f plan #:lease lease)))
       (check-false (file-exists? (observation-path f))))))

  (test-case "exact identity: unknown operation sha, lease mismatch, allocation mismatch"
    (with-obs-fixture
     (lambda (f plan)
       (define lease (lease-of f plan))
       (define forged (make-string 64 #\0))
       (check-exn #rx"stale or unknown operation sha256" (lambda () (observe! f plan #:sha forged)))
       (check-exn #rx"observation identity does not match the current coordinator lease"
                  (lambda ()
                    (record-supplemental-terminal-observation! (fixture-grant f)
                                                               #:lease lease
                                                               #:operation-sha256 (retained-sha-of f)
                                                               #:attempt-id "fixture-new-attempt"
                                                               #:attempt-fence 4
                                                               #:coordinator-fence 9
                                                               #:terminal
                                                               (terminal-evidence-fixture))))
       ;; Lease and explicit triple agree with each other but not with the
       ;; operation's durably bound allocation.
       (persist-campaign-fixture! (fixture-workspace f)
                                  plan
                                  #:fence 12
                                  #:attempt-id "another-attempt"
                                  #:attempt-fence 11)
       (define other-lease (lease-of f plan))
       (check-exn #rx"stale or unknown attempt fences"
                  (lambda ()
                    (record-supplemental-terminal-observation! (fixture-grant f)
                                                               #:lease other-lease
                                                               #:operation-sha256 (retained-sha-of f)
                                                               #:attempt-id "another-attempt"
                                                               #:attempt-fence 11
                                                               #:coordinator-fence 12
                                                               #:terminal
                                                               (terminal-evidence-fixture)))))))

  (test-case "terminal-ONLY: non-terminal registry state and malformed evidence refuse"
    (with-obs-fixture
     (lambda (f plan)
       (check-exn #rx"terminal-ONLY observation: registry job state is not terminal"
                  (lambda ()
                    (observe! f plan #:terminal (terminal-evidence-fixture #:state 'running))))
       (check-exn #rx"invalid terminal job evidence"
                  (lambda ()
                    (observe! f plan #:terminal (hash-set (terminal-evidence-fixture) 'extra 1))))
       (check-exn #rx"invalid supplemental usage observation"
                  (lambda () (observe! f plan #:usage (hasheq 'bogus 1))))
       (check-false (file-exists? (observation-path f))))))

  (test-case "a pending write-ahead intent is never settled by an observation"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       ;; Leave a real interrupted write-ahead intent behind.
       (check-exn exn:fail?
                  (lambda ()
                    (parameterize ([current-supplemental-journal-crash-cut
                                    (lambda (name)
                                      (when (eq? name 'intent-written)
                                        (error 'crash-cut "simulated crash")))])
                      (step! f 'launched (hasheq 'dispatch-id "interrupted")))))
       (define intent-path
         (build-path (fixture-root f)
                     "supplemental-operations"
                     (string-append (hash-ref grant 'sha256) ".intent.json")))
       (check-true (file-exists? intent-path))
       (define intent-bytes (file->bytes intent-path))
       ;; The sha peek stays #f while the intent is pending; a well-formed but
       ;; unknown digest reaches the writer's unsettled-identity refusal.
       (check-false (supplemental-retained-operation-sha grant))
       (check-exn #rx"pending write-ahead intent; operation identity unsettled"
                  (lambda () (observe! f plan #:sha (make-string 64 #\a))))
       (check-equal? (file->bytes intent-path) intent-bytes "never reconciled or settled")
       (check-false (file-exists? (observation-path f))))))

  (test-case "usage: known once, unknown stays unknown-pending, prior known preserved"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       ;; 1. Known delta applied exactly once (replay checked above too).
       (define r1
         (observe! f
                   plan
                   #:terminal (terminal-evidence-fixture #:job "vj-1")
                   #:usage (hasheq 'input-tokens 10 'output-tokens 5 'total-tokens 15)))
       (check-equal? (hash-ref (hash-ref r1 'usage) 'coverage)
                     "unknown"
                     "the never-observed cost stays explicit unknown coverage")
       (check-equal? (hash-ref (hash-ref r1 'usage) 'cost-usd) #f "unobserved stays pending")
       ;; 2. A second job observed with the explicit unknown marker: prior
       ;; known contributions survive, coverage stays honestly unknown.
       (define r2
         (observe! f
                   plan
                   #:terminal (terminal-evidence-fixture #:job "vj-2" #:incarnation "inc-2")
                   #:usage 'unknown))
       (check-equal? (length (observation-entries r2)) 2)
       (check-equal? (hash-ref (hash-ref r2 'usage) 'input-tokens) 10 "prior known preserved")
       (check-equal? (hash-ref (hash-ref r2 'usage) 'coverage) "unknown")
       (check-equal? (length (hash-ref r2 'accounting-keys)) 2)
       ;; 3. A partial delta (cost never reported) keeps tokens known and
       ;; marks the unknown coverage without resetting anything.
       (define r3
         (observe! f
                   plan
                   #:terminal (terminal-evidence-fixture #:job "vj-3" #:incarnation "inc-3")
                   #:usage (hasheq 'input-tokens 1 'output-tokens 1 'total-tokens 2)))
       (check-equal? (hash-ref (hash-ref r3 'usage) 'input-tokens) 11)
       (check-equal? (hash-ref (hash-ref r3 'usage) 'total-tokens) 17)
       (check-equal? (hash-ref (hash-ref r3 'usage) 'coverage) "unknown"))))

  (test-case "crash cut before accounting recovers exactly once on replay"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       (check-exn
        exn:fail?
        (lambda ()
          (parameterize ([current-supplemental-journal-crash-cut
                          (lambda (name)
                            (when (eq? name 'observation-appended)
                              (error 'crash-cut "simulated crash")))])
            (observe! f plan #:usage (hasheq 'input-tokens 6 'output-tokens 4 'total-tokens 10)))))
       ;; Observation durable, accounting explicitly pending.
       (define pending (supplemental-terminal-observation grant))
       (check-equal? (length (observation-entries pending)) 1)
       (check-equal? (hash-ref pending 'accounting-keys) '())
       (check-equal? (hash-ref (hash-ref pending 'usage) 'coverage) "none")
       ;; Replay performs exactly the missing accounting.
       (define recovered
         (observe! f plan #:usage (hasheq 'input-tokens 6 'output-tokens 4 'total-tokens 10)))
       (check-equal? (length (observation-entries recovered)) 1)
       (check-equal? (length (hash-ref recovered 'accounting-keys)) 1)
       (check-equal? (hash-ref (hash-ref recovered 'usage) 'input-tokens) 6)
       (check-equal? (hash-ref (hash-ref recovered 'usage) 'total-tokens) 10))))

  (test-case "crash cut after accounting recovers without double-charge"
    (with-obs-fixture
     (lambda (f plan)
       (define evidence (terminal-evidence-fixture))
       (check-exn exn:fail?
                  (lambda ()
                    (parameterize ([current-supplemental-journal-crash-cut
                                    (lambda (name)
                                      (when (eq? name 'observation-accounting-applied)
                                        (error 'crash-cut "simulated crash")))])
                      (observe! f plan #:terminal evidence #:usage (hasheq 'cost-usd 150)))))
       (define recovered (observe! f plan #:terminal evidence #:usage (hasheq 'cost-usd 150)))
       (check-equal? (length (observation-entries recovered)) 1)
       (check-equal? (length (hash-ref recovered 'accounting-keys)) 1)
       (check-equal? (hash-ref (hash-ref recovered 'usage) 'cost-usd)
                     150
                     "no double-charge after ack cut"))))

  (test-case "concurrent writers serialize safely on the store lock"
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       (define start (make-semaphore))
       (define results
         (for/list ([_ (in-range 4)])
           (make-channel)))
       (for ([ch (in-list results)])
         (thread (lambda ()
                   (semaphore-wait start)
                   (channel-put
                    ch
                    (with-handlers ([exn:fail? (lambda (_) 'failed)])
                      (observe! f
                                plan
                                #:usage
                                (hasheq 'input-tokens 3 'output-tokens 3 'total-tokens 6)))))))
       (semaphore-post start)
       (semaphore-post start)
       (semaphore-post start)
       (semaphore-post start)
       (define final (channel-get (car results)))
       (check-not-equal? final 'failed)
       (for ([ch (in-list (cdr results))])
         (check-equal? (channel-get ch) final))
       (check-equal? (length (observation-entries final)) 1)
       (check-equal? (length (hash-ref final 'accounting-keys)) 1)
       (check-equal? (hash-ref (hash-ref final 'usage) 'total-tokens)
                     6
                     "four concurrent writers, exactly one charge"))))

  ;; --------------------------------------------------------------
  ;; Part B — controller loss-path integration on the real C1 ladder
  ;; --------------------------------------------------------------
  (test-case "controller C1 loss records the captured registry observation as secondary"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define grant (fixture-grant-of fx))
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
         (define result (with-auto-operator (lambda () (author-run fx (author-runner fx #:red? #t)))))
         ;; The primary typed failure is unchanged: C1's ORIGINAL cause, the
         ;; blocked transition reports its stale-CAS secondary exactly as the
         ;; unit-1 suite pins it (the phase advanced the operation), and the
         ;; observation is a recorded secondary with no pending noise.
         (check-equal? (campaign-result-status result) 'wave-failed)
         (define message (campaign-result-message result))
         (check-true (string-prefix? message "supplemental C1 verify failed:") message)
         (check-true (string-contains? message "blocked not persisted") message)
         (check-false (string-contains? message "terminal observation pending") message)
         ;; The phase advanced the operation before failing; the observation
         ;; carries the REAL registry terminal facts of the failed C1 job.
         (check-equal? (hash-ref (peek-record fx) 'state) "verifying")
         (define obs (supplemental-terminal-observation grant))
         (check-not-false obs)
         (define entry (first-entry obs))
         (check-equal? (hash-ref (hash-ref entry 'terminal) 'state) "failed")
         (check-equal? (hash-ref (hash-ref entry 'terminal) 'exit-code) 1)
         (check-true (hash-ref (hash-ref entry 'terminal) 'reaped?))
         (check-not-false (hash-ref (hash-ref entry 'terminal) 'log-path))
         (define ids (hash-ref (hash-ref entry 'terminal) 'identities))
         (check-true (and (string? (hash-ref ids 'job-id))
                          (string-prefix? (hash-ref ids 'job-id) "vj-")))
         (check-true (and (string? (hash-ref ids 'registry-incarnation))
                          (positive? (string-length (hash-ref ids 'registry-incarnation)))))
         ;; Exact identity: the operation's bound allocation, authenticated
         ;; against the CURRENT lease at loss time.
         (check-equal? (hash-ref (hash-ref obs 'identity) 'attempt-id)
                       (hash-ref (hash-ref (hash-ref (peek-record fx) 'facts) 'allocation)
                                 'attempt-id))
         (check-equal? (hash-ref (hash-ref obs 'identity) 'coordinator-fence) 8)
         ;; Unknown usage stays explicitly unknown/pending.
         (check-equal? (hash-ref (hash-ref obs 'usage) 'coverage) "unknown")
         (check-equal? (hash-ref (hash-ref obs 'usage) 'total-tokens) #f)
         (check-equal? (length (hash-ref obs 'accounting-keys)) 1)))))

  (test-case "authority lost during C1: the observation records although the task expired"
    (call-with-supplemental-fixture
     #:expires-in-sec 120
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define grant (fixture-grant-of fx))
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
         (define result
           (with-auto-operator (lambda () (author-run fx (author-runner fx #:sleep-until #t)))))
         (check-equal? (campaign-result-status result) 'wave-failed)
         ;; The ORIGINAL cause stays primary; the observation neither masks it
         ;; nor adds noise (it recorded successfully).
         (check-true (string-prefix? (campaign-result-message result)
                                     "supplemental C1 verify failed:")
                     (campaign-result-message result))
         (check-false (string-contains? (campaign-result-message result)
                                        "terminal observation pending")
                      (campaign-result-message result))
         (check-true (string-contains? (campaign-result-message result) "blocked not persisted"))
         (check-equal? (hash-ref (peek-record fx) 'state) "verifying")
         (check-false (hash-has-key? (hash-ref (peek-record fx) 'facts) 'green-proof-ref))
         ;; The task approval is expired, yet the terminal observation of the
         ;; actually-completed subprocess job exists — under the live owner
         ;; lease, never the dead task grant.
         (define view (supplemental-discover root (supplemental-fixture-plan fx) 1))
         (check-equal? (hash-ref view 'status) 'retained-ineligible)
         (check-equal? (hash-ref view 'cause) 'expired)
         (define obs (supplemental-terminal-observation grant))
         (check-not-false obs)
         (check-equal? (hash-ref (hash-ref (first-entry obs) 'terminal) 'state) "completed")
         (check-equal? (hash-ref (hash-ref (first-entry obs) 'terminal) 'exit-code) 0)))))

  (test-case "a lease lost mid-C1 reports the observation as explicitly pending"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define grant (fixture-grant-of fx))
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")])
         (define result
           (with-auto-operator
            (lambda ()
              (with-bus (lambda (event _)
                          (when (eq? event 'gsd.verification.started)
                            ;; A successor attempt takes the lease during the C1 wait.
                            (steal-attempt! fx)))
                        (lambda () (author-run fx (author-runner fx #:red? #t)))))))
         (check-equal? (campaign-result-status result) 'wave-failed)
         (define message (campaign-result-message result))
         ;; The primary C1 cause survives; the observation failure is a
         ;; DISTINCT, separately reported secondary.
         (check-true (string-prefix? message "supplemental C1 verify failed:") message)
         (check-true (string-contains? message "terminal observation pending") message)
         (check-true
          (string-contains? message
                            "observation identity does not match the current coordinator lease")
          message)
         ;; The observation was NOT recorded: nothing durable, nothing fake.
         (check-false (supplemental-terminal-observation grant))
         (check-false (file-exists? (build-path (supplemental-fixture-private fx)
                                                "supplemental-terminal-observations"
                                                (string-append (hash-ref grant 'sha256)
                                                               ".json"))))))))

  ;; --------------------------------------------------------------
  ;; Part C — amendment-3: the expired retained-operation terminalizer
  ;; --------------------------------------------------------------
  (test-case "terminalizer seals an expired non-terminal operation blocked under the current lease"
    (expired-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       (sleep 6)
       (define pre-seal-sha (retained-sha-of f))
       (define sealed (terminalize! f plan))
       ;; Exact typed blocked transition: revision +1, launch consumed, and
       ;; the derived typed block-reason — never caller-supplied free text.
       (check-equal? (hash-ref sealed 'state) "blocked")
       (check-equal? (hash-ref sealed 'revision) 2 "reserved(1) + 1")
       (check-true (hash-ref sealed 'launch-consumed))
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'block-reason) "operator deadline expired")
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'allocation)
                     (hasheq 'attempt-id "fixture-new-attempt" 'attempt-fence 4 'coordinator-fence 8))
       (check-equal? (file->bytes (operation-path f)) (bytes-for sealed))
       ;; AMENDMENT-3 digest derivation: terminal-body-sha256 is the record's
       ;; own sha256 field; terminal-record-sha256 is sha256 over the complete
       ;; canonical JSON bytes durably on disk.
       (define body (hash-ref sealed 'sha256))
       (check-true (and (string? body) (regexp-match? #px"^[0-9a-f]{64}$" body)))
       (check-not-equal? body pre-seal-sha)
       (define record-digest (sha256-string (bytes->string/utf-8 (file->bytes (operation-path f)))))
       (check-equal? record-digest (sha256-string (canonical-evidence-json sealed)))
       ;; The sealed record is structurally valid historical storage.
       (check-equal? (op-record f) sealed)
       ;; No WAL and no observation were created by the seal.
       (check-false (file-exists? (operation-path f ".intent.json")))
       (check-false (supplemental-terminal-observation grant)))))

  (test-case "terminalizer seals a late verifying-prefix operation blocked/revision-6 under the current lease"
    ;; The live attempt-6 shape: the expired operation had already traversed
    ;; candidate/verify stages when the operator deadline passed. The same
    ;; supported terminal writer must seal exactly that shape, deriving the
    ;; typed block-reason rather than accepting caller text.
    (with-obs-fixture
     (lambda (f plan)
       (define grant (fixture-grant f))
       ;; with-obs-fixture already started the operation and reserved it.
       (step! f 'launched (hasheq 'dispatch-id "fixture-late-dispatch"))
       (step! f 'candidate-sealed)
       (step! f 'candidate-approved)
       (step! f 'verifying)
       (define before (op-record f))
       (check-equal? (hash-ref before 'state) "verifying")
       (check-equal? (hash-ref before 'revision) 5)
       (check-true (hash-ref before 'launch-consumed))
       (sleep 11)
       (define sealed (terminalize! f plan #:sha (hash-ref before 'sha256)))
       (check-equal? (hash-ref sealed 'state) "blocked")
       (check-equal? (hash-ref sealed 'revision) 6 "verifying(5) + 1")
       (check-true (hash-ref sealed 'launch-consumed))
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'allocation)
                     (hasheq 'attempt-id "fixture-new-attempt" 'attempt-fence 4 'coordinator-fence 8))
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'block-reason) "operator deadline expired")
       ;; Exactly the six verifying-prefix facts the fixed amendment binds.
       (check-equal?
        (sort (hash-keys (hash-ref sealed 'facts)) symbol<?)
        '(allocation block-reason candidate-approval-ref candidate-ref dispatch-id verify-job))
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'candidate-ref)
                     (hash-ref (hash-ref before 'facts) 'candidate-ref))
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'candidate-approval-ref)
                     (hash-ref (hash-ref before 'facts) 'candidate-approval-ref))
       (check-equal? (hash-ref (hash-ref sealed 'facts) 'verify-job)
                     (hash-ref (hash-ref before 'facts) 'verify-job))
       ;; The two amendment-3 digest namespaces remain derivable from this
       ;; authentic sealed record.
       (check-equal? (file->bytes (operation-path f)) (bytes-for sealed))
       (check-equal? (sha256-string (bytes->string/utf-8 (file->bytes (operation-path f))))
                     (sha256-string (canonical-evidence-json sealed)))
       (check-equal? (op-record f) sealed)
       (check-false (file-exists? (operation-path f ".intent.json")))
       (check-false (supplemental-terminal-observation grant)))
     #:payload (hasheq 'task task 'expires-at (+ (current-seconds) 10))))

  (test-case "already-blocked replay is an idempotent no-op; a stale sha refuses"
    (expired-obs-fixture
     (lambda (f plan)
       (sleep 6)
       (define sealed (terminalize! f plan))
       (define bytes (file->bytes (operation-path f)))
       (define replay (terminalize! f plan))
       (check-equal? replay sealed)
       (check-equal? (hash-ref replay 'revision) 2 "no second revision bump")
       (check-equal? (file->bytes (operation-path f)) bytes "byte-identical replay")
       (check-exn #rx"stale or unknown operation sha256"
                  (lambda () (terminalize! f plan #:sha (make-string 64 #\0))))
       (check-equal? (file->bytes (operation-path f)) bytes))))

  (test-case "a stale coordinator lease refuses; nothing is sealed"
    (expired-obs-fixture
     (lambda (f plan)
       (define lease (lease-of f plan))
       (sleep 6)
       (persist-campaign-fixture! (fixture-workspace f) plan #:fence 9)
       (check-exn #rx"stale coordinator lease fence" (lambda () (terminalize! f plan #:lease lease)))
       (check-exn #rx"invalid coordinator lease proof" (lambda () (terminalize! f plan #:lease #f)))
       (check-equal? (hash-ref (op-record f) 'state) "reserved")
       ;; A successor attempt's lease mismatches the bound allocation too.
       (persist-campaign-fixture! (fixture-workspace f)
                                  plan
                                  #:fence 8
                                  #:attempt-id "fixture-successor"
                                  #:attempt-fence 5)
       (check-exn #rx"terminalization identity does not match the current coordinator lease"
                  (lambda () (terminalize! f plan)))
       (check-equal? (hash-ref (op-record f) 'state) "reserved")
       (check-false (file-exists? (operation-path f ".intent.json"))))))

  (test-case "an unexpired live operation refuses terminalization"
    (with-obs-fixture (lambda (f plan)
                        (define bytes (file->bytes (operation-path f)))
                        (check-exn #rx"operation is not expired or revoked; terminalization refused"
                                   (lambda () (terminalize! f plan)))
                        (check-equal? (file->bytes (operation-path f)) bytes)
                        (check-equal? (hash-ref (op-record f) 'state) "reserved"))))

  (test-case "a revoked approval permits the seal on an unexpired deadline"
    (with-obs-fixture (lambda (f plan)
                        (revoke-approval! (fixture-grant f))
                        (define sealed (terminalize! f plan))
                        (check-equal? (hash-ref sealed 'state) "blocked")
                        (check-equal? (hash-ref sealed 'revision) 2)
                        (check-equal? (hash-ref (hash-ref sealed 'facts) 'block-reason)
                                      "operator approval revoked")
                        (check-false (file-exists? (operation-path f ".intent.json")))
                        (check-false (supplemental-terminal-observation (fixture-grant f)))))

    (test-case "a plain entry on the expired retained operation seals it blocked"
      (expired-obs-fixture
       (lambda (f plan)
         (define grant (fixture-grant f))
         (write-fixture-pointer! f plan)
         (sleep 6)
         (define result (supplemental-entry-outcome (fixture-workspace f) plan 1))
         (check-equal? (campaign-result-status result) 'wave-blocked)
         (define message (campaign-result-message result))
         ;; The typed denial is unchanged; the seal is a distinct secondary note.
         (check-true (string-prefix? message "retained supplemental task is not launchable;") message)
         (check-true (string-contains? message "expired retained operation sealed blocked") message)
         (define sealed (op-record f))
         (check-equal? (hash-ref sealed 'state) "blocked")
         (check-equal? (hash-ref sealed 'revision) 2)
         (check-equal? (hash-ref (hash-ref sealed 'facts) 'block-reason) "operator deadline expired")
         ;; No WAL and no observation; the AMENDMENT-3 digests are derivable.
         (check-false (file-exists? (operation-path f ".intent.json")))
         (check-false (supplemental-terminal-observation grant))
         (check-equal? (sha256-string (bytes->string/utf-8 (file->bytes (operation-path f))))
                       (sha256-string (canonical-evidence-json sealed)))
         ;; Replay: a second plain entry is an idempotent no-op.
         (define bytes (file->bytes (operation-path f)))
         (define result2 (supplemental-entry-outcome (fixture-workspace f) plan 1))
         (check-equal? (campaign-result-status result2) 'wave-blocked)
         (check-equal? (file->bytes (operation-path f)) bytes))))

    (test-case "the coordinator checkpoint's retained seam routes through the entry seal"
      (expired-obs-fixture
       (lambda (f plan)
         (write-fixture-pointer! f plan)
         (sleep 6)
         (define wave (car (campaign-record-waves (load-campaign-record (fixture-workspace f) plan))))
         (define advanced (box #f))
         (define blocked (box #f))
         (run-coordinator-checkpoint (fixture-workspace f)
                                     plan
                                     wave
                                     "supplemental resume"
                                     (lambda (_dir _id _w)
                                       (error 'coordinator "ordinary fallback must never run"))
                                     #:on-advance (lambda () (set-box! advanced #t))
                                     #:on-blocked (lambda (why) (set-box! blocked why)))
         (check-false (unbox advanced) "the seal must not launch anything")
         (define result (unbox blocked))
         (check-equal? (campaign-result-status result) 'wave-blocked)
         (check-true (string-contains? (campaign-result-message result)
                                       "expired retained operation sealed blocked")
                     (campaign-result-message result))
         (check-equal? (hash-ref (op-record f) 'state) "blocked")
         (check-equal? (hash-ref (hash-ref (op-record f) 'facts) 'block-reason)
                       "operator deadline expired"))))

    (test-case "without a current lease the seal stays explicitly pending, nothing written"
      (with-obs-fixture (lambda (f plan)
                          (write-fixture-pointer! f plan)
                          (sleep 6)
                          (define result (supplemental-entry-outcome (fixture-workspace f) plan 1))
                          (check-equal? (campaign-result-status result) 'wave-blocked)
                          (check-true (string-contains? (campaign-result-message result)
                                                        "expired operation seal pending")
                                      (campaign-result-message result))
                          (check-true (string-contains? (campaign-result-message result)
                                                        "current coordinator lease unavailable")
                                      (campaign-result-message result))
                          (check-equal? (hash-ref (op-record f) 'state) "reserved")
                          (check-false (file-exists? (operation-path f ".intent.json"))))
                        #:payload (hasheq 'task task 'expires-at (+ (current-seconds) 5))
                        #:campaign? #f))))
