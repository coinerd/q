#lang racket/base
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @isolation process
;; @mutates repo
;; @timeout 1500
;; @covers scripts/gsd-delivery-auto-review.rkt scripts/gsd-binding-data.rkt scripts/gsd-gates/owned-audit.rkt extensions/gsd/review-proof.rkt
;;
;; Native port of the deleted `tests/test-gsd-owned-audit.py` (thirteen
;; cases) plus the "blocked readiness cannot downgrade authentic legacy
;; authority" case its binding shim carried inline. Each case names the Python
;; case it replaces and keeps its assertion set.
;;
;; Real-Git owned audit consumer fixtures, not live W1/model acceptance.
;; Positive path: an actual compiled implementation with wrong behavior plus a
;; failing focused test produces an authentic registered behavioral RED at the
;; committed pre-fix head; the SAME function is then fixed, the real frozen
;; Verify chain (raco fmt -i, raco make, honest fixture run-tests/lint-all
;; runners, --suite fast) is executed by the owned registry, and the approved
;; audit — exact audit-ack plus meaningful red-ack of the retained raw failure
;; output — finalizes through the unchanged strict gate and publishes with
;; ZERO duplicate Verify launches. F11 (operator merge authorization) remains a
;; separate gate and is never manufactured. Missing RED fails closed as typed
;; review-infra and is never consumed as a rejection or approval.
;;
;; Worlds (built ONCE per suite by `tests/helpers/owned-audit-fixture.rkt`, a
;; real owned Verify run each): `nored` has no retained RED, `infra` and
;; `strict` carry a linked RED, `positive` carries the approved-audit world the
;; binding case then continues on (approval publishes the implementation
;; verdict; the post-merge binding draft is the production next step). A
;; published verdict ends a world's implementation-review generation under the
;; explicit reject-retry law, so no world is shared across independent verdicts
;; and every case restores the mutations it makes. Worlds are NOT copied per
;; case: an owned manifest authenticates the absolute execution paths it was
;; produced under, so a copy is either a different world or a tampered one.
;;
;; Measured wall clock on a loaded 4-CPU host (concurrent unrelated test
;; processes): ~5.5 minutes total, of which ~3.5 minutes is the five real owned
;; Verify world builds.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         "../scripts/gsd-delivery-auto-review.rkt"
         (only-in "../scripts/gsd-delivery-pr.rkt"
                  content-digest
                  make-delivery-pr-context
                  delivery-pr-context-run)
         (only-in "../scripts/gsd-delivery-git.rkt" exn:fail:delivery-action?)
         (only-in "../scripts/gsd-delivery-status.rkt" binding-branch)
         (only-in "../scripts/gsd-delivery-merge.rkt" draft-request)
         (only-in "../scripts/gsd-delivery-binding.rkt"
                  binding-staging-path
                  delivery-binding-review!
                  staged-bundle-files
                  empty-sha)
         (only-in "../extensions/gsd/evidence-readiness.rkt" evidence-readiness)
         (only-in "../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-exit-code
                  subprocess-result-stdout
                  subprocess-result-stderr)
         "helpers/owned-audit-fixture.rkt")

(provide worlds
         audit-output
         strict-trio
         gate-run!)

;; ============================================================
;; Fixture plumbing
;; ============================================================

;; One world per suite, built on first use inside the fixture-local HOME.
(define worlds (make-hash))

(define (world-for key build)
  (hash-ref! worlds key (lambda () (with-owned-proof-root build))))

;; Per-suite worlds. Each accessor builds its world once on first use, so a
;; `QAR_ONLY` subset run still gets a real world, and the cases that share a
;; world never rebuild it.
(define (nored-world)
  (world-for 'nored
             (lambda ()
               (build-owned-world (make-temporary-file "q-owned-audit-nored-~a" 'directory)
                                  #:with-red? #f))))
;; ONE linked-RED world serves the negative matrix, the offline strict-gate
;; matrix and the honest rejection. Order matters only in one direction: the
;; rejection case PUBLISHES a verdict, so it runs last (the file order below),
;; and every other case on this world leaves the remote tip at head2.
(define (linked-red-world)
  (world-for 'linked-red
             (lambda ()
               (build-owned-world (make-temporary-file "q-owned-audit-linked-~a"
                                                       'directory)))))
(define (positive-world)
  (world-for 'positive
             (lambda ()
               (build-owned-world (make-temporary-file "q-owned-audit-positive-~a" 'directory)))))
(define (readiness-world)
  (world-for 'readiness
             (lambda ()
               (build-owned-world (make-temporary-file "q-owned-audit-readiness-~a"
                                                       'directory)))))

;; Every in-process owned step runs under the fixture's trusted proof root so
;; the in-process capture and the `gsd-binding-data.rkt` subprocesses agree.
(define-syntax-rule (owned body ...)
  (with-owned-proof-root (lambda () body ...)))

(define (plan-of world) (hash-ref world 'plan))
(define (campaign-of world) (hash-ref world 'campaign))
(define (repo-of world) (hash-ref world 'subject))
(define (staging-of world) (hash-ref world 'staging))

(define (remote-tip world)
  (first (string-split (git! #f
                             "-C"
                             (path->string (hash-ref world 'origin))
                             "rev-parse"
                             (format "refs/heads/~a" (hash-ref world 'branch)))
                       "\n")))

(define (prepare! world [stage "implementation-review"])
  (define generation 0)
  (define staging
    (if (equal? stage "binding-review")
        (binding-staging-path (campaign-of world) (plan-of world) (hash-ref world 'wave) generation)
        (staging-of world)))
  (owned (auto-review! (repo-of world)
                       (plan-of world)
                       (hash-ref world 'wave)
                       "prepare"
                       stage
                       (hash-ref world 'head2)
                       (hash-ref world 'branch)
                       (campaign-of world)
                       generation
                       staging)))

(define (finalize! world output [stage "implementation-review"])
  (define generation 0)
  (define staging
    (if (equal? stage "binding-review")
        (binding-staging-path (campaign-of world) (plan-of world) (hash-ref world 'wave) generation)
        (staging-of world)))
  (make-directory* staging)
  (define reviewer (build-path staging "reviewer-output.json"))
  (put-json! reviewer output)
  (owned (auto-review! (repo-of world)
                       (plan-of world)
                       (hash-ref world 'wave)
                       "finalize"
                       stage
                       (hash-ref world 'head2)
                       (hash-ref world 'branch)
                       (campaign-of world)
                       generation
                       staging
                       #:reviewer-output reviewer)))

;; The approved/findings reviewer output the controller reads: a genuine
;; non-sentinel reviewer identity, real narratives and — in owned audit mode —
;; an exact audit acknowledgement (never model-authored gate facts).
(define (reviewer-output [kind "approved"] [findings #f])
  (define base
    (hash 'kind
          kind
          'reviewer
          "fresh-reviewer-session"
          'scope
          (string-append "independent review of the implementation, gates and provenance at the "
                         "pinned receipt head with fresh session context")
          'report
          (string-append "ran the focused and fast gates at the pinned head and verified the "
                         "provenance chain; no blocking defect found")))
  (cond
    [(equal? kind "request-changes")
     (hash-set base 'findings (or findings (list "focused tests fail at the head")))]
    [(equal? kind "approved")
     (hash-set base
               'validation
               (hash 'red-first
                     (hash 'command
                           "raco test tests/test-wave-fixture.rkt"
                           'failure
                           (string-append "reproduction observed: the malformed digest passed the "
                                          "gate before the fix"))
                     'focused-tests
                     (hash 'result "passed" 'command "raco test tests/test-wave.rkt")
                     'format-compile
                     (hash 'result "passed" 'command "raco fmt --check x.rkt")
                     'lint
                     (hash 'result "passed" 'command "raco make")
                     'fast
                     (hash 'result "passed" 'command "raco test tests/fast")
                     'remaining-items
                     '()
                     'planning-sync
                     "current"))]
    [else (hash-set base 'reason "reviewer session timed out")]))

;; The audit acknowledgement a genuine auditor returns: the exact pinned audit
;; binding plus the LINKED RED identity and a raw-output read.
(define (audit-output world request [kind "approved"])
  (define authority (hash-ref request 'owned-authority))
  (define red (hash-ref authority 'red))
  (define out (reviewer-output kind))
  (define ack
    (hash 'mode
          owned-audit-mode
          'verification-origin
          "audited-owned"
          'source-read
          #t
          'raw-output-read
          #t
          'binding
          (audit-binding request)
          'red-ack
          (hash 'proof-sha256 (hash-ref (hash-ref red 'reference) 'sha256) 'raw-output-read #t)))
  (hash-set (hash-remove out 'validation)
            'audit-ack
            ack))

;; One audit-ack key replacement.
(define (dig-hash value key)
  (and (hash? value) (hash-ref value key #f)))

(define (hash-merge* base extra)
  (for/fold ([acc base]) ([(key value) (in-hash extra)]) (hash-set acc key value)))

;; Python's `dict == dict`: two JSON datums are equal when their canonical
;; JSON renderings are (a datum read through the trusted reader is hasheq-based
;; while a locally built one is equal-based — the KINDS differ, the DATA does
;; not).
(define (json-equal? left right)
  (equal? (jsexpr->string left) (jsexpr->string right)))

;; The pinned audit identity keys, in the controller's own order (Python
;; `AUDIT_KEYS`). Case "no model gate fact fallback or unbound ack" asserts
;; this list still equals the keys the controller's `audit-binding` emits, so
;; the copy can never drift silently.
(define audit-key-symbols
  '(plan-id
    wave
    generation
    head
    tree
    branch
    attempt-id
    attempt-fence
    content-digest
    proof-sha256))

;; The pinned audit identity keys (Python `AUDIT_KEYS`), taken from the
;; controller's own binding builder instead of a second copy of the list.
(define (audit-keys-of request)
  (sort (for/list ([(key _value) (in-hash (audit-binding request))]) key) symbol<?))

;; A stale-fact variant: replace one audit-ack key with a stale value.
(define (with-stale-ack output key value)
  (hash-set output 'audit-ack (hash-set (hash-ref output 'audit-ack) key value)))

;; The controller-shaped finalized trio for the OFFLINE strict gate: every gate
;; fact from the trusted authority, never from model prose (Python
;; `OwnedAuditTests.strict_trio`).
(define (strict-trio world request root)
  (define authority (hash-ref request 'owned-authority))
  (define red (hash-ref authority 'red))
  (define plan (plan-of world))
  (define branch (hash-ref world 'branch))
  (define head (hash-ref world 'head2))
  (define binding (audit-binding request))
  (define digest (hash-ref request 'content-digest))
  (define common
    (hash 'review-mode owned-audit-mode
          'owned-proof
          (hash-ref authority 'reference)
          'audit-binding
          binding
          'owned-red
          (hash-ref red 'reference)))
  (define paths
    (hash 'review
          "docs/reports/gsd-wave-reviews/strict.rktd"
          'validation
          "docs/reports/gsd-wave-validation/strict.rktd"
          'evidence
          "docs/reports/gsd-wave-evidence/strict.rktd"))
  (define evidence
    (hash-merge* common
                 (hash 'schema-version
                      2
                      'plan-id
                      plan
                      'wave
                      "W1"
                      'milestone
                      895
                      'issue
                      9686
                      'status
                      "ready-for-merge"
                      'branch
                      branch
                      'implementation-sha
                      head
                      'content-digest
                      digest
                      'required-checks
                      policy-names
                      'review-artifact
                      (hash-ref paths 'review)
                      'validation-artifact
                      (hash-ref paths 'validation))))
  (define out (audit-output world request))
  (define review
    (hash-merge* common
                 (hash 'reviewer
                      (hash-ref out 'reviewer)
                      'verdict
                      "APPROVED"
                      'timestamp
                      "2026-09-14T16:00:00Z"
                      'reviewed-sha
                      head
                      'content-digest
                      digest
                      'scope
                      (hash-ref out 'scope)
                      'report
                      (hash-ref out 'report)
                      'audit-ack
                      (hash-ref out 'audit-ack))))
  (define gates (owned-approval-gates #f authority))
  (define validation
    (hash-merge* common
                 (hash 'status
                      "current"
                      'planning-sync
                      "current"
                      'milestone
                      895
                      'wave
                      "W1"
                      'issue
                      9686
                      'branch
                      branch
                      'implementation-sha
                      head
                      'content-digest
                      digest
                      'red-first
                      (hash-ref gates 'red-first)
                      'focused-tests
                      (hash-ref gates 'focused-tests)
                      'format-compile
                      (hash-ref gates 'format-compile)
                      'lint
                      (hash-ref gates 'lint)
                      'fast
                      (hash-ref gates 'fast)
                      'review-artifact
                      (hash-ref paths 'review)
                      'remaining-items
                      '())))
  (write-datum! evidence (build-path root (hash-ref paths 'evidence)))
  (write-datum! review (build-path root (hash-ref paths 'review)))
  (write-datum! validation (build-path root (hash-ref paths 'validation)))
  ;; The offline validator opens only the portable bundle: copy the addressed
  ;; manifest/raw-log files from the staging export.
  (for ([ref (in-list (list (hash-ref authority 'reference)
                            (hash-ref authority 'log-reference)
                            (hash-ref (hash-ref authority 'red) 'reference)
                            (hash-ref (hash-ref authority 'red) 'log-reference)))])
    (define relative (string-append (hash-ref authority 'bundle) "/" (hash-ref ref 'path)))
    (define target (build-path root relative))
    (unless (file-exists? target)
      (make-directory* (path-only target))
      (copy-file (build-path (hash-ref world 'staging) relative) target)))
  (put-text! (build-path root "scripts/required-pr-checks.policy")
             (format "(\"~a\")" (string-join '("lint" "test (0)" "workflows (0)") "\" \"")))
  (hash-set paths 'digest digest))

;; Run the unchanged strict gate offline over a materialized trio root.
(define (gate-run! root trio [authority #f])
  (define payload (build-path root "authority.json"))
  (when authority (put-json! payload authority))
  (define args
    (append (list "--content-digest"
                  (hash-ref trio 'digest)
                  "--root"
                  (path->string root)
                  "--policy"
                  (path->string (build-path root "scripts/required-pr-checks.policy")))
            (if authority
                (list "--owned-proof-authority" (path->string payload))
                '())))
  (run-gate! (build-path root (hash-ref trio 'evidence)) args))

(define (gate-output result)
  (string-append (subprocess-result-stdout result) (subprocess-result-stderr result)))

;; The store of one world's owned proofs (the manifest kinds live here).
(define (store-files world)
  (for/list ([path (in-directory (hash-ref world 'store))]
             #:when (file-exists? path)
             #:when (equal? (path-get-extension path) #".json"))
    path))

(define (store-kinds world)
  (for/list ([path (in-list (store-files world))])
    (hash-ref (read-json-file path) 'kind #f)))

(define progress-box (box #f))

;; Optional progress tracing (`QAR_PROGRESS=1`): a host-level subprocess stall
;; then names the case that was running instead of hiding it (the same
;; discipline as tests/test-gsd-delivery-auto-review-native.rkt).
(define (record-progress! name)
  (when (getenv "QAR_PROGRESS")
    (define path (build-path (find-system-path 'temp-dir) "q-owned-audit-progress.log"))
    (define rss
      (with-handlers ([exn:fail? (lambda (_error) "?")])
        (define status (file->string "/proc/self/status"))
        (define match (regexp-match #px"VmRSS:\\s+([0-9]+) kB" status))
        (if match (cadr match) "?")))
    (call-with-output-file path
                           (lambda (out) (fprintf out "~a (rss ~a kB)\n" name rss))
                           #:exists (if (unbox progress-box) 'append 'truncate))
    (set-box! progress-box #t)))

;; `QAR_ONLY=<substring>` runs any subset in isolation, e.g.
;; `QAR_ONLY=binding raco test tests/test-gsd-owned-audit.rkt`. Each case owns
;; the worlds it needs, so a subset run still builds real worlds.
(define-syntax-rule (native-case name body ...)
  (test-case name
    (record-progress! name)
    (define qar-only (getenv "QAR_ONLY"))
    (when (or (not qar-only) (string-contains? name qar-only)) body ...)))

;; ============================================================
;; Pure audit laws (no world)
;; ============================================================

(module+ test
  (native-case "existing prompt audits instead of replaying owned gates"
    ;; Python `test_existing_prompt_audits_instead_of_replaying_owned_gates`.
    (define paths
      (hash 'evidence "e" 'review "r" 'validation "v"))
    (for ([stage (in-list auto-review-stages)])
      (define prompt
        (review-prompt stage
                       (make-string 64 #\a)
                       1
                       (make-string 40 #\b)
                       "campaign/w1"
                       paths
                       "/staging"
                       (make-string 64 #\c)
                       '("lint")))
      (check-true (string-contains? prompt "AUDIT"))
      (check-true (string-contains? prompt "Do not rerun"))
      (check-true (string-contains? prompt "600 seconds"))
      (check-false (string-contains? prompt "Run the genuine gates"))))

  (native-case "no model gate fact fallback or unbound ack"
    ;; Python `test_no_model_gate_fact_fallback_or_unbound_ack`.
    (define request
      (for/fold ([acc (hash 'review-mode owned-audit-mode)]) ([key (in-list audit-key-symbols)])
        (hash-set acc key "x")))
    ;; The pinned key list above must still be the controller's binding keys.
    (check-equal? (audit-keys-of (hash-set request 'owned-authority #f))
                  (sort audit-key-symbols symbol<?))
    (define out (reviewer-output))
    ;; Model-authored gate facts are typed review-infra, never a fallback.
    (check-exn exn:fail:review-infra?
               (lambda () (validate-reviewer-output out request)))
    (define ack
      (hash 'mode
            owned-audit-mode
            'verification-origin
            "audited-owned"
            'source-read
            #t
            'raw-output-read
            #t
            'binding
            (audit-binding request)))
    (define audited
      (hash-set (hash-remove out 'validation) 'audit-ack ack))
    (check-equal? (hash-ref (validate-reviewer-output audited request) 'kind) "approved")
    (for ([key (in-list audit-key-symbols)])
      (check-exn exn:fail:review-infra?
                 (lambda ()
                   (validate-reviewer-output
                    (hash-set audited
                              'audit-ack
                              (hash-set ack
                                        'binding
                                        (hash-set (hash-ref ack 'binding) key "stale")))
                    request))))
    (for ([key (in-list '(source-read raw-output-read verification-origin mode))])
      (check-exn exn:fail:review-infra?
                 (lambda ()
                   (validate-reviewer-output
                    (hash-set audited 'audit-ack (hash-set ack key #f))
                    request)))))

  (native-case "red ack is meaningful only for a linked red"
    ;; Python `test_red_ack_is_meaningful_only_for_a_linked_red`.
    (define red-sha (make-string 64 #\r))
    (define request
      (for/fold ([acc
                  (hash 'review-mode
                        owned-audit-mode
                        'owned-authority
                        (hash 'red (hash 'reference (hash 'sha256 red-sha))))])
                 ([key (in-list audit-key-symbols)])
        (hash-set acc key "x")))
    (define out (reviewer-output))
    (define ack
      (hash 'mode
            owned-audit-mode
            'verification-origin
            "audited-owned"
            'source-read
            #t
            'raw-output-read
            #t
            'binding
            (audit-binding request)))
    (define with-red
      (hash-set (hash-remove out 'validation)
                'audit-ack
                (hash-set ack
                          'red-ack
                          (hash 'proof-sha256 red-sha 'raw-output-read #t))))
    (check-equal? (hash-ref (validate-reviewer-output with-red request) 'kind) "approved")
    (for ([bad (in-list (list (hash 'proof-sha256 "wrong" 'raw-output-read #t)
                              (hash 'proof-sha256 red-sha 'raw-output-read #f)
                              (hash 'proof-sha256 red-sha)))])
      (check-exn exn:fail:review-infra?
                 (lambda ()
                   (validate-reviewer-output
                    (hash-set with-red 'audit-ack (hash-set ack 'red-ack bad))
                    request))))
    ;; The same red-ack without a linked owned red in the request is a stop.
    (check-exn exn:fail:review-infra?
               (lambda ()
                 (validate-reviewer-output with-red (hash-remove request 'owned-authority)))))

  (native-case "binding bundle collector is safe and posix-relative"
    ;; Python `test_binding_bundle_collector_is_safe_and_posix_relative`.
    (define staging (make-temporary-file "q-bundle-collect-~a" 'directory))
    (define sha (make-string 64 #\a))
    (define proofs (build-path staging "docs" "reports" "gsd-wave-validation" "owned-proofs"))
    (define bundle (build-path proofs sha))
    (make-directory* bundle)
    (for ([name (in-list (list (string-append sha ".json")
                               (string-append sha ".log")
                               "authority.json"))])
      (put-text! (build-path bundle name) "{}"))
    (define files (staged-bundle-files staging))
    (check-equal? (sort (hash-keys files) string<?)
                  (sort (for/list ([name (in-list (list (string-append sha ".json")
                                                        (string-append sha ".log")
                                                        "authority.json"))])
                          (format "docs/reports/gsd-wave-validation/owned-proofs/~a/~a"
                                  sha
                                  name))
                        string<?))
    (make-file-or-directory-link "/etc" (build-path bundle "escape"))
    (define escaped (staged-bundle-files staging))
    (check-false (hash-has-key? escaped
                                (format "docs/reports/gsd-wave-validation/owned-proofs/~a/escape"
                                        sha)))
    (check-equal? (staged-bundle-files (build-path staging "missing")) #hash()))

  ;; ============================================================
  ;; Missing RED fails closed (no-RED world)
  ;; ============================================================

  (native-case "missing red fails closed and is never consumed"
    ;; Python `NoRedWorldTests.test_missing_red_fails_closed_and_is_never_consumed`.
    (define world (nored-world))
    (define prepared (prepare! world))
    (check-equal? (hash-ref prepared 'status) "awaiting-review")
    (define request (hash-ref prepared 'request))
    (check-false (hash-ref (hash-ref request 'owned-authority) 'red))
    (check-true (string-contains? (hash-ref request 'prompt)
                                  "No behavioral RED proof is linked"))
    (define out
      (hash-set (hash-remove (reviewer-output) 'validation)
                'audit-ack
                (hash 'mode
                      owned-audit-mode
                      'verification-origin
                      "audited-owned"
                      'source-read
                      #t
                      'raw-output-read
                      #t
                      'binding
                      (audit-binding request))))
    (define result (finalize! world out))
    (check-equal? (hash-ref result 'status) "review-infra")
    (check-true (string-contains? (hash-ref result 'reason) "missing-meaningful-red-proof"))
    (check-equal? (remote-tip world) (hash-ref world 'head2))
    ;; Recovery: the next prepare still dispatches, and a fabricated red-ack is
    ;; still a typed infra stop (never a rejection, never an approval).
    (check-equal? (hash-ref (prepare! world) 'status) "awaiting-review")
    (check-equal? (hash-ref (finalize! world
                                       (with-stale-ack out
                                                       'red-ack
                                                       (hash 'proof-sha256 (make-string 64 #\f)
                                                             'raw-output-read
                                                             #t)))
                             'status)
                  "review-infra"))

  ;; ============================================================
  ;; Linked-RED negative matrix
  ;; ============================================================

  (native-case "stale or missing red ack and model gate prose are infra"
    ;; Python `RedInfraStopTests.test_stale_or_missing_red_ack_and_model_gate_prose_are_infra`.
    (define world (linked-red-world))
    (define request (hash-ref (prepare! world) 'request))
    (define audited (audit-output world request))
    (check-equal? (hash-ref (finalize! world
                                       (hash-set audited
                                                  'audit-ack
                                                  (hash-remove (hash-ref audited 'audit-ack)
                                                               'red-ack)))
                            'status)
                  "review-infra")
    (check-equal? (hash-ref (finalize! world
                                       (with-stale-ack (audit-output world request)
                                                       'red-ack
                                                       (hash 'proof-sha256 (make-string 64 #\0)
                                                             'raw-output-read
                                                             #t)))
                            'status)
                  "review-infra")
    ;; Model-authored gate facts are never consumed as gate facts.
    (define with-validation (hash-set (audit-output world request)
                                      'validation
                                      (hash-ref (reviewer-output) 'validation)))
    (check-equal? (hash-ref (finalize! world with-validation) 'status) "review-infra")
    (check-equal? (remote-tip world) (hash-ref world 'head2)))

  (native-case "stale ack binding generation is also infra"
    ;; Python `RedInfraStopTests.test_stale_ack_binding_generation_is_also_infra`.
    (define world (linked-red-world))
    (define request (hash-ref (prepare! world) 'request))
    (define out (audit-output world request))
    (check-equal? (hash-ref (finalize! world
                                       (with-stale-ack out
                                                       'binding
                                                       (hash-set (dig-hash (hash-ref out 'audit-ack)
                                                                           'binding)
                                                                 'generation
                                                                 1)))
                            'status)
                  "review-infra"))

  (native-case "request proof and tree drift are typed stops"
    ;; Python `RedInfraStopTests.test_request_proof_and_tree_drift_are_typed_stops`.
    (define world (linked-red-world))
    (define staging (staging-of world))
    (define path (build-path staging "request.json"))
    (define request (hash-ref (prepare! world) 'request))
    (define original (file->bytes path))
    (for ([key (in-list '(proof-sha256 tree attempt-id attempt-fence))])
      (define bad (hash-set request key (if (equal? key 'attempt-fence) 999 "wrong")))
      (put-json! path bad)
      (define outcome
        (with-handlers ([exn:fail:delivery-action? (lambda (error) error)])
          (finalize! world (audit-output world request))))
      (cond
        [(exn:fail:delivery-action? outcome)
         ;; A durable-receipt identity mismatch refuses before any write.
         (check-true (string-contains? (exn-message outcome) "receipt")
                     (format "~a" key))]
        [else (check-equal? (hash-ref outcome 'status) "review-infra" (format "~a" key))])
      (call-with-output-file path #:exists 'truncate (lambda (out) (write-bytes original out)))
      (void)))

  (native-case "publication guard rechecks the raw proof after fetch and before push"
    ;; Python `RedInfraStopTests.test_publication_guard_rechecks_raw_proof_after_fetch_before_push`.
    (define world (linked-red-world))
    (define request (hash-ref (prepare! world) 'request))
    (define authority (hash-ref request 'owned-authority))
    (define raw (build-path (hash-ref world 'store)
                            (hash-ref (hash-ref authority 'log-reference) 'path)))
    (define original (file->bytes raw))
    (define (drift-guard)
      (file-or-directory-permissions raw #o600)
      (call-with-output-file raw
                             #:exists 'append
                             (lambda (out) (write-bytes #"drift-before-push" out)))
      (reauthenticate-owned-request request
                                    (delivery-pr-context-run (make-delivery-pr-context (repo-of world)))
                                    (campaign-of world)
                                    (plan-of world)
                                    (hash-ref world 'wave)
                                    (staging-of world)
                                    #f))
    (dynamic-wind
     void
     (lambda ()
       (check-exn exn:fail:review-infra?
                  (lambda ()
                    (owned (publish-candidate! (make-delivery-pr-context (repo-of world))
                                              (repo-of world)
                                              (hash-ref world 'branch)
                                              (hash-ref world 'head2)
                                              (hash-ref world 'head2)
                                              drift-guard)))))
     (lambda ()
       (call-with-output-file raw #:exists 'truncate (lambda (out) (write-bytes original out)))
       (file-or-directory-permissions raw #o400)))
    ;; The remote was never moved by the refused publication.
    (check-equal? (remote-tip world) (hash-ref world 'head2)))

  ;; ============================================================
  ;; Strict offline gate + genuine rejection
  ;; ============================================================

  (native-case "tampered red bundle bytes fail offline strict validation"
    ;; Python `RedStrictGateTests.test_tampered_red_bundle_bytes_fail_offline_strict_validation`.
    (define world (linked-red-world))
    (define request (hash-ref (prepare! world) 'request))
    (define authority (hash-ref request 'owned-authority))
    (define root (make-temporary-file "q-strict-~a" 'directory))
    (define trio (strict-trio world request root))
    (define gate (gate-run! root trio authority))
    (check-equal? (subprocess-result-exit-code gate) 0 (gate-output gate))
    ;; Model-authored red facts (prose instead of retained bytes) fail.
    (define tampered
      (hash-set authority
                'red
                (hash-set (hash-ref authority 'red)
                          'failure-summary
                          (string-append "reproduction observed: the malformed digest passed the "
                                         "gate before the fix"))))
    (define prose-gate (gate-run! root trio tampered))
    (check-not-equal? (subprocess-result-exit-code prose-gate) 0)
    (check-true (string-contains? (gate-output prose-gate)
                                  "authority-red-facts-do-not-match-retained-red"))
    ;; Raw retained red bytes are revalidated byte for byte offline.
    (define red-path
      (build-path root
                  (hash-ref authority 'bundle)
                  (hash-ref (hash-ref (hash-ref authority 'red) 'reference) 'path)))
    (file-or-directory-permissions red-path #o600)
    (call-with-output-file red-path
                           #:exists 'append
                           (lambda (out) (write-bytes #"tamper" out)))
    (define tampered-gate (gate-run! root trio authority))
    (check-not-equal? (subprocess-result-exit-code tampered-gate) 0))

  (native-case "genuine rejection publishes with red and recovers exactly"
    ;; Python `RedStrictGateTests.test_z_genuine_rejection_publishes_with_red_and_recovers_exactly`.
    (define world (linked-red-world))
    (define request (hash-ref (prepare! world) 'request))
    (define findings
      (list (string-append "The fixture widget fix is behaviorally correct but the operator "
                           "merge authorization (F11) remains outstanding.")))
    (define out (audit-output world request "request-changes"))
    (define result (finalize! world (hash-set out 'findings findings)))
    (check-equal? (hash-ref result 'status) "review-rejected" (format "~s" result))
    (check-equal? (hash-ref result 'findings) findings)
    (define authority (hash-ref request 'owned-authority))
    (for ([ref (in-list (list (hash-ref authority 'reference)
                              (hash-ref authority 'log-reference)
                              (hash-ref (hash-ref authority 'red) 'reference)
                              (hash-ref (hash-ref authority 'red) 'log-reference)))])
      (define relative (string-append (hash-ref authority 'bundle) "/" (hash-ref ref 'path)))
      (define published
        (git-raw! (repo-of world) "show" (format "~a:~a" (hash-ref result 'tip) relative)))
      (check-equal? (string->bytes/utf-8 published)
                    (file->bytes (build-path (staging-of world) relative))))
    ;; The published rejection replays on the next prepare (the reject-retry
    ;; law), it is never silently re-dispatched or dropped.
    (check-equal? (hash-ref (prepare! world) 'status) "review-rejected"))

  ;; ============================================================
  ;; Positive path: approved audit through the strict gate
  ;; ============================================================

  (native-case "approved audit publishes through the strict gate with red"
    ;; Python `LinkedRedApprovalTests.test_approved_audit_publishes_through_strict_gate_with_red`.
    (define world (positive-world))
    (define request (hash-ref (prepare! world) 'request))
    (define authority (hash-ref request 'owned-authority))
    (check-true (and (hash? (hash-ref authority 'red)) #t))
    (check-equal? (hash-ref (hash-ref authority 'red) 'relation) "preimplementation-ancestor")
    (check-equal? (hash-ref (hash-ref authority 'red) 'red-head) (hash-ref world 'head1))
    (define green-manifest
      (read-json-file (build-path (staging-of world)
                                  (hash-ref authority 'bundle)
                                  (hash-ref (hash-ref authority 'reference) 'path))))
    (check-not-equal? (hash-ref (hash-ref authority 'red) 'red-tree)
                      (hash-ref (hash-ref green-manifest 'binding) 'tree))
    (check-true (string-contains? (hash-ref (hash-ref authority 'red) 'failure-summary)
                                  "test failures"))
    (define result (finalize! world (audit-output world request)))
    (check-equal? (hash-ref result 'status) "reviewed" (format "~s" result))
    (define tip (hash-ref result 'tip))
    ;; The strict offline gate passes on the PUBLISHED committed trio.
    (define root (make-temporary-file "q-published-~a" 'directory))
    (define published-files
      (for/list ([relative (in-list (list (hash-ref request 'evidence)
                                          (hash-ref request 'review)
                                          (hash-ref request 'validation)
                                          "scripts/required-pr-checks.policy"))])
        (cons relative
              (git-raw! (repo-of world) "show" (format "~a:~a" tip relative)))))
    (for ([ref (in-list (list (hash-ref authority 'reference)
                              (hash-ref authority 'log-reference)
                              (hash-ref (hash-ref authority 'red) 'reference)
                              (hash-ref (hash-ref authority 'red) 'log-reference)))])
      (define relative (string-append (hash-ref authority 'bundle) "/" (hash-ref ref 'path)))
      (set! published-files
            (cons (cons relative
                        (git-raw! (repo-of world) "show" (format "~a:~a" tip relative)))
                  published-files)))
    (for ([pair (in-list published-files)])
      (put-text! (build-path root (car pair)) (cdr pair)))
    (define payload (build-path root "authority.json"))
    (put-json! payload authority)
    (define published-gate
      (run-gate! (build-path root (hash-ref request 'evidence))
                 (list "--content-digest"
                       (hash-ref request 'content-digest)
                       "--root"
                       (path->string root)
                       "--policy"
                       (path->string (build-path root "scripts/required-pr-checks.policy"))
                       "--owned-proof-authority"
                       (path->string payload))))
    (check-equal? (subprocess-result-exit-code published-gate) 0 (gate-output published-gate))
    ;; The publication COMMITTED the authority payload inside the excluded
    ;; bundle directory, and push CI (no explicit payload argument) validates
    ;; the same trio through gate discovery of that payload.
    (define committed
      (git-raw! (repo-of world)
                "show"
                (format "~a:~a/authority.json" tip (hash-ref authority 'bundle))))
    (check-equal? (read-json (open-input-string committed))
                  (read-json (open-input-string (jstring authority))))
    (define discovered-root (make-temporary-file "q-discovered-~a" 'directory))
    (for ([pair (in-list published-files)])
      (when (not (string-suffix? (car pair) "authority.json"))
        (put-text! (build-path discovered-root (car pair)) (cdr pair))))
    (put-text! (build-path discovered-root (hash-ref authority 'bundle) "authority.json")
               committed)
    (define discovered-gate
      (run-gate! (build-path discovered-root (hash-ref request 'evidence))
                 (list "--content-digest"
                       (hash-ref request 'content-digest)
                       "--root"
                       (path->string discovered-root)
                       "--policy"
                       (path->string (build-path discovered-root
                                                 "scripts/required-pr-checks.policy")))))
    (check-equal? (subprocess-result-exit-code discovered-gate) 0 (gate-output discovered-gate))
    ;; Without any discoverable payload the audit trio fails closed.
    (delete-file (build-path discovered-root (hash-ref authority 'bundle) "authority.json"))
    (define undiscovered-gate
      (run-gate! (build-path discovered-root (hash-ref request 'evidence))
                 (list "--content-digest"
                       (hash-ref request 'content-digest)
                       "--root"
                       (path->string discovered-root)
                       "--policy"
                       (path->string (build-path discovered-root
                                                 "scripts/required-pr-checks.policy")))))
    (check-not-equal? (subprocess-result-exit-code undiscovered-gate) 0)
    ;; F11 stays a separate operator gate: approval never manufactures it.
    (define review-blob (git-raw! (repo-of world) "show" (format "~a:~a" tip (hash-ref request 'review))))
    (check-false (string-contains? review-blob "merge-authorization"))
    (check-false (hash-has-key? result 'merge-authorization))
    ;; Zero duplicate Verify launches: exactly one green manifest in the owner
    ;; store, and the audit cycle never added a second execution.
    (define kinds (store-kinds world))
    (check-equal? (count (lambda (kind) (equal? kind "declared-verify-green")) kinds) 1)
    (check-equal? (count (lambda (kind) (equal? kind "declared-red-failure")) kinds) 1)
    (define fresh (owned (owned-world-authority (campaign-of world)
                                                (plan-of world)
                                                (hash-ref world 'wave)
                                                (staging-of world))))
    (check-equal? (hash-ref fresh 'reference) (hash-ref authority 'reference))
    (check-equal? (hash-ref (hash-ref fresh 'red) 'reference)
                  (hash-ref (hash-ref authority 'red) 'reference)))

  ;; ============================================================
  ;; Post-merge binding provenance (continues the approved world)
  ;; ============================================================

  (native-case "binding finalize publishes audit provenance and validates"
    ;; Python `OwnedBindingTests.test_binding_finalize_publishes_audit_provenance_and_validates`.
    ;; Runs AFTER the approved-audit case on the same world: the implementation
    ;; verdict is already published, exactly like the production order.
    (define world (positive-world))
    (define plan (plan-of world))
    (define head (hash-ref world 'head2))
    (define q (hash-ref world 'q))
    ;; Fast-forward origin/main to the verified implementation head: the only
    ;; merged relation the audit law supports (merge == head).
    (git! q "push" "-q" "origin" (format "~a:refs/heads/main" head))
    (define staging (binding-staging-path (campaign-of world) plan (hash-ref world 'wave) 0))
    (make-directory* staging)
    (define pr-payload
      (hash 'number
            42
            'merged
            #t
            'merge_commit_sha
            head
            'merged_at
            "2026-09-14T16:23:06Z"
            'head
            (hash 'sha head
                  'ref
                  (hash-ref world 'branch)
                  'repo
                  (hash 'full_name slug))
            'base
            (hash 'ref
                  "main"
                  'sha
                  (hash-ref world 'c0)
                  'repo
                  (hash 'full_name slug))))
    (define request
      (draft-request (hash 'wave "W1" 'milestone 895 'issue 9686)
                     plan
                     (hash-ref world 'wave)
                     pr-payload
                     (binding-branch plan (hash-ref world 'wave) 0)
                     policy-names
                     0))
    (define payload (build-path staging "request.json"))
    (put-json! payload request)
    (define draft
      (run-subprocess (path->string (find-executable-path "racket"))
                      #:args (list (path->string binding-data-script)
                                   "prepare"
                                   (path->string payload)
                                   (path->string staging))
                      #:timeout 120))
    (check-equal? (subprocess-result-exit-code draft) 0 (subprocess-result-stderr draft))
    (delete-file payload)
    (define prepared (prepare! world "binding-review"))
    (check-equal? (hash-ref prepared 'status) "awaiting-review" (format "~s" prepared))
    (define binding-request (hash-ref prepared 'request))
    (define authority (hash-ref binding-request 'owned-authority))
    (check-equal? (hash-ref binding-request 'content-digest) empty-sha)
    (define out
      (hash-set (hash-remove (reviewer-output) 'validation)
                'audit-ack
                (hash 'mode
                      owned-audit-mode
                      'verification-origin
                      "audited-owned"
                      'source-read
                      #t
                      'raw-output-read
                      #t
                      'binding
                      (audit-binding binding-request)
                      'red-ack
                      (hash 'proof-sha256
                            (hash-ref (hash-ref (hash-ref authority 'red) 'reference) 'sha256)
                            'raw-output-read
                            #t))))
    (define result (finalize! world out "binding-review"))
    (check-equal? (hash-ref result 'status) "reviewed" (format "~s" result))
    (define artifact (format "~a-w1.rktd" plan))
    (define evidence (read-datum! (build-path staging "docs/reports/gsd-wave-evidence" artifact)))
    (define review (read-datum! (build-path staging "docs/reports/gsd-wave-reviews" artifact)))
    (define validation (read-datum! (build-path staging "docs/reports/gsd-wave-validation" artifact)))
    (for ([datum (in-list (list evidence review validation))])
      (check-equal? (hash-ref datum 'review-mode) owned-audit-mode)
      (check-true (json-equal? (hash-ref datum 'owned-proof) (hash-ref authority 'reference)))
      (check-true (json-equal? (hash-ref datum 'audit-binding) (audit-binding binding-request))))
    (check-true (json-equal? (hash-ref review 'audit-ack) (hash-ref out 'audit-ack)))
    ;; The staged strict gate validated the trio through the DISCOVERED bundle
    ;; law (mirrored from this staging directory).
    (define bundle (build-path staging (hash-ref authority 'bundle)))
    (check-true (file-exists? (build-path bundle "authority.json")))
    (define reviewed
      (owned (delivery-binding-review! (repo-of world)
                                       plan
                                       (hash-ref world 'wave)
                                       staging
                                       #:campaign-root (campaign-of world)
                                       #:expected-branch (hash-ref world 'branch))))
    (check-equal? (hash-ref reviewed 'status) "reviewed"))

  ;; ============================================================
  ;; Blocked readiness cannot downgrade authentic legacy authority
  ;; ============================================================

  (native-case "blocked readiness cannot downgrade authentic legacy authority"
    ;; Carried inline by the deleted `test-gsd-owned-audit-binding.rkt` shim.
    (define world (readiness-world))
    (define plan (plan-of world))
    (define campaign (campaign-of world))
    (define valid (build-path campaign "readiness-valid"))
    (define authority (owned (owned-world-authority campaign plan 1 valid)))
    (check-equal? (hash-ref authority 'evidence-revision) 0)
    (check-equal? (hash-ref (hash-ref authority 'red) 'red-head) (hash-ref world 'head1))
    ;; A blocked readiness must never export a "legacy" authority. The repair
    ;; ledger lives beside the trusted owned-proof store (its PARENT directory),
    ;; never under the campaign root.
    (define repairs
      (simplify-path (build-path (simplify-path (build-path owned-proof-root 'up))
                                 "evidence-repairs")))
    (define ledger (build-path repairs plan "W1" "ledger.json"))
    (put-text! ledger "{broken")
    (define blocked (build-path campaign "readiness-blocked"))
    (define error
      (with-handlers ([exn:fail? (lambda (error) error)])
        (owned (owned-world-authority campaign plan 1 blocked))))
    (check-true (exn:fail? error))
    (check-true (string-contains? (exn-message error) "invalid-current-evidence"))
    (check-true (string-contains? (exn-message error) "blocked"))
    (check-false (file-exists? blocked))
    (check-false (directory-exists? blocked))))
