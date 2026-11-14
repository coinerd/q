#lang racket

;; @suite testing
;; @speed fast
;; @covers scripts/github/wave-finalizer-controller.rkt
;; @covers scripts/github/wave-finalizer.rkt
;;
;; Port job J3: the wave-finalizer controller (scripts/github/wave-finalizer-controller.rkt).
;;
;; The 17 security/recovery invariants of `scripts/github/test_wave_finalizer.py`
;; are ported one for one against an in-file fake adapter (a `finalizer-client`
;; procedure bundle, the port's replacement for Python's injected `GitHubClient`),
;; plus the port-plan invariants that have no Python test: the mutation ORDER
;; (merge before merge-CI before close-issue before board), the freshness law
;; (every read is `#:refresh? #t`, so a post-prepare re-read cannot be a memoized
;; no-op), the changed-after-prepare refusals, and the CLI (exit 0 with the exact
;; stdout key sets, exit 1 with the `wave-finalizer: <message>` line, exit 2 with
;; the argparse-parity launcher message, Python's default `json.dumps` rendering).
;;
;; The last section is a differential parity table: the same eleven scenarios are
;; driven through the Python module (imported by path through the committed
;; `test_wave_finalizer` fixtures) and through this port, and the refusal kind +
;; message (or the success payload and mutation order) must match exactly.  A
;; missing python3 fails these tests instead of skipping them.
;;
;; Freshness note: the fake records the `#:refresh?` flag of every read, which is
;; how "every controller read is fresh" is asserted.  In production the frozen
;; adapter has no refresh path for branch protection / issues / check runs; the
;; wiring reaches the adapter's own delivery session api for the first two (same
;; session, same budget) and the check-run list stays memoized -- see the
;; controller's header, where that single exception is justified.

(require rackunit
         json
         racket/bytes
         racket/list
         racket/runtime-path
         racket/string
         file/sha1
         openssl
         "../scripts/github/wave-finalizer.rkt"
         "../scripts/github/wave-finalizer-adapter.rkt"
         "../scripts/github/wave-finalizer-controller.rkt"
         (submod "../scripts/github/wave-finalizer.rkt" for-testing))

(define repository "acme/widgets")
(define head-sha (make-string 40 #\1))
(define merge-sha (make-string 40 #\2))
(define app-id 15368)
(define merged-at "2025-01-02T12:00:00Z")
(define run-at "2025-01-02T12:01:00Z")
(define pull-number 17)

;; ================================================================ fixtures
;; Ported verbatim from the Python fixtures so a differential row is the same
;; document on both sides.

(define (trusted-config)
  (hash "schema_version"
        1
        "repository"
        repository
        "base_ref"
        "main"
        "merge_method"
        "squash"
        "attestation_issuer"
        "trusted-wave-launcher"
        "required_workflows"
        (list (hash "context"
                    "W0 / protected"
                    "workflow"
                    ".github/workflows/ci.yml"
                    "ref"
                    "feature/w0"
                    "event"
                    "pull_request"
                    "app"
                    (hash "slug" "github-actions" "id" app-id)))
        "merge_workflows"
        (list (hash "context"
                    "W0 / merge"
                    "workflow"
                    ".github/workflows/merge-ci.yml"
                    "ref"
                    "main"
                    "event"
                    "push"
                    "app"
                    (hash "slug" "github-actions" "id" app-id)))
        "wave_issue_numbers"
        (list 40 41)
        "board"
        (hash "project_id" "PVT_1" "item_id" "PVTI_1" "field_id" "PVTF_1" "value" "Done")))

(define (hash-set-depth value keys replacement)
  (cond
    [(null? (cdr keys)) (hash-set value (car keys) replacement)]
    [else
     (hash-set value
               (car keys)
               (hash-set-depth (hash-ref value (car keys)) (cdr keys) replacement))]))

(define (envelope [projection (hash "pull_number" pull-number "head_sha" head-sha)])
  (define body
    (hash "schema_version"
          1
          "issuer"
          "trusted-wave-launcher"
          "projection"
          projection
          "evidence"
          (hash "kind" "artifact" "id" 7001)))
  (hash-set body
            "canonical_digest"
            (hash "algorithm" "sha256" "value" (compute-envelope-digest body))))

(define (projection-digest value)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 (finalizer-canonical-json
                                                         (hash-ref value "projection"))))))

(define (protection #:app-id [pinned-app-id app-id])
  (hash "required_status_checks"
        (hash "strict"
              #t
              "checks"
              (list (hash "context" "W0 / protected" "app_id" pinned-app-id))
              "contexts"
              '())
        "enforce_admins"
        (hash "enabled" #t)
        "required_linear_history"
        (hash "enabled" #t)
        "allow_force_pushes"
        (hash "enabled" #f)
        "allow_deletions"
        (hash "enabled" #f)))

(define (pull #:merged? [merged? #f]
              #:state [state "open"]
              #:merge-sha-value [pr-merge-sha #f]
              #:merged-at-value [pr-merged-at #f])
  (hash "number"
        pull-number
        "state"
        state
        "merged"
        merged?
        "merge_commit_sha"
        pr-merge-sha
        "merged_at"
        pr-merged-at
        "base"
        (hash "ref" "main" "repo" (hash "full_name" repository))
        "head"
        (hash "sha" head-sha "ref" "feature/w0" "repo" (hash "full_name" repository))))

(define (workflow-run #:merge? [merge? #f]
                      #:sha-value [run-sha #f]
                      #:attempt [attempt 1]
                      #:conclusion [conclusion "success"]
                      #:event-value [run-event #f]
                      #:ref-value [run-ref #f]
                      #:suite-value [suite #f]
                      #:run-id-value [run-id #f]
                      #:created-at-value [created-at #f]
                      #:path-value [path #f])
  (hash "id"
        (or run-id (+ (if merge? 200 100) attempt))
        "workflow_id"
        (if merge? 8 7)
        "path"
        (or path (if merge? ".github/workflows/merge-ci.yml" ".github/workflows/ci.yml"))
        "repository"
        (hash "full_name" repository)
        "head_sha"
        (or run-sha (if merge? merge-sha head-sha))
        "head_branch"
        (or run-ref (if merge? "main" "feature/w0"))
        "event"
        (or run-event (if merge? "push" "pull_request"))
        "run_number"
        (if merge? 9 8)
        "run_attempt"
        attempt
        "check_suite_id"
        (or suite (if merge? 190 90))
        "status"
        "completed"
        "conclusion"
        conclusion
        "created_at"
        (or created-at (if merge? run-at "2025-01-01T12:00:00Z"))))

(define (check-run #:merge? [merge? #f]
                   #:sha-value [check-sha #f]
                   #:conclusion [conclusion "success"]
                   #:suite-value [suite #f]
                   #:app-id [pinned-app-id app-id]
                   #:slug [slug "github-actions"])
  (hash "id"
        (if merge? 600 500)
        "name"
        (if merge? "W0 / merge" "W0 / protected")
        "head_sha"
        (or check-sha (if merge? merge-sha head-sha))
        "status"
        "completed"
        "conclusion"
        conclusion
        "check_suite"
        (hash "id" (or suite (if merge? 190 90)))
        "app"
        (hash "slug" slug "id" pinned-app-id)))

;; ================================================================ fake adapter
;; Python: FakeGitHub.  Every read records its `#:refresh?` flag; `calls` is the
;; ordered evidence for the mutation-order assertions.

(struct fake
        (prs protection
             workflow-runs
             check-runs
             merge-response
             attestation-overrides
             issues
             board-value
             calls
             refreshes
             close-fail-after-apply
             close-without-apply
             board-fail-after-apply
             board-without-apply
             merge-fail-after-apply)
  #:mutable)

(define (make-fake)
  (fake (list (pull))
        (protection)
        (hash head-sha (list (workflow-run)))
        (hash head-sha (list (check-run)))
        (hash "merged" #t "sha" merge-sha)
        (hash)
        (hash 40 "open" 41 "open")
        "Todo"
        '()
        '()
        '()
        '()
        #f
        #f
        #f))

;; Python: ready_client() -- an open pull request, then the merged refetch, and a
;; push workflow run + check run for the merge SHA.
(define (ready-fake #:merge-sha-value [ready-merge-sha merge-sha])
  (define value (make-fake))
  (set-fake-prs! value
                 (list (pull)
                       (pull)
                       (pull #:merged? #t
                             #:state "closed"
                             #:merge-sha-value ready-merge-sha
                             #:merged-at-value merged-at)))
  (set-fake-workflow-runs! value
                           (hash head-sha
                                 (list (workflow-run))
                                 ready-merge-sha
                                 (list (workflow-run #:merge? #t #:sha-value ready-merge-sha))))
  (set-fake-check-runs! value
                        (hash head-sha
                              (list (check-run))
                              ready-merge-sha
                              (list (check-run #:merge? #t #:sha-value ready-merge-sha))))
  value)

(define (record! fake entry)
  (set-fake-calls! fake (cons entry (fake-calls fake))))

(define (record-refresh! fake method refresh?)
  (set-fake-refreshes! fake (cons (cons method refresh?) (fake-refreshes fake))))

(define (call-order fake)
  (reverse (fake-calls fake)))

(define (call-names fake)
  (map car (call-order fake)))

(define (mutation-names fake)
  (for/list ([name (in-list (call-names fake))]
             #:when (member name '(merge close_issue update_board)))
    name))

(define (fake-verify-evidence fake repo reference)
  (record! fake (list 'attestation repo reference))
  (define expected
    (hash "verified"
          #t
          "reference"
          reference
          "repository"
          repo
          "pull_number"
          pull-number
          "head_sha"
          head-sha
          "projection_digest"
          (projection-digest (envelope))
          "issuer"
          "trusted-wave-launcher"
          "latest"
          #t))
  (for/fold ([result expected]) ([(key item) (in-hash (fake-attestation-overrides fake))])
    (hash-set result key item)))

(define (fake-get-pr fake refresh?)
  (record-refresh! fake 'get_pr refresh?)
  (record! fake (list 'get_pr repository pull-number))
  (define prs (fake-prs fake))
  (cond
    [(> (length prs) 1)
     (set-fake-prs! fake (cdr prs))
     (car prs)]
    [else (car prs)]))

(define (fake-get-protection fake refresh?)
  (record-refresh! fake 'protection refresh?)
  (record! fake (list 'protection repository "main"))
  (fake-protection fake))

(define (fake-list-workflow-runs fake sha refresh?)
  (record-refresh! fake 'workflow_runs refresh?)
  (record! fake (list 'workflow_runs repository sha))
  (hash-ref (fake-workflow-runs fake) sha '()))

(define (fake-list-check-runs fake sha refresh?)
  (record-refresh! fake 'check_runs refresh?)
  (record! fake (list 'check_runs repository sha))
  (hash-ref (fake-check-runs fake) sha '()))

(define (fake-merge-pr fake number sha merge-method)
  (record! fake (list 'merge repository number sha merge-method))
  (when (fake-merge-fail-after-apply fake)
    (error "response lost"))
  (fake-merge-response fake))

(define (fake-get-issue fake number refresh?)
  (record-refresh! fake 'get_issue refresh?)
  (record! fake (list 'get_issue repository number))
  (define state (hash-ref (fake-issues fake) number #f))
  (unless state
    (error (format "issue ~a is unavailable" number)))
  (hash "number" number "state" state))

(define (fake-close-issue fake number)
  (record! fake (list 'close_issue repository number))
  (unless (member number (fake-close-without-apply fake))
    (set-fake-issues! fake (hash-set (fake-issues fake) number "closed")))
  (when (member number (fake-close-fail-after-apply fake))
    (set-fake-close-fail-after-apply! fake (remove number (fake-close-fail-after-apply fake)))
    (error "response lost"))
  (void))

(define (fake-get-field fake)
  (record! fake (list 'get_board repository "PVT_1" "PVTI_1" "PVTF_1"))
  (fake-board-value fake))

(define (fake-update-field fake value)
  (record! fake (list 'update_board repository "PVT_1" "PVTI_1" "PVTF_1" value))
  (unless (fake-board-without-apply fake)
    (set-fake-board-value! fake value))
  (when (fake-board-fail-after-apply fake)
    (set-fake-board-fail-after-apply! fake #f)
    (error "response lost"))
  (void))

(define (fake-client fake)
  (make-finalizer-client
   #:verify-evidence (lambda (repo reference) (fake-verify-evidence fake repo reference))
   #:get-pr (lambda (repo number #:refresh? [refresh? #f]) (fake-get-pr fake refresh?))
   #:get-protection (lambda (repo ref #:refresh? [refresh? #f]) (fake-get-protection fake refresh?))
   #:list-workflow-runs (lambda (repo sha #:refresh? [refresh? #f])
                          (fake-list-workflow-runs fake sha refresh?))
   #:list-check-runs (lambda (repo sha #:refresh? [refresh? #f])
                       (fake-list-check-runs fake sha refresh?))
   #:merge-pr (lambda (repo number #:sha sha #:merge-method merge-method)
                (fake-merge-pr fake number sha merge-method))
   #:get-issue (lambda (repo number #:refresh? [refresh? #f]) (fake-get-issue fake number refresh?))
   #:close-issue (lambda (repo number) (fake-close-issue fake number))
   #:get-field (lambda (repo project item field) (fake-get-field fake))
   #:update-field (lambda (repo project item field value) (fake-update-field fake value))))

;; ================================================================ assertions

(define (caught-error thunk)
  (with-handlers ([exn:fail:finalizer? (lambda (error) error)])
    (thunk)
    #f))

(define (check-refusal thunk kinds message)
  (define kinds*
    (if (list? kinds)
        kinds
        (list kinds)))
  (define caught (caught-error thunk))
  (check-true (exn:fail:finalizer? caught) (format "expected a finalizer refusal, got ~e" caught))
  (when (exn:fail:finalizer? caught)
    (check-true (if (member (exn:fail:finalizer-kind caught) kinds*) #t #f)
                (format "kind ~a not in ~a" (exn:fail:finalizer-kind caught) kinds*))
    (when message
      (check-equal? (exn-message caught) message))))

(define (check-refusal-kind thunk kinds)
  (check-refusal thunk kinds #f))

(define (index-of-call calls predicate)
  (for/first ([call (in-list calls)]
              [index (in-naturals)]
              #:when (predicate call))
    index))

;; ================================================================ pure configuration

(module+ test
  (test-case "envelope digest is only canonical self-consistency"
    (define value (envelope))
    (define verified (verify-external-envelope value))
    (check-true (verified-envelope-canonical-self-consistent verified))
    ;; The digest covers exactly the canonical document, so anyone holding the
    ;; envelope can recompute it: self-consistency, never integrity or
    ;; authorization (the value carries no integrity flag to check).
    (check-equal? (compute-envelope-digest value) (verified-envelope-digest verified))
    (check-refusal (lambda ()
                     ;; Python mutates the projection AFTER the digest is computed.
                     (verify-external-envelope
                      (hash-set (envelope) "projection" (hash "pull_number" 99 "head_sha" head-sha))))
                   "EnvelopeError"
                   "envelope canonical self-consistency mismatch"))

  (test-case "envelope cannot supply trusted policy or any mutation"
    (define fake (make-fake))
    (define client (fake-client fake))
    (check-refusal
     (lambda ()
       (prepare-finalization client (trusted-config) (hash-set (envelope) "repository" "evil/repo")))
     "EnvelopeError"
     "envelope has unknown ['repository']")
    (check-refusal (lambda ()
                     (load-trusted-configuration (hash-set (trusted-config) "merge_workflows" '())))
                   "ConfigurationError"
                   "merge_workflows must be a nonempty array")
    (check-equal? (call-names fake) '()))

  (test-case "board value and issue configuration are type strict"
    (check-refusal
     (lambda ()
       (load-trusted-configuration (hash-set (trusted-config) "wave_issue_numbers" (list 40 #t))))
     "ConfigurationError"
     "wave_issue_numbers[1] must be a positive integer")
    (check-refusal
     (lambda ()
       (load-trusted-configuration (hash-set-depth (trusted-config) '("board" "value") +nan.0)))
     "ConfigurationError"
     "board.value is not canonical JSON: Out of range float values are not JSON compliant: nan")))

;; ================================================================ prepare

(module+ test
  (test-case "attestation is verified before github facts and is exact"
    (define fake (make-fake))
    (define prepared (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
    (check-equal? (car (call-names fake)) 'attestation)
    (check-equal? (prepared-finalization-head-sha prepared) head-sha)
    (check-true (string? (prepared-finalization-attestation-digest prepared)))
    (check-equal? (mutation-names fake) '()))

  (test-case "wrong attestation binding, unverified and stale are all refused before any github read"
    (define overrides
      (list (hash "verified" #f)
            (hash "repository" "evil/repo")
            (hash "pull_number" 18)
            (hash "head_sha" (make-string 40 #\3))
            (hash "projection_digest" (make-string 64 #\4))
            (hash "issuer" "attacker")
            (hash "latest" #f)
            (hash "reference" (hash "kind" "artifact" "id" 999))))
    (for ([override (in-list overrides)])
      (test-case (format "attestation override ~s" override)
        (define fake (make-fake))
        (set-fake-attestation-overrides! fake override)
        (check-refusal (lambda ()
                         (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
                       "AttestationError"
                       #f)
        (check-false (member 'get_pr (call-names fake))))))

  (test-case "a fresh post-merge prepare cannot authorize closure"
    (define fake (make-fake))
    (set-fake-prs!
     fake
     (list
      (pull #:merged? #t #:state "closed" #:merge-sha-value merge-sha #:merged-at-value merged-at)))
    (check-refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
                   "MergeError"
                   "cannot prepare finalization for an already merged pull request")
    (check-equal? (mutation-names fake) '()))

  (test-case "branch policy rejects legacy unpinned, wrong app and malformed protection"
    (define policies
      (list (protection #:app-id 'null)
            (protection #:app-id 999)
            (hash-set (protection)
                      "required_status_checks"
                      (hash "strict" #t "checks" '() "contexts" (list "W0 / protected")))
            (hash-set (protection) "required_status_checks" 'null)))
    (for ([policy (in-list policies)]
          [index (in-naturals)])
      (test-case (format "policy variant ~a" index)
        (define fake (make-fake))
        (set-fake-protection! fake policy)
        (check-refusal-kind (lambda ()
                              (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
                            (list "PolicyError" "ApiUncertainty")))))

  (test-case "workflow provenance, app and latest-attempt rejections"
    (define mutations
      (list (cons "path"
                  (cons ".github/workflows/other.yml"
                        "required workflow '.github/workflows/ci.yml' has no run"))
            (cons "head_branch" (cons "other" "latest workflow attempt provenance mismatch"))
            (cons "event" (cons "push" "latest workflow attempt provenance mismatch"))
            (cons "head_sha"
                  (cons (make-string 40 #\3) "latest workflow attempt provenance mismatch"))))
    (for ([mutation (in-list mutations)])
      (test-case (format "workflow run ~a" (car mutation))
        (define fake (make-fake))
        (set-fake-workflow-runs!
         fake
         (hash head-sha (list (hash-set (workflow-run) (car mutation) (cadr mutation)))))
        (check-refusal (lambda ()
                         (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
                       "CheckError"
                       (cddr mutation))))
    (test-case "check app id is exact"
      (define fake (make-fake))
      (set-fake-check-runs! fake (hash head-sha (list (check-run #:app-id 999))))
      (check-refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
                     "CheckError"
                     "required check App provenance mismatch"))
    (test-case "the latest attempt is ranked and must be successful"
      (define fake (make-fake))
      (set-fake-workflow-runs!
       fake
       (hash head-sha
             (list (workflow-run)
                   (workflow-run #:attempt 2 #:conclusion "failure" #:suite-value 91))))
      (check-refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
                     "CheckError"
                     "latest workflow attempt is not successful"))))

;; ================================================================ finalize

(module+ test
  (test-case "merge CI is mandatory, exact, and runs after the confirmed merge"
    (define fake (ready-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (define result (finalize-prepared client prepared))
    (check-true (finalization-result-merge-ci-verified result))
    (check-equal? (finalization-result-merge-sha result) merge-sha)
    (define calls (call-order fake))
    (define merge-index (index-of-call calls (lambda (call) (eq? (car call) 'merge))))
    (define ci-index
      (index-of-call calls (lambda (call) (equal? call (list 'workflow_runs repository merge-sha)))))
    (define close-index (index-of-call calls (lambda (call) (eq? (car call) 'close_issue))))
    (define board-index (index-of-call calls (lambda (call) (eq? (car call) 'update_board))))
    ;; Mutation order: merge -> merge CI -> close issue -> board.
    (check-true (if (and merge-index ci-index close-index board-index) #t #f))
    (check-true (< merge-index ci-index))
    (check-true (< ci-index close-index))
    (check-true (< close-index board-index)))

  (test-case "merge CI wrong ref, event, sha, failure, reused run id and predating run reject without sync"
    (define variants
      (list (workflow-run #:merge? #t #:ref-value "feature/w0")
            (workflow-run #:merge? #t #:event-value "pull_request")
            (workflow-run #:merge? #t #:sha-value head-sha)
            (workflow-run #:merge? #t #:conclusion "failure")
            (workflow-run #:merge? #t #:run-id-value 101)
            (workflow-run #:merge? #t #:created-at-value "2025-01-02T11:59:59Z")))
    (for ([variant (in-list variants)]
          [index (in-naturals)])
      (test-case (format "merge CI variant ~a" index)
        (define fake (ready-fake))
        (set-fake-workflow-runs! fake (hash head-sha (list (workflow-run)) merge-sha (list variant)))
        (define client (fake-client fake))
        (define prepared (prepare-finalization client (trusted-config) (envelope)))
        (check-refusal (lambda () (finalize-prepared client prepared)) "CheckError" #f)
        (check-false (member 'close_issue (call-names fake)))
        (check-false (member 'update_board (call-names fake))))))

  (test-case "merge CI cannot reuse pull request evidence when the merge sha equals the head sha"
    (define fake (ready-fake #:merge-sha-value head-sha))
    (set-fake-merge-response! fake (hash "merged" #t "sha" head-sha))
    (set-fake-workflow-runs! fake (hash head-sha (list (workflow-run))))
    (set-fake-check-runs! fake (hash head-sha (list (check-run))))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (check-refusal (lambda () (finalize-prepared client prepared)) "CheckError" #f)
    (check-false (member 'close_issue (call-names fake))))

  (test-case "merged=false never reaches a post-merge mutation"
    (define fake (make-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (set-fake-merge-response! fake (hash "merged" #f "sha" merge-sha))
    (set-fake-prs! fake (list (pull)))
    (check-refusal (lambda () (finalize-prepared client prepared))
                   "MergeError"
                   "GitHub did not confirm merged=true")
    (check-false (member 'close_issue (call-names fake)))
    (check-false (member 'update_board (call-names fake))))

  (test-case "merge response sha must be 40-hex and equal the refetched merge sha"
    (define fake (ready-fake))
    (set-fake-merge-response! fake (hash "merged" #t "sha" (make-string 40 #\5)))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (check-refusal (lambda () (finalize-prepared client prepared))
                   "MergeError"
                   "merge response SHA differs from refetched pull request")
    (check-false (member 'close_issue (call-names fake)))
    (define short-fake (ready-fake))
    (set-fake-merge-response! short-fake (hash "merged" #t "sha" "abc"))
    (define short-client (fake-client short-fake))
    (define short-prepared (prepare-finalization short-client (trusted-config) (envelope)))
    (check-refusal (lambda () (finalize-prepared short-client short-prepared))
                   "MergeError"
                   "merge response SHA must be a full 40-character hexadecimal SHA"))

  (test-case "a prepared value is revalidated, never handcraftable, and its seal detects field mutation"
    (define fake (ready-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (define fields
      (list (prepared-finalization-config-json prepared)
            (prepared-finalization-envelope-json prepared)
            (prepared-finalization-config-digest prepared)
            (prepared-finalization-envelope-digest prepared)
            (prepared-finalization-attestation-digest prepared)
            (prepared-finalization-head-sha prepared)
            (prepared-finalization-policy prepared)
            (prepared-finalization-premerge-run-ids prepared)
            (prepared-finalization-already-merged prepared)))
    ;; Racket structs are immutable (no setter is generated), so a "field write" is
    ;; modelled the way the model layer documents it: a forged value carrying a
    ;; genuine seal over different material, and an unregistered lookalike.
    (define forged-seal
      (forge-prepared-finalization (list-ref fields 0)
                                   (list-ref fields 1)
                                   (list-ref fields 2)
                                   (list-ref fields 3)
                                   (list-ref fields 4)
                                   (list-ref fields 5)
                                   (list-ref fields 6)
                                   (list-ref fields 7)
                                   (list-ref fields 8)
                                   #:register? #t
                                   #:seal "forged"))
    (check-refusal (lambda () (finalize-prepared client forged-seal))
                   "ValidationError"
                   "PreparedFinalization seal is invalid")
    (check-false (member 'merge (call-names fake)))
    (define lookalike (apply forge-prepared-finalization fields))
    (check-refusal (lambda () (finalize-prepared client lookalike))
                   "ValidationError"
                   "PreparedFinalization was not issued by prepare_finalization")
    ;; A registered copy sealed over DIFFERENT material (a changed attestation
    ;; digest) revalidates its own seal and is caught by the attestation re-read.
    (define mutated
      (forge-prepared-finalization (list-ref fields 0)
                                   (list-ref fields 1)
                                   (list-ref fields 2)
                                   (list-ref fields 3)
                                   (make-string 64 #\9)
                                   (list-ref fields 5)
                                   (list-ref fields 6)
                                   (list-ref fields 7)
                                   (list-ref fields 8)
                                   #:register? #t))
    (check-refusal (lambda () (finalize-prepared client mutated))
                   "AttestationError"
                   "attestation changed after prepare")
    (check-false (member 'merge (call-names fake))))

  (test-case "a changed attestation, policy and pre-merge evidence are refused after prepare"
    (test-case "attestation"
      (define fake (ready-fake))
      (define client (fake-client fake))
      (define prepared (prepare-finalization client (trusted-config) (envelope)))
      (set-fake-attestation-overrides! fake (hash "issuer" "attacker"))
      (check-refusal (lambda () (finalize-prepared client prepared)) "AttestationError" #f)
      (check-false (member 'merge (call-names fake))))
    (test-case "policy"
      ;; The sealed contracts fully determine the policy (contexts must be equal
      ;; and every context's pinned App must match the contract), so the literal
      ;; "branch protection changed after prepare" branch is unreachable -- in
      ;; Python too.  The observable refusal for a changed protection snapshot is
      ;; the pinned-App PolicyError raised while re-observing.
      (define fake (ready-fake))
      (define client (fake-client fake))
      (define prepared (prepare-finalization client (trusted-config) (envelope)))
      (set-fake-protection! fake (protection #:app-id 999))
      (check-refusal (lambda () (finalize-prepared client prepared))
                     "PolicyError"
                     "workflow App does not match pinned branch protection App")
      (check-false (member 'merge (call-names fake))))
    (test-case "pre-merge workflow evidence"
      (define fake (ready-fake))
      (define client (fake-client fake))
      (define prepared (prepare-finalization client (trusted-config) (envelope)))
      (set-fake-workflow-runs! fake (hash head-sha (list (workflow-run) (workflow-run #:attempt 2))))
      (set-fake-check-runs! fake (hash head-sha (list (check-run))))
      (check-refusal (lambda () (finalize-prepared client prepared))
                     "CheckError"
                     "pre-merge workflow evidence changed after prepare")
      (check-false (member 'merge (call-names fake)))))

  (test-case "every controller read requests a fresh adapter read"
    (define fake (ready-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (finalize-prepared client prepared)
    (define reads (reverse (fake-refreshes fake)))
    (check-true (pair? reads))
    ;; The only reads that may be served from the adapter cache are check-run
    ;; lists: the frozen adapter's `checks` closure memoizes per (slug, sha) and
    ;; has no refresh path (documented in the controller header).  Everything the
    ;; changed-after-prepare refusals depend on must be a fresh read.
    (define stale
      (for/list ([read (in-list reads)]
                 #:when (not (cdr read)))
        (car read)))
    (check-equal? (remove-duplicates stale) '(check_runs))))

;; ================================================================ post-merge sync

(module+ test
  (test-case "only configured issues are closed, never the pull request, and each close is confirmed"
    (define fake (ready-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (finalize-prepared client prepared)
    (define closed
      (for/list ([call (in-list (call-order fake))]
                 #:when (eq? (car call) 'close_issue))
        (caddr call)))
    (check-equal? closed (list 40 41))
    (check-false (member pull-number closed))
    (check-equal? (hash-ref (fake-issues fake) 40) "closed")
    (check-equal? (hash-ref (fake-issues fake) 41) "closed"))

  (test-case "close response loss is recovered and a second run re-closes nothing"
    (define fake (ready-fake))
    (set-fake-close-fail-after-apply! fake (list 40))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (finalize-prepared client prepared)
    (check-equal? (fake-board-value fake) "Done")
    (define (closes-of-40)
      (length (for/list ([call (in-list (call-order fake))]
                         #:when (and (eq? (car call) 'close_issue) (equal? (caddr call) 40)))
                call)))
    (define first-count (closes-of-40))
    (finalize-prepared client prepared)
    (check-equal? (closes-of-40) first-count)
    (check-equal? (fake-board-value fake) "Done"))

  (test-case "an issue read failure stops the board"
    (define fake (ready-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (set-fake-issues! fake (hash-remove (fake-issues fake) 41))
    (check-refusal-kind (lambda () (finalize-prepared client prepared)) "ApiUncertainty")
    (check-equal? (fake-board-value fake) "Todo")
    (check-false (member 'update_board (call-names fake))))

  (test-case "an unconfirmed close fails and stops the board"
    (define fake (ready-fake))
    (set-fake-close-without-apply! fake (list 40))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (check-refusal (lambda () (finalize-prepared client prepared))
                   "ApiUncertainty"
                   "issue 40 close was not confirmed")
    (check-equal? (fake-board-value fake) "Todo")
    (check-false (member 'update_board (call-names fake))))

  (test-case "board sync is type-strict, idempotent and recovers a lost update response"
    (test-case "lost update response is recovered by the confirmation read"
      (define fake (ready-fake))
      (set-fake-board-fail-after-apply! fake #t)
      (define client (fake-client fake))
      (define prepared (prepare-finalization client (trusted-config) (envelope)))
      (finalize-prepared client prepared)
      (define (updates)
        (length (for/list ([call (in-list (call-order fake))]
                           #:when (eq? (car call) 'update_board))
                  call)))
      (define first-count (updates))
      (check-equal? first-count 1)
      (finalize-prepared client prepared)
      (check-equal? (updates) first-count))
    (test-case "a type-cast-equal but not type-equal value is rewritten"
      (define fake (ready-fake))
      (set-fake-board-value! fake #t)
      (define client (fake-client fake))
      (define prepared
        (prepare-finalization client
                              (hash-set-depth (trusted-config) '("board" "value") 1)
                              (envelope)))
      (finalize-prepared client prepared)
      (check-equal? (fake-board-value fake) 1)
      (check-true (exact-integer? (fake-board-value fake))))
    (test-case "an unapplied update is refused"
      (define fake (ready-fake))
      (set-fake-board-without-apply! fake #t)
      (define client (fake-client fake))
      (define prepared (prepare-finalization client (trusted-config) (envelope)))
      (check-refusal (lambda () (finalize-prepared client prepared))
                     "ApiUncertainty"
                     "project field synchronization was not type-strictly confirmed")))

  (test-case "a lost merge response is recovered by the refetch"
    (define fake (ready-fake))
    (set-fake-merge-fail-after-apply! fake #t)
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (define result (finalize-prepared client prepared))
    (check-equal? (finalization-result-merge-sha result) merge-sha)
    (check-false (finalization-result-already-merged result)))

  (test-case "an already merged pull request is finalized without a merge mutation"
    (define fake (ready-fake))
    (define client (fake-client fake))
    (define prepared (prepare-finalization client (trusted-config) (envelope)))
    (set-fake-prs!
     fake
     (list
      (pull #:merged? #t #:state "closed" #:merge-sha-value merge-sha #:merged-at-value merged-at)))
    (define result (finalize-prepared client prepared))
    (check-true (finalization-result-already-merged result))
    (check-equal? (finalization-result-merge-sha result) merge-sha)
    (check-false (member 'merge (call-names fake)))))

;; ================================================================ CLI

(module+ test
  (define (to-jsexpr value)
    (cond
      [(hash? value)
       (for/fold ([acc (hash)]) ([(key item) (in-hash value)])
         (hash-set acc
                   (if (string? key)
                       (string->symbol key)
                       key)
                   (to-jsexpr item)))]
      [(list? value) (map to-jsexpr value)]
      [else value]))

  (define (temp-json value)
    (define path (make-temporary-file "wave-finalizer-~a.json"))
    (call-with-output-file path #:exists 'truncate (lambda (out) (write-json (to-jsexpr value) out)))
    path)

  (define (run-cli argv #:client [client #f])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (finalizer-main argv #:client client #:exit (lambda (status) status))))
    (values code (get-output-string out) (get-output-string err)))

  (test-case "prepare-only exits 0 with exactly the python key set and separators"
    (define fake (make-fake))
    (define-values (code out err)
      (run-cli (list "--trusted-config"
                     (temp-json (trusted-config))
                     "--envelope"
                     (temp-json (envelope))
                     "--prepare-only")
               #:client (fake-client fake)))
    (check-equal? code 0)
    (check-equal? err "")
    (define parsed (with-input-from-string out read-json))
    (check-equal? (sort (map symbol->string (hash-keys parsed)) string<?)
                  '("attestation_digest" "canonical_self_consistency_verified"
                                         "config_digest"
                                         "github_attestation_verified"
                                         "head_sha"
                                         "prepared"))
    (check-equal? out (string-append (python-json-text parsed) "\n"))
    (check-true (string-contains? out "\": "))
    (define prepared (prepare-finalization (fake-client (make-fake)) (trusted-config) (envelope)))
    (check-equal? out
                  (string-append
                   "{\"attestation_digest\": \""
                   (prepared-finalization-attestation-digest prepared)
                   "\", \"canonical_self_consistency_verified\": true, \"config_digest\": \""
                   (prepared-finalization-config-digest prepared)
                   "\", \"github_attestation_verified\": true, \"head_sha\": \""
                   head-sha
                   "\", \"prepared\": true}\n")))

  (test-case "full run exits 0 with exactly the python key set"
    (define fake (ready-fake))
    (define-values (code out err)
      (run-cli
       (list "--trusted-config" (temp-json (trusted-config)) "--envelope" (temp-json (envelope)))
       #:client (fake-client fake)))
    (check-equal? code 0)
    (check-equal? err "")
    (define parsed (with-input-from-string out read-json))
    (check-equal? (sort (map symbol->string (hash-keys parsed)) string<?)
                  '("already_merged" "canonical_self_consistency_verified"
                                     "github_attestation_verified"
                                     "head_sha"
                                     "merge_ci_verified"
                                     "merge_sha"
                                     "post_merge_synchronized"
                                     "pull_number"
                                     "repository"))
    (check-equal? out (string-append (python-json-text parsed) "\n"))
    (check-equal? (hash-ref parsed 'merge_sha) merge-sha)
    (check-equal? (hash-ref parsed 'pull_number) pull-number))

  (test-case "a refusal exits 1 with the wave-finalizer error line"
    (define bad-envelope (hash-set (envelope) "issuer" "attacker"))
    (define-values (code out err)
      (run-cli
       (list "--trusted-config" (temp-json (trusted-config)) "--envelope" (temp-json bad-envelope))
       #:client (fake-client (make-fake))))
    (check-equal? code 1)
    (check-equal? out "")
    (check-equal? err "wave-finalizer: envelope canonical self-consistency mismatch\n"))

  (test-case "an unreadable configuration file exits 1"
    (define-values (code out err)
      (run-cli (list "--trusted-config"
                     (build-path (make-temporary-file "missing-~a" 'directory) "absent.json")
                     "--envelope"
                     (temp-json (envelope)))
               #:client (fake-client (make-fake))))
    (check-equal? code 1)
    (check-equal? out "")
    (check-true (string-prefix? err "wave-finalizer: ")))

  (test-case "no injected adapter exits 2 with the argparse-parity launcher message"
    (define-values (code out err) (run-cli (list)))
    (check-equal? code 2)
    (check-equal? out "")
    (check-true (string-contains? err "usage: wave-finalizer "))
    (check-true (string-contains?
                 err
                 (string-append "wave-finalizer: error: the digest-verifying launcher must inject "
                                "an authenticated GitHub adapter\n"))))

  (test-case "missing required arguments exit 2"
    (define-values (code out err)
      (run-cli (list "--prepare-only") #:client (fake-client (make-fake))))
    (check-equal? code 2)
    (check-equal? out "")
    (check-true (string-contains? err
                                  (string-append "wave-finalizer: error: the following arguments are "
                                                 "required: --trusted-config, --envelope\n"))))
  (test-case "python-json-text uses python's default separators and sorted keys"
    (check-equal? (python-json-text (hash 'b (list 1 #t "x") 'a 'null))
                  "{\"a\": null, \"b\": [1, true, \"x\"]}")))

;; ================================================================ production wiring

(module+ test
  (test-case "the production wiring composes the frozen adapter into the client seam"
    ;; Only the composition is exercised here: constructing a wave-finalizer-adapter
    ;; creates its ONE delivery session and spawns nothing, and every wired method
    ;; must be a procedure of the documented shape.
    (define adapter (make-wave-finalizer-adapter repository #:budget 1.0 #:start-ms 0.0))
    (define client (wave-finalizer-adapter->finalizer-client adapter))
    (check-true (finalizer-client? client))
    (check-equal? (wave-finalizer-adapter-repository adapter) repository)
    (for ([method (in-list (list (finalizer-client-verify-evidence client)
                                 (finalizer-client-get-pr client)
                                 (finalizer-client-get-protection client)
                                 (finalizer-client-list-workflow-runs client)
                                 (finalizer-client-list-check-runs client)
                                 (finalizer-client-merge-pr client)
                                 (finalizer-client-get-issue client)
                                 (finalizer-client-close-issue client)
                                 (finalizer-client-get-field client)
                                 (finalizer-client-update-field client)))])
      (check-true (procedure? method)))))

;; ================================================================ differential parity

(define-runtime-path github-dir "../scripts/github")
(define-runtime-path finalizer-path "../scripts/github/wave-finalizer-controller.rkt")
(define python3 (find-executable-path "python3"))

(define python-parity-code
  (string-append
   "import json,sys,test_wave_finalizer as t\n"
   "wf=t.wf\n"
   "def outcome(name):\n"
   " c=t.FakeGitHub(); cfg=t.trusted_config(); env=t.envelope()\n"
   " def muts():\n"
   "  n=[x[0] for x in c.calls]\n"
   "  return [x for x in n if x in ('merge','close_issue','update_board')]\n"
   " try:\n"
   "  if name=='prepare-ok':\n"
   "   p=wf.prepare_finalization(c,cfg,env)\n"
   "   return {'ok':p.head_sha,'mutations':muts()}\n"
   "  if name=='finalize-ok':\n"
   "   c=t.ready_client(); p=wf.prepare_finalization(c,cfg,env)\n"
   "   r=wf.finalize_prepared(c,p)\n"
   "   return {'ok':r.merge_sha,'mutations':muts()}\n"
   "  if name=='attestation-unverified':\n"
   "   c.attestation_overrides={'verified':False}\n"
   "   wf.prepare_finalization(c,cfg,env)\n"
   "  if name=='already-merged-prepare':\n"
   "   c.prs=[t.pull(merged=True,state='closed',merge_sha=t.MERGE_SHA,merged_at=t.MERGED_AT)]\n"
   "   wf.prepare_finalization(c,cfg,env)\n"
   "  if name=='policy-unpinned':\n"
   "   c.protection=t.protection(app_id=None)\n"
   "   wf.prepare_finalization(c,cfg,env)\n"
   "  if name=='workflow-ref':\n"
   "   c.workflow_runs[t.HEAD][0]['head_branch']='other'\n"
   "   wf.prepare_finalization(c,cfg,env)\n"
   "  if name=='merge-ci-predates':\n"
   "   c=t.ready_client()\n"
   "   c.workflow_runs[t.MERGE_SHA]=[t.workflow_run(merge=True,created_at='2025-01-02T11:59:59Z')]\n"
   "   p=wf.prepare_finalization(c,cfg,env)\n"
   "   wf.finalize_prepared(c,p)\n"
   "  if name=='merge-not-confirmed':\n"
   "   c=t.ready_client(); p=wf.prepare_finalization(c,cfg,env)\n"
   "   c.merge_response={'merged':False,'sha':t.MERGE_SHA}\n"
   "   c.prs=[t.pull()]\n"
   "   wf.finalize_prepared(c,p)\n"
   "  if name=='merge-sha-mismatch':\n"
   "   c=t.ready_client(); p=wf.prepare_finalization(c,cfg,env)\n"
   "   c.merge_response={'merged':True,'sha':'5'*40}\n"
   "   wf.finalize_prepared(c,p)\n"
   "  if name=='close-unconfirmed':\n"
   "   c=t.ready_client(); p=wf.prepare_finalization(c,cfg,env)\n"
   "   c.close_without_apply={40}\n"
   "   wf.finalize_prepared(c,p)\n"
   "  if name=='board-unconfirmed':\n"
   "   c=t.ready_client(); p=wf.prepare_finalization(c,cfg,env)\n"
   "   c.board_without_apply=True\n"
   "   wf.finalize_prepared(c,p)\n"
   " except wf.FinalizerError as e:\n"
   "  return {'error':type(e).__name__,'message':str(e)}\n"
   " return {'unexpected':'scenario did not refuse'}\n"
   "print(json.dumps([outcome(n) for n in json.load(sys.stdin)]))\n"))

(define (python-outcomes names)
  (unless python3
    (error 'test-wave-finalizer-controller "python3 is required for wave-finalizer parity tests"))
  (parameterize ([current-directory github-dir])
    (define-values (proc out in err) (subprocess #f #f #f python3 "-c" python-parity-code))
    (write-json (apply list names) in)
    (close-output-port in)
    (define out-text (port->string out))
    (define err-text (port->string err))
    (subprocess-wait proc)
    (check-equal? (subprocess-status proc) 0 (string-append err-text out-text))
    (read-json (open-input-string out-text))))

;; `read-json` yields eq?-keyed hash tables while the port's values are equal?-keyed
;; hashes, and `equal?` never equates the two.  Compare a key-sorted alist form.
(define (plain value)
  (cond
    [(hash? value)
     (sort (for/list ([(key item) (in-hash value)])
             (cons (if (symbol? key)
                       (symbol->string key)
                       key)
                   (plain item)))
           string<?
           #:key car)]
    [(list? value) (map plain value)]
    [else value]))

(define (racket-outcome name)
  (define (refusal thunk)
    (with-handlers ([exn:fail:finalizer?
                     (lambda (error)
                       (hash 'error (exn:fail:finalizer-kind error) 'message (exn-message error)))])
      (thunk)))
  (cond
    [(equal? name "prepare-ok")
     (define fake (make-fake))
     (refusal
      (lambda ()
        (define prepared (prepare-finalization (fake-client fake) (trusted-config) (envelope)))
        (hash 'ok (prepared-finalization-head-sha prepared) 'mutations (mutation-strings fake))))]
    [(equal? name "finalize-ok")
     (define fake (ready-fake))
     (refusal
      (lambda ()
        (define client (fake-client fake))
        (define prepared (prepare-finalization client (trusted-config) (envelope)))
        (define result (finalize-prepared client prepared))
        (hash 'ok (finalization-result-merge-sha result) 'mutations (mutation-strings fake))))]
    [(equal? name "attestation-unverified")
     (define fake (make-fake))
     (set-fake-attestation-overrides! fake (hash "verified" #f))
     (refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope))))]
    [(equal? name "already-merged-prepare")
     (define fake (make-fake))
     (set-fake-prs!
      fake
      (list
       (pull #:merged? #t #:state "closed" #:merge-sha-value merge-sha #:merged-at-value merged-at)))
     (refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope))))]
    [(equal? name "policy-unpinned")
     (define fake (make-fake))
     (set-fake-protection! fake (protection #:app-id 'null))
     (refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope))))]
    [(equal? name "workflow-ref")
     (define fake (make-fake))
     (set-fake-workflow-runs! fake
                              (hash head-sha (list (hash-set (workflow-run) "head_branch" "other"))))
     (refusal (lambda () (prepare-finalization (fake-client fake) (trusted-config) (envelope))))]
    [(equal? name "merge-ci-predates")
     (define fake (ready-fake))
     (set-fake-workflow-runs! fake
                              (hash head-sha
                                    (list (workflow-run))
                                    merge-sha
                                    (list (workflow-run #:merge? #t
                                                        #:created-at-value "2025-01-02T11:59:59Z"))))
     (refusal (lambda ()
                (define client (fake-client fake))
                (define prepared (prepare-finalization client (trusted-config) (envelope)))
                (finalize-prepared client prepared)))]
    [(equal? name "merge-not-confirmed")
     (define fake (ready-fake))
     (refusal (lambda ()
                (define client (fake-client fake))
                (define prepared (prepare-finalization client (trusted-config) (envelope)))
                (set-fake-merge-response! fake (hash "merged" #f "sha" merge-sha))
                (set-fake-prs! fake (list (pull)))
                (finalize-prepared client prepared)))]
    [(equal? name "merge-sha-mismatch")
     (define fake (ready-fake))
     (refusal (lambda ()
                (define client (fake-client fake))
                (define prepared (prepare-finalization client (trusted-config) (envelope)))
                (set-fake-merge-response! fake (hash "merged" #t "sha" (make-string 40 #\5)))
                (finalize-prepared client prepared)))]
    [(equal? name "close-unconfirmed")
     (define fake (ready-fake))
     (refusal (lambda ()
                (define client (fake-client fake))
                (define prepared (prepare-finalization client (trusted-config) (envelope)))
                (set-fake-close-without-apply! fake (list 40))
                (finalize-prepared client prepared)))]
    [(equal? name "board-unconfirmed")
     (define fake (ready-fake))
     (refusal (lambda ()
                (define client (fake-client fake))
                (define prepared (prepare-finalization client (trusted-config) (envelope)))
                (set-fake-board-without-apply! fake #t)
                (finalize-prepared client prepared)))]
    [else (hash 'unexpected "unknown scenario")]))

(define (mutation-strings fake)
  (for/list ([name (in-list (mutation-names fake))])
    (symbol->string name)))

;; ------------------------------------------------------------ runnable CLI
;; The deleted artifact is executable, so the replacement must be too.  These
;; cases pin the launcher contract: without an injected, digest-verifying
;; adapter the CLI refuses with exit 2 and Python's exact message; with one it
;; reaches the normal argument validation; and a failing verifier is a typed
;; refusal (exit 1, `wave-finalizer: ` prefix), never a silent success.
(define (run-cli-process . args)
  (define-values (proc out in err)
    (apply subprocess
           #f
           #f
           #f
           (find-executable-path "env")
           "-u"
           "Q_WAVE_FINALIZER_REPO"
           "-u"
           "Q_WAVE_FINALIZER_ATTESTATION_CMD"
           args))
  (close-output-port in)
  (define out-text (port->string out))
  (define err-text (port->string err))
  (subprocess-wait proc)
  (values (subprocess-status proc) out-text err-text))

(module+ test
  (test-case "cli refuses without the launcher injection (exit 2, exact message)"
    (define-values (code out err)
      (run-cli-process (find-executable-path "racket") finalizer-path "--trusted-config" "/tmp/none" "--envelope" "/tmp/none"))
    (check-equal? code 2)
    (check-equal? out "")
    (check-true (string-contains? err missing-launcher-message)))

  (test-case "cli validates arguments once an adapter is injected"
    (define-values (code out err)
      (run-cli-process "Q_WAVE_FINALIZER_REPO=coinerd/q"
               "Q_WAVE_FINALIZER_ATTESTATION_CMD=true"
               (find-executable-path "racket")
               finalizer-path))
    (check-equal? code 2)
    (check-equal? out "")
    (check-true (string-contains? err "the following arguments are required: --trusted-config, --envelope")))

  (test-case "a bad input is a typed refusal with the python prefix, never a success"
    (define config-file (make-temporary-file "wf-cli-config-~a.json"))
    (define envelope-file (make-temporary-file "wf-cli-envelope-~a.json"))
    (call-with-output-file config-file (lambda (out) (display "{}" out)) #:exists 'truncate)
    (call-with-output-file envelope-file (lambda (out) (display "{}" out)) #:exists 'truncate)
    (dynamic-wind
     void
     (lambda ()
       (define-values (code out err)
         (run-cli-process "Q_WAVE_FINALIZER_REPO=coinerd/q"
                  "Q_WAVE_FINALIZER_ATTESTATION_CMD=false"
                  (find-executable-path "racket")
                  finalizer-path
                  "--trusted-config"
                  (path->string config-file)
                  "--envelope"
                  (path->string envelope-file)))
       ;; An empty trusted configuration cannot be authenticated: the CLI must
       ;; exit 1 with Python's stderr prefix and print nothing on stdout.
       (check-equal? code 1)
       (check-equal? out "")
       (check-true (string-prefix? err "wave-finalizer: ")))
     (lambda ()
       (delete-file config-file)
       (delete-file envelope-file)))))

(module+ test
  (test-case "differential parity table matches the python controller"
    (define names
      (list "prepare-ok"
            "finalize-ok"
            "attestation-unverified"
            "already-merged-prepare"
            "policy-unpinned"
            "workflow-ref"
            "merge-ci-predates"
            "merge-not-confirmed"
            "merge-sha-mismatch"
            "close-unconfirmed"
            "board-unconfirmed"))
    (define theirs (python-outcomes names))
    (for ([name (in-list names)]
          [expected (in-list theirs)])
      (test-case (format "parity ~a" name)
        (check-equal? (plain (racket-outcome name)) (plain expected))))))
