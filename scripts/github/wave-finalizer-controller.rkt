#lang racket

;; scripts/github/wave-finalizer-controller.rkt -- the finalizing controller of the
;; protected-wave finalizer port (port job J3).
;;
;; Ported from `scripts/github/wave-finalizer.py`: `_verify_attestation`,
;; `_validate_pr`, `_verify_checks`, `_observe`, `prepare_finalization`,
;; `finalize_prepared`, `_sync_issues`, `_sync_board` and the `main` CLI.  The
;; model layer (`wave-finalizer.rkt`: trusted configuration, envelope, branch
;; policy, canonical JSON, digests, the sealed prepared snapshot, typed equality,
;; timestamps and the API-shape readers) and the authenticated adapter
;; (`wave-finalizer-adapter.rkt`) are frozen modules this one composes; it adds no
;; validation rules of its own.
;;
;; Authorization comes only from the separately supplied trusted configuration.
;; The envelope digest is a canonical self-consistency check, never integrity,
;; authenticity or authorization: GitHub-attested evidence is verified before any
;; GitHub fact is read, and no issue/project mutation is reachable until a merge
;; is confirmed AND the post-merge CI is verified for the exact merge SHA (whose
;; workflow runs may not reuse pre-merge run ids and must postdate the merge).
;;
;; Client protocol.  Python injects a `GitHubClient` Protocol value; the frozen
;; adapter struct exposes no constructor a test could fake (only
;; `make-wave-finalizer-adapter`, which builds a real delivery session), so the
;; port's seam is `finalizer-client`: a transparent struct of procedures with the
;; same method-for-method shape.  Production wiring is one call,
;; `(wave-finalizer-adapter->finalizer-client adapter)`; tests pass a fake bundle.
;;
;; Freshness law.  Python re-reads GitHub fresh on every `_observe`, and a stale
;; read would silently turn every "changed after prepare" refusal into a no-op.
;; The adapter memoizes each read in its ONE api session, so every read this
;; controller performs carries `#:refresh? #t`.  The adapter honours that for the
;; pull request and (internally) for workflow runs; branch protection and issues
;; expose no refresh argument, so the production wiring reaches the adapter's own
;; delivery session api closure (`delivery-api-session-api-call`) with
;; `#:refresh? #t` -- the SAME session and the same budget window, so the
;; one-budget law (one adapter = one session = one budget) still holds and no
;; second session is ever created.  Check-run lists are refreshed the same way:
;; the adapter pages `commits/<sha>/check-runs` through that session with
;; `#:refresh? #t` (it does NOT use the memoizing `checks` closure when a fresh
;; read is requested), so a check that was revoked, re-run or re-concluded
;; between prepare and finalize is observed here rather than trusted.
;;
;; Parity notes (always fail-closed):
;;  * refusal kinds are the Python class names in `exn:fail:finalizer-kind`, and
;;    messages are byte-identical to the Python controller's.
;;  * `_sha`/`_time` are not exported by the model layer, so this module carries
;;    the same two checkers with the caller's error kind (Python passed one too).
;;  * the stdout payload is rendered with Python's default `json.dumps` separators
;;    (", " / ": ") and sorted keys, i.e. plain `json.dumps(value, sort_keys=True)`.

;; The frozen adapter exports its session but not its api closure, so the fresh
;; protection/issue reads go through the session's own exported api closure
;; (`delivery-api-session-api-call` from scripts/gsd-delivery-api.rkt).
(require json
         racket/bytes
         racket/list
         racket/string
         file/sha1
         openssl
         "../gsd-delivery-api.rkt"
         "wave-finalizer.rkt"
         "wave-finalizer-adapter.rkt")

(provide (struct-out finalizer-client)
         make-finalizer-client
         wave-finalizer-adapter->finalizer-client
         verify-attestation
         verify-checks
         prepare-finalization
         finalize-prepared
         finalize
         python-json-text
         finalizer-main
         launcher-arguments
         attestation-verifier
         missing-launcher-message)

;; ================================================================ errors

(define (refuse kind message)
  (raise (exn:fail:finalizer message (current-continuation-marks) kind)))

(define (uncertain message)
  (refuse "ApiUncertainty" message))
(define (validation-error message)
  (refuse "ValidationError" message))
(define (policy-error message)
  (refuse "PolicyError" message))
(define (check-error message)
  (refuse "CheckError" message))
(define (merge-error message)
  (refuse "MergeError" message))
(define (attestation-error message)
  (refuse "AttestationError" message))
(define (configuration-error message)
  (refuse "ConfigurationError" message))

(define (api-uncertainty? error)
  (and (exn:fail:finalizer? error) (equal? (exn:fail:finalizer-kind error) "ApiUncertainty")))

;; Python `_api_call(label, operation, ...)`: a finalizer refusal is re-raised
;; unchanged, every other failure becomes an ApiUncertainty naming the operation.
(define (api-call label thunk)
  (with-handlers ([(lambda (error) (exn:fail:finalizer? error)) (lambda (error) (raise error))]
                  [exn:fail? (lambda (error)
                               (uncertain (format "GitHub API uncertainty during ~a: ~a"
                                                  label
                                                  (exn-message error))))])
    (thunk)))

;; ================================================================ digests

(define (sha256-hex-of-string text)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text))))

;; Python compared every digest/secret with hmac.compare_digest; a mismatch
;; position must not leak through the first differing character.
(define (constant-time-equal? left right)
  (and (string? left)
       (string? right)
       (let* ([a (string->bytes/utf-8 left)]
              [b (string->bytes/utf-8 right)]
              [a-length (bytes-length a)]
              [b-length (bytes-length b)]
              [limit (max a-length b-length)])
         (zero? (for/fold ([acc (bitwise-xor a-length b-length)]) ([index (in-range limit)])
                  (bitwise-ior acc
                               (bitwise-xor (if (< index a-length)
                                                (bytes-ref a index)
                                                0)
                                            (if (< index b-length)
                                                (bytes-ref b index)
                                                0))))))))

;; ================================================================ client protocol
;; Python: the GitHubClient Protocol.  `#:refresh?` is the port's explicit
;; freshness request (see the freshness law above); the mutation methods never
;; take it.

(struct finalizer-client
        (verify-evidence get-pr
                         get-protection
                         list-workflow-runs
                         list-check-runs
                         merge-pr
                         get-issue
                         close-issue
                         get-field
                         update-field)
  #:transparent)

(define (make-finalizer-client #:verify-evidence verify-evidence
                               #:get-pr get-pr
                               #:get-protection get-protection
                               #:list-workflow-runs list-workflow-runs
                               #:list-check-runs list-check-runs
                               #:merge-pr merge-pr
                               #:get-issue get-issue
                               #:close-issue close-issue
                               #:get-field get-field
                               #:update-field update-field)
  (for ([method (in-list (list verify-evidence
                               get-pr
                               get-protection
                               list-workflow-runs
                               list-check-runs
                               merge-pr
                               get-issue
                               close-issue
                               get-field
                               update-field))])
    (unless (procedure? method)
      (configuration-error "finalizer-client methods must be procedures")))
  (finalizer-client verify-evidence
                    get-pr
                    get-protection
                    list-workflow-runs
                    list-check-runs
                    merge-pr
                    get-issue
                    close-issue
                    get-field
                    update-field))

;; Production wiring: the frozen adapter, read fresh.  Branch protection and
;; issues have no refresh argument in the adapter, so they go through the
;; adapter's OWN session api closure (exported via `wave-finalizer-adapter-session`
;; + `delivery-api-session-api-call`) with `#:refresh? #t`.  Same session, same
;; budget window, no second session; a transport failure there is wrapped by this
;; module's `api-call` with the very label the adapter would have used.
(define (wave-finalizer-adapter->finalizer-client adapter)
  (define repository (wave-finalizer-adapter-repository adapter))
  (define session-api (delivery-api-session-api-call (wave-finalizer-adapter-session adapter)))
  (define (fresh-api route label)
    (api-mapping (session-api repository route #:refresh? #t) label))
  (make-finalizer-client
   #:verify-evidence (lambda (repo reference)
                       (wave-finalizer-adapter-verify-evidence adapter repo reference))
   #:get-pr (lambda (repo number #:refresh? [refresh? #f])
              (wave-finalizer-adapter-get-pr adapter number #:refresh? refresh?))
   #:get-protection (lambda (repo ref #:refresh? [refresh? #f])
                      (if refresh?
                          (fresh-api (format "branches/~a/protection" ref) "branch protection")
                          (wave-finalizer-adapter-get-protection adapter ref)))
   #:list-workflow-runs
   (lambda (repo sha #:refresh? [refresh? #f])
     ;; The adapter's workflow-run read is already refresh-only (manual pages,
     ;; `#:refresh? #t` per page); a `#:refresh? #f` request cannot un-refresh it.
     (wave-finalizer-adapter-list-workflow-runs adapter sha))
   #:list-check-runs (lambda (repo sha #:refresh? [refresh? #f])
                       ;; Fresh like every other post-prepare read: the adapter pages the
                       ;; check-run route itself with `#:refresh? #t` (same session/budget),
                       ;; so a revoked or re-run check between prepare and finalize is seen.
                       (wave-finalizer-adapter-list-check-runs adapter sha #:refresh? refresh?))
   #:merge-pr
   (lambda (repo number #:sha sha #:merge-method merge-method)
     (wave-finalizer-adapter-merge-pr adapter number #:sha sha #:merge-method merge-method))
   #:get-issue (lambda (repo number #:refresh? [refresh? #f])
                 (if refresh?
                     (fresh-api (format "issues/~a" number) (format "issue ~a" number))
                     (wave-finalizer-adapter-get-issue adapter number)))
   #:close-issue (lambda (repo number) (wave-finalizer-adapter-close-issue adapter number))
   #:get-field (lambda (repo project-id item-id field-id)
                 ;; The GraphQL field read is a fresh request every time (the adapter runs it
                 ;; through the runner, never through the read memo).
                 (wave-finalizer-adapter-get-field adapter project-id item-id field-id))
   #:update-field (lambda (repo project-id item-id field-id value)
                    (wave-finalizer-adapter-update-field adapter project-id item-id field-id value))))

;; ================================================================ small readers
;; Python `_sha`/`_time` are private to the model layer; the controller passed its
;; own error kind, so the checks are repeated here with the same messages.

(define sha-rx #px"^[0-9a-fA-F]{40}$")

(define (sha-value value label kind)
  (unless (and (string? value) (regexp-match? sha-rx value))
    (refuse kind (format "~a must be a full 40-character hexadecimal SHA" label)))
  value)

(define (time-value value label)
  (parse-timestamp value label))

(define (merged? pr)
  (eq? (hash-ref pr "merged" #f) #t))

;; Python `repr()` of a configured workflow identity (`str` or positive `int`), used
;; by "required workflow '...' has no run".
(define (python-repr value)
  (cond
    [(string? value)
     (define quote
       (if (and (string-contains? value "'") (not (string-contains? value "\""))) "\"" "'"))
     (define out (open-output-string))
     (display quote out)
     (for ([char (in-string value)])
       (cond
         [(char=? char (string-ref quote 0))
          (display "\\" out)
          (write-char char out)]
         [(char=? char #\\) (display "\\\\" out)]
         [(char=? char #\newline) (display "\\n" out)]
         [(char=? char #\return) (display "\\r" out)]
         [(char=? char #\tab) (display "\\t" out)]
         [else (write-char char out)]))
     (display quote out)
     (get-output-string out)]
    [(exact-integer? value) (number->string value)]
    [else (format "~a" value)]))

;; ================================================================ attestation

(define attestation-keys
  (list "verified"
        "reference"
        "repository"
        "pull_number"
        "head_sha"
        "projection_digest"
        "issuer"
        "latest"))

;; Python `_verify_attestation`: the envelope issuer must be the configured one,
;; the GitHub-attested evidence must bind EXACTLY the attested projection, and the
;; digest of that evidence is what `finalize-prepared` re-compares.
(define (verify-attestation client config envelope)
  (unless (equal? (verified-envelope-issuer envelope)
                  (trusted-configuration-attestation-issuer config))
    (attestation-error "envelope issuer is not the configured issuer"))
  (define reference (verified-envelope-evidence-reference envelope))
  (define projection (verified-envelope-projection envelope))
  (define evidence
    (api-mapping
     (api-call "verify GitHub-attested evidence"
               (lambda ()
                 ((finalizer-client-verify-evidence client) (trusted-configuration-repository config)
                                                            reference)))
     "attested evidence"))
  (unless (and (= (hash-count evidence) (length attestation-keys))
               (for/and ([key (in-list attestation-keys)])
                 (hash-has-key? evidence key)))
    (attestation-error "attested evidence response has missing or unknown fields"))
  (define expected
    (hash "verified"
          #t
          "reference"
          reference
          "repository"
          (trusted-configuration-repository config)
          "pull_number"
          (external-projection-pull-number projection)
          "head_sha"
          (external-projection-head-sha projection)
          "projection_digest"
          (verified-envelope-projection-digest envelope)
          "issuer"
          (trusted-configuration-attestation-issuer config)
          "latest"
          #t))
  (unless (typed-equal? evidence expected)
    (attestation-error "GitHub-attested evidence is unverified, stale, or not exactly bound"))
  (sha256-hex-of-string (finalizer-canonical-json evidence "attested evidence" "AttestationError")))

;; ================================================================ pull request

;; Python `_validate_pr`: identity, state, base, head and the configured workflow
;; ref must all match the trusted configuration and the attested projection.
(define (validate-pr value config projection)
  (define pr (api-mapping value "pull request"))
  (define number (api-nested pr (list "number") "pull request"))
  (unless (and (not (boolean? number)) (equal? number (external-projection-pull-number projection)))
    (validation-error "pull request number does not match the attested projection"))
  (define state (api-nested pr (list "state") "pull request"))
  (unless (member state (list "open" "closed"))
    (uncertain "pull request state is malformed"))
  (define is-merged (api-bool (api-nested pr (list "merged") "pull request") "pull request.merged"))
  (when (and (not is-merged) (not (equal? state "open")))
    (validation-error "unmerged pull request is not open"))
  (unless (equal? (api-nested pr (list "base" "ref") "pull request")
                  (trusted-configuration-base-ref config))
    (validation-error "pull request base ref does not match trusted configuration"))
  (unless (equal? (api-nested pr (list "base" "repo" "full_name") "pull request")
                  (trusted-configuration-repository config))
    (validation-error "pull request repository does not match trusted configuration"))
  (define head-sha (api-nested pr (list "head" "sha") "pull request"))
  (unless (equal? head-sha (external-projection-head-sha projection))
    (validation-error "pull request head does not match attested projection"))
  (sha-value head-sha "pull request head" "ApiUncertainty")
  (define head-ref (api-nested pr (list "head" "ref") "pull request"))
  (api-nested pr (list "head" "repo" "full_name") "pull request")
  (when (for/or ([contract (in-list (trusted-configuration-required-workflows config))])
          (not (equal? (workflow-contract-ref contract) head-ref)))
    (check-error "configured workflow ref does not exactly match pull request head ref"))
  (when (member (external-projection-pull-number projection)
                (trusted-configuration-wave-issue-numbers config))
    (configuration-error "wave issue numbers must never include the pull request number"))
  (when is-merged
    (sha-value (hash-ref pr "merge_commit_sha" #f) "pull request merge SHA" "ApiUncertainty")
    (time-value (hash-ref pr "merged_at" #f) "pull request.merged_at"))
  pr)

;; Python `_confirmed_merge`.
(define (confirmed-merge pr)
  (unless (merged? pr)
    (merge-error "refetched pull request is not merged"))
  (values (sha-value (hash-ref pr "merge_commit_sha" #f) "refetched merge SHA" "MergeError")
          (time-value (hash-ref pr "merged_at" #f) "refetched pull request.merged_at")))

;; ================================================================ checks

(define (identity-matches? run workflow)
  (if (exact-integer? workflow)
      (equal? (hash-ref run "workflow_id" #f) workflow)
      (equal? (hash-ref run "path" #f) workflow)))

(define (latest-attempt ranked)
  ;; max over the (run_number, run_attempt) rank, compared lexicographically like
  ;; Python's tuple comparison.
  (define (rank<? left right)
    (or (< (first left) (first right))
        (and (= (first left) (first right)) (< (second left) (second right)))))
  (define sorted (sort ranked rank<? #:key car))
  (define best (car (last sorted)))
  (for/list ([entry (in-list sorted)]
             #:when (equal? (car entry) best))
    (cdr entry)))

(define (same-context-set? contracts policy)
  (equal? (sort (remove-duplicates (map workflow-contract-context contracts)) string<?)
          (protection-policy-required-contexts policy)))

;; Python `_verify_checks`: the latest attempt of every required workflow must be
;; a completed, successful `push`/`pull_request` run of the exact repository, ref
;; and SHA; it must carry exactly one matching check run in its own suite, from the
;; exact App, for the exact SHA, and (post-merge) it must not reuse pre-merge run
;; ids and must not predate the merge.  Returns the sorted selected run ids.
(define (verify-checks client
                       config
                       sha
                       contracts
                       policy
                       #:forbidden-run-ids [forbidden-run-ids '()]
                       #:not-before [not-before #f])
  (define repository (trusted-configuration-repository config))
  (define workflows
    (api-items
     (api-call "list workflow runs"
               (lambda ()
                 ((finalizer-client-list-workflow-runs client) repository sha #:refresh? #t)))
     "workflow runs"))
  (define checks
    (api-items (api-call "list check runs"
                         (lambda () ((finalizer-client-list-check-runs client) repository sha)))
               "check runs"))
  (when (and policy (not (same-context-set? contracts policy)))
    (policy-error "trusted workflow contracts must exactly cover protected contexts"))
  (define (check-suite-id check)
    (define suite (hash-ref check "check_suite" #f))
    (unless (hash? suite)
      (uncertain "check run check_suite is malformed"))
    (hash-ref suite "id" #f))
  (define selected-ids
    (for/list ([contract (in-list contracts)])
      (define candidates
        (filter (lambda (run) (identity-matches? run (workflow-contract-workflow contract)))
                workflows))
      (when (null? candidates)
        (check-error (format "required workflow ~a has no run"
                             (python-repr (workflow-contract-workflow contract)))))
      (define ranked
        (for/list ([run (in-list candidates)])
          (cons (list (api-positive-int (hash-ref run "run_number" #f) "workflow run_number")
                      (api-positive-int (hash-ref run "run_attempt" #f) "workflow run_attempt"))
                run)))
      (define latest (latest-attempt ranked))
      (unless (= 1 (length latest))
        (uncertain "latest workflow attempt is ambiguous"))
      (define run (car latest))
      (define actual
        (hash "repository"
              (api-nested run (list "repository" "full_name") "workflow run")
              "ref"
              (hash-ref run "head_branch" #f)
              "event"
              (hash-ref run "event" #f)
              "SHA"
              (hash-ref run "head_sha" #f)))
      (define expected
        (hash "repository"
              repository
              "ref"
              (workflow-contract-ref contract)
              "event"
              (workflow-contract-event contract)
              "SHA"
              sha))
      (unless (typed-equal? actual expected)
        (check-error "latest workflow attempt provenance mismatch"))
      (unless (and (equal? (hash-ref run "status" #f) "completed")
                   (equal? (hash-ref run "conclusion" #f) "success"))
        (check-error "latest workflow attempt is not successful"))
      (define run-id (api-positive-int (hash-ref run "id" #f) "workflow run id"))
      (when (member run-id forbidden-run-ids)
        (check-error "merge CI reused pre-merge workflow evidence"))
      (when (and not-before
                 (< (time-value (hash-ref run "created_at" #f) "workflow run.created_at") not-before))
        (check-error "merge CI predates the confirmed merge"))
      (define suite-id
        (api-positive-int (hash-ref run "check_suite_id" #f) "workflow check_suite_id"))
      (define matching
        (for/list ([check (in-list checks)]
                   #:when
                   (and (equal? (hash-ref check "name" #f) (workflow-contract-context contract))
                        (equal? (check-suite-id check) suite-id)))
          check))
      (unless (= 1 (length matching))
        (check-error "latest attempt must have exactly one required check run"))
      (define check (car matching))
      (unless (equal? (hash-ref check "head_sha" #f) sha)
        (check-error "required check is not for the exact SHA"))
      (unless (and (equal? (hash-ref check "status" #f) "completed")
                   (equal? (hash-ref check "conclusion" #f) "success"))
        (check-error "required check is not successful"))
      (define app (hash-ref check "app" #f))
      (unless (hash? app)
        (uncertain "required check App provenance is missing"))
      (unless (and (equal? (hash-ref app "slug" #f) (workflow-contract-app-slug contract))
                   (equal? (hash-ref app "id" #f) (workflow-contract-app-id contract)))
        (check-error "required check App provenance mismatch"))
      (when (and policy
                 (not (equal? (protection-policy-app-id-for policy
                                                            (workflow-contract-context contract))
                              (workflow-contract-app-id contract))))
        (policy-error "workflow App does not match pinned branch protection App"))
      run-id))
  (sort selected-ids <))

;; ================================================================ observe

;; Python `_observe`: pull request, then branch protection, then the required
;; workflow checks.  `#:require-unmerged?` is `prepare`'s "never prepare a merged
;; pull request" refusal.
(define (observe client config projection #:require-unmerged? [require-unmerged? #f])
  (define repository (trusted-configuration-repository config))
  (define pr
    (validate-pr
     (api-call "get pull request"
               (lambda ()
                 ((finalizer-client-get-pr client) repository
                                                   (external-projection-pull-number projection)
                                                   #:refresh? #t)))
     config
     projection))
  (when (and require-unmerged? (merged? pr))
    (merge-error "cannot prepare finalization for an already merged pull request"))
  (define policy
    (parse-protection-policy (api-call "get branch protection"
                                       (lambda ()
                                         ((finalizer-client-get-protection client)
                                          repository
                                          (trusted-configuration-base-ref config)
                                          #:refresh? #t)))))
  (define run-ids
    (verify-checks client
                   config
                   (external-projection-head-sha projection)
                   (trusted-configuration-required-workflows config)
                   policy))
  (values pr policy run-ids))

;; ================================================================ prepare

;; Authenticate evidence and read every pre-merge fact without any mutation.
(define (prepare-finalization client trusted-configuration-value envelope-value)
  (define config (load-trusted-configuration trusted-configuration-value))
  (define envelope (verify-external-envelope envelope-value))
  (define attestation-digest (verify-attestation client config envelope))
  (define projection (verified-envelope-projection envelope))
  (define-values (pr policy run-ids) (observe client config projection #:require-unmerged? #t))
  (issue-prepared-finalization (trusted-configuration-canonical-json config)
                               (verified-envelope-canonical-json envelope)
                               (trusted-configuration-digest config)
                               (verified-envelope-digest envelope)
                               attestation-digest
                               (external-projection-head-sha projection)
                               policy
                               run-ids
                               #f))

;; ================================================================ post-merge sync

(define (validated-issue value number)
  (define issue (api-mapping value (format "issue ~a" number)))
  (define actual (hash-ref issue "number" #f))
  (unless (and (not (boolean? actual)) (equal? actual number))
    (uncertain (format "issue ~a identity is malformed" number)))
  (when (hash-has-key? issue "pull_request")
    (validation-error (format "configured issue ~a is a pull request" number)))
  (unless (member (hash-ref issue "state" #f) (list "open" "closed"))
    (uncertain (format "issue ~a state is malformed" number)))
  issue)

;; Every issue read is a FRESH read: the close confirmation must observe the state
;; after the (possibly lost) mutation, never the memoized pre-close state.
(define (read-issue client config number)
  (validated-issue
   (api-call (format "get issue ~a" number)
             (lambda ()
               ((finalizer-client-get-issue client) (trusted-configuration-repository config)
                                                    number
                                                    #:refresh? #t)))
   number))

;; Python `_sync_issues`: only the configured issue numbers, never the pull
;; request, and every close must be confirmed by a fresh read.
(define (sync-issues client config)
  (for ([number (in-list (trusted-configuration-wave-issue-numbers config))])
    (define issue (read-issue client config number))
    (when (equal? (hash-ref issue "state") "open")
      (with-handlers ([api-uncertainty? (lambda (close-error)
                                          ;; A lost mutation response is recoverable only when a
                                          ;; fresh read proves the exact configured issue closed.
                                          (define confirmed (read-issue client config number))
                                          (unless (equal? (hash-ref confirmed "state") "closed")
                                            (raise close-error)))])
        (api-call (format "close issue ~a" number)
                  (lambda ()
                    ((finalizer-client-close-issue client) (trusted-configuration-repository config)
                                                           number))))
      (define confirmed (read-issue client config number))
      (unless (equal? (hash-ref confirmed "state") "closed")
        (uncertain (format "issue ~a close was not confirmed" number)))))
  (void))

;; Python `_sync_board`: type-strict comparison, idempotent skip, and the same
;; lost-response recovery.
(define (sync-board client config)
  (define board (trusted-configuration-board config))
  (define desired (board-sync-value board))
  (define repository (trusted-configuration-repository config))
  (define project-id (board-sync-project-id board))
  (define item-id (board-sync-item-id board))
  (define field-id (board-sync-field-id board))
  (define (read-field label)
    (api-call label
              (lambda ()
                ((finalizer-client-get-field client) repository project-id item-id field-id))))
  (define current (read-field "read project field"))
  (unless (typed-equal? current desired)
    (with-handlers ([api-uncertainty? (lambda (update-error)
                                        (define confirmed
                                          (read-field "confirm project field after uncertain update"))
                                        (unless (typed-equal? confirmed desired)
                                          (raise update-error)))])
      (api-call
       "update project field"
       (lambda ()
         ((finalizer-client-update-field client) repository project-id item-id field-id desired))))
    (define confirmed (read-field "confirm project field"))
    (unless (typed-equal? confirmed desired)
      (uncertain "project field synchronization was not type-strictly confirmed")))
  (void))

;; ================================================================ finalize

;; Revalidate the sealed snapshot, re-verify the attestation and every pre-merge
;; fact, merge (only when not already merged), verify the post-merge CI for the
;; confirmed merge SHA, and only then synchronize issues and the board.
(define (finalize-prepared client prepared)
  (define-values (config envelope) (validate-prepared-finalization prepared))
  (define attestation-digest (verify-attestation client config envelope))
  (unless (constant-time-equal? attestation-digest
                                (prepared-finalization-attestation-digest prepared))
    (attestation-error "attestation changed after prepare"))
  (define projection (verified-envelope-projection envelope))
  (define-values (fresh-pr fresh-policy fresh-run-ids) (observe client config projection))
  (unless (equal? fresh-policy (prepared-finalization-policy prepared))
    (policy-error "branch protection changed after prepare"))
  (unless (equal? fresh-run-ids (prepared-finalization-premerge-run-ids prepared))
    (check-error "pre-merge workflow evidence changed after prepare"))
  (define already-merged (merged? fresh-pr))
  (define-values (merge-sha merged-at)
    (if already-merged
        (confirmed-merge fresh-pr)
        (let ([response #f]
              [merge-uncertainty (box #f)])
          (set! response
                (with-handlers ([api-uncertainty? (lambda (error)
                                                    (set-box! merge-uncertainty error)
                                                    #f)])
                  (api-mapping (api-call "merge pull request"
                                         (lambda ()
                                           ((finalizer-client-merge-pr client)
                                            (trusted-configuration-repository config)
                                            (external-projection-pull-number projection)
                                            #:sha (external-projection-head-sha projection)
                                            #:merge-method
                                            (trusted-configuration-merge-method config))))
                               "merge response")))
          (when (and response (not (merged? response)))
            (merge-error "GitHub did not confirm merged=true"))
          (define response-sha
            (and response (sha-value (hash-ref response "sha" #f) "merge response SHA" "MergeError")))
          (define refetched
            (validate-pr (api-call "refetch merged pull request"
                                   (lambda ()
                                     ((finalizer-client-get-pr client)
                                      (trusted-configuration-repository config)
                                      (external-projection-pull-number projection)
                                      #:refresh? #t)))
                         config
                         projection))
          (when (and (not (merged? refetched)) (unbox merge-uncertainty))
            (raise (unbox merge-uncertainty)))
          (define-values (refetched-sha refetched-at) (confirmed-merge refetched))
          (when (and response-sha (not (constant-time-equal? response-sha refetched-sha)))
            (merge-error "merge response SHA differs from refetched pull request"))
          (values refetched-sha refetched-at))))
  (verify-checks client
                 config
                 merge-sha
                 (trusted-configuration-merge-workflows config)
                 #f
                 #:forbidden-run-ids (prepared-finalization-premerge-run-ids prepared)
                 #:not-before merged-at)
  ;; No issue or project mutation is reachable before confirmed merge + merge CI.
  (sync-issues client config)
  (sync-board client config)
  (finalization-result (trusted-configuration-repository config)
                       (external-projection-pull-number projection)
                       (external-projection-head-sha projection)
                       merge-sha
                       already-merged
                       #t
                       #t))

;; Python `finalize`.
(define (finalize client trusted-configuration-value envelope-value)
  (finalize-prepared client (prepare-finalization client trusted-configuration-value envelope-value)))

;; ================================================================ CLI

;; Python: json.dumps(value, sort_keys=True) -- default separators (", " / ": ")
;; and the same scalar spellings as the canonical writer.
(define (python-json-text value)
  (define (render item)
    (cond
      [(hash? item)
       (define keys
         (sort (for/list ([(key unused) (in-hash item)])
                 (cond
                   [(string? key) key]
                   [(symbol? key) (symbol->string key)]
                   [else (format "~a" key)]))
               string<?))
       (string-append
        "{"
        (string-join
         (for/list ([key (in-list keys)])
           (string-append (finalizer-canonical-json key)
                          ": "
                          (render (hash-ref item key (hash-ref item (string->symbol key) 'null)))))
         ", ")
        "}")]
      [(or (list? item) (vector? item))
       (string-append "["
                      (string-join (map render
                                        (if (list? item)
                                            item
                                            (vector->list item)))
                                   ", ")
                      "]")]
      [else (finalizer-canonical-json item)]))
  (render value))

(define (read-json-file path)
  (call-with-input-file path (lambda (in) (read-json in))))

(define usage-line
  "usage: wave-finalizer --trusted-config TRUSTED_CONFIG --envelope ENVELOPE [--prepare-only]\n")

(define (argparse-failure exit-procedure message)
  (eprintf "~a" usage-line)
  (eprintf "wave-finalizer: error: ~a\n" message)
  (exit-procedure 2))

;; Python argparse subset: `--flag value`, `--flag=value`, `--prepare-only`.
;; Returns (values flags problem); `problem` is #f or an argparse-shaped message.
(define (parse-arguments argv)
  (define (split-flag argument)
    (define pieces (string-split argument "=" #:trim? #f))
    (if (>= (length pieces) 2)
        (values (car pieces) (string-join (cdr pieces) "="))
        (values argument #f)))
  (let loop ([remaining argv]
             [flags (hash)])
    (cond
      [(null? remaining) (values flags #f)]
      [else
       (define argument (car remaining))
       (define-values (flag inline-value) (split-flag argument))
       (cond
         [(equal? flag "--prepare-only")
          (if inline-value
              (values flags (format "unrecognized arguments: ~a" argument))
              (loop (cdr remaining) (hash-set flags 'prepare-only #t)))]
         [(member flag (list "--trusted-config" "--envelope"))
          (define key (if (equal? flag "--trusted-config") 'trusted-config 'envelope))
          (cond
            [inline-value (loop (cdr remaining) (hash-set flags key inline-value))]
            [(pair? (cdr remaining)) (loop (cddr remaining) (hash-set flags key (cadr remaining)))]
            [else (values flags (format "argument ~a: expected one argument" flag))])]
         [else (values flags (format "unrecognized arguments: ~a" argument))])])))

(define (missing-flags flags)
  (filter (lambda (key) (not (hash-has-key? flags key))) (list 'trusted-config 'envelope)))

(define (run-finalizer client flags exit-procedure)
  (with-handlers
      ([(lambda (error)
          (or (exn:fail:filesystem? error) (exn:fail:read? error) (exn:fail:finalizer? error)))
        (lambda (error)
          (eprintf "wave-finalizer: ~a\n" (exn-message error))
          (exit-procedure 1))])
    (define config-value (read-json-file (hash-ref flags 'trusted-config)))
    (define envelope-value (read-json-file (hash-ref flags 'envelope)))
    (define prepared (prepare-finalization client config-value envelope-value))
    (define output
      (if (hash-ref flags 'prepare-only #f)
          (hash 'prepared
                #t
                'head_sha
                (prepared-finalization-head-sha prepared)
                'canonical_self_consistency_verified
                #t
                'github_attestation_verified
                #t
                'config_digest
                (prepared-finalization-config-digest prepared)
                'attestation_digest
                (prepared-finalization-attestation-digest prepared))
          (let ([result (finalize-prepared client prepared)])
            (hash 'repository
                  (finalization-result-repository result)
                  'pull_number
                  (finalization-result-pull-number result)
                  'head_sha
                  (finalization-result-head-sha result)
                  'merge_sha
                  (finalization-result-merge-sha result)
                  'already_merged
                  (finalization-result-already-merged result)
                  'merge_ci_verified
                  (finalization-result-merge-ci-verified result)
                  'post_merge_synchronized
                  (finalization-result-post-merge-synchronized result)
                  'canonical_self_consistency_verified
                  #t
                  'github_attestation_verified
                  #t))))
    (printf "~a\n" (python-json-text output))
    (exit-procedure 0)))

;; Python `main(argv=None, *, client=None)`: without an injected,
;; digest-verifying adapter there is no authenticated client at all, so argparse
;; refuses with exit 2 and the exact launcher message.  `exit-procedure` is the
;; same injection the frozen adapter's CLI uses (tests pass a recorder that
;; returns the status code).
(define (finalizer-main argv #:client [client #f] #:exit [exit-procedure exit])
  (define-values (flags problem) (parse-arguments argv))
  (define missing (missing-flags flags))
  (cond
    [(not (finalizer-client? client)) (argparse-failure exit-procedure missing-launcher-message)]
    [problem (argparse-failure exit-procedure problem)]
    [(pair? missing)
     (argparse-failure
      exit-procedure
      (format "the following arguments are required: ~a"
              (string-join (for/list ([key (in-list missing)])
                             (if (eq? key 'trusted-config) "--trusted-config" "--envelope"))
                           ", ")))]
    [else (run-finalizer client flags exit-procedure)]))

;; ---------------------------------------------------------------- runnable CLI
;; A runnable replacement for `python3 scripts/github/wave-finalizer.py` must
;; exist, because the artifact being replaced is executable.  The injection
;; Python received as `client=` comes from the digest-verifying launcher, so this
;; CLI takes the same two inputs from the environment and refuses (exit 2, the
;; exact launcher message) when they are absent:
;;
;;   Q_WAVE_FINALIZER_REPO           owner/name of the repository to authenticate
;;   Q_WAVE_FINALIZER_ATTESTATION_CMD
;;       command reading {"repository":..., "reference":...} as JSON on stdin and
;;       printing the attestation response JSON on stdout; it MUST exit non-zero
;;       when the evidence does not verify.  Its output is the only attestation
;;       input this CLI accepts -- there is no fallback.
;;
;; No GraphQL field query/mutation is supplied here: a configuration that
;; requires board synchronization refuses rather than guessing a query, exactly
;; like a missing adapter.
(define (launcher-arguments)
  ;; Returns (values repo attestation-command) or (values #f #f): the launcher
  ;; contract is all-or-nothing, never a partial injection.
  (define repo (getenv "Q_WAVE_FINALIZER_REPO"))
  (define attestation (getenv "Q_WAVE_FINALIZER_ATTESTATION_CMD"))
  (if (and (nonblank-string? repo) (nonblank-string? attestation))
      (values repo attestation)
      (values #f #f)))

(define (attestation-verifier command)
  (lambda (repository reference)
    (define payload (python-json-text (hash 'repository repository 'reference reference)))
    (define-values (proc out in err)
      (subprocess #f #f #f "/bin/sh" "-c" (string-append command " 2>/dev/null")))
    (display payload in)
    (close-output-port in)
    (define out-text (port->string out))
    (define err-text (port->string err))
    (subprocess-wait proc)
    (unless (zero? (subprocess-status proc))
      (attestation-error (string-append "attestation verifier failed: " err-text)))
    (with-handlers ([exn:fail?
                     (lambda (_error)
                       (attestation-error "attestation verifier produced malformed JSON"))])
      (read-json (open-input-string out-text)))))

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(module+ main
  (define argv (vector->list (current-command-line-arguments)))
  (define-values (repo attestation) (launcher-arguments))
  (if (not repo)
      (begin
        (eprintf "~a\n"
                 (string-append "wave-finalizer: error: "
                                missing-launcher-message))
        (exit 2))
      (let* ([adapter (make-wave-finalizer-adapter repo #:verify-evidence (attestation-verifier attestation))]
             [client (wave-finalizer-adapter->finalizer-client adapter)])
        (finalizer-main argv #:client client))))
