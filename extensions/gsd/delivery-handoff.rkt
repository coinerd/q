#lang racket/base
;; Durable coordinator handoff, distinct from implementation DONE/outbox state.
;; The authenticated external controller owns publication readback. No worker
;; approves/merges PRs and no runtime module imports scripts-layer modules.
;;
;; v1.00.30 (delivery runtime finalization): every proof decision the
;; coordinator loop makes is factored HERE so go-orchestrator never
;; re-implements it. The ONLY thing that satisfies the wave-advance gate,
;; the final full-loop done-wave check, and the pending→delivered ledger
;; reconcile is an exact campaign/wave delivered proof carrying a full
;; 40-hex merge SHA from the authenticated controller. Local evidence
;; files are never parsed as proof — stale regex fixtures cannot count
;; as true production proof.
(require json
         racket/file
         racket/format
         racket/path
         racket/runtime-path
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "../../sandbox/subprocess.rkt")
(provide verified-wave-merge-sha
         delivery-readback
         delivered-proof-merge-sha
         undelivered-proof-reason
         delivery-pending-wave
         delivery-action-hint
         delivery-handoff-path
         reconcile-delivered-handoff!
         persist-delivery-handoff!
         ready-for-run-checkpoint
         delivered-predecessor-resolver
         ;; B2b: the coordinator's default controller shells gsd-delivery.py
         ;; through the SAME credential boundary as readback; these helpers
         ;; are exported for reuse (never re-implement the boundary).
         controller-environment
         redact-delivery-text)
(define-runtime-path controller "../../scripts/gsd-delivery.py")

;; ============================================================
;; Credential boundary for the coordinator-owned trusted controller
;; ============================================================
;;
;; sandbox run-subprocess scrubs GH_TOKEN/GITHUB_TOKEN (secret patterns)
;; from every subprocess environment by default — that default stays in
;; force for ALL wave workers and every other subprocess. The delivery
;; controller is the single deliberate exception: it is coordinator-owned
;; trusted code (this checkout, direct-exec'd, no shell) that MUST
;; authenticate to GitHub to prove publication, so the coordinator's own
;; credential variables are copied from current-environment-variables and
;; re-added to an otherwise scrubbed environment for THAT ONE process.
;; Tokens never reach worker prompts, never appear in campaign messages
;; (reasons are redacted below), and the controller itself never echoes
;; credential material. When no token is held, gh config credential
;; storage (gh auth login) is the required path; the controller's
;; auth-failure reason plus delivery-action-hint make that explicit
;; instead of silently degrading.
(define CREDENTIAL-ENV-NAMES '("GH_TOKEN" "GITHUB_TOKEN"))

(define (controller-environment)
  (define incoming (current-environment-variables))
  (define env (sanitize-env))
  (for ([name (in-list CREDENTIAL-ENV-NAMES)])
    (define value (environment-variables-ref incoming (string->bytes/utf-8 name)))
    (when value
      (environment-variables-set! env (string->bytes/utf-8 name) value)))
  env)

;; Credential-shaped assignments never surface in operator-visible text.
(define (redact-delivery-text text)
  (if (string? text)
      (regexp-replace* #px"(?i:((?:GH|GITHUB)_TOKEN|token|password|secret)[=:][^ \\\"\n]+)"
                       text
                       "\\1=[redacted]")
      "invalid delivery controller response"))

(define (delivery-readback base-dir plan-id wave-index)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (hasheq 'status "delivery-pending" 'reason (redact-delivery-text (exn-message e))))])
    (define repo
      (if (or (directory-exists? (build-path base-dir ".git"))
              (file-exists? (build-path base-dir ".git")))
          base-dir
          (build-path base-dir "q")))
    (define result
      (run-subprocess "python3"
                      #:args (list (path->string controller)
                                   "status"
                                   "--repo"
                                   (path->string (path->complete-path repo))
                                   "--plan"
                                   plan-id
                                   "--wave"
                                   (number->string wave-index))
                      #:directory base-dir
                      #:environment (controller-environment)
                      #:timeout 240
                      #:process-group? #t))
    (define data (string->jsexpr (subprocess-result-stdout result)))
    (if (and (hash? data)
             (not (subprocess-result-timed-out? result))
             (not (subprocess-result-truncated? result))
             (or (equal? (hash-ref data 'status #f) "delivery-pending")
                 (and (zero? (subprocess-result-exit-code result))
                      (equal? (hash-ref data 'plan-id #f) plan-id)
                      (equal? (hash-ref data 'wave #f) wave-index))))
        data
        (hasheq 'status "delivery-pending" 'reason "delivery controller failed or timed out"))))

;; ============================================================
;; Proof validation (single source of truth for the coordinator loop)
;; ============================================================

;; A proof counts ONLY when the authenticated controller says "delivered"
;; for the EXACT campaign and wave and carries a full 40-hex merge SHA.
;; Anything else — wrong campaign, wrong wave, short/sha-less payload,
;; plain local file data — is not delivery proof.
(define FULL-MERGE-SHA-RX #px"^[0-9a-f]{40}$")

(define (delivered-proof-merge-sha proof plan-id wave-index)
  (define sha (and (hash? proof) (hash-ref proof 'merge-sha #f)))
  (and (hash? proof)
       (equal? (hash-ref proof 'status #f) "delivered")
       (equal? (hash-ref proof 'plan-id #f) plan-id)
       (equal? (hash-ref proof 'wave #f) wave-index)
       (string? sha)
       (regexp-match? FULL-MERGE-SHA-RX sha)
       sha))

;; Honest, redacted, operator-actionable reason for an undelivered proof.
(define (undelivered-proof-reason proof)
  (redact-delivery-text (cond
                          [(not (hash? proof)) "invalid delivery controller response"]
                          [(equal? (hash-ref proof 'status #f) "delivered")
                           "delivered proof failed exact campaign/wave/full-SHA validation"]
                          [(hash-ref proof 'reason #f)
                           =>
                           (lambda (r) (if (string? r) r "publication or sync pending"))]
                          [else "publication or sync pending"])))

;; Scan durable done-waves for the first one whose authenticated delivery
;; proof is NOT verified-delivered for the exact campaign/wave. A newly
;; verified wave is memoized in `verified` only during one loop iteration.
;; The caller clears it after any execution: checkout bytes can change
;; and its pending handoff ledger is reconciled to 'delivered
;; idempotently. Returns (cons wave reason) for the first undelivered
;; done-wave, or #f when every done wave carries exact delivered proof.
;; Verified DONE waves and their attempts are never touched.
(define (delivery-pending-wave base-dir plan-id waves delivery-reader verified)
  (for/or ([w (in-list waves)]
           #:when (eq? (campaign-wave-status w) 'done))
    (define idx (campaign-wave-index w))
    (and (not (hash-ref verified idx #f))
         (let* ([proof (delivery-reader base-dir plan-id idx)]
                [sha (delivered-proof-merge-sha proof plan-id idx)])
           (cond
             [sha
              (hash-set! verified idx sha)
              (reconcile-delivered-handoff! base-dir plan-id idx sha)
              #f]
             [else (cons w (undelivered-proof-reason proof))])))))

;; ============================================================
;; Merge-SHA provenance for the wave-advance gate
;; ============================================================

(define (verified-wave-merge-sha base-dir plan-id wave-index)
  (delivered-proof-merge-sha (delivery-readback base-dir plan-id wave-index) plan-id wave-index))

;; ============================================================
;; Durable handoff ledger (pending) + delivered reconcile
;; ============================================================

(define (delivery-handoff-path base-dir plan-id wave-index)
  (unless (and (string? plan-id)
               (regexp-match? #px"^[0-9a-f]{64}$" plan-id)
               (exact-nonnegative-integer? wave-index))
    (error 'delivery-handoff "invalid campaign/wave identity"))
  (define parts (list ".planning" "campaigns" plan-id (format "delivery-w~a.rktd" wave-index)))
  (for/fold ([parent base-dir]) ([part (in-list parts)])
    (define path (build-path parent part))
    (when (link-exists? path)
      (error 'delivery-handoff "refusing symlinked handoff path"))
    path))

(define (read-handoff path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (file-exists? path)
         (call-with-input-file path
                               (lambda (in)
                                 (parameterize ([read-accept-reader #f]
                                                [read-accept-lang #f]
                                                [read-accept-graph #f]
                                                [read-accept-compiled #f])
                                   (define datum (read in))
                                   (and (hash? datum) (eof-object? (read in)) datum)))))))

(define (persist-delivery-handoff! base-dir plan-id wave reason)
  (unless (regexp-match? #px"^[0-9a-f]{64}$" plan-id)
    (error 'delivery-handoff "invalid campaign identity"))
  (define idx (campaign-wave-index wave))
  (define path (delivery-handoff-path base-dir plan-id idx))
  (define data
    (hasheq
     'schema-version
     1
     'plan-id
     plan-id
     'wave
     idx
     'status
     'delivery-pending
     'attempt-count
     (campaign-wave-attempt-count wave)
     'delivery-branch
     (campaign-wave-delivery-branch wave)
     'delivery-head-sha
     (campaign-wave-delivery-head-sha wave)
     'binding
     (format "docs/reports/gsd-wave-evidence/~a-w~a.rktd" plan-id idx)
     'reason
     (redact-delivery-text reason)
     'next-steps
     (list "Open implementation PR from retained delivery branch"
           "Obtain independent review and required checks; protected squash merge"
           (string-append "python3 scripts/gsd-delivery.py prepare --campaign-root <campaign-root> "
                          "--repo <repo> --plan <plan-id> --wave <n> --pr <merged-PR> "
                          "--evidence <frozen trio source> --output <fresh directory>")
           "Review and validate the gate-red binding trio; protected binding PR"
           "Wait for binding main governance; explicitly fast-forward idle checkout"
           "Resume /go; verified implementation is not rerun")))
  (make-parent-directory* path)
  (define old (read-handoff path))
  (unless (equal? old data)
    (call-with-atomic-output-file path
                                  (lambda (out _)
                                    (write data out)
                                    (newline out))))
  path)

;; Idempotent pending→delivered reconcile of an EXISTING handoff ledger
;; entry, preserving its exact campaign/wave/attempt/branch/head metadata
;; and adding the verified merge SHA. A missing entry is never invented:
;; the authenticated delivered proof itself gates completion, and the
;; ledger only records handoffs that were actually issued — no bypass for
;; missing artifacts. Atomic, path-confined (64-hex campaign identity),
;; identical bytes on repeat calls.
(define (reconcile-delivered-handoff! base-dir plan-id wave-index merge-sha)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "delivery reconcile failed: ~a" (exn-message e))
                               #f)])
    (define path (delivery-handoff-path base-dir plan-id wave-index))
    (define old (read-handoff path))
    (and (hash? old)
         (equal? (hash-ref old 'plan-id #f) plan-id)
         (equal? (hash-ref old 'wave #f) wave-index)
         (string? merge-sha)
         (regexp-match? FULL-MERGE-SHA-RX merge-sha)
         (let ()
           (unless (and (eq? (hash-ref old 'status #f) 'delivered)
                        (equal? (hash-ref old 'merge-sha #f) merge-sha))
             (call-with-atomic-output-file
              path
              (lambda (out _)
                (write (hasheq 'schema-version
                               (hash-ref old 'schema-version 1)
                               'plan-id
                               plan-id
                               'wave
                               wave-index
                               'status
                               'delivered
                               'attempt-count
                               (hash-ref old 'attempt-count 0)
                               'delivery-branch
                               (hash-ref old 'delivery-branch "")
                               'delivery-head-sha
                               (hash-ref old 'delivery-head-sha "")
                               'merge-sha
                               merge-sha
                               'binding
                               (hash-ref old 'binding "")
                               'reason
                               "delivered: exact protected-main proof verified"
                               'next-steps
                               '("Resume /go; verified implementation is not rerun"))
                       out)
                (newline out))))
           path))))

;; Actionable operator hint naming the controller CLI. --campaign-root is
;; mandatory for prepare (frozen-snapshot provenance); gh credential
;; storage or a coordinator-held token is required for authenticated
;; readback.
(define (delivery-action-hint plan-id wave-index)
  (format
   (string-append "hint: python3 scripts/gsd-delivery.py status --repo <repo> --plan ~a --wave ~a; "
                  "after the protected merge run 'prepare' with --campaign-root <campaign-root> "
                  "(gh credential storage via 'gh auth login' or a coordinator GH_TOKEN/GITHUB_TOKEN "
                  "is required for authenticated readback)")
   plan-id
   wave-index))

;; ============================================================
;; Coordinator loop decision (v1.00.30) — the single place that
;; turns the bounded authenticated readback into the next loop action.
;; ============================================================

;; Human-readable blocked message; also persists the pending handoff
;; ledger entry (idempotent). `pending` is (cons wave reason).
(define (pending-delivery-message base-dir plan-id pending)
  (define w (car pending))
  (define handoff (persist-delivery-handoff! base-dir plan-id w (cdr pending)))
  (format (string-append "wave ~a verified; delivery pending: ~a. Coordinator handoff: ~a. "
                         "~a. Complete protected delivery and binding, then /go "
                         "(do not rerun implementation).")
          (campaign-wave-index w)
          (cdr pending)
          handoff
          (delivery-action-hint plan-id (campaign-wave-index w))))

;; Decide the next loop action AFTER the bounded authenticated readback
;; (up to 240 s controller call). Re-reads durable disk truth so an
;; external mutation that landed during the call wins; cancellation/fence
;; discipline is preserved. A FRESH campaign has no durable record until
;; the first wave persists it, so the very first iteration legitimately
;; falls back to `current`; a record that vanishes after waves completed
;; is still a hard error. Returns (values status live extra message):
;;   status ∈ 'wave-cancelled | 'wave-blocked | 'campaign-complete | 'run
;;   'wave-cancelled   → durable mutation/cancellation during readback
;;   'wave-blocked     → a done wave lacks authenticated delivery proof
;;   'campaign-complete→ every wave done/deferred and delivery-proven
;;   'run              → proceed with `live` and next index `extra`
(define (ready-for-run-checkpoint base-dir plan-id current completed delivery-reader verified)
  (hash-clear! verified)
  (define pending
    (and (not (campaign-record-cancellation current))
         (delivery-pending-wave base-dir
                                plan-id
                                (campaign-record-waves current)
                                delivery-reader
                                verified)))
  (define reloaded (load-campaign-record base-dir plan-id))
  (define live (or reloaded current))
  (define disappeared (and (not reloaded) (not (null? completed))))
  (define stale?
    (or disappeared
        (and reloaded (not (= (campaign-fence-token reloaded) (campaign-fence-token current))))
        (and reloaded
             (not (equal? (map campaign-wave-status (campaign-record-waves reloaded))
                          (map campaign-wave-status (campaign-record-waves current)))))))
  (define next-idx
    (and (not (or stale? pending (campaign-record-cancellation live)))
         (select-next-actionable-wave live)))
  (cond
    [stale?
     (values 'wave-cancelled
             live
             #f
             "campaign changed during delivery readback; stale result ignored, resume /go")]
    [(campaign-record-cancellation live)
     (values 'wave-cancelled live #f "campaign cancellation requested")]
    [pending
     (values 'wave-blocked live (car pending) (pending-delivery-message base-dir plan-id pending))]
    [(not next-idx) (values 'campaign-complete live #f "all waves done or deferred")]
    [else (values 'run live next-idx #f)]))

;; Memoized, ledger-reconciled predecessor-proof resolver derived from a
;; delivery-reader — the SAME authenticated source as the final full-loop
;; check, never a local regex fixture. `verified` is cleared once per loop
;; iteration by ready-for-run-checkpoint, so a later runner mutation forces
;; revalidation of publication AND synchronized bytes.
(define (delivered-predecessor-resolver base-dir plan-id verified delivery-reader)
  (lambda (b p w)
    (or (hash-ref verified w #f)
        (let ([sha (delivered-proof-merge-sha (delivery-reader b p w) p w)])
          (and sha
               (begin
                 (hash-set! verified w sha)
                 (reconcile-delivered-handoff! base-dir plan-id w sha)
                 sha))))))
