#lang racket/base

;; Phase B shared seam: the bounded-git runner, the typed failure
;; classification and the controller-environment handling that the native
;; gsd-delivery.py ports share. Extracted verbatim from the S1 `preflight` port
;; (scripts/gsd-delivery-preflight.rkt) so the S2 `sync` port
;; (scripts/gsd-delivery-sync.rkt) reuses exactly the proven infrastructure
;; instead of re-deriving it. Both actions keep the SAME contract:
;;
;;   * ONE total process budget (GSD_DELIVERY_BUDGET_SEC clamped to [10,235],
;;     default 200) strictly under the 240 s runtime caller; every command is
;;     clamped to min(45, remaining) and fails closed;
;;   * credential isolation through controller-environment (GH_TOKEN/
;;     GITHUB_TOKEN re-added to an otherwise scrubbed environment) and
;;     process-group cleanup through sandbox run-subprocess
;;     #:process-group? #t;
;;   * raw git stderr is NEVER surfaced — only the typed classifier summary,
;;     which never carries credential material;
;;   * every refusal names its CAUSE: budget-exhausted, delivery-timeout,
;;     command-not-found, process-group-unavailable, group-cleanup-unconfirmed,
;;     command-unavailable, output-truncated, remote-ref-missing,
;;     branch-not-published, non-fast-forward, unknown-ref. A generic "git
;;     command failed" can therefore never hide an infrastructure fault, and
;;     no diagnostic ever carries child output;
;;   * in-process refusal is an exception carrying the typed reason; the CLI
;;     prints {"status":"delivery-pending","reason":<typed reason>} on STDOUT
;;     and exits 2 — the exact shape delivery-coordinator.rkt's typed-stop
;;     parser expects.
;;
;; Budget honesty: the ENFORCED guard is the INTERNAL budget below. The
;; runtime-caller budget (240 s) is only the ceiling that budget must stay
;; under — it is NOT an outer kill switch here.
;;
;; Cleanup honesty: process-group cleanup is group-wide only when `setsid` is
;; available (sandbox run-subprocess then launches a group owner under setsid)
;; and its confirmation proof (Linux /proc) is readable. A MISSING setsid is
;; NEVER an error: the SAME run-subprocess call SILENTLY degrades to
;; DIRECT-CHILD cleanup (it signals only the direct child, TERM then KILL), so
;; a descendant that creates its own session escapes and no typed reason is
;; produced. Only a missing Linux /proc raises (the grouped launch refuses as
;; process-group-unavailable), and a cleanup whose confirmation cannot be
;; proven raises group-cleanup-unconfirmed. This module adds no availability
;; guard of its own; it reports whichever of those the sandbox raised instead
;; of claiming containment.
;; Git and its configured helpers are trusted: a cleanup process group is not
;; a sandbox.
;;
;; Infrastructure vs repository state: callers that translate a shared
;; refusal into a repository-state reason (the native sync maps a failed
;; `symbolic-ref` to "detached HEAD") MUST re-raise infrastructure refusals
;; unchanged via `infrastructure-refusal?`; only a genuine command failure may
;; become a repository-state reason.

(require json
         racket/path
         racket/string
         "../extensions/gsd/delivery-handoff.rkt"
         "../sandbox/subprocess.rkt")

(provide runtime-caller-budget
         default-process-budget
         command-timeout-budget
         ;; typed refusal
         exn:fail:delivery-action
         exn:fail:delivery-action?
         refuse!
         infrastructure-refusal?
         ;; budget
         budget-seconds
         whole-seconds
         ;; classifiers (pure; exported for direct tests of branches a CI host
         ;; cannot trigger)
         classify-failure
         classify-exec-failure
         ;; bounded subprocess seams
         make-bounded-runner
         make-repo-git-command
         ;; CLI surface (same JSON shapes as the Python actions)
         jsexpr-string
         delivery-pending-json
         read-flag-value)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

;; delivery-handoff run-subprocess timeout (seconds) in the Python action's
;; caller. It is a CEILING for the internal budget, not an enforced outer kill
;; switch.
(define runtime-caller-budget 240.0)
(define default-process-budget 200.0)
(define command-timeout-budget 45.0)

;; Failure classifiers (W3 register F8). Raw stderr may contain credential
;; URLs or remote text, so only these typed summaries are ever exposed.
(define auth-error-rx
  #rx"(?i:gh auth|GITHUB_TOKEN|bad credentials|authentication required|HTTP 40[13])")
(define remote-ref-missing-rx #rx"(?i:couldn't find remote ref|does not (?:appear to|exist))")
(define non-fast-forward-rx #rx"(?i:non-fast-forward|\\[rejected\\]|fetch first|denied.*push)")
(define unknown-ref-rx #rx"(?i:unknown revision|bad revision|ambiguous argument)")
(define refspec-rx #rx"refs/heads/[A-Za-z0-9._/=-]+")
;; sandbox run-subprocess reports ITS OWN exec/cleanup failures as an
;; EXIT-CODE-ERROR result whose stderr is a synthesized diagnostic — never
;; child output. These are the only shapes reaching classify-exec-failure.
(define cleanup-unconfirmed-rx #rx"(?i:cleanup unconfirmed|watcher identity unconfirmed)")
(define process-group-unavailable-rx #rx"requires Linux /proc")
(define command-missing-rx #rx"(?i:command not found|no such file or directory)")

;; ============================================================
;; Typed refusal
;; ============================================================

;; A refusal is the in-process equivalent of the Python action's
;; {"status":"delivery-pending","reason":...} + exit 2: the message IS the
;; typed reason and is surfaced verbatim by the coordinator.
(struct exn:fail:delivery-action exn:fail () #:transparent)

(define (refuse! message)
  (raise (exn:fail:delivery-action message (current-continuation-marks))))

;; The INFRASTRUCTURE causes: host/runner faults that say nothing about the
;; repository under inspection. Every typed reason below begins with its cause
;; token followed by ":", so the cause is recoverable from the message the
;; coordinator already surfaces. A caller must never re-label one of these as
;; repository state (e.g. "detached HEAD").
(define infrastructure-causes
  '("budget-exhausted" "delivery-timeout"
                       "command-not-found"
                       "command-unavailable"
                       "process-group-unavailable"
                       "group-cleanup-unconfirmed"
                       "output-truncated"))

(define (infrastructure-refusal? message)
  (and (string? message)
       (for/or ([cause (in-list infrastructure-causes)])
         (string-prefix? message (string-append cause ":")))))

;; ============================================================
;; Budget
;; ============================================================

;; Python: float(GSD_DELIVERY_BUDGET_SEC) or 200.0; kept only within
;; [10, 240-5]; any parse failure or out-of-range value falls back to 200.
(define (budget-seconds)
  (define raw (getenv "GSD_DELIVERY_BUDGET_SEC"))
  (define parsed (and raw (string->number (string-trim raw))))
  (if (and (real? parsed) (>= parsed 10.0) (<= parsed (- runtime-caller-budget 5.0)))
      (exact->inexact parsed)
      default-process-budget))

;; Python's "%.0f" (round-half-even).
(define (whole-seconds value)
  (inexact->exact (round value)))

;; ============================================================
;; Typed failure classification
;; ============================================================

(define (classify-failure name exit-code stderr args)
  (define text (if (string? stderr) stderr ""))
  (cond
    [(regexp-match? auth-error-rx text)
     (string-append "GitHub authentication failed (check: gh auth status); "
                    "credentials are never logged or exposed")]
    [(regexp-match? remote-ref-missing-rx text)
     (define match
       (or (regexp-match refspec-rx text)
           (for/first ([arg (in-list args)]
                       #:when (string? arg))
             (regexp-match refspec-rx arg))))
     (if match
         (format
          "remote-ref-missing: ~a is not published on origin; push the verified head first (~a, exit ~a)"
          (car match)
          name
          exit-code)
         (format
          "remote-ref-missing: the requested ref is not published on origin; push the verified head first (~a, exit ~a)"
          name
          exit-code))]
    [(regexp-match? non-fast-forward-rx text)
     (format
      "non-fast-forward: the remote ref diverged; fetch, reanchor the verified head and retry (~a, exit ~a)"
      name
      exit-code)]
    [(regexp-match? unknown-ref-rx text)
     (format
      "unknown-ref: ~a referenced an unknown revision; inspect the repository refs locally before retrying (~a, exit ~a)"
      name
      name
      exit-code)]
    [else
     (format
      "delivery command failed (~a, exit ~a); inspect authentication, repository refs and required artifacts locally"
      name
      exit-code)]))

;; Typed cause for a sandbox exec/cleanup failure. The diagnostic is READ only
;; to pick the cause; it is never surfaced (it can echo child output).
;; Exported so the unreachable-in-CI cleanup branches stay directly testable.
(define (classify-exec-failure name diagnostic)
  (define text (if (string? diagnostic) diagnostic ""))
  (cond
    [(regexp-match? cleanup-unconfirmed-rx text)
     (format
      "group-cleanup-unconfirmed: ~a exited without confirmed process-group cleanup; descendants may survive - inspect the host first"
      name)]
    [(regexp-match? process-group-unavailable-rx text)
     (format
      "process-group-unavailable: ~a has no confirmed group cleanup; setsid and Linux /proc are required, direct-child cleanup only"
      name)]
    [(regexp-match? command-missing-rx text)
     (format
      "command-not-found: the ~a executable is not on PATH; nothing was inspected and no credential was used"
      name)]
    [else
     (format
      "command-unavailable: ~a could not be executed (sandbox exec boundary); no repository state was inspected"
      name)]))

;; ============================================================
;; Bounded subprocess seam
;; ============================================================

;; LAW: (budget start-ms) -> runner. Each command is clamped to the remaining
;; budget; the process as a whole can never exceed it. Every diagnostic is a
;; typed summary naming its CAUSE — subprocess output is never echoed.
(define (make-bounded-runner budget start-ms)
  (define (remaining-seconds)
    (- budget (/ (- (current-inexact-monotonic-milliseconds) start-ms) 1000.0)))
  (define (bounded-timeout requested)
    (define left (remaining-seconds))
    (when (<= left 1.0)
      (refuse!
       (format
        "budget-exhausted: the delivery process budget (~as) is spent; rerun the action separately"
        (whole-seconds budget))))
    (min (exact->inexact requested) (max 0.5 (- left 0.5))))
  (lambda (command args requested-timeout)
    (define name (path->string (file-name-from-path command)))
    (define bounded (bounded-timeout requested-timeout))
    (define result
      (with-handlers ([exn:fail? (lambda (error)
                                   (refuse! (classify-exec-failure name (exn-message error))))])
        (run-subprocess command
                        #:args args
                        #:environment (controller-environment)
                        #:timeout bounded
                        #:process-group? #t)))
    (cond
      [(subprocess-result-timed-out? result)
       (refuse!
        (format
         "delivery-timeout: ~a did not finish within ~as (internal budget ~as) and was killed; nothing was published"
         name
         (whole-seconds bounded)
         (whole-seconds budget)))]
      [(= (subprocess-result-exit-code result) EXIT-CODE-ERROR)
       (refuse! (classify-exec-failure name (subprocess-result-stderr result)))]
      [(not (zero? (subprocess-result-exit-code result)))
       (refuse! (classify-failure name
                                  (subprocess-result-exit-code result)
                                  (subprocess-result-stderr result)
                                  args))]
      [(subprocess-result-truncated? result)
       (refuse!
        (format "output-truncated: ~a output exceeded the capture limit; rerun the action separately"
                name))]
      [else (subprocess-result-stdout result)])))

;; The budgeted `git -C <repo> …` command every native action uses. The budget
;; starts when the command is created, so one action costs one total budget.
(define (make-repo-git-command repo)
  (define budget (budget-seconds))
  (define start-ms (current-inexact-monotonic-milliseconds))
  (define run (make-bounded-runner budget start-ms))
  (define repo-string
    (if (path? repo)
        (path->string repo)
        repo))
  (lambda (args) (run "git" (append (list "-C" repo-string) args) command-timeout-budget)))

;; ============================================================
;; JSON / CLI surface (same shapes as the Python actions)
;; ============================================================

(define (jsexpr-string value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

(define (delivery-pending-json reason)
  (jsexpr-string (hasheq 'status "delivery-pending" 'reason reason)))

(define (read-flag-value argv)
  ;; argv: ("--flag" "value" ...) -> hash of flag -> value; a flag without a
  ;; value or a non-flag token is a typed refusal (never a guess).
  (let loop ([rest argv]
             [acc (hash)])
    (cond
      [(null? rest) acc]
      [(and (pair? (cdr rest)) (string-prefix? (car rest) "--"))
       (loop (cddr rest) (hash-set acc (car rest) (cadr rest)))]
      [else (refuse! (format "unsupported argument: ~a" (car rest)))])))
