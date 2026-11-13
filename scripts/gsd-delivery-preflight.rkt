#lang racket/base

;; Phase B slice S1 — native port of the READ-ONLY `preflight` action of
;; scripts/gsd-delivery.py (Register F5, v1.00.31 W3).
;;
;; Scope law: this module replaces the Python `preflight` action ONLY. Every
;; other gsd-delivery.py action (status/prepare/sync/merge/resolve-pr/review/
;; open-pr/pr-ci/binding-*/governance/publish-head/auto-review) dispatches to
;; Python through delivery-coordinator.rkt unless another native slice owns it
;; (S2 owns `sync` via scripts/gsd-delivery-sync.rkt).
;;
;; The bounded-git runner, the typed failure classification, the budget and the
;; exit-2 CLI surface are the SHARED seam in scripts/gsd-delivery-git.rkt; this
;; module keeps only the preflight-specific ladder.
;;
;; Parity with the Python action is the contract:
;;   * success -> hash {status "ready", plan-id, wave, branch, head,
;;     receipt-head}; the CLI prints it as JSON and exits 0;
;;   * refusal -> exn:fail:delivery-preflight carrying the typed reason
;;     in-process; the CLI prints
;;     {"status":"delivery-pending","reason":<typed reason>} on STDOUT and
;;     exits 2 (the exact shape the coordinator's typed-stop parser expects);
;;   * read-only: it fetches remote-tracking refs only, never publishes,
;;     never switches branches, never inspects checkout cleanliness;
;;   * ONE total process budget strictly under the 240 s runtime caller; every
;;     command is clamped to min(45, remaining) and fails closed;
;;   * credential isolation through controller-environment (GH_TOKEN/
;;     GITHUB_TOKEN re-added to an otherwise scrubbed environment) and
;;     process-group cleanup through sandbox run-subprocess
;;     #:process-group? #t;
;;   * raw git/API stderr is NEVER surfaced — only the typed classifier
;;     summary, which never carries credential material;
;;   * every refusal names its CAUSE (budget-exhausted, delivery-timeout,
;;     command-not-found, process-group-unavailable, group-cleanup-unconfirmed,
;;     command-unavailable, output-truncated, remote-ref-missing,
;;     branch-not-published, non-fast-forward, unknown-ref). A generic "git
;;     command failed" can therefore never hide an infrastructure fault, and
;;     no diagnostic ever carries child output.
;;
;; Known parity limitations (deliberate; not closed by this slice):
;;   * origin identity is validated for GitHub SHAPE only (https://github.com/
;;     <owner>/<repo> or git@github.com:<owner>/<repo>); no GitHub request
;;     confirms the slug exists or that the token can see it;
;;   * the required-check policy at origin/main is read and shape-checked, but
;;     its CONTENT is never compared with the receipt's policy snapshot (the
;;     remaining Python ladder stages own that comparison).

(require racket/file
         racket/list
         racket/path
         racket/string
         "../extensions/gsd/delivery-handoff.rkt"
         "../sandbox/subprocess.rkt"
         (only-in "gsd-binding-data.rkt" read-any-datum)
         (prefix-in git: "gsd-delivery-git.rkt"))

(provide delivery-preflight!
         delivery-preflight-json
         delivery-pending-json
         delivery-preflight-main
         ;; Pure typed classifier for sandbox exec/cleanup failures (exported
         ;; for direct tests of branches a CI host cannot trigger).
         classify-exec-failure
         ;; Historical S1 name for the shared typed refusal (now defined once
         ;; in scripts/gsd-delivery-git.rkt and shared with the native sync).
         exn:fail:delivery-preflight
         exn:fail:delivery-preflight?)

(define exn:fail:delivery-preflight git:exn:fail:delivery-action)
(define exn:fail:delivery-preflight? git:exn:fail:delivery-action?)
(define classify-exec-failure git:classify-exec-failure)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

(define policy-path "scripts/required-pr-checks.policy")

;; EXCLUDED_PREFIXES in the Python controller: later wave-branch commits may
;; touch ONLY these evidence directories after the verified receipt head.
(define excluded-prefixes
  (list "docs/reports/gsd-wave-evidence/"
        "docs/reports/gsd-wave-reviews/"
        "docs/reports/gsd-wave-validation/"))

(define full-sha-rx #px"^[0-9a-f]{40}$")
(define plan-id-rx #px"^[0-9a-f]{64}$")
;; repository(): the CONFIGURED origin URL must identify a GitHub repository.
;; url.<x>.insteadOf rewrites are transport conveniences and never redefine
;; repository identity, so this is read with `git config --get`.
(define github-origin-rx
  #px"^(?:https://github\\.com/|git@github\\.com:)[\\w.-]+/[\\w.-]+?(?:\\.git)?$")

(define (refuse! message)
  (git:refuse! message))

(define (full-sha? value)
  (and (string? value) (regexp-match? full-sha-rx value)))

(define (hash-prefix value)
  (substring value 0 (min 12 (string-length value))))

;; ============================================================
;; preflight
;; ============================================================

(define (validate-binding-identity! plan wave)
  (unless (and (string? plan) (regexp-match? plan-id-rx plan))
    (refuse! "invalid campaign id"))
  (unless (and (exact-integer? wave) (>= wave 0))
    (refuse! "invalid wave index")))

;; policy_names(): the expected required-check name snapshot at a ref, read
;; from the TRUSTED local object through the trusted datum reader.
(define (policy-names! git ref)
  (define blob
    (with-handlers ([exn:fail:delivery-preflight? (lambda (_e) #f)])
      (git (list "show" (string-append ref ":" policy-path)))))
  (unless blob
    (refuse! (format "required-check policy is absent at ~a" ref)))
  (define temp (make-temporary-file "q-delivery-policy-~a"))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file temp
                                         #:exists 'truncate
                                         (lambda (out) (write-bytes (string->bytes/utf-8 blob) out)))
                  (define names
                    (with-handlers ([exn:fail? (lambda (_e) #f)])
                      (read-any-datum temp)))
                  (unless (and (list? names)
                               (pair? names)
                               (andmap (lambda (item)
                                         (and (string? item) (not (string=? (string-trim item) ""))))
                                       names)
                               (= (length names) (length (remove-duplicates names))))
                    (refuse! (format "malformed required-check policy at ~a" ref)))
                  names)
                (lambda ()
                  (with-handlers ([exn:fail? void])
                    (delete-file temp)))))

;; require_receipt_tip(): only the exact receipt head, or a descendant whose
;; tail touches excluded evidence paths only.
(define (require-receipt-tip! git expected-head tip action)
  (define common (string-trim (git (list "merge-base" expected-head tip))))
  (unless (equal? common expected-head)
    (refuse!
     (format
      "receipt head ~a is not an ancestor of tip ~a; the wave branch may have been rewritten; re-verify before ~a"
      (hash-prefix expected-head)
      (hash-prefix tip)
      action)))
  (define drifted
    (git (list "diff" "--name-only" "--no-renames" (string-append expected-head ".." tip) "--")))
  (define foreign
    (for/list ([line (in-list (string-split drifted "\n"))]
               #:when (let ([path (string-trim line)])
                        (and (not (string=? path ""))
                             (not (for/or ([prefix (in-list excluded-prefixes)])
                                    (string-prefix? path prefix))))))
      (string-trim line)))
  (unless (null? foreign)
    (refuse!
     (format
      "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: ~a; re-verify before ~a"
      (hash-prefix expected-head)
      (string-join (take foreign (min 3 (length foreign))) ", ")
      action))))

;; The read-only handoff-seam preflight. Returns the success hash or raises
;; exn:fail:delivery-preflight with the typed reason.
(define (delivery-preflight! repo plan wave expected-head expected-branch)
  (unless (and (string? expected-branch) (not (string=? (string-trim expected-branch) "")))
    (refuse! "preflight requires --expected-branch"))
  (unless (and (string? expected-head) (full-sha? expected-head))
    (refuse! "preflight requires --expected-head (the durable receipt head)"))
  (unless (path-string? repo)
    (refuse! "preflight requires --repo"))
  (validate-binding-identity! plan wave)
  (define git (git:make-repo-git-command repo))
  ;; 1. Repository identity: the CONFIGURED origin URL (never rewritten).
  (define origin (string-trim (git (list "config" "--get" "remote.origin.url"))))
  (unless (regexp-match? github-origin-rx origin)
    (refuse! "origin must identify a GitHub repository (no arbitrary API host)"))
  ;; 2. Refresh origin/main, exactly like refresh().
  (git (list "fetch" "--no-tags" "origin" "refs/heads/main:refs/remotes/origin/main"))
  ;; 3. The verified branch must be published at origin (non-forced fetch, so
  ;; a rewritten branch is refused here as non-fast-forward).
  (define remote-ref (string-append "refs/heads/" expected-branch))
  (define tracking-ref (string-append "refs/remotes/origin/" expected-branch))
  (with-handlers
      ([exn:fail:delivery-preflight?
        (lambda (error)
          (refuse!
           (format
            "branch-not-published: branch ~a is not published on origin; push the verified head ~a... first (underlying: ~a)"
            expected-branch
            (hash-prefix expected-head)
            (exn-message error))))])
    (git (list "fetch" "--no-tags" "origin" (string-append remote-ref ":" tracking-ref))))
  (define tip (string-trim (git (list "rev-parse" tracking-ref))))
  (unless (full-sha? tip)
    (refuse! (format "malformed published branch tip for ~a" expected-branch)))
  (require-receipt-tip! git expected-head tip "preflight")
  ;; 4. The required-check policy object must exist at origin/main.
  (define main (string-trim (git (list "rev-parse" "refs/remotes/origin/main"))))
  (unless (full-sha? main)
    (refuse! "malformed origin/main head"))
  (policy-names! git main)
  (hasheq 'status
          "ready"
          'plan-id
          plan
          'wave
          wave
          'branch
          expected-branch
          'head
          tip
          'receipt-head
          expected-head))

;; ============================================================
;; JSON / CLI surface (same shapes as the Python action)
;; ============================================================

(define (delivery-preflight-json result)
  (git:jsexpr-string result))

(define (delivery-pending-json reason)
  (git:delivery-pending-json reason))

(define (read-flag-value argv)
  (git:read-flag-value argv))

;; Exact Python main() contract for this action: argparse requires an action
;; and --repo/--plan/--wave; preflight then requires both identity flags.
;; Success -> JSON on stdout, exit 0. Refusal -> JSON on stdout, exit 2.
(define (delivery-preflight-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (define action (and (pair? argv) (car argv)))
    (define flags
      (read-flag-value (if (pair? argv)
                           (cdr argv)
                           '())))
    (define (flag name)
      (hash-ref flags name #f))
    (unless (equal? action "preflight")
      (refuse!
       (format "unsupported action ~a; this module implements the read-only preflight action only"
               (or action ""))))
    (unless (flag "--repo")
      (refuse! "preflight requires --repo"))
    (unless (and (flag "--expected-head") (flag "--expected-branch"))
      (refuse! "preflight requires --expected-head and --expected-branch"))
    (define wave (and (flag "--wave") (string->number (flag "--wave"))))
    (printf "~a\n"
            (delivery-preflight-json (delivery-preflight! (flag "--repo")
                                                          (flag "--plan")
                                                          wave
                                                          (flag "--expected-head")
                                                          (flag "--expected-branch"))))
    (exit 0)))

(module+ main
  (delivery-preflight-main (vector->list (current-command-line-arguments))))
