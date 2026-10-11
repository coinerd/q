#lang racket/base
;; STABILITY: internal
;;
;; extensions/gsd/review-dispatch.rkt — automatic independent review dispatch
;; (BUG_PLAN automatic-review slice B).
;;
;; The dispatch port between the delivery coordinator and a FRESH non-author
;; reviewer session. When the trusted controller reports a genuinely MISSING
;; implementation review trio, or a staged binding trio that is still
;; pending-review (post-merge finalization), the coordinator routes the typed
;; review request through the exported `current-gsd-auto-review-runner` hook.
;;
;; Laws enforced HERE (the trusted controller enforces the rest):
;;   * the hook is a PORT, not a default: with no runner installed (the
;;     parameter default #f) the dispatcher declines and the coordinator
;;     falls back to the unchanged validation-only controller action, so
;;     injected/manual fixtures keep working;
;;   * the runner never receives credentials: it is a plain procedure called
;;     with the typed request only. Every subprocess the dispatcher shells
;;     itself runs through the SAME credential boundary as the delivery
;;     controller (controller-environment); no reviewer-facing path ever
;;     sees a token, and no reviewer prose is ever executed as authority;
;;   * typed statuses stay separate: 'awaiting-review (missing info),
;;     'review-rejected (explicit head-bound REQUEST_CHANGES carrying the
;;     exact receipt head/branch/attempt/fence + findings), 'review-infra
;;     (transport/timeout/stale/malformed reviewer output). An
;;     infrastructure failure can never masquerade as a rejection or an
;;     approval, and only the trusted controller's strict gates can produce
;;     an approval outcome;
;;   * the receipt pin (head/branch/attempt-id/fence) captured before the
;;     dispatch must still hold after the reviewer ran — drift is a typed
;;     stale review-infra stop, never a rejection.
;;
;; The effect results are plain (kind . data) pairs — exactly the shape
;; delivery-coordinator.rkt's delivery-effect-result produces — so this
;; module never imports the coordinator (no dependency cycle, no runtime
;; imports in core).

(require racket/file
         racket/format
         racket/list
         racket/runtime-path
         json
         racket/path
         racket/string
         (only-in "delivery-handoff.rkt" controller-environment redact-delivery-text)
         (only-in "delivery-journal.rkt" load-delivery-journal)
         (only-in "../../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-exit-code
                  subprocess-result-timed-out?
                  subprocess-result-truncated?))

(provide current-gsd-auto-review-runner
         review-runner-installed?
         runner-result-kind
         call-fresh-reviewer
         review-dispatch-staging-path
         default-review-python
         auto-review-dispatch!)

;; ---------------------------------------------------------------------------
;; The exported port: fresh reviewer runner
;; ---------------------------------------------------------------------------
;;
;; current-gsd-auto-review-runner holds #f (default: no automatic review)
;; or a procedure in ONE of two accepted shapes:
;;
;;   typed request→result callback — (request hash?) -> result hash?
;;     Called ONCE per dispatch with the typed request (below). The
;;     callback MUST be backed by a FRESH reviewer session that was never
;;     exposed to the implementation author's context.
;;
;;   fresh-session factory — () -> (prompt string?) -> result hash?
;;     Called with no arguments to PRODUCE a fresh prompt→result reviewer;
;;     the returned procedure is then applied to the request's prompt.
;;
;; When both arities are accepted (case-lambda) the typed shape wins.
;;
;; request  — immutable hash, symbol keys: kind ("implementation-review" |
;;     "binding-review"), plan, wave, generation, head, branch, tree,
;;     attempt-id, attempt-fence, evidence/review/validation (repo-relative
;;     trio paths), staging (absolute staging dir), reviewer-output
;;     (absolute path), content-digest, required-checks, prompt,
;;     fresh-session-required (#t), non-author-review-required (#t).
;;
;; result — hash: kind ("approved" | "request-changes" | "review-failed"),
;;     reviewer (genuine session identity label), scope, report (genuine
;;     narrative), findings (non-empty string list; request-changes only),
;;     validation (gate facts hash; approved only), reason (review-failed
;;     only). Malformed output is a typed review-infra failure — NEVER a
;;     rejection and NEVER an approval.
(define current-gsd-auto-review-runner (make-parameter #f))

(define (review-runner-installed?)
  (procedure? (current-gsd-auto-review-runner)))

;; A reviewer identity that merely encodes a placeholder is never a genuine
;; fresh session (register F12, same law as the strict gate).
(define reviewer-sentinel?
  (let ([pattern
         #px"(?i:^(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx|n/a)[:.;!,?]*$)"])
    (lambda (value) (and (string? value) (regexp-match? pattern (string-trim value))))))

(define (genuine-text? value)
  (and (string? value) (positive? (string-length (string-trim value)))))

(define (genuine-findings? value)
  (and (list? value) (pair? value) (<= (length value) 50) (andmap genuine-text? value)))

;; Classify a runner result: 'approved | 'request-changes | 'review-failed |
;; 'invalid. Structural only — genuineness beyond shape (narrative length,
;; gate facts) is enforced by the trusted controller and the strict gate.
(define (runner-result-kind result)
  (cond
    [(not (hash? result)) 'invalid]
    [else
     (define kind (hash-ref result 'kind #f))
     (cond
       [(not (member kind '("approved" "request-changes" "review-failed"))) 'invalid]
       [(equal? kind "review-failed")
        (if (genuine-text? (hash-ref result 'reason #f)) 'review-failed 'invalid)]
       [(not (genuine-text? (hash-ref result 'reviewer #f))) 'invalid]
       [(reviewer-sentinel? (hash-ref result 'reviewer #f)) 'invalid]
       [(not (genuine-text? (hash-ref result 'scope #f))) 'invalid]
       [(not (genuine-text? (hash-ref result 'report #f))) 'invalid]
       [(equal? kind "request-changes")
        (if (genuine-findings? (hash-ref result 'findings #f)) 'request-changes 'invalid)]
       [else (if (hash? (hash-ref result 'validation #f)) 'approved 'invalid)])]))

;; Invoke the runner in its accepted shape. A runner that raises is a typed
;; reviewer-session failure, never a crash of the coordinator loop.
(define (call-fresh-reviewer runner request)
  (with-handlers ([exn:fail? (lambda (e)
                               (hasheq 'kind
                                       "review-failed"
                                       'reason
                                       (format "fresh reviewer session raised: ~a"
                                               (exn-message e))))])
    (cond
      [(procedure-arity-includes? runner 1) (runner request)]
      [(procedure-arity-includes? runner 0) ((runner) (hash-ref request 'prompt ""))]
      [else
       (hasheq 'kind
               "review-failed"
               'reason
               "review runner has an unusable arity (expected request->result or a "
               "prompt->result factory)")])))

;; ---------------------------------------------------------------------------
;; Staging + the trusted-controller subprocess seam
;; ---------------------------------------------------------------------------

;; The campaign-local, generation-scoped staging directory for an automatic
;; implementation-review dispatch. Deterministic; never reuses another
;; generation's staging (old generations are preserved verbatim). Binding
;; dispatches stage inside the deterministic binding-w<N>[-r<G>] family.
(define (review-dispatch-staging-path base-dir plan wave generation)
  (path->string
   (build-path base-dir ".planning" "campaigns" plan (format "review-w~a-g~a" wave generation))))

(define-runtime-path review-python-script "../../scripts/gsd-delivery.py")

;; Production trusted-controller seam: (args (listof string?)) -> jsexpr.
;; Shells scripts/gsd-delivery.py through the SAME credential boundary as
;; the delivery controller; a nonzero exit, timeout or truncation raises
;; with a redacted typed reason (the caller maps failures to review-infra).
;; The reviewer runner is NEVER invoked through this seam — credentials and
;; reviewer sessions stay on opposite sides of the boundary.
(define (default-review-python base-dir plan wave)
  (define repo
    (if (or (directory-exists? (build-path base-dir ".git"))
            (file-exists? (build-path base-dir ".git")))
        base-dir
        (build-path base-dir "q")))
  (lambda (args)
    (define result
      (run-subprocess "python3"
                      #:args (append (list (path->string review-python-script))
                                     args
                                     (list "--repo"
                                           (path->string (path->complete-path repo))
                                           "--plan"
                                           plan
                                           "--wave"
                                           (number->string wave)))
                      #:directory base-dir
                      #:environment (controller-environment)
                      #:timeout 240
                      #:process-group? #t))
    (cond
      [(or (subprocess-result-timed-out? result) (subprocess-result-truncated? result))
       (raise-argument-error
        'review-dispatch
        (redact-delivery-text "review controller timed out or produced truncated output")
        #f)]
      [(zero? (subprocess-result-exit-code result))
       (with-handlers ([exn:fail?
                        (lambda (_e)
                          (raise-argument-error
                           'review-dispatch
                           (redact-delivery-text "review controller returned malformed JSON output")
                           #f))])
         (string->jsexpr (subprocess-result-stdout result)))]
      [else
       ;; A typed controller refusal (exit 2 with a delivery-pending JSON
       ;; payload) is a BLOCKED stop with the preserved reason — never an
       ;; infrastructure failure, never a rejection. Anything else raises
       ;; with a redacted typed reason (mapped to review-infra upstream).
       (define refused
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (define data (string->jsexpr (subprocess-result-stdout result)))
           (and (hash? data) (equal? (hash-ref data 'status #f) "delivery-pending") data)))
       (cond
         [refused refused]
         [else
          (raise-argument-error
           'review-dispatch
           (redact-delivery-text (string-trim (format "review controller failed: ~a ~a"
                                                      (subprocess-result-stderr result)
                                                      (subprocess-result-stdout result))))
           #f)])])))

;; ---------------------------------------------------------------------------
;; The dispatcher
;; ---------------------------------------------------------------------------

(define (effect kind data)
  (cons kind data))

(define (infra-stop reason)
  (effect 'review-infra
          (hasheq 'stage 'auto-review 'reason (format "review infrastructure failure: ~a" reason))))

(define (blocked-stop reason)
  (effect 'blocked (hasheq 'stage 'auto-review 'reason reason)))

(define (controller-status data)
  (and (hash? data) (hash-ref data 'status #f)))

(define (string-case value actions default)
  (cond
    [(assoc value actions)
     =>
     (lambda (pair) ((cdr pair)))]
    [else (default)]))

(define (run-phase run-python phase-args)
  (with-handlers ([exn:fail? (lambda (e) (hasheq 'status "review-infra" 'reason (exn-message e)))])
    (run-python phase-args)))

;; The receipt pin law: the exact identity the dispatch was prepared for
;; must still be the journal's live receipt when the reviewer's result is
;; accepted — anything else is a stale dispatch.
(define (receipt-pin journal)
  (define receipt (and journal (hash-ref journal 'receipt #f)))
  (and (hash? receipt)
       (list (hash-ref receipt 'head #f)
             (hash-ref receipt 'branch #f)
             (hash-ref receipt 'attempt-id #f)
             (hash-ref receipt 'attempt-fence #f))))

(define (pin-holds? base-dir plan wave pin)
  (equal? (receipt-pin (load-delivery-journal base-dir plan wave)) pin))

;; Normalize the trusted controller's typed request: the staging and
;; reviewer-output paths must be present (the controller is trusted, so a
;; malformed request is typed infrastructure, never a silent fallback).
(define (normalize-request data staging stage)
  (define request (and (hash? data) (hash-ref data 'request #f)))
  (cond
    [(not (hash? request)) (hasheq 'error "controller returned no typed review request")]
    [else
     (define staged (hash-ref request 'staging #f))
     (define output
       (hash-ref request
                 'reviewer-output
                 (and (string? staged) (path->string (build-path staged "reviewer-output.json")))))
     (cond
       [(not (and (string? staged) (string? output) (string? (hash-ref request 'prompt #f))))
        (hasheq 'error "controller review request is missing staging/output/prompt paths")]
       [else (hash-set* request 'reviewer-output output 'kind (hash-ref request 'kind stage))])]))

(define (auto-review-dispatch! base-dir
                               plan
                               wave
                               stage
                               #:generation [generation #f]
                               #:runner [runner (current-gsd-auto-review-runner)]
                               #:run-python [run-python #f]
                               ;; The binding stage finalizes the DETERMINISTIC
                               ;; binding staging family instead of the
                               ;; review-w<N>-g<G> implementation staging.
                               #:staging-root [staging-root #f])
  (cond
    [(not (procedure? runner)) #f]
    [else
     (define run (or run-python (default-review-python base-dir plan wave)))
     (define journal
       (with-handlers ([exn:fail? (lambda (_e) #f)])
         (load-delivery-journal base-dir plan wave)))
     (define pin (receipt-pin journal))
     (cond
       [(not pin) (blocked-stop "review dispatch requires a durable delivery receipt")]
       [(not (exact-nonnegative-integer? generation))
        (blocked-stop "review dispatch requires the authoritative binding generation")]
       [else
        (define staging
          (or staging-root (review-dispatch-staging-path base-dir plan wave generation)))
        (define prepared
          (run-phase run
                     (list "auto-review"
                           "--phase"
                           "prepare"
                           "--stage"
                           stage
                           "--expected-head"
                           (first pin)
                           "--expected-branch"
                           (second pin)
                           "--campaign-root"
                           (path->string (path->complete-path base-dir))
                           "--generation"
                           (number->string generation)
                           "--staging-root"
                           staging)))
        (string-case
         (controller-status prepared)
         (list
          (cons "reviewed" (lambda () (effect 'ok prepared)))
          (cons
           "awaiting-review"
           (lambda ()
             (dispatch-reviewer run base-dir plan wave stage generation staging pin runner prepared)))
          (cons "review-rejected" (lambda () (rejected-effect base-dir plan wave stage pin prepared)))
          (cons "review-infra"
                (lambda ()
                  (infra-stop (hash-ref prepared 'reason "reviewer infrastructure failed")))))
         (lambda ()
           (blocked-stop (hash-ref prepared 'reason "review controller refused the dispatch"))))])]))

;; Phase 2/3: run the fresh reviewer, persist its typed output for the
;; trusted controller, then let the controller finalize through the strict
;; gates with the receipt pin re-verified on both sides of the dispatch.
(define (dispatch-reviewer run base-dir plan wave stage generation staging pin runner prepared)
  (define request (normalize-request prepared staging stage))
  (cond
    [(hash-ref request 'error #f)
     =>
     (lambda (message) (infra-stop message))]
    [else
     (define result (call-fresh-reviewer runner request))
     (case (runner-result-kind result)
       [(invalid)
        (define rendered (format "~s" result))
        (infra-stop (format "malformed reviewer output: ~a"
                            (substring rendered 0 (min 200 (string-length rendered)))))]
       [(review-failed) (infra-stop (hash-ref result 'reason "the reviewer session failed"))]
       [else
        ;; Persist the reviewer's TYPED result where the trusted controller
        ;; reads it. The reviewer never writes here itself and never holds
        ;; credentials; approval still only exists through the controller's
        ;; strict gates.
        (define output-path (hash-ref request 'reviewer-output))
        (make-parent-directory* output-path)
        (call-with-output-file output-path
                               (lambda (out)
                                 (write-json result out)
                                 (newline))
                               #:exists 'truncate/replace)
        (finalize-dispatch run
                           base-dir
                           plan
                           wave
                           stage
                           generation
                           pin
                           (hash-ref request 'staging)
                           output-path)])]))

;; Same pin validation for freshly published and recovered rejections.
(define (rejected-effect base-dir plan wave stage pin data)
  (define bound
    (and (hash? data)
         (equal? (hash-ref data 'head #f) (first pin))
         (equal? (hash-ref data 'branch #f) (second pin))
         (equal? (hash-ref data 'attempt-id #f) (third pin))
         (equal? (hash-ref data 'attempt-fence #f) (fourth pin))
         (genuine-findings? (hash-ref data 'findings #f))))
  (cond
    [(not bound)
     (infra-stop "the controller rejection is not bound to the pinned receipt head/attempt identity")]
    [(not (pin-holds? base-dir plan wave pin))
     (infra-stop "stale review dispatch: the durable receipt changed during review")]
    [else (effect 'review-rejected (hash-set* data 'stage stage))]))

(define (finalize-dispatch run base-dir plan wave stage generation pin staging output-path)
  (define finalized
    (run-phase run
               (list "auto-review"
                     "--phase"
                     "finalize"
                     "--stage"
                     stage
                     "--expected-head"
                     (first pin)
                     "--expected-branch"
                     (second pin)
                     "--campaign-root"
                     (path->string (path->complete-path base-dir))
                     "--generation"
                     (number->string generation)
                     "--staging-root"
                     staging
                     "--reviewer-output"
                     output-path)))
  (string-case
   (controller-status finalized)
   (list
    (cons "reviewed"
          (lambda ()
            (if (pin-holds? base-dir plan wave pin)
                (effect 'ok finalized)
                (infra-stop "stale review dispatch: the durable receipt changed during review"))))
    (cons "review-rejected" (lambda () (rejected-effect base-dir plan wave stage pin finalized)))
    (cons "review-infra"
          (lambda () (infra-stop (hash-ref finalized 'reason "reviewer infrastructure failed")))))
   (lambda ()
     (blocked-stop (hash-ref finalized 'reason "review controller refused the finalization")))))
