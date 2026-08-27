#lang racket/base

;; stall-policy.rkt — GSD stall-watchdog policy seam (BUG-0042, v1.00.22 W7).
;;
;; Extracted VERBATIM from go-orchestrator.rkt (behavior-preserving
;; decomposition): the gsd-stall-exn protocol, soft/hard-limit message
;; constructors, the parameterized stall steerer hook, watchdog wiring for
;; wave runners, and the stall / no-change classification predicates used
;; by error routing. go-orchestrator re-provides these names for
;; compatibility with existing importers; new code should import this
;; module directly.
;; Also hosts the tiny pure campaign-record selector find-wave (used by
;; the notification-kind classifier and watchdog wiring).

(require racket/string
         (only-in "campaign-state.rkt"
                  campaign-plan-id
                  campaign-wave-title
                  campaign-record-waves
                  campaign-wave-index)
         (only-in "wave-docs.rkt" wave-slug read-wave-doc)
         (only-in "prompts.rkt" executor-reanchor-prompt)
         (only-in "wave-executor.rkt" stall-watchdog-observe! stall-watchdog-snapshot)
         (only-in "wave-runner-port.rkt" wave-execution-outcome)
         (only-in "../../runtime/settings-query.rkt"
                  gsd-stall-soft-limit
                  gsd-stall-hard-limit
                  gsd-stall-window
                  gsd-stall-backstop
                  STALL-SOFT-LIMIT-DEFAULT
                  STALL-HARD-LIMIT-DEFAULT
                  STALL-REPETITION-WINDOW-DEFAULT
                  STALL-BACKSTOP-LIMIT-DEFAULT)
         (only-in "../../runtime/iteration/step-executor.rkt" current-post-tool-result-hook)
         (only-in "../../agent/state.rkt" current-empty-response-nudge))

(provide gsd-stall-exn
         gsd-stall-exn?
         make-gsd-stall-exn
         stall-steering-message
         stall-hard-failure-message
         current-gsd-stall-steerer
         wave-file-line-rx
         wave-doc-target-files
         wrap-run-one-with-stall-watchdog
         stall-cause-prefix
         stall-cause-message?
         no-change-rejection-prefix
         no-change-rejection?
         no-change-target-files
         wave-failure-notification-kind
         find-wave
         resolve-effective-stall-thresholds)

(define (find-wave rec wave-idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) wave-idx))
    w))

;; Hard-limit exception. An exn:fail subtype so the production runner
;; boundary's existing exn:fail? handler (execute-campaign-request!)
;; converts it into (wave-execution-outcome 'failed <stall message>) with
;; the explicit cause intact; run-campaign-wave installs its own
;; gsd-stall-exn? guard so plain/test runners see the same conversion.
(struct gsd-stall-exn exn:fail () #:transparent)

(define (make-gsd-stall-exn message)
  (gsd-stall-exn message (current-continuation-marks)))

;; Soft-limit steering message. Reuses W2's re-anchor constructor
;; (executor-reanchor-prompt) so the executor role context travels with
;; the steering — the steered session cannot reinterpret itself as an
;; interactive assistant. Pure constructor; no I/O.
(define (stall-steering-message calls-since-mutation wave-id campaign-id task-line target-files)
  (string-append
   (executor-reanchor-prompt wave-id
                             campaign-id
                             task-line
                             "(no edit has been made yet — this session has only read/explored)")
   "\n\n"
   "[MUTATION-STALL WATCHDOG — SOFT LIMIT REACHED]\n"
   (format "You have made ~a calls without any edit. Wave targets: ~a. "
           calls-since-mutation
           (if (null? target-files)
               "(none recorded)"
               (string-join target-files ", ")))
   "Begin the first edit now."))

;; Hard-limit failure cause. BUG-0037 W1 reclassification: a stall death
;; during an attempt with zero file mutations maps to the INFRA-RETRY path
;; ('infra-failed outcome), not straight to campaign stop — the bounded
;; automatic re-attempt carries prior-attempt context instead of forcing a
;; manual /retry. Wording keeps D8 (#9357)'s infra vocabulary out so
;; infra-failure? itself does not double-match; classification happens via
;; the explicit gsd-stall-exn? handlers.
(define (stall-hard-failure-message calls-since-mutation
                                    limit
                                    target-files
                                    [stall-tool #f]
                                    [recent-tools '()])
  (define targets-desc
    (if (null? target-files)
        "(none recorded)"
        (string-join target-files ", ")))
  (define tools-desc
    (if (null? recent-tools)
        "(none recorded)"
        (string-join (map (lambda (t) (format "~a" t)) recent-tools) ", ")))
  (format (string-append "mutation-stall watchdog: attempt terminated after ~a mutation-free "
                         "calls (limit ~a)~a. Target files: ~a. Recent tools: ~a. "
                         "The attempt will be re-attempted automatically with its prior "
                         "context preserved — resume implementation from recorded state.")
          calls-since-mutation
          limit
          (if stall-tool
              (format " — repeating '~a'" stall-tool)
              "")
          targets-desc
          tools-desc))

;; Steering injection hook. Default implementation logs the steering and
;; arms the thread's empty-response re-anchor (W2's plumbing: the same
;; channel command-handlers parameterizes at session start) with the
;; steering message, so the next reasoning-only turn re-anchors the
;; executor to "begin the first edit now". Bindable for tests and for
;; future direct-injection adapters.
(define current-gsd-stall-steerer
  (make-parameter (lambda (message)
                    (log-info "gsd mutation-stall watchdog: steering executor (~a chars)"
                              (string-length (if (string? message) message "")))
                    ;; W2 plumbing: the re-anchor nudge is re-sent on the next empty
                    ;; visible-output turn — exactly the failure mode of v1.00.16 W3
                    ;; attempt-2 (long reasoning turns, no edits).
                    (current-empty-response-nudge message)
                    (void))))

;; Read a wave's declared target files from its wave doc (best effort).
;; The campaign record carries no file list; the wave doc does. Any
;; failure degrades to '() — steering without target names still orders
;; the first edit.
;; Wave-doc "File:" declaration lines: `- File: <path>` with optional
;; [exists]/[MISSING] and role annotations after the path. Paths contain
;; no spaces, so capture the first token.
(define wave-file-line-rx #rx"^[-*] *File: *([^ \t\n]+)")

(define (wave-doc-target-files base-dir wave-idx)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define slug (wave-slug base-dir wave-idx))
    (and slug
         (string? slug)
         (let ([doc (read-wave-doc base-dir wave-idx slug)])
           (and (hash? doc)
                (let ([content (hash-ref doc 'content "")])
                  (and (string? content)
                       (for/list ([line (in-list (string-split content "\n"))]
                                  #:when (regexp-match? wave-file-line-rx line))
                         (cadr (regexp-match wave-file-line-rx line))))))))))

;; Wrap a run-one function with stall observation. Chained onto the
;; existing post-tool-result hook (memory extraction keeps working) and
;; thread-inherited by the run-wave-with-timeout worker, so the parameter
;; IS visible in the live executor session. Returns a function idx →
;; outcome; a hard-stall raise is converted HERE for runners without
;; their own exn handler (the production path converts at the runner
;; boundary with the same message).
(define (wrap-run-one-with-stall-watchdog run-one-fn
                                          watchdog
                                          base-dir
                                          rec
                                          wave-idx
                                          soft-limit
                                          hard-limit)
  (if (not watchdog)
      run-one-fn
      (let* ([target-files (or (wave-doc-target-files base-dir wave-idx) '())]
             [wave (find-wave rec wave-idx)]
             [task-line (if wave
                            (format "W~a: ~a" wave-idx (campaign-wave-title wave))
                            (format "W~a" wave-idx))]
             [campaign-id (campaign-plan-id rec)]
             [prev-hook (current-post-tool-result-hook)])
        (lambda (idx)
          ;; BUG-0037 W1: a watchdog kill is RETRYABLE infrastructure —
          ;; map to 'infra-failed so run-once*'s bounded auto-retry picks
          ;; the attempt back up with prior-attempt context instead of
          ;; halting the campaign on 'wave-failed.
          (with-handlers ([gsd-stall-exn? (lambda (e)
                                            (wave-execution-outcome 'infra-failed (exn-message e)))])
            (parameterize
                ([current-post-tool-result-hook
                  (lambda (msgs sid root)
                    (prev-hook msgs sid root)
                    ;; BUG-0037 W1: records MUST carry 'arguments — the v2
                    ;; signature is tool name + normalized arguments hash,
                    ;; so a read of file A and a read of file B are
                    ;; DIFFERENT signatures. Without arguments every read
                    ;; collapses to one signature and any three reads trip
                    ;; the repetition limit.
                    (define records
                      (for/list ([m (in-list (if (list? msgs)
                                                 msgs
                                                 '()))]
                                 #:when (and (hash? m) (hash-ref m 'name #f)))
                        (hasheq 'name (hash-ref m 'name #f) 'arguments (hash-ref m 'arguments #f))))
                    (define event (stall-watchdog-observe! watchdog records))
                    (case event
                      [(soft-stall)
                       (define snap (stall-watchdog-snapshot watchdog))
                       (log-info "gsd: wave ~a soft stall (~a calls, no mutation) — steering"
                                 wave-idx
                                 (hash-ref snap 'calls-since-mutation))
                       ((current-gsd-stall-steerer)
                        (stall-steering-message (hash-ref snap 'calls-since-mutation)
                                                (format "W~a" wave-idx)
                                                campaign-id
                                                task-line
                                                target-files))]
                      [(hard-stall)
                       (define snap (stall-watchdog-snapshot watchdog))
                       (log-error
                        "gsd: wave ~a hard stall (~a calls, no mutation, reason ~a) — failing attempt"
                        wave-idx
                        (hash-ref snap 'calls-since-mutation)
                        (hash-ref snap 'stall-reason 'unknown))
                       (raise (make-gsd-stall-exn (stall-hard-failure-message
                                                   (hash-ref snap 'calls-since-mutation)
                                                   (or hard-limit 0)
                                                   target-files
                                                   (hash-ref snap 'stall-tool #f)
                                                   (hash-ref snap 'recent-tools '()))))]
                      [else (void)]))])
              (run-one-fn idx)))))))

;; Prefix of the delivery-verifier message emitted when a wave finished but
;; ZERO declared target files changed (delivery-verifier.rkt: "no wave target
;; files changed: f1, f2, ..."). Only this verdict gets the bounded
;; failure-context retry: a plain "verifier rejected" (empty message) or any
;; other rejection still fails the wave on the first attempt.
;; BUG-0037 W1 follow-up (live campaign evidence, v1.00.20 W2 attempt 1):
;; the executor session's tool loop catches the gsd-stall-exn INSIDE the
;; worker and converts it to a loop-result 'error termination carrying the
;; stall message — so the gsd-stall-exn? handlers at the runner boundary
;; never fire and the death classified as a plain 'failed. Recognize the
;; canonical prefix here and route it to the retryable infra-failed path.
(define stall-cause-prefix "mutation-stall watchdog:")

;; BUG-0037 W1: classify a failure message as a stall-watchdog kill so
;; infra-retry treats it as bounded-auto-resume, and W6 (BUG-0040) maps it
;; to the 'stall-terminal notification kind.
(define (stall-cause-message? msg)
  (and (string? msg)
       (>= (string-length msg) (string-length stall-cause-prefix))
       (string-prefix? msg stall-cause-prefix)))

(define no-change-rejection-prefix "no wave target files changed")

;; v1.00.22 W7 (BUG-0042): effective stall-threshold composition moved
;; here from run-campaign-wave (v1.00.21 W1, BUG-0044) — behavior
;; preserved exactly. Pure: `settings` is the pre-loaded project
;; settings (or #f when the best-effort load failed); each override is
;; a number or 'unset. Precedence: keyword override > settings key >
;; canonical default (8/15/30/300).
(define (resolve-effective-stall-thresholds settings
                                            #:soft [soft 'unset]
                                            #:hard [hard 'unset]
                                            #:window [window 'unset]
                                            #:backstop [backstop 'unset])
  (values (if (eq? soft 'unset)
              (or (gsd-stall-soft-limit settings) STALL-SOFT-LIMIT-DEFAULT)
              soft)
          (if (eq? hard 'unset)
              (or (gsd-stall-hard-limit settings) STALL-HARD-LIMIT-DEFAULT)
              hard)
          (if (eq? window 'unset)
              (or (gsd-stall-window settings) STALL-REPETITION-WINDOW-DEFAULT)
              window)
          (if (eq? backstop 'unset)
              (or (gsd-stall-backstop settings) STALL-BACKSTOP-LIMIT-DEFAULT)
              backstop)))

(define (no-change-rejection? verifier-message)
  (and (string? verifier-message)
       (>= (string-length verifier-message) (string-length no-change-rejection-prefix))
       (string-prefix? verifier-message no-change-rejection-prefix)))

;; "no wave target files changed: f1, f2" → '("f1" "f2"). The verifier
;; comma-space-joins the declared targets into its message; recover the list
;; so the retry prompt can name the files explicitly.
(define (no-change-target-files verifier-message)
  (define body
    (substring verifier-message
               (min (string-length verifier-message)
                    (add1 (string-length no-change-rejection-prefix)))))
  (filter non-empty-string? (map string-trim (string-split body ","))))

;; v1.00.22 W6 (BUG-0040): map a runner failure message to the terminal
;; notification kind — a stall-watchdog kill is its own surface kind so a
;; detached operator can tell "broken" from "hung".
(define (wave-failure-notification-kind message)
  (if (and (string? message) (stall-cause-message? message)) 'stall-terminal 'wave-failed))
