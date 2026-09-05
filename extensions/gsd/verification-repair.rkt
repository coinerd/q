#lang racket/base

;; Bounded same-wave repair policy and durable retry transition (BUG-0060).

(require racket/match
         "attempt-artifacts.rkt"
         "campaign-repository.rkt"
         "campaign-state.rkt"
         "delivery-verifier.rkt"
         "events.rkt"
         "policy.rkt"
         "prompts.rkt"
         "wave-executor.rkt")

(provide repairable-verification-rejection?
         verification-repair-no-progress?
         start-verification-repair!
         emit-verification-repair-exhausted!
         resolve-verification-rejection!)

(define (repairable-verification-rejection? result)
  ;; Repository, branch, evidence, and boolean/blank rejections are not made
  ;; repairable merely by retrying an executor. Only an owned declared-Verify
  ;; subprocess failure carries the command/exit/log diagnostic contract.
  (and
   (delivery-verification? result)
   (for/or ([check (in-list (delivery-verification-evidence result))])
     (match check
       [(cons "verify" (cons #f (? string? detail))) (regexp-match? #rx"cmd=.*exit=.*log=" detail)]
       [_ #f]))))

(define (verification-repair-no-progress? previous-head current-head)
  (and previous-head current-head (equal? previous-head current-head)))

(define (emit-repair-event! name wave-idx attempt message)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "gsd: verification repair event emission failed: ~a"
                                            (exn-message e)))])
    (emit-gsd-event! name (hasheq 'wave wave-idx 'attempt attempt 'message message))))

(define (emit-verification-repair-exhausted! wave-idx attempt message)
  (emit-repair-event! 'gsd.verification.repair-exhausted wave-idx attempt message))

(define (terminal-artifact! base-dir active wave-idx attempt-id wt)
  (mark-attempt-artifact-terminal!
   base-dir
   (campaign-plan-id active)
   wave-idx
   attempt-id
   'failure
   #:merge-status
   (and wt (artifact-merge-status/local (wave-worktree-repo-root wt) (wave-worktree-branch wt)))))

;; Consume one truthful attempt and preserve its worktree/branch for `retry`.
;; `stale-result` is a thunk producing the orchestrator's stale-completion
;; result; `retry` re-enters the wave loop under the repair context.
(define (start-verification-repair! base-dir
                                    active
                                    wave-idx
                                    fence
                                    attempt-id
                                    wt
                                    repair-head-box
                                    verifier-message
                                    stale-result
                                    retry)
  (define head (and wt (wave-worktree-head-sha wt)))
  (set-box! repair-head-box head)
  (terminal-artifact! base-dir active wave-idx attempt-id wt)
  (define rec (load-campaign-record base-dir (campaign-plan-id active)))
  (define wave
    (and rec
         (= (campaign-fence-token rec) fence)
         (for/first ([candidate (in-list (campaign-record-waves rec))]
                     #:when (= (campaign-wave-index candidate) wave-idx))
           (define attempt (campaign-wave-current-attempt candidate))
           (and attempt
                (= (campaign-attempt-fence-token attempt) fence)
                (equal? (campaign-attempt-id attempt) attempt-id)
                candidate))))
  (if (not wave)
      (stale-result)
      (begin
        (set-campaign-wave-status! wave 'pending)
        (persist-campaign! base-dir rec)
        (emit-repair-event! 'gsd.verification.repair-started wave-idx attempt-id verifier-message)
        (parameterize ([current-gsd-wave-failure-context (verification-repair-context-block
                                                          verifier-message)])
          (retry)))))

;; Resolve every non-no-change verifier rejection. Result constructors are
;; injected so this module stays free of the orchestrator result type while
;; owning the durable retry/failure transition.
(define (resolve-verification-rejection! #:base-dir base-dir
                                         #:active active
                                         #:wave-index wave-idx
                                         #:fence fence
                                         #:attempt-id attempt-id
                                         #:worktree wt
                                         #:verifier-result verifier-result
                                         #:message verifier-message
                                         #:retries-left retries-left
                                         #:repair-head-box repair-head-box
                                         #:retry retry
                                         #:make-cancelled make-cancelled
                                         #:make-failed make-failed
                                         #:notify-failed! notify-failed!)
  (define repairable? (repairable-verification-rejection? verifier-result))
  (define no-progress?
    (verification-repair-no-progress? (unbox repair-head-box) (and wt (wave-worktree-head-sha wt))))
  (cond
    [(and repairable? (> retries-left 0) (not no-progress?))
     (define result
       (start-verification-repair! base-dir
                                   active
                                   wave-idx
                                   fence
                                   attempt-id
                                   wt
                                   repair-head-box
                                   verifier-message
                                   make-cancelled
                                   retry))
     (if (eq? result 'stale)
         (make-cancelled)
         result)]
    [else
     (when repairable?
       (emit-verification-repair-exhausted! wave-idx
                                            attempt-id
                                            (if no-progress?
                                                "verification repair made no branch progress"
                                                "verification repair retry budget exhausted")))
     (terminal-artifact! base-dir active wave-idx attempt-id wt)
     (define reason (if (equal? verifier-message "") "verifier rejected" verifier-message))
     (notify-failed! reason)
     (make-failed reason)]))
