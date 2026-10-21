#lang racket/base
;; Explicit owner recovery only. No broker wait or provider selection occurs here.
;; Provider choice remains operator-selected, NOT pinned or a quota guarantee.
(require racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         (only-in "delivery-receipt.rkt" committed-delivery-snapshot)
         (only-in "review-proof.rkt" load-receipt-owned-proof)
         (only-in "evidence-readiness.rkt" current-evidence-context)
         "supplemental-evidence-authority.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-journal.rkt"
         (only-in "supplemental-evidence-controller.rkt"
                  load-supplemental-pointer supplemental-pointer-path)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide propose-supplemental-recovery! install-supplemental-recovery!)
(define-runtime-path orchestrator "go-orchestrator.rkt")
(define-runtime-path control-root "../..")
(define (refuse text) (error 'supplemental-evidence-recovery "~a" text))
(define (under-lease base plan proc)
  (when (current-gsd-author-phase) (refuse "author phase cannot recover tasks"))
  (define acquire (dynamic-require orchestrator 'acquire-lease))
  (define release (dynamic-require orchestrator 'release-lease!))
  (define lease (acquire base plan #:session-id "supplemental-owner-recovery"))
  (unless lease (refuse "campaign lease busy"))
  (dynamic-wind void proc (lambda () (release lease))))
(define (control-head)
  (define out (open-output-string))
  (unless (zero? (parameterize ([current-directory control-root] [current-output-port out])
                   (system*/exit-code (find-executable-path "git") "rev-parse" "HEAD")))
    (refuse "cannot authenticate control head"))
  (string-trim (get-output-string out)))
(define (parent-pointer lineage)
  (hasheq 'schema "supplemental-pointer/v1"
          'task-proposal (hash-ref lineage 'parent-proposal)
          'task-approval (hash-ref lineage 'parent-approval)))
(define (eligible! base plan wave pointer)
  (define grant (hash-ref pointer 'task-approval))
  (define-values (proposal task terminal) (inspect-supplemental-recovery-parent grant))
  (unless (equal? proposal (hash-ref pointer 'task-proposal)) (refuse "wrong parent proposal"))
  (define allocation (hash-ref (hash-ref terminal 'facts) 'allocation))
  (define rec (load-campaign-record base plan))
  (define w (and rec (findf (lambda (w) (= wave (campaign-wave-index w))) (campaign-record-waves rec))))
  (define attempt (and w (campaign-wave-current-attempt w)))
  ;; Exclusive lease + terminal failed attempt means the old owned runner has
  ;; exited its dynamic-wind and closed its dedicated verification registry.
  (unless (and rec w attempt (eq? (campaign-wave-status w) 'failed)
               (not (campaign-record-cancellation rec)) (not (campaign-record-budget-pause rec))
               (equal? (campaign-attempt-id attempt) (hash-ref allocation 'attempt-id))
               (equal? (campaign-attempt-fence-token attempt) (hash-ref allocation 'attempt-fence))
               (equal? (campaign-fence-token rec) (hash-ref allocation 'coordinator-fence)))
    (refuse "parent must own the current failed, uncancelled, unpaused attempt and fences"))
  (define context (hash-ref task 'context))
  (unless (and (equal? (hash-ref context 'plan-id) plan) (= (hash-ref context 'wave) wave)
               (equal? context (current-evidence-context base plan wave)))
    (refuse "parent context or receipt changed"))
  (define source (committed-delivery-snapshot (hash-ref context 'repo)))
  (define journal (load-delivery-journal base plan wave))
  (define receipt (and journal (hash-ref journal 'receipt #f)))
  (define manifest (and receipt (load-receipt-owned-proof receipt base plan wave)))
  (unless (and source manifest
               (equal? (hash-ref manifest 'execution-cwd) (hash-ref context 'repo))
               (for/and ([key '(branch origin head tree)])
                 (equal? (hash-ref source key) (hash-ref context key))))
    (refuse "retained clean source or authenticated owned receipt proof changed"))
  ;; These facts prove unchanged clean source/no captured artifacts, NOT zero
  ;; model output or zero tool calls at the failed provider.
  (values proposal task terminal))
(define (propose-supplemental-recovery! base plan wave expected-parent-proposal-sha expires-at)
  (under-lease
   base plan
   (lambda ()
     (call-with-supplemental-authority-lock
      (lambda ()
        (define pointer (or (load-supplemental-pointer base plan wave) (refuse "missing parent pointer")))
        (unless (equal? expected-parent-proposal-sha (hash-ref (hash-ref pointer 'task-proposal) 'sha256))
          (refuse "expected parent proposal changed"))
        (define-values (proposal parent terminal) (eligible! base plan wave pointer))
        (unless (and (exact-positive-integer? expires-at) (< (current-seconds) expires-at)
                     (<= expires-at (+ (current-seconds) (hash-ref (hash-ref parent 'bounds) 'deadline-sec))))
          (refuse "fresh bounded expiry required"))
        (define lineage
          (hasheq 'parent-proposal proposal 'parent-approval (hash-ref pointer 'task-approval)
                  'terminal-sha256 (hash-ref terminal 'sha256)
                  'pointer-sha256 (supplemental-record-digest pointer)
                  'allocation (hash-ref (hash-ref terminal 'facts) 'allocation)))
        (define task (seal-supplemental-record
                      (hash-set* parent 'kind "supplemental-evidence-task/v2"
                                 'control-plane-head (control-head) 'recovery lineage)))
        (unless (valid-supplemental-task? task) (refuse "invalid recovery lineage"))
        (values (propose-retain! "task-proposal" (hasheq 'task task 'expires-at expires-at)) task))))))
(define (install-supplemental-recovery! base plan wave new-proposal-ref new-approval-ref)
  (under-lease
   base plan
   (lambda ()
     (call-with-supplemental-authority-lock
      (lambda ()
        (unless (validate-supplemental-approval new-approval-ref new-proposal-ref)
          (refuse "exact genuine child task approval required"))
        (define task (hash-ref (hash-ref (load-supplemental-record new-proposal-ref) 'payload) 'task))
        (unless (and (valid-supplemental-task? task) (hash-has-key? task 'recovery))
          (refuse "original task cannot replace a pointer"))
        (define old (parent-pointer (hash-ref task 'recovery)))
        (eligible! base plan wave old)
        (define target (hasheq 'schema "supplemental-pointer/v1"
                               'task-proposal new-proposal-ref 'task-approval new-approval-ref))
        (install-supplemental-recovery-claim!
         (supplemental-pointer-path base plan wave) old target new-proposal-ref new-approval-ref))))))
