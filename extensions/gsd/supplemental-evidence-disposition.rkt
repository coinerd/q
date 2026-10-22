#lang racket/base
;; Sole fixed owner disposition. Retention is inert; broker waits belong to UI.
(require racket/list
         racket/runtime-path
         racket/string
         racket/system
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         (only-in "evidence-repair-format.rkt" canonical-evidence-json)
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "delivery-receipt.rkt" committed-delivery-snapshot)
         (only-in "review-proof.rkt" load-receipt-owned-proof)
         (only-in "evidence-readiness.rkt" current-evidence-context)
         "supplemental-evidence-authority.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-journal.rkt"
         "supplemental-disposition-policy.rkt"
         (only-in "supplemental-evidence-controller.rkt"
                  load-supplemental-pointer
                  supplemental-pointer-path)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide propose-supplemental-disposition!
         install-supplemental-disposition!)
(define-runtime-path orchestrator "go-orchestrator.rkt")
(define-runtime-path control-root "../..")
(define (refuse text)
  (error 'supplemental-evidence-disposition "~a" text))
(define (amendment)
  (disposition-amendment))
(define (under-lease base plan proc)
  (when (current-gsd-author-phase)
    (refuse "author phase cannot dispose tasks"))
  (define acquire (dynamic-require orchestrator 'acquire-lease))
  (define release (dynamic-require orchestrator 'release-lease!))
  (define lease (acquire base plan #:session-id "supplemental-owner-disposition"))
  (unless lease
    (refuse "campaign lease busy"))
  (dynamic-wind void proc (lambda () (release lease))))
(define (control-head)
  (define out (open-output-string))
  (unless (zero? (parameterize ([current-directory control-root]
                                [current-output-port out])
                   (system*/exit-code (find-executable-path "git") "rev-parse" "HEAD")))
    (refuse "cannot authenticate control head"))
  (string-trim (get-output-string out)))
(define (parent-pointer a)
  (hasheq 'schema
          "supplemental-pointer/v1"
          'task-proposal
          (hash-ref a 'parent-proposal)
          'task-approval
          (hash-ref a 'parent-approval)))
(define (eligible! base plan wave pointer)
  (define a (amendment))
  (unless (and (equal? plan (hash-ref a 'plan-id))
               (equal? wave (hash-ref a 'wave))
               (equal? pointer (parent-pointer a)))
    (refuse "outside the fixed disposition amendment"))
  ;; Passive immutable inspection ignores historical expiry only, never revocation.
  (define-values (proposal task terminal)
    (inspect-supplemental-disposition-parent (hash-ref pointer 'task-approval)))
  (unless (and (equal? proposal (hash-ref a 'parent-proposal))
               (equal? (hash-ref terminal 'sha256) (hash-ref a 'terminal-body-sha256))
               (equal? (sha256-string (canonical-evidence-json terminal))
                       (hash-ref a 'terminal-record-sha256)))
    (refuse "parent terminal or digest namespace changed"))
  (define allocation (hash-ref (hash-ref terminal 'facts) 'allocation))
  (unless (equal? allocation (hash-ref a 'allocation))
    (refuse "wrong failed parent allocation"))
  (define rec (load-campaign-record base plan))
  (define w
    (and rec (findf (lambda (w) (= wave (campaign-wave-index w))) (campaign-record-waves rec))))
  (define attempt (and w (campaign-wave-current-attempt w)))
  (unless (and rec
               w
               attempt
               (eq? (campaign-wave-status w) 'failed)
               (not (campaign-record-cancellation rec))
               (not (campaign-record-budget-pause rec))
               (equal? (campaign-attempt-id attempt) (hash-ref allocation 'attempt-id))
               (equal? (campaign-attempt-fence-token attempt) (hash-ref allocation 'attempt-fence))
               (equal? (campaign-fence-token rec) (hash-ref allocation 'coordinator-fence)))
    (refuse "parent must own current failed uncancelled unpaused attempt and fences"))
  (define context (hash-ref task 'context))
  (unless (and (equal? (hash-ref context 'plan-id) plan)
               (equal? (hash-ref context 'wave) wave)
               (equal? (hash-ref context 'snapshot-sha256) (hash-ref a 'snapshot-sha256))
               (equal? context (current-evidence-context base plan wave)))
    (refuse "parent context, frozen snapshot or receipt changed"))
  (define source (committed-delivery-snapshot (hash-ref context 'repo)))
  (define journal (load-delivery-journal base plan wave))
  (define receipt (and journal (hash-ref journal 'receipt #f)))
  (define manifest (and receipt (load-receipt-owned-proof receipt base plan wave)))
  (unless (and source
               manifest
               (equal? (hash-ref manifest 'execution-cwd) (hash-ref context 'repo))
               (for/and ([key '(branch origin head tree)])
                 (equal? (hash-ref source key) (hash-ref context key))))
    (refuse "retained clean source or authenticated owned receipt proof changed"))
  (values proposal task terminal))
(define (propose-supplemental-disposition! base plan wave expected-parent-proposal-sha expires-at)
  (under-lease
   base
   plan
   (lambda ()
     (call-with-supplemental-authority-lock
      (lambda ()
        (define pointer
          (or (load-supplemental-pointer base plan wave) (refuse "missing parent pointer")))
        (unless (equal? expected-parent-proposal-sha
                        (hash-ref (hash-ref pointer 'task-proposal) 'sha256))
          (refuse "expected parent proposal changed"))
        (define-values (proposal parent terminal) (eligible! base plan wave pointer))
        (define now (current-seconds))
        (unless (and (exact-positive-integer? expires-at)
                     (< now expires-at)
                     (<= expires-at (+ now (hash-ref (hash-ref parent 'bounds) 'deadline-sec))))
          (refuse "fresh bounded expiry required"))
        (define a (amendment))
        (define disposition
          (for/fold ([d (hasheq 'amendment-id
                                (hash-ref a 'sha256)
                                'pointer-sha256
                                (supplemental-record-digest pointer))])
                    ([key '(root-proposal root-approval
                                          parent-proposal
                                          parent-approval
                                          terminal-body-sha256
                                          terminal-record-sha256
                                          allocation)])
            (hash-set d key (hash-ref a key))))
        (define task
          (seal-supplemental-record (hash-set* parent
                                               'kind
                                               "supplemental-evidence-task/v3"
                                               'control-plane-head
                                               (control-head)
                                               'disposition
                                               disposition)))
        (unless (valid-supplemental-task? task)
          (refuse "invalid disposition lineage"))
        (values (propose-retain! "task-proposal" (hasheq 'task task 'expires-at expires-at))
                task))))))
(define (install-supplemental-disposition! base plan wave new-proposal-ref new-approval-ref)
  (under-lease
   base
   plan
   (lambda ()
     (call-with-supplemental-authority-lock
      (lambda ()
        (unless (validate-supplemental-approval new-approval-ref new-proposal-ref)
          (refuse "exact genuine disposition task approval required"))
        (define task (hash-ref (hash-ref (load-supplemental-record new-proposal-ref) 'payload) 'task))
        (unless (and (valid-supplemental-task? task)
                     (equal? (hash-ref task 'kind) "supplemental-evidence-task/v3"))
          (refuse "disposition requires task/v3"))
        (define old (parent-pointer (amendment)))
        (eligible! base plan wave old)
        (define target
          (hasheq 'schema
                  "supplemental-pointer/v1"
                  'task-proposal
                  new-proposal-ref
                  'task-approval
                  new-approval-ref))
        (install-supplemental-disposition-claim! (supplemental-pointer-path base plan wave)
                                                 old
                                                 target
                                                 new-proposal-ref
                                                 new-approval-ref))))))
