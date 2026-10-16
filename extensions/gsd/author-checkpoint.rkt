#lang racket/base
;; Durable author permission -> ordinary successor attempt, without rejection.
(require racket/path
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         "delivery-receipt.rkt"
         (only-in "wave-executor.rkt" wave-worktree))
(provide author-repair-pending?
         author-repair-running?
         resolve-author-checkpoint!
         retained-author-worktree)
(define (author-repair-pending? root plan wave)
  (evidence-author-repair-pending? (load-delivery-journal root plan wave)))
(define (author-repair-running? root plan wave)
  (define journal (load-delivery-journal root plan wave))
  (define marker (journal-evidence-author-repair journal))
  (and marker (evidence-author-repair-pending? journal) (equal? (hash-ref marker 'state) "running")))
(define (resolve-author-checkpoint! root plan wave)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (define journal (load-delivery-journal root plan wave))
    (define marker (journal-evidence-author-repair journal))
    (unless (and (evidence-author-repair-pending? journal)
                 (equal? (hash-ref marker 'state) "pending"))
      (error 'author-checkpoint "no unlaunched evidence author permission; ambiguous launches pause"))
    (define record (load-campaign-record root plan))
    (define w
      (and record
           (for/first ([w (in-list (campaign-record-waves record))]
                       #:when (= wave (campaign-wave-index w)))
             w)))
    (define attempt (and w (campaign-wave-current-attempt w)))
    (define context (hash-ref marker 'context))
    (unless (and record
                 w
                 attempt
                 (not (campaign-record-cancellation record))
                 (not (campaign-record-budget-pause record))
                 (>= (campaign-fence-token record) (hash-ref context 'attempt-fence))
                 (memq (campaign-wave-status w) '(awaiting-delivery pending))
                 (equal? (campaign-attempt-id attempt) (hash-ref context 'attempt-id))
                 (equal? (campaign-attempt-fence-token attempt) (hash-ref context 'attempt-fence)))
      (error 'author-checkpoint "stale, cancelled or paused evidence author checkpoint"))
    (unless (equal? journal (load-delivery-journal root plan wave))
      (error 'author-checkpoint "author permission changed before record transition"))
    ;; No failed attempt, findings, rejection counter, approval or completion
    ;; is invented. The prior verified receipt remains immutable until a real
    ;; successor full Verify supersedes it.
    (set-campaign-wave-status! w 'pending)
    (set-campaign-wave-attempt-context!
     w
     "Evidence author permission: retain prior implementation; author compatible regression inside frozen FILES; then full Verify and fresh review.")
    (persist-campaign! root record)
    #t))
(define (retained-author-worktree root rec wave)
  (define plan (campaign-plan-id rec))
  (define journal (load-delivery-journal root plan wave))
  (and (evidence-author-repair-pending? journal)
       (let* ([marker (journal-evidence-author-repair journal)]
              [receipt (hash-ref journal 'receipt)]
              [context (hash-ref marker 'context)]
              [cwd (string->path (hash-ref context 'repo))]
              [source (committed-delivery-snapshot cwd)])
         (unless (and (equal? (hash-ref marker 'state) "pending")
                      source
                      (for/and ([k '(repo branch origin head tree)])
                        (equal? (hash-ref source k) (hash-ref receipt k))))
           (error 'author-checkpoint
                  "exact retained author checkout unavailable; no fallback or rewind"))
         (wave-worktree (string->path (hash-ref receipt 'repo))
                        cwd
                        (hash-ref receipt 'branch)
                        "origin/main"
                        (build-path root ".planning")))))
