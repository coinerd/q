#lang racket/base
;; STABILITY: internal
;; Receipt-pinned existing checkout selection and premerge retry continuity.
;; No new checkout, branch reset, or source rewind is authorized here.
(require racket/path
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         "review-rejection.rkt"
         "delivery-handoff.rkt"
         (only-in "wave-executor.rkt" wave-worktree)
         "../../sandbox/subprocess.rkt")
(provide review-retry-pending?
         resolve-review-checkout
         retained-review-retry-worktree)
(define excludes
  '(":(exclude)docs/reports/gsd-wave-evidence/**" ":(exclude)docs/reports/gsd-wave-reviews/**"
                                                  ":(exclude)docs/reports/gsd-wave-validation/**"))
(define (git-output cwd . args)
  (define r
    (run-subprocess "git"
                    #:args args
                    #:directory cwd
                    #:environment (controller-environment)
                    #:timeout 60
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code r))
               (not (subprocess-result-timed-out? r))
               (not (subprocess-result-truncated? r)))
    (error 'review-worktree "receipt checkout Git operation refused"))
  (string-trim (subprocess-result-stdout r)))
(define (evidence-tail? cwd head tip)
  (git-output cwd "merge-base" "--is-ancestor" head tip)
  (equal? (apply git-output cwd (append (list "diff" "--name-only" head tip "--" ".") excludes)) ""))
(define (resolve-review-checkout receipt)
  (define repo (hash-ref receipt 'repo))
  (define paths
    (for/list ([line (in-list (string-split (git-output repo "worktree" "list" "--porcelain") "\n"))]
               #:when (string-prefix? line "worktree "))
      (substring line 9)))
  (or (for/first ([cwd (in-list paths)]
                  #:when
                  (with-handlers ([exn:fail? (lambda (_) #f)])
                    (and (equal? (git-output cwd "branch" "--show-current")
                                 (hash-ref receipt 'branch))
                         (equal? (git-output cwd "status" "--porcelain" "--untracked-files=no") "")
                         (evidence-tail? cwd (hash-ref receipt 'head) "HEAD"))))
        (path->complete-path cwd))
      (error 'review-worktree "no existing clean exact-receipt worktree; refusing another checkout")))
(define (review-retry-pending? root plan wave)
  (define journal (load-delivery-journal root plan wave))
  (define marker (and journal (journal-review-rejection journal)))
  (and marker
       (not (hash-ref marker 'superseded-by-attempt #f))
       (review-rejection-matches-receipt? marker (hash-ref journal 'receipt #f))))
(define (retained-review-retry-worktree root rec wave)
  (define plan (campaign-plan-id rec))
  (and (review-retry-pending? root plan wave)
       (let* ([journal (load-delivery-journal root plan wave)]
              [receipt (hash-ref journal 'receipt)]
              [cwd (resolve-review-checkout receipt)]
              [w (for/first ([w (in-list (campaign-record-waves rec))]
                             #:when (= wave (campaign-wave-index w)))
                   w)]
              [a (and w (campaign-wave-current-attempt w))])
         (define (guard!)
           (unless (and a
                        (not (equal? (campaign-attempt-id a) (hash-ref receipt 'attempt-id)))
                        (review-rejection-live-authorized? (load-campaign-record root plan)
                                                           plan
                                                           wave
                                                           (campaign-attempt-id a)
                                                           (campaign-attempt-fence-token a))
                        (equal? journal (load-delivery-journal root plan wave)))
             (error 'review-worktree "stale or cancelled review retry checkout")))
         (guard!)
         ;; Incorporate the controller's durably published rejected artifact.
         ;; The remote tail must preserve all receipt implementation bytes.
         (git-output cwd
                     "fetch"
                     "--no-tags"
                     "origin"
                     (string-append "refs/heads/" (hash-ref receipt 'branch)))
         (define tip (git-output cwd "rev-parse" "FETCH_HEAD"))
         (unless (evidence-tail? cwd (hash-ref receipt 'head) tip)
           (error 'review-worktree "rejected branch gained source drift"))
         (guard!)
         (git-output cwd "merge" "--ff-only" tip)
         (wave-worktree (string->path (hash-ref receipt 'repo))
                        cwd
                        (hash-ref receipt 'branch)
                        "origin/main"
                        (build-path root ".planning")))))
