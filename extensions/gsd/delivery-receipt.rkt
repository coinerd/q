#lang racket/base
;; Capture committed provenance on BOTH isolated and shared-checkout Verify.
;; A missing receipt does not rewrite an existing Verify verdict, but autonomous
;; publication subsequently refuses it. No inference from arbitrary current HEAD.
(require racket/path
         racket/string
         "delivery-journal.rkt"
         "delivery-verifier.rkt"
         "campaign-state.rkt"
         (only-in "plan-context-builder.rkt" find-git-root-dir)
         "../../sandbox/subprocess.rkt"
         (only-in "../../util/credential-redaction.rkt" redact-credential-data))
(provide committed-delivery-snapshot
         current-wave-for-attempt
         durable-receipt-head
         default-remote-published?
         verify-with-delivery-receipt
         verify-campaign-delivery
         recover-legacy-delivery-receipt!
         delivery-receipt-blocker)
;; Register F5 (v1.00.31 W3): a Verify verdict does not publish a receipt
;; until the local head is fetchable at origin. The default check is a real
;; ls-remote against the configured origin; tests inject a pure procedure.
(define (default-remote-published? repo branch head)
  (define out (git repo "ls-remote" "origin" (string-append "refs/heads/" branch)))
  (and (string? out) (string-contains? out head)))
;; Shared pure attempt fence: the same identity test guards the implementation
;; result and the Verify receipt, avoiding a second weaker completion predicate.
(define (current-wave-for-attempt rec wave-idx fence attempt-id)
  (define wave
    (and rec
         (for/first ([w (in-list (campaign-record-waves rec))]
                     #:when (= (campaign-wave-index w) wave-idx))
           w)))
  (define attempt (and wave (campaign-wave-current-attempt wave)))
  (and rec
       wave
       attempt
       (= (campaign-fence-token rec) fence)
       (= (campaign-attempt-fence-token attempt) fence)
       (equal? (campaign-attempt-id attempt) attempt-id)
       wave))
(define (git root . args)
  (define r
    (run-subprocess "git"
                    #:args (append (list "-C" (path->string (path->complete-path root))) args)
                    #:directory root
                    #:timeout 15))
  (and (not (subprocess-result-timed-out? r))
       (not (subprocess-result-truncated? r))
       (equal? (subprocess-result-exit-code r) 0)
       (string-trim (subprocess-result-stdout r))))
(define (sha? s)
  (and (string? s) (regexp-match? #px"^[0-9a-f]{40}$" s)))
(define (identity root branch head)
  (define tree (git root "rev-parse" (string-append head "^{tree}")))
  (define origin (git root "config" "--get" "remote.origin.url"))
  (define common (git root "rev-parse" "--path-format=absolute" "--git-common-dir"))
  ;; Reject credential-bearing or rewritten arbitrary origins before persistence.
  (and
   (sha? head)
   (sha? tree)
   origin
   common
   (equal? (path->string (file-name-from-path common)) ".git")
   (regexp-match? #px"^(?:https://github\\.com/|git@github\\.com:)[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$"
                  origin)
   (hasheq 'repo
           (path->string (simplify-path (path-only common)))
           'branch
           branch
           'head
           head
           'tree
           tree
           'origin
           origin)))
(define (committed-delivery-snapshot cwd)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define root (find-git-root-dir cwd))
    (and root
         (equal? (git root "status" "--porcelain=v1" "--untracked-files=all") "")
         (let ([branch (git root "symbolic-ref" "--quiet" "--short" "HEAD")]
               [head (git root "rev-parse" "HEAD")])
           (and branch
                (not (member branch '("main" "master")))
                (sha? head)
                (identity root branch head))))))
(define (verify-with-delivery-receipt base
                                      plan
                                      wave
                                      cwd
                                      thunk
                                      #:approved? approved?
                                      #:evidence evidence
                                      #:snapshot [snapshot committed-delivery-snapshot]
                                      #:attempt [attempt #f]
                                      #:remote-published [remote-published default-remote-published?])
  (define before (snapshot cwd))
  (define result (thunk))
  (define after (snapshot cwd))
  (when (and (approved? result) before (equal? before after))
    (with-handlers
        ([exn:fail?
          (lambda (_)
            (log-warning
             "delivery receipt could not be recorded; delivery will require provenance reconciliation"))])
      (define old (load-delivery-journal base plan wave))
      (cond
        ;; A durable receipt resolves any stale marker (idempotent re-verify).
        [old (clear-remote-pending! base plan wave)]
        [(remote-published (hash-ref before 'repo) (hash-ref before 'branch) (hash-ref before 'head))
         (define text (format "~a" (redact-credential-data (evidence result))))
         (record-delivery-receipt! base
                                   plan
                                   wave
                                   (hash-set* (if attempt
                                                  (hash-set* before
                                                             'attempt-id
                                                             (campaign-attempt-id attempt)
                                                             'attempt-fence
                                                             (campaign-attempt-fence-token attempt))
                                                  before)
                                              'verified-at
                                              (current-seconds)
                                              'evidence
                                              (substring text 0 (min 8192 (string-length text)))))
         (clear-remote-pending! base plan wave)]
        ;; Register F5: an unpublished branch records the typed remote-pending
        ;; marker instead of a receipt. The state is NOT verified and blocks
        ;; ladder entry with 'branch-not-published naming branch and head.
        [else
         (define reason
           (format
            "branch ~a head ~a is not published on origin; receipt not verified; push the verified head first, then re-verify"
            (hash-ref before 'branch)
            (hash-ref before 'head)))
         (record-remote-pending! base
                                 plan
                                 wave
                                 (hash-ref before 'branch)
                                 (hash-ref before 'head)
                                 reason)
         (log-warning "delivery receipt withheld: ~a" reason)])))
  result)
;; Thin orchestration seam: context and verdict interpretation stay beside
;; receipt capture. `current-record` rejects stale attempts before persistence.
(define (verify-campaign-delivery base
                                  plan
                                  wave
                                  cwd
                                  context
                                  verifier
                                  current-record
                                  #:remote-published [remote-published default-remote-published?])
  (define initial (current-record))
  (define attempt
    (and initial
         (for/first ([w (in-list (campaign-record-waves initial))]
                     #:when (= (campaign-wave-index w) wave))
           (campaign-wave-current-attempt w))))
  (verify-with-delivery-receipt
   base
   plan
   wave
   cwd
   (lambda ()
     (parameterize ([current-gsd-delivery-branch-context context])
       (verifier wave)))
   #:attempt attempt
   #:remote-published remote-published
   #:approved? (lambda (v)
                 (define current (current-record))
                 (and current
                      attempt
                      (equal? (campaign-plan-id current) plan)
                      (not (campaign-record-cancellation current))
                      (current-wave-for-attempt current
                                                wave
                                                (campaign-attempt-fence-token attempt)
                                                (campaign-attempt-id attempt))
                      (if (delivery-verification? v)
                          (delivery-verification-approved? v)
                          v)))
   #:evidence (lambda (v)
                (if (delivery-verification? v)
                    (delivery-verification-evidence v)
                    "verifier approved"))))
;; Pure eligibility prerequisite, NOT approval or delivery proof. The caller
;; must load journal and record from disk while holding the lease and repeat
;; this check before each effect. Git identity/review/checks belong to the
;; protected action adapter. A resumed coordinator has a new fence; the DONE
;; implementation attempt retains its own historical fence.
(define (delivery-receipt-blocker journal record plan wave expected-fence)
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (define w
    (and (campaign-record? record)
         (for/first ([w (in-list (campaign-record-waves record))]
                     #:when (equal? (campaign-wave-index w) wave))
           w)))
  (define attempt (and w (campaign-wave-current-attempt w)))
  (cond
    [(not (and (hash? journal)
               (equal? (hash-ref journal 'schema-version #f) 1)
               (equal? (hash-ref journal 'plan-id #f) plan)
               (equal? (hash-ref journal 'wave #f) wave)
               (valid-delivery-receipt? receipt)))
     'missing-provenance]
    [(not (and (campaign-record? record) (equal? (campaign-plan-id record) plan) w))
     'campaign-mismatch]
    [(campaign-record-cancellation record) 'cancelled]
    [(not (and (exact-nonnegative-integer? expected-fence)
               (equal? (campaign-fence-token record) expected-fence)))
     'stale-coordinator]
    [(not (eq? (campaign-wave-status w) 'done)) 'implementation-not-done]
    [(not (and attempt
               (string? (hash-ref receipt 'attempt-id #f))
               (exact-nonnegative-integer? (hash-ref receipt 'attempt-fence #f))
               (equal? (hash-ref receipt 'attempt-id) (campaign-attempt-id attempt))
               (equal? (hash-ref receipt 'attempt-fence) (campaign-attempt-fence-token attempt))))
     'unbound-attempt]
    [(member (hash-ref receipt 'branch) '("main" "master")) 'protected-branch]
    [(or (and (not (equal? (campaign-wave-delivery-branch w) ""))
              (not (equal? (campaign-wave-delivery-branch w) (hash-ref receipt 'branch))))
         (and (not (equal? (campaign-wave-delivery-head-sha w) ""))
              (not (equal? (campaign-wave-delivery-head-sha w) (hash-ref receipt 'head)))))
     'stale-provenance]
    [else #f]))

;; The durable receipt head (register F3): the verified branch head recorded
;; by verify-with-delivery-receipt for this wave, or #f when no verified
;; receipt exists. Evidence and review heads must equal it before delivery.
(define (durable-receipt-head base plan wave)
  (define journal (load-delivery-journal base plan wave))
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (define head (and (hash? receipt) (hash-ref receipt 'head #f)))
  (and (sha? head) head))

(define (recover-legacy-delivery-receipt! base plan wave branch head cwd)
  (and
   (string? branch)
   (not (string=? branch ""))
   (sha? head)
   (with-handlers ([exn:fail? (lambda (_) #f)])
     (define root (find-git-root-dir cwd))
     (and
      root
      (equal? (git root "rev-parse" (string-append "refs/heads/" branch)) head)
      (let ([snapshot (identity root branch head)])
        (and
         snapshot
         (record-delivery-receipt!
          base
          plan
          wave
          (hash-set*
           snapshot
           'verified-at
           (current-seconds)
           'evidence
           "Recovered from coordinator's durable DONE branch/head; original Verify log not retained"))))))))
