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
         verify-with-delivery-receipt
         verify-campaign-delivery
         recover-legacy-delivery-receipt!)
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
                                      #:snapshot [snapshot committed-delivery-snapshot])
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
      (unless old
        (define text (format "~a" (redact-credential-data (evidence result))))
        (record-delivery-receipt! base
                                  plan
                                  wave
                                  (hash-set* before
                                             'verified-at
                                             (current-seconds)
                                             'evidence
                                             (substring text 0 (min 8192 (string-length text))))))))
  result)
;; Thin orchestration seam: context and verdict interpretation stay beside
;; receipt capture. `current-record` rejects stale attempts before persistence.
(define (verify-campaign-delivery base plan wave cwd context verifier current-record)
  (verify-with-delivery-receipt base
                                plan
                                wave
                                cwd
                                (lambda ()
                                  (parameterize ([current-gsd-delivery-branch-context context])
                                    (verifier wave)))
                                #:approved? (lambda (v)
                                              (define current (current-record))
                                              (and current
                                                   (not (campaign-record-cancellation current))
                                                   (if (delivery-verification? v)
                                                       (delivery-verification-approved? v)
                                                       v)))
                                #:evidence (lambda (v)
                                             (if (delivery-verification? v)
                                                 (delivery-verification-evidence v)
                                                 "verifier approved"))))
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
