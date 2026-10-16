#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 600
;; @covers extensions/gsd/evidence-author-journal.rkt extensions/gsd/delivery-finalize.rkt
;; BUG-0085: an evidence author that was durably launched ("running") and then
;; lost — the orchestrator died before any author return, the ownership
;; contract reaped the session — used to strand the wave forever: the
;; checkpoint's resolve demanded "pending" and the publication recovery had
;; nothing confirmed. The honest terminal is retained/session-lost with the
;; receipt unchanged, exactly like the no-edit return, gated on a real
;; liveness probe and an unchanged tree.
(require rackunit
         "../extensions/gsd/author-scope.rkt"
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave)
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         (only-in "../extensions/gsd/evidence-author-journal.rkt"
                  reserve-evidence-author-repair!
                  start-evidence-author-repair!
                  abandon-evidence-author-session!)
         (only-in "../extensions/gsd/campaign-result.rkt" campaign-result-status)
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd-planning/execution-policy.rkt")

(module+ test
  (define (make-campaign-with-running-author!)
    ;; A verified wave-0 receipt, a reserved and LAUNCHED author permission
    ;; ("running"), and no successor publication: the live W1 shape.
    (define root (make-temporary-file "author-reclaim-~a" 'directory))
    (define repo (build-path root "q"))
    (define (put path text)
      (make-directory* (path-only path))
      (display-to-file text path #:exists 'truncate))
    (make-directory repo)
    (git-quiet! repo "init" "-q")
    (hermetic-identity! repo)
    (define remote (build-path root "origin.git"))
    (git-quiet! repo "init" "--bare" "-q" (path->string remote))
    (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
    (git-quiet! repo
                "config"
                (string-append "url.file://" (path->string remote) ".insteadOf")
                "https://github.com/example/q.git")
    (put (build-path repo ".gitignore") "compiled/\n")
    (put (build-path repo "source.rkt") "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
    (git-quiet! repo "add" ".")
    (git-quiet! repo "commit" "-qm" "delivered candidate")
    (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/main")
    (git-quiet! repo "update-ref" "refs/remotes/origin/main" "HEAD")
    (make-directory* (build-path root ".planning" "waves"))
    (write-plan! root 0 "Checkpoint" "checkpoint")
    (write-wave-doc! root
                     0
                     "checkpoint"
                     '("q/source.rkt" "q/tests/probe.rkt")
                     "raco test tests/probe.rkt")
    (define record (migrate-campaign! root))
    (set-campaign-record-execution-policy! record #f)
    (persist-campaign! root record)
    (define plan (campaign-plan-id record))
    (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                   [current-gsd-verification-registry (make-verification-registry)])
      ;; The candidate is already committed: the first pass verifies and
      ;; delivers it as-is, producing the genuine full Verify receipt.
      (run-campaign-wave
       root
       record
       0
       #:fence-token 1
       #:timeout-sec 180
       #:timeout-retries 0
       #:no-change-retries 0
       #:isolate? #t
       #:stall-soft-limit #f
       #:stall-hard-limit #f
       #:runner
       (lambda (_)
         ;; The candidate lands in the retained
         ;; worktree and is committed by the owner,
         ;; so the declared Verify passes.
         (define cwd (current-directory))
         (put (build-path cwd "source.rkt")
              "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
         (put
          (build-path cwd "tests/probe.rkt")
          "#lang racket/base\n(require rackunit \"../source.rkt\")\n(module+ test (check-equal? (answer) 0))\n")
         'ok)
       #:verifier (lambda (_)
                    (define check (check-verify-command root 0 #f))
                    (delivery-verification (cadr check) (list check) "actual declared Verify")))
      ;; Everything downstream reads the owned-proof store and the journal
      ;; through the same parameter extent: the proof store is a process
      ;; parameter, so a checkpoint outside it would refuse the receipt.
      (define (journal)
        (load-delivery-journal root plan 0))
      (define old (hash-ref (journal) 'receipt))
      ;; Move the ladder to the author-eligible stage, reserve the allowance,
      ;; and launch it — then lose the session: the marker stays "running".
      (define advanced? #f)
      (run-coordinator-checkpoint
       root
       plan
       (car (campaign-record-waves (load-campaign-record root plan)))
       "repair"
       (lambda (dir id wave)
         (define result (default-delivery-controller dir id wave "implementation-review"))
         (cons (delivery-effect-result-kind result)
               (format "~s" (delivery-effect-result-data result))))
       #:on-advance (lambda () (set! advanced? #t)))
      (check-true advanced?)
      (define context (current-evidence-context root plan 0))
      (define files (frozen-author-files root plan 0))
      (reserve-evidence-author-repair! root plan 0 context files #:guard (lambda () #t))
      (start-evidence-author-repair! root plan 0 "lost-author-attempt" 2 #:guard (lambda () #t))
      (check-equal? (hash-ref (journal-evidence-author-repair (journal)) 'state) "running")
      ;; The owned-proof store and the verification registry are process
      ;; parameters: every downstream checkpoint must run in the same extent.
      (define proof-root (build-path root "owned-proofs"))
      (define (checkpoint! #:session-live? [live? #t])
        (define advanced? #f)
        (define blocked-reason #f)
        (parameterize ([current-owned-proof-root proof-root]
                       [current-gsd-verification-registry (make-verification-registry)])
          (run-coordinator-checkpoint
           root
           plan
           (car (campaign-record-waves (load-campaign-record root plan)))
           "reclaim"
           (lambda (dir id wave)
             (define result (default-delivery-controller dir id wave "implementation-review"))
             (cons (delivery-effect-result-kind result)
                   (format "~s" (delivery-effect-result-data result))))
           #:author-session-live? (lambda () live?)
           #:on-advance (lambda () (set! advanced? #t))
           #:on-blocked (lambda (why) (set! blocked-reason why))))
        (values advanced? blocked-reason))
      (values root plan journal old checkpoint!)))

  (test-case "a lost author session is reclaimed into retained/session-lost and advances (BUG-0085)"
    (define-values (root plan journal old checkpoint!) (make-campaign-with-running-author!))
    (define-values (advanced? blocked-reason) (checkpoint! #:session-live? #f))
    (check-true advanced? "the durable reclaim IS the advance")
    (check-false blocked-reason)
    (define j (journal))
    (define m (journal-evidence-author-repair j))
    (check-equal? (hash-ref m 'state) "retained")
    (check-equal? (hash-ref m 'disposition) "session-lost")
    (check-equal? (hash-ref m 'retained-tree-sha256) (hash-ref old 'tree))
    (check-equal? (hash-ref j 'receipt) old "session loss never supersedes the receipt")
    (check-false (evidence-author-repair-pending? j) "the allowance is spent"))

  (test-case "a live author session still pauses; the checkpoint never reclaims it"
    (define-values (root plan journal old checkpoint!) (make-campaign-with-running-author!))
    (define-values (advanced? blocked-reason) (checkpoint! #:session-live? #t))
    (check-false advanced?)
    (check-true (and (string? blocked-reason) (string-contains? blocked-reason "author-checkpoint"))
                (format "the honest ambiguous-launch pause names its checkpoint: ~s" blocked-reason))
    (check-equal? (hash-ref (journal-evidence-author-repair (journal)) 'state) "running"))

  (test-case "the session-loss transition is write-once and refuses a changed tree"
    (define-values (root plan journal old checkpoint!) (make-campaign-with-running-author!))
    ;; Write-once: after the reclaim, a second transition refuses and the
    ;; durable state is byte-identical.
    (define-values (advanced? _) (checkpoint! #:session-live? #f))
    (check-true advanced?)
    (define j (journal))
    (check-exn exn:fail?
               (lambda ()
                 (abandon-evidence-author-session! root plan 0 (hash-ref old 'tree) #:guard (lambda () #t))))
    (check-equal? (journal) j)
    ;; A fresh campaign whose author DID land an edit before the loss: the
    ;; tree no longer matches the retained receipt — reconciliation event,
    ;; never a reclaim.
    (define-values (root2 plan2 journal2 old2 checkpoint2!) (make-campaign-with-running-author!))
    (call-with-output-file (build-path root2 "q" "source.rkt")
                           (lambda (out) (display "\n; author edit\n" out))
                           #:exists 'append)
    ;; The MEASURED changed tree refuses: the author may have landed edits,
    ;; and that is a reconciliation event, never a silent reclaim.
    (define changed
      (let ((repo (build-path root2 "q")))
        (git-quiet! repo "add" "-A")
        (string-trim
         (with-output-to-string
           (lambda () (system* (find-executable-path "git") "write-tree"))))))
    (check-true (and (string? changed) (= (string-length changed) 40)))
    (check-exn exn:fail?
               (lambda ()
                 (abandon-evidence-author-session! root2 plan2 0 changed #:guard (lambda () #t))))
    (check-equal? (hash-ref (journal-evidence-author-repair (journal2)) 'state) "running")))
