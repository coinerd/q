#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/go-orchestrator.rkt extensions/gsd/delivery-finalize.rkt
(require rackunit
         json
         "../extensions/gsd/author-scope.rkt"
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave)
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         (only-in "../extensions/gsd/delivery-publication.rkt" approved-publication-path)
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/evidence-repair-controller.rkt"
         "../extensions/gsd/evidence-repair-ledger.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd-planning/execution-policy.rkt")
(module+ test
  (test-case "legacy owned receipt gains an ordinary author successor and authenticated RED"
    (define root (make-temporary-file "author-go-~a" 'directory))
    (define repo (build-path root "q"))
    (define (put path text)
      (make-directory* (path-only path))
      (display-to-file text path #:exists 'truncate))
    (define source "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
    (dynamic-wind
     void
     (lambda ()
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
       (put (build-path repo "source.rkt")
            "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
       (git-quiet! repo "add" ".")
       (git-quiet! repo "commit" "-qm" "unfixed baseline")
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
         (define cwd #f)
         (define (run rec fence runner)
           (define result
             (run-campaign-wave root
                                rec
                                0
                                #:fence-token fence
                                #:timeout-sec 180
                                #:timeout-retries 0
                                #:no-change-retries 0
                                #:isolate? #t
                                #:stall-soft-limit #f
                                #:stall-hard-limit #f
                                #:runner runner
                                #:verifier (lambda (_)
                                             (define check (check-verify-command root 0 #f))
                                             (delivery-verification (cadr check)
                                                                    (list check)
                                                                    "actual declared Verify"))))
           (printf "wave ~a: ~s\n" fence result)
           result)
         (run
          record
          1
          (lambda (_)
            (check-false (current-gsd-author-phase))
            (set! cwd (current-directory))
            (put (build-path cwd "source.rkt") source)
            (put
             (build-path cwd "tests/probe.rkt")
             "#lang racket/base\n(require rackunit \"../source.rkt\")\n(module+ test (check-equal? (answer) 1))\n")
            'ok))
         (define (journal)
           (load-delivery-journal root plan 0))
         (define old (hash-ref (journal) 'receipt))
         (check-true (hash? (hash-ref old 'owned-proof #f)))
         (define store (build-path (current-owned-proof-root) plan))
         (define ref (hash-ref old 'owned-proof))
         (define green
           (load-owned-review-proof store
                                    ref
                                    (hash-ref ref 'binding)
                                    #:command "raco test tests/probe.rkt"
                                    #:normalized-command "raco test tests/probe.rkt"))
         (check-equal? (find-owned-red-proofs store green) '())
         ;; Authority negatives use the genuine full Verify receipt above.
         (define context (current-evidence-context root plan 0))
         (define files (frozen-author-files root plan 0))
         (define (refuses-unchanged thunk)
           (define before (journal))
           (check-exn exn:fail? thunk)
           (check-equal? (journal) before))
         (define (reserve c fs guard)
           (reserve-evidence-author-repair! root plan 0 c fs #:guard guard))
         (refuses-unchanged (lambda () (reserve context files (lambda () #f))))
         (refuses-unchanged
          (lambda () (reserve (hash-set context 'repo (path->string root)) files (lambda () #t))))
         (refuses-unchanged (lambda () (reserve context '("q/outside.rkt") (lambda () #t))))
         (refuses-unchanged
          (lambda ()
            (update-delivery-journal! root plan 0 (hasheq 'evidence-author-repair (hasheq)))))
         (check-false (journal-evidence-author-repair (journal)))
         (define advanced? #f)
         (define rec (load-campaign-record root plan))
         (run-coordinator-checkpoint
          root
          plan
          (car (campaign-record-waves rec))
          "repair"
          (lambda (dir id wave)
            (define result (default-delivery-controller dir id wave "implementation-review"))
            (printf "controller: ~s\n" result)
            (cons (delivery-effect-result-kind result)
                  (format "~s" (delivery-effect-result-data result))))
          #:on-advance (lambda () (set! advanced? #t))
          #:on-blocked (lambda (why) (printf "blocked: ~s\n" why)))
         (check-true advanced?)
         (check-equal? (hash-ref (journal) 'receipt) old)
         (check-true (evidence-author-repair-pending? (journal)))
         (check-equal? (journal-review-rejection-count (journal)) 0)
         (check-equal? (reserve context files (lambda () #t)) (journal))
         (refuses-unchanged
          (lambda () (update-delivery-journal! root plan 0 (hasheq 'evidence-author-repair #f))))
         (refuses-unchanged
          (lambda ()
            (start-evidence-author-repair! root plan 0 "stale-successor" 2 #:guard (lambda () #f))))
         (refuses-unchanged (lambda ()
                              (start-evidence-author-repair! root
                                                             plan
                                                             0
                                                             (hash-ref old 'attempt-id)
                                                             (hash-ref old 'attempt-fence)
                                                             #:guard (lambda () #t))))
         (check-true (evidence-author-repair-pending? (journal)))
         (define pending (load-campaign-record root plan))
         (check-eq? (campaign-wave-status (car (campaign-record-waves pending))) 'pending)
         (define phases '())
         (define running-journal #f)
         (run
          pending
          2
          (lambda (_)
            (check-equal? (current-directory) cwd)
            (define running (journal-evidence-author-repair (journal)))
            (check-equal? (hash-ref running 'state) "running")
            (refuses-unchanged (lambda ()
                                 (start-evidence-author-repair! root
                                                                plan
                                                                0
                                                                "wrong-successor"
                                                                (hash-ref running 'successor-fence)
                                                                #:guard (lambda () #t))))
            (refuses-unchanged (lambda ()
                                 (reconcile-evidence-author-receipt!
                                  root
                                  plan
                                  0
                                  old
                                  #:guard (lambda () #t)
                                  #:head-ancestor? (lambda (_old-head _new-head) #t))))
            (define phase (hash-ref (current-gsd-author-phase) 'phase))
            (set! phases (append phases (list phase)))
            (case phase
              [(test-author)
               (put
                (build-path cwd "tests/probe.rkt")
                (string-append
                 "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
                 "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
                 "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))]
              [(implementation)
               (check-equal? (file->string (build-path cwd "source.rkt"))
                             source
                             "implementation must retain the authored test's source")
               ;; Crash-window snapshot: marker RUNNING, old receipt durable,
               ;; successor publication not yet written.
               (set! running-journal (file->bytes (delivery-journal-path root plan 0)))])
            'ok))
         (check-equal? phases '(test-author implementation))
         (define next (hash-ref (journal) 'receipt))
         (check-not-equal? next
                           old
                           (format "pending publication: ~s" (load-remote-pending root plan 0)))
         (check-equal? (hash-ref (journal) 'receipt-history) (list old))
         (define marker (journal-evidence-author-repair (journal)))
         (check-equal? (hash-ref marker 'state) "completed")
         (check-equal? (hash-ref marker 'successor-attempt) (hash-ref next 'attempt-id))
         (check-equal? (hash-ref marker 'successor-fence) (hash-ref next 'attempt-fence))
         (check-not-equal? (hash-ref next 'head) (hash-ref old 'head))
         (check-equal? (hash-ref next 'branch) (hash-ref old 'branch))
         (check-equal? (hash-ref (journal) 'stage) "context-ready")
         (check-equal? (journal-review-rejection-count (journal)) 0)
         (refuses-unchanged (lambda ()
                              (reserve (current-evidence-context root plan 0) files (lambda () #t))))
         (define completed-journal (journal))
         (define replay-called? #f)
         (run (load-campaign-record root plan)
              2
              (lambda (_)
                (set! replay-called? #t)
                'ok))
         (check-false replay-called?)
         (check-equal? (journal) completed-journal)
         (check-false (evidence-author-repair-pending? (journal)))
         (define journal-path (delivery-journal-path root plan 0))
         (define original-bytes (file->bytes journal-path))
         (define corrupt
           (hash-set (journal)
                     'evidence-author-repair
                     (hash-set marker 'successor-receipt-sha256 (make-string 64 #\0))))
         (dynamic-wind void
                       (lambda ()
                         (call-with-output-file journal-path
                                                (lambda (out) (write-json corrupt out))
                                                #:exists 'truncate)
                         (check-exn #rx"invalid journal" journal))
                       (lambda ()
                         (call-with-output-file journal-path
                                                (lambda (out) (write-bytes original-bytes out))
                                                #:exists 'truncate)))
         (check-equal? (file->bytes journal-path) original-bytes)
         (check-equal? (hash-ref (journal) 'receipt) next)
         (define repaired (resume-evidence-repair! root plan 0 #:guard (lambda () #t)))
         (printf "repair: ~s\n" repaired)
         (check-equal? (hash-ref repaired 'status) "ready")
         (check-equal? (hash-ref (evidence-readiness root plan 0) 'status) "ready")
         (check-equal? (hash-ref (current-evidence-context root plan 0) 'head) (hash-ref next 'head))
         (check-equal? (hash-ref (hash-ref (load-evidence-repair root plan 0 1) 'operation) 'state)
                       "registered")
         (check-equal? (hash-ref (journal) 'receipt) next)
         (check-equal? (hash-ref (journal) 'stage) "context-ready")
         (check-equal? (journal-review-rejection-count (journal)) 0)
         ;; Crash window: publication confirmed for the successor, then the
         ;; process died before receipt supersession. Restoring the running
         ;; journal snapshot recreates that exact durable state against the
         ;; still-confirmed successor publication. Recovery must replay ONLY
         ;; the authenticated publication — the coordinator/model are never
         ;; redispatched.
         (define pub-path (approved-publication-path root plan 0))
         (define coordinator-called? #f)
         (define (checkpoint)
           (define advanced? #f)
           (define blocked #f)
           (define result
             (run-coordinator-checkpoint root
                                         plan
                                         (car (campaign-record-waves (load-campaign-record root
                                                                                           plan)))
                                         "simulated crash window"
                                         (lambda (_dir _plan _wave)
                                           (set! coordinator-called? #t)
                                           (cons 'ok "coordinator must not run"))
                                         #:on-advance (lambda () (set! advanced? #t))
                                         #:on-blocked (lambda (reason) (set! blocked reason))))
           (values result advanced? blocked))
         (define pub-exists (file->bytes pub-path))
         (dynamic-wind
          void
          (lambda ()
            ;; Negative first: a running marker WITHOUT its confirmed
            ;; publication stays an honest pause (no ladder, no model).
            (call-with-output-file journal-path
                                   (lambda (out) (write-bytes running-journal out))
                                   #:exists 'truncate)
            (rename-file-or-directory pub-path (string-append (path->string pub-path) ".away"))
            (define-values (_n advanced-n blocked-n) (checkpoint))
            (check-false advanced-n)
            (check-false coordinator-called?)
            (check-true (and (string? blocked-n)
                             (or (string-contains? blocked-n "durable author permission pending")
                                 (string-contains? blocked-n "ambiguous launches pause")))
                        (format "honest pause expected: ~s" blocked-n))
            (check-equal? (hash-ref (journal-evidence-author-repair (journal)) 'state) "running")
            ;; Crash window replay: restored publication authorizes the
            ;; publication-only supersession and the journal advances.
            (rename-file-or-directory (string-append (path->string pub-path) ".away") pub-path)
            (define-values (result advanced blocked-reason) (checkpoint))
            (check-true advanced (format "recovery must advance: ~s / ~s" result blocked-reason))
            (check-false coordinator-called? "recovery must not redispatch the coordinator")
            (check-equal? (hash-ref (journal-evidence-author-repair (journal)) 'state) "completed")
            (define recovered-receipt (hash-ref (journal) 'receipt))
            (for ([k '(head tree branch origin repo attempt-id attempt-fence evidence)])
              (check-equal? (hash-ref recovered-receipt k)
                            (hash-ref next k)
                            (format "recovered receipt field ~a" k)))
            (check-equal? (hash-ref recovered-receipt 'owned-proof)
                          (hash-ref next 'owned-proof)
                          "recovery retains the successor full owned proof")
            (check-equal? (hash-ref (journal) 'receipt-history) (list old))
            (check-equal? (hash-ref (journal) 'stage) "context-ready"))
          (lambda ()
            (when (file-exists? (string-append (path->string pub-path) ".away"))
              (rename-file-or-directory (string-append (path->string pub-path) ".away") pub-path))
            (when (bytes? pub-exists)
              (call-with-output-file pub-path
                                     (lambda (out) (write-bytes pub-exists out))
                                     #:exists 'truncate))
            (call-with-output-file journal-path
                                   (lambda (out) (write-bytes original-bytes out))
                                   #:exists 'truncate)))))
     (lambda () (delete-directory/files root)))))
