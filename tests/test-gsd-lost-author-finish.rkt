#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/author-session-recovery.rkt extensions/gsd/delivery-finalize.rkt extensions/gsd/go-orchestrator.rkt
(require rackunit
         json
         "helpers/lost-author-fixture.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/author-session-recovery.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/evidence-author-journal.rkt"
         "../extensions/gsd/review-proof.rkt")
(define (go! f coordinator)
  (run-campaign! (lost-author-root f)
                 (load-campaign-record (lost-author-root f) (lost-author-plan f))
                 #:isolate? #t
                 #:delivery-reader (lambda _ #f)
                 #:runner (lambda (_) (error 'regression "MUST NOT redispatch any model phase"))
                 #:verifier (lambda (_) (error 'regression "MUST use owner full frozen Verify"))
                 #:delivery-coordinator coordinator))
(module+ test
  (test-case "BUG-0086 /go finishes the committed successor with owned Verify then fresh review"
    (with-lost-author
     (lambda (f)
       (define review-calls 0)
       (define running (journal f))
       (define before-head (git-output (lost-author-cwd f) "rev-parse" "HEAD"))
       (define result
         (go! f
              (lambda (_root _plan _wave)
                (set! review-calls (add1 review-calls))
                (check-equal? (hash-ref (journal f) 'stage) "context-ready")
                ;; Exercise production review admission, not just a stage
                ;; label. Remote F5/PR/CI transport is outside this fixture.
                (define admitted
                  (default-delivery-controller _root _plan _wave "implementation-review"))
                (check-not-false (delivery-effect-result? admitted))
                (check-equal? (delivery-effect-result-kind admitted) 'blocked)
                (check-regexp-match #rx"a genuine independent reviewer must commit it after Verify"
                                    (hash-ref (delivery-effect-result-data admitted) 'reason))
                (cons (delivery-effect-result-kind admitted)
                      (format "~s" (delivery-effect-result-data admitted))))))
       (check-equal? review-calls 1 (format "must route to fresh review: ~s" result))
       (define j (journal f))
       (check-equal? (hash-ref (journal-evidence-author-repair j) 'state) "completed")
       (check-equal? (hash-ref j 'receipt-history '()) (list (lost-author-old f)))
       (define next (hash-ref j 'receipt))
       (check-equal? (hash-ref next 'head) before-head)
       (check-not-false (load-receipt-owned-proof next (lost-author-root f) (lost-author-plan f) 0))
       (check-equal? (git-output (lost-author-cwd f) "rev-parse" "HEAD") before-head)
       (check-equal? (journal-review-rejection-count j) 0)
       (define manifest (load-receipt-owned-proof next (lost-author-root f) (lost-author-plan f) 0))
       (check-equal? (hash-ref manifest 'command)
                     "raco test tests/probe.rkt && raco test tests/second.rkt")
       (check-equal? (path->directory-path (hash-ref manifest 'execution-cwd))
                     (path->directory-path (lost-author-cwd f)))
       (check-not-equal? (hash-ref next 'repo)
                         (hash-ref manifest 'execution-cwd)
                         "common Git identity is not the execution worktree path")
       (check-equal? (git-output (lost-author-cwd f)
                                 "ls-remote"
                                 "origin"
                                 (string-append "refs/heads/" (hash-ref next 'branch)))
                     (string-append before-head "\trefs/heads/" (hash-ref next 'branch)))
       (check-equal? (hash-ref (load-approved-publication (lost-author-root f) (lost-author-plan f) 0)
                               'status)
                     "confirmed")
       (go! f
            (lambda _
              (set! review-calls (add1 review-calls))
              (cons 'blocked "fresh review")))
       (check-equal? review-calls 2)
       (check-equal? (journal f) j "repeat /go is review-only; no new proof or receipt")
       (define rec (load-campaign-record (lost-author-root f) (lost-author-plan f)))
       (check-false (delivery-receipt-blocker j rec (lost-author-plan f) 0 (campaign-fence-token rec))
                    "successor provenance must admit the production delivery coordinator")
       ;; Crash after confirmed publication/provenance but before journal
       ;; supersession: /go replays the existing exact proof, not Verify.
       (call-with-output-file (delivery-journal-path (lost-author-root f) (lost-author-plan f) 0)
                              (lambda (out) (write-json running out))
                              #:exists 'truncate)
       (parameterize ([current-gsd-delivery-verify-command "exit 99"])
         (go! f (lambda _ (cons 'blocked "fresh review after publication crash"))))
       (check-equal? (journal f) j))))

  (define (assert-refused f rx)
    (define before (journal f))
    (define head (git-output (lost-author-cwd f) "rev-parse" "HEAD"))
    (define calls 0)
    (define result
      (go! f
           (lambda _
             (set! calls (add1 calls))
             (cons 'blocked "unexpected review"))))
    (check-equal? calls 0)
    (check-equal? (campaign-result-status result) 'wave-blocked)
    (check-regexp-match rx (campaign-result-message result))
    (check-equal? (journal f) before "refusal keeps running marker and old receipt byte-for-byte")
    (check-equal? (git-output (lost-author-cwd f) "rev-parse" "HEAD") head)
    result)

  (test-case "live session never starts owner Verify or publication"
    (with-lost-author (lambda (f)
                        (define before (journal f))
                        (define blocked #f)
                        (run-coordinator-checkpoint
                         (lost-author-root f)
                         (lost-author-plan f)
                         (car (campaign-record-waves (load-campaign-record (lost-author-root f)
                                                                           (lost-author-plan f))))
                         "live session"
                         (lambda _ (error 'test "no coordinator dispatch"))
                         #:author-session-live? (lambda () #t)
                         #:on-advance (lambda () (error 'test "no live recovery"))
                         #:on-blocked (lambda (why) (set! blocked why)))
                        (check-regexp-match #rx"ambiguous launches pause" blocked)
                        (check-equal? (journal f) before))))

  (for ([kind '(unstaged staged untracked)])
    (test-case (format "dirty ~a snapshot refuses /go and preserves files" kind)
      (with-lost-author
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (define path (if (eq? kind 'untracked) "operator-notes.txt" "README.md"))
         ;; Put an unrelated metrics delta in the same checkout, not a copy.
         (when (not (eq? kind 'untracked))
           (commit! cwd path "baseline metrics\n"))
         (put! (build-path cwd path) "operator's dirty metrics -- preserve me\n")
         (when (eq? kind 'staged)
           (git-output cwd "add" "--" path))
         (define status (git-output cwd "status" "--porcelain=v1" "--untracked-files=all"))
         (assert-refused f #rx"dirty or unavailable.*preserve all files")
         (check-equal? (file->string (build-path cwd path))
                       "operator's dirty metrics -- preserve me\n")
         (check-equal? (git-output cwd "status" "--porcelain=v1" "--untracked-files=all") status)))))

  (for ([kind
         '(successor fence coordinator-fence branch-binding head-binding cancellation budget frozen)])
    (test-case (format "refuses ~a authority drift" kind)
      (with-lost-author
       (lambda (f)
         (define root (lost-author-root f))
         (define plan (lost-author-plan f))
         (define rec (load-campaign-record root plan))
         (define w (car (campaign-record-waves rec)))
         (case kind
           [(successor) (set-campaign-wave-current-attempt! w (campaign-attempt "other" 2 0))]
           [(fence) (set-campaign-wave-current-attempt! w (campaign-attempt "attempt-2" 3 0))]
           [(coordinator-fence) (set-campaign-fence-token! rec 3)]
           [(branch-binding) (set-campaign-wave-delivery-branch! w "wrong-branch")]
           [(head-binding) (set-campaign-wave-delivery-head-sha! w (make-string 40 #\0))]
           [(cancellation)
            (set-campaign-cancellation! rec (make-campaign-cancellation "cancelled" 0))]
           [(budget)
            (set-campaign-record-budget-pause! rec (campaign-budget-pause 'max-cost 1 2 "paused" 0))]
           [(frozen)
            (define p (build-path root ".planning" "waves" "W0-checkpoint.md"))
            (put! p (string-append (file->string p) "\nDrifting author instructions.\n"))])
         (unless (eq? kind 'frozen)
           (persist-campaign! root rec))
         (cond
           [(eq? kind 'cancellation)
            (define before (journal f))
            (check-equal? (campaign-result-status (go! f (lambda _ (error 'test "no dispatch"))))
                          'wave-cancelled)
            (check-equal? (journal f) before)]
           [else
            (assert-refused f (if (eq? kind 'frozen) #rx"drift" #rx"stale successor/fence"))])))))

  (for ([kind '(outside reverted-outside rename whitespace-path non-descendant red-verify)])
    (test-case (format "refuses ~a committed successor" kind)
      (with-lost-author
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (case kind
           [(outside reverted-outside)
            (commit! cwd "outside.rkt" "#lang racket/base\n")
            (when (eq? kind 'reverted-outside)
              (git-output cwd "revert" "--no-edit" "HEAD"))]
           [(whitespace-path) (commit! cwd " tests/probe.rkt" "forbidden leading-space path\n")]
           [(rename)
            (git-output cwd "mv" "tests/probe.rkt" "outside.rkt")
            (git-output cwd "commit" "-qm" "rename outside frozen scope")]
           [(non-descendant)
            (git-output cwd "checkout" "--orphan" "unrelated")
            (git-output cwd "commit" "-qm" "unrelated root")
            (define unrelated (git-output cwd "rev-parse" "HEAD"))
            (git-output cwd "checkout" (hash-ref (lost-author-old f) 'branch))
            ;; Fixture-only force branch move, preserving the bound name.
            (git-output cwd "read-tree" "--reset" "-u" unrelated)
            (git-output cwd "update-ref" "HEAD" unrelated)]
           [(red-verify)
            (commit! cwd
                     "tests/second.rkt"
                     "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 2))\n")])
         (assert-refused f
                         (case kind
                           [(non-descendant) #rx"not a strict descendant"]
                           [(red-verify) #rx"full frozen Verify failed.*running author retained"]
                           [else #rx"outside frozen FILES"]))
         ;; Still cannot claim no-edit or buy another allowance.
         (check-exn exn:fail?
                    (lambda ()
                      (abandon-evidence-author-session! (lost-author-root f)
                                                        (lost-author-plan f)
                                                        0
                                                        (git-output cwd "rev-parse" "HEAD^{tree}")
                                                        #:guard (lambda () #t))))))))

  (test-case "publication failure resumes the exact owned intent without a second Verify"
    (with-lost-author
     (lambda (f)
       (parameterize ([current-gsd-approved-head-publisher
                       (lambda _ (hasheq 'status "blocked" 'reason "transport unavailable"))])
         (assert-refused f #rx"publication refused: transport unavailable"))
       (define pub (load-approved-publication (lost-author-root f) (lost-author-plan f) 0))
       (check-equal? (hash-ref pub 'status) "intent")
       (define expected (hash-ref pub 'receipt-candidate))
       (check-not-false
        (load-receipt-owned-proof expected (lost-author-root f) (lost-author-plan f) 0))
       ;; Override would withhold fresh proof; successful replay must reuse
       ;; the original authenticated full proof, not run another suite.
       (parameterize ([current-gsd-delivery-verify-command "exit 99"])
         (go! f (lambda _ (cons 'blocked "fresh review"))))
       (check-equal? (hash-ref (journal f) 'receipt) expected)
       (check-equal? (hash-ref (journal-evidence-author-repair (journal f)) 'state) "completed"))))

  (test-case "fence drift during real publication never certifies stale approval"
    (with-lost-author (lambda (f)
                        (define pub-root (lost-author-root f))
                        (define plan (lost-author-plan f))
                        (parameterize ([current-gsd-approved-head-publisher
                                        (lambda args
                                          (define result (apply default-approved-head-publisher args))
                                          (define rec (load-campaign-record pub-root plan))
                                          (set-campaign-fence-token! rec 3)
                                          (persist-campaign! pub-root rec)
                                          result)])
                          (assert-refused f #rx"stale successor/fence"))
                        (check-equal? (hash-ref (journal f) 'receipt) (lost-author-old f))
                        (check-equal? (hash-ref (journal-evidence-author-repair (journal f)) 'state)
                                      "running"))))

  (test-case "a passing override cannot mint owned full Verify proof"
    (with-lost-author (lambda (f)
                        (parameterize ([current-gsd-delivery-verify-command "exit 0"])
                          (assert-refused f #rx"lacks authentic owned proof")))))

  (test-case "Verify dirties its source: refuse without cleaning operator files"
    (with-lost-author
     (lambda (f)
       (commit! (lost-author-cwd f)
                "tests/second.rkt"
                (string-append
                 "#lang racket/base\n(require rackunit racket/file)\n"
                 "(module+ test (display-to-file \"keep\" \"unexpected.txt\") (check-true #t))\n"))
       (assert-refused f #rx"clean snapshot drift")
       (check-equal? (file->string (build-path (lost-author-cwd f) "tests" "unexpected.txt"))
                     "keep"))))

  (for ([kind '(branch origin)])
    (test-case (format "common repository identity refuses ~a substitution" kind)
      (with-lost-author
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (case kind
           [(branch) (git-output cwd "checkout" "-b" "wrong-branch")]
           [(origin) (git-output cwd "config" "remote.origin.url" "https://github.com/other/q.git")])
         (assert-refused f #rx"repository/branch/origin differs")))))

  (test-case "legacy done checkpoint also finishes without author/model redispatch"
    (with-lost-author (lambda (f)
                        (define rec (load-campaign-record (lost-author-root f) (lost-author-plan f)))
                        (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
                        (persist-campaign! (lost-author-root f) rec)
                        (define reviews 0)
                        (go! f
                             (lambda _
                               (set! reviews (add1 reviews))
                               (cons 'blocked "fresh review")))
                        (check-equal? reviews 1)
                        (check-equal? (hash-ref (journal-evidence-author-repair (journal f)) 'state)
                                      "completed"))))

  (test-case "concurrent checkpoint finishers serialize one Verify and one receipt generation"
    (with-lost-author
     (lambda (f)
       (define results (make-channel))
       (define (start)
         (thread (lambda ()
                   (channel-put results
                                (recover-lost-author-session! (lost-author-root f)
                                                              (lost-author-plan f)
                                                              0
                                                              #:session-live? (lambda () #f))))))
       (define a (start))
       (define b (start))
       (define x (sync/timeout 120 results))
       (define y (sync/timeout 120 results))
       (check-not-false (or (eq? x #t) (eq? y #t)))
       (thread-wait a)
       (thread-wait b)
       (check-equal? (hash-ref (journal f) 'receipt-history) (list (lost-author-old f)))
       (check-equal? (hash-ref (journal-evidence-author-repair (journal f)) 'state) "completed")))))
