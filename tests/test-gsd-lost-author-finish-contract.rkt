#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 300
;; @covers extensions/gsd/author-session-recovery.rkt extensions/gsd/delivery-finalize.rkt extensions/gsd/go-orchestrator.rkt
;; E-wave required-PR contract for the BUG-0086 lost-author /go finish
;; (milestone #899). The deep two-generation owned-Verify world stays in
;; tests/test-gsd-lost-author-finish.rkt (slow/full regression); THIS file is
;; its required-PR counterpart and keeps the family's @covers claims on a
;; focused real-boundary slice:
;;
;;   * the REAL lost-author fixture world: a genuine owned full Verify (the
;;     frozen `raco test tests/probe.rkt && raco test tests/second.rkt`
;;     command through the real verification registry) produces the old
;;     receipt, and a durable committed successor awaits the owner finish;
;;   * the flagship BUG-0086 claim: /go finishes the committed successor with
;;     an owned full Verify proof, routes the fresh-review admission through
;;     the REAL production delivery controller (blocked until an independent
;;     reviewer commits), pins the receipt head, publication and ls-remote
;;     branch identity to the exact successor, stays review-only on repeat,
;;     and replays the exact proof after a publication crash — the `exit 99`
;;     override proves no second Verify ever runs;
;;   * refusal truth with real command exit-truth: a passing Verify override
;;     (`exit 0`) still cannot mint owned proof, a live session pauses the
;;     coordinator checkpoint, and a drifted successor/fence authority is
;;     refused with the journal byte-for-byte intact;
;;   * genuine RED Verify exit-truth: a committed successor with a failing
;;     test file must refuse with "full frozen Verify failed" while the
;;     running author is retained (and cannot be abandoned out of band).
;;
;; No mock-only security decision: every verdict above flows through the real
;; Git state, real owned-proof registry and real Verify command execution.
(require rackunit
         json
         "helpers/lost-author-fixture.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
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

  (test-case "live session and a passing Verify override are refused without dispatch"
    (with-lost-author (lambda (f)
                        ;; Live author session: the coordinator checkpoint
                        ;; pauses before any launch.
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
                        (check-equal? (journal f) before)
                        ;; A passing Verify override executes for real, but its
                        ;; green exit can never mint an owned proof.
                        (parameterize ([current-gsd-delivery-verify-command "exit 0"])
                          (assert-refused f #rx"lacks authentic owned proof"))))

    (test-case "refuses a committed successor whose full frozen Verify is red"
      (with-lost-author
       (lambda (f)
         (define cwd (lost-author-cwd f))
         (commit! cwd
                  "tests/second.rkt"
                  "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 2))\n")
         (assert-refused f #rx"full frozen Verify failed.*running author retained")
         ;; Still cannot claim no-edit or buy another allowance.
         (check-exn exn:fail?
                    (lambda ()
                      (abandon-evidence-author-session! (lost-author-root f)
                                                        (lost-author-plan f)
                                                        0
                                                        (git-output cwd "rev-parse" "HEAD^{tree}")
                                                        #:guard (lambda () #t))))
         ;; Authority drift: a stale successor/fence is refused before any
         ;; dispatch, journal byte-for-byte intact.
         (define root (lost-author-root f))
         (define plan (lost-author-plan f))
         (define rec (load-campaign-record root plan))
         (define w (car (campaign-record-waves rec)))
         (set-campaign-wave-current-attempt! w (campaign-attempt "attempt-2" 3 0))
         (persist-campaign! root rec)
         (assert-refused f #rx"stale successor/fence"))))))
