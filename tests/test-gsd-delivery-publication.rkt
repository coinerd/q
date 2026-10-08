#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-publication.rkt
;; @covers extensions/gsd/delivery-receipt.rkt
(require json
         racket/file
         racket/list
         racket/port
         racket/string
         racket/runtime-path
         rackunit
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         (only-in "helpers/gsd-golden-trace.rkt" seed-golden-project!)
         (only-in "helpers/honest-delivery-fixture.rkt" test-delivered-reader)
         (only-in "../extensions/gsd/campaign-repository.rkt" load-campaign-record)
         (only-in "../extensions/gsd/delivery-coordinator.rkt"
                  run-delivery-coordinator!
                  delivery-effect-result
                  delivery-outcome-kind)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign!
                  campaign-result-status
                  campaign-result-completed-waves
                  campaign-result-message)
         (only-in "../extensions/gsd/wave-completion.rkt" load-outbox)
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt")

(define-runtime-path publication-module "../extensions/gsd/delivery-publication.rkt")
(define current-gsd-approved-head-publisher
  (dynamic-require publication-module 'current-gsd-approved-head-publisher))
(define approved-publication-path (dynamic-require publication-module 'approved-publication-path))
(define load-approved-publication (dynamic-require publication-module 'load-approved-publication))
(define replay-approved-publication!
  (dynamic-require publication-module 'replay-approved-publication!))
(define approved-publication-branches
  (dynamic-require publication-module 'approved-publication-branches))
(define publish-approved-head! (dynamic-require publication-module 'publish-approved-head!))
(define durable-spare-branches (dynamic-require publication-module 'durable-spare-branches))

(define plan (make-string 64 #\a))
(define identity
  (hasheq 'repo
          "/repo"
          'branch
          "campaign/abc/w0"
          'head
          (make-string 40 #\b)
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"))
(define attempt (campaign-attempt "attempt-1" 7 0))
(define snapshot-digest (make-string 64 #\d))
(define (authorized-record #:fence [fence 9] #:cancelled? [cancelled? #f])
  (define w (make-campaign-wave 0 "delivery" 'verifying 1 attempt))
  (define rec
    (make-campaign-record plan
                          (make-campaign-manifest 1 "delivery" '() '() "constraints")
                          (list w)
                          (and cancelled? (make-campaign-cancellation "stop" 1))
                          fence
                          'test
                          0
                          0))
  (set-campaign-record-plan-snapshot-digest! rec snapshot-digest)
  rec)
(define approved-verification (delivery-verification #t '((verify . (#t . "passed"))) "approved"))

(define (with-root proc)
  (define root (make-temporary-file "delivery-publication-~a" 'directory))
  (dynamic-wind void (lambda () (proc root)) (lambda () (delete-directory/files root))))

(module+ test
  (test-case "approved unpublished Verify publishes exact head before recording receipt"
    (with-root
     (lambda (root)
       (define calls '())
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (base plan-id wave snapshot attempt-id attempt-fence coordinator-fence evidence)
               (set! calls
                     (cons (list plan-id
                                 wave
                                 (hash-ref snapshot 'branch)
                                 (hash-ref snapshot 'head)
                                 attempt-id
                                 attempt-fence
                                 coordinator-fence
                                 evidence)
                           calls))
               (hasheq 'status
                       "published"
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (define remote-seen? #f)
         (define result
           (verify-with-delivery-receipt root
                                         plan
                                         0
                                         root
                                         (lambda () approved-verification)
                                         #:approved? (lambda (v)
                                                       (and (delivery-verification? v)
                                                            (delivery-verification-approved? v)))
                                         #:evidence delivery-verification-evidence
                                         #:attempt attempt
                                         #:current-record (lambda () (authorized-record))
                                         #:coordinator-fence 9
                                         #:publish-approved-head publish-approved-head!
                                         #:remote-published (lambda (_repo _branch _head)
                                                              (begin0 remote-seen?
                                                                (set! remote-seen? #t)))
                                         #:snapshot (lambda (_) identity)))
         (check-equal? result approved-verification)
         (check-equal? (length calls) 1)
         (check-equal? (car calls)
                       (list plan
                             0
                             "campaign/abc/w0"
                             (make-string 40 #\b)
                             "attempt-1"
                             7
                             9
                             "((verify #t . passed))"))
         (define receipt (hash-ref (load-delivery-journal root plan 0) 'receipt))
         (check-equal? (hash-ref receipt 'head) (hash-ref identity 'head))
         (check-equal? (hash-ref receipt 'attempt-id) "attempt-1")
         (check-false (load-remote-pending root plan 0))
         (define publication (load-approved-publication root plan 0))
         (check-equal? (hash-ref publication 'status) "confirmed")
         (check-equal? (hash-ref publication 'head) (hash-ref identity 'head))))))

  (test-case "publication refusal leaves receipt absent and remote-pending typed marker durable"
    (with-root (lambda (root)
                 (parameterize ([current-gsd-approved-head-publisher
                                 (lambda (_base
                                          _plan
                                          _wave
                                          _snapshot
                                          _attempt-id
                                          _attempt-fence
                                          _coordinator-fence
                                          _evidence)
                                   (hasheq 'status "blocked" 'reason "remote diverged"))])
                   (verify-with-delivery-receipt
                    root
                    plan
                    0
                    root
                    (lambda () approved-verification)
                    #:approved?
                    (lambda (v) (and (delivery-verification? v) (delivery-verification-approved? v)))
                    #:evidence delivery-verification-evidence
                    #:attempt attempt
                    #:current-record (lambda () (authorized-record))
                    #:coordinator-fence 9
                    #:publish-approved-head publish-approved-head!
                    #:remote-published (lambda (_repo _branch _head) #f)
                    #:snapshot (lambda (_) identity))
                   (check-false (load-delivery-journal root plan 0))
                   (define marker (load-remote-pending root plan 0))
                   (check-true (hash? marker))
                   (check-true (regexp-match? #rx"publication refused" (hash-ref marker 'reason)))))))

  (test-case "legacy under-bound remote-pending marker alone does not authorize publication"
    (with-root (lambda (root)
                 (record-remote-pending! root
                                         plan
                                         0
                                         "campaign/abc/w0"
                                         (make-string 40 #\b)
                                         "legacy unpublished")
                 (check-equal? (remote-pending-blocker root plan 0) 'branch-not-published)
                 (check-false (load-approved-publication root plan 0)))))

  (test-case "confirmed replay performs authenticated readback instead of trusting cache"
    (with-root
     (lambda (root)
       (define calls 0)
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
               (set! calls (add1 calls))
               (hasheq 'status
                       (if (= calls 1) "published" "already-published")
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (publish-approved-head! root
                                 plan
                                 0
                                 identity
                                 "attempt-1"
                                 7
                                 9
                                 "approved evidence"
                                 (lambda () (authorized-record)))
         (check-equal? calls 1)
         (publish-approved-head! root
                                 plan
                                 0
                                 identity
                                 "attempt-1"
                                 7
                                 10
                                 "approved evidence"
                                 (lambda () (authorized-record #:fence 10)))
         (check-equal? calls 2)
         (check-equal? (hash-ref (load-approved-publication root plan 0) 'status) "confirmed")))))

  (test-case "cancellation or active-fence takeover during publication prevents confirmation and receipt"
    (with-root
     (lambda (root)
       (define cancelled? #f)
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
               (set! cancelled? #t)
               (hasheq 'status
                       "published"
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (check-exn #rx"authorization refused"
                    (lambda ()
                      (publish-approved-head! root
                                              plan
                                              0
                                              identity
                                              "attempt-1"
                                              7
                                              9
                                              "approved evidence"
                                              (lambda ()
                                                (authorized-record #:cancelled? cancelled?)))))
         (check-false (load-delivery-journal root plan 0))
         (check-equal? (hash-ref (load-approved-publication root plan 0) 'status) "intent"))))
    (with-root
     (lambda (root)
       (define fence 9)
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
               (set! fence 10)
               (hasheq 'status
                       "published"
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (check-exn #rx"authorization refused"
                    (lambda ()
                      (publish-approved-head! root
                                              plan
                                              0
                                              identity
                                              "attempt-1"
                                              7
                                              9
                                              "approved evidence"
                                              (lambda () (authorized-record #:fence fence)))))
         (check-equal? (hash-ref (load-approved-publication root plan 0) 'status) "intent")))))

  (test-case "publication-only replay certifies persisted receipt candidate and clears pending marker"
    (with-root
     (lambda (root)
       (define calls 0)
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
               (set! calls (add1 calls))
               (hasheq 'status
                       (if (= calls 1) "published" "already-published")
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (publish-approved-head! root
                                 plan
                                 0
                                 identity
                                 "attempt-1"
                                 7
                                 9
                                 "approved evidence"
                                 (lambda () (authorized-record)))
         (record-remote-pending! root
                                 plan
                                 0
                                 (hash-ref identity 'branch)
                                 (hash-ref identity 'head)
                                 "crashed before receipt")
         (check-false (load-delivery-journal root plan 0))
         (replay-approved-publication! root plan 0 (lambda () (authorized-record)))
         (check-equal? calls 2)
         (check-false (load-remote-pending root plan 0))
         (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 0) 'receipt) 'head)
                       (hash-ref identity 'head))))))

  (test-case "changed frozen plan digest refuses stale publication replay"
    (with-root
     (lambda (root)
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
               (hasheq 'status
                       "published"
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (publish-approved-head! root
                                 plan
                                 0
                                 identity
                                 "attempt-1"
                                 7
                                 9
                                 "approved evidence"
                                 (lambda () (authorized-record)))
         (check-exn #rx"identity changed|authorization refused"
                    (lambda ()
                      (publish-approved-head!
                       root
                       plan
                       0
                       identity
                       "attempt-1"
                       7
                       9
                       "approved evidence"
                       (lambda ()
                         (define rec (authorized-record))
                         (set-campaign-record-plan-snapshot-digest! rec (make-string 64 #\e))
                         rec))))))))

  (test-case "symbol or bool approval cannot authorize publication side effect"
    (with-root
     (lambda (root)
       (define calls 0)
       (parameterize ([current-gsd-approved-head-publisher (lambda args
                                                             (set! calls (add1 calls))
                                                             (hasheq 'status
                                                                     "published"
                                                                     'branch
                                                                     (hash-ref identity 'branch)
                                                                     'head
                                                                     (hash-ref identity 'head)))])
         (verify-with-delivery-receipt root
                                       plan
                                       0
                                       root
                                       (lambda () 'approved)
                                       #:approved? (lambda (v) (eq? v 'approved))
                                       #:evidence (lambda (_) "bool fallback")
                                       #:attempt attempt
                                       #:current-record (lambda () (authorized-record))
                                       #:coordinator-fence 9
                                       #:publish-approved-head publish-approved-head!
                                       #:remote-published (lambda (_repo _branch _head) #f)
                                       #:snapshot (lambda (_) identity))
         (check-equal? calls 0)
         (check-false (load-delivery-journal root plan 0))
         (check-true (hash? (load-remote-pending root plan 0)))))))

  (test-case "active frozen plan digest drift during publication refuses confirmation"
    (with-root
     (lambda (root)
       (define digest snapshot-digest)
       (parameterize
           ([current-gsd-approved-head-publisher
             (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
               (set! digest (make-string 64 #\e))
               (hasheq 'status
                       "published"
                       'branch
                       (hash-ref snapshot 'branch)
                       'head
                       (hash-ref snapshot 'head)))])
         (check-exn #rx"authorization refused"
                    (lambda ()
                      (publish-approved-head! root
                                              plan
                                              0
                                              identity
                                              "attempt-1"
                                              7
                                              9
                                              "approved evidence"
                                              (lambda ()
                                                (define rec (authorized-record))
                                                (set-campaign-record-plan-snapshot-digest! rec digest)
                                                rec)))))
       (check-equal? (hash-ref (load-approved-publication root plan 0) 'status) "intent")))))

(test-case "replay final receipt certification refuses later active-fence takeover"
  (with-root
   (lambda (root)
     (define calls 0)
     (parameterize ([current-gsd-approved-head-publisher
                     (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
                       (hasheq 'status
                               "published"
                               'branch
                               (hash-ref snapshot 'branch)
                               'head
                               (hash-ref snapshot 'head)))])
       (publish-approved-head! root
                               plan
                               0
                               identity
                               "attempt-1"
                               7
                               9
                               "approved evidence"
                               (lambda () (authorized-record)))
       (check-exn #rx"authorization refused"
                  (lambda ()
                    (replay-approved-publication! root
                                                  plan
                                                  0
                                                  (lambda ()
                                                    (set! calls (add1 calls))
                                                    (authorized-record #:fence
                                                                       (if (<= calls 4) 9 10)))))))
     (check-false (load-delivery-journal root plan 0)))))

(test-case "corrupt existing publication file is rejected and never overwritten"
  (with-root
   (lambda (root)
     (define path (approved-publication-path root plan 0))
     (make-parent-directory* path)
     (call-with-output-file path #:exists 'truncate (lambda (out) (display "{not-json" out)))
     (check-exn #rx"invalid|refusing|corrupt"
                (lambda ()
                  (publish-approved-head! root
                                          plan
                                          0
                                          identity
                                          "attempt-1"
                                          7
                                          9
                                          "approved evidence"
                                          (lambda () (authorized-record)))))
     (check-equal? (file->string path) "{not-json"))))

(test-case "publication loader rejects receipt candidate inconsistent with top-level identity"
  (with-root
   (lambda (root)
     (parameterize ([current-gsd-approved-head-publisher
                     (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
                       (hasheq 'status
                               "published"
                               'branch
                               (hash-ref snapshot 'branch)
                               'head
                               (hash-ref snapshot 'head)))])
       (publish-approved-head! root
                               plan
                               0
                               identity
                               "attempt-1"
                               7
                               9
                               "approved evidence"
                               (lambda () (authorized-record)))
       (define path (approved-publication-path root plan 0))
       (define data (call-with-input-file path read-json))
       (call-with-output-file path
                              #:exists 'truncate
                              (lambda (out)
                                (write-json (hash-set data
                                                      'receipt-candidate
                                                      (hash-set (hash-ref data 'receipt-candidate)
                                                                'head
                                                                (make-string 40 #\e)))
                                            out)))
       (check-exn #rx"receipt candidate|invalid|inconsistent"
                  (lambda () (load-approved-publication root plan 0)))))))

(test-case "empty approved Verify evidence cannot authorize publication"
  (with-root
   (lambda (root)
     (define calls 0)
     (parameterize ([current-gsd-approved-head-publisher (lambda args
                                                           (set! calls (add1 calls))
                                                           (hasheq 'status
                                                                   "published"
                                                                   'branch
                                                                   (hash-ref identity 'branch)
                                                                   'head
                                                                   (hash-ref identity 'head)))])
       (verify-with-delivery-receipt
        root
        plan
        0
        root
        (lambda () (delivery-verification #t '() "empty"))
        #:approved? (lambda (v) (and (delivery-verification? v) (delivery-verification-approved? v)))
        #:evidence delivery-verification-evidence
        #:attempt attempt
        #:current-record (lambda () (authorized-record))
        #:coordinator-fence 9
        #:publish-approved-head publish-approved-head!
        #:remote-published (lambda (_repo _branch _head) #f)
        #:snapshot (lambda (_) identity))
       (check-equal? calls 0)
       (check-false (load-delivery-journal root plan 0))
       (check-true (hash? (load-remote-pending root plan 0)))))))

(test-case "approved publication branches are protected for reclaim"
  (with-root
   (lambda (root)
     (parameterize ([current-gsd-approved-head-publisher
                     (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
                       (hasheq 'status
                               "published"
                               'branch
                               (hash-ref snapshot 'branch)
                               'head
                               (hash-ref snapshot 'head)))])
       (publish-approved-head! root
                               plan
                               0
                               identity
                               "attempt-1"
                               7
                               9
                               "approved evidence"
                               (lambda () (authorized-record)))
       (check-equal? (approved-publication-branches root plan) (list "campaign/abc/w0"))))))

;; REVIEW-2 item 5: a strict-descendant SAME-attempt repair-tail fresh
;; approval must be authorizable while the durable record still binds the
;; older receipt head — proven ancestry over real git, prior publication
;; history preserved; anything not a proven descendant still refuses.
(test-case "same-attempt repair-tail fresh approval publishes while the record binds the older head"
  (define root (make-temporary-file "publication-repair-tail-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define repo (build-path root "q"))
     (make-directory repo)
     (git-quiet! repo "init" "-q" "-b" "main")
     (hermetic-identity! repo)
     (git-quiet! repo "commit" "--allow-empty" "-qm" "baseline")
     (define branch (format "campaign/~a/w0" (substring plan 0 8)))
     (git-quiet! repo "checkout" "-qb" branch)
     (git-quiet! repo "commit" "--allow-empty" "-qm" "approved")
     (define (git-out . args)
       (string-trim (with-output-to-string (lambda () (apply git-quiet! repo args)))))
     (define old-head (git-out "rev-parse" "HEAD"))
     (git-quiet! repo "commit" "--allow-empty" "-qm" "repair tail")
     (define new-head (git-out "rev-parse" "HEAD"))
     (define (snapshot-for head)
       (hasheq 'repo
               (path->string repo)
               'branch
               branch
               'head
               head
               'tree
               (git-out "rev-parse" (string-append head "^{tree}"))
               'origin
               "https://github.com/example/q.git"))
     (define (bound-record)
       (define w (make-campaign-wave 0 "delivery" 'verifying 1 attempt))
       (set-campaign-wave-delivery-branch! w branch)
       (set-campaign-wave-delivery-head-sha! w old-head)
       (define rec
         (make-campaign-record plan
                               (make-campaign-manifest 1 "delivery" '() '() "constraints")
                               (list w)
                               #f
                               9
                               'test
                               0
                               0))
       (set-campaign-record-plan-snapshot-digest! rec snapshot-digest)
       rec)
     (define calls 0)
     (parameterize ([current-gsd-approved-head-publisher
                     (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
                       (set! calls (add1 calls))
                       (hasheq 'status
                               "published"
                               'branch
                               (hash-ref snapshot 'branch)
                               'head
                               (hash-ref snapshot 'head)))])
       ;; The approved head publishes while the record binds it...
       (publish-approved-head! root
                               plan
                               0
                               (snapshot-for old-head)
                               "attempt-1"
                               7
                               9
                               "approved evidence"
                               (lambda () (bound-record)))
       (check-equal? calls 1)
       ;; ...then a same-attempt repair tail advances the branch: the fresh
       ;; approval at the descendant head must be authorizable even though
       ;; the durable record still binds the older head.
       (publish-approved-head! root
                               plan
                               0
                               (snapshot-for new-head)
                               "attempt-1"
                               7
                               9
                               "approved evidence"
                               (lambda () (bound-record)))
       (check-equal? calls 2)
       (define publication (load-approved-publication root plan 0))
       (check-equal? (hash-ref publication 'status) "confirmed")
       (check-equal? (hash-ref publication 'head) new-head)
       (define history (hash-ref publication 'publication-history))
       (check-true (and (pair? history) (equal? (hash-ref (car history) 'head) old-head)))
       ;; A same-branch head that is NOT a strict descendant of the bound
       ;; head is drift, never a repair tail.
       (git-quiet! repo "checkout" "-q" branch)
       (git-quiet! repo "reset" "-q" "--hard" "main")
       (git-quiet! repo "commit" "--allow-empty" "-qm" "divergent")
       (define divergent-head (git-out "rev-parse" "HEAD"))
       (check-exn #rx"head binding drifted"
                  (lambda ()
                    (publish-approved-head! root
                                            plan
                                            0
                                            (snapshot-for divergent-head)
                                            "attempt-1"
                                            7
                                            9
                                            "approved evidence"
                                            (lambda () (bound-record)))))))
   (lambda () (delete-directory/files root #:must-exist? #f))))

;; ============================================================
;; durable-spare-branches — the extracted BUG-0079 reclaim spare
;; computation (REVIEW-2 item 9; moved out of go-orchestrator to
;; hold its size pin).
;; ============================================================

(test-case "durable-spare-branches unions delivered record branches with approved publication branches"
  (with-root
   (lambda (root)
     (parameterize ([current-gsd-approved-head-publisher
                     (lambda (_base _plan _wave snapshot _attempt-id _attempt-fence _fence _evidence)
                       (hasheq 'status
                               "published"
                               'branch
                               (hash-ref snapshot 'branch)
                               'head
                               (hash-ref snapshot 'head)))])
       (publish-approved-head! root
                               plan
                               0
                               identity
                               "attempt-1"
                               7
                               9
                               "approved evidence"
                               (lambda () (authorized-record))))
     (check-equal? (durable-spare-branches root (authorized-record))
                   '("campaign/abc/w0")
                   "approved publication branch is spared")
     (define w0 (make-campaign-wave 0 "delivery" 'verifying 1 attempt))
     (define w1 (make-campaign-wave 1 "delivery" 'done 1 attempt))
     (set-campaign-wave-delivery-branch! w1 "campaign/abc/w1")
     (define rec
       (make-campaign-record plan
                             (make-campaign-manifest 1 "delivery" '() '() "constraints")
                             (list w0 w1)
                             #f
                             9
                             'test
                             0
                             0))
     (set-campaign-record-plan-snapshot-digest! rec snapshot-digest)
     (check-equal? (sort (durable-spare-branches root rec) string<?)
                   '("campaign/abc/w0" "campaign/abc/w1")
                   "delivered record branches AND publication branches are spared"))))

;; ============================================================
;; BUG-0079 fake-port E2E — the entire approved-head publication
;; chain in ONE test: owned Verify approval → durable publication
;; intent → non-force publish effect (REAL git against a local
;; origin stand-in; the publisher port is the ONLY pusher) →
;; authenticated exact readback → receipt certification → the real
;; protected-delivery ladder machine → post-DONE tracker
;; reconciliation checkpoint → DONE → successor wave selection.
;; Positive tracker-EFFECT coverage is composed with
;; test-gsd-tracker-reconciliation.rkt; this E2E covers the DONE-gated
;; wiring (not-configured → zero tracker effects, never blocking DONE).
;; No manual DONE stamping and no operator push: the runner/verifier
;; drive the campaign; the only injected proof is the established
;; authenticated-readback reader seam (test-delivered-reader).
;; ============================================================

(define e2e-origin-url "https://github.com/example/q.git")
(define e2e-branch "campaign/bug0079-e2e")
(define expected-ladder
  '("delivery-preflight" "implementation-review"
                         "implementation-pr"
                         "implementation-ci"
                         "implementation-merged"
                         "binding-prepared"
                         "binding-review"
                         "binding-pr"
                         "binding-ci"
                         "binding-merged"
                         "governance"
                         "sync"
                         "delivered"))

(define (git-out repo . args)
  (string-trim (with-output-to-string (lambda () (apply git-quiet! repo args)))))

;; Real git, zero network: the configured origin URL is github-shaped (so
;; the committed-snapshot identity check accepts it) while
;; url.<bare>.insteadOf routes every push/ls-remote to a local bare repo.
(define (make-e2e-project specs)
  (define host (make-temporary-file "bug0079-e2e-~a" 'directory))
  (define repo (build-path host "q"))
  (define origin (build-path host "origin.git"))
  (make-directory repo)
  (git-quiet! host "init" "-q" "--bare" (path->string origin))
  (git-quiet! repo "init" "-q" "-b" "main")
  (hermetic-identity! repo)
  (git-quiet! repo "config" "remote.origin.url" e2e-origin-url)
  (git-quiet! repo "config" (string-append "url." (path->string origin) ".insteadOf") e2e-origin-url)
  (call-with-output-file (build-path repo ".gitignore") (lambda (out) (displayln ".planning/" out)))
  (git-quiet! repo "add" ".gitignore")
  (git-quiet! repo "commit" "-q" "-m" "fixture baseline")
  (git-quiet! repo "checkout" "-q" "-b" e2e-branch)
  (seed-golden-project! repo specs)
  (values repo host))

;; The fake publisher port: witnesses the durable intent BEFORE the effect,
;; performs a REAL non-force (fast-forward-only) push, then authenticates
;; the exact head via readback. Any refusal path performs NO push.
(define (make-fake-port-publisher repo effects-box)
  (lambda (base plan-id wave snapshot attempt-id attempt-fence coordinator-fence evidence)
    (define branch (hash-ref snapshot 'branch))
    (define head (hash-ref snapshot 'head))
    (with-handlers ([exn:fail? (lambda (e) (hasheq 'status "blocked" 'reason (exn-message e)))])
      (define intent (load-approved-publication base plan-id wave))
      (unless (and intent
                   (equal? (hash-ref intent 'status) "intent")
                   (equal? (hash-ref intent 'head) head)
                   (equal? (hash-ref intent 'branch) branch))
        (error 'fake-port-publisher "durable publication intent missing before the effect"))
      (git-quiet! repo "push" "-q" "origin" (string-append branch ":refs/heads/" branch))
      (define remote (git-out repo "ls-remote" "origin" (string-append "refs/heads/" branch)))
      (unless (and (>= (string-length remote) 40) (equal? (substring remote 0 40) head))
        (error 'fake-port-publisher "authenticated exact readback mismatch"))
      (set-box! effects-box (cons (hasheq 'branch branch 'head head) (unbox effects-box)))
      (hasheq 'status "published" 'branch branch 'head head))))

(define (refusing-publisher _base _plan _wave _snapshot _attempt-id _attempt-fence _fence _evidence)
  (hasheq 'status "blocked" 'reason "remote diverged"))

;; Run proc with a child logger and return (list outcome log-lines) so the
;; post-DONE tracker checkpoint (and other durable witnesses) are observable.
(define (with-campaign-logs proc)
  (define logger (make-logger #f (current-logger)))
  (define receiver (make-log-receiver logger 'info))
  (define outcome
    (parameterize ([current-logger logger])
      (proc)))
  (define log-lines
    (let drain ([acc '()])
      (define v (sync/timeout 0.1 receiver))
      (if v
          (drain (cons (format "~a" (vector-ref v 1)) acc))
          (reverse acc))))
  (list outcome log-lines))

(define (e2e-runner repo runner-calls)
  (lambda (wave-idx)
    (set-box! runner-calls (cons wave-idx (unbox runner-calls)))
    (git-quiet! repo
                "commit"
                "-q"
                "--allow-empty"
                "--no-gpg-sign"
                "-m"
                (format "w~a implementation" wave-idx))
    'ok))

(define (e2e-verifier)
  (lambda (wave-idx)
    (delivery-verification
     #t
     (list (cons 'verify (cons #t (format "raco test . wave ~a: all checks passed" wave-idx))))
     (format "wave ~a successful Verify evidence: command/exit/log attached" wave-idx))))

(define (e2e-controller ladder-effects)
  (lambda (base plan wave target)
    (set-box! ladder-effects (cons (cons wave target) (unbox ladder-effects)))
    (delivery-effect-result 'ok (hasheq 'stage target))))

(module+ test
  (test-case "BUG-0079 E2E: owned approval publishes non-forced, certifies, ladders, reconciles tracker, DONE, successor runs"
    (define specs '((0 "E2E Wave Alpha" "alpha") (1 "E2E Wave Beta" "beta")))
    (define-values (repo host) (make-e2e-project specs))
    (dynamic-wind
     void
     (lambda ()
       (define rec (migrate-campaign! repo))
       (define plan-id (campaign-plan-id rec))
       (define publish-effects (box '()))
       (define ladder-effects (box '()))
       (define runner-calls (box '()))
       (define head-before (git-out repo "rev-parse" "HEAD"))
       (define result
         (with-campaign-logs (lambda ()
                               (parameterize ([current-gsd-approved-head-publisher
                                               (make-fake-port-publisher repo publish-effects)])
                                 (run-campaign! repo
                                                rec
                                                #:runner (e2e-runner repo runner-calls)
                                                #:verifier (e2e-verifier)
                                                #:isolate? #f
                                                #:delivery-reader (test-delivered-reader)
                                                #:delivery-coordinator
                                                (lambda (base plan idx)
                                                  (run-delivery-coordinator!
                                                   base
                                                   plan
                                                   idx
                                                   #:controller (e2e-controller ladder-effects))))))))
       (define outcome (first result))
       (define log-lines (second result))
       ;; The full chain reached DONE with successor selection and NO manual
       ;; stamping or operator push.
       (check-eq? (campaign-result-status outcome) 'campaign-complete)
       (check-equal? (campaign-result-completed-waves outcome) '(0 1))
       (check-equal? (reverse (unbox runner-calls))
                     '(0 1)
                     "successor wave selected only after wave-0 DONE")
       ;; Non-force publish effect + authenticated exact readback: two pushes
       ;; at two DISTINCT heads (the second is a genuine fast-forward).
       (define effects (reverse (unbox publish-effects)))
       (check-equal? (length effects) 2)
       (check-true (andmap (lambda (e) (equal? (hash-ref e 'branch) e2e-branch)) effects))
       (check-false (equal? (hash-ref (first effects) 'head) (hash-ref (second effects) 'head)))
       (define final-head (git-out repo "rev-parse" "HEAD"))
       (check-equal? (hash-ref (second effects) 'head) final-head)
       (define remote (git-out repo "ls-remote" "origin" (string-append "refs/heads/" e2e-branch)))
       (check-true (string-prefix? remote final-head) "origin carries the exact published head")
       (check-false (string-prefix? remote head-before) "origin started without the branch head")
       (check-equal? (length (string-split remote "\n")) 1 "exactly one published ref")
       ;; Durable publication intent → confirmed for BOTH waves.
       (for ([wave (in-list '(0 1))])
         (define publication (load-approved-publication repo plan-id wave))
         (check-equal? (hash-ref publication 'status) "confirmed")
         (check-equal? (hash-ref publication 'branch) e2e-branch))
       ;; Receipt certification: attempt-bound receipts, terminal ladder stage.
       (for ([wave (in-list '(0 1))])
         (define journal (load-delivery-journal repo plan-id wave))
         (define receipt (and journal (hash-ref journal 'receipt #f)))
         (check-true (hash? receipt) (format "wave ~a receipt certified" wave))
         (check-equal? (hash-ref journal 'stage) "delivered")
         (check-equal? (hash-ref receipt 'branch) e2e-branch)
         (check-equal? (hash-ref receipt 'attempt-id) "attempt-1")
         (check-true (non-empty-string? (hash-ref receipt 'evidence #f))))
       ;; The real ladder machine walked both waves preflight → delivered.
       (check-equal? (reverse (unbox ladder-effects))
                     (append (for/list ([t expected-ladder])
                               (cons 0 t))
                             (for/list ([t expected-ladder])
                               (cons 1 t))))
       ;; DONE is durable: both waves done, derived completion outbox, and the
       ;; post-DONE tracker reconciliation checkpoint ran as an OUTPUT of
       ;; delivery with zero effects (not-configured), never blocking DONE.
       (define durable (load-campaign-record repo plan-id))
       (for ([w (campaign-record-waves durable)])
         (check-eq? (campaign-wave-status w) 'done))
       (check-equal? (length (load-outbox repo plan-id)) 2)
       (check-equal? (count (lambda (l) (string-contains? l "tracker reconciliation not-configured"))
                            log-lines)
                     2)
       (check-false (findf (lambda (l) (string-contains? l "tracker reconciliation blocked"))
                           log-lines)))
     (lambda () (delete-directory/files host #:must-exist? #f))))

  (test-case "BUG-0079 E2E negative: publication refusal leaves zero PR/ladder/tracker/DONE effects"
    (define specs '((0 "E2E Wave Alpha" "alpha") (1 "E2E Wave Beta" "beta")))
    (define-values (repo host) (make-e2e-project specs))
    (dynamic-wind
     void
     (lambda ()
       (define rec (migrate-campaign! repo))
       (define plan-id (campaign-plan-id rec))
       (define publish-effects (box '()))
       (define ladder-effects (box '()))
       (define runner-calls (box '()))
       (define result
         (with-campaign-logs
          (lambda ()
            (parameterize ([current-gsd-approved-head-publisher refusing-publisher])
              (run-campaign! repo
                             rec
                             #:runner (e2e-runner repo runner-calls)
                             #:verifier (e2e-verifier)
                             #:isolate? #f
                             #:delivery-reader (test-delivered-reader)
                             #:delivery-coordinator
                             (lambda (base plan idx)
                               (run-delivery-coordinator! base
                                                          plan
                                                          idx
                                                          #:controller
                                                          (e2e-controller ladder-effects))))))))
       (define outcome (first result))
       (define log-lines (second result))
       ;; The refusal blocks the campaign with the typed reason.
       (check-eq? (campaign-result-status outcome) 'wave-blocked)
       (check-true (string-contains? (campaign-result-message outcome) "branch-not-published"))
       ;; Zero effects: no push, no ladder/PR action, no successor run.
       (check-equal? (unbox publish-effects) '() "the publisher port pushed nothing")
       (check-equal? (unbox ladder-effects) '() "no ladder/PR effect ever fired")
       (check-equal? (reverse (unbox runner-calls)) '(0) "the successor wave never ran")
       (check-equal? (git-out repo "ls-remote" "origin") "" "origin stays untouched")
       ;; The refusal leaves the durable INTENT (never confirmed) and the
       ;; typed remote-pending marker — but NO receipt, NO DONE, NO outbox,
       ;; and NO tracker step (tracker is an output of DONE only).
       (define publication (load-approved-publication repo plan-id 0))
       (check-equal? (hash-ref publication 'status) "intent")
       (define marker (load-remote-pending repo plan-id 0))
       (check-true (hash? marker))
       (check-true (regexp-match? #rx"publication refused" (hash-ref marker 'reason)))
       (check-false (load-delivery-journal repo plan-id 0))
       (define durable (load-campaign-record repo plan-id))
       (check-eq? (campaign-wave-status (first (campaign-record-waves durable))) 'awaiting-delivery)
       (check-equal? (length (load-outbox repo plan-id)) 0)
       (check-false (findf (lambda (l) (string-contains? l "tracker reconciliation")) log-lines)
                    "no DONE → the tracker checkpoint never ran → zero tracker effects"))
     (lambda () (delete-directory/files host #:must-exist? #f)))))
