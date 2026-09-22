#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-receipt.rkt
(require rackunit
         racket/file
         racket/path
         racket/string
         racket/port
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         racket/runtime-path
         "../extensions/gsd/campaign-state.rkt")
(define-runtime-path journal-module "../extensions/gsd/delivery-journal.rkt")
(define-runtime-path receipt-module "../extensions/gsd/delivery-receipt.rkt")
(define plan (make-string 64 #\a))

;; W3 exports under test (red-first: dynamic until the modules provide them).
(define load-remote-pending (dynamic-require journal-module 'load-remote-pending))
(define remote-pending-blocker (dynamic-require journal-module 'remote-pending-blocker))
(define default-remote-published? (dynamic-require receipt-module 'default-remote-published?))
(define identity
  (hasheq 'repo
          "/repo"
          'branch
          "campaign/w2"
          'head
          (make-string 40 #\b)
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"))
(define (with-root f)
  (define root (make-temporary-file "delivery-receipt-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))
(define (done-record)
  (define w (make-campaign-wave 2 "delivery" 'done 1 (campaign-attempt "attempt-1" 7 0)))
  (make-campaign-record plan
                        (make-campaign-manifest 1 "delivery" '() '() "constraints")
                        (list w)
                        #f
                        8
                        'test
                        0
                        0))
(define receipt
  (hash-set* identity
             'verified-at
             0
             'evidence
             "Verify passed"
             'attempt-id
             "attempt-1"
             'attempt-fence
             7))
(define journal (hasheq 'schema-version 1 'plan-id plan 'wave 2 'receipt receipt))
(module+ test
  (test-case "publication eligibility requires exact DONE attempt, not merely successful Verify"
    (define blocker (dynamic-require receipt-module 'delivery-receipt-blocker))
    (define rec (done-record))
    (check-false (blocker journal rec plan 2 8))
    ;; Resuming increments coordinator fence without rewriting the old attempt.
    (set-campaign-fence-token! rec 9)
    (check-false (blocker journal rec plan 2 9))
    (check-not-false (blocker journal rec plan 2 8))
    (for ([status '(running verifying pending failed interrupted)])
      (set-campaign-wave-status! (car (campaign-record-waves rec)) status)
      (check-not-false (blocker journal rec plan 2 9)))
    (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
    (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))
    (check-not-false (blocker journal rec plan 2 9)))
  (test-case "missing, stale, mismatched, and legacy provenance cannot authorize publication"
    (define blocker (dynamic-require receipt-module 'delivery-receipt-blocker))
    (define rec (done-record))
    (define w (car (campaign-record-waves rec)))
    (check-not-false (blocker journal #f plan 2 8))
    (check-not-false (blocker #f rec plan 2 8))
    (check-not-false (blocker journal rec (make-string 64 #\f) 2 8))
    (check-not-false (blocker journal rec plan 3 8))
    (for ([bad (list (hash-remove receipt 'attempt-id)
                     (hash-remove receipt 'attempt-fence)
                     (hash-set receipt 'attempt-id "attempt-2")
                     (hash-set receipt 'attempt-fence 6)
                     (hash-set receipt 'branch "main")
                     (hash-set receipt 'head "short"))])
      (check-not-false (blocker (hash-set journal 'receipt bad) rec plan 2 8)))
    (set-campaign-wave-delivery-branch! w "other-branch")
    (check-not-false (blocker journal rec plan 2 8))
    (set-campaign-wave-delivery-branch! w (hash-ref receipt 'branch))
    (set-campaign-wave-delivery-head-sha! w (make-string 40 #\d))
    (check-not-false (blocker journal rec plan 2 8))
    (set-campaign-wave-delivery-head-sha! w (hash-ref receipt 'head))
    (check-false (blocker journal rec plan 2 8))
    (set-campaign-wave-current-attempt! w (campaign-attempt "replacement" 8 0))
    (check-not-false (blocker journal rec plan 2 8)))
  (test-case "isolated receipt names surviving object repository, not disposable worktree"
    (with-root
     (lambda (root)
       (define repo (build-path root "q"))
       (define wt (build-path root "worker"))
       (make-directory repo)
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (git-quiet! repo "commit" "--allow-empty" "-qm" "baseline")
       (git-quiet! repo "worktree" "add" "-qb" "campaign/w2" (path->string wt))
       (define snapshot (committed-delivery-snapshot wt))
       (check-true (hash? snapshot))
       (check-equal? (string->path (hash-ref snapshot 'repo)) (path->directory-path repo))
       (for ([kind '(approved cancelled replaced takeover disappeared)])
         (with-root
          (lambda (campaign-root)
            (define rec (done-record))
            (set-campaign-fence-token! rec 7)
            (set-campaign-wave-status! (car (campaign-record-waves rec)) 'running)
            (check-true
             (verify-campaign-delivery
              campaign-root
              plan
              2
              wt
              #f
              (lambda (_)
                (case kind
                  [(takeover) (set-campaign-fence-token! rec 8)]
                  [(disappeared) (set! rec #f)]
                  [(cancelled) (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))]
                  [(replaced)
                   (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                                       (campaign-attempt "replacement" 7 0))])
                #t)
              (lambda () rec)
              #:remote-published (lambda (_repo _branch _head) #t)))
            (define saved (load-delivery-journal campaign-root plan 2))
            (if (eq? kind 'approved)
                (begin
                  (check-equal? (hash-ref (hash-ref saved 'receipt) 'attempt-id #f) "attempt-1")
                  (check-equal? (hash-ref (hash-ref saved 'receipt) 'attempt-fence #f) 7))
                (check-false saved)))))
       (git-quiet! repo "worktree" "remove" (path->string wt))
       (check-not-false
        (recover-legacy-delivery-receipt! root plan 2 "campaign/w2" (hash-ref snapshot 'head) repo))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 2) 'receipt) 'head)
                     (hash-ref snapshot 'head))
       (check-false
        (recover-legacy-delivery-receipt! root plan 3 "campaign/w2" (make-string 40 #\f) repo)))))
  (test-case "only stable clean identity across successful Verify becomes a receipt"
    (with-root (lambda (root)
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "full Verify log")
                                                 #:remote-published (lambda (_repo _branch _head) #t)
                                                 #:snapshot (lambda (_) identity)))
                 (check-eq? result 'approved)
                 (define receipt (hash-ref (load-delivery-journal root plan 2) 'receipt))
                 (check-equal? (hash-ref receipt 'head) (hash-ref identity 'head))
                 (check-equal? (hash-ref receipt 'evidence) "full Verify log"))))
  (test-case "dirty/missing identity, head changes, and failed Verify create no receipt"
    (for ([kind '(dirty changed failed)])
      (with-root
       (lambda (root)
         (define count 0)
         (verify-with-delivery-receipt
          root
          plan
          2
          root
          (lambda () (not (eq? kind 'failed)))
          #:approved? values
          #:evidence (lambda (_) "result")
          #:remote-published (lambda (_repo _branch _head) #t)
          #:snapshot
          (lambda (_)
            (set! count (add1 count))
            (case kind
              [(dirty) #f]
              [(changed) (hash-set identity 'head (make-string 40 (if (= count 1) #\b #\d)))]
              [else identity])))
         (check-false (load-delivery-journal root plan 2))))))
  (test-case "F5: an unpublished branch records remote-pending, never a verified receipt"
    (with-root (lambda (root)
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "full Verify log")
                                                 #:remote-published (lambda (_repo _branch _head) #f)
                                                 #:snapshot (lambda (_) identity)))
                 ;; Verify's own verdict is untouched; only the receipt is withheld.
                 (check-eq? result 'approved)
                 (check-false (load-delivery-journal root plan 2)
                              "an unpublished branch must not produce a verified receipt")
                 (define marker (load-remote-pending root plan 2))
                 (check-true (hash? marker) "the remote-pending state is recorded")
                 (check-equal? (hash-ref marker 'branch) (hash-ref identity 'branch))
                 (check-equal? (hash-ref marker 'head) (hash-ref identity 'head))
                 (check-true (string-contains? (hash-ref marker 'reason) "not published")))))

  (test-case "F5: re-verify after publication records the receipt and clears the marker"
    (with-root
     (lambda (root)
       (verify-with-delivery-receipt root
                                     plan
                                     2
                                     root
                                     (lambda () 'approved)
                                     #:approved? (lambda (v) (eq? v 'approved))
                                     #:evidence (lambda (_) "log")
                                     #:remote-published (lambda (_repo _branch _head) #f)
                                     #:snapshot (lambda (_) identity))
       (check-true (hash? (load-remote-pending root plan 2)))
       (verify-with-delivery-receipt root
                                     plan
                                     2
                                     root
                                     (lambda () 'approved)
                                     #:approved? (lambda (v) (eq? v 'approved))
                                     #:evidence (lambda (_) "log")
                                     #:remote-published (lambda (_repo _branch _head) #t)
                                     #:snapshot (lambda (_) identity))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 2) 'receipt) 'head)
                     (hash-ref identity 'head))
       (check-false (load-remote-pending root plan 2) "resolution clears the typed marker"))))

  (test-case "remote-pending marker alone blocks the ladder with a typed reason"
    (with-root (lambda (root)
                 (verify-with-delivery-receipt root
                                               plan
                                               2
                                               root
                                               (lambda () 'approved)
                                               #:approved? (lambda (v) (eq? v 'approved))
                                               #:evidence (lambda (_) "log")
                                               #:remote-published (lambda (_repo _branch _head) #f)
                                               #:snapshot (lambda (_) identity))
                 (check-equal? (remote-pending-blocker root plan 2) 'branch-not-published)
                 (check-true (hash? (load-remote-pending root plan 2)))
                 ;; Without a marker the typed gate is silent.
                 (check-false (remote-pending-blocker root plan 3)))))

  (test-case "default-remote-published? consults the real origin"
    (with-root (lambda (root)
                 (define bare (build-path root "origin.git"))
                 (define repo (build-path root "q"))
                 (make-directory* repo)
                 (make-directory* bare)
                 (git-quiet! bare "init" "--bare" "-q")
                 (git-quiet! repo "init" "-q")
                 (hermetic-identity! repo)
                 (git-quiet! repo "remote" "add" "origin" (path->string bare))
                 (git-quiet! repo "commit" "--allow-empty" "-qm" "baseline")
                 (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/campaign/w2")
                 (define head
                   (string-trim (with-output-to-string (lambda ()
                                                         (git-quiet! repo "rev-parse" "HEAD")))))
                 (check-true (default-remote-published? repo "campaign/w2" head))
                 (check-false (default-remote-published? repo "campaign/w2" (make-string 40 #\e))
                              "an unpushed head is not remotely published")
                 (check-false (default-remote-published? repo "campaign/absent" head)))))

  (test-case "legacy empty head cannot be replaced by current checkout"
    (with-root (lambda (root)
                 (check-false (recover-legacy-delivery-receipt! root plan 2 "" "" root))
                 (check-false (load-delivery-journal root plan 2)))))

  (test-case "durable-receipt-head returns the verified head, and #f without a receipt"
    (with-root (lambda (root)
                 (check-false (durable-receipt-head root plan 2))
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "log")
                                                 #:remote-published (lambda (_repo _branch _head) #t)
                                                 #:snapshot (lambda (_) identity)))
                 (check-eq? result 'approved)
                 (check-equal? (durable-receipt-head root plan 2) (hash-ref identity 'head))))))
