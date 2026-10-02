#lang racket/base
;; tests/test-gsd-delivery-repair-tail.rkt — v1.00.33 W0 repair-tail protocol.
;;
;; The demonstrated production defect (audit §21.2 of
;; .planning/REPORT-attempt-audit-v10033-w0-attempt5-fence2.md):
;; verify-with-delivery-receipt treated ANY existing journal as an idempotent
;; receipt — an approved new-head result kept the old-head receipt and
;; cleared remote-pending. These suites pin the fail-closed repair:
;;   1. receipt head mismatch never satisfies re-verification (marker kept,
;;      receipt kept, verdict untouched);
;;   2. an explicit atomic fenced SAME-ATTEMPT transition
;;      (reconcile-repair-tail-receipt!) records the new receipt while the
;;      prior receipt becomes immutable append-only history;
;;   3. the transition refuses stale attempts/fences, foreign branches,
;;      non-descendant heads, terminal stages and unsafe shapes;
;;   4. the repair-tail files gate verifies delivered-target intactness plus
;;      a real repair delta, never a manufactured target diff;
;;   5. real production invocation: verify-campaign-delivery reconciles the
;;      receipt under the authentic live attempt over a REAL git history,
;;      and the sanctioned provenance writer rebinds the campaign record.
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-journal.rkt
;; @covers extensions/gsd/delivery-receipt.rkt
;; @covers extensions/gsd/delivery-verifier.rkt
(require rackunit
         json
         racket/file
         racket/path
         racket/port
         racket/string
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         (only-in "helpers/delivery-fixtures.rkt"
                  make-tmp-git-repo
                  write-wave-doc!
                  load-plan**
                  cleanup-tmp)
         "../extensions/gsd/campaign-state.rkt"
         (only-in "../extensions/gsd/campaign-repository.rkt" load-campaign-record persist-campaign!)
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         (only-in "../extensions/gsd/delivery-finalize.rkt" record-attempt-delivery-provenance!)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  make-branch-delivery-context
                  branch-delivery-context-ref
                  check-wave-files-changed
                  delivery-verification
                  delivery-verification-approved?
                  current-gsd-git-runner
                  current-gsd-delivery-branch-context))
;; Red-first (repo convention, cf. test-gsd-delivery-receipt.rkt): the
;; repair-tail transition is required dynamically until the journal module
;; provides it; the wrapper/verifier keywords fail at runtime the same way.
(define reconcile-repair-tail-receipt!
  (dynamic-require (string->path "../extensions/gsd/delivery-journal.rkt")
                   'reconcile-repair-tail-receipt!))

(define plan (make-string 64 #\e))
(define old-head (make-string 40 #\b))
(define new-head (make-string 40 #\d))
(define (with-root f)
  (define root (make-temporary-file "repair-tail-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))
(define (receipt-for head #:attempt-id [attempt-id "attempt-5"] #:fence [fence 2])
  (hasheq 'repo
          "/repo/q"
          'branch
          "campaign/w2"
          'head
          head
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"
          'verified-at
          1
          'evidence
          "full Verify passed"
          'attempt-id
          attempt-id
          'attempt-fence
          fence))
(define (seed-journal! root #:stage [stage "context-ready"])
  (record-delivery-receipt! root plan 2 (receipt-for old-head))
  (unless (equal? stage "context-ready")
    (update-delivery-journal! root plan 2 (hasheq 'stage stage))))
(define (always-descendant? _old _new)
  #t)
(define (never-descendant? _old _new)
  #f)
(define (transition root
                    #:new-receipt [new (receipt-for new-head)]
                    #:attempt-id [attempt-id "attempt-5"]
                    #:fence [fence 2]
                    #:ancestor? [ancestor? always-descendant?])
  (reconcile-repair-tail-receipt! root
                                  plan
                                  2
                                  new
                                  #:expected-attempt-id attempt-id
                                  #:expected-fence fence
                                  #:head-ancestor? ancestor?))
(module+ test
  (test-case "transition records the new receipt and preserves the old as history"
    (with-root (lambda (root)
                 (seed-journal! root)
                 (define recorded (transition root))
                 (check-equal? recorded (receipt-for new-head))
                 (define journal (load-delivery-journal root plan 2))
                 (check-equal? (hash-ref (hash-ref journal 'receipt) 'head) new-head)
                 (define history (hash-ref journal 'receipt-history #f))
                 (check-equal? history
                               (list (receipt-for old-head))
                               "prior receipt is preserved verbatim as append-only history")
                 (define third (receipt-for (make-string 40 #\f)))
                 (reconcile-repair-tail-receipt! root
                                                 plan
                                                 2
                                                 third
                                                 #:expected-attempt-id "attempt-5"
                                                 #:expected-fence 2
                                                 #:head-ancestor? always-descendant?)
                 (check-equal? (hash-ref (load-delivery-journal root plan 2) 'receipt-history)
                               (list (receipt-for old-head) (receipt-for new-head))))))
  (test-case "transition refuses unsafe shapes"
    (with-root
     (lambda (root)
       (check-exn exn:fail? (lambda () (transition root)))
       (seed-journal! root)
       (check-exn exn:fail? (lambda () (transition root #:attempt-id "attempt-6")))
       (check-exn exn:fail? (lambda () (transition root #:fence 3)))
       (check-exn exn:fail?
                  (lambda ()
                    (transition root #:new-receipt (receipt-for new-head #:attempt-id "attempt-6"))))
       (check-exn exn:fail?
                  (lambda () (transition root #:new-receipt (receipt-for new-head #:fence 9))))
       (check-exn
        exn:fail?
        (lambda ()
          (transition root #:new-receipt (hash-set (receipt-for new-head) 'branch "campaign/other"))))
       (check-exn exn:fail?
                  (lambda ()
                    (transition root #:new-receipt (hash-set (receipt-for new-head) 'branch "main"))))
       (check-exn exn:fail? (lambda () (transition root #:new-receipt (receipt-for old-head))))
       (check-exn exn:fail? (lambda () (transition root #:ancestor? never-descendant?)))
       (check-exn exn:fail?
                  (lambda ()
                    (transition root #:new-receipt (hash-remove (receipt-for new-head) 'head))))
       (for ([stage '("implementation-merged" "binding-prepared"
                                              "binding-review"
                                              "binding-pr"
                                              "binding-ci"
                                              "binding-merged"
                                              "governance"
                                              "sync"
                                              "delivered"
                                              "awaiting-approval"
                                              "retryable"
                                              "blocked")])
         (with-root (lambda (root2)
                      (record-delivery-receipt! root2 plan 2 (receipt-for old-head))
                      (update-delivery-journal! root2 plan 2 (hasheq 'stage stage))
                      (check-exn exn:fail?
                                 (lambda () (transition root2))
                                 (format "stage ~a must refuse repair-tail reconciliation" stage)))))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 2) 'receipt) 'head)
                     old-head))))
  (test-case "pre-merge stages admit the transition; history is update-immutable"
    (with-root
     (lambda (root)
       (for ([stage
              '("context-ready" "implementation-review" "implementation-pr" "implementation-ci")])
         (with-root (lambda (root2)
                      (record-delivery-receipt! root2 plan 2 (receipt-for old-head))
                      (update-delivery-journal! root2 plan 2 (hasheq 'stage stage))
                      (transition root2)
                      (check-equal? (hash-ref (hash-ref (load-delivery-journal root2 plan 2) 'receipt)
                                              'head)
                                    new-head))))
       (seed-journal! root)
       (transition root)
       (check-exn exn:fail?
                  (lambda () (update-delivery-journal! root plan 2 (hasheq 'receipt-history '())))))))
  (test-case "receipt-history is validated at load and refuses malformed entries"
    (with-root
     (lambda (root)
       (seed-journal! root)
       (define path (delivery-journal-path root plan 2))
       (define datum (load-delivery-journal root plan 2))
       (define poisoned
         (hash-set datum 'receipt-history (list (hash-remove (receipt-for old-head) 'head))))
       (call-with-atomic-output-file path
                                     (lambda (out _)
                                       (write-json poisoned out)
                                       (newline out)))
       (check-exn exn:fail? (lambda () (load-delivery-journal root plan 2))))))
  (define identity-old
    (hasheq 'repo
            "/repo"
            'branch
            "campaign/w2"
            'head
            old-head
            'tree
            (make-string 40 #\c)
            'origin
            "https://github.com/example/q.git"))
  (define identity-new (hash-set identity-old 'head new-head))
  (define attempt (campaign-attempt "attempt-5" 2 0))
  (test-case "approved re-verify at a different head keeps the old receipt and marker"
    (with-root (lambda (root)
                 (seed-journal! root)
                 (record-remote-pending! root plan 2 "campaign/w2" old-head "push raced")
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "re-verify log")
                                                 #:attempt #f
                                                 #:remote-published (lambda (_repo _branch _head) #t)
                                                 #:snapshot (lambda (_) identity-new)
                                                 #:repair-tail-ancestor?
                                                 (lambda (_repo _old _new) #t)))
                 (check-eq? result 'approved)
                 (define journal (load-delivery-journal root plan 2))
                 (check-equal? (hash-ref (hash-ref journal 'receipt) 'head)
                               old-head
                               "the old-head receipt must be retained untouched")
                 (check-false (hash-ref journal 'receipt-history #f))
                 (define marker (load-remote-pending root plan 2))
                 (check-true (hash? marker) "a mismatched receipt must never clear the marker")
                 (check-equal? (hash-ref marker 'head) old-head))))
  (test-case "same-attempt repair tail: the wrapper reconciles the receipt atomically"
    (with-root (lambda (root)
                 (seed-journal! root)
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "repair-tail re-verify log")
                                                 #:attempt attempt
                                                 #:remote-published (lambda (_repo _branch _head) #t)
                                                 #:snapshot (lambda (_) identity-new)
                                                 #:repair-tail-ancestor?
                                                 (lambda (_repo _old _new) #t)))
                 (check-eq? result 'approved)
                 (define journal (load-delivery-journal root plan 2))
                 (define receipt (hash-ref journal 'receipt))
                 (check-equal? (hash-ref receipt 'head) new-head)
                 (check-equal? (hash-ref receipt 'attempt-id) "attempt-5")
                 (check-equal? (hash-ref receipt 'attempt-fence) 2)
                 (check-true (string-contains? (hash-ref receipt 'evidence) "repair-tail"))
                 (check-equal? (hash-ref journal 'receipt-history) (list (receipt-for old-head)))
                 (check-false (load-remote-pending root plan 2)))))
  (test-case "same-attempt repair tail without publication withholds and records the marker"
    (with-root (lambda (root)
                 (seed-journal! root)
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "log")
                                                 #:attempt attempt
                                                 #:remote-published (lambda (_repo _branch _head) #f)
                                                 #:snapshot (lambda (_) identity-new)
                                                 #:repair-tail-ancestor?
                                                 (lambda (_repo _old _new) #t)))
                 (check-eq? result 'approved)
                 (define journal (load-delivery-journal root plan 2))
                 (check-equal? (hash-ref (hash-ref journal 'receipt) 'head)
                               old-head
                               "unpublished head must not reconcile the receipt")
                 (define marker (load-remote-pending root plan 2))
                 (check-true (hash? marker))
                 (check-equal? (hash-ref marker 'head)
                               new-head
                               "the marker names the unpublished repair head"))))
  (test-case "a repair tail bound to a different attempt or branch never reconciles"
    (with-root
     (lambda (root)
       (seed-journal! root)
       (verify-with-delivery-receipt root
                                     plan
                                     2
                                     root
                                     (lambda () 'approved)
                                     #:approved? (lambda (v) (eq? v 'approved))
                                     #:evidence (lambda (_) "log")
                                     #:attempt (campaign-attempt "attempt-6" 2 0)
                                     #:remote-published (lambda (_repo _branch _head) #t)
                                     #:snapshot (lambda (_) identity-new)
                                     #:repair-tail-ancestor? (lambda (_repo _old _new) #t))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 2) 'receipt) 'head)
                     old-head)
       (verify-with-delivery-receipt root
                                     plan
                                     2
                                     root
                                     (lambda () 'approved)
                                     #:approved? (lambda (v) (eq? v 'approved))
                                     #:evidence (lambda (_) "log")
                                     #:attempt attempt
                                     #:remote-published (lambda (_repo _branch _head) #t)
                                     #:snapshot
                                     (lambda (_) (hash-set identity-new 'branch "campaign/other"))
                                     #:repair-tail-ancestor? (lambda (_repo _old _new) #t))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 2) 'receipt) 'head)
                     old-head))))
  (test-case "crash/retry: a repeated verification at the reconciled head is idempotent"
    (with-root
     (lambda (root)
       (seed-journal! root)
       (define (verify-once)
         (verify-with-delivery-receipt root
                                       plan
                                       2
                                       root
                                       (lambda () 'approved)
                                       #:approved? (lambda (v) (eq? v 'approved))
                                       #:evidence (lambda (_) "log")
                                       #:attempt attempt
                                       #:remote-published (lambda (_repo _branch _head) #t)
                                       #:snapshot (lambda (_) identity-new)
                                       #:repair-tail-ancestor? (lambda (_repo _old _new) #t)))
       (verify-once)
       (define after-first (load-delivery-journal root plan 2))
       (record-remote-pending! root plan 2 "campaign/w2" new-head "retry after crash")
       (verify-once)
       (define after-retry (load-delivery-journal root plan 2))
       (check-equal? (hash-ref after-retry 'receipt) (hash-ref after-first 'receipt))
       (check-equal? (hash-ref after-retry 'receipt-history) (hash-ref after-first 'receipt-history))
       (check-false (load-remote-pending root plan 2)))))
  (define (make-repair-repo!)
    (define root (make-temporary-file "repair-tail-repo-~a" 'directory))
    (define repo (build-path root "q"))
    (define wt (build-path root "work"))
    (make-directory repo)
    (git-quiet! repo "init" "-q" "-b" "main")
    (hermetic-identity! repo)
    (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
    (display-to-file "base" (build-path repo "adapter.rkt") #:exists 'truncate)
    (git-quiet! repo "add" "-A")
    (git-quiet! repo "commit" "--allow-empty" "-qm" "baseline")
    (define base-commit
      (string-trim (with-output-to-string (lambda () (git-quiet! repo "rev-parse" "HEAD")))))
    (git-quiet! repo "worktree" "add" "-qb" "campaign/w2" (path->string wt))
    (display-to-file "implemented" (build-path wt "adapter.rkt") #:exists 'truncate)
    (display-to-file "tests" (build-path wt "tests.rkt") #:exists 'truncate)
    (git-quiet! wt "add" "-A")
    (git-quiet! wt "commit" "-qm" "implementation: adapter + tests")
    (define receipt-head
      (string-trim (with-output-to-string (lambda () (git-quiet! wt "rev-parse" "HEAD")))))
    (display-to-file "repair" (build-path wt "repair.rkt") #:exists 'truncate)
    (git-quiet! wt "add" "-A")
    (git-quiet! wt "commit" "-qm" "repair tail: unrelated to targets")
    (values repo wt base-commit receipt-head))
  (define (wave-0 rec)
    (for/first ([w (in-list (campaign-record-waves rec))]
                #:when (= (campaign-wave-index w) 0))
      w))
  (test-case "real production seam reconciles the receipt under the authentic attempt"
    ;; The persisted campaign record requires plan-id == manifest hash, so
    ;; this seam uses the hash of the actual manifest (not the synthetic
    ;; 64×"e" id used by the pure journal-level cases above).
    (define seam-plan
      (campaign-manifest-hash (make-campaign-manifest 1 "canary" '() '() "constraints")))
    (define-values (repo wt base-commit receipt-head) (make-repair-repo!))
    (define root (make-temporary-file "repair-tail-campaign-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (record-delivery-receipt! root seam-plan 0 (receipt-for receipt-head))
       (define record
         (make-campaign-record
          seam-plan
          (make-campaign-manifest 1 "canary" '() '() "constraints")
          (list
           (make-campaign-wave 0 "canary" 'awaiting-delivery 2 (campaign-attempt "attempt-5" 2 0)))
          #f
          ;; record fence-token must equal the live attempt's fence (2) for
          ;; verify-campaign-delivery's stale-attempt gate to admit the result
          2
          'test
          0
          0))
       (persist-campaign! root record)
       (define result
         (verify-campaign-delivery root
                                   seam-plan
                                   0
                                   wt
                                   (make-branch-delivery-context #:repo-root repo
                                                                 #:branch "campaign/w2"
                                                                 #:base-commit base-commit
                                                                 #:worktree-path wt
                                                                 #:repair-tail-head receipt-head)
                                   (lambda (_) (delivery-verification #t '() "repair-tail verified"))
                                   (lambda () record)
                                   #:remote-published (lambda (_repo _branch _head) #t)))
       (check-true (delivery-verification-approved? result))
       (define journal (load-delivery-journal root seam-plan 0))
       (define receipt (hash-ref journal 'receipt))
       (define tip
         (string-trim (with-output-to-string (lambda () (git-quiet! wt "rev-parse" "HEAD")))))
       (check-equal? (hash-ref receipt 'head) tip)
       (check-equal? (hash-ref receipt 'branch) "campaign/w2")
       (check-equal? (hash-ref (car (hash-ref journal 'receipt-history)) 'head)
                     receipt-head
                     "the prior receipt must be preserved as history[0]")
       (define provenance (record-attempt-delivery-provenance! root seam-plan 0 "attempt-5" 2))
       (check-equal? provenance (cons "campaign/w2" tip))
       (define reloaded (load-campaign-record root seam-plan))
       (check-equal? (campaign-wave-delivery-branch (wave-0 reloaded)) "campaign/w2")
       (check-equal? (campaign-wave-delivery-head-sha (wave-0 reloaded)) tip))
     (lambda ()
       (delete-directory/files root #:must-exist? #f)
       (with-handlers ([exn:fail? void])
         (git-quiet! repo "worktree" "remove" "--force" (path->string wt)))))
    (define base-sha (make-string 40 #\1))
    (define receipt-sha (make-string 40 #\2))
    (define (repair-tail-git-facts base-diff delta)
      (lambda (_git-root args)
        (cond
          [(and (pair? args) (equal? (car args) "rev-parse")) (list 0 "ok\n" "")]
          [(and (>= (length args) 3)
                (equal? (car args) "diff")
                (string-contains? (list-ref args 2) base-sha))
           (list 0 (string-append (string-join base-diff "\n") (if (null? base-diff) "" "\n")) "")]
          [(and (>= (length args) 3)
                (equal? (car args) "diff")
                (string-contains? (list-ref args 2) receipt-sha))
           (list 0 (string-append (string-join delta "\n") (if (null? delta) "" "\n")) "")]
          [else (list 1 "" "repair-tail-git-facts: unhandled")])))
    (define (gate-result base-dir plan-struct base-diff delta)
      (parameterize ([current-gsd-git-runner (repair-tail-git-facts base-diff delta)]
                     [current-gsd-delivery-branch-context
                      (make-branch-delivery-context #:repo-root
                                                    (path->string (build-path base-dir "q"))
                                                    #:branch "campaign/w2"
                                                    #:base-commit base-sha
                                                    #:worktree-path #f
                                                    #:repair-tail-head receipt-sha)])
        (check-wave-files-changed base-dir 0 plan-struct)))
    (test-case "repair-tail files gate: intact targets + real delta approve"
      (define base (make-tmp-git-repo))
      (dynamic-wind void
                    (lambda ()
                      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
                      (define plan-struct (load-plan** base '("q/ui-core/preferences.rkt") "exit 0"))
                      (define result
                        (gate-result base
                                     plan-struct
                                     '("ui-core/preferences.rkt")
                                     '("repair.rkt" "tests/wrapper.rkt")))
                      (check-true (car (cdr result)) (format "expected approval, got ~s" result))
                      (check-true (string-contains? (cdr (cdr result)) "repair-tail")
                                  (format "detail must name the repair-tail mode: ~s" result)))
                    (lambda () (cleanup-tmp base))))
    (test-case "repair-tail files gate: delivered target missing from the branch refuses"
      (define base (make-tmp-git-repo))
      (dynamic-wind void
                    (lambda ()
                      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
                      (define plan-struct (load-plan** base '("q/ui-core/preferences.rkt") "exit 0"))
                      (define result
                        (gate-result base plan-struct '("unrelated.rkt") '("repair.rkt")))
                      (check-false (car (cdr result)) "a target absent from the branch must refuse")
                      (check-true (string-contains? (cdr (cdr result)) "target")))
                    (lambda () (cleanup-tmp base))))
    (test-case "repair-tail files gate: empty repair delta refuses"
      (define base (make-tmp-git-repo))
      (dynamic-wind void
                    (lambda ()
                      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
                      (define plan-struct (load-plan** base '("q/ui-core/preferences.rkt") "exit 0"))
                      (define result (gate-result base plan-struct '() '()))
                      (check-false (car (cdr result)) "an empty repair tail is not repair content")
                      (check-true (string-contains? (cdr (cdr result)) "no repair content")))
                    (lambda () (cleanup-tmp base))))
    (test-case "repair-tail files gate: a target inside the repair delta refuses"
      (define base (make-tmp-git-repo))
      (dynamic-wind void
                    (lambda ()
                      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
                      (define plan-struct (load-plan** base '("q/ui-core/preferences.rkt") "exit 0"))
                      (define result
                        (gate-result base
                                     plan-struct
                                     '("ui-core/preferences.rkt")
                                     '("ui-core/preferences.rkt" "repair.rkt")))
                      (check-false (car (cdr result)) "repair drift on a declared target must refuse")
                      (check-true (string-contains? (cdr (cdr result)) "target")
                                  (format "detail must name the drifted target: ~s" result)))
                    (lambda () (cleanup-tmp base))))
    (test-case "repair-tail files gate: git failure fails closed"
      (define base (make-tmp-git-repo))
      (dynamic-wind
       void
       (lambda ()
         (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
         (define plan-struct (load-plan** base '("q/ui-core/preferences.rkt") "exit 0"))
         (parameterize ([current-gsd-git-runner (lambda (_root _args) (list 128 "" "boom"))]
                        [current-gsd-delivery-branch-context
                         (make-branch-delivery-context #:repo-root
                                                       (path->string (build-path base "q"))
                                                       #:branch "campaign/w2"
                                                       #:base-commit base-sha
                                                       #:worktree-path #f
                                                       #:repair-tail-head receipt-sha)])
           (define result (check-wave-files-changed base 0 plan-struct))
           (check-false (car (cdr result)) "git failure must fail closed")))
       (lambda () (cleanup-tmp base))))
    (test-case "normal verification semantics are unchanged without repair-tail-head"
      (define base (make-tmp-git-repo))
      (dynamic-wind
       void
       (lambda ()
         (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "exit 0")
         (define plan-struct (load-plan** base '("q/ui-core/preferences.rkt") "exit 0"))
         (parameterize ([current-gsd-git-runner
                         (lambda (_root args)
                           (cond
                             [(and (pair? args) (equal? (car args) "rev-parse")) (list 0 "ok\n" "")]
                             [else (list 0 "\n" "")]))]
                        [current-gsd-delivery-branch-context
                         (make-branch-delivery-context #:repo-root
                                                       (path->string (build-path base "q"))
                                                       #:branch "campaign/w2"
                                                       #:base-commit base-sha
                                                       #:worktree-path #f)])
           (define result (check-wave-files-changed base 0 plan-struct))
           (check-false (car (cdr result)) "unchanged targets must still refuse in normal mode")
           (check-true (string-contains? (cdr (cdr result)) "no wave target files changed"))))
       (lambda () (cleanup-tmp base))))))
