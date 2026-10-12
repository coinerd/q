#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 150
;; @covers extensions/gsd/review-proof.rkt extensions/gsd/delivery-receipt.rkt extensions/gsd/delivery-verifier.rkt
(require rackunit
         json
         (only-in "../util/json/checksum.rkt" sha256-file)
         "../extensions/gsd/review-proof.rkt"
         "helpers/private-fixture-templates.rkt"
         "helpers/delivery-fixtures.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/plan-snapshot.rkt")
(define plan-id (make-string 64 #\a))
(define command "printf 'owned-output\\n' && printf 'second-stage\\n'")
(define (fixture f #:command [declared command])
  (define root (make-temporary-file "owned-proof-~a" 'directory))
  (define repo (build-path root "q"))
  (define wt (build-path root "worker"))
  (dynamic-wind
   void
   (lambda ()
     (make-directory repo)
     (git-quiet! repo "init" "-q")
     (hermetic-identity! repo)
     (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
     (git-quiet! repo "commit" "--allow-empty" "-qm" "baseline")
     (git-quiet! repo "worktree" "add" "-qb" "campaign/w2" (path->string wt))
     (make-directory* (build-path root ".planning" "waves"))
     (write-plan! root 2 "Proof" "proof")
     (write-wave-doc! root 2 "proof" '("q/proof.rkt") declared)
     (make-plan-snapshot! root plan-id (file->string (build-path root ".planning" "PLAN.md")))
     (define w (make-campaign-wave 2 "proof" 'awaiting-delivery 1 (campaign-attempt "attempt-1" 7 0)))
     (define rec
       (make-campaign-record plan-id
                             (make-campaign-manifest 1 "proof" '() '() "constraints")
                             (list w)
                             #f
                             7
                             'test
                             0
                             0))
     (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                    [current-gsd-delivery-verify-timeout-sec 10]
                    [current-owned-proof-root (build-path root "managed-proofs")])
       (f root repo wt rec)))
   (lambda () (delete-directory/files root))))
(define (verify! root wt rec #:refresh? [refresh? #f] #:after-job [after-job void])
  (verify-campaign-delivery
   root
   plan-id
   2
   wt
   (make-branch-delivery-context #:repo-root (hash-ref (committed-delivery-snapshot wt) 'repo)
                                 #:branch "campaign/w2"
                                 #:base-commit (hash-ref (committed-delivery-snapshot wt) 'head)
                                 #:worktree-path wt)
   (lambda (_)
     (define check (check-verify-command root 2 #f))
     (after-job)
     (delivery-verification (cadr check) (list check) "owned verification"))
   (lambda () rec)
   #:remote-published (lambda (_repo _branch _head) #t)
   #:refresh-owned-proof? refresh?))
(define (saved-receipt root)
  (hash-ref (load-delivery-journal root plan-id 2) 'receipt))
(define (proof-dir)
  (build-path (current-owned-proof-root) plan-id))
(define (read-proof r [cmd command])
  (define ref (hash-ref r 'owned-proof))
  (load-owned-review-proof (proof-dir)
                           ref
                           (hash-ref ref 'binding)
                           #:command cmd
                           #:normalized-command cmd))
(define (seed-legacy! root wt)
  (define r
    (hash-set* (committed-delivery-snapshot wt)
               'attempt-id
               "attempt-1"
               'attempt-fence
               7
               'verified-at
               1
               'evidence
               "legacy successful Verify without owner metadata"))
  (record-delivery-receipt! root plan-id 2 r)
  r)
(define (write-json-file! p v)
  (call-with-output-file p
                         (lambda (out)
                           (write-json v out)
                           (newline out))
                         #:exists 'truncate))
(define (forge-bundle! dir ref manifest)
  (make-directory* dir)
  (define p (build-path dir "candidate.json"))
  (write-json-file! p manifest)
  (define sha (sha256-file p))
  (define target (build-path dir (string-append sha ".json")))
  (rename-file-or-directory p target)
  (hash-set* ref 'sha256 sha 'path (string-append sha ".json") 'size (file-size target)))
(define (check-no-copy! root r m)
  (define old (load-delivery-journal root plan-id 2))
  (check-exn exn:fail?
             (lambda ()
               (record-delivery-receipt! root plan-id 2 (hash-set r 'head (make-string 40 #\e)))))
  (define ref (hash-ref r 'owned-proof))
  (define new-ref
    (forge-bundle! (proof-dir) ref (hash-set m 'job (hash-set (hash-ref m 'job) 'id "fake-new-job"))))
  (define replacement (hash-set r 'owned-proof new-ref))
  (for ([guard (list (lambda () #t) (lambda () #f))])
    (check-exn exn:fail?
               (lambda ()
                 (refresh-owned-proof-receipt! root
                                               plan-id
                                               2
                                               replacement
                                               #:old-receipt r
                                               #:authorization #f
                                               #:guard guard))))
  (check-equal? (load-delivery-journal root plan-id 2) old))
(module+ test
  (test-case "fast operational jobs flag is preserved but partial/dry-run profiles cannot certify broad"
    (check-true (proof-verify-command-eligible? "racket scripts/run-tests.rkt fast --jobs 2"))
    (for ([flag '("--impact-dry-run" "--changed-base abc"
                                     "--files a.rkt"
                                     "--partial"
                                     "--help"
                                     "--inventory"
                                     "--suite unit"
                                     "--profile canary"
                                     "--explain"
                                     "--generate-covers-manifest"
                                     "--diagnose-overhead"
                                     "--lint-metadata"
                                     "--shard-plan report"
                                     "tests/a.rkt")])
      (check-false (proof-verify-command-eligible? (string-append "racket scripts/run-tests.rkt fast "
                                                                  flag)))))
  (test-case "existing receipt API must retain owned terminal job rather than only cmd/exit prose"
    (fixture
     (lambda (root repo wt rec)
       (check-true (delivery-verification-approved? (verify! root wt rec)))
       (define r (hash-ref (load-delivery-journal root plan-id 2) 'receipt))
       (check-true (hash? (hash-ref r 'owned-proof #f))
                   "approved owned job/log identity is lost at receipt creation")
       (define m (read-proof r))
       (check-equal? (hash-ref m 'command) command)
       (check-equal? (hash-ref m 'before) (committed-delivery-snapshot wt))
       (check-equal? (hash-ref m 'after) (hash-ref m 'before))
       (check-equal? (hash-ref m 'execution-cwd) (path->string wt))
       (check-equal? (hash-ref (hash-ref m 'job) 'state) "completed")
       (check-equal? (hash-ref (hash-ref m 'job) 'exit-code) 0)
       (check-true (hash-ref (hash-ref m 'job) 'reaped))
       (check-true (positive? (hash-ref (hash-ref m 'job) 'proc-start-ticks)))
       (check-equal? (file->string (build-path (proof-dir) (hash-ref (hash-ref m 'log) 'path)))
                     "owned-output\nsecond-stage\n")
       (check-false (receipt-owned-proof-blocker r root plan-id 2 #:command command))
       (check-equal?
        (receipt-owned-proof-blocker (hash-remove r 'owned-proof) root plan-id 2 #:command command)
        'missing-owned-proof)
       (check-true (valid-delivery-receipt? (hash-remove r 'owned-proof)))
       (check-false (proof-reference-matches-receipt? (hash-ref r 'owned-proof)
                                                      (hash-set r 'head (make-string 40 #\f)))))))
  (test-case "full closed raw output retained, not bounded registry tail"
    (define long-command
      "i=0; while [ $i -lt 9000 ]; do printf '0123456789abcdef\\n'; i=$((i+1)); done")
    (fixture (lambda (root repo wt rec)
               (verify! root wt rec)
               (define m (read-proof (saved-receipt root) long-command))
               (define log-ref (hash-ref m 'log))
               (check-equal? (hash-ref log-ref 'size) (* 9000 17))
               (check-equal? (sha256-file (build-path (proof-dir) (hash-ref log-ref 'path)))
                             (hash-ref log-ref 'sha256)))
             #:command long-command))
  (test-case "explicit fresh same-head refresh appends old receipt; implicit repeat preserves bytes"
    (fixture (lambda (root repo wt rec)
               (define old (seed-legacy! root wt))
               (update-delivery-journal!
                root
                plan-id
                2
                (hasheq 'stage "implementation-review" 'old-packet "keep-frozen-trio"))
               (define path (delivery-journal-path root plan-id 2))
               (define bytes (file->bytes path))
               (verify! root wt rec)
               (check-equal? (file->bytes path) bytes "ordinary reverify cannot implicitly refresh")
               (check-true (delivery-verification-approved? (verify! root wt rec #:refresh? #t)))
               (define j (load-delivery-journal root plan-id 2))
               (check-equal? (hash-ref j 'receipt-history) (list old))
               (check-equal? (length (hash-ref j 'receipt-history))
                             1
                             "zero-based review generation is history length, not job generation")
               (check-equal? (hash-ref j 'old-packet) "keep-frozen-trio")
               (check-equal? (hash-ref j 'stage) "context-ready")
               (read-proof (hash-ref j 'receipt))
               (define upgraded (file->bytes path))
               (certify-publication-receipt! root
                                             plan-id
                                             2
                                             (hash-set (hash-ref j 'receipt) 'verified-at 999)
                                             #:expected-attempt-id "attempt-1"
                                             #:expected-fence 7
                                             #:guard void)
               (check-equal? (file->bytes path) upgraded)
               (verify! root wt rec #:refresh? #t)
               (define next (load-delivery-journal root plan-id 2))
               (check-equal? (hash-ref next 'receipt-history) (list old (hash-ref j 'receipt)))
               (check-equal? (length (hash-ref next 'receipt-history)) 2)
               (check-not-equal? (hash-ref (hash-ref next 'receipt) 'owned-proof)
                                 (hash-ref (hash-ref j 'receipt) 'owned-proof)))))
  (test-case "refresh requires live guard, fresh owned job, exact old source and premerge stage"
    (fixture
     (lambda (root repo wt rec)
       (define old (seed-legacy! root wt))
       (for ([kind
              '(postmerge cancelled failed paused failed-after stale replaced override drift raced)])
         ;; Reuse an isolated repository, not assertions or gate outcomes.
         (set-campaign-wave-status! (car (campaign-record-waves rec)) 'awaiting-delivery)
         (set-campaign-record-budget-pause! rec #f)
         (set-campaign-cancellation! rec #f)
         (set-campaign-fence-token! rec 7)
         (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                             (campaign-attempt "attempt-1" 7 0))
         (update-delivery-journal! root plan-id 2 (hasheq 'stage "context-ready"))
         (case kind
           [(postmerge)
            (update-delivery-journal! root plan-id 2 (hasheq 'stage "implementation-merged"))]
           [(cancelled) (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))]
           [(failed) (set-campaign-wave-status! (car (campaign-record-waves rec)) 'failed)]
           [(paused)
            (set-campaign-record-budget-pause! rec (campaign-budget-pause 'max-cost 1 2 "test" 0))]
           [(stale) (set-campaign-fence-token! rec 8)]
           [(replaced)
            (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                                (campaign-attempt "other" 7 0))])
         (define before (file->bytes (delivery-journal-path root plan-id 2)))
         (parameterize ([current-gsd-delivery-verify-command (and (eq? kind 'override) "true")])
           (check-exn
            exn:fail?
            (lambda ()
              (verify!
               root
               wt
               rec
               #:refresh? #t
               #:after-job
               (lambda ()
                 (case kind
                   [(failed-after)
                    (set-campaign-wave-status! (car (campaign-record-waves rec)) 'failed)]
                   [(drift) (display-to-file "dirty" (build-path wt "dirty") #:exists 'truncate)]
                   [(raced) (update-delivery-journal! root plan-id 2 (hasheq 'raced #t))]))))))
         (check-equal? (saved-receipt root) old)
         (unless (eq? kind 'raced)
           (check-equal? (file->bytes (delivery-journal-path root plan-id 2)) before))
         (when (eq? kind 'drift)
           (delete-file (build-path wt "dirty")))))))
  (test-case "failed owned execution, source drift and cancellation never stamp audit proof"
    (for ([kind '(failed drift cancelled)])
      (fixture
       (lambda (root repo wt rec)
         (verify! root
                  wt
                  rec
                  #:after-job
                  (lambda ()
                    (case kind
                      [(drift) (display-to-file "dirty" (build-path wt "dirty") #:exists 'truncate)]
                      [(cancelled)
                       (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))])))
         (check-false (load-delivery-journal root plan-id 2))
         (when (eq? kind 'failed)
           (define diagnostics
             (for/list ([p (directory-list (proof-dir) #:build? #t)]
                        #:when (equal? (filename-extension p) #"json"))
               (call-with-input-file p read-json)))
           (check-equal? (length diagnostics) 1)
           (define d (car diagnostics))
           (check-equal? (hash-ref d 'kind) "declared-verify-terminal-diagnostic")
           (check-equal? (hash-ref (hash-ref d 'job) 'exit-code) 9)
           (check-equal? (file->string (build-path (proof-dir) (hash-ref (hash-ref d 'log) 'path)))
                         "actual-failure\n")
           (check-false (valid-owned-proof-manifest? d))))
       #:command (if (eq? kind 'failed) "printf 'actual-failure\\n'; exit 9" command))))
  (test-case "expired owned observation cannot be replayed as fresh execution"
    (fixture (lambda (root repo wt rec)
               (define observation #f)
               (parameterize ([current-owned-verify-observer (lambda (o) (set! observation o))])
                 (check-verify-command root 2 #f))
               (check-true (owned-verify-observation? observation))
               (check-false (consume-owned-verify-observation! observation)))))
  (test-case "offline export and strict field/binding tamper rejection"
    (fixture
     (lambda (root repo wt rec)
       (verify! root wt rec)
       (define r (saved-receipt root))
       (define ref (hash-ref r 'owned-proof))
       (define expected (hash-ref ref 'binding))
       (define m (read-proof r))
       (check-no-copy! root r m)
       (define bundle (build-path root "offline"))
       (export-owned-review-proof! (proof-dir)
                                   ref
                                   expected
                                   bundle
                                   #:command command
                                   #:normalized-command command)
       (check-equal?
        (load-owned-review-proof bundle ref expected #:command command #:normalized-command command)
        m)
       (for ([field
              '(head tree attempt-id attempt-fence wave plan-id snapshot-sha256 origin repo branch)])
         (check-exn exn:fail?
                    (lambda ()
                      (load-owned-review-proof bundle
                                               ref
                                               (hash-set expected field "wrong")
                                               #:command command
                                               #:normalized-command command))))
       (check-exn exn:fail?
                  (lambda ()
                    (load-owned-review-proof bundle
                                             ref
                                             expected
                                             #:command "true"
                                             #:normalized-command command)))
       (for ([bad (list
                   (hash-set m 'kind "meaningful-red")
                   (hash-set m 'verification-origin "model-authored")
                   (hash-set m 'after (hash-set (hash-ref m 'after) 'head (make-string 40 #\f)))
                   (hash-set m 'job (hash-set (hash-ref m 'job) 'exit-code 'null))
                   (hash-set m 'job (hash-set (hash-ref m 'job) 'exit-code 7))
                   (hash-set m 'job (hash-set (hash-ref m 'job) 'reaped #f))
                   (hash-set m 'job (hash-set (hash-ref m 'job) 'state "cancelled"))
                   (hash-set m 'job (hash-set (hash-ref m 'job) 'identity-key (make-string 64 #\b)))
                   (hash-set m 'command "racket scripts/test.rkt fast --impact-dry-run")
                   (hash-remove m 'log))]
             [n (in-naturals)])
         (define dir (build-path root (format "tamper-~a" n)))
         (define forged (forge-bundle! dir ref bad))
         ;; Keep valid raw bytes present: a structural negative must not pass
         ;; accidentally merely because the forged bundle omitted its log.
         (copy-file (build-path (proof-dir) (hash-ref (hash-ref m 'log) 'path))
                    (build-path dir (hash-ref (hash-ref m 'log) 'path)))
         (check-exn exn:fail?
                    (lambda ()
                      (load-owned-review-proof dir
                                               forged
                                               expected
                                               #:command command
                                               #:normalized-command command))))
       (check-false (valid-owned-proof-reference? (hash-set ref 'path "../escape.json")))
       (check-false (valid-delivery-receipt?
                     (hash-set r 'owned-proof (hash-set ref 'path "/tmp/fake.json"))))
       (define log-path (build-path bundle (hash-ref (hash-ref m 'log) 'path)))
       (file-or-directory-permissions log-path #o600)
       (display-to-file "tampered" log-path #:exists 'truncate)
       (check-exn exn:fail?
                  (lambda ()
                    (load-owned-review-proof bundle
                                             ref
                                             expected
                                             #:command command
                                             #:normalized-command command)))
       (delete-file log-path)
       (make-file-or-directory-link (build-path (proof-dir) (hash-ref (hash-ref m 'log) 'path))
                                    log-path)
       (check-exn exn:fail?
                  (lambda ()
                    (load-owned-review-proof bundle
                                             ref
                                             expected
                                             #:command command
                                             #:normalized-command command))))))
  (test-case "managed store inside source or symlinked path is never eligible"
    (for ([kind '(inside symlink)])
      (fixture (lambda (root repo wt rec)
                 (define bad-root (build-path (if (eq? kind 'inside) wt root) "bad-proofs"))
                 (when (eq? kind 'symlink)
                   (make-directory (build-path root "redirect"))
                   (make-file-or-directory-link (build-path root "redirect") bad-root))
                 (parameterize ([current-owned-proof-root bad-root])
                   (verify! root wt rec)
                   (define r (saved-receipt root))
                   (check-false (hash-ref r 'owned-proof #f))))))))
