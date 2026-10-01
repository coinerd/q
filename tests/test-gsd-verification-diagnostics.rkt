#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/verification-diagnostics.rkt
;; @covers extensions/gsd/delivery-receipt.rkt
;;
;; §17.3 regression coverage: per-verification diagnostics at the single
;; owned wrapper boundary (verify-with-delivery-receipt). Pins: outcome on
;; approved/rejected/error, expected-vs-resolved branch (unresolved recorded,
;; never dropped), snapshot-drift third path, append-only history, tmp-orphan
;; tolerance, fail-closed validation, collision-safe atomic no-replace
;; publication, containment (a diagnostic failure never alters the wrapped
;; result), and non-substitution (diagnostics are not receipts, cannot clear
;; remote-pending markers, and carry no journal stage).

(require racket/file
         racket/async-channel
         racket/list
         racket/path
         rackunit
         (file "../extensions/gsd/campaign-state.rkt")
         (file "../extensions/gsd/delivery-journal.rkt")
         (file "../extensions/gsd/delivery-receipt.rkt")
         (file "../extensions/gsd/delivery-verifier.rkt")
         (file "../extensions/gsd/verification-diagnostics.rkt"))

(define plan (apply string-append (make-list 32 "a1"))) ; 64 hex chars
(define head (apply string-append (make-list 5 "b1234567"))) ; 40 hex chars
(define base-sha (apply string-append (make-list 5 "c9876543"))) ; 40 hex chars

(define (fresh-base!)
  (make-temporary-file "qdiag-~a" 'directory))

(define snap
  (hasheq 'repo
          "https://example.invalid/q.git"
          'branch
          "campaign/x/w2"
          'head
          head
          'tree
          base-sha
          'origin
          "https://example.invalid/q.git"))

(define (run-wrapper base
                     thunk
                     #:snap1 [snap1 snap]
                     #:snap2 [snap2 snap]
                     #:attempt [attempt (campaign-attempt "attempt-1" 2 0)]
                     #:ctx [ctx
                            (make-branch-delivery-context #:repo-root base
                                                          #:branch "campaign/x/w2"
                                                          #:base-commit base-sha)])
  (define n (box 0))
  (parameterize ([current-gsd-delivery-branch-context ctx]
                 [current-gsd-remote-published (lambda (_root _branch _head) #t)])
    (verify-with-delivery-receipt base
                                  plan
                                  2
                                  base
                                  thunk
                                  #:approved? (lambda (r) (eq? r 'approved))
                                  #:evidence
                                  (lambda (r) (hasheq 'summary "focused validation evidence"))
                                  #:snapshot (lambda (_cwd)
                                               (begin0 (if (zero? (unbox n)) snap1 snap2)
                                                 (set-box! n (add1 (unbox n)))))
                                  #:attempt attempt
                                  #:remote-published (lambda (_root _branch _head) #t))))

(define (diags base)
  (load-verification-diagnostics base plan))

;; (a) approved + snapshot-unchanged → receipt recorded, outcome approved.
(let* ([base (fresh-base!)])
  (check-eq? (run-wrapper base (lambda () 'approved)) 'approved)
  (define j (load-delivery-journal base plan 2))
  (check-true (hash? j) "receipt must be recorded on the approved path")
  (define r (hash-ref j 'receipt #f))
  (check-true (hash? r) "receipt identity is nested under the 'receipt slot")
  (check-equal? (hash-ref r 'attempt-id #f) "attempt-1")
  (define ds (diags base))
  (check-equal? (length ds) 1)
  (define d (car ds))
  (check-equal? (hash-ref d 'outcome) "approved")
  (check-equal? (hash-ref d 'receipt-decision) "receipt-recorded")
  (check-true (hash-ref d 'snapshot-unchanged?))
  (check-equal? (hash-ref d 'head) head)
  (check-equal? (hash-ref d 'base) base-sha)
  (check-true (hash-ref d 'base-bound?))
  (check-equal? (hash-ref d 'attempt-fence) 2)
  (check-true (hash-ref d 'attempt-fence-bound?)))

;; (b) rejected + unresolved branch → outcome rejected; resolved branch is
;; recorded as literally unresolved ("" + branch-resolved? #f), never dropped.
(let* ([base (fresh-base!)])
  (check-eq? (run-wrapper base (lambda () 'rejected)) 'rejected)
  (check-false (load-delivery-journal base plan 2) "no receipt on rejection")
  (define d (car (diags base)))
  (check-equal? (hash-ref d 'outcome) "rejected")
  (check-equal? (hash-ref d 'receipt-decision) "rejected")
  (check-equal? (hash-ref d 'expected-branch) "campaign/x/w2")
  (check-true (hash-ref d 'branch-resolved?))
  (check-equal? (hash-ref d 'resolved-branch) "campaign/x/w2"))

;; (c) verifier exception → diagnostic outcome=error, exception re-raised.
(let* ([base (fresh-base!)])
  (check-exn exn:fail?
             (lambda ()
               (run-wrapper base
                            (lambda ()
                              (raise (exn:fail "verify exploded" (current-continuation-marks)))))))
  (define d (car (diags base)))
  (check-equal? (hash-ref d 'outcome) "error")
  (check-true (> (string-length (hash-ref d 'exception)) 0)))

;; (d) snapshot drift → third silent path closed: drift flag recorded,
;; receipt skipped, no journal entry, original approval still returned.
(let* ([base (fresh-base!)])
  (check-eq? (run-wrapper base
                          (lambda () 'approved)
                          #:snap2 (hasheq 'repo
                                          "https://example.invalid/q.git"
                                          'branch
                                          "campaign/x/w2"
                                          'head
                                          (apply string-append (make-list 5 "d1234567"))))
             'approved)
  (check-false (load-delivery-journal base plan 2) "drift must skip receipt recording")
  (define d (car (diags base)))
  (check-equal? (hash-ref d 'outcome) "approved")
  (check-false (hash-ref d 'snapshot-unchanged?))
  (check-equal? (hash-ref d 'receipt-decision) "receipt-skipped:snapshot-drift"))

;; (e) tmp orphan (crash before publication) is ignored by the loader.
(let* ([base (fresh-base!)])
  (run-wrapper base (lambda () 'approved))
  (call-with-output-file (build-path (verification-diagnostics-dir base plan) "orphan.json.tmp")
                         (lambda (o) (display "not-json-at-all" o)))
  (check-equal? (length (diags base)) 1 "loader must skip *.tmp orphans"))

;; (f) two verifications → two immutable files (history preserved, no dedupe).
(let* ([base (fresh-base!)])
  (run-wrapper base (lambda () 'approved))
  (run-wrapper base (lambda () 'rejected))
  (check-equal? (length (diags base)) 2)
  (check-equal? (length (remove-duplicates (map (lambda (d) (hash-ref d 'outcome)) (diags base)))) 2))

;; (g) malformed record fails closed: nothing written, publish contained.
(let* ([base (fresh-base!)])
  (define bad
    (hash-copy (make-verification-diagnostic plan
                                             2
                                             "attempt-1"
                                             2
                                             #t
                                             base-sha
                                             #t
                                             "campaign/x/w2"
                                             ""
                                             #f
                                             "https://example.invalid/q.git"
                                             ""
                                             head
                                             base-sha
                                             base-sha
                                             0
                                             1
                                             "bogus-outcome"
                                             #t
                                             #t
                                             "rejected"
                                             ""
                                             "")))
  (define out (publish-verification-diagnostic! base plan bad))
  (check-eq? (car out) 'contained)
  (check-equal? (length (diags base)) 0))

;; (h) non-substitution: a diagnostic alone is not a receipt — journal absent,
;; no stage key, remote-pending marker neither created nor cleared.
(let* ([base (fresh-base!)])
  (run-wrapper base (lambda () 'rejected))
  (check-false (load-delivery-journal base plan 2))
  (define d (car (diags base)))
  (check-false (hash-has-key? d 'stage))
  (check-false
   (file-exists?
    (build-path base ".planning" "campaigns" plan (format "delivery-remote-pending-w~a.rktd" 2)))))

;; (j) collision-safe atomic no-replace publication: identical identity +
;; 32-hex nonce → second publish is contained, original file byte-identical,
;; and the published record embeds its own verification id (file name and
;; content correlated).
(let* ([base (fresh-base!)]
       [nonce (apply string-append (make-list 4 "deadbeef"))]
       [rec (make-verification-diagnostic plan
                                          2
                                          "attempt-1"
                                          2
                                          #t
                                          base-sha
                                          #t
                                          "campaign/x/w2"
                                          ""
                                          #f
                                          "https://example.invalid/q.git"
                                          ""
                                          head
                                          base-sha
                                          base-sha
                                          0
                                          1
                                          "approved"
                                          #t
                                          #t
                                          "receipt-recorded"
                                          ""
                                          "")]
       [p1 (publish-verification-diagnostic! base plan rec #:nonce nonce)])
  (check-eq? (car p1) 'published)
  (define before-bytes (file->bytes (cdr p1)))
  (check-true (regexp-match? (format "~a\\.json$" nonce) (cdr p1))
              "filename suffix is the verification id")
  (check-equal? (hash-ref (car (diags base)) 'verification-id)
                nonce
                "embedded verification id equals the filename suffix")
  (define p2 (publish-verification-diagnostic! base plan rec #:nonce nonce))
  (check-eq? (car p2) 'contained)
  (check-true (regexp-match? #px"collision" (cdr p2)))
  (check-equal? (file->bytes (cdr p1)) before-bytes "no overwrite, ever")
  (check-equal? (length (diags base)) 1))

;; (k) containment at the wrapper boundary: a diagnostics-directory conflict
;; (file where the directory must live) cannot alter the wrapped result.
(let* ([base (fresh-base!)])
  (make-directory* (build-path base ".planning"))
  (call-with-output-file (build-path base ".planning" "campaigns")
                         (lambda (o) (display "conflict" o)))
  (check-eq? (run-wrapper base (lambda () 'approved)) 'approved)
  (check-false (directory-exists? (verification-diagnostics-dir base plan))))

(check-true (valid-verification-diagnostic?
             (hash-set (make-verification-diagnostic plan
                                                     2
                                                     "attempt-1"
                                                     2
                                                     #t
                                                     base-sha
                                                     #t
                                                     "campaign/x/w2"
                                                     ""
                                                     #f
                                                     "https://example.invalid/q.git"
                                                     ""
                                                     head
                                                     base-sha
                                                     base-sha
                                                     0
                                                     1
                                                     "approved"
                                                     #t
                                                     #t
                                                     "rejected"
                                                     ""
                                                     "")
                       'verification-id
                       "a1b2c3d4a1b2c3d4a1b2c3d4a1b2c3d4"))
            "validity is defined for the ID-injected record, as publish requires")

;; Independent-review regressions. Run with raco test, not plain racket.
(module+ test
  (define (rec)
    (make-verification-diagnostic plan
                                  2
                                  "attempt-1"
                                  2
                                  #t
                                  base-sha
                                  #t
                                  "campaign/x/w2"
                                  ""
                                  #f
                                  "/repo"
                                  "/repo"
                                  head
                                  base-sha
                                  base-sha
                                  100
                                  101
                                  "rejected"
                                  #t
                                  #t
                                  "rejected"
                                  ""
                                  ""))
  (test-case "production campaign seam captures context without outer parameterization"
    (define root (fresh-base!))
    (define w (make-campaign-wave 2 "diagnostic" 'verifying 1 (campaign-attempt "attempt-1" 2 0)))
    (define record
      (make-campaign-record plan
                            (make-campaign-manifest 1 "diagnostic" '() '() "x")
                            (list w)
                            #f
                            2
                            'test
                            0
                            0))
    (define ctx
      (make-branch-delivery-context #:repo-root root #:branch "campaign/x/w2" #:base-commit base-sha))
    (check-false (verify-campaign-delivery root plan 2 root ctx (lambda (_) #f) (lambda () record)))
    (define d (car (diags root)))
    (check-equal? (hash-ref d 'base) base-sha)
    (check-equal? (hash-ref d 'expected-branch) "campaign/x/w2")
    (check-equal? (hash-ref d 'repo-root) (path->string root)))
  (test-case "actual snapshot branch and after-head are distinct from expected ref"
    (define root (fresh-base!))
    (define actual (hash-set snap 'branch "actual/snapshot"))
    (define later (hash-set actual 'head (make-string 40 #\d)))
    (check-eq? (run-wrapper root (lambda () 'rejected) #:snap1 actual #:snap2 later) 'rejected)
    (define d (car (diags root)))
    (check-equal? (hash-ref d 'expected-branch) "campaign/x/w2")
    (check-equal? (hash-ref d 'resolved-branch) "actual/snapshot")
    (check-equal? (hash-ref (hash-ref d 'snapshot-after) 'head) (make-string 40 #\d)))
  (test-case "anonymous same-second collision regenerates ID and preserves original bytes"
    (define root (fresh-base!))
    (define id1 (make-string 32 #\a))
    (define id2 (make-string 32 #\b))
    (define p1 (publish-verification-diagnostic! root plan (rec) #:nonce id1))
    (define bytes (file->bytes (cdr p1)))
    (define ids (box (list id1 id2)))
    (define p2
      (parameterize ([current-gsd-diagnostic-id-source (lambda ()
                                                         (begin0 (car (unbox ids))
                                                           (set-box! ids (cdr (unbox ids)))))])
        (publish-verification-diagnostic! root plan (rec))))
    (check-eq? (car p2) 'published)
    (check-equal? (file->bytes (cdr p1)) bytes)
    (check-equal? (length (diags root)) 2))
  (test-case "concurrent same-ID publication has one winner without overwrite"
    (define root (fresh-base!))
    (define gate (make-semaphore 0))
    (define results (make-async-channel))
    (define workers
      (for/list ([i (in-range 8)])
        (thread
         (lambda ()
           (semaphore-wait gate)
           (async-channel-put
            results
            (publish-verification-diagnostic! root plan (rec) #:nonce (make-string 32 #\c)))))))
    (for ([i (in-range 8)])
      (semaphore-post gate))
    (define outcomes
      (for/list ([i (in-range 8)])
        (async-channel-get results)))
    (for-each thread-wait workers)
    (check-equal? (count (lambda (o) (eq? (car o) 'published)) outcomes) 1)
    (check-equal? (count (lambda (o) (eq? (car o) 'contained)) outcomes) 7)
    (check-equal? (length (diags root)) 1)
    (check-equal? (filter (lambda (p) (equal? (path-get-extension p) #".tmp"))
                          (directory-list (verification-diagnostics-dir root plan)))
                  '()))
  (test-case "rejected diagnostic cannot clear existing remote-pending marker"
    (define root (fresh-base!))
    (record-remote-pending! root plan 2 "campaign/x/w2" head "unpublished")
    (define before (load-remote-pending root plan 2))
    (check-eq? (run-wrapper root (lambda () 'rejected)) 'rejected)
    (check-equal? (load-remote-pending root plan 2) before)
    (check-false (load-delivery-journal root plan 2)))
  (test-case "diagnostic ID-source failure preserves exact verifier exception"
    (define root (fresh-base!))
    (define original (exn:fail "original verifier error" (current-continuation-marks)))
    (parameterize ([current-gsd-diagnostic-id-source (lambda () (error 'diagnostic "ID failed"))])
      (check-exn (lambda (e) (eq? e original))
                 (lambda () (run-wrapper root (lambda () (raise original)))))))
  (test-case "diagnostic field-construction failure preserves exact verifier exception"
    (define root (fresh-base!))
    (define original (exn:fail "original construction case" (current-continuation-marks)))
    (check-exn (lambda (e) (eq? e original))
               (lambda () (run-wrapper root (lambda () (raise original)) #:attempt 'invalid))))
  (test-case "verdict predicate runs once despite diagnostic failure"
    (define root (fresh-base!))
    (define calls (box 0))
    (parameterize ([current-gsd-diagnostic-id-source (lambda () (error 'diagnostic "failed"))])
      (check-eq? (verify-with-delivery-receipt root
                                               plan
                                               2
                                               root
                                               (lambda () 'rejected)
                                               #:snapshot (lambda (_) snap)
                                               #:approved? (lambda (_)
                                                             (set-box! calls (add1 (unbox calls)))
                                                             #f)
                                               #:evidence values)
                 'rejected))
    (check-equal? (unbox calls) 1))
  (test-case "symlinked diagnostic directory fails closed"
    (define root (fresh-base!))
    (define other (fresh-base!))
    (define dir (verification-diagnostics-dir root plan))
    (make-parent-directory* dir)
    (make-file-or-directory-link other dir)
    (check-eq? (car (publish-verification-diagnostic! root plan (rec))) 'contained)
    (check-equal? (directory-list other) '()))
  (test-case "cross-plan record cannot publish under another campaign"
    (define root (fresh-base!))
    (check-eq?
     (car (publish-verification-diagnostic! root plan (hash-set (rec) 'plan-id (make-string 64 #\f))))
     'contained)
    (check-equal? (diags root) '())))

(module+ test
  (test-case "diagnostic logger failure preserves result and exact verifier exception"
    (define root (fresh-base!))
    (define original (exn:fail "original logging case" (current-continuation-marks)))
    (parameterize ([current-gsd-diagnostic-id-source (lambda () (error 'diagnostic "failed"))]
                   [current-gsd-diagnostic-logger (lambda (_) (error 'logging "failed"))])
      (check-eq? (run-wrapper root (lambda () 'rejected)) 'rejected)
      (check-exn (lambda (e) (eq? e original))
                 (lambda () (run-wrapper root (lambda () (raise original)))))))
  (test-case "before-snapshot and verdict errors are diagnosed and re-raised unchanged"
    (for ([position '(before verdict)])
      (define root (fresh-base!))
      (define original (exn:fail "snapshot/verdict original" (current-continuation-marks)))
      (check-exn (lambda (e) (eq? e original))
                 (lambda ()
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'rejected)
                                                 #:snapshot (lambda (_)
                                                              (if (eq? position 'before)
                                                                  (raise original)
                                                                  snap))
                                                 #:approved? (lambda (_) (raise original))
                                                 #:evidence values)))
      (check-equal? (hash-ref (car (diags root)) 'outcome) "error")))
  (test-case "detached missing snapshot keeps expected ref but unresolved actual branch"
    (define root (fresh-base!))
    (check-eq? (run-wrapper root (lambda () 'rejected) #:snap1 #f #:snap2 #f) 'rejected)
    (define d (car (diags root)))
    (check-equal? (hash-ref d 'expected-branch) "campaign/x/w2")
    (check-false (hash-ref d 'branch-resolved?))
    (check-equal? (hash-ref d 'resolved-branch) "")
    (check-false (hash-ref d 'snapshot-before))))

(module+ test
  (test-case "non-exception diagnostic failures cannot replace results or original errors"
    (define root (fresh-base!))
    (define original (exn:fail "original arbitrary raise case" (current-continuation-marks)))
    (parameterize ([current-gsd-diagnostic-id-source (lambda () (raise 'id-failed))]
                   [current-gsd-diagnostic-logger (lambda (_) (raise 'logger-failed))])
      (check-eq? (run-wrapper root (lambda () 'rejected)) 'rejected)
      (check-exn (lambda (e) (eq? e original))
                 (lambda () (run-wrapper root (lambda () (raise original)))))))
  (test-case "before-snapshot error retains available branch/base context"
    (define root (fresh-base!))
    (define ctx
      (make-branch-delivery-context #:repo-root root #:branch "campaign/x/w2" #:base-commit base-sha))
    (parameterize ([current-gsd-delivery-branch-context ctx])
      (check-exn exn:fail?
                 (lambda ()
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'rejected)
                                                 #:snapshot (lambda (_) (error 'snapshot "failed"))
                                                 #:approved? (lambda (_) #f)
                                                 #:evidence values))))
    (define d (car (diags root)))
    (check-equal? (hash-ref d 'expected-branch) "campaign/x/w2")
    (check-equal? (hash-ref d 'base) base-sha)
    (check-true (hash-ref d 'base-bound?)))
  (test-case "omitting any required boolean field is invalid and cannot publish"
    (define root (fresh-base!))
    (define rec
      (hash-set (make-verification-diagnostic plan
                                              2
                                              "attempt-1"
                                              2
                                              #t
                                              base-sha
                                              #t
                                              "campaign/x/w2"
                                              ""
                                              #f
                                              "/repo"
                                              "/repo"
                                              head
                                              base-sha
                                              base-sha
                                              100
                                              101
                                              "rejected"
                                              #t
                                              #t
                                              "rejected"
                                              ""
                                              "")
                'verification-id
                (make-string 32 #\a)))
    (for ([key
           '(attempt-fence-bound? base-bound? branch-resolved? snapshot-before? snapshot-unchanged?)])
      (define bad (hash-remove rec key))
      (check-false (valid-verification-diagnostic? bad))
      (check-eq? (car (publish-verification-diagnostic! root plan bad)) 'contained))))

(module+ test
  (test-case "diagnostic failure logs redact synthetic credential assignments"
    (define root (fresh-base!))
    (define messages (box '()))
    (define fixture "api_key=FAKE_REVIEW_FIXTURE_ONLY")
    (parameterize ([current-gsd-diagnostic-id-source (lambda () (error 'fixture fixture))]
                   [current-gsd-diagnostic-logger (lambda (m)
                                                    (set-box! messages (cons m (unbox messages))))])
      (check-eq? (run-wrapper root (lambda () 'rejected)) 'rejected))
    (check-equal? (length (unbox messages)) 1)
    (check-false (regexp-match? #rx"FAKE_REVIEW_FIXTURE_ONLY" (car (unbox messages))))))
