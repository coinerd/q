#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @covers extensions/gsd/review-rejection.rkt
;; @covers extensions/gsd/delivery-journal.rkt
;; @covers extensions/gsd/policy.rkt
;;
;; Automatic-review slice A state law: durable explicit rejection
;; authorization, bounded persistent retry accounting (default 1), the
;; resolve-review-rejection! state port, and cross-attempt receipt
;; supersession authorized ONLY by the prior exact rejection marker plus a
;; live record guard.
(require rackunit
         racket/file
         racket/port
         racket/list
         racket/runtime-path
         racket/string
         json
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/review-rejection.rkt")

;; RED-first: new exports in EXISTING modules arrive with slice A;
;; dynamic-require keeps those failures inside the test cases instead of
;; aborting module load. The review-rejection module itself is required
;; directly so its struct accessors are checked at compile time.
(define-runtime-path here ".")
(define journal-module (build-path here "../extensions/gsd/delivery-journal.rkt"))
(define rejection-module (build-path here "../extensions/gsd/review-rejection.rkt"))
(define policy-module (build-path here "../extensions/gsd/policy.rkt"))

(define (jport name)
  (dynamic-require journal-module name))
(define (pport name)
  (dynamic-require policy-module name))

(define plan (make-string 64 #\a))
(define branch "campaign/aaaaaaaa/w0")
(define receipt
  (hasheq 'repo
          "/repo"
          'branch
          branch
          'head
          (make-string 40 #\b)
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"
          'verified-at
          1
          'evidence
          "full Verify passed"
          'attempt-id
          "attempt-1"
          'attempt-fence
          4))
(define findings '("w1-r1 evidence trio missing" "review head drifted from receipt"))

(define (with-root f)
  (define root (make-temporary-file "review-rejection-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))

(define (journal-with-receipt! root [stage "context-ready"])
  (record-delivery-receipt! root plan 0 receipt)
  (unless (equal? stage "context-ready")
    (update-delivery-journal! root plan 0 (hasheq 'stage stage))))

(define (rejected-record [status 'done])
  (define w (make-campaign-wave 0 "w0" status 1 (campaign-attempt "attempt-1" 4 0)))
  (set-campaign-wave-artifact-ledger!
   w
   (list (make-campaign-artifact-entry
          "attempt-1"
          branch
          (path->string (build-path (find-system-path 'temp-dir) "review-rejection-fixture" "w0"))
          (make-string 40 #\0))))
  (make-campaign-record plan
                        (make-campaign-manifest 1 "w0" '() '() "constraints")
                        (list w)
                        #f
                        4
                        'test
                        0
                        0))

(define (wave-of record)
  (car (campaign-record-waves record)))

;; attempt-2 receipt: same branch, fresh head, fresh attempt identity.
(define (attempt-2-receipt [head (make-string 40 #\d)])
  (hash-set* receipt 'head head 'attempt-id "attempt-2" 'attempt-fence 5 'verified-at 2))

(module+ test
  (test-case "bounded retry policy parameter defaults to exactly one retry"
    (define retries (pport 'current-gsd-wave-review-rejection-retries))
    (check-equal? (retries) 1 "default retry budget is 1")
    (check-exn exn:fail:contract? (lambda () (retries -1)))
    (check-equal? (parameterize ([retries 3])
                    (retries))
                  3))

  (test-case "record-review-rejection! durably authorizes the exact premerge receipt"
    (with-root (lambda (root)
                 (journal-with-receipt! root)
                 (define saved
                   ((jport 'record-review-rejection!) root
                                                      plan
                                                      0
                                                      #:findings findings
                                                      #:expected-attempt-id "attempt-1"
                                                      #:expected-fence 4
                                                      #:max-rejection-retries 1))
                 (define marker (hash-ref saved 'review-rejection))
                 (check-equal? (hash-ref marker 'attempt-id) "attempt-1")
                 (check-equal? (hash-ref marker 'attempt-fence) 4)
                 (check-equal? (hash-ref marker 'branch) branch)
                 (check-equal? (hash-ref marker 'head) (make-string 40 #\b))
                 (check-equal? (hash-ref marker 'findings) findings)
                 (check-equal? (hash-ref marker 'stage) "context-ready")
                 (check-equal? (hash-ref saved 'review-rejection-count) 1)
                 (check-false (hash-ref marker 'superseded-by-attempt #f))
                 ;; restart replay of the identical rejection consumes no second retry
                 (define replayed
                   ((jport 'record-review-rejection!) root
                                                      plan
                                                      0
                                                      #:findings findings
                                                      #:expected-attempt-id "attempt-1"
                                                      #:expected-fence 4
                                                      #:max-rejection-retries 1))
                 (check-equal? (hash-ref replayed 'review-rejection-count) 1)
                 (check-equal? (hash-ref replayed 'review-rejection) marker)
                 ;; a DIFFERENT rejection for the same receipt is a refusal
                 (check-exn exn:fail?
                            (lambda ()
                              ((jport 'record-review-rejection!) root
                                                                 plan
                                                                 0
                                                                 #:findings '("different finding")
                                                                 #:expected-attempt-id "attempt-1"
                                                                 #:expected-fence 4
                                                                 #:max-rejection-retries 1))))))

  (test-case "rejection authorization refuses mismatched identity and postmerge stages"
    (with-root (lambda (root)
                 (journal-with-receipt! root)
                 (check-exn exn:fail?
                            (lambda ()
                              ((jport 'record-review-rejection!) root
                                                                 plan
                                                                 0
                                                                 #:findings findings
                                                                 #:expected-attempt-id "attempt-2"
                                                                 #:expected-fence 4
                                                                 #:max-rejection-retries 1)))
                 (check-exn exn:fail?
                            (lambda ()
                              ((jport 'record-review-rejection!) root
                                                                 plan
                                                                 0
                                                                 #:findings findings
                                                                 #:expected-attempt-id "attempt-1"
                                                                 #:expected-fence 5
                                                                 #:max-rejection-retries 1)))
                 (check-false (hash-ref (load-delivery-journal root plan 0) 'review-rejection #f))
                 (for ([stage '("implementation-review" "implementation-pr" "implementation-ci")])
                   (update-delivery-journal! root plan 0 (hasheq 'stage stage))
                   (check-not-exn (lambda ()
                                    ((jport 'record-review-rejection!) root
                                                                       plan
                                                                       0
                                                                       #:findings findings
                                                                       #:expected-attempt-id
                                                                       "attempt-1"
                                                                       #:expected-fence 4
                                                                       #:max-rejection-retries 2))
                                  (format "premerge stage ~a may be rejected" stage)))
                 ;; postmerge the implementation merge is anchored; rejection fails closed
                 (update-delivery-journal! root plan 0 (hasheq 'stage "implementation-merged"))
                 (define before-postmerge (load-delivery-journal root plan 0))
                 (check-exn exn:fail?
                            (lambda ()
                              ((jport 'record-review-rejection!) root
                                                                 plan
                                                                 0
                                                                 #:findings findings
                                                                 #:expected-attempt-id "attempt-1"
                                                                 #:expected-fence 4
                                                                 #:max-rejection-retries 5)))
                 (check-equal? (load-delivery-journal root plan 0)
                               before-postmerge
                               "the postmerge refusal leaves the recorded marker untouched"))))

  (test-case "rejection markers cannot be smuggled through ordinary journal updates"
    (with-root
     (lambda (root)
       (journal-with-receipt! root)
       (check-exn exn:fail?
                  (lambda ()
                    (update-delivery-journal! root plan 0 (hasheq 'review-rejection (hasheq))))))))

  (test-case "resolve-review-rejection! persists findings and retries without a new attempt"
    (with-root
     (lambda (root)
       (journal-with-receipt! root)
       (define record (rejected-record))
       (define resolution
         (resolve-review-rejection! root
                                    plan
                                    0
                                    record
                                    #:findings findings
                                    #:expected-attempt-id "attempt-1"
                                    #:expected-fence 4))
       (check-eq? (review-rejection-resolution-status resolution) 'resolved)
       (check-false (review-rejection-resolution-reason resolution))
       (check-equal? (review-rejection-resolution-consumed-retries resolution) 1)
       (check-equal? (review-rejection-resolution-retry-budget resolution) 1)
       ;; the durable marker carries the findings verbatim
       (define marker (review-rejection-resolution-marker resolution))
       (check-equal? (hash-ref marker 'findings) findings)
       (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection) marker)
       ;; the attempt is superseded and the wave re-enters ordinary retry
       (define w (wave-of record))
       (check-eq? (campaign-wave-status w) 'failed)
       (check-equal? (campaign-wave-attempt-count w) 1 "no attempt id was generated")
       (check-equal? (campaign-attempt-id (campaign-wave-current-attempt w)) "attempt-1")
       (check-not-false (string-contains? (wave-failure-reason w) "w1-r1 evidence trio missing"))
       (check-eq? (campaign-artifact-entry-terminal-status (car (wave-artifact-ledger w)))
                  'superseded)
       ;; a marker mismatch check: the journal receipt identity is binding
       (check-true ((jport 'review-rejection-matches-receipt?) marker receipt))
       (check-false ((jport 'review-rejection-matches-receipt?) marker (attempt-2-receipt))))))

  (test-case "resolve is idempotent across restarts and re-derives the record transition"
    (with-root (lambda (root)
                 (journal-with-receipt! root)
                 (define first
                   (resolve-review-rejection! root
                                              plan
                                              0
                                              (rejected-record)
                                              #:findings findings
                                              #:expected-attempt-id "attempt-1"
                                              #:expected-fence 4))
                 (check-eq? (review-rejection-resolution-status first) 'resolved)
                 ;; restart: the persisted record reloads as failed with the marker durable
                 (define reloaded (rejected-record 'failed))
                 (stamp-wave-failure! (wave-of reloaded) "independent review rejected attempt-1")
                 (define replay
                   (resolve-review-rejection! root
                                              plan
                                              0
                                              reloaded
                                              #:findings findings
                                              #:expected-attempt-id "attempt-1"
                                              #:expected-fence 4))
                 (check-eq? (review-rejection-resolution-status replay) 'resolved)
                 (check-equal? (review-rejection-resolution-consumed-retries replay)
                               1
                               "restart replay never double-consumes the retry budget"))))

  (test-case "resolve refuses cancelled, budget-paused, stale and unreviewed waves with zero effects"
    (with-root
     (lambda (root)
       (journal-with-receipt! root)
       (define (refuse! record who)
         (define resolution
           (resolve-review-rejection! root
                                      plan
                                      0
                                      record
                                      #:findings findings
                                      #:expected-attempt-id "attempt-1"
                                      #:expected-fence 4))
         (check-eq? (review-rejection-resolution-status resolution) 'refused who)
         (check-not-false (string? (review-rejection-resolution-reason resolution)) who)
         (check-false (hash-ref (load-delivery-journal root plan 0) 'review-rejection #f)
                      "refusals write nothing"))
       (define cancelled (rejected-record))
       (set-campaign-cancellation! cancelled (make-campaign-cancellation "stop" 0))
       (refuse! cancelled "cancelled campaign")
       (define paused (rejected-record))
       (set-campaign-record-budget-pause! paused (campaign-budget-pause 'max-cost 1 2 "over" 0))
       (refuse! paused "budget pause")
       (refuse! (rejected-record 'in-progress) "wave not awaiting delivery review")
       (define stale (rejected-record))
       (set-campaign-fence-token! stale 9)
       (refuse! stale "stale coordinator fence")
       (define replaced (rejected-record))
       (set-campaign-wave-current-attempt! (wave-of replaced) (campaign-attempt "attempt-2" 5 0))
       (refuse! replaced "live attempt is not the rejected attempt")
       ;; malformed findings never authorize anything
       (define malformed
         (resolve-review-rejection! root
                                    plan
                                    0
                                    (rejected-record)
                                    #:findings '("")
                                    #:expected-attempt-id "attempt-1"
                                    #:expected-fence 4))
       (check-eq? (review-rejection-resolution-status malformed) 'refused)
       (check-false (hash-ref (load-delivery-journal root plan 0) 'review-rejection #f)))))

  (test-case "the bounded retry budget is persistent and refuses when exhausted"
    (with-root
     (lambda (root)
       (journal-with-receipt! root)
       ;; budget 0: no rejection-driven retry is ever authorized
       (define zero
         (resolve-review-rejection! root
                                    plan
                                    0
                                    (rejected-record)
                                    #:findings findings
                                    #:expected-attempt-id "attempt-1"
                                    #:expected-fence 4
                                    #:max-rejection-retries 0))
       (check-eq? (review-rejection-resolution-status zero) 'refused)
       (check-not-false (string-contains? (review-rejection-resolution-reason zero) "budget"))
       (check-false (hash-ref (load-delivery-journal root plan 0) 'review-rejection #f))
       ;; first rejection consumes the single default retry
       (check-eq? (review-rejection-resolution-status (resolve-review-rejection! root
                                                                                 plan
                                                                                 0
                                                                                 (rejected-record)
                                                                                 #:findings findings
                                                                                 #:expected-attempt-id
                                                                                 "attempt-1"
                                                                                 #:expected-fence 4))
                  'resolved)
       ;; the retry attempt verified and was itself rejected: budget exhausted
       ((jport 'reconcile-review-rejected-receipt!) root
                                                    plan
                                                    0
                                                    (attempt-2-receipt)
                                                    #:guard (lambda () #t))
       (define rec2 (rejected-record))
       (set-campaign-fence-token! rec2 5)
       (set-campaign-wave-current-attempt! (wave-of rec2) (campaign-attempt "attempt-2" 5 0))
       (define second
         (resolve-review-rejection! root
                                    plan
                                    0
                                    rec2
                                    #:findings '("second attempt still missing evidence")
                                    #:expected-attempt-id "attempt-2"
                                    #:expected-fence 5))
       (check-eq? (review-rejection-resolution-status second) 'refused)
       (check-not-false (string-contains? (review-rejection-resolution-reason second) "budget"))
       (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection-count) 1))))

  (test-case "cross-attempt supersession requires the exact prior rejection and a live guard"
    (with-root (lambda (root)
                 (journal-with-receipt! root)
                 ((jport 'record-review-rejection!) root
                                                    plan
                                                    0
                                                    #:findings findings
                                                    #:expected-attempt-id "attempt-1"
                                                    #:expected-fence 4
                                                    #:max-rejection-retries 1)
                 (define superseded
                   ((jport 'reconcile-review-rejected-receipt!) root
                                                                plan
                                                                0
                                                                (attempt-2-receipt)
                                                                #:guard (lambda () #t)))
                 ;; the old receipt is preserved immutably; generation advances exactly once
                 (check-equal? (hash-ref (hash-ref superseded 'receipt) 'attempt-id) "attempt-2")
                 (check-equal? (hash-ref (hash-ref superseded 'receipt) 'head) (make-string 40 #\d))
                 (check-equal? (hash-ref superseded 'receipt-history) (list receipt))
                 (check-equal? (hash-ref superseded 'stage) "context-ready")
                 (check-equal? (hash-ref (hash-ref superseded 'review-rejection)
                                         'superseded-by-attempt)
                               "attempt-2")
                 (check-equal? (length (hash-ref superseded 'receipt-history)) 1)
                 ;; idempotent replay never appends the old receipt a second time
                 (define replay
                   ((jport 'reconcile-review-rejected-receipt!) root
                                                                plan
                                                                0
                                                                (attempt-2-receipt)
                                                                #:guard (lambda () #t)))
                 (check-equal? (hash-ref replay 'receipt-history) (list receipt)))))

  (test-case "supersession refuses unmarked, guarded-off, stale and postmerge transitions"
    (with-root
     (lambda (root)
       ;; no durable marker at all: unmarked cross-attempt supersession refuses
       (journal-with-receipt! root)
       (check-exn exn:fail?
                  (lambda ()
                    ((jport 'reconcile-review-rejected-receipt!) root
                                                                 plan
                                                                 0
                                                                 (attempt-2-receipt)
                                                                 #:guard (lambda () #t))))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 0) 'receipt) 'attempt-id)
                     "attempt-1")
       ((jport 'record-review-rejection!) root
                                          plan
                                          0
                                          #:findings findings
                                          #:expected-attempt-id "attempt-1"
                                          #:expected-fence 4
                                          #:max-rejection-retries 1)
       ;; a dead live record guard refuses and writes nothing
       (check-exn exn:fail?
                  (lambda ()
                    ((jport 'reconcile-review-rejected-receipt!) root
                                                                 plan
                                                                 0
                                                                 (attempt-2-receipt)
                                                                 #:guard (lambda () #f))))
       (check-equal? (hash-ref (load-delivery-journal root plan 0) 'receipt-history '()) '())
       ;; different branch: stale/different branch identity refuses
       (check-exn exn:fail?
                  (lambda ()
                    ((jport 'reconcile-review-rejected-receipt!)
                     root
                     plan
                     0
                     (hash-set (attempt-2-receipt) 'branch "campaign/aaaaaaaa/w0-r1")
                     #:guard (lambda () #t))))
       ;; same attempt identity is the repair-tail protocol, not supersession
       (check-exn exn:fail?
                  (lambda ()
                    ((jport 'reconcile-review-rejected-receipt!)
                     root
                     plan
                     0
                     (hash-set* (attempt-2-receipt) 'attempt-id "attempt-1" 'attempt-fence 4)
                     #:guard (lambda () #t))))
       ;; postmerge: the merged implementation cannot be undone
       (update-delivery-journal! root plan 0 (hasheq 'stage "implementation-merged"))
       (check-exn exn:fail?
                  (lambda ()
                    ((jport 'reconcile-review-rejected-receipt!) root
                                                                 plan
                                                                 0
                                                                 (attempt-2-receipt)
                                                                 #:guard (lambda () #t))))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 0) 'receipt) 'attempt-id)
                     "attempt-1"))))

  (test-case "live-authorization requires the exact live attempt, uncancelled and within budget"
    (define live (rejected-record))
    (check-true (review-rejection-live-authorized? live plan 0 "attempt-1" 4))
    (check-false (review-rejection-live-authorized? live (make-string 64 #\f) 0 "attempt-1" 4))
    (check-false (review-rejection-live-authorized? live plan 1 "attempt-1" 4))
    (check-false (review-rejection-live-authorized? live plan 0 "attempt-2" 4))
    (check-false (review-rejection-live-authorized? live plan 0 "attempt-1" 5))
    (define cancelled (rejected-record))
    (set-campaign-cancellation! cancelled (make-campaign-cancellation "stop" 0))
    (check-false (review-rejection-live-authorized? cancelled plan 0 "attempt-1" 4))
    (define paused (rejected-record))
    (set-campaign-record-budget-pause! paused (campaign-budget-pause 'max-tokens 1 2 "over" 0))
    (check-false (review-rejection-live-authorized? paused plan 0 "attempt-1" 4))
    (check-false (review-rejection-live-authorized? #f plan 0 "attempt-1" 4)))

  (test-case "slice-A modules obey the module size law"
    (define max-lines 900)
    (for ([path (in-list (list journal-module
                               rejection-module
                               policy-module
                               (build-path here "../extensions/gsd/delivery-receipt.rkt")))])
      (define lines (length (string-split (call-with-input-file path port->string) "\n")))
      (check-true (< lines max-lines) (format "~a exceeds ~a lines" path max-lines)))))
