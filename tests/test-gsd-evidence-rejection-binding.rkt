#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @covers scripts/gsd-delivery-auto-review.rkt
;;
;; Native port of `tests/test-gsd-evidence-rejection-binding.py` (deleted in the
;; same change): the seven bounded recovery regressions that were driven with
;; `unittest` + `unittest.mock` against `scripts/gsd-delivery.py` now pin the
;; Racket implementation (`recover-rejected-artifact` in
;; scripts/gsd-delivery-auto-review.rkt) through injected seams.
;;
;; Fixture law: no owned gate execution, no external process, no real git. The
;; Python patches (`blob_or_none`, `read_datum`, `digest`) collapse into two
;; fakes: `fake-git` serves the published tip blob, the receipt-head blob and the
;; diff text the content digest hashes; `fake-run` is the trusted-datum reader.
;; Every case keeps the Python case's assertion (a recovered
;; `review-rejected` payload or a typed refusal):
;;
;;   test_registered_rejection_recovers -> "a registered rejection recovers as review-rejected"
;;   test_missing_authority_refused     -> "a missing recovery authority is refused"
;;   test_drift_refused                 -> "authority drift across the five pinned keys is refused"
;;   test_receipt_drift_refused         -> "receipt drift across all six receipt keys is refused"
;;   test_ack_guards_refused            -> "ack guards (source-read, raw-output-read, superset) are refused"
;;   test_revision_zero                 -> "revision zero recovers without evidence-registration keys"
;;   test_manual_compatibility          -> "manual compatibility recovers without a review-mode"

(require rackunit
         json
         racket/list
         racket/string
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-auto-review.rkt"
         (only-in "../scripts/gsd-delivery-pr.rkt" content-digest))

;; ============================================================
;; Fixture: the pinned receipt, the authenticated authority, the seams
;; ============================================================

(define receipt-head (make-string 40 #\a))
(define tip (make-string 40 #\c))
;; The fixture datums use `hasheq`, like every datum the controller reads back
;; from JSON: the recovery compares the committed review against the durable
;; receipt with `equal?`, which distinguishes the two hash kinds.
(define owned-proof (hasheq 'sha256 "green"))
(define receipt
  (hasheq 'head
          receipt-head
          'tree
          (make-string 40 #\b)
          'branch
          "work"
          'attempt-id
          "attempt"
          'attempt-fence
          1
          'owned-proof
          owned-proof))
(define authority
  (hasheq 'receipt
          receipt
          'generation
          2
          'reference
          owned-proof
          'evidence-revision
          1
          'evidence-registration-sha256
          "registered"
          'red
          (hasheq 'reference (hasheq 'sha256 "red"))))

;; The injected git seam. The tip and the receipt head must serve DIFFERENT blobs
;; (the "published evidence tail" guard), and the diff text is fixed so the
;; content digest is reproducible without a repository.
(define (fake-git args)
  (cond
    [(equal? (car args) "show")
     (define target (cadr args))
     (cond
       [(string-prefix? target (string-append tip ":")) "published"]
       [(string-prefix? target (string-append receipt-head ":")) "old"]
       [else (error 'fake-git "unexpected blob read: ~s" args)])]
    [(equal? (car args) "diff") "fixed-diff-text"]
    [else (error 'fake-git "unexpected git call: ~s" args)]))

;; The injected `run` seam: the trusted datum reader returns the current case's
;; committed review (Python's `patch.object(m, 'read_datum', ...)`).
(define current-datum (make-parameter #f))
(define (fake-run _program _args _timeout)
  (jsexpr->string (current-datum)))

;; Python `dict(base, **extra)` over immutable hashes.
(define (hash-put base extra)
  (for/fold ([acc base]) ([(key value) (in-hash extra)])
    (hash-set acc key value)))

;; The request the controller would have written for this receipt, exactly as
;; Python's `setUp` builds it: the datum pins ITS audit binding.
(define pinned-request
  (hash-put receipt
            (hash 'plan-id
                  "plan"
                  'wave
                  1
                  'generation
                  2
                  'content-digest
                  (content-digest fake-git "refs/remotes/origin/main" receipt-head)
                  'proof-sha256
                  "green"
                  'evidence-revision
                  1
                  'evidence-registration-sha256
                  "registered"
                  'owned-authority
                  authority)))

(define (narrative text)
  (apply string-append (make-list 20 text)))

;; One committed REQUEST_CHANGES review. `binding` is used for BOTH the datum's
;; `audit-binding` and the acknowledgement's `binding`, like the Python fixture
;; (one shared dict), so a binding rewrite must reach both.
(define (rejection-datum binding)
  (hash-put pinned-request
            (hash 'stage
                  "implementation-review"
                  'wave
                  "W1"
                  'verdict
                  "REQUEST_CHANGES"
                  'reviewed-sha
                  receipt-head
                  'timestamp
                  "2026-01-01T00:00:00Z"
                  'review-mode
                  owned-audit-mode
                  'owned-proof
                  owned-proof
                  'owned-red
                  (hash 'sha256 "red")
                  'audit-binding
                  binding
                  'audit-ack
                  (hash 'mode
                        owned-audit-mode
                        'verification-origin
                        "audited-owned"
                        'source-read
                        #t
                        'raw-output-read
                        #t
                        'binding
                        binding
                        'red-ack
                        (hash 'proof-sha256 "red" 'raw-output-read #t))
                  'reviewer
                  "Independent reviewer"
                  'scope
                  (narrative "Inspected implementation source. ")
                  'report
                  (narrative "Observed a concrete correctness defect. ")
                  'findings
                  (list "Fix incorrect behavior."))))

(define (pinned-datum)
  (rejection-datum (audit-binding pinned-request)))

;; Python `Recovery.recover()`: fixed tip/relative/plan/wave/generation and one
;; datum per case; the authority is supplied unless the case omits it.
(define (recover datum [authority-for-case authority])
  (parameterize ([current-datum datum])
    (recover-rejected-artifact fake-run fake-git "plan" 1 2 receipt tip "review" authority-for-case)))

;; Provided so a debug script can attribute a refusal to its guard without
;; re-deriving the fixture.
(provide receipt
         authority
         pinned-request
         pinned-datum
         rejection-datum
         recover
         fake-git
         fake-run)

;; ============================================================
;; Cases
;; ============================================================

(module+ test
  (test-case "a registered rejection recovers as review-rejected"
    (check-equal? (hash-ref (recover (pinned-datum)) 'status) "review-rejected"))

  (test-case "a missing recovery authority is refused"
    (check-exn exn:fail:delivery-action? (lambda () (recover (pinned-datum) #f))))

  (test-case "authority drift across the five pinned keys is refused"
    (for ([drift (in-list (list (cons 'evidence-revision 2)
                                (cons 'evidence-registration-sha256 "other")
                                (cons 'reference (hasheq 'sha256 "other"))
                                (cons 'generation 3)
                                (cons 'red (hasheq 'reference (hasheq 'sha256 "other")))))])
      (define drifted (hash-set authority (car drift) (cdr drift)))
      (check-exn exn:fail:delivery-action?
                 (lambda () (recover (pinned-datum) drifted))
                 (format "authority ~a drift must be refused" (car drift)))))

  (test-case "receipt drift across all six receipt keys is refused"
    (for ([key (in-list '(head tree branch attempt-id attempt-fence owned-proof))])
      (define drifted-receipt (hash-set receipt key "drift"))
      (define drifted (hash-set authority 'receipt drifted-receipt))
      (check-exn exn:fail:delivery-action?
                 (lambda () (recover (pinned-datum) drifted))
                 (format "authority receipt ~a drift must be refused" key))))

  (test-case "ack guards (source-read, raw-output-read, superset) are refused"
    (for ([key (in-list '(source-read raw-output-read))])
      (define datum (pinned-datum))
      (define ack (hash-ref datum 'audit-ack))
      (check-exn exn:fail:delivery-action?
                 (lambda () (recover (hash-set datum 'audit-ack (hash-set ack key #f))))
                 (format "ack ~a = #f must be refused" key)))
    (define datum (pinned-datum))
    (define ack (hash-ref datum 'audit-ack))
    (define superset (hash-set (hash-ref ack 'binding) 'extra "untrusted"))
    (check-exn exn:fail:delivery-action?
               (lambda () (recover (hash-set datum 'audit-ack (hash-set ack 'binding superset))))
               "an ack binding superset must be refused"))

  (test-case "revision zero recovers without evidence-registration keys"
    (define zero-authority
      (hash-remove (hash-set authority 'evidence-revision 0) 'evidence-registration-sha256))
    (define zero-binding
      (hash-remove (hash-remove (audit-binding pinned-request) 'evidence-revision)
                   'evidence-registration-sha256))
    (check-equal? (hash-ref (recover (rejection-datum zero-binding) zero-authority) 'status)
                  "review-rejected"))

  (test-case "manual compatibility recovers without a review-mode"
    (check-equal? (hash-ref (recover (hash-remove (pinned-datum) 'review-mode) #f) 'status)
                  "review-rejected")))
