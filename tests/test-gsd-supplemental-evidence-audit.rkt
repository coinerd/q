#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @covers extensions/gsd/review-audit-format.rkt
;; @covers scripts/gsd-delivery-auto-review.rkt
;;
;; Native home of the four consumer-schema cases of
;; tests/test-gsd-supplemental-audit-binding.py (deleted in the same change):
;;   test_explicit_pin_and_ack_key_identity   -> "supplemental request and ACK explicitly bind P"
;;   test_missing_malformed_or_wrong_pin_refuses -> "supplemental request cannot omit or substitute P"
;;   test_ordinary_old_binding_unchanged      -> "ordinary old bindings remain unchanged"
;;   test_prepare_copies_pin_from_trusted_consumer_seam -> "owned-request binding copies the P pin"
(require rackunit
         json
         "../extensions/gsd/review-audit-format.rkt"
         (only-in "../scripts/gsd-delivery-auto-review.rkt"
                  bind-owned-request
                  reauthenticate-owned-request
                  exn:fail:review-infra?
                  [audit-binding delivery-audit-binding]))
(define sha (make-string 64 #\a))
(define other (make-string 64 #\b))
(define authority
  (hasheq 'reference (hasheq 'supplemental-contract (hasheq)) 'supplemental-promotion-sha256 sha))
(define request
  (hasheq 'owned-authority
          authority
          'evidence-revision
          1
          'evidence-registration-sha256
          other
          'supplemental-promotion-sha256
          sha))
;; JSON-normalized comparison: a datum read back from JSON is `hasheq` while
;; `audit-binding` builds `hash`, and `equal?` distinguishes the two hash kinds.
(define (normalized-json value)
  (string->jsexpr (jsexpr->string value)))
(define (refusal-message thunk)
  (with-handlers ([exn:fail? exn-message])
    (thunk)
    #f))
(module+ test
  (test-case "supplemental request and ACK explicitly bind P"
    (define binding (audit-binding request))
    (check-equal? (hash-ref binding 'supplemental-promotion-sha256 #f) sha)
    (define ack
      (hasheq 'mode
              owned-audit-mode
              'verification-origin
              "audited-owned"
              'source-read
              #t
              'raw-output-read
              #t
              'binding
              binding))
    (check-true (valid-audit-ack? ack binding))
    (check-false (valid-audit-ack?
                  (hash-set ack 'binding (hash-remove binding 'supplemental-promotion-sha256))
                  binding)))
  (test-case "supplemental request cannot omit or substitute P"
    (check-exn exn:fail?
               (lambda () (audit-binding (hash-remove request 'supplemental-promotion-sha256))))
    (check-exn exn:fail?
               (lambda () (audit-binding (hash-set request 'supplemental-promotion-sha256 other))))
    (check-exn exn:fail?
               (lambda () (audit-binding (hash-set request 'supplemental-promotion-sha256 "bad")))))
  (test-case "ordinary old bindings remain unchanged"
    (define old (audit-binding (hasheq 'head "old")))
    (check-false (hash-has-key? old 'supplemental-promotion-sha256))
    (check-equal? (hash-count old) 10))
  ;; Python: test_prepare_copies_pin_from_trusted_consumer_seam. The trusted
  ;; consumer seam (`_owned_authority`) is injected as the bounded `run` seam.
  (test-case "owned-request binding copies the P pin"
    (define pin (make-string 64 #\a))
    (define receipt
      (hasheq 'head "head" 'tree "tree" 'branch "branch" 'attempt-id "attempt" 'attempt-fence 2))
    (define seam
      (hasheq 'reference
              (hasheq 'supplemental-contract (hasheq) 'sha256 (make-string 64 #\c))
              'supplemental-promotion-sha256
              pin
              'receipt
              receipt
              'generation
              1
              'bundle
              "bundle"))
    (define (seam-run value)
      (lambda (_program _args _timeout) (jsexpr->string value)))
    (define bound
      (bind-owned-request (hasheq 'head
                                  "head"
                                  'tree
                                  "tree"
                                  'branch
                                  "branch"
                                  'attempt-id
                                  "attempt"
                                  'attempt-fence
                                  2
                                  'generation
                                  1
                                  'prompt
                                  "review")
                          (seam-run seam)
                          "/unused"
                          "plan"
                          1
                          "/unused"
                          #f))
    (check-equal? (hash-ref bound 'supplemental-promotion-sha256) pin)
    (check-equal? (normalized-json (hash-ref (hash-ref bound 'owned-authority) 'audit-binding))
                  (normalized-json (delivery-audit-binding bound)))
    ;; Re-authentication re-reads the authority and refuses a changed P.
    (define drifted (hash-set seam 'supplemental-promotion-sha256 (make-string 64 #\d)))
    (define (reauthenticate)
      (reauthenticate-owned-request bound (seam-run drifted) "/unused" "plan" 1 "/unused" #f))
    (check-exn exn:fail:review-infra? reauthenticate)
    (check-regexp-match #rx"evidence registration drifted" (refusal-message reauthenticate))))
