#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 300
;; @covers extensions/gsd/owner-correction.rkt extensions/gsd/generated-readme.rkt extensions/gsd/author-session-recovery.rkt
;; @covers scripts/gsd-owner-correction.rkt
;; B3 required-PR contract for generated-metadata correction (milestone #899).
;; The deep generated-metadata world stays in
;; tests/test-gsd-generated-metadata-correction.rkt (slow/full); this file
;; keeps its four @covers claims and the two UNIQUE claims
;; (extensions/gsd/owner-correction.rkt, scripts/gsd-owner-correction.rkt) on
;; a focused real-boundary slice:
;;   * ONE focused Verify stage through the production check-verify-command
;;     and verification registry with honest green/RED/timeout exit-truth;
;;   * the real operator CLI, real Git, real receipt/owned proof, real owner
;;     authority, revocation at the durable apply cut, replay and conflict
;;     refusals, journal/receipt canaries;
;;   * real confirmed publication replay that refuses after authority is
;;     revoked, leaving the exactly-once receipt/journal identity intact.
;; No mock-only security decision, no whole-Verify-campaign replay, and no
;; shared mutable helper from another wave.
(require rackunit
         racket/file
         racket/path
         racket/string
         "helpers/b3-metadata-contract-fixture.rkt"
         "../extensions/gsd/owner-correction.rkt"
         (prefix-in readme: "../extensions/gsd/generated-readme.rkt")
         "../extensions/gsd/author-session-recovery.rkt"
         "../extensions/gsd/author-scope.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/evidence-author-journal.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/review-proof.rkt"
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt" make-verification-registry)
         (only-in "../extensions/gsd/attempt-artifacts.rkt" record-wave-delivery!)
         (prefix-in operator: "../scripts/gsd-owner-correction.rkt"))
(define (stopped)
  #f)
(define w (b3-metadata-world! #:seed "required-pr-contract"))
(define root (hash-ref w 'root))
(define repo (hash-ref w 'repo))
(define plan (hash-ref w 'plan))
(define branch (hash-ref w 'branch))
(define store (hash-ref w 'store))
(define proofs (hash-ref w 'proofs))
(define registry (hash-ref w 'registry))
(define old-receipt (hash-ref w 'old-receipt))
(define (journal)
  (load-delivery-journal root plan 0))
(define (boundary-context #:base [base (hash-ref old-receipt 'head)])
  (make-branch-delivery-context #:repo-root repo
                                #:branch branch
                                #:base-commit base
                                #:worktree-path repo))
(define (with-owner-boundary proc)
  (parameterize ([current-owned-proof-root proofs]
                 [current-owner-correction-root store]
                 [current-gsd-verification-registry registry])
    (proc)))
(define (git-value . args)
  (apply b3-git-output! repo args))
(define (operator op . args)
  (define out (open-output-string))
  (define result
    (parameterize ([current-output-port out])
      (operator:main
       (list->vector (append (list "--root" (path->string root) "--plan" plan "--wave" "0" op)
                             args)))))
  (values result (get-output-string out)))
(define (delete-owner-authorities!)
  (for ([p (in-directory store)]
        #:when (string-suffix? (path->string p) ".authority"))
    (delete-file p)))

(module+ test
  (dynamic-wind
   void
   (lambda ()
     (test-case "one focused owned Verify stage keeps honest green/RED/timeout exit-truth"
       (with-owner-boundary
        (lambda ()
          (check-true (delivery-verification-approved? (hash-ref w 'green-result)))
          (check-not-false (load-receipt-owned-proof old-receipt root plan 0))
          (parameterize ([current-gsd-delivery-branch-context (boundary-context)])
            (parameterize ([current-gsd-delivery-verify-command b3-red-verify-command])
              (define red (b3-check! root))
              (check-false (delivery-verification-approved? red))
              (check-regexp-match #rx"exit=1" (delivery-verification-message red))
              (check-regexp-match #rx"state=failed" (delivery-verification-message red))
              (check-regexp-match #rx"1 failure\\(s\\)" (delivery-verification-message red)))
            ;; A real process that outlives the bounded deadline is attributed
            ;; honestly: timed-out/124, never a green or a RED proof.
            (parameterize ([current-gsd-delivery-verify-command b3-slow-verify-command]
                           [current-gsd-delivery-verify-timeout-sec 2])
              (define slow (b3-check! root))
              (check-false (delivery-verification-approved? slow))
              (check-regexp-match #rx"exit=124" (delivery-verification-message slow))
              (check-regexp-match #rx"state=timed-out" (delivery-verification-message slow)))))))
     (test-case "real Git/receipt authority, revocation, replay and conflict refusals preserve canaries"
       (with-owner-boundary
        (lambda ()
          (define before-repair (journal))
          (define context (current-evidence-context root plan 0))
          (define record (load-campaign-record root plan))
          (define successor
            (campaign-attempt-id (campaign-wave-current-attempt
                                  (car (campaign-record-waves record)))))
          (reserve-evidence-author-repair! root
                                           plan
                                           0
                                           context
                                           (frozen-author-files root plan 0)
                                           #:guard (lambda () #t))
          (start-evidence-author-repair! root plan 0 successor 2 #:guard (lambda () #t))
          (define running (journal))
          (check-equal? (hash-ref (journal-evidence-author-repair running) 'state) "running")
          ;; An in-scope author commit leaves the generated README stale.
          (b3-put! (build-path repo "tests/probe.rkt")
                   (string-append "#lang racket/base\n(require rackunit)\n"
                                  "(module+ test (check-true #t) (check-equal? 1 1))\n"))
          (b3-git! repo "add" "--" "tests/probe.rkt")
          (b3-git! repo "commit" "-qm" "B3 real successor delta")
          (define parent (git-value "rev-parse" "HEAD"))
          (define readme-before (file->bytes (build-path repo "README.md")))
          (define-values (proposal _) (operator "--propose"))
          (define digest (hash-ref proposal 'digest))
          (define delta (hash-ref (hash-ref proposal 'proposal) 'delta))
          (check-equal? (hash-ref delta 'parent) parent)
          (check-equal? (git-value "rev-parse" "HEAD") parent)
          (check-equal? (journal) running "proposal never mutates the receipt/journal")
          (check-equal? (file->bytes (build-path repo "README.md")) readme-before)
          (check-exn #rx"absent.*owner authority" (lambda () (operator "--apply" digest)))
          (operator "--authorize" digest)
          ;; Revoke the durable private authority INSIDE the real apply boundary.
          (define revoked? #f)
          (define refused-red
            (parameterize ([current-owner-correction-cut (lambda (where)
                                                           (when (eq? where 'intent)
                                                             (set! revoked? #t)
                                                             (delete-owner-authorities!)))])
              (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
                (operator "--apply" digest)
                "APPLIED-DESPITE-REVOCATION")))
          (check-true revoked? "revocation must occur at the real durable intent cut")
          (check-regexp-match #rx"owner authority" refused-red)
          (check-equal? (git-value "rev-parse" "HEAD") parent)
          (check-equal? (file->bytes (build-path repo "README.md")) readme-before)
          (check-equal? (journal) running)
          ;; Re-authorize the SAME inspected digest; authority is explicit.
          (operator "--authorize" digest)
          (define-values (correction _applied-out) (operator "--apply" digest))
          (check-equal? correction (hash-ref delta 'commit))
          (check-equal? (git-value "rev-parse" "HEAD") correction)
          (check-equal? (git-value "rev-list" "--parents" "-n" "1" "HEAD")
                        (string-append correction " " parent))
          (check-equal? (git-value "diff" "--name-only" parent "HEAD") "README.md")
          (check-equal? (git-value "status" "--porcelain") "")
          (check-equal? (journal) running "correction is not Verify or allowance refresh")
          (check-equal? (file->string (build-path repo "README.md")) (hash-ref delta 'after))
          (check-true (string-contains? (hash-ref delta 'after) "| Test assertions |"))
          ;; Conflict: arbitrary operator dirt is never mistaken for the exact
          ;; generated bytes, and an unauthorized digest cannot buy authority.
          (b3-put! (build-path repo "README.md") "operator conflict\n")
          ;; The correction commit itself is outside frozen FILES, so a new
          ;; proposal is refused as a conflict; arbitrary dirt is not accepted.
          (check-exn #rx"outside frozen FILES|prose/bytes/mode differ"
                     (lambda () (operator "--propose")))
          (b3-put! (build-path repo "README.md") (hash-ref delta 'after))
          (check-exn #rx"absent.*owner authority"
                     (lambda () (operator "--apply" (make-string 64 #\0))))
          (check-equal? (git-value "rev-parse" "HEAD") correction)
          (check-equal? (journal) running)))
       (test-case "confirmed publication replay refuses revoked authority and keeps exactly-once receipt"
         (with-owner-boundary
          (lambda ()
            (define record (load-campaign-record root plan))
            (define successor
              (campaign-attempt-id (campaign-wave-current-attempt
                                    (car (campaign-record-waves record)))))
            (define source (committed-delivery-snapshot repo))
            (define j-before (journal))
            ;; The successor's proof comes from the SAME focused real stage, not
            ;; from a replay of the four-command campaign or a fabricated receipt.
            (define-values (green capture)
              (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                             [current-gsd-delivery-branch-context (boundary-context)])
                (call-with-owned-proof-capture root
                                               plan
                                               0
                                               source
                                               successor
                                               2
                                               (lambda () (b3-check! root))
                                               #:cwd repo)))
            (check-true (delivery-verification-approved? green))
            (check-not-false capture)
            (record-wave-delivery! root plan 0 (hash-ref source 'branch) (hash-ref source 'head))
            (publish-approved-head! root
                                    plan
                                    0
                                    source
                                    successor
                                    2
                                    2
                                    "actual focused owned Verify GREEN"
                                    (lambda () (load-campaign-record root plan))
                                    #:owned-proof (owned-proof-capture-reference capture))
            (define publication (load-approved-publication root plan 0))
            (check-equal? (hash-ref publication 'status) "confirmed")
            (check-equal? (hash-ref publication 'head) (hash-ref source 'head))
            (define candidate (hash-ref publication 'receipt-candidate))
            (check-not-false (load-receipt-owned-proof candidate root plan 0))
            (delete-owner-authorities!)
            (define replay (author-publication-recovery root plan 0))
            (check-not-false replay "a confirmed publication must be replayable in principle")
            (check-exn #rx"owner authority" replay)
            (check-equal? (journal) j-before "refused replay never certifies a second receipt")
            (check-equal? (hash-ref (journal) 'receipt) (hash-ref j-before 'receipt))
            (check-equal? (hash-ref (journal) 'receipt-history '()) '())
            (check-not-false (load-receipt-owned-proof (hash-ref (journal) 'receipt) root plan 0))
            (check-equal? (load-approved-publication root plan 0) publication))))))
   (lambda () (b3-metadata-cleanup! w))))
