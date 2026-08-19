#lang racket
;; @covers extensions/gsd/github-port.rkt
;; @speed fast
;; @suite extensions
;; @boundary unit

;; v0.99.90 W4 (#9235): GitHub/Release side-effect adapter — idempotent
;; correlated commands; dry-run default; SHA assertions; token redaction.
;;
;; Security/workflow contract under test:
;;   - retries with the same correlation-id create NO duplicate external effect
;;   - external dedup (issue dedup-key / release tag / already-merged PR)
;;   - pr-merge asserts immutable head sha before merging (fail closed)
;;   - dry-run is the default; a dry-run port never touches the adapter
;;   - token redaction scrubs adapter error messages
;;   - inert adapter raises if ever invoked (safe production default)

(require rackunit
         racket/string
         "../extensions/gsd/effect-ports.rkt"
         "../extensions/gsd/github-port.rkt"
         "helpers/gsd-port-fakes.rkt")

(module+ test
  ;; ============================================================
  ;; Command contract
  ;; ============================================================

  (test-case "command contract: kind, correlation-id, params, sha shape"
    (check-true (gsd-github-command? (gsd-github-command 'issue-create "c1" #hasheq() #f)))
    (check-exn exn:fail:contract?
               (lambda () (gsd-github-command 'bogus "c1" #hasheq() #f))
               "kind must be closed")
    (check-exn exn:fail:contract?
               (lambda () (gsd-github-command 'issue-create "" #hasheq() #f))
               "correlation-id must be nonempty")
    (check-exn exn:fail:contract?
               (lambda () (gsd-github-command 'issue-create "c1" 'not-a-hash #f))
               "params must be a hash")
    (check-exn exn:fail:contract?
               (lambda () (gsd-github-command 'pr-merge "c1" #hasheq() 42))
               "expected-sha must be string or #f"))

  (test-case "result contract shape"
    (define r (gsd-github-command-result "c1" 'issue-create "42" #f #f ""))
    (check-equal? (gsd-github-command-result-correlation-id r) "c1")
    (check-equal? (gsd-github-command-result-external-id r) "42")
    (check-false (gsd-github-command-result-dry-run? r))
    (check-false (gsd-github-command-result-already-done? r)))

  ;; ============================================================
  ;; Journal replay: retries create no duplicate external effect
  ;; ============================================================

  (test-case "issue-create retry with same correlation-id is deduplicated (journal replay)"
    (define-values (adapter state) (make-fake-github-adapter))
    (define port (make-github-port adapter #:dry-run? #f))
    (define cmd
      (gsd-github-command 'issue-create "wave-1-issue" #hasheq((title . "Fix the widget")) #f))
    (define first ((gsd-github-port-execute port) cmd))
    (define retry ((gsd-github-port-execute port) cmd))
    (check-equal? (gsd-github-command-result-external-id first) "1")
    (check-equal? (gsd-github-command-result-external-id retry) "1")
    (check-equal? (fake-github-call-count state 'create-issue!)
                  1
                  "retry must not create a second issue")
    (check-not-false (member "wave-1-issue" ((gsd-github-port-journal port)))
                     "journal records the correlation-id")
    (check-equal? (gsd-github-command-result-already-done? retry) #t))

  (test-case "release-create retry with same correlation-id is deduplicated"
    (define-values (adapter state) (make-fake-github-adapter))
    (define port (make-github-port adapter #:dry-run? #f))
    (define cmd (gsd-github-command 'release-create "wave-1-release" #hasheq((tag . "v0.99.90")) #f))
    (define first ((gsd-github-port-execute port) cmd))
    (define retry ((gsd-github-port-execute port) cmd))
    (check-equal? (gsd-github-command-result-external-id first) "rel-1")
    (check-equal? (gsd-github-command-result-external-id retry) "rel-1")
    (check-equal? (fake-github-call-count state 'create-release!) 1))

  (test-case "pr-merge retry with same correlation-id is deduplicated"
    (define-values (adapter state) (make-fake-github-adapter))
    (fake-github-seed-pr! state 10 "abc123")
    (define port (make-github-port adapter #:dry-run? #f))
    (define cmd (gsd-github-command 'pr-merge "wave-1-merge" #hasheq((pull-number . 10)) "abc123"))
    (define first ((gsd-github-port-execute port) cmd))
    (define retry ((gsd-github-port-execute port) cmd))
    (check-equal? (gsd-github-command-result-external-id first) "sha-merge-10")
    (check-equal? (gsd-github-command-result-external-id retry) "sha-merge-10")
    (check-true (gsd-github-command-result-already-done? retry))
    (check-equal? (fake-github-call-count state 'merge-pr!) 1))

  ;; ============================================================
  ;; External dedup (cross-restart safety)
  ;; ============================================================

  (test-case "issue-create dedups on dedup-key even without a journal hit"
    (define-values (adapter state) (make-fake-github-adapter))
    ;; adapter already contains an issue with dedup-key "Fix the widget"
    (fake-github-seed-issue! state "Fix the widget" "7")
    (define port (make-github-port adapter #:dry-run? #f))
    (define result
      ((gsd-github-port-execute port) (gsd-github-command 'issue-create
                                                          "fresh-correlation"
                                                          #hasheq((title . "Fix the widget")
                                                                  (dedup-key . "Fix the widget"))
                                                          #f)))
    (check-equal? (gsd-github-command-result-external-id result) "7")
    (check-true (gsd-github-command-result-already-done? result))
    (check-equal? (fake-github-call-count state 'create-issue!) 0 "no duplicate issue created"))

  (test-case "release-create dedups on existing tag"
    (define-values (adapter state) (make-fake-github-adapter))
    (fake-github-seed-release! state "v0.99.89" "rel-0")
    (define port (make-github-port adapter #:dry-run? #f))
    (define result
      ((gsd-github-port-execute port)
       (gsd-github-command 'release-create "fresh-release" #hasheq((tag . "v0.99.89")) #f)))
    (check-equal? (gsd-github-command-result-external-id result) "rel-0")
    (check-true (gsd-github-command-result-already-done? result))
    (check-equal? (fake-github-call-count state 'create-release!) 0))

  (test-case "release-create dedup on existing tag still enforces the SHA assertion"
    (define-values (adapter state) (make-fake-github-adapter))
    (fake-github-seed-release! state "v0.99.90" "rel-0")
    (define port (make-github-port adapter #:dry-run? #f))
    ;; pre-existing release is on the WRONG commit -> fail closed, no success
    (check-exn exn:fail:github-sha-mismatch?
               (lambda ()
                 ((gsd-github-port-execute port)
                  (gsd-github-command 'release-create
                                      "release-dedup-sha"
                                      #hasheq((tag . "v0.99.90") (target-commitish . "abc"))
                                      "def")))
               "dedup path must not bypass the immutable sha assertion")
    (check-equal? (fake-github-call-count state 'create-release!) 0)
    ;; consistent sha -> dedup returns the existing release id
    (define result
      ((gsd-github-port-execute port) (gsd-github-command 'release-create
                                                          "release-dedup-ok"
                                                          #hasheq((tag . "v0.99.90")
                                                                  (target-commitish . "abc"))
                                                          "abc")))
    (check-equal? (gsd-github-command-result-external-id result) "rel-0")
    (check-true (gsd-github-command-result-already-done? result))
    (check-equal? (fake-github-call-count state 'create-release!) 0))

  (test-case "pr-merge on an already-merged PR returns the existing merge sha"
    (define-values (adapter state) (make-fake-github-adapter))
    (fake-github-seed-merged-pr! state 11 "abc111" "sha-merged-11")
    (define port (make-github-port adapter #:dry-run? #f))
    (define result
      ((gsd-github-port-execute port)
       (gsd-github-command 'pr-merge "fresh-merge" #hasheq((pull-number . 11)) "abc111")))
    (check-equal? (gsd-github-command-result-external-id result) "sha-merged-11")
    (check-true (gsd-github-command-result-already-done? result))
    (check-equal? (fake-github-call-count state 'merge-pr!) 0))

  ;; ============================================================
  ;; Immutable SHA assertions
  ;; ============================================================

  (test-case "pr-merge asserts head sha before merging (immutable); mismatch fails closed"
    (define-values (adapter state) (make-fake-github-adapter))
    (fake-github-seed-pr! state 12 "live-head-abc")
    (define port (make-github-port adapter #:dry-run? #f))
    (check-exn exn:fail:github-sha-mismatch?
               (lambda ()
                 ((gsd-github-port-execute port) (gsd-github-command 'pr-merge
                                                                     "merge-sha-check"
                                                                     #hasheq((pull-number . 12))
                                                                     "expected-but-moved")))
               "live head differs from the attested immutable sha")
    (check-equal? (fake-github-call-count state 'merge-pr!) 0 "no merge must happen on sha mismatch"))

  (test-case "pr-merge passes when live head matches the attested sha"
    (define-values (adapter state) (make-fake-github-adapter))
    (fake-github-seed-pr! state 13 "head-ok")
    (define port (make-github-port adapter #:dry-run? #f))
    (define result
      ((gsd-github-port-execute port)
       (gsd-github-command 'pr-merge "merge-ok" #hasheq((pull-number . 13)) "head-ok")))
    (check-equal? (gsd-github-command-result-external-id result) "sha-merge-13")
    (check-equal? (fake-github-call-count state 'merge-pr!) 1))

  (test-case "release-create asserts expected-sha against target-commitish"
    (define-values (adapter state) (make-fake-github-adapter))
    (define port (make-github-port adapter #:dry-run? #f))
    (check-exn exn:fail:github-sha-mismatch?
               (lambda ()
                 ((gsd-github-port-execute port)
                  (gsd-github-command 'release-create
                                      "release-sha"
                                      #hasheq((tag . "v0.99.90") (target-commitish . "abc"))
                                      "def")))
               "release target sha must equal expected-sha")
    (check-equal? (fake-github-call-count state 'create-release!) 0)
    ;; consistent sha -> release created
    (define result
      ((gsd-github-port-execute port) (gsd-github-command 'release-create
                                                          "release-sha-2"
                                                          #hasheq((tag . "v0.99.91")
                                                                  (target-commitish . "def"))
                                                          "def")))
    (check-equal? (gsd-github-command-result-external-id result) "rel-1"))

  ;; ============================================================
  ;; Dry-run default
  ;; ============================================================

  (test-case "dry-run is the default and never touches the adapter"
    (define-values (adapter state) (make-fake-github-adapter))
    (define port (make-github-port adapter)) ; no #:dry-run? -> #t
    (check-true ((gsd-github-port-dry-run? port)))
    (define result
      ((gsd-github-port-execute port)
       (gsd-github-command 'issue-create "dry-issue" #hasheq((title . "Dry")) #f)))
    (check-true (gsd-github-command-result-dry-run? result))
    (check-equal? (fake-github-call-count state 'create-issue!)
                  0
                  "dry-run must not create an external effect")
    (check-equal? ((gsd-github-port-journal port)) '() "dry-run must not poison the journal"))

  (test-case "explicit #:dry-run? #f executes (approved live/smoke path)"
    (define-values (adapter state) (make-fake-github-adapter))
    (define port (make-github-port adapter #:dry-run? #f))
    (define result
      ((gsd-github-port-execute port)
       (gsd-github-command 'issue-create "live-issue" #hasheq((title . "Live")) #f)))
    (check-false (gsd-github-command-result-dry-run? result))
    (check-equal? (gsd-github-command-result-external-id result) "1"))

  ;; ============================================================
  ;; Token redaction
  ;; ============================================================

  (test-case "redact-token scrubs the configured token from text"
    (check-equal? (redact-token "ghp_abc123def" "push with ghp_abc123def token")
                  "push with [REDACTED] token")
    (check-equal? (redact-token #f "no token configured") "no token configured")
    (check-equal? (redact-token "ghp_x" "nothing here") "nothing here"))

  (test-case "adapter errors are re-raised with the token redacted"
    (define adapter
      (make-github-adapter (lambda (params)
                             (error 'create-issue! "authentication failed with ghp_abc123def"))
                           (lambda (number) (void))
                           (lambda (params) (void))
                           (lambda (params) (error 'merge-pr! "merge failed with ghp_abc123def"))
                           (lambda (params) (void))
                           (lambda (number) #f)
                           (lambda (number) #f)
                           (lambda (key) #f)
                           (lambda (tag) #f)))
    (define port (make-github-port adapter #:dry-run? #f #:token "ghp_abc123def"))
    (define raised
      (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
        ((gsd-github-port-execute port)
         (gsd-github-command 'issue-create "token-issue" #hasheq() #f))))
    (check-false (string-contains? raised "ghp_abc123def")
                 (format "error must be redacted, got: ~a" raised))
    (check-true (string-contains? raised "[REDACTED]")))

  (test-case "the token is never serialized into journal or results"
    (define-values (adapter state) (make-fake-github-adapter))
    (define port (make-github-port adapter #:dry-run? #f #:token "ghp_secret_token"))
    (define result
      ((gsd-github-port-execute port)
       (gsd-github-command 'issue-create "no-token-echo" #hasheq((title . "Hi")) #f)))
    (check-false (string-contains? (gsd-github-command-result-note result) "ghp_secret_token"))
    (for ([corr (in-list ((gsd-github-port-journal port)))])
      (check-false (string-contains? corr "ghp_secret_token"))))

  ;; ============================================================
  ;; Inert adapter (safe production default)
  ;; ============================================================

  (test-case "inert adapter raises if ever invoked (guards accidental live calls)"
    (define port (make-dry-run-github-port))
    (check-true ((gsd-github-port-dry-run? port)))
    (define result
      ((gsd-github-port-execute port) (gsd-github-command 'issue-create "inert" #hasheq() #f)))
    (check-true (gsd-github-command-result-dry-run? result))
    (check-exn exn:fail:user?
               (lambda () ((github-adapter-create-issue! (make-inert-github-adapter)) #hasheq()))))

  ;; ============================================================
  ;; Port contract at call time
  ;; ============================================================

  (test-case "port execute contract enforces command->result shape at call time"
    (define bad-port (make-github-port (make-inert-github-adapter) #:dry-run? #f))
    ;; an execute that returns a non-result must be a contract violation
    (define fake-result-port
      (gsd-github-port (lambda (cmd) 'not-a-result) (lambda () #f) (lambda () '())))
    (check-exn exn:fail:contract?
               (lambda ()
                 ((gsd-github-port-execute fake-result-port)
                  (gsd-github-command 'issue-create "x" #hasheq() #f))))))
