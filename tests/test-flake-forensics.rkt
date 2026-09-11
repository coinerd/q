#lang racket/base

;; @speed fast
;; @suite unit-fast
;; @boundary unit
;;
;; tests/test-flake-forensics.rkt — v1.00.29 W3 flake forensic capture tests.
;;
;; Pure, deterministic coverage of flake-forensics.rkt: every environment
;; probe is pinned via the current-forensics-* parameters, the clock is
;; injected, and all writes go into a temporary repo root — no real git,
;; /proc, or repo-artifact access, no sleeps, fully parallelizable.
;;
;; Asserted contracts:
;;   - every mandatory field key is present in every bundle;
;;   - a missing probe answer is recorded as the string "unknown", never as
;;     an absent key;
;;   - the schema string "q.flake-forensics/1" is present;
;;   - the bundle file is written under
;;     artifacts/proof-graph/v1.00.29-w3/bundles/<incident-id>.json;
;;   - rerun ancestry round-trips through the JSON bundle;
;;   - capture is deterministic given identical pinned inputs (structure, not
;;     time values — the clock is injected, so ids match exactly);
;;   - green runs are never captured (post-failure only).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         json
         (only-in "../scripts/run-tests/flake-forensics.rkt"
                  capture-flake-forensics!
                  flake-forensics-schema
                  flake-forensics-mandatory-fields
                  bundles-dir
                  bundle-path-for
                  canonical-bundle-string
                  incident-id
                  jsexpr->json-string
                  seconds->iso8601
                  sorted-paths-digest
                  parse-worktree-porcelain
                  current-forensics-env
                  current-forensics-git-runner
                  current-forensics-now
                  current-forensics-proc-reader
                  current-forensics-fd-reader)
         (only-in "../scripts/run-tests/sha256.rkt" sha256-hex))

;; ------------------------------------------------------------
;; Pinned-world helpers
;; ------------------------------------------------------------

(define pin-instant 1757289600) ; fixed clock: 2025-09-08T00:00:00Z

(define (pin-world thunk)
  ;; Every probe answers deterministically; the clock is frozen.
  (parameterize ([current-forensics-env (lambda (name) #f)]
                 [current-forensics-git-runner (lambda (args) #f)]
                 [current-forensics-now (lambda () pin-instant)]
                 [current-forensics-proc-reader (lambda () #f)]
                 [current-forensics-fd-reader (lambda () #f)])
    (thunk)))

(define (make-fixture-root)
  (make-temporary-file "q-flake-forensics-~a" 'directory))

(define (cleanup-fixture! path)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files path #:must-exist? #f)))

(define (capture-into-root! root
                            #:keys [keys #f]
                            #:ancestry [ancestry '()]
                            #:failing-test-file [failing-test-file "tests/test-some-suite.rkt"]
                            #:write? [write? #t])
  (pin-world (lambda ()
               (capture-flake-forensics! #:failing? #t
                                         #:failing-test-file failing-test-file
                                         #:behavior-ids '("behavior-a" "behavior-b")
                                         #:predecessor-sequence '("p-one" "p-two")
                                         #:shard "3/8"
                                         #:worker "worker-5"
                                         #:scheduler-mode "fifo"
                                         #:seed "1234"
                                         #:selected-manifest-digest "deadbeef"
                                         #:environment-keys keys
                                         #:test-durations '(("tests/test-some-suite.rkt" 1200))
                                         #:original-stdout "raco test output captured verbatim"
                                         #:original-stderr "error: check-equal? failed"
                                         #:original-result-artifact "artifacts/run-1.json"
                                         #:rerun-ancestry ancestry
                                         #:repo-root root
                                         #:write? write?))))

;; ------------------------------------------------------------
;; Suite
;; ------------------------------------------------------------

(define suite
  (test-suite "flake-forensics capture"

    (test-case "every mandatory field key is present in the bundle"
      (define root (make-fixture-root))
      (define bundle (capture-into-root! root #:write? #f))
      (for ([field (in-list flake-forensics-mandatory-fields)])
        (check-true (hash-has-key? bundle field) (format "mandatory field missing: ~a" field)))
      (cleanup-fixture! root))

    (test-case "missing probes become the string unknown, never absent keys"
      (define root (make-fixture-root))
      (define bundle (capture-into-root! root #:write? #f))
      (for ([field (in-list flake-forensics-mandatory-fields)])
        (check-false (eq? (hash-ref bundle field 'MISSING) 'MISSING)
                     (format "mandatory field absent: ~a" field)))
      (check-equal? (hash-ref bundle 'prepared-env) "unknown")
      (check-equal? (hash-ref (hash-ref bundle 'workflow) 'run-id) "unknown")
      (check-equal? (hash-ref (hash-ref bundle 'git) 'commit) "unknown")
      (check-equal? (hash-ref bundle 'worktrees) "unknown")
      (check-equal? (hash-ref bundle 'child-process-tree) "unknown")
      (check-equal? (hash-ref bundle 'surviving-pids) "unknown")
      (check-equal? (hash-ref bundle 'open-handles) "unknown")
      (check-equal? (hash-ref bundle 'selected-manifest-digest) "deadbeef")
      (cleanup-fixture! root))

    (test-case "bundle carries the q.flake-forensics/1 schema string"
      (define root (make-fixture-root))
      (define bundle (capture-into-root! root #:write? #f))
      (check-equal? (hash-ref bundle 'schema) "q.flake-forensics/1")
      (check-equal? flake-forensics-schema "q.flake-forensics/1")
      (cleanup-fixture! root))

    (test-case "bundle file is written under artifacts/proof-graph/v1.00.29-w3/bundles"
      (define root (make-fixture-root))
      (define bundle (capture-into-root! root #:write? #t))
      (define id (hash-ref bundle 'incident-id))
      (check-true (regexp-match? #px"^[0-9a-f]{16}$" id) "incident-id must be 16 hex chars")
      (define expected (bundle-path-for root id))
      (check-true (file-exists? expected) "bundle file must exist at the canonical path")
      (check-equal? (path->string (find-relative-path root expected))
                    (string-append "artifacts/proof-graph/v1.00.29-w3/bundles/" id ".json"))
      (check-equal? (hash-ref bundle 'bundle-path) (path->string expected))
      (cleanup-fixture! root))

    (test-case "written bundle parses back as JSON and rerun ancestry round-trips"
      (define root (make-fixture-root))
      (define ancestry '("bundle-parent-1" "bundle-grandparent-2"))
      (define bundle (capture-into-root! root #:ancestry ancestry #:write? #t))
      (define raw
        (call-with-input-file (bundle-path-for root (hash-ref bundle 'incident-id)) read-json))
      (check-true (hash? raw) "bundle file must be a JSON object")
      (define j
        (for/hash ([(k v) (in-hash raw)])
          (values (string->symbol (format "~a" k)) v)))
      (check-equal? (hash-ref j 'schema) "q.flake-forensics/1")
      (check-equal? (hash-ref j 'incident-id) (hash-ref bundle 'incident-id))
      (check-equal? (hash-ref j 'rerun-ancestry)
                    ancestry
                    "rerun ancestry must round-trip through the JSON bundle")
      (check-equal? (hash-ref j 'failing-test-file) "tests/test-some-suite.rkt")
      (check-equal? (hash-ref j 'original-stderr) "error: check-equal? failed")
      (check-equal? (hash-ref j 'captured-at) (seconds->iso8601 pin-instant))
      (cleanup-fixture! root))

    (test-case "capture is deterministic for identical pinned inputs"
      (define root (make-fixture-root))
      (define one (capture-into-root! root #:keys '("B_KEY" "A_KEY") #:write? #f))
      (define two (capture-into-root! root #:keys '("B_KEY" "A_KEY") #:write? #f))
      (check-equal? one two "two captures under identical pinned inputs must be equal")
      (check-equal? (hash-ref one 'incident-id) (hash-ref two 'incident-id))
      (define three (capture-into-root! root #:failing-test-file "tests/test-other.rkt" #:write? #f))
      (check-false (equal? (hash-ref one 'incident-id) (hash-ref three 'incident-id))
                   "a changed input must change the incident identity")
      (cleanup-fixture! root))

    (test-case "green runs are never captured (post-failure only)"
      (define root (make-fixture-root))
      (define result
        (pin-world (lambda () (capture-flake-forensics! #:failing? #f #:repo-root root))))
      (check-false result "a green run must not produce a bundle")
      (check-false (directory-exists? (bundles-dir root))
                   "no bundles directory may be created on a green run")
      (cleanup-fixture! root))

    (test-case "environment-policy digest uses the sorted keys, unknown otherwise"
      (define root (make-fixture-root))
      (define with-keys (capture-into-root! root #:keys '("B_KEY" "A_KEY") #:write? #f))
      (check-equal? (hash-ref with-keys 'environment-policy-digest)
                    (sorted-paths-digest '("B_KEY" "A_KEY")))
      (check-equal? (sorted-paths-digest '("B_KEY" "A_KEY"))
                    (sorted-paths-digest '("A_KEY" "B_KEY"))
                    "digest must be order-insensitive")
      (check-not-equal? (hash-ref with-keys 'environment-policy-digest) "unknown")
      (define no-keys (capture-into-root! root #:write? #f))
      (check-equal? (hash-ref no-keys 'environment-policy-digest) "unknown")
      (cleanup-fixture! root))

    (test-case "worktree porcelain parsing is pure and unknown-safe"
      (define parsed
        (parse-worktree-porcelain
         (string-append "worktree /repo/main\nHEAD abc123\nbranch refs/heads/main\n"
                        "\n"
                        "worktree /repo/w3\nHEAD def456\nbranch refs/heads/campaign/w3\n")))
      (check-equal? (length parsed) 2)
      (check-equal? (hash-ref (first parsed) 'path) "/repo/main")
      (check-equal? (hash-ref (first parsed) 'head) "abc123")
      (check-equal? (hash-ref (first parsed) 'branch) "refs/heads/main")
      (check-equal? (hash-ref (second parsed) 'branch) "refs/heads/campaign/w3")
      (check-equal? (parse-worktree-porcelain "") '()))

    (test-case "timestamps render as RFC 3339 UTC"
      (check-equal? (seconds->iso8601 pin-instant) "2025-09-08T00:00:00Z"))

    (test-case "incident-id is the first 16 hex of sha256 over the canonical bundle"
      (define root (make-fixture-root))
      (define bundle (capture-into-root! root #:write? #f))
      (define expected
        (substring (sha256-hex (string->bytes/utf-8 (canonical-bundle-string bundle))) 0 16))
      (check-equal? (hash-ref bundle 'incident-id) expected)
      (check-true (equal? (canonical-bundle-string bundle) (canonical-bundle-string bundle))
                  "canonical form must be pure/stable")
      (check-false (hash-has-key? (string->jsexpr (canonical-bundle-string bundle)) 'incident-id)
                   "canonical form excludes the id itself")
      (cleanup-fixture! root))

    (test-case "json writer escapes strings and sorts object keys"
      (define s (jsexpr->json-string (hasheq 'b "x\"y\n" 'a "z")))
      (check-true (string-prefix? s "{\"a\":"))
      (check-true (string-contains? s "\"b\":\"x\\\"y\\n\""))))) ;; close (define suite ...)

;; decode canonical JSON (string-keyed) for the pure checks above
(define (string->jsexpr s)
  (define raw (with-input-from-string s read-json))
  (for/hash ([(k v) (in-hash raw)])
    (values (string->symbol (format "~a" k)) v)))

;; ------------------------------------------------------------
;; Run
;; ------------------------------------------------------------

(define failures (run-tests suite))

(module+ main
  (when (positive? failures)
    (exit 1)))
