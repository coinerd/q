#lang racket/base

;; @speed fast
;; @suite default
;; W6 red-first: multi-step explorer drivers, step isolation, and credential defense.

(require rackunit
         racket/string
         racket/list
         racket/hash
         "../scripts/tmux-tui-explore.rkt"
         "../scripts/tmux-explore/verifiers.rkt"
         "../scripts/tmux-explore/evidence.rkt"
         "../util/credential-redaction.rkt")

;; Test 1: Multi-step driver has step IDs and ordered prompts
(test-case "multi-step scenario has step IDs and ordered prompts"
  (define scenario (find-scenarios #:filter "memory"))
  (check-equal? (length scenario) 1)
  (define steps (explore-scenario-steps (car scenario)))
  (check-true (list? steps))
  (check-true (positive? (length steps)))
  (for ([step (in-list steps)]
        [i (in-naturals)])
    (check-equal? (explore-step-id step) (format "memory-~a" i))
    (check-true (string? (explore-step-prompt step)))))

;; Test 2: Step-local stale completion from previous step is rejected
(test-case "stale completion from previous step is rejected"
  (define step1-completion
    (hash 'phase "turn.completed" 'session-id "s1" 'turn-id "t1" 'data (hash 'reason "completed")))
  (define step2-start
    (hash 'phase "turn.started" 'session-id "s1" 'turn-id "t2" 'data (hash 'reason "started")))
  (define step2-completion
    (hash 'phase "turn.completed" 'session-id "s1" 'turn-id "t2" 'data (hash 'reason "completed")))
  (define events (list step1-completion step2-start step2-completion))
  (define step2-baseline 1) ; after step1-completion
  (check-false (step-terminal-event? "memory" events step2-baseline "t2")))

;; Test 3: Cross-session event in same step fails
(test-case "cross-session event in same step fails"
  (define foreign-session
    (hash 'phase
          "turn.completed"
          'session-id
          "foreign"
          'turn-id
          "t1"
          'data
          (hash 'reason "completed")))
  (define local-completion
    (hash 'phase "turn.completed" 'session-id "local" 'turn-id "t1" 'data (hash 'reason "completed")))
  (define events (list foreign-session local-completion))
  (check-false (step-terminal-event? "memory" events 0 "t1")))

;; Test 4: Cross-turn event in same step fails
(test-case "cross-turn event in same step fails"
  (define wrong-turn
    (hash 'phase "turn.completed" 'session-id "s1" 'turn-id "wrong" 'data (hash 'reason "completed")))
  (define right-turn
    (hash 'phase "turn.completed" 'session-id "s1" 'turn-id "right" 'data (hash 'reason "completed")))
  (define events (list wrong-turn right-turn))
  (check-false (step-terminal-event? "memory" events 0 "right")))

;; Test 5: Credential redaction covers OIDC/private-key/cloud patterns
(test-case "credential redaction covers OIDC private-key and cloud patterns"
  (define raw
    (string-append
     "private_key=-----BEGIN RSA PRIVATE KEY-----\n"
     "client_email=service@project.iam.gserviceaccount.com\n"
     "aws_access_key_id=AKIAIOSFODNN7EXAMPLE\n"
     "aws_secret_access_key=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\n"
     "azure_client_id=12345678-1234-1234-1234-123456789abc\n"
     "oidc_token=eyJhbGciOiJSUzI1NiIsImtpZCI6IjEyMyJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIn0"))
  (define redacted (redact-secrets raw))
  (check-false (string-contains? redacted "BEGIN RSA PRIVATE KEY"))
  (check-false (string-contains? redacted "AKIAIOSFODNN7EXAMPLE"))
  (check-false (string-contains? redacted "wJalrXUtnFEMI"))
  (check-false (string-contains? redacted "12345678-1234-1234-1234-123456789abc"))
  (check-true (string-contains? redacted "<REDACTED>")))

;; Test 6: Mock multi-step execution produces correct step ordering
(test-case "mock multi-step execution produces correct step ordering"
  (define results (run-exploration #:mode 'mock #:filter "memory"))
  (check-equal? (length results) 1)
  (define result (car results))
  (check-equal? (explore-result-tag result) "memory")
  (define evidence (explore-result-evidence result))
  (define step-order (assoc 'step-order evidence))
  (check-not-false step-order)
  (check-equal? (cdr step-order) '("memory-0")))

;; Test 7: Verifier rejects decoy cross-step correlation
(test-case "verifier rejects decoy cross-step correlation"
  (define decoy-store
    (hash 'phase
          "memory.item.stored"
          'session-id
          "s1"
          'turn-id
          "t1"
          'data
          (hash 'memory-id "foreign-id")))
  (define local-retrieval
    (hash 'phase
          "memory.retrieval.performed"
          'session-id
          "s1"
          'turn-id
          "t2"
          'data
          (hash 'memory-id "local-id" 'result-present? #t)))
  (define completion
    (hash 'phase "turn.completed" 'session-id "s1" 'turn-id "t2" 'data (hash 'reason "completed")))
  (define observation
    (hash 'status
          'completed
          'trace-events
          (list decoy-store local-retrieval completion)
          'capture
          "retrieved"
          'mock-provider?
          #f
          'timed-out?
          #f
          'crashed?
          #f))
  (define verification (verify-scenario-evidence "memory" observation))
  (check-false (verification-result-passed? verification)))

;; Test 8: Evidence manifest includes step count and per-step digests
(test-case "evidence manifest includes step count and per-step digests"
  (define manifest
    (build-evidence-manifest #:tag "memory"
                             #:status 'pass
                             #:classification 'pass
                             #:repo-sha "a"
                             #:version "0.99.52"
                             #:output-dir "/tmp"
                             #:scenario-count 1
                             #:step-count 3
                             #:step-digests (list "abc" "def" "ghi")))
  (check-equal? (hash-ref manifest 'step-count) 3)
  (check-equal? (hash-ref manifest 'step-digests) (list "abc" "def" "ghi")))
