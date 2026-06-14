#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-capability.rkt — Capability taxonomy unit tests
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../util/capability.rkt"
                  VALID-CAPABILITIES
                  ROLE-CAPABILITIES
                  valid-capability?
                  role-has-capability?
                  all-capabilities
                  current-session-capabilities))

(define suite
  (test-suite "MAS Capability Taxonomy"

    ;; ── valid-capability? ──

    (test-case "valid-capability?: accepts all defined capabilities"
      (for ([cap (in-list VALID-CAPABILITIES)])
        (check-true (valid-capability? cap) (format "~a should be valid" cap))))

    (test-case "valid-capability?: rejects unknown symbols"
      (check-false (valid-capability? 'unknown))
      (check-false (valid-capability? 'foo)))

    (test-case "valid-capability?: rejects non-symbols"
      (check-false (valid-capability? "read-only"))
      (check-false (valid-capability? 42))
      (check-false (valid-capability? #f)))

    ;; ── all-capabilities ──

    (test-case "all-capabilities: returns 9 non-'any capabilities"
      (define caps (all-capabilities))
      (check-equal? (length caps) 9)
      (check-false (member 'any caps)))

    (test-case "all-capabilities: all returned values are valid"
      (for ([cap (in-list (all-capabilities))])
        (check-true (valid-capability? cap))))

    ;; ── role-has-capability? ──

    (test-case "role-has-capability?: supervisor has read-only"
      (check-true (role-has-capability? 'supervisor 'read-only)))

    (test-case "role-has-capability?: supervisor has subagent"
      (check-true (role-has-capability? 'supervisor 'subagent)))

    (test-case "role-has-capability?: supervisor lacks shell-exec"
      (check-false (role-has-capability? 'supervisor 'shell-exec)))

    (test-case "role-has-capability?: verifier has only read-only"
      (check-true (role-has-capability? 'verifier 'read-only))
      (check-false (role-has-capability? 'verifier 'file-write))
      (check-false (role-has-capability? 'verifier 'shell-exec)))

    (test-case "role-has-capability?: tool-gateway has 5 capabilities"
      (check-true (role-has-capability? 'tool-gateway 'shell-exec))
      (check-true (role-has-capability? 'tool-gateway 'file-write))
      (check-true (role-has-capability? 'tool-gateway 'git-write))
      (check-true (role-has-capability? 'tool-gateway 'network))
      (check-true (role-has-capability? 'tool-gateway 'browser)))

    (test-case "role-has-capability?: 'any always passes"
      (check-true (role-has-capability? 'verifier 'any))
      (check-true (role-has-capability? 'supervisor 'any)))

    ;; ── ROLE-CAPABILITIES completeness ──

    (test-case "ROLE-CAPABILITIES: defines 5 roles"
      (check-equal? (hash-count ROLE-CAPABILITIES) 5))

    (test-case "ROLE-CAPABILITIES: every granted capability is valid"
      (for ([(role caps) (in-hash ROLE-CAPABILITIES)])
        (for ([cap (in-list caps)])
          (check-true (valid-capability? cap)
                      (format "role ~a capability ~a should be valid" role cap)))))

    ;; ── current-session-capabilities ──

    (test-case "current-session-capabilities: default is '(any)"
      (check-equal? (current-session-capabilities) '(any)))

    (test-case "current-session-capabilities: can be parameterized"
      (parameterize ([current-session-capabilities '(read-only)])
        (check-equal? (current-session-capabilities) '(read-only))))))

(run-tests suite)
