#lang racket

;; @speed fast
;; @suite fast

;; tests/test-adapter-layer-contracts.rkt — W6 (#8480)
;; Adapter layer contract characterization tests.
;;
;; Tests pure adapter boundary functions across 6 adapter layers:
;; §1 Provider factory: local-provider? and create-provider-for-name boundary
;; §2 Sandbox: env sanitization + secret detection boundary
;; §3 Browser: adapter struct contract boundary
;; §4 GUI: display environment assumption
;; §5 Config path resolution consistency

(require rackunit)

;; --- Provider factory adapter boundary ---
(require (only-in "../runtime/provider/provider-factory.rkt"
                  local-provider?
                  provider-is-mock?
                  build-mock-provider))

;; --- Sandbox secret detection (pure helpers) ---
(require (only-in "../sandbox/subprocess-helpers.rkt"
                  check-secret-var?
                  secret-patterns
                  SECRET-IMPLICIT-ALLOWLIST))

;; --- Browser adapter contract ---
(require (only-in "../browser/adapter.rkt" browser-adapter? make-browser-adapter)
         (only-in "../browser/types.rkt" browser-action?))

;; --- Config path resolution ---
(require (only-in "../util/config-paths.rkt" global-config-dir project-config-dirs))

;; --- LLM provider interface ---
(require (only-in "../llm/provider.rkt" provider? provider-name))

;; ============================================================
;; §1 Provider Factory Adapter Boundary
;; ============================================================

(test-case "P1: local-provider? rejects empty string"
  (check-false (local-provider? "")))

(test-case "P2: local-provider? rejects #f"
  (check-false (local-provider? #f)))

(test-case "P3: local-provider? accepts localhost with port"
  (check-true (local-provider? "http://localhost:8080")))

(test-case "P4: local-provider? accepts 127.0.0.1"
  (check-true (local-provider? "http://127.0.0.1:3000")))

(test-case "P5: local-provider? accepts ::1 (IPv6 loopback)"
  (check-true (local-provider? "http://[::1]:8080")))

(test-case "P6: local-provider? accepts 192.168.x.x"
  (check-true (local-provider? "http://192.168.1.100/api")))

(test-case "P7: local-provider? accepts 10.x.x.x"
  (check-true (local-provider? "http://10.0.0.1/v1")))

(test-case "P8: local-provider? accepts 172.16.x.x (RFC 1918 start)"
  (check-true (local-provider? "http://172.16.0.1/v1")))

(test-case "P9: local-provider? accepts 172.31.x.x (RFC 1918 end)"
  (check-true (local-provider? "http://172.31.255.255/v1")))

(test-case "P10: local-provider? rejects 172.32.x.x (outside RFC 1918)"
  (check-false (local-provider? "http://172.32.0.1/v1")))

(test-case "P11: local-provider? rejects 172.15.x.x (outside RFC 1918)"
  (check-false (local-provider? "http://172.15.0.1/v1")))

(test-case "P12: local-provider? rejects public domains"
  (check-false (local-provider? "https://api.openai.com")))

(test-case "P13: local-provider? rejects 172.0.x.x"
  (check-false (local-provider? "http://172.0.0.1/v1")))

(test-case "P14: mock provider has name 'mock'"
  (define mock (build-mock-provider))
  (check-true (provider-is-mock? mock))
  (check-equal? (provider-name mock) "mock"))

(test-case "P15: provider-is-mock? rejects non-providers"
  (check-false (provider-is-mock? "not-a-provider"))
  (check-false (provider-is-mock? 42))
  (check-false (provider-is-mock? #f)))

;; ============================================================
;; §2 Sandbox Environment Sanitization Boundary
;; ============================================================

(test-case "S1: check-secret-var? detects *_KEY pattern"
  (check-true (check-secret-var? "ANTHROPIC_API_KEY" '() '())))

(test-case "S2: check-secret-var? detects *_TOKEN pattern"
  (check-true (check-secret-var? "GITHUB_TOKEN" '() '())))

(test-case "S3: check-secret-var? detects *_SECRET pattern"
  (check-true (check-secret-var? "CLIENT_SECRET" '() '())))

(test-case "S4: check-secret-var? detects *_PASSWORD pattern"
  (check-true (check-secret-var? "DB_PASSWORD" '() '())))

(test-case "S5: check-secret-var? detects bare AUTH"
  (check-true (check-secret-var? "AUTH" '() '())))

(test-case "S6: check-secret-var? allows PATH"
  (check-false (check-secret-var? "PATH" '() '())))

(test-case "S7: check-secret-var? allows HOME"
  (check-false (check-secret-var? "HOME" '() '())))

(test-case "S8: check-secret-var? allows LANG"
  (check-false (check-secret-var? "LANG" '() '())))

(test-case "S9: extra denylist adds custom pattern"
  (check-true (check-secret-var? "MY_PRIVATE_DATA" (list #rx"(?i:PRIVATE)") '())))

(test-case "S10: allowlist overrides denylist"
  (check-false (check-secret-var? "MY_TOKEN_SAFE" (list #rx"(?i:TOKEN)") (list #rx"(?i:SAFE)"))))

(test-case "S11: secret-patterns is non-empty list of regexes"
  (check-true (pair? secret-patterns))
  (for ([p (in-list secret-patterns)])
    (check-pred regexp? p)))

(test-case "S12: SECRET-IMPLICIT-ALLOWLIST includes security-critical display vars"
  (check-not-false (member "SSH_AUTH_SOCK" SECRET-IMPLICIT-ALLOWLIST))
  (check-not-false (member "XAUTHORITY" SECRET-IMPLICIT-ALLOWLIST)))

;; ============================================================
;; §3 Browser Adapter Contract Boundary
;; ============================================================

(define (noop . args)
  'ok)

(test-case "B1: make-browser-adapter accepts valid procedures"
  (define adapter
    (make-browser-adapter #:open noop
                          #:close noop
                          #:navigate noop
                          #:observe noop
                          #:act noop
                          #:screenshot noop))
  (check-true (browser-adapter? adapter)))

(test-case "B2: make-browser-adapter rejects non-procedure open-fn"
  (check-exn exn:fail:contract?
             (lambda ()
               (make-browser-adapter #:open "not-a-proc"
                                     #:close noop
                                     #:navigate noop
                                     #:observe noop
                                     #:act noop
                                     #:screenshot noop))))

(test-case "B3: make-browser-adapter rejects non-procedure close-fn"
  (check-exn exn:fail:contract?
             (lambda ()
               (make-browser-adapter #:open noop
                                     #:close 42
                                     #:navigate noop
                                     #:observe noop
                                     #:act noop
                                     #:screenshot noop))))

(test-case "B4: browser-adapter? rejects non-adapters"
  (check-false (browser-adapter? "not-an-adapter"))
  (check-false (browser-adapter? 42))
  (check-false (browser-adapter? '()))
  (check-false (browser-adapter? #f)))

;; ============================================================
;; §4 Config Path Resolution Consistency
;; ============================================================

(test-case "C1: global-config-dir returns path ending in .q"
  (define p (global-config-dir))
  (check-true (path? p))
  (define parts (explode-path p))
  (check-equal? (last parts) (string->path ".q")))

(test-case "C2: project-config-dirs returns .q then .pi"
  (define dirs (project-config-dirs "/some/project"))
  (check-equal? (length dirs) 2)
  (check-equal? (last (explode-path (car dirs))) (string->path ".q"))
  (check-equal? (last (explode-path (cadr dirs))) (string->path ".pi")))

(test-case "C3: global-config-dir does not depend on current-directory"
  (define p1
    (parameterize ([current-directory "/tmp"])
      (global-config-dir)))
  (define p2
    (parameterize ([current-directory "/"])
      (global-config-dir)))
  (check-equal? p1 p2 "global-config-dir must be cwd-independent"))

;; ============================================================
;; §5 LLM Provider Interface Boundary
;; ============================================================

(test-case "L1: mock provider satisfies provider? contract"
  (define mock (build-mock-provider))
  (check-true (provider? mock)))

(test-case "L2: mock provider has expected name"
  (define mock (build-mock-provider))
  (check-equal? (provider-name mock) "mock"))

(test-case "L3: provider? rejects non-providers"
  (check-false (provider? "not-a-provider"))
  (check-false (provider? 42))
  (check-false (provider? '())))
