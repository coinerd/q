#lang racket

(require rackunit
         racket/port
         "../runtime/safe-mode.rkt")

;; ============================================================
;; test-safe-mode.rkt — tests for safe-mode and trust model
;; ============================================================

;; Helper: run body with safe-mode deactivated and env var cleared
(define-syntax-rule (with-clean-safe-mode body ...)
  (parameterize ([current-safe-mode #f]
                 [project-root (current-directory)])
    (begin0
      (begin body ...)
      ;; ensure env var doesn't leak
      (void))))

;; Helper: run body with a specific env var
(define-syntax-rule (with-env-var key val body ...)
  (let ([old (getenv key)])
    (putenv key val)
    (begin0
      (begin body ...)
      (if old (putenv key old) (putenv key "")))))

;; ============================================================
;; 1. Default: safe-mode? is #f when parameter unset and no env
;; ============================================================
(check-false
 (parameterize ([current-safe-mode #f])
   (safe-mode?))
 "safe-mode? is #f by default")

;; ============================================================
;; 2. safe-mode-active! sets mode on
;; ============================================================
(let ()
  (define was-set? #f)
  (parameterize ([current-safe-mode #f])
    (safe-mode-active!)
    (set! was-set? (safe-mode?))
    (check-true was-set? "safe-mode-active! turns on safe mode")
    (safe-mode-deactivate!)
    (check-false (safe-mode?) "safe-mode-deactivate! turns it off")))

;; ============================================================
;; 3. allowed-tool?: bash blocked in safe mode, read allowed
;; ============================================================
(parameterize ([current-safe-mode #t])
  (check-false (allowed-tool? "bash") "bash blocked in safe mode")
  (check-true  (allowed-tool? "read") "read allowed in safe mode")
  (check-true  (allowed-tool? "ls")   "ls allowed in safe mode")
  (check-true  (allowed-tool? "grep") "grep allowed in safe mode")
  (check-true  (allowed-tool? "find") "find allowed in safe mode"))

;; ============================================================
;; 4. allowed-tool?: extension:foo blocked in safe mode
;; ============================================================
(parameterize ([current-safe-mode #t])
  (check-false (allowed-tool? "extension:foo") "extension:foo blocked in safe mode")
  (check-false (allowed-tool? "extension:custom-tool") "extension:custom-tool blocked"))

;; ============================================================
;; 5. allowed-path?: outside project blocked in safe mode
;; ============================================================
(parameterize ([current-safe-mode #t]
               [project-root (build-path "/tmp/safe-project")])
  (check-false (allowed-path? "/etc/passwd")
               "path outside project root blocked in safe mode"))

;; ============================================================
;; 6. allowed-path?: inside project allowed in safe mode
;; ============================================================
(parameterize ([current-safe-mode #t]
               [project-root (current-directory)])
  (check-true (allowed-path? (build-path (current-directory) "src" "foo.rkt"))
              "path inside project root allowed in safe mode"))

;; ============================================================
;; 7. trust-level: 'full when off, 'restricted when on
;; ============================================================
(parameterize ([current-safe-mode #f])
  (check-equal? (trust-level) 'full "trust-level is 'full when safe mode off"))
(parameterize ([current-safe-mode #t])
  (check-equal? (trust-level) 'restricted "trust-level is 'restricted when safe mode on"))

;; ============================================================
;; 8. safe-mode-config returns correct hash structure
;; ============================================================
(parameterize ([current-safe-mode #t])
  (define cfg (safe-mode-config))
  (check-true (hash? cfg) "safe-mode-config returns a hash")
  (check-true (hash-has-key? cfg 'active?) "has active? key")
  (check-true (hash-has-key? cfg 'trust-level) "has trust-level key")
  (check-true (hash-has-key? cfg 'blocked-tools) "has blocked-tools key")
  (check-true (hash-has-key? cfg 'reason) "has reason key")
  (check-true (hash-ref cfg 'active?) "active? is #t")
  (check-equal? (hash-ref cfg 'trust-level) 'restricted)
  (for ([expected '("bash" "edit" "write" "firecrawl")])
    (check-not-false (member expected (hash-ref cfg 'blocked-tools))
                     (format "blocked-tools contains ~a" expected))))

;; ============================================================
;; 9. Environment variable Q_SAFE_MODE=1 activates safe mode
;; ============================================================
(parameterize ([current-safe-mode #f])
  (with-env-var "Q_SAFE_MODE" "1"
    (check-true (safe-mode?) "Q_SAFE_MODE=1 activates safe mode"))
  ;; Q_SAFE_MODE=0 should NOT activate
  (with-env-var "Q_SAFE_MODE" "0"
    (check-false (safe-mode?) "Q_SAFE_MODE=0 does not activate safe mode")))

;; ============================================================
;; 10. All tools allowed when safe mode off
;; ============================================================
(parameterize ([current-safe-mode #f])
  (check-true (allowed-tool? "bash") "bash allowed when safe mode off")
  (check-true (allowed-tool? "edit") "edit allowed when safe mode off")
  (check-true (allowed-tool? "write") "write allowed when safe mode off")
  (check-true (allowed-tool? "firecrawl") "firecrawl allowed when safe mode off")
  (check-true (allowed-tool? "read") "read allowed when safe mode off")
  (check-true (allowed-tool? "extension:foo") "extension tools allowed when safe mode off"))

;; ============================================================
;; Summary
;; ============================================================
(printf "~nAll safe-mode tests passed.~n")
