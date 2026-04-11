#lang racket

(require rackunit
         racket/port
         racket/runtime-path
         "../runtime/safe-mode.rkt")

(define-runtime-path test-dir ".")

;; ============================================================
;; test-safe-mode.rkt — tests for safe-mode and trust model
;; ============================================================

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

(test-case
 "safe-mode? is #f by default"
 (check-false
  (parameterize ([current-safe-mode #f])
    (safe-mode?))))

;; ============================================================
;; 2. safe-mode-active! sets mode on
;; ============================================================

(test-case
 "safe-mode-active! turns on and off"
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

(test-case
 "bash blocked in safe mode, read/ls/grep/find allowed"
 (parameterize ([current-safe-mode #t])
   (check-false (allowed-tool? "bash"))
   (check-true  (allowed-tool? "read"))
   (check-true  (allowed-tool? "ls"))
   (check-true  (allowed-tool? "grep"))
   (check-true  (allowed-tool? "find"))))

;; ============================================================
;; 4. allowed-tool?: extension:foo blocked in safe mode
;; ============================================================

(test-case
 "extension tools blocked in safe mode"
 (parameterize ([current-safe-mode #t])
   (check-false (allowed-tool? "extension:foo"))
   (check-false (allowed-tool? "extension:custom-tool"))))

;; ============================================================
;; 5. allowed-path?: outside project blocked in safe mode
;; ============================================================

(test-case
 "path outside project root blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root (build-path "/tmp/safe-project")])
   (check-false (allowed-path? "/etc/passwd"))))

;; ============================================================
;; 6. allowed-path?: inside project allowed in safe mode
;; ============================================================

(test-case
 "path inside project root allowed in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root (current-directory)])
   (check-true (allowed-path? (build-path (current-directory) "src" "foo.rkt")))))

;; ============================================================
;; 7. trust-level: 'full when off, 'restricted when on
;; ============================================================

(test-case
 "trust-level is 'full when safe mode off"
 (parameterize ([current-safe-mode #f])
   (check-equal? (trust-level) 'full)))

(test-case
 "trust-level is 'restricted when safe mode on"
 (parameterize ([current-safe-mode #t])
   (check-equal? (trust-level) 'restricted)))

;; ============================================================
;; 8. safe-mode-config-info returns correct hash structure
;; ============================================================

(test-case
 "safe-mode-config-info returns correct hash structure"
 (parameterize ([current-safe-mode #t])
   (define cfg (safe-mode-config-info))
   (check-pred hash? cfg "safe-mode-config-info returns a hash")
   (check-true (hash-has-key? cfg 'active?) "has active? key")
   (check-true (hash-has-key? cfg 'trust-level) "has trust-level key")
   (check-true (hash-has-key? cfg 'blocked-tools) "has blocked-tools key")
   (check-true (hash-has-key? cfg 'reason) "has reason key")
   (check-true (hash-ref cfg 'active?) "active? is #t")
   (check-equal? (hash-ref cfg 'trust-level) 'restricted)
   (for ([expected '("bash" "edit" "write" "firecrawl")])
     (check-not-false (member expected (hash-ref cfg 'blocked-tools))
                      (format "blocked-tools contains ~a" expected)))))

;; ============================================================
;; 9. Environment variable Q_SAFE_MODE=1 activates safe mode
;; ============================================================

(test-case
 "Q_SAFE_MODE=1 activates safe mode, Q_SAFE_MODE=0 does not"
 (parameterize ([current-safe-mode #f])
   (with-env-var "Q_SAFE_MODE" "1"
     (check-true (safe-mode?) "Q_SAFE_MODE=1 activates safe mode"))
   (with-env-var "Q_SAFE_MODE" "0"
     (check-false (safe-mode?) "Q_SAFE_MODE=0 does not activate safe mode"))))

;; ============================================================
;; 10. All tools allowed when safe mode off
;; ============================================================

(test-case
 "all tools allowed when safe mode off"
 (parameterize ([current-safe-mode #f])
   (check-true (allowed-tool? "bash"))
   (check-true (allowed-tool? "edit"))
   (check-true (allowed-tool? "write"))
   (check-true (allowed-tool? "firecrawl"))
   (check-true (allowed-tool? "read"))
   (check-true (allowed-tool? "extension:foo"))))

;; ============================================================
;; 11. SEC-13: safe-mode one-way lock
;; ============================================================

(test-case
 "safe-mode-deactivate! works when not locked"
 (parameterize ([current-safe-mode #f]
                [safe-mode-locked? #f])
   (safe-mode-active!)
   (check-true (safe-mode?))
   (safe-mode-deactivate!)
   (check-false (safe-mode?))))

(test-case
 "safe-mode-deactivate! raises error when locked"
 (parameterize ([current-safe-mode #t]
                [safe-mode-locked? #f])
   (lock-safe-mode!)
   (check-exn
    #rx"safe-mode is locked and cannot be deactivated"
    (lambda () (safe-mode-deactivate!)))
   ;; Safe mode should still be active
   (check-true (safe-mode?))))

(test-case
 "lock-safe-mode! prevents deactivation"
 (parameterize ([current-safe-mode #t]
                [safe-mode-locked? #f])
   (lock-safe-mode!)
   (check-true (safe-mode-locked?))
   (check-exn
    exn:fail?
    (lambda () (safe-mode-deactivate!)))))

(test-case
 "safe-mode can still be activated when locked"
 (parameterize ([current-safe-mode #f]
                [safe-mode-locked? #t])
   ;; Activation should still work even when locked
   (safe-mode-active!)
   (check-true (safe-mode?))))

(test-case
 "safe-mode-locked? defaults to #f"
 (parameterize ([safe-mode-locked? #f])
   (check-false (safe-mode-locked?))))
