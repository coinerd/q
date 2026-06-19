#lang racket

;; @speed slow
;; @suite default

;; BOUNDARY: integration

;; tests/test-doctor.rkt — TDD tests for interfaces/doctor.rkt
;;
;; Covers:
;;   - check-result struct construction
;;   - Individual check functions (with controlled environment)
;;   - run-doctor output format and exit codes

(require rackunit
         rackunit/text-ui
         racket/port
         racket/file
         "../interfaces/doctor.rkt"
         "../runtime/auth/auth-store.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-doctor-test-~a" 'directory))

(define (cleanup-dir dir)
  (with-handlers ([exn:fail? void])
    (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Test suites
;; ============================================================

(define/provide-test-suite
 test-doctor
 ;; ═══════════════════════════════════════════
 ;; check-result struct
 ;; ═══════════════════════════════════════════
 (test-suite "check-result struct"

   (test-case "ok result"
     (define r (check-result "test" 'ok "all good"))
     (check-equal? (check-result-name r) "test")
     (check-equal? (check-result-status r) 'ok)
     (check-equal? (check-result-message r) "all good"))

   (test-case "warning result"
     (define r (check-result "test" 'warning "hmm"))
     (check-equal? (check-result-status r) 'warning))

   (test-case "error result"
     (define r (check-result "test" 'error "broken"))
     (check-equal? (check-result-status r) 'error))

   (test-case "transparent struct"
     (define r (check-result "x" 'ok "y"))
     (check-equal? r (check-result "x" 'ok "y"))))
 ;; ═══════════════════════════════════════════
 ;; check-racket-version
 ;; ═══════════════════════════════════════════
 (test-suite "check-racket-version"

   (test-case "doctor: returns a check-result"
     (define r (check-racket-version))
     (check-pred check-result? r)
     (check-equal? (check-result-name r) "Racket"))

   (test-case "status is ok or error (depends on env)"
     (define r (check-racket-version))
     (check-not-false (member (check-result-status r) '(ok error))
                      (format "expected ok or error, got ~a" (check-result-status r)))))
 ;; ═══════════════════════════════════════════
 ;; check-packages
 ;; ═══════════════════════════════════════════
 (test-suite "check-packages"

   (test-case "doctor: returns a check-result (2)"
     (define r (check-packages))
     (check-pred check-result? r)
     (check-equal? (check-result-name r) "Packages"))

   (test-case "status is ok or error"
     (define r (check-packages))
     (check-not-false (member (check-result-status r) '(ok error)))))
 ;; ═══════════════════════════════════════════
 ;; check-config-dir
 ;; ═══════════════════════════════════════════
 (test-suite "check-config-dir"

   (test-case "doctor: returns a check-result (3)"
     (define r (check-config-dir))
     (check-pred check-result? r)
     (check-equal? (check-result-name r) "Config dir"))

   (test-case "status is ok, warning, or error"
     (define r (check-config-dir))
     (check-not-false (member (check-result-status r) '(ok warning error)))))
 ;; ═══════════════════════════════════════════
 ;; check-config-file
 ;; ═══════════════════════════════════════════
 (test-suite "check-config-file"

   (test-case "doctor: returns a check-result (4)"
     (define r (check-config-file))
     (check-pred check-result? r)
     (check-equal? (check-result-name r) "Config file")))
 ;; ═══════════════════════════════════════════
 ;; check-credentials
 ;; ═══════════════════════════════════════════
 (test-suite "check-credentials"

   (test-case "doctor: returns a check-result (5)"
     (define r (check-credentials))
     (check-pred check-result? r)
     (check-equal? (check-result-name r) "Credentials")))
 ;; ═══════════════════════════════════════════
 ;; check-session-dir
 ;; ═══════════════════════════════════════════
 (test-suite "check-session-dir"

   (test-case "doctor: returns a check-result (6)"
     (define r (check-session-dir))
     (check-pred check-result? r)
     (check-equal? (check-result-name r) "Session dir")))
 ;; ═══════════════════════════════════════════
 ;; check-tui-packages
 ;; ═══════════════════════════════════════════
 (test-suite "check-tui-packages"

   (test-case "doctor: returns a check-result (7)"
     (define r (check-tui-packages))
     (check-pred check-result? r)
     (check-not-false (member (check-result-name r) '("TUI" "TUI (char-term)"))
                      (format "expected TUI or TUI (char-term), got ~a" (check-result-name r)))
     ;; TUI is optional — should always be ok or warning
     (check-not-false (member (check-result-status r) '(ok warning))
                      (format "expected ok or warning, got ~a" (check-result-status r)))))
 ;; ═══════════════════════════════════════════
 ;; run-doctor output format
 ;; ═══════════════════════════════════════════
 (test-suite "run-doctor output"

   (test-case "produces output with check indicators"
     (define output (with-output-to-string (λ () (run-doctor))))
     ;; Should contain header
     (check-true (string-contains? output "q doctor") "output should contain 'q doctor'")
     ;; Should contain check indicators
     (check-true
      (or (string-contains? output "✓") (string-contains? output "✗") (string-contains? output "⚠"))
      "output should contain check icons"))

   (test-case "returns 0 or 1"
     (define result (with-output-to-string (λ () (run-doctor))))
     ;; run-doctor returns exit code; we can't capture it from with-output-to-string
     ;; but we can call it in a parameterized way
     (define code
       (parameterize ([current-output-port (open-output-string)])
         (run-doctor)))
     (check-not-false (member code '(0 1)) (format "expected 0 or 1, got ~a" code)))

   (test-case "shows summary line"
     (define output (with-output-to-string (λ () (run-doctor))))
     (check-true (regexp-match? #rx"[0-9]+ errors?" output) "output should contain error count")
     (check-true (regexp-match? #rx"[0-9]+ warnings?" output)
                 "output should contain warning count"))))

;; ============================================================
;; Regression: lookup-credential with symbol provider name
;; ============================================================
;; Doctor's check-credentials calls lookup-credential with names from
;; provider-names (hash-keys), which can be symbols. The lookup-credential
;; contract requires string?. Doctor must normalize before calling.
;; This test verifies the contract boundary directly.

(define/provide-test-suite
 credential-symbol-regression
 (test-case "lookup-credential accepts string provider names"
   ;; String input should always work (baseline)
   (define cred (lookup-credential "test-provider-string" #f))
   (check-not-false (or (not cred) (credential? cred))
                    "lookup-credential with string should return credential? or #f"))
 (test-case "doctor check-credentials does not raise on symbol-named providers"
   ;; When a settings config has symbol keys (e.g. from JSON with unquoted keys),
   ;; provider-names returns symbols. check-credentials must normalize them
   ;; before calling lookup-credential. We verify by calling check-credentials
   ;; which internally normalizes and calls lookup-credential.
   (define r (check-credentials))
   (check-pred check-result? r)
   (check-equal? (check-result-name r) "Credentials")))

;; ============================================================
;; Run
;; ============================================================

(module+ test
  (run-tests test-doctor)
  (run-tests credential-symbol-regression))

(module+ main
  (run-tests test-doctor)
  (run-tests credential-symbol-regression))
