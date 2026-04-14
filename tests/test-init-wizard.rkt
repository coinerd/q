#lang racket

;; tests/test-init-wizard.rkt — Tests for cli/init-wizard.rkt
;;
;; Tests run-init-wizard using string ports for input/output.
;; Note: the wizard writes to ~/.q/config.json via find-system-path.
;; Tests clean up after themselves.

(require rackunit
         racket/port
         racket/file
         json
         "../cli/init-wizard.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (run-wizard-with-input input-str)
  ;; Run the wizard with string input, capture output.
  (define in (open-input-string input-str))
  (define out (open-output-string))
  (run-init-wizard #:in in #:out out)
  (get-output-string out))

(define (cleanup-config)
  (define q-dir (build-path (find-system-path 'home-dir) ".q"))
  (when (directory-exists? q-dir)
    (delete-directory/files q-dir)))

(define (config-exists?)
  (file-exists? (build-path (find-system-path 'home-dir) ".q" "config.json")))

(define (read-config)
  (call-with-input-file
      (build-path (find-system-path 'home-dir) ".q" "config.json")
    read-json))

;; ============================================================
;; Test cases
;; ============================================================

(test-case "run-init-wizard writes config for valid openai input"
  (cleanup-config)
  (define output (run-wizard-with-input "openai\nsk-test-key-123\ngpt-4o\n"))
  (check-true (string-contains? output "Configuration saved"))
  (check-true (string-contains? output "Run 'q' to start"))
  (when (config-exists?)
    (define cfg (read-config))
    (check-equal? (hash-ref cfg 'default-provider) "openai"))
  (cleanup-config))

(test-case "run-init-wizard rejects invalid provider"
  (cleanup-config)
  (define output (run-wizard-with-input "bad-provider\n"))
  (check-true (string-contains? output "Invalid provider"))
  (cleanup-config))

(test-case "run-init-wizard accepts anthropic provider"
  (cleanup-config)
  (define output (run-wizard-with-input "anthropic\nsk-ant-test\nclaude-3\n"))
  (check-true (string-contains? output "Configuration saved"))
  (cleanup-config))

(test-case "run-init-wizard accepts gemini provider"
  (cleanup-config)
  (define output (run-wizard-with-input "gemini\nAIza-test\n\n"))
  (check-true (string-contains? output "Configuration saved"))
  (cleanup-config))

(test-case "run-init-wizard handles empty model (no default-model key)"
  (cleanup-config)
  (define output (run-wizard-with-input "openai\nsk-test\n\n"))
  (check-true (string-contains? output "Configuration saved"))
  (when (config-exists?)
    (define cfg (read-config))
    (define providers (hash-ref cfg 'providers))
    (define openai-cfg (hash-ref providers 'openai))
    (check-false (hash-has-key? openai-cfg 'default-model)))
  (cleanup-config))

(test-case "run-init-wizard sets default-model when provided"
  (cleanup-config)
  (define output (run-wizard-with-input "openai\nsk-test\ngpt-4o-mini\n"))
  (check-true (string-contains? output "Configuration saved"))
  (when (config-exists?)
    (define cfg (read-config))
    (define providers (hash-ref cfg 'providers))
    (define openai-cfg (hash-ref providers 'openai))
    (check-equal? (hash-ref openai-cfg 'default-model) "gpt-4o-mini"))
  (cleanup-config))

(test-case "run-init-wizard overwrites existing config when user says y"
  (cleanup-config)
  ;; First run: create config
  (run-wizard-with-input "openai\nsk-first\ngpt-4\n")
  ;; Second run: overwrite
  (define output (run-wizard-with-input "y\nanthropic\nsk-second\nclaude-3\n"))
  (check-true (string-contains? output "Configuration saved"))
  (cleanup-config))

(test-case "run-init-wizard aborts when user declines overwrite"
  (cleanup-config)
  ;; First run: create config
  (run-wizard-with-input "openai\nsk-first\n\n")
  ;; Second run: decline overwrite
  (define output (run-wizard-with-input "n\n"))
  (check-true (string-contains? output "Aborted"))
  (cleanup-config))
