#lang racket

;; tests/test-init-wizard.rkt — Tests for cli/init-wizard.rkt
;;
;; Tests run-init-wizard using string ports for input/output.
;; v0.28.20: All tests use temp-dir isolation — no ~/.q/ pollution.

(require rackunit
         racket/port
         racket/file
         json
         "../cli/init-wizard.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (run-wizard-with-input input-str #:config-dir [config-dir #f])
  ;; Run the wizard with string input, capture output.
  (define in (open-input-string input-str))
  (define out (open-output-string))
  (run-init-wizard #:in in #:out out #:config-dir config-dir)
  (get-output-string out))

(define (with-isolated-config thunk)
  ;; Create a temp dir, run thunk with it, clean up.
  (define tmp (make-temporary-file "q-wizard-test-~a" 'directory))
  (dynamic-wind (lambda () (void))
                (lambda () (thunk tmp))
                (lambda () (delete-directory/files tmp #:must-exist? #f))))

(define (config-exists? dir)
  (file-exists? (build-path dir "config.json")))

(define (read-config dir)
  (call-with-input-file (build-path dir "config.json") read-json))

;; ============================================================
;; Test cases — all isolated to temp dirs
;; ============================================================

(test-case "run-init-wizard writes config for valid openai input"
  (with-isolated-config
   (lambda (tmp)
     (define output (run-wizard-with-input "openai\nsk-real-key-not-test\ngpt-4o\n" #:config-dir tmp))
     (check-true (string-contains? output "Configuration saved"))
     (check-true (string-contains? output "Run 'q' to start"))
     (when (config-exists? tmp)
       (define cfg (read-config tmp))
       (check-equal? (hash-ref cfg 'default-provider) "openai")))))

(test-case "run-init-wizard rejects invalid provider"
  (with-isolated-config (lambda (tmp)
                          (define output (run-wizard-with-input "bad-provider\n" #:config-dir tmp))
                          (check-true (string-contains? output "Invalid provider")))))

(test-case "run-init-wizard accepts openai-compatible provider with base URL"
  (with-isolated-config
   (lambda (tmp)
     (define output
       (run-wizard-with-input "openai-compatible\nhttp://localhost:11434/v1\n\nllama3\n"
                              #:config-dir tmp))
     (check-true (string-contains? output "Configuration saved"))
     (when (config-exists? tmp)
       (define cfg (read-config tmp))
       (check-equal? (hash-ref cfg 'default-provider) "openai-compatible")
       (define providers (hash-ref cfg 'providers))
       (define compat-cfg (hash-ref providers 'openai-compatible))
       (check-equal? (hash-ref compat-cfg 'base-url) "http://localhost:11434/v1")
       (check-equal? (hash-ref compat-cfg 'default-model) "llama3")))))

(test-case "run-init-wizard openai-compatible with empty base URL"
  (with-isolated-config
   (lambda (tmp)
     (define output
       (run-wizard-with-input "openai-compatible\n\nsk-real-key-not-test\nmodel-x\n"
                              #:config-dir tmp))
     (check-true (string-contains? output "Configuration saved"))
     (when (config-exists? tmp)
       (define cfg (read-config tmp))
       (define providers (hash-ref cfg 'providers))
       (define compat-cfg (hash-ref providers 'openai-compatible))
       ;; No base-url key when empty
       (check-false (hash-ref compat-cfg 'base-url #f))
       (check-equal? (hash-ref compat-cfg 'default-model) "model-x")))))

(test-case "run-init-wizard accepts anthropic provider"
  (with-isolated-config
   (lambda (tmp)
     (define output (run-wizard-with-input "anthropic\nsk-ant-real-key\nclaude-3\n" #:config-dir tmp))
     (check-true (string-contains? output "Configuration saved")))))

(test-case "run-init-wizard accepts gemini provider"
  (with-isolated-config (lambda (tmp)
                          (define output
                            (run-wizard-with-input "gemini\nAIza-real-key\n\n" #:config-dir tmp))
                          (check-true (string-contains? output "Configuration saved")))))

(test-case "run-init-wizard uses provider default when model is empty"
  (with-isolated-config
   (lambda (tmp)
     (define output (run-wizard-with-input "openai\nsk-real-key-not-test\n\n" #:config-dir tmp))
     (check-true (string-contains? output "Configuration saved"))
     (when (config-exists? tmp)
       (define cfg (read-config tmp))
       (define providers (hash-ref cfg 'providers))
       (define openai-cfg (hash-ref providers 'openai))
       ;; Now always sets a sensible default (#455)
       (check-equal? (hash-ref openai-cfg 'default-model) "gpt-4o")))))

(test-case "run-init-wizard sets default-model when provided"
  (with-isolated-config (lambda (tmp)
                          (define output
                            (run-wizard-with-input "openai\nsk-real-key-not-test\ngpt-4o-mini\n"
                                                   #:config-dir tmp))
                          (check-true (string-contains? output "Configuration saved"))
                          (when (config-exists? tmp)
                            (define cfg (read-config tmp))
                            (define providers (hash-ref cfg 'providers))
                            (define openai-cfg (hash-ref providers 'openai))
                            (check-equal? (hash-ref openai-cfg 'default-model) "gpt-4o-mini")))))

(test-case "run-init-wizard overwrites existing config when user says y"
  (with-isolated-config
   (lambda (tmp)
     ;; First run: create config
     (run-wizard-with-input "openai\nsk-first-real-key\ngpt-4\n" #:config-dir tmp)
     ;; Second run: overwrite
     (define output
       (run-wizard-with-input "y\nanthropic\nsk-second-real-key\nclaude-3\n" #:config-dir tmp))
     (check-true (string-contains? output "Configuration saved")))))

(test-case "run-init-wizard aborts when user declines overwrite"
  (with-isolated-config (lambda (tmp)
                          ;; First run: create config
                          (run-wizard-with-input "openai\nsk-first-real-key\n\n" #:config-dir tmp)
                          ;; Second run: decline overwrite
                          (define output (run-wizard-with-input "n\n" #:config-dir tmp))
                          (check-true (string-contains? output "Aborted")))))
