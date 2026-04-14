#lang racket/base

;; q/cli/init-wizard.rkt — First-run guided setup wizard
;;
;; Extracted from interfaces/cli.rkt for modularity (Issue #193).
;; Updated (#455): API keys stored in ~/.q/credentials.json (not config.json).
;;
;; Provides:
;;   run-init-wizard — creates ~/.q/config.json via interactive prompts

(require json
         racket/string
         racket/file
         racket/list
         "../runtime/auth-store.rkt")

(provide run-init-wizard)

;; ============================================================
;; I/O: run-init-wizard (Issue #143)
;; ============================================================

;; Guided setup wizard that creates ~/.q/config.json.
;; For testability, accepts optional input/output ports.
(define (run-init-wizard #:in [in (current-input-port)] #:out [out (current-output-port)])
  (define config-dir (build-path (find-system-path 'home-dir) ".q"))
  (define config-path (build-path config-dir "config.json"))

  ;; Helper: read a line, handling EOF
  (define (read-input)
    (define line (read-line in))
    (if (eof-object? line)
        ""
        (string-trim line)))

  ;; Use aborted? flag for early exit
  (define aborted? (box #f))

  ;; Check if config already exists
  (when (file-exists? config-path)
    (display "Config already exists at ~/.q/config.json. Overwrite? (y/N): " out)
    (flush-output out)
    (define answer (read-input))
    (unless (or (string=? answer "y") (string=? answer "Y"))
      (displayln "Aborted." out)
      (set-box! aborted? #t)))

  (unless (unbox aborted?)
    ;; Ask for provider
    (display "Choose provider (openai/anthropic/gemini/openai-compatible): " out)
    (flush-output out)
    (define provider (read-input))
    (cond
      [(not (member provider '("openai" "anthropic" "gemini" "openai-compatible")))
       (displayln "Invalid provider. Choose openai, anthropic, gemini, or openai-compatible." out)
       (set-box! aborted? #t)]
      [else
       ;; For openai-compatible, ask for base URL
       (define base-url
         (if (string=? provider "openai-compatible")
             (begin
               (display "Base URL (e.g. http://localhost:11434/v1): " out)
               (flush-output out)
               (let ([url (read-input)])
                 (if (string=? url "") #f url)))
             #f))

       ;; Ask for API key (optional for local providers)
       (display "API key (press Enter if not required): " out)
       (flush-output out)
       (define api-key (read-input))

       ;; Ask for default model
       (display "Default model (press Enter for default): " out)
       (flush-output out)
       (define model (read-input))

       ;; Resolve default model for provider
       (define default-model
         (cond
           [(string=? provider "openai") "gpt-4o"]
           [(string=? provider "anthropic") "claude-3-5-sonnet-20241022"]
           [(string=? provider "gemini") "gemini-2.5-pro"]
           [(string=? provider "openai-compatible") ""]
           [else ""]))

       ;; The effective provider name for config storage:
       ;; openai-compatible uses the base URL hostname as provider name
       ;; so multiple OpenAI-compatible endpoints can coexist.
       ;; But for the default case, just use "openai-compatible".
       (define config-provider-name
         (if (string=? provider "openai-compatible")
             'openai-compatible
             (string->symbol provider)))

       ;; Build provider config (no secrets)
       (define provider-hash-base
         (list
          (cons 'default-model
                (if (and model (not (string=? model "")))
                    model
                    default-model))))
       ;; Add base-url for openai-compatible providers
       (define provider-hash-entries
         (if base-url
             (cons (cons 'base-url base-url) provider-hash-base)
             provider-hash-base))
       (define provider-hash (make-hash provider-hash-entries))

       ;; Build non-secret config
       (define config
         (make-hash
          (list
           (cons 'default-provider
                 (if (string=? provider "openai-compatible")
                     "openai-compatible"
                     provider))
           (cons 'providers
                 (make-hash
                  (list
                   (cons config-provider-name provider-hash)))))))

       ;; Write config
       (make-directory* config-dir)
       (call-with-output-file config-path
         (lambda (port) (write-json config port))
         #:exists 'replace)

       ;; Store API key in dedicated credentials file with restricted permissions (#455)
       (when (and api-key (not (string=? api-key "")))
         (save-credential-file! (symbol->string config-provider-name) api-key))

       (displayln "Configuration saved to ~/.q/config.json." out)
       (when (and api-key (not (string=? api-key "")))
         (displayln "API key saved to ~/.q/credentials.json (owner-only permissions)." out))
       (displayln "Run 'q' to start chatting." out)])))
