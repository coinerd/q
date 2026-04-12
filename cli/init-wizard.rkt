#lang racket/base

;; q/cli/init-wizard.rkt — First-run guided setup wizard
;;
;; Extracted from interfaces/cli.rkt for modularity (Issue #193).
;;
;; Provides:
;;   run-init-wizard — creates ~/.q/config.json via interactive prompts

(require json
         racket/string
         racket/file
         racket/list)

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
    (display "Choose provider (openai/anthropic/gemini): " out)
    (flush-output out)
    (define provider (read-input))
    (cond
      [(not (member provider '("openai" "anthropic" "gemini")))
       (displayln "Invalid provider. Choose openai, anthropic, or gemini." out)
       (set-box! aborted? #t)]
      [else
       ;; Ask for API key
       (display "API key (will be visible): " out)
       (flush-output out)
       (define api-key (read-input))

       ;; Ask for default model
       (display "Default model (press Enter for default): " out)
       (flush-output out)
       (define model (read-input))

       ;; Build config hash
       (define provider-hash (make-hash (list (cons 'api-key (if (string=? api-key "") "" api-key)))))
       (when (and model (not (string=? model "")))
         (hash-set! provider-hash 'default-model model))

       (define config
         (make-hash (list (cons 'default-provider provider)
                          (cons 'providers
                                (make-hash (list (cons (string->symbol provider) provider-hash)))))))

       ;; Write config
       (make-directory* config-dir)
       (call-with-output-file config-path (lambda (port) (write-json config port)) #:exists 'replace)

       (displayln "Configuration saved to ~/.q/config.json." out)
       (displayln "Run 'q' to start chatting." out)])))
