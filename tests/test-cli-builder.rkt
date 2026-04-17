#lang racket

;; tests/test-cli-builder.rkt — tests for wiring/run-modes.rkt
;;
;; Covers mode-for-config (pure function), load-extensions-from-dir!,
;; and make-terminal-subscriber construction.
;; The full build-runtime-from-cli and mode runners require heavyweight
;; dependencies (providers, agent sessions), so they are tested
;; through integration tests rather than here.

(require rackunit
         rackunit/text-ui
         racket/file
         "../interfaces/cli.rkt"
         "../util/protocol-types.rkt"
         "../extensions/api.rkt"
         "../wiring/run-modes.rkt")

;; ── Helpers ──

(define (make-temp-dir)
  (make-temporary-file "q-cli-builder-test-~a" 'directory))

;; ── Test suite ──

(define-test-suite test-cli-builder

  ;; ============================================================
  ;; mode-for-config — pure function
  ;; ============================================================

  (test-case "mode-for-config: help command → 'help"
    (define cfg (parse-cli-args #("--help")))
    (check-equal? (mode-for-config cfg) 'help))

  (test-case "mode-for-config: version command → 'version"
    (define cfg (parse-cli-args #("--version")))
    (check-equal? (mode-for-config cfg) 'version))

  (test-case "mode-for-config: doctor command → 'doctor"
    (define cfg (cli-config 'doctor #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
    (check-equal? (mode-for-config cfg) 'doctor))

  (test-case "mode-for-config: sessions command → 'sessions"
    (define cfg (cli-config 'sessions #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
    (check-equal? (mode-for-config cfg) 'sessions))

  (test-case "mode-for-config: chat command → interactive mode"
    (define cfg (parse-cli-args #()))
    (check-equal? (cli-config-command cfg) 'chat)
    (check-equal? (mode-for-config cfg) 'interactive))

  (test-case "mode-for-config: prompt command → single mode"
    (define cfg (parse-cli-args #("Explain recursion")))
    (check-equal? (cli-config-command cfg) 'prompt)
    (check-equal? (mode-for-config cfg) 'single))

  (test-case "mode-for-config: --json flag → json mode"
    (define cfg (parse-cli-args #("--json")))
    (check-equal? (mode-for-config cfg) 'json))

  (test-case "mode-for-config: --rpc flag → rpc mode"
    (define cfg (parse-cli-args #("--rpc")))
    (check-equal? (mode-for-config cfg) 'rpc))

  (test-case "mode-for-config: --tui flag → tui mode"
    (define cfg (parse-cli-args #("--tui")))
    (check-equal? (mode-for-config cfg) 'tui))

  (test-case "mode-for-config: --session flag → interactive (resume)"
    (define cfg (parse-cli-args #("--session" "abc123")))
    (check-equal? (cli-config-command cfg) 'resume)
    (check-equal? (mode-for-config cfg) 'interactive))

  ;; ============================================================
  ;; mode-for-config: mode-specific configuration details
  ;; ============================================================

  (test-case "mode-for-config: --json with prompt still returns 'json"
    (define cfg (parse-cli-args #("--json" "hello")))
    (check-equal? (mode-for-config cfg) 'json))

  (test-case "mode-for-config: --rpc with --model returns 'rpc"
    (define cfg (parse-cli-args #("--rpc" "--model" "gpt-4")))
    (check-equal? (mode-for-config cfg) 'rpc))

  ;; ============================================================
  ;; load-extensions-from-dir!
  ;; ============================================================

  (test-case "load-extensions-from-dir! silently skips nonexistent directory"
    ;; Should not throw
    (define ext-reg (make-extension-registry))
    (check-not-exn
     (lambda ()
       (load-extensions-from-dir! ext-reg "/nonexistent/extensions"))))

  (test-case "load-extensions-from-dir! silently skips empty directory"
    (define dir (make-temp-dir))
    (define ext-reg (make-extension-registry))
    (check-not-exn
     (lambda ()
       (load-extensions-from-dir! ext-reg (build-path dir "ext"))))
    (delete-directory/files dir))

  (test-case "load-extensions-from-dir! skips non-.rkt files"
    (define dir (make-temp-dir))
    (define ext-dir (build-path dir "extensions"))
    (make-directory* ext-dir)
    (call-with-output-file (build-path ext-dir "notes.txt")
      (lambda (out) (display "not an extension" out)))
    (call-with-output-file (build-path ext-dir "data.json")
      (lambda (out) (display "{}" out)))
    (define ext-reg (make-extension-registry))
    (check-not-exn
     (lambda ()
       (load-extensions-from-dir! ext-reg ext-dir)))
    (delete-directory/files dir))

  ;; ============================================================
  ;; make-terminal-subscriber
  ;; ============================================================

  (test-case "make-terminal-subscriber returns a procedure"
    (check-pred procedure? (make-terminal-subscriber)))

  (test-case "make-terminal-subscriber handles non-stream events without error"
    (define sub (make-terminal-subscriber))
    (define out (open-output-string))
    (parameterize ([current-output-port out])
      (sub (make-event "session.started" 1000 "s1" #f (hasheq 'info "test"))))
    ;; Should not throw
    (check-true #t))

  ;; ============================================================
  ;; Error handling: invalid mode configurations
  ;; ============================================================

  (test-case "mode-for-config: unknown command falls back to mode from config"
    ;; Create a config with an arbitrary command and explicit mode
    (define cfg (cli-config 'custom #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
    (check-equal? (mode-for-config cfg) 'interactive))

  (test-case "mode-for-config: custom command with json mode"
    (define cfg (cli-config 'custom #f #f #f 'json #f #f #f 10 #f '() #f #f '() #f))
    (check-equal? (mode-for-config cfg) 'json))

  ;; ============================================================
  ;; Argument parsing integration: mode-for-config with various CLI args
  ;; ============================================================

  (test-case "mode-for-config integration: --verbose --model gpt-4o → interactive"
    (define cfg (parse-cli-args #("--verbose" "--model" "gpt-4o")))
    (check-equal? (mode-for-config cfg) 'interactive)
    (check-true (cli-config-verbose? cfg))
    (check-equal? (cli-config-model cfg) "gpt-4o"))

  (test-case "mode-for-config integration: --json --model test → json"
    (define cfg (parse-cli-args #("--json" "--model" "test")))
    (check-equal? (mode-for-config cfg) 'json)
    (check-equal? (cli-config-model cfg) "test"))

  (test-case "mode-for-config integration: --no-tools with prompt → single"
    (define cfg (parse-cli-args #("--no-tools" "do stuff")))
    (check-equal? (mode-for-config cfg) 'single)
    (check-true (cli-config-no-tools? cfg))
    (check-equal? (cli-config-prompt cfg) "do stuff"))
  )

;; Run
(run-tests test-cli-builder)
