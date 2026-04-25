#lang racket

;; tests/test-print-mode.rkt — Tests for G9.3: -p/--print flag (print mode)
;;
;; Tests:
;;   - `-p` flag sets print-mode in config
;;   - `--print` flag sets print-mode in config
;;   - Print mode without `-p` is #f in config
;;   - Print mode extracts prompt from remaining args
;;   - Help text includes -p/--print description

(require rackunit
         rackunit/text-ui
         "../cli/args.rkt")

;; ============================================================
;; Test suite
;; ============================================================

(define print-mode-suite
  (test-suite "G9.3: Print Mode (-p/--print flag)"

    ;; ── -p flag sets print-mode in config ──
    (test-case "-p flag sets print-mode? to #t"
      (define cfg (parse-cli-args #("-p" "hello world")))
      (check-equal? (cli-config-print-mode? cfg) #t)
      (check-equal? (cli-config-mode cfg) 'print))

    ;; ── --print flag sets print-mode in config ──
    (test-case "--print flag sets print-mode? to #t"
      (define cfg (parse-cli-args #("--print" "hello world")))
      (check-equal? (cli-config-print-mode? cfg) #t)
      (check-equal? (cli-config-mode cfg) 'print))

    ;; ── Print mode without -p is #f in config ──
    (test-case "print-mode? is #f without -p/--print"
      (define cfg (parse-cli-args #("hello world")))
      (check-equal? (cli-config-print-mode? cfg) #f)
      (check-not-equal? (cli-config-mode cfg) 'print))

    ;; ── Print mode extracts prompt from remaining args ──
    (test-case "-p with prompt extracts prompt text"
      (define cfg (parse-cli-args #("-p" "What is 2+2?")))
      (check-equal? (cli-config-prompt cfg) "What is 2+2?")
      (check-equal? (cli-config-mode cfg) 'print))

    (test-case "--print with prompt extracts prompt text"
      (define cfg (parse-cli-args #("--print" "Explain recursion")))
      (check-equal? (cli-config-prompt cfg) "Explain recursion")
      (check-equal? (cli-config-mode cfg) 'print))

    ;; ── -p combines with other flags ──
    (test-case "-p with --model flag"
      (define cfg (parse-cli-args #("-p" "--model" "gpt-4" "test prompt")))
      (check-equal? (cli-config-print-mode? cfg) #t)
      (check-equal? (cli-config-model cfg) "gpt-4")
      (check-equal? (cli-config-prompt cfg) "test prompt"))

    (test-case "-p with --no-tools flag"
      (define cfg (parse-cli-args #("-p" "--no-tools" "test prompt")))
      (check-equal? (cli-config-print-mode? cfg) #t)
      (check-equal? (cli-config-no-tools? cfg) #t))

    ;; ── Help text includes -p/--print description ──
    (test-case "print-usage includes -p and --print"
      (define out (open-output-string))
      (print-usage out)
      (define usage-text (get-output-string out))
      (check-true (string-contains? usage-text "-p,") "Usage text should mention -p short flag")
      (check-true (string-contains? usage-text "--print")
                  "Usage text should mention --print long flag")
      (check-true (string-contains? usage-text "Print mode") "Usage text should describe print mode"))

    ;; ── -p without prompt: prompt is #f ──
    (test-case "-p without prompt gives #f prompt"
      (define cfg (parse-cli-args #("-p")))
      (check-equal? (cli-config-print-mode? cfg) #t)
      (check-equal? (cli-config-prompt cfg) #f))

    ;; ── runtime-config includes print-mode ──
    (test-case "cli-config->runtime-config includes print-mode"
      (define cfg (parse-cli-args #("-p" "test")))
      (define rt (cli-config->runtime-config cfg))
      (check-equal? (hash-ref rt 'print-mode #f) #t))

    (test-case "cli-config->runtime-config omits print-mode when not set"
      (define cfg (parse-cli-args #("test")))
      (define rt (cli-config->runtime-config cfg))
      (check-equal? (hash-ref rt 'print-mode #f) #f))

    ;; ── mode-for-config returns 'print for print mode ──
    (test-case "mode-for-config returns 'print"
      (define cfg (parse-cli-args #("-p" "test")))
      (check-equal? (cli-config-mode cfg) 'print))))

;; ============================================================
;; Run
;; ============================================================

(run-tests print-mode-suite)
