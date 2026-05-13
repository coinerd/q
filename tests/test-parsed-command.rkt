#lang racket

;; BOUNDARY: integration

;; tests/test-parsed-command.rkt — TUI command parsing tests (R-17)

(require rackunit
         rackunit/text-ui
         "../tui/command-parse.rkt")

(define parse-suite
  (test-suite "parsed-command tests"

    ;; ── Basic parsing ──
    (test-case "non-command returns #f"
      (check-false (parse-command-name "hello"))
      (check-false (parse-command-name "")))

    ;; ── No-arg commands ──
    (test-case "/help returns parsed-command"
      (define r (parse-command-name "/help"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'help)
      (check-equal? (parsed-command-arg-kind r) 'none))

    (test-case "/h alias returns help"
      (define r (parse-command-name "/h"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'help))

    (test-case "/quit returns parsed-command"
      (define r (parse-command-name "/quit"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'quit))

    (test-case "/clear returns parsed-command"
      (define r (parse-command-name "/clear"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'clear))

    ;; ── Optional-arg commands ──
    (test-case "/model without arg"
      (define r (parse-command-name "/model"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'model)
      (check-equal? (parsed-command-args r) '()))

    (test-case "/model with arg"
      (define r (parse-command-name "/model gpt-4"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'model)
      (check-equal? (parsed-command-args r) '("gpt-4")))

    (test-case "/m alias with arg"
      (define r (parse-command-name "/m claude"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'model))

    ;; ── Required-arg commands ──
    (test-case "/switch with arg"
      (define r (parse-command-name "/switch 3"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'switch)
      (check-equal? (parsed-command-args r) '("3")))

    (test-case "/switch without arg → error"
      (define r (parse-command-name "/switch"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-arg-kind r) 'error))

    (test-case "/children with arg"
      (define r (parse-command-name "/children abc"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'children))

    (test-case "/children without arg → error"
      (define r (parse-command-name "/children"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-arg-kind r) 'error))

    ;; ── Unknown commands ──
    (test-case "unknown slash command returns 'unknown"
      (define r (parse-command-name "/xyz"))
      (check-equal? r 'unknown))

    ;; ── Other commands ──
    (test-case "/retry returns parsed-command"
      (define r (parse-command-name "/retry"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'retry))

    (test-case "/compact returns parsed-command"
      (define r (parse-command-name "/compact"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'compact))

    (test-case "/status returns parsed-command"
      (define r (parse-command-name "/status"))
      (check-pred parsed-command? r)
      (check-equal? (parsed-command-canonical-name r) 'status))

    ;; ── resolve-command-name ──
    (test-case "resolve-command-name for alias"
      (check-equal? (resolve-command-name "/h") 'help)
      (check-equal? (resolve-command-name "/q") 'quit)
      (check-equal? (resolve-command-name "/m") 'model)
      (check-false (resolve-command-name "/xyz")))

    ;; ── Structured result properties ──
    (test-case "parsed-command is transparent"
      (define r (parse-command-name "/help"))
      (check-true (parsed-command? r))
      (check-false (not (parsed-command? r))))))

(run-tests parse-suite 'verbose)
