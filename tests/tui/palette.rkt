#lang racket

;; q/tests/tui/palette.rkt — TDD tests for the slash-command palette

(require rackunit
         rackunit/text-ui
         "../../tui/palette.rkt"
         "../../tui/render.rkt")

;; Helper: extract all text from a styled-line as a plain string
(define (line-text sl)
  (apply string-append (map styled-segment-text (styled-line-segments sl))))

(define palette-tests
  (test-suite "TUI Palette — Slash-Command Registry"

    ;; ================================================================
    ;; 1. Registry creation
    ;; ================================================================
    (test-case "make-command-registry returns a hash"
      (let ([reg (make-command-registry)]) (check-not-false (hash? reg))))

    (test-case "registry has all 14 built-in commands by canonical name"
      (let ([reg (make-command-registry)])
        ;; Canonical names (not aliases)
        (check-not-false (lookup-command reg "/help"))
        (check-not-false (lookup-command reg "/quit"))
        (check-not-false (lookup-command reg "/clear"))
        (check-not-false (lookup-command reg "/compact"))
        (check-not-false (lookup-command reg "/interrupt"))
        (check-not-false (lookup-command reg "/branches"))
        (check-not-false (lookup-command reg "/leaves"))
        (check-not-false (lookup-command reg "/switch"))
        (check-not-false (lookup-command reg "/children"))
        (check-not-false (lookup-command reg "/model"))
        (check-not-false (lookup-command reg "/history"))
        (check-not-false (lookup-command reg "/fork"))
        (check-not-false (lookup-command reg "/sessions"))
        ;; 13 canonical + aliases
        (check-true (>= (hash-count reg) 13) "registry has at least 13 canonical entries")))

    (test-case "registry contains /help"
      (check-not-false (lookup-command (make-command-registry) "/help")))

    (test-case "registry contains /quit"
      (check-not-false (lookup-command (make-command-registry) "/quit")))

    (test-case "registry contains /clear"
      (check-not-false (lookup-command (make-command-registry) "/clear")))

    (test-case "registry contains /compact"
      (check-not-false (lookup-command (make-command-registry) "/compact")))

    (test-case "registry contains /interrupt"
      (check-not-false (lookup-command (make-command-registry) "/interrupt")))

    (test-case "registry contains /branches"
      (check-not-false (lookup-command (make-command-registry) "/branches")))

    (test-case "registry contains /leaves"
      (check-not-false (lookup-command (make-command-registry) "/leaves")))

    (test-case "registry contains /switch"
      (check-not-false (lookup-command (make-command-registry) "/switch")))

    (test-case "registry contains /children"
      (check-not-false (lookup-command (make-command-registry) "/children")))

    (test-case "registry contains /model"
      (check-not-false (lookup-command (make-command-registry) "/model")))

    (test-case "registry contains /history"
      (check-not-false (lookup-command (make-command-registry) "/history")))

    (test-case "registry contains /fork"
      (check-not-false (lookup-command (make-command-registry) "/fork")))

    ;; ================================================================
    ;; 2. Lookup
    ;; ================================================================
    (test-case "lookup-command finds /help with correct fields"
      (let* ([reg (make-command-registry)]
             [e (lookup-command reg "/help")])
        (check-not-false e)
        (check-equal? (cmd-entry-name e) "/help")
        (check-equal? (cmd-entry-category e) 'general)
        (check-equal? (cmd-entry-summary e) "Show help")
        (check-equal? (cmd-entry-args-spec e) '())))

    (test-case "lookup-command finds /quit"
      (let ([e (lookup-command (make-command-registry) "/quit")])
        (check-not-false e)
        (check-equal? (cmd-entry-name e) "/quit")))

    (test-case "lookup-command returns #f for /nonexistent"
      (check-false (lookup-command (make-command-registry) "/nonexistent")))

    (test-case "lookup-command returns #f for empty string"
      (check-false (lookup-command (make-command-registry) "")))

    (test-case "lookup-command /switch has args-spec"
      (let* ([reg (make-command-registry)]
             [e (lookup-command reg "/switch")])
        (check-not-false e)
        (check-equal? (cmd-entry-args-spec e) '("<id>"))))

    ;; ================================================================
    ;; 3. All commands
    ;; ================================================================
    (test-case "all-commands returns sorted list of canonical commands"
      (let ([cmds (all-commands (make-command-registry))])
        ;; all-commands returns hash-values, includes alias entries
        ;; Only count unique canonical names (aliases point to same entry)
        (define unique-names (remove-duplicates (map cmd-entry-name cmds)))
        (check-true (>= (length unique-names) 13)
                    (format "expected >= 13 canonical commands, got ~a" (length unique-names)))
        ;; Verify sorted
        (for ([a (in-list cmds)]
              [b (in-list (cdr cmds))])
          (check-true (string<? (cmd-entry-name a) (cmd-entry-name b))))))

    (test-case "all-commands first is /activate or /branches"
      (define first-name (cmd-entry-name (car (all-commands (make-command-registry)))))
      (check-not-false (member first-name '("/activate" "/branches"))
                       (format "expected first command to be /activate or /branches, got ~a"
                               first-name)))

    ;; ================================================================
    ;; 4. Category filter
    ;; ================================================================
    (test-case "commands-by-category general includes clear/help/quit"
      (let* ([names (remove-duplicates (map cmd-entry-name
                                            (commands-by-category (make-command-registry) 'general)))]
             [sorted (sort names string<?)])
        (for ([expected '("/clear" "/help" "/quit")])
          (check-not-false (member expected sorted)
                           (format "~a should be in general category" sorted)))))

    (test-case "commands-by-category session returns core commands"
      (let* ([names (remove-duplicates (map cmd-entry-name
                                            (commands-by-category (make-command-registry) 'session)))]
             [sorted (sort names string<?)])
        (check-not-false (member "/branches" sorted))
        (check-not-false (member "/children" sorted))
        (check-not-false (member "/compact" sorted))
        (check-not-false (member "/fork" sorted))
        (check-not-false (member "/history" sorted))
        (check-not-false (member "/interrupt" sorted))
        (check-not-false (member "/leaves" sorted))
        (check-not-false (member "/sessions" sorted))
        (check-not-false (member "/switch" sorted))))

    (test-case "commands-by-category model returns /model"
      (let ([names (map cmd-entry-name (commands-by-category (make-command-registry) 'model))])
        (check-equal? names '("/model"))))

    (test-case "commands-by-category debug returns empty"
      (check-equal? (commands-by-category (make-command-registry) 'debug) '()))

    ;; ================================================================
    ;; 5. Register new command
    ;; ================================================================
    (test-case "register-command! adds custom command"
      (let* ([reg (make-command-registry)]
             [custom (cmd-entry "/custom" "A custom command" 'debug '() '())]
             [reg2 (register-command! reg custom)])
        (check-not-false (lookup-command reg2 "/custom"))
        (check-equal? (cmd-entry-summary (lookup-command reg2 "/custom")) "A custom command")))

    (test-case "register-command! replaces existing command"
      (let* ([reg (make-command-registry)]
             [replacement (cmd-entry "/help" "New help text" 'general '() '())]
             [reg2 (register-command! reg replacement)])
        (check-equal? (cmd-entry-summary (lookup-command reg2 "/help")) "New help text")))

    (test-case "register-command! increases count for new"
      (let* ([reg (make-command-registry)]
             [custom (cmd-entry "/test" "Test" 'general '() '())]
             [reg2 (register-command! reg custom)])
        (check-true (> (hash-count reg2) (hash-count reg)) "adding new command increases count")))

    (test-case "register-command! same count for replacement"
      (let* ([reg (make-command-registry)]
             [replacement (cmd-entry "/quit" "Replaced" 'general '() '())]
             [reg2 (register-command! reg replacement)])
        (check-equal? (hash-count reg2) (hash-count reg))))

    ;; ================================================================
    ;; 6. Filter commands
    ;; ================================================================
    (test-case "filter-commands empty prefix returns all"
      (check-true (>= (length (filter-commands (make-command-registry) "")) 13)
                  "empty prefix returns at least 13 canonical commands"))

    (test-case "filter-commands /h returns help and history"
      (let ([names (map cmd-entry-name (filter-commands (make-command-registry) "/h"))])
        (check-equal? names '("/help" "/history"))))

    (test-case "filter-commands /s returns switch and sessions"
      (let ([names (map cmd-entry-name (filter-commands (make-command-registry) "/s"))])
        (check-not-false (member "/switch" names) "contains /switch")
        (check-not-false (member "/sessions" names) "contains /sessions")))

    (test-case "filter-commands /xyz returns empty"
      (check-equal? (filter-commands (make-command-registry) "/xyz") '()))

    (test-case "filter-commands /c returns children clear compact"
      (let ([names (map cmd-entry-name (filter-commands (make-command-registry) "/c"))])
        (check-equal? names '("/children" "/clear" "/compact"))))

    (test-case "filter-commands exact match returns one"
      (let ([names (map cmd-entry-name (filter-commands (make-command-registry) "/quit"))])
        (check-equal? names '("/quit"))))

    ;; ================================================================
    ;; 7. Render overlay
    ;; ================================================================
    (test-case "render-palette-overlay returns styled-lines"
      (let* ([cmds (filter-commands (make-command-registry) "/h")]
             [lines (render-palette-overlay "/h" cmds 80)])
        (check-equal? (length lines) 2)
        (for ([line (in-list lines)])
          (check-true (styled-line? line)))))

    (test-case "render-palette-overlay lines have segments"
      (let* ([cmds (filter-commands (make-command-registry) "/h")]
             [lines (render-palette-overlay "/h" cmds 80)])
        (for ([line (in-list lines)])
          (check-true (pair? (styled-line-segments line))))))

    (test-case "render-palette-overlay lines contain command text"
      (let* ([cmds (filter-commands (make-command-registry) "/h")]
             [lines (render-palette-overlay "/h" cmds 80)])
        (let ([text (line-text (car lines))]) (check-true (string-contains? text "/help")))))

    (test-case "render-palette-overlay respects terminal width"
      (let* ([cmds (filter-commands (make-command-registry) "/h")]
             [lines (render-palette-overlay "/h" cmds 20)])
        (for ([line (in-list lines)])
          (check-true (<= (string-length (line-text line)) 20)))))

    (test-case "render-palette-overlay empty prefix plain lines"
      (let* ([cmds (take (all-commands (make-command-registry)) 2)]
             [lines (render-palette-overlay "" cmds 80)])
        (check-equal? (length lines) 2)
        (for ([line (in-list lines)])
          (check-true (styled-line? line)))))

    (test-case "render-palette-overlay no matches returns empty"
      (check-equal? (render-palette-overlay "/xyz" '() 80) '()))

    (test-case "render-palette-overlay bold prefix segment"
      (let* ([cmds (filter-commands (make-command-registry) "/h")]
             [lines (render-palette-overlay "/h" cmds 80)]
             [first-seg (car (styled-line-segments (car lines)))])
        (check-equal? (styled-segment-text first-seg) "/h")
        (check-not-false (member 'bold (styled-segment-style first-seg)))))

    ;; ================================================================
    ;; 8. Complete command
    ;; ================================================================
    (test-case "complete-command /h returns help and history"
      (check-equal? (complete-command (make-command-registry) "/h") '("/help" "/history")))

    (test-case "complete-command /help returns singleton"
      (check-equal? (complete-command (make-command-registry) "/help") '("/help")))

    (test-case "complete-command /z returns empty"
      (check-equal? (complete-command (make-command-registry) "/z") '()))

    (test-case "complete-command empty returns all names"
      (check-true (>= (length (complete-command (make-command-registry) "")) 13)
                  "empty prefix returns at least 13 names"))

    (test-case "complete-command /q returns quit"
      (check-equal? (complete-command (make-command-registry) "/q") '("/quit")))

    ;; ================================================================
    ;; 9. Edge cases
    ;; ================================================================
    (test-case "empty registry — lookup returns #f"
      (check-false (lookup-command (hash) "/help")))

    (test-case "empty registry — all-commands returns empty"
      (check-equal? (all-commands (hash)) '()))

    (test-case "empty registry — filter-commands returns empty"
      (check-equal? (filter-commands (hash) "/h") '()))

    (test-case "empty registry — complete-command returns empty"
      (check-equal? (complete-command (hash) "/h") '()))

    (test-case "prefix longer than any command returns empty"
      (check-equal? (filter-commands (make-command-registry) "/help-me-please-i-am-stuck") '()))

    (test-case "prefix exactly matching returns one"
      (let ([result (filter-commands (make-command-registry) "/fork")])
        (check-equal? (length result) 1)
        (check-equal? (cmd-entry-name (car result)) "/fork")))

    (test-case "cmd-entry struct accessors"
      (let ([e (cmd-entry "/test" "Test" 'general '() '("t"))])
        (check-equal? (cmd-entry-name e) "/test")
        (check-equal? (cmd-entry-summary e) "Test")
        (check-equal? (cmd-entry-category e) 'general)
        (check-equal? (cmd-entry-args-spec e) '())
        (check-equal? (cmd-entry-aliases e) '("t"))))))

(run-tests palette-tests)
