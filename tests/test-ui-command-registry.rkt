#lang racket

;; q/tests/test-ui-command-registry.rkt — Tests for unified command metadata registry
;;
;; W4.1 (v0.94.4): Verify canonical command list, registry operations, and parity.

(require rackunit
         rackunit/text-ui
         "../ui-core/command-registry.rkt")

(define reg (make-ui-command-registry canonical-commands))

(define-test-suite
 test-ui-command-registry

 ;; ── Canonical list ───

 (test-case "19 canonical commands registered"
   (check-equal? (length (ui-registry-all reg)) 19))

 (test-case "all commands have / prefix"
   (for ([cmd (ui-registry-all reg)])
     (check-true (string-prefix? (ui-command-name cmd) "/")
                 (format "~a should start with /" (ui-command-name cmd)))))

 (test-case "all commands have non-empty summary"
   (for ([cmd (ui-registry-all reg)])
     (check-true (non-empty-string? (ui-command-summary cmd))
                 (format "~a should have summary" (ui-command-name cmd)))))

 (test-case "categories are valid"
   (for ([cmd (ui-registry-all reg)])
     (check-not-false (member (ui-command-category cmd) '(general session model debug))
                 (format "~a has invalid category ~a"
                         (ui-command-name cmd) (ui-command-category cmd)))))

 ;; ── Lookup ───

 (test-case "lookup finds /help"
   (check-not-false (ui-registry-lookup reg "/help")))

 (test-case "lookup returns #f for unknown command"
   (check-false (ui-registry-lookup reg "/nonexistent")))

 (test-case "lookup by alias is NOT supported (by name only)"
   ;; Aliases are for display, not registry lookup
   (check-false (ui-registry-lookup reg "h")))

 ;; ── By category ───

 (test-case "general commands include /help, /quit, /clear, /status"
   (define general (ui-registry-by-category reg 'general))
   (define names (map ui-command-name general))
   (for ([name '("/help" "/quit" "/clear" "/status")])
     (check-not-false (member name names) (format "~a should be general" name))))

 (test-case "session commands include /branches, /switch, /fork, /tree"
   (define session (ui-registry-by-category reg 'session))
   (define names (map ui-command-name session))
   (for ([name '("/branches" "/switch" "/fork" "/tree")])
     (check-not-false (member name names) (format "~a should be session" name))))

 ;; ── Filter ───

 (test-case "filter by 'help' returns /help"
   (define results (ui-registry-filter reg "help"))
   (check-true (>= (length results) 1))
   (check-not-false (member "/help" (map ui-command-name results))))

 (test-case "filter by 'sess' returns session-related commands"
   (define results (ui-registry-filter reg "sess"))
   (check-true (>= (length results) 1)))

 (test-case "filter by alias 'h' returns /help"
   (define results (ui-registry-filter reg "h"))
   (check-not-false (member "/help" (map ui-command-name results))))

 ;; ── Names ───

 (test-case "ui-registry-names returns sorted list"
   (define names (ui-registry-names reg))
   (check-equal? names (sort names string<?)))

 ;; ── Frontend availability ───

 (test-case "all canonical commands available in TUI"
   (for ([cmd (ui-registry-all reg)])
     (check-true (ui-command-tui? cmd)
                 (format "~a should be available in TUI" (ui-command-name cmd)))))

 (test-case "all canonical commands available in GUI"
   (for ([cmd (ui-registry-all reg)])
     (check-true (ui-command-gui? cmd)
                 (format "~a should be available in GUI" (ui-command-name cmd)))))

 ;; ── TUI/GUI parity ───

 (test-case "TUI and GUI have identical command names"
   (define tui-names (sort (map ui-command-name (filter ui-command-tui? (ui-registry-all reg))) string<?))
   (define gui-names (sort (map ui-command-name (filter ui-command-gui? (ui-registry-all reg))) string<?))
   (check-equal? tui-names gui-names))

 ;; ── Custom registry ───

 (test-case "empty registry works"
   (define empty-reg (make-ui-command-registry '()))
   (check-equal? (length (ui-registry-all empty-reg)) 0))

 (test-case "custom registry with one command"
   (define cmd (ui-command "/test" "Test" 'debug '() '() #t #f))
   (define r (make-ui-command-registry (list cmd)))
   (check-equal? (length (ui-registry-all r)) 1)
   (check-false (ui-command-gui? (ui-registry-lookup r "/test")))))

(run-tests test-ui-command-registry)
