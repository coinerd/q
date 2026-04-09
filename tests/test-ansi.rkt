#lang racket

(require rackunit
         rackunit/text-ui
         "../util/ansi.rkt")

(define ansi-suite
  (test-suite
   "ansi tests"

   ;; ============================================================
   ;; Style constants — exact escape sequences
   ;; ============================================================

   (test-case "ANSI-RESET is escape sequence"
     (check-equal? ANSI-RESET "\x1b[0m"))

   (test-case "ANSI-BOLD is escape sequence"
     (check-equal? ANSI-BOLD "\x1b[1m"))

   (test-case "ANSI-DIM is escape sequence"
     (check-equal? ANSI-DIM "\x1b[2m"))

   (test-case "ANSI-ITALIC is escape sequence"
     (check-equal? ANSI-ITALIC "\x1b[3m"))

   (test-case "ANSI-UNDERLINE is escape sequence"
     (check-equal? ANSI-UNDERLINE "\x1b[4m"))

   (test-case "ANSI-INVERSE is escape sequence"
     (check-equal? ANSI-INVERSE "\x1b[7m"))

   (test-case "color constants have correct codes"
     (check-equal? ANSI-RED      "\x1b[31m")
     (check-equal? ANSI-GREEN    "\x1b[32m")
     (check-equal? ANSI-YELLOW   "\x1b[33m")
     (check-equal? ANSI-BLUE     "\x1b[34m")
     (check-equal? ANSI-MAGENTA  "\x1b[35m")
     (check-equal? ANSI-CYAN     "\x1b[36m")
     (check-equal? ANSI-WHITE    "\x1b[37m")
     (check-equal? ANSI-GRAY     "\x1b[90m"))

   (test-case "all constants start with ESC"
     (for ([c (list ANSI-RESET ANSI-BOLD ANSI-DIM ANSI-ITALIC
                    ANSI-UNDERLINE ANSI-INVERSE
                    ANSI-RED ANSI-GREEN ANSI-YELLOW ANSI-BLUE
                    ANSI-MAGENTA ANSI-CYAN ANSI-WHITE ANSI-GRAY)])
       (check-equal? (string-ref c 0) #\u001B)))

   ;; ============================================================
   ;; styled — applies styles to text
   ;; ============================================================

   (test-case "styled with single style wraps text when color enabled"
     (define result (styled "hello" '(red)))
     (if (color-enabled?)
         (begin
           (check-true (string-contains? result ANSI-RED))
           (check-true (string-contains? result ANSI-RESET))
           (check-true (string-contains? result "hello")))
         (check-equal? result "hello")))

   (test-case "styled with multiple styles concatenates codes"
     (define result (styled "prompt" '(bold cyan)))
     (if (color-enabled?)
         (begin
           (check-true (string-contains? result ANSI-BOLD))
           (check-true (string-contains? result ANSI-CYAN))
           (check-true (string-contains? result "prompt"))
           (check-true (string-contains? result ANSI-RESET)))
         (check-equal? result "prompt")))

   (test-case "styled with empty style list returns text with reset only if color enabled"
     (define result (styled "plain" '()))
     (if (color-enabled?)
         (check-equal? result (string-append "plain" ANSI-RESET))
         (check-equal? result "plain")))

   (test-case "styled with empty string"
     (define result (styled "" '(bold)))
     (if (color-enabled?)
         (check-equal? result (string-append ANSI-BOLD ANSI-RESET))
         (check-equal? result "")))

   (test-case "styled with unknown style symbol produces no code for that symbol"
     (define result (styled "test" '(fake bold)))
     (if (color-enabled?)
         (begin
           ;; bold is known, fake is not — result contains bold but no garbage
           (check-true (string-contains? result ANSI-BOLD))
           (check-true (string-contains? result "test"))
           (check-true (string-contains? result ANSI-RESET)))
         (check-equal? result "test")))

   (test-case "styled with all known style symbols does not error"
     (define result (styled "ok" '(bold dim italic underline inverse
                                    red green yellow blue magenta cyan white gray)))
     (if (color-enabled?)
         (begin
           (check-true (string-contains? result "ok"))
           (check-true (string-contains? result ANSI-RESET)))
         (check-equal? result "ok")))

   ;; ============================================================
   ;; styled-prompt
   ;; ============================================================

   (test-case "styled-prompt applies bold cyan"
     (define result (styled-prompt "q>"))
     (if (color-enabled?)
         (begin
           (check-true (string-contains? result ANSI-BOLD))
           (check-true (string-contains? result ANSI-CYAN))
           (check-true (string-contains? result "q>"))
           (check-true (string-contains? result ANSI-RESET)))
         (check-equal? result "q>")))

   ;; ============================================================
   ;; ansi — wrap text with raw codes
   ;; ============================================================

   (test-case "ansi wraps text with given codes and reset when color enabled"
     (define result (ansi ANSI-RED "error"))
     (if (color-enabled?)
         (check-equal? result (string-append ANSI-RED "error" ANSI-RESET))
         (check-equal? result "error")))

   (test-case "ansi with empty string"
     (define result (ansi ANSI-BOLD ""))
     (if (color-enabled?)
         (check-equal? result (string-append ANSI-BOLD ANSI-RESET))
         (check-equal? result "")))

   (test-case "ansi with multiple concatenated codes"
     (define codes (string-append ANSI-BOLD ANSI-GREEN))
     (define result (ansi codes "ok"))
     (if (color-enabled?)
         (check-equal? result (string-append codes "ok" ANSI-RESET))
         (check-equal? result "ok")))

   ;; ============================================================
   ;; ansi-reset
   ;; ============================================================

   (test-case "ansi-reset returns reset code or empty depending on TTY"
     (define result (ansi-reset))
     (if (color-enabled?)
         (check-equal? result ANSI-RESET)
         (check-equal? result "")))

   ;; ============================================================
   ;; color-enabled? — boolean predicate
   ;; ============================================================

   (test-case "color-enabled? returns a boolean"
     (check-pred boolean? (color-enabled?)))

   (test-case "color-enabled? is deterministic across calls"
     (check-equal? (color-enabled?) (color-enabled?)))

   ;; ============================================================
   ;; Consistency across API
   ;; ============================================================

   (test-case "styled and ansi behave consistently with color state"
     (define styled-result (styled "x" '(bold)))
     (define ansi-result (ansi ANSI-BOLD "x"))
     ;; Both should either contain ANSI codes or both be plain
     (check-equal? (string-contains? styled-result "\x1b[")
                   (string-contains? ansi-result "\x1b[")))

   (test-case "ansi-reset and styled agree on color state"
     (define reset-result (ansi-reset))
     (define styled-result (styled "test" '(red)))
     (check-equal? (string=? reset-result "")
                   (string=? styled-result "test")))))

(run-tests ansi-suite 'verbose)
