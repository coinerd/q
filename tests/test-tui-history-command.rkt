#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tui-history-command.rkt
;; W0 (#8715): Tests for /history command improvements (V9949-TMUX-05).

(require rackunit
         racket/string
         (only-in "../util/message/message.rkt"
                  message
                  message-id
                  message-role
                  message-content
                  message-timestamp
                  message-meta)
         (only-in "../util/content/content-parts.rkt" text-part)
         "../tui/commands/session.rkt")

;; ---------------------------------------------------------------------------
;; pad2
;; ---------------------------------------------------------------------------

(test-case "pad2 zero-pads single digit"
  (check-equal? (pad2 5) "05"))

(test-case "pad2 does not pad double digit"
  (check-equal? (pad2 12) "12"))

(test-case "pad2 handles zero"
  (check-equal? (pad2 0) "00"))

;; ---------------------------------------------------------------------------
;; format-ts
;; ---------------------------------------------------------------------------

(test-case "format-ts formats valid timestamp"
  (define result (format-ts 1689300000))
  (check-true (regexp-match? #rx"^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]$" result)
              (format "result: ~a" result)))

(test-case "format-ts returns placeholder for zero"
  (check-equal? (format-ts 0) "--:--:--"))

(test-case "format-ts returns placeholder for #f"
  (check-equal? (format-ts #f) "--:--:--"))

;; ---------------------------------------------------------------------------
;; message-excerpt
;; ---------------------------------------------------------------------------

(test-case "message-excerpt extracts text from message"
  (define msg (message "m1" #f 'user 'text (list (text-part "text" "Hello world")) 0 (hash)))
  (define excerpt (message-excerpt msg))
  (check-true (string-contains? excerpt "Hello world")))

(test-case "message-excerpt truncates long text"
  (define long-text (make-string 100 #\x))
  (define msg (message "m2" #f 'user 'text (list (text-part "text" long-text)) 0 (hash)))
  (define excerpt (message-excerpt msg))
  (check-true (< (string-length excerpt) 70))
  (check-true (string-suffix? excerpt "...")))

(test-case "message-excerpt handles empty content"
  (define msg (message "m3" #f 'user 'text '() 0 (hash)))
  (define excerpt (message-excerpt msg))
  (check-equal? excerpt "(non-text)"))

(test-case "message-excerpt collapses whitespace"
  (define msg (message "m4" #f 'user 'text (list (text-part "text" "  hello   world  ")) 0 (hash)))
  (define excerpt (message-excerpt msg))
  (check-false (string-contains? excerpt "  "))
  (check-true (string-contains? excerpt "hello world")))

(test-case "message-excerpt respects custom max-len"
  (define msg (message "m5" #f 'user 'text (list (text-part "text" "abcdefghij")) 0 (hash)))
  (define excerpt (message-excerpt msg 5))
  (check-true (string-suffix? excerpt "...")))
