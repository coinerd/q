#lang racket

;; tests/test-browser-safe-mode.rkt — F1: browser tools blocked in safe-mode

(require rackunit
         "../util/safe-mode/safe-mode-state.rkt")

;; ---------------------------------------------------------------------------

(test-case "safe-mode blocks browser_open"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_open"))))

(test-case "safe-mode blocks browser_click"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_click"))))

(test-case "safe-mode blocks browser_type"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_type"))))

(test-case "safe-mode blocks browser_screenshot"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_screenshot"))))

(test-case "safe-mode blocks browser_check_local_app"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_check_local_app"))))

(test-case "safe-mode blocks browser_close"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_close"))))

(test-case "safe-mode blocks browser_observe"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_observe"))))

(test-case "safe-mode blocks browser_scroll"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_scroll"))))

(test-case "safe-mode blocks browser_extract"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_extract"))))

(test-case "safe-mode blocks browser_press"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_press"))))

(test-case "safe-mode allows browser_open when safe-mode off"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #f)])
    (check-true (allowed-tool? "browser_open"))))

(test-case "safe-mode blocks via prefix: browser_future_tool"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "browser_future_tool"))))

(test-case "safe-mode still blocks bash and edit"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-false (allowed-tool? "bash"))
    (check-false (allowed-tool? "edit"))))

(test-case "safe-mode still allows read when active"
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t)])
    (check-true (allowed-tool? "read"))))

(test-case "blocked-browser-tools list has 10 entries"
  (check-equal? (length blocked-browser-tools) 10))
