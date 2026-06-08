#lang racket

;; @speed fast  ;; @suite security

;; tests/test-browser-permission-tiers.rkt — F6: browser tool risk tiers

(require rackunit
         "../tools/permission-gate.rkt")

;; ---------------------------------------------------------------------------
;; Browser tool risk tier classification
;; ---------------------------------------------------------------------------

(test-case "LOW risk browser tools: auto-approved"
  (define cfg (make-default-permission-config))
  (check-false (tool-needs-approval? cfg "browser_observe"))
  (check-false (tool-needs-approval? cfg "browser_extract"))
  (check-false (tool-needs-approval? cfg "browser_screenshot"))
  (check-false (tool-needs-approval? cfg "browser_scroll"))
  (check-false (tool-needs-approval? cfg "browser_close")))

(test-case "MEDIUM risk browser tools: needs approval"
  (define cfg (make-default-permission-config))
  (check-true (tool-needs-approval? cfg "browser_open")))

(test-case "HIGH risk browser tools: needs approval"
  (define cfg (make-default-permission-config))
  (check-true (tool-needs-approval? cfg "browser_click"))
  (check-true (tool-needs-approval? cfg "browser_type"))
  (check-true (tool-needs-approval? cfg "browser_press"))
  (check-true (tool-needs-approval? cfg "browser_check_local_app")))

(test-case "browser tools in permissive mode: unknown tools auto-approved"
  (define cfg (make-default-permission-config #:policy-mode 'permissive))
  ;; Unknown tools auto-approved in permissive mode
  (check-false (tool-needs-approval? cfg "unknown_tool"))
  ;; But explicitly listed needs-approval tools still need approval
  (check-true (tool-needs-approval? cfg "browser_click"))
  (check-true (tool-needs-approval? cfg "browser_open")))

(test-case "browser tools in auto-approved set are accessible"
  (define cfg (make-default-permission-config))
  (define auto (permission-config-auto-approved-tools cfg))
  (check-true (set-member? auto "browser_observe"))
  (check-true (set-member? auto "browser_extract"))
  (check-true (set-member? auto "browser_screenshot"))
  (check-true (set-member? auto "browser_scroll"))
  (check-true (set-member? auto "browser_close")))

(test-case "browser tools in needs-approval set"
  (define cfg (make-default-permission-config))
  (define needs (permission-config-needs-approval-tools cfg))
  (check-true (set-member? needs "browser_open"))
  (check-true (set-member? needs "browser_click"))
  (check-true (set-member? needs "browser_type"))
  (check-true (set-member? needs "browser_press"))
  (check-true (set-member? needs "browser_check_local_app")))
