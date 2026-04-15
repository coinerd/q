#lang racket

;; tests/test-dialog-api.rkt — tests for Extension Dialog Primitives (#721-#724)
;;
;; Covers:
;;   - #721: ctx-notify for transient status messages
;;   - #722: ctx-confirm for yes/no prompts
;;   - #723: ctx-select for pick-from-list
;;   - #724: Parent feature

(require rackunit
         "../tui/state.rkt"
         "../extensions/dialog-api.rkt")

;; ============================================================
;; #721: ctx-notify
;; ============================================================

(test-case "notify-level?: validates levels"
  (check-true (notify-level? 'info))
  (check-true (notify-level? 'warn))
  (check-true (notify-level? 'error))
  (check-true (notify-level? 'success))
  (check-false (notify-level? 'invalid))
  (check-false (notify-level? "info")))

(test-case "ctx-notify: creates notification"
  (define n (ctx-notify 'info "Operation complete"))
  (check-true (notification? n))
  (check-equal? (notification-level n) 'info)
  (check-equal? (notification-message n) "Operation complete")
  (check-equal? (notification-duration n) 3))

(test-case "ctx-notify: custom duration"
  (define n (ctx-notify 'warn "Slow operation" #:duration 10))
  (check-equal? (notification-duration n) 10))

(test-case "ctx-notify: error level"
  (define n (ctx-notify 'error "Something failed"))
  (check-equal? (notification-level n) 'error))

(test-case "ctx-notify: success level"
  (define n (ctx-notify 'success "All tests pass"))
  (check-equal? (notification-level n) 'success))

(test-case "notification-state: initial state"
  (define ns (notification-state #f '()))
  (check-false (notification-state-active ns))
  (check-equal? (notification-state-queue ns) '()))

(test-case "notification-state-add: activates when empty"
  (define ns (notification-state #f '()))
  (define n (ctx-notify 'info "hello"))
  (define ns1 (notification-state-add ns n))
  (check-equal? (notification-state-active ns1) n)
  (check-equal? (notification-state-queue ns1) '()))

(test-case "notification-state-add: queues when active"
  (define n1 (ctx-notify 'info "first"))
  (define n2 (ctx-notify 'warn "second"))
  (define ns (notification-state n1 '()))
  (define ns1 (notification-state-add ns n2))
  (check-equal? (notification-state-active ns1) n1)
  (check-equal? (length (notification-state-queue ns1)) 1))

(test-case "notification-state-pop: advances to next"
  (define n1 (ctx-notify 'info "first"))
  (define n2 (ctx-notify 'warn "second"))
  (define ns (notification-state n1 (list n2)))
  (define ns1 (notification-state-pop ns))
  (check-equal? (notification-state-active ns1) n2)
  (check-equal? (notification-state-queue ns1) '()))

(test-case "notification-state-pop: empty queue returns #f active"
  (define n1 (ctx-notify 'info "only"))
  (define ns (notification-state n1 '()))
  (define ns1 (notification-state-pop ns))
  (check-false (notification-state-active ns1)))

(test-case "expired-notification?: detects expired"
  (define n (notification 'info "old" (- (current-seconds) 10) 3))
  (define ns (notification-state n '()))
  (check-true (expired-notification? ns)))

(test-case "expired-notification?: not expired"
  (define n (notification 'info "fresh" (current-seconds) 30))
  (define ns (notification-state n '()))
  (check-false (expired-notification? ns)))

(test-case "expired-notification?: no active returns #f"
  (define ns (notification-state #f '()))
  (check-false (expired-notification? ns)))

(test-case "apply-notification: sets status message"
  (define state (initial-ui-state))
  (define n (ctx-notify 'info "Test message"))
  (define s1 (apply-notification state n))
  (check-true (string-contains? (ui-state-status-message s1) "Test message")))

;; ============================================================
;; #722: ctx-confirm
;; ============================================================

(test-case "ctx-confirm: returns confirm-result"
  (define result (ctx-confirm "Continue?"))
  (check-true (confirm-result? result))
  (check-false (confirm-result-timed-out? result)))

(test-case "ctx-confirm: default value"
  (define result (ctx-confirm "Are you sure?" #:default #t))
  (check-true (confirm-result-value result)))

(test-case "confirm-result: struct"
  (define r (confirm-result #f #t))
  (check-false (confirm-result-value r))
  (check-true (confirm-result-timed-out? r)))

;; ============================================================
;; #723: ctx-select
;; ============================================================

(test-case "select-option: struct"
  (define opt (select-option "id1" "Option 1" "A description"))
  (check-equal? (select-option-id opt) "id1")
  (check-equal? (select-option-label opt) "Option 1")
  (check-equal? (select-option-description opt) "A description"))

(test-case "ctx-select: returns select-result with first option"
  (define options (list (select-option "a" "Alpha" "")
                        (select-option "b" "Beta" "")))
  (define result (ctx-select options))
  (check-true (select-result? result))
  (check-equal? (select-result-selected-id result) "a")
  (check-false (select-result-timed-out? result)))

(test-case "ctx-select: returns #f for empty options"
  (define result (ctx-select '()))
  (check-false result))

(test-case "select-result: struct"
  (define r (select-result "opt-42" #t))
  (check-equal? (select-result-selected-id r) "opt-42")
  (check-true (select-result-timed-out? r)))

;; ============================================================
;; #724: Integration
;; ============================================================

(test-case "integration: notify then confirm"
  ;; Simulate: notify user, then ask for confirmation
  (define n (ctx-notify 'info "3 files modified"))
  (check-true (notification? n))
  (define confirm (ctx-confirm "Apply changes?" #:default #t))
  (check-true (confirm-result-value confirm)))
