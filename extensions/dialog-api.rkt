#lang racket/base

;; extensions/dialog-api.rkt — Extension Dialog Primitives (#721-#724)
;;
;; Provides:
;;   #721: ctx-notify — transient status bar messages
;;   #722: ctx-confirm — yes/no prompts
;;   #723: ctx-select — pick-from-list
;;   #724: Parent feature

(require racket/contract
         racket/list
         "ui-surface.rkt"
         (only-in "../tui/state.rkt" ui-state ui-state-status-message set-status-message))

;; Notification levels
(provide NOTIFY-INFO
         NOTIFY-WARN
         NOTIFY-ERROR
         NOTIFY-SUCCESS
         notify-level?
         notification
         notification?
         notification-level
         notification-message
         notification-timestamp
         notification-duration
         confirm-result
         confirm-result?
         confirm-result-value
         confirm-result-timed-out?
         select-option
         select-option?
         select-option-id
         select-option-label
         select-option-description
         select-result
         select-result?
         select-result-selected-id
         select-result-timed-out?
         notification-state
         notification-state?
         notification-state-active
         notification-state-queue
         (contract-out
          [ctx-notify
           (->* (notify-level? string?) (#:duration exact-nonnegative-integer?) notification?)]
          [ctx-confirm
           (->* (string?)
                (#:default (or/c boolean? #f) #:timeout exact-nonnegative-integer?)
                confirm-result?)]
          [ctx-select
           (->* ((listof select-option?))
                (#:prompt string?
                          #:timeout exact-nonnegative-integer?
                          #:max-visible exact-nonnegative-integer?)
                (or/c select-result? #f))]
          [apply-notification (-> (or/c any/c box?) notification? any/c)]
          [expired-notification? (-> notification-state? boolean?)]
          [notification-state-add (-> notification-state? notification? notification-state?)]
          [notification-state-pop (-> notification-state? notification-state?)]
          [notification-state-clear-expired (-> notification-state? notification-state?)]))

;; ============================================================
;; Constants
;; ============================================================

(define NOTIFY-INFO 'info)
(define NOTIFY-WARN 'warn)
(define NOTIFY-ERROR 'error)
(define NOTIFY-SUCCESS 'success)

(define (notify-level? v)
  (and (symbol? v)
       (or (eq? v NOTIFY-INFO) (eq? v NOTIFY-WARN) (eq? v NOTIFY-ERROR) (eq? v NOTIFY-SUCCESS))))

;; ============================================================
;; Structs
;; ============================================================

;; A single notification
(struct notification
        (level ; notify-level?
         message ; string?
         timestamp ; number? — current-seconds
         duration ; number? — seconds to display (default 3)
         )
  #:transparent)

;; Result of a confirm dialog
(struct confirm-result
        (value ; boolean — user's answer
         timed-out? ; boolean — did the prompt time out?
         )
  #:transparent)

;; A single option in a select dialog
(struct select-option
        (id ; string?
         label ; string?
         description ; string? (may be empty)
         )
  #:transparent)

;; Result of a select dialog
(struct select-result
        (selected-id ; string? — id of the selected option
         timed-out? ; boolean?
         )
  #:transparent)

;; Notification state — tracks active notification and queue
(struct notification-state
        (active ; (or/c notification? #f)
         queue ; (listof notification?) — pending notifications
         )
  #:transparent)

;; ============================================================
;; #721: ctx-notify
;; ============================================================

;; Create a notification and return it for state integration.
(define (ctx-notify level message #:duration [duration 3])
  (notification level message (current-seconds) duration))

;; Apply a notification to notification-state.
(define (notification-state-add ns notif)
  (if (not (notification-state-active ns))
      (notification-state notif (notification-state-queue ns))
      (notification-state (notification-state-active ns)
                          (append (notification-state-queue ns) (list notif)))))

;; Pop the next notification (when active expires or is dismissed).
(define (notification-state-pop ns)
  (if (null? (notification-state-queue ns))
      (notification-state #f '())
      (notification-state (car (notification-state-queue ns)) (cdr (notification-state-queue ns)))))

;; Check if active notification has expired.
(define (expired-notification? ns)
  (define active (notification-state-active ns))
  (if (not active)
      #f
      (> (- (current-seconds) (notification-timestamp active)) (notification-duration active))))

;; Clear expired notification and pop next from queue.
(define (notification-state-clear-expired ns)
  (if (expired-notification? ns)
      (notification-state-pop ns)
      ns))

;; ============================================================
;; #722: ctx-confirm
;; ============================================================

;; Create a confirm result.
;; In a real implementation, this would show an overlay and block.
;; For now, returns the result directly for state-based integration.
(define (ctx-confirm message #:default [default #f] #:timeout [timeout 30])
  ;; This is the stateless version that returns a confirm-result.
  ;; The TUI integration layer would show an overlay and call this
  ;; when the user responds.
  (confirm-result default #f))

;; ============================================================
;; #723: ctx-select
;; ============================================================

;; Create a select result.
(define (ctx-select options
                    #:prompt [prompt "Select an option:"]
                    #:timeout [timeout 60]
                    #:max-visible [max-visible 20])
  ;; Stateless version — returns first option as default.
  ;; TUI integration would show overlay with arrow keys + Enter.
  (if (null? options)
      #f
      (select-result (select-option-id (car options)) #f)))

;; ============================================================
;; Apply notification to ui-state (status bar integration)
;; ============================================================

(define (apply-notification state notif)
  ;; Set the status message from the notification via ui-surface callback.
  ;; state may be a ui-state-box (callback-based) or plain ui-state (legacy).
  (define msg (format "[~a] ~a" (notification-level notif) (notification-message notif)))
  (cond
    [(box? state)
     (ui-set-status-message! state msg)
     (unbox state)]
    ;; Plain state: return a new state with the status message set
    [else (set-status-message state msg)]))
