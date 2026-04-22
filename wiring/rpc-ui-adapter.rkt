#lang racket/base

;; wiring/rpc-ui-adapter.rkt — FEAT-80: Extension UI over RPC
;;
;; Bridges extension UI channel interactions (confirm/select/input)
;; to RPC notifications and responses.
;;
;; When an extension calls ui-confirm!/ui-select!/ui-input!,
;; the RPC adapter forwards the request as a notification to the
;; RPC client and waits for a response via the request ID.
;;
;; Per-adapter state: pending-requests maps requestId → response-ch
;; so that responses can be routed back to the waiting extension.

(require racket/contract
         "../interfaces/rpc-mode.rkt"
         "../extensions/ui-channel.rkt")

(provide (contract-out [start-rpc-ui-bridge! (-> channel? output-port? void?)]
                       [make-rpc-ui-response-handler (-> channel? (-> hash? hash?))]))

;; ============================================================
;; start-rpc-ui-bridge! : channel? output-port? -> void?
;;
;; Starts a thread that reads ui-request structs from the UI channel
;; and forwards them as RPC notifications.  Each request is recorded
;; in a pending-requests table keyed by requestId so that the
;; response handler can route replies back to the correct channel.
;; ============================================================

;; Per-bridge pending request table: requestId-string → response-ch
;; Each call to start-rpc-ui-bridge! creates its own table.
(define (start-rpc-ui-bridge! ui-ch output-port)
  (define pending-requests (make-hash))
  ;; Store the table where the response handler can find it.
  ;; We attach it as a property on the thread for testability,
  ;; but the response handler receives the same table reference.
  (hash-set! (current-bridge-table) ui-ch pending-requests)
  (void (thread (lambda ()
                  (let loop ()
                    (define req (channel-get ui-ch))
                    (when req
                      (with-handlers ([exn:fail? (lambda (e)
                                                   (displayln (format "rpc-ui-bridge error: ~a"
                                                                      (exn-message e))
                                                              (current-error-port)))])
                        (define request-id (format "ui-~a" (eq-hash-code req)))
                        (define response-ch (ui-request-response-ch req))
                        ;; Record mapping so the response handler can deliver
                        (hash-set! pending-requests request-id response-ch)
                        (define notif
                          (rpc-notification (string->symbol (format "ui.~a" (ui-request-type req)))
                                            (hasheq 'requestId
                                                    request-id
                                                    'type
                                                    (symbol->string (ui-request-type req))
                                                    'prompt
                                                    (ui-request-prompt req))))
                        (displayln (rpc-notification->json notif) output-port)
                        (flush-output output-port))
                      (loop)))))))

;; ============================================================
;; Global bridge table: ui-channel → pending-requests hash
;;
;; Allows the response handler to look up the correct pending
;; table for a given UI channel.
;; ============================================================

;; Q-21: parameterised bridge table for testability and isolation
(define current-bridge-table (make-parameter (make-hash)))

;; ============================================================
;; make-rpc-ui-response-handler : channel? -> (hash? -> hash?)
;;
;; Creates an RPC method handler that receives UI responses and
;; forwards them to the appropriate waiting UI channel.
;;
;; When a response arrives:
;;   1. Look up the requestId in the pending-requests table
;;   2. Channel-put a ui-response to the stored response channel
;;   3. Clean up the table entry
;; ============================================================

(define (make-rpc-ui-response-handler ui-ch)
  (lambda (params)
    (define request-id (hash-ref params 'requestId #f))
    (define response-value (hash-ref params 'value #f))
    (cond
      [(not request-id) (hasheq 'status "error" 'message "requestId required")]
      [else
       (define pending-table (hash-ref (current-bridge-table) ui-ch #f))
       (define resp-ch (and pending-table (hash-ref pending-table request-id #f)))
       (cond
         [(not resp-ch) (hasheq 'status "error" 'message (format "unknown requestId: ~a" request-id))]
         [else
          ;; Deliver the response to the waiting extension
          (channel-put resp-ch (ui-response response-value #f))
          ;; Clean up
          (hash-remove! pending-table request-id)
          (hasheq 'status "ok" 'requestId request-id)])])))
