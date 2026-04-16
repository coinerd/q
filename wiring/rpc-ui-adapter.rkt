#lang racket/base

;; wiring/rpc-ui-adapter.rkt — FEAT-80: Extension UI over RPC
;;
;; Bridges extension UI channel interactions (confirm/select/input)
;; to RPC notifications and responses.
;;
;; When an extension calls ui-confirm!/ui-select!/ui-input!,
;; the RPC adapter forwards the request as a notification to the
;; RPC client and waits for a response via the request ID.

(require racket/contract
         "../interfaces/rpc-mode.rkt"
         "../extensions/ui-channel.rkt")

(provide
 (contract-out
  [start-rpc-ui-bridge! (-> channel? output-port? void?)]
  [make-rpc-ui-response-handler (-> channel? (-> hash? hash?))]))

;; ============================================================
;; start-rpc-ui-bridge! : channel? output-port? -> void?
;;
;; Starts a thread that reads ui-request structs from the UI channel
;; and forwards them as RPC notifications.
;; ============================================================

(define (start-rpc-ui-bridge! ui-ch output-port)
  (void
   (thread
   (lambda ()
     (let loop ()
       (define req (channel-get ui-ch))
       (when req
         (define notif
           (rpc-notification
            (string->symbol (format "ui.~a" (ui-request-type req)))
            (hasheq 'requestId (format "ui-~a" (eq-hash-code req))
                    'type (symbol->string (ui-request-type req)))))
         (displayln (rpc-notification->json notif) output-port)
         (flush-output output-port)
         (loop)))))))

;; ============================================================
;; make-rpc-ui-response-handler : channel? -> (hash? -> hash?)
;;
;; Creates an RPC method handler that receives UI responses and
;; forwards them to the appropriate waiting UI channel.
;; ============================================================

;; Simple response table: request-id -> (cons response-ch response)
(define response-table (make-hash))

(define (make-rpc-ui-response-handler ui-ch)
  (lambda (params)
    (define request-id (hash-ref params 'requestId #f))
    (define response-value (hash-ref params 'value #f))
    (cond
      [(not request-id)
       (hasheq 'status "error" 'message "requestId required")]
      [else
       (hash-set! response-table request-id response-value)
       (hasheq 'status "ok" 'requestId request-id)])))

;; Register a pending UI request for response matching
(define (register-pending-request! request-id response-ch)
  (hash-set! response-table request-id (cons response-ch #f)))
