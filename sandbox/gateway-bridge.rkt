#lang racket/base

;; sandbox/gateway-bridge.rkt — MAS envelope → IPC request bridge
;;
;; Translates mas-envelope payloads (tool calls) into ipc-requests,
;; sends them to the worker process, and translates ipc-responses back
;; into the hash format expected by agent-role-handle-envelope.
;;
;; Feature-gated: when current-execution-plane-enabled is #f (default),
;; no worker is started and execute-via-worker raises an error.
;; The tool-gateway checks the flag before calling this module.

(require racket/contract
         racket/match
         (only-in "../util/message/mas-envelope.rkt"
                  mas-envelope?
                  mas-envelope-payload
                  mas-envelope-capability
                  mas-envelope-trace-id
                  mas-envelope-deadline)
         "ipc-protocol.rkt"
         "gateway-ipc.rkt")

;; ── Feature Flags ───────────────────────────────────────────────

;; Master switch for execution plane externalization.
;; Default: #f — all tool execution is in-process (backward compat).
(define current-execution-plane-enabled (make-parameter #f))

;; Worker command — the racket binary to use.
;; Default: "racket" (resolved via find-executable-path at spawn time).
(define current-worker-command (make-parameter "racket"))

;; Worker script arguments — the worker main entry point.
(define current-worker-args (make-parameter '("-tm" "sandbox/worker-main.rkt")))

;; Timeout for execution plane requests (ms).
;; Default: 120000 (2 minutes).
(define current-execution-plane-timeout-ms (make-parameter 120000))

;; ── Worker Lifecycle (memoized) ─────────────────────────────────

;; Box holding the current worker (or #f).
(define worker-box (box #f))
(define worker-lock (make-semaphore 1))

(define (ensure-worker!)
  (call-with-semaphore worker-lock
                       (lambda ()
                         (define current (unbox worker-box))
                         (cond
                           [(and current (gateway-alive? current)) current]
                           [else
                            ;; Start a fresh worker
                            (when current
                              ;; Old worker is dead — clean up
                              (with-handlers ([exn:fail? void])
                                (gateway-shutdown! current)))
                            (define cmd (current-worker-command))
                            (define args (current-worker-args))
                            (define gw (start-worker! cmd args #f))
                            (set-box! worker-box gw)
                            gw]))))

(define (shutdown-worker!)
  (call-with-semaphore worker-lock
                       (lambda ()
                         (define current (unbox worker-box))
                         (when current
                           (with-handlers ([exn:fail? void])
                             (gateway-shutdown! current)))
                         (set-box! worker-box #f))))

(define (current-worker)
  (unbox worker-box))

;; ── Payload Extraction ──────────────────────────────────────────

;; Extract tool-name, arguments, and timeout-ms from envelope payload.
;; Returns (values tool-name arguments timeout-ms) or #f on malformed payload.
(define (extract-tool-call envelope)
  (define payload (mas-envelope-payload envelope))
  (cond
    [(not (hash? payload)) (values #f #f #f)]
    [else
     (define tool-name (hash-ref payload 'tool-name #f))
     (define arguments (hash-ref payload 'arguments (hasheq)))
     (define timeout-ms (hash-ref payload 'timeout-ms #f)) ; LF1: fall through to parameter
     (values tool-name arguments timeout-ms)]))

;; ── Envelope → IPC Translation ──────────────────────────────────

;; Build an ipc-request from a mas-envelope.
(define (envelope->ipc-request envelope)
  (define-values (tool-name arguments timeout-ms) (extract-tool-call envelope))
  (cond
    [(not tool-name) (error 'envelope->ipc-request "envelope payload missing 'tool-name")]
    [(not (hash? arguments))
     (error 'envelope->ipc-request "envelope payload 'arguments must be a hash")]
    [else
     (define req-id (generate-request-id))
     (define capability (mas-envelope-capability envelope))
     (define timeout
       (if (and (number? timeout-ms) (positive? timeout-ms))
           (inexact->exact (floor timeout-ms))
           (current-execution-plane-timeout-ms))) ; L7: use parameter
     (ipc-request req-id
                  (if (symbol? tool-name)
                      (symbol->string tool-name)
                      tool-name)
                  arguments
                  timeout
                  #f ; working-dir — let worker use its CWD
                  capability
                  IPC-SCHEMA-VERSION)]))

;; ── IPC Response → Result Hash ──────────────────────────────────

(define (ipc-response->result-hash resp envelope)
  (define status (ipc-response-status resp))
  (define trace-id (mas-envelope-trace-id envelope))
  (case status
    [(ok)
     (hasheq 'status
             'ok
             'role
             'tool-gateway
             'result
             (ipc-response-content resp)
             'details
             (ipc-response-details resp)
             'trace-id
             trace-id)]
    [(timeout)
     (hasheq 'status
             'error
             'role
             'tool-gateway
             'error
             'timeout
             'error-message
             (or (ipc-response-error-message resp) "request timed out")
             'trace-id
             trace-id)]
    [(crashed)
     (hasheq 'status
             'error
             'role
             'tool-gateway
             'error
             'crashed
             'error-message
             (or (ipc-response-error-message resp) "worker crashed")
             'trace-id
             trace-id)]
    [else
     ;; 'error or unknown
     (hasheq 'status
             'error
             'role
             'tool-gateway
             'error-message
             (or (ipc-response-error-message resp) "unknown error")
             'details
             (ipc-response-details resp)
             'trace-id
             trace-id)]))

;; ── Main Entry Point ────────────────────────────────────────────

;; Execute a tool call via the worker process.
;; envelope must be a mas-envelope? with a payload containing:
;;   - 'tool-name: string
;;   - 'arguments: hash
;;   - 'timeout-ms: (optional) positive integer
;;
;; Returns a result hash suitable for agent-role-handle-envelope.
(define (execute-via-worker envelope)
  (if (not (mas-envelope? envelope))
      (hasheq 'status
              'error
              'role
              'tool-gateway
              'error-message
              "execute-via-worker: expected mas-envelope?"
              'trace-id
              #f)
      (with-handlers ([exn:fail? (lambda (e)
                                   (hasheq 'status
                                           'error
                                           'role
                                           'tool-gateway
                                           'error-message
                                           (format "execution plane error: ~a" (exn-message e))
                                           'trace-id
                                           (and (mas-envelope? envelope)
                                                (mas-envelope-trace-id envelope))))])
        ;; Build the request
        (define req (envelope->ipc-request envelope))
        ;; Ensure worker is running
        (define gw (ensure-worker!))
        ;; Send the request and get response
        (define resp (send-request! gw req (ipc-request-timeout-ms req)))
        ;; Translate response to result hash
        (ipc-response->result-hash resp envelope))))

;; ── H4: Scheduler Entry Point ───────────────────────────────────

;; Execute a tool via the worker, returning ipc-response directly.
;; This is the scheduler's single entry point — it bypasses the envelope layer.
;; The scheduler then translates the ipc-response to tool-result.
(define (execute-tool-via-worker tool-name args required-capability)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (make-error-response #f (format "execution plane error: ~a" (exn-message e))))])
    (define req-id (generate-request-id))
    (define timeout-ms
      (let ([t (hash-ref args 'timeout #f)])
        (if (and (real? t) (positive? t))
            (inexact->exact (* t 1000))
            (current-execution-plane-timeout-ms))))
    (define req
      (ipc-request req-id tool-name args timeout-ms #f required-capability IPC-SCHEMA-VERSION))
    (define gw (ensure-worker!))
    (send-request! gw req timeout-ms)))

;; ── Provides ────────────────────────────────────────────────────

(provide current-execution-plane-enabled
         current-worker-command
         current-worker-args
         current-execution-plane-timeout-ms
         current-worker
         shutdown-worker!
         envelope->ipc-request
         ipc-response->result-hash
         extract-tool-call
         ;; H1/M3: Re-export IPC items so scheduler has a single import
         IPC-SCHEMA-VERSION
         IPC-DEFAULT-TIMEOUT-MS
         ipc-request
         ipc-request?
         ipc-request-request-id
         ipc-request-tool-name
         ipc-request-arguments
         ipc-request-timeout-ms
         ipc-request-capability
         ipc-response
         ipc-response?
         ipc-response-status
         ipc-response-content
         ipc-response-details
         ipc-response-error-message
         send-request!
         generate-request-id
         gateway-worker?
         gateway-alive?
         gateway-worker-process)

(provide (contract-out [ensure-worker! (-> gateway-worker?)]
                       [execute-via-worker (-> any/c hash?)]
                       [execute-tool-via-worker (-> string? hash? symbol? ipc-response?)]))
