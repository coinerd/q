#lang racket/base

;; sandbox/gateway-ipc.rkt — Async IPC manager for gateway ↔ worker communication
;;
;; Spawns and manages a worker subprocess. Reads newline-delimited JSON on
;; stdout, dispatches responses to waiting requestors via sync channels.
;; Stderr is drained and accumulated for diagnostics.
;;
;; Architecture:
;;   ┌──────────┐    stdin ──→     ┌──────────┐
;;   │ Gateway  │    ←── stdout    │ Worker   │
;;   │ (parent) │    ←── stderr    │ (child)  │
;;   └──────────┘                  └──────────┘
;;
;; Each request gets a unique id. Response drain thread matches by id
;; and puts the response on a per-request channel. send-request! blocks
;; on that channel with a timeout via sync/timeout.

(require racket/contract
         racket/match
         racket/string
         racket/port
         racket/system
         (only-in racket/file make-directory*)
         "ipc-protocol.rkt")

;; Need json for serialization on the wire
(require json)

;; ── Logging ─────────────────────────────────────────────────────

(define-logger gateway-ipc)

;; ── Gateway Worker Struct ───────────────────────────────────────

(struct gateway-worker
        (process ; subprocess? or #f
         custodian ; custodian?
         stdin ; input-port? (our write end)
         stdout ; output-port? (our read end)
         stderr ; output-port? (our read end)
         drain-stdout ; thread?
         drain-stderr ; thread?
         response-channel ; channel? — responses from drain thread
         stderr-log ; (boxof string)
         active? ; (boxof boolean)
         started-ms ; exact-nonnegative-integer?
         pending-requests ; (boxof (hash/c string? channel?)) — req-id → channel
         lock) ; semaphore? — serializes access to pending
  #:transparent)

;; ── Response Wrapper ────────────────────────────────────────────
;; The response channel carries either an ipc-response or an error symbol.

(struct response-packet (id response) #:transparent)

;; ── Internal: pending request management ────────────────────────

(define (register-pending-request! gw req-id resp-ch)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (hash-set! current req-id resp-ch))))

(define (unregister-pending-request! gw req-id)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (hash-remove! current req-id))))

(define (get-pending-request-channel gw req-id)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (hash-ref current req-id #f))))

(define (clear-all-pending! gw reason)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (for ([(id ch) (in-hash current)])
                           (channel-put ch (response-packet id (cons 'worker-error reason))))
                         (hash-clear! current))))

;; ── Stdout Drain Thread ─────────────────────────────────────────

(define (start-stdout-drain! gw)
  (thread
   (lambda ()
     (define port (gateway-worker-stdout gw))
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-gateway-ipc-warning "stdout drain thread crashed: ~a"
                                                           (exn-message e))
                                  (clear-all-pending! gw 'drain-crash))])
       (let loop ()
         (define line (read-line port 'any))
         (unless (eof-object? line)
           (define trimmed (string-trim line))
           (unless (string=? trimmed "")
             (with-handlers ([exn:fail? (lambda (e)
                                          (log-gateway-ipc-warning "failed to parse response line: ~a"
                                                                   (exn-message e)))])
               (define jsexpr (with-input-from-string trimmed read-json/string))
               (define resp (and jsexpr (jsexpr->ipc-response jsexpr)))
               (when (and resp (ipc-response? resp))
                 (define ch (get-pending-request-channel gw (ipc-response-request-id resp)))
                 (when ch
                   (channel-put ch (response-packet (ipc-response-request-id resp) resp))))))
           (loop)))))))

;; Read JSON from string using string-port
(define (read-json/string)
  (read-json (current-input-port)))

;; ── Stderr Drain Thread ─────────────────────────────────────────

(define (start-stderr-drain! gw)
  (thread (lambda ()
            (define port (gateway-worker-stderr gw))
            (with-handlers ([exn:fail? (lambda (e)
                                         (log-gateway-ipc-warning "stderr drain thread crashed: ~a"
                                                                  (exn-message e)))])
              (let loop ()
                (define line (read-line port 'any))
                (unless (eof-object? line)
                  (define current-log (unbox (gateway-worker-stderr-log gw)))
                  (set-box! (gateway-worker-stderr-log gw) (string-append current-log line "\n"))
                  (loop)))))))

;; ── Worker Lifecycle ────────────────────────────────────────────

(define (start-worker! command [args '()] [working-directory #f])
  ;; Normalize command to a string
  (define cmd-str
    (if (path? command)
        (path->string command)
        command))
  ;; Create a custodian for the worker subprocess + drain threads
  (define worker-custodian (make-custodian))
  (parameterize ([current-custodian worker-custodian])
    (define-values (proc sub-out sub-in sub-err)
      (apply subprocess
             (append (list #f ; don't redirect — use ports
                           #f
                           #f
                           (find-executable-path cmd-str))
                     args)))
    ;; proc is the subprocess, sub-out is our stdout read, sub-in is our stdin write,
    ;; sub-err is our stderr read
    ;; Wait — subprocess returns: (values subprocess stdout stdin stderr)
    ;; where stdout = child's stdout, stdin = child's stdin, stderr = child's stderr
    ;; So: sub-out = read child stdout, sub-in = write child stdin, sub-err = read child stderr
    (define gw
      (gateway-worker proc
                      worker-custodian
                      sub-in ; our stdin (write to child)
                      sub-out ; our stdout (read from child)
                      sub-err ; our stderr (read from child)
                      #f ; drain threads started below
                      #f
                      (make-channel)
                      (box "")
                      (box #t)
                      (current-inexact-milliseconds)
                      (box (make-hash))
                      (make-semaphore 1)))
    ;; Start drain threads under the custodian
    (define stdout-thread
      (parameterize ([current-custodian worker-custodian])
        (start-stdout-drain! gw)))
    (define stderr-thread
      (parameterize ([current-custodian worker-custodian])
        (start-stderr-drain! gw)))
    ;; Store thread references via struct-copy
    (define gw-with-threads
      (struct-copy gateway-worker gw [drain-stdout stdout-thread] [drain-stderr stderr-thread]))
    (log-gateway-ipc-info "worker started: pid=~a" (subprocess-pid proc))
    gw-with-threads))

;; ── Request/Response ────────────────────────────────────────────

(define request-counter (box 0))

(define (generate-request-id)
  (call-with-semaphore (make-semaphore 1) ; not ideal but safe
                       (lambda ()
                         (set-box! request-counter (add1 (unbox request-counter)))
                         (format "req-~a-~a"
                                 (exact->inexact (round (current-inexact-milliseconds)))
                                 (unbox request-counter)))))

(define (send-request! gw req [timeout-ms IPC-DEFAULT-TIMEOUT-MS])
  ;; If worker is not alive, return error immediately
  (unless (gateway-alive? gw)
    (raise (exn:fail:gateway "worker not alive" (current-continuation-marks))))
  ;; Check size
  (when (ipc-request-too-large? req)
    (raise (exn:fail:gateway "request too large" (current-continuation-marks))))
  ;; Register response channel
  (define req-id (ipc-request-request-id req))
  (define resp-ch (make-channel))
  (register-pending-request! gw req-id resp-ch)
  ;; Serialize and write
  (define jsexpr (ipc-request->jsexpr req))
  (define json-str (jsexpr->string jsexpr))
  (define out (gateway-worker-stdin gw))
  ;; Write with newline delimiter
  (display json-str out)
  (newline out)
  (flush-output out)
  ;; Wait for response with timeout
  (define result (sync/timeout (/ timeout-ms 1000.0) resp-ch))
  ;; Unregister
  (unregister-pending-request! gw req-id)
  (cond
    ;; Timeout
    [(not result) (make-timeout-response req-id)]
    [(response-packet? result)
     (define resp (response-packet-response result))
     (if (and (pair? resp) (eq? (car resp) 'worker-error))
         (make-error-response req-id (format "worker error: ~a" (cdr resp)))
         resp)]
    [else (make-error-response req-id "unexpected response format")]))

;; ── Status / Lifecycle Queries ──────────────────────────────────

(define (gateway-alive? gw)
  (and (gateway-worker-process gw)
       (eq? (subprocess-status (gateway-worker-process gw)) 'running)
       (unbox (gateway-worker-active? gw))))

(define (gateway-shutdown! gw)
  (set-box! (gateway-worker-active? gw) #f)
  (clear-all-pending! gw 'shutdown)
  (define proc (gateway-worker-process gw))
  (when proc
    (with-handlers ([exn:fail? void])
      (close-input-port (gateway-worker-stdout gw))
      (close-output-port (gateway-worker-stdin gw))
      (close-input-port (gateway-worker-stderr gw))
      (subprocess-kill proc #t)))
  ;; Kill custodian to clean up threads
  (custodian-shutdown-all (gateway-worker-custodian gw))
  (log-gateway-ipc-info "worker shut down"))

(define (gateway-restart! gw command [args '()])
  (gateway-shutdown! gw)
  (start-worker! command args))

(define (gateway-stderr gw)
  (unbox (gateway-worker-stderr-log gw)))

(define (gateway-pid gw)
  (define proc (gateway-worker-process gw))
  (and proc (subprocess-pid proc)))

;; ── Error type ──────────────────────────────────────────────────

(struct exn:fail:gateway exn:fail () #:transparent)

;; ── Provides ────────────────────────────────────────────────────

(provide gateway-worker
         gateway-worker?
         gateway-worker-process
         gateway-worker-custodian
         gateway-worker-stdin
         gateway-worker-stdout
         gateway-worker-stderr
         gateway-worker-drain-stdout
         gateway-worker-drain-stderr
         gateway-worker-response-channel
         gateway-worker-stderr-log
         gateway-worker-active?
         gateway-worker-started-ms
         gateway-worker-pending-requests
         gateway-worker-lock)

(provide response-packet
         response-packet?
         response-packet-id
         response-packet-response)

(provide exn:fail:gateway
         exn:fail:gateway?)

(provide (contract-out
          [start-worker!
           (->* ((or/c string? path?)) ((listof string?) (or/c path-string? #f)) gateway-worker?)]
          [send-request! (->* (gateway-worker? ipc-request?) (exact-positive-integer?) ipc-response?)]
          [gateway-alive? (-> gateway-worker? boolean?)]
          [gateway-shutdown! (-> gateway-worker? void?)]
          [gateway-restart!
           (->* (gateway-worker? (or/c string? path?)) ((listof string?)) gateway-worker?)]
          [gateway-stderr (-> gateway-worker? string?)]
          [gateway-pid (-> gateway-worker? (or/c exact-nonnegative-integer? #f))]
          [generate-request-id (-> string?)]))
