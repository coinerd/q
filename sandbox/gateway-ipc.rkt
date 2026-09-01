#lang racket/base

;; sandbox/gateway-ipc.rkt — Async IPC manager for gateway ↔ worker communication
;;
;; Spawns and manages a worker subprocess. Reads newline-delimited JSON on
;; stdout, dispatches responses to waiting requestors via async channels.
;; Stderr is drained and accumulated (capped) for diagnostics.
;;
;; v0.99.3 Audit Remediation:
;;   C1: Module-level request-id semaphore (was fresh per call)
;;   C2: async-channel instead of channel (prevents drain thread deadlock)
;;   C3: stdin-write-lock prevents concurrent write interleaving
;;   C4: EOF handler notifies all pending requests on worker exit
;;   L1: stderr accumulation capped at 64KB
;;   L8: working-directory parameter wired to subprocess
;;
;; W3 WP3.5 (v1.00.24, BUG-0056) — bounded fair queue + cancellation:
;;   Concurrent callers no longer write into the worker pipe blindly.
;;   Requests enter a bounded FIFO queue; a dedicated dispatcher thread
;;   hands exactly one request to the worker at a time and forwards the
;;   response before dispatching the next.
;;   Concurrency model (deliberate, documented):
;;     - The worker stays single-threaded; arbitrary dangerous commands are
;;       NEVER parallelized. The queue only makes the serialization explicit.
;;     - Queue depth is bounded (current-gateway-max-queue-depth). When the
;;       bound is exceeded the caller immediately receives a structured
;;       `worker-busy` rejection carrying owner metadata (tool class, owner
;;       request id, elapsed time — never command bodies).
;;     - A client timeout cancels a QUEUED (not-yet-dispatched) request by
;;       removing it before it is ever written to the worker: a cancelled
;;       queued request cannot execute later.
;;     - An already-dispatched request that times out on the client side
;;       keeps its correlation entry so the late worker response is routed
;;       to the dispatcher (not lost) and is then discarded without
;;       attribution — it can never be delivered to a later caller.
;;   Structured outcome classes (details.error-class):
;;     worker-busy / command-timeout / worker-crashed / protocol-error,
;;     so contention, shell command timeouts, and worker crashes are
;;     distinguishable without parsing message strings.
;;
;; Architecture:
;;   ┌──────────┐    stdin ──→     ┌──────────┐
;;   │ Gateway  │    ←── stdout    │ Worker   │
;;   │ (parent) │    ←── stderr    │ (child)  │
;;   └──────────┘                  └──────────┘
;;
;; Each request gets a unique id. The stdout drain thread matches responses
;; by id and puts them on the request's paired async channels (client + dispatcher).

(require racket/contract
         racket/match
         racket/string
         racket/port
         racket/system
         racket/async-channel
         (only-in racket/file make-directory*)
         "ipc-protocol.rkt")

;; Need json for serialization on the wire
(require json)

;; ── Logging ─────────────────────────────────────────────────────

(define-logger gateway-ipc)

;; ── Constants (v0.99.3 L1) ──────────────────────────────────────

(define IPC-STDERR-MAX-CHARS 65536) ; 64KB cap

;; B3: Maximum time allowed for writing a request to the worker's stdin.
;; If the pipe is full (double deadlock: worker blocked on stdout write,
;; gateway blocked on stdin write), the write would block forever and
;; sync/timeout would never fire.  This timeout detects that case.
(define IPC-WRITE-TIMEOUT-MS 10000) ; 10 seconds

;; WP3.5: Extra time the dispatcher waits beyond the entry's client timeout
;; for the worker response before declaring the entry unresponsive.
(define IPC-DISPATCH-GRACE-MS 2000)

;; WP3.5: Default bound on concurrently queued requests. The scheduler's
;; parallel tool pool defaults to 8; 64 leaves generous headroom while the
;; bound still converts unbounded pipe queueing into an explicit busy result.
(define current-gateway-max-queue-depth (make-parameter 64))

;; ── C1: Module-level semaphore for request-id generation ────────

(define request-id-lock (make-semaphore 1))

;; ── Gateway Worker Struct ───────────────────────────────────────

(struct gateway-worker
        (process ; subprocess? or #f
         custodian ; custodian?
         stdin ; output-port? (write to child stdin)
         stdout ; input-port? (read from child stdout)
         stderr ; input-port? (read from child stderr)
         drain-stdout ; thread?
         drain-stderr ; thread?
         response-channel ; async-channel? — responses from drain thread (C2, legacy surface)
         stderr-log ; (boxof string)
         active? ; (boxof boolean)
         started-ms ; exact-nonnegative-integer?
         pending-requests ; (boxof (hash/c string? pending-entry?)) — req-id → channels (C2)
         lock ; semaphore? — serializes access to pending + queue
         stdin-write-lock ; semaphore? — serializes stdin writes (C3)
         request-queue ; (boxof (listof queued-request?)) — FIFO, first = next to dispatch
         in-flight ; (boxof (or/c #f hash?)) — owner metadata for the dispatched request
         work-available) ; semaphore? — posted when the queue gains an entry
  #:transparent)

;; WP3.5: correlation entry for one request. The client channel serves the
;; original caller; the worker channel serves the dispatcher thread.
(struct pending-entry (client-ch worker-ch) #:transparent)

;; WP3.5: one queued verification/tool request awaiting dispatch.
(struct queued-request
        (id ; string? unique request id
         request ; ipc-request?
         client-ch ; async-channel? — response for the caller
         worker-ch ; async-channel? — response for the dispatcher
         enqueued-ms ; real? enqueue timestamp
         timeout-ms ; exact-positive-integer? client timeout
         )
  #:transparent)

;; ── Response Wrapper ────────────────────────────────────────────
;; The response channel carries either a response-packet (normal round trip)
;; or (cons 'worker-error reason) (crash/shutdown marker).

(struct response-packet (id response) #:transparent)

;; ── Internal: pending request management ────────────────────────

(define (register-pending-entry! gw req-id entry)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (hash-set! current req-id entry))))

(define (unregister-pending-request! gw req-id)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (hash-remove! current req-id))))

(define (get-pending-request-entry gw req-id)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (hash-ref current req-id #f))))

;; Public compatibility surface (used by tests): registering a raw client
;; channel allocates a private dispatcher channel automatically.
(define (register-pending-request! gw req-id resp-ch)
  (register-pending-entry! gw req-id (pending-entry resp-ch (make-async-channel))))

;; C2: Use async-channel-put instead of channel-put — never blocks.
;; WP3.5: notifies BOTH halves of each pending entry so a dispatcher waiting
;; on the current entry's worker channel is woken by a crash/shutdown too.
(define (clear-all-pending! gw reason)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define current (unbox (gateway-worker-pending-requests gw)))
                         (for ([(id entry) (in-hash current)])
                           (define marker (response-packet id (cons 'worker-error reason)))
                           (async-channel-put (pending-entry-client-ch entry) marker)
                           (async-channel-put (pending-entry-worker-ch entry) marker))
                         (hash-clear! current))))

;; ── WP3.5: bounded FIFO queue operations (all under gw lock) ────

;; Atomically append an entry unless the queue is at capacity.
;; Returns 'enqueued or 'busy.
(define (enqueue-request! gw entry)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define q (unbox (gateway-worker-request-queue gw)))
                         (if (>= (length q) (current-gateway-max-queue-depth))
                             'busy
                             (begin
                               (set-box! (gateway-worker-request-queue gw)
                                         (append q (list entry)))
                               (semaphore-post (gateway-worker-work-available gw))
                               'enqueued)))))

;; Pop the head entry, or #f when the queue is empty.
(define (dequeue-head! gw)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define q (unbox (gateway-worker-request-queue gw)))
                         (cond
                           [(null? q) #f]
                           [else
                            (set-box! (gateway-worker-request-queue gw) (cdr q))
                            (car q)]))))

;; Remove a queued (not-yet-dispatched) entry by id.
;; Returns 'cancelled when found, 'dispatched when it is no longer queued.
(define (cancel-queued-request! gw req-id)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (define q (unbox (gateway-worker-request-queue gw)))
                         (let loop ([rest q] [before '()])
                           (cond
                             [(null? rest) 'dispatched]
                             [(string=? (queued-request-id (car rest)) req-id)
                              (set-box! (gateway-worker-request-queue gw)
                                        (append (reverse before) (cdr rest)))
                              'cancelled]
                             [else (loop (cdr rest) (cons (car rest) before))])))))

;; Empty the queue after a fatal worker event. Clients were already notified
;; by clear-all-pending!, so this only removes the entries.
(define (drain-queue-entries! gw)
  (let drain ()
    (define entry (dequeue-head! gw))
    (when entry
      (unregister-pending-request! gw (queued-request-id entry))
      (drain))))

(define (set-in-flight! gw req-id tool-name)
  (set-box! (gateway-worker-in-flight gw)
            (hasheq 'request-id req-id
                    'tool tool-name
                    'started-ms (current-inexact-milliseconds))))

(define (clear-in-flight! gw)
  (set-box! (gateway-worker-in-flight gw) #f))

;; Structured contention diagnostics: owner/session/tool metadata and queue
;; depth only — never command bodies or arguments.
(define (gateway-queue-stats gw)
  (call-with-semaphore (gateway-worker-lock gw)
                       (lambda ()
                         (hasheq 'max-queue-depth (current-gateway-max-queue-depth)
                                 'queue-depth (length (unbox (gateway-worker-request-queue gw)))
                                 'in-flight (unbox (gateway-worker-in-flight gw))))))

;; ── Stdout Drain Thread ─────────────────────────────────────────

;; WP3.5: attach the structured command-timeout class to worker-produced
;; timeout responses that lack one, so a shell command timing out inside the
;; worker is distinguishable from a gateway/queue timeout.
(define (normalize-response-classes resp)
  (if (and (eq? (ipc-response-status resp) 'timeout)
           (let ([d (ipc-response-details resp)])
             (or (not (hash? d)) (not (hash-ref d 'error-class #f)))))
      (ipc-response (ipc-response-request-id resp)
                    (ipc-response-status resp)
                    (ipc-response-content resp)
                    (hash-set (if (hash? (ipc-response-details resp))
                                  (ipc-response-details resp)
                                  (hasheq))
                              'error-class 'command-timeout)
                    (ipc-response-error-message resp)
                    (ipc-response-schema-version resp))
      resp))

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
         (cond
           ;; C4: EOF means worker closed stdout or crashed — notify all pending
           [(eof-object? line)
            (log-gateway-ipc-warning "worker stdout EOF — worker may have crashed")
            (clear-all-pending! gw 'worker-exit)]
           [else
            (define trimmed (string-trim line))
            (unless (string=? trimmed "")
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-gateway-ipc-warning
                                            "failed to parse response line: ~a"
                                            (exn-message e)))])
                (define jsexpr (with-input-from-string trimmed read-json/string))
                (define resp (and jsexpr (jsexpr->ipc-response jsexpr)))
                (when (and resp (ipc-response? resp))
                  (define entry (get-pending-request-entry gw (ipc-response-request-id resp)))
                  (when entry
                    ;; C2: async-channel-put never blocks. Deliver to the caller
                    ;; AND wake the dispatcher for the entry it owns.
                    (define packet
                      (response-packet (ipc-response-request-id resp)
                                       (normalize-response-classes resp)))
                    (async-channel-put (pending-entry-client-ch entry) packet)
                    (async-channel-put (pending-entry-worker-ch entry) packet)))))
            (loop)]))))))

;; Read JSON from string using string-port
(define (read-json/string)
  (read-json (current-input-port)))

;; ── Stderr Drain Thread (L1: capped accumulation) ───────────────

(define (start-stderr-drain! gw)
  (thread (lambda ()
            (define port (gateway-worker-stderr gw))
            (with-handlers ([exn:fail? (lambda (e)
                                         (log-gateway-ipc-warning "stderr drain thread crashed: ~a"
                                                                  (exn-message e)))])
              (let loop ()
                (define line (read-line port 'any))
                (cond
                  [(eof-object? line) (void)]
                  [else
                   ;; L1: Cap stderr accumulation to prevent unbounded memory growth
                   (define current-log (unbox (gateway-worker-stderr-log gw)))
                   (define new-log (string-append current-log line "\n"))
                   (set-box! (gateway-worker-stderr-log gw)
                             (if (> (string-length new-log) IPC-STDERR-MAX-CHARS)
                                 (substring new-log (- (string-length new-log) IPC-STDERR-MAX-CHARS))
                                 new-log))
                   (loop)]))))))

;; ── WP3.5: Dispatcher Thread ────────────────────────────────────
;; Owns the single write slot to the worker stdin. FIFO: entry N+1 is written
;; only after entry N's response arrives (or its bound expires), which keeps
;; at most one request outstanding at the worker and maximizes the window in
;; which a queued request can still be cancelled.

(define (deliver-entry! gw entry packet)
  (unregister-pending-request! gw (queued-request-id entry))
  (async-channel-put (queued-request-client-ch entry) packet))

(define (dispatch-entry! gw entry)
  (define req (queued-request-request entry))
  (define req-id (queued-request-id entry))
  (set-in-flight! gw req-id (ipc-request-tool-name req))
  ;; B3: write in a helper thread with a timeout. If the pipe is full
  ;; (double deadlock), the write blocks forever; detect and kill the worker.
  (define json-str (jsexpr->string (ipc-request->jsexpr req)))
  (define out (gateway-worker-stdin gw))
  (define write-ch (make-channel))
  (define write-thread
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (channel-put write-ch (cons 'error e)))])
                ;; C3: wrap write sequence with lock to prevent interleaving
                (call-with-semaphore (gateway-worker-stdin-write-lock gw)
                                     (lambda ()
                                       (display json-str out)
                                       (newline out)
                                       (flush-output out)))
                (channel-put write-ch 'ok)))))
  (define write-result (sync/timeout (/ IPC-WRITE-TIMEOUT-MS 1000.0) write-ch))
  (cond
    ;; B3: write blocked — pipe deadlock. Kill the worker so the drain
    ;; thread's EOF handler notifies all pending requests.
    [(not write-result)
     (log-gateway-ipc-warning "write to worker stdin blocked (pipe deadlock) — killing worker")
     (kill-thread write-thread)
     (deliver-entry! gw entry
                     (make-error-response req-id
                                          "worker pipe write deadlock — worker killed"
                                          #:error-class 'protocol-error))
     (with-handlers ([exn:fail? void])
       (gateway-shutdown! gw))]
    ;; Write raised an exception (broken pipe, etc.)
    [(and (pair? write-result) (eq? (car write-result) 'error))
     (log-gateway-ipc-warning "worker write error: ~a" (exn-message (cdr write-result)))
     (deliver-entry! gw entry
                     (make-error-response req-id
                                          (format "worker write error: ~a"
                                                  (exn-message (cdr write-result)))
                                          #:error-class 'protocol-error))
     (clear-in-flight! gw)]
    ;; Write succeeded — wait for the worker response before dispatching the
    ;; next queued entry (strict FIFO handoff).
    [else
     (define bound-ms (+ (queued-request-timeout-ms entry) IPC-DISPATCH-GRACE-MS))
     (define result (sync/timeout (/ bound-ms 1000.0) (queued-request-worker-ch entry)))
     (cond
       [(response-packet? result)
        (deliver-entry! gw entry result)
        (clear-in-flight! gw)]
       ;; Crash/shutdown marker: clear-all-pending! already notified the client.
       [(and (pair? result) (eq? (car result) 'worker-error))
        (clear-in-flight! gw)
        (log-gateway-ipc-warning "worker error while dispatching ~a: ~a" req-id (cdr result))
        (drain-queue-entries! gw)]
       ;; Worker never answered within the bound — report an explicit timeout,
       ;; drop the correlation so any LATE response cannot be misattributed,
       ;; and let the next entry proceed (bounded, explicit behavior).
       [else
        (log-gateway-ipc-warning "worker unresponsive: no response for ~a within ~a ms"
                                 req-id bound-ms)
        (deliver-entry! gw entry
                        (make-timeout-response
                         req-id
                         "worker did not respond in time; late result will be discarded"))
        (clear-in-flight! gw)])]))

(define (start-dispatcher! gw)
  (thread
   (lambda ()
     (let loop ()
       (semaphore-wait (gateway-worker-work-available gw))
       (let work ()
         (define entry (dequeue-head! gw))
         (when entry
           (with-handlers
               ([exn:fail?
                 (lambda (e)
                   (log-gateway-ipc-warning "dispatcher failed on ~a: ~a"
                                            (queued-request-id entry)
                                            (exn-message e))
                   (deliver-entry! gw entry
                                   (make-error-response (queued-request-id entry)
                                                        (format "dispatcher error: ~a"
                                                                (exn-message e))
                                                        #:error-class 'protocol-error))
                   (clear-in-flight! gw))])
             (dispatch-entry! gw entry))
           (work)))
       (loop)))))

;; ── Worker Lifecycle ────────────────────────────────────────────

;; L8: working-directory parameter now wired to subprocess
(define (start-worker! command [args '()] [working-directory #f])
  ;; Normalize command to a string
  (define cmd-str
    (if (path? command)
        (path->string command)
        command))
  ;; Create a custodian for the worker subprocess + drain threads
  (define worker-custodian (make-custodian))
  (parameterize ([current-custodian worker-custodian])
    ;; L8: Wire working-directory to subprocess spawn
    (define-values (proc sub-out sub-in sub-err)
      (if working-directory
          (parameterize ([current-directory working-directory])
            (apply subprocess (append (list #f #f #f (find-executable-path cmd-str)) args)))
          (apply subprocess (append (list #f #f #f (find-executable-path cmd-str)) args))))
    ;; sub-out = read child stdout, sub-in = write child stdin, sub-err = read child stderr
    (define gw
      (gateway-worker proc
                      worker-custodian
                      sub-in ; our stdin (write to child)
                      sub-out ; our stdout (read from child)
                      sub-err ; our stderr (read from child)
                      #f ; drain threads started below
                      #f
                      (make-async-channel) ; C2: async-channel instead of channel
                      (box "")
                      (box #t)
                      (current-inexact-milliseconds)
                      (box (make-hash))
                      (make-semaphore 1)
                      (make-semaphore 1) ; C3: stdin-write-lock
                      (box '()) ; WP3.5: FIFO request queue
                      (box #f) ; WP3.5: in-flight owner metadata
                      (make-semaphore))) ; WP3.5: work-available signal
    ;; Start drain threads under the custodian
    (define stdout-thread
      (parameterize ([current-custodian worker-custodian])
        (start-stdout-drain! gw)))
    (define stderr-thread
      (parameterize ([current-custodian worker-custodian])
        (start-stderr-drain! gw)))
    ;; Start the dispatch thread (WP3.5) under the custodian so shutdown reaps it
    (define dispatcher-thread
      (parameterize ([current-custodian worker-custodian])
        (start-dispatcher! gw)))
    ;; Store thread references via struct-copy
    (define gw-with-threads
      (struct-copy gateway-worker gw
                   [drain-stdout stdout-thread]
                   [drain-stderr stderr-thread]))
    (log-gateway-ipc-info "worker started: pid=~a" (subprocess-pid proc))
    gw-with-threads))

;; ── Request/Response ────────────────────────────────────────────

(define request-counter (box 0))

;; C1: Use module-level semaphore (was creating fresh one per call)
(define (generate-request-id)
  (call-with-semaphore request-id-lock
                       (lambda ()
                         (set-box! request-counter (add1 (unbox request-counter)))
                         (format "req-~a-~a"
                                 (exact->inexact (round (current-inexact-milliseconds)))
                                 (unbox request-counter)))))

;; LF2-new (v0.99.5): IPC-DEFAULT-TIMEOUT-MS (120000) matches the default
;; of current-execution-plane-timeout-ms. Callers should always pass
;; timeout-ms explicitly (typically from the execution-plane parameter).
;;
;; WP3.5: the caller's request enters the bounded FIFO queue and waits on
;; its private client channel; the dispatcher performs the actual write.
;; A timeout while still queued cancels the request BEFORE it is ever sent
;; to the worker, so a cancelled queued request cannot execute later.
(define (send-request! gw req [timeout-ms IPC-DEFAULT-TIMEOUT-MS])
  ;; If worker is not alive, return error immediately
  (unless (gateway-alive? gw)
    (raise (exn:fail:gateway "worker not alive" (current-continuation-marks))))
  ;; Check size
  (when (ipc-request-too-large? req)
    (raise (exn:fail:gateway "request too large" (current-continuation-marks))))
  (define req-id (ipc-request-request-id req))
  (define client-ch (make-async-channel))
  (define worker-ch (make-async-channel))
  ;; Atomically register the correlation entry and append to the bounded
  ;; queue (or reject as busy under contention).
  (define enqueue-result
    (call-with-semaphore (gateway-worker-lock gw)
                         (lambda ()
                           (define q (unbox (gateway-worker-request-queue gw)))
                           (if (>= (length q) (current-gateway-max-queue-depth))
                               'busy
                               (begin
                                 ;; Inline registration: we already hold gw-lock, and
                                 ;; register-pending-entry! would re-acquire it (deadlock).
                                 (let ([current (unbox (gateway-worker-pending-requests gw))])
                                   (hash-set! current req-id (pending-entry client-ch worker-ch)))
                                 (set-box! (gateway-worker-request-queue gw)
                                           (append q
                                                   (list (queued-request req-id
                                                                         req
                                                                         client-ch
                                                                         worker-ch
                                                                         (current-inexact-milliseconds)
                                                                         timeout-ms))))
                                 (semaphore-post (gateway-worker-work-available gw))
                                 'enqueued)))))
  (cond
    ;; WP3.5: explicit structured busy rejection — the queue bound was hit.
    ;; Owner metadata identifies WHO holds the worker (never command bodies).
    [(eq? enqueue-result 'busy)
     (define owner (unbox (gateway-worker-in-flight gw)))
     (define owner-tool (or (and (hash? owner) (hash-ref owner 'tool #f)) "unknown"))
     (define owner-req (or (and (hash? owner) (hash-ref owner 'request-id #f)) "unknown"))
     (define elapsed
       (if (hash? owner)
           (max 0 (inexact->exact (floor (- (current-inexact-milliseconds)
                                            (hash-ref owner 'started-ms)))))
           0))
     (make-busy-response req-id
                         #:requested-tool (ipc-request-tool-name req)
                         #:owner-tool owner-tool
                         #:owner-request-id owner-req
                         #:busy-elapsed-ms elapsed)]
    [else
     (define result (sync/timeout (/ timeout-ms 1000.0) client-ch))
     (cond
       ;; Client timeout
       [(not result)
        (define cancel-result (cancel-queued-request! gw req-id))
        (cond
          ;; Still queued → removed before dispatch: it can never execute now.
          [(eq? cancel-result 'cancelled)
           (unregister-pending-request! gw req-id)
           (make-timeout-response req-id
                                  #:details (hasheq 'cancelled-before-exec #t)
                                  "request timed out while queued — cancelled before execution (never sent to worker)")]
          ;; Already dispatched → keep correlation so the late response is
          ;; routed to the dispatcher and discarded, never misattributed.
          [else
           (make-timeout-response req-id
                                  #:details (hasheq 'cancelled-before-exec #f)
                                  "request timed out (already executing; late result will be discarded)")])]
       [(response-packet? result)
        (define resp (response-packet-response result))
        (if (and (pair? resp) (eq? (car resp) 'worker-error))
            (make-error-response req-id (format "worker error: ~a" (cdr resp))
                                 #:error-class 'worker-crashed)
            resp)]
       [else (make-error-response req-id "unexpected response format"
                                  #:error-class 'protocol-error)])]))

;; ── Status / Lifecycle Queries ──────────────────────────────────

(define (gateway-alive? gw)
  (and (gateway-worker-process gw)
       (eq? (subprocess-status (gateway-worker-process gw)) 'running)
       (unbox (gateway-worker-active? gw))))

(define (gateway-shutdown! gw)
  (set-box! (gateway-worker-active? gw) #f)
  (clear-all-pending! gw 'shutdown)
  (clear-in-flight! gw)
  (drain-queue-entries! gw)
  (define proc (gateway-worker-process gw))
  (when proc
    (with-handlers ([exn:fail? void])
      (close-input-port (gateway-worker-stdout gw))
      (close-output-port (gateway-worker-stdin gw))
      (close-input-port (gateway-worker-stderr gw))
      (subprocess-kill proc #t)))
  ;; Kill custodian to clean up threads (drains + dispatcher)
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
         gateway-worker-lock
         gateway-worker-stdin-write-lock
         gateway-worker-request-queue
         gateway-worker-in-flight
         gateway-worker-work-available)

(provide response-packet
         response-packet?
         response-packet-id
         response-packet-response)

(provide exn:fail:gateway
         exn:fail:gateway?)

(provide current-gateway-max-queue-depth
         gateway-queue-stats)

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
          [generate-request-id (-> string?)]
          [register-pending-request! (-> gateway-worker? string? async-channel? void?)]))
