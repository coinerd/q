#lang racket/base

;; sandbox/ipc-protocol.rkt — Wire protocol for gateway ↔ worker IPC
;;
;; Defines request/response structs and JSON serialization for
;; the execution-plane JSON-RPC-over-stdio channel.
;; All values are jsexpr-serializable (strings, numbers, hashes, lists).

(require racket/contract
         racket/match
         (only-in racket/port with-output-to-string))

;; ── Protocol Constants ──────────────────────────────────────────

(define IPC-SCHEMA-VERSION 1)
(define IPC-MAX-REQUEST-BYTES 1048576) ; 1 MB
(define IPC-MAX-RESPONSE-BYTES 10485760) ; 10 MB
(define IPC-DEFAULT-TIMEOUT-MS 120000) ; 2 minutes

;; ── Struct Definitions ──────────────────────────────────────────

;; BUG-0028 core fix (v1.00.19 W1): `trusted-working-dir` carries the
;; COORDINATOR's working directory (from the execution context) separately
;; from the model-visible `working-dir`. The worker extends its request-scoped
;; allowed roots ONLY from the trusted field, so a model-supplied
;; 'working-directory argument keeps its plain cwd semantics (bash tool
;; feature) but can never authorize new roots.
;; Backward compatibility: the constructor below accepts the historical 7-arg
;; positional form (trusted-working-dir defaults #f); serialization omits the
;; key when #f and the parser defaults it to #f, so old↔new mixes work.
(struct ipc-request
        (request-id ; string?
         tool-name ; string?
         arguments ; hash?
         timeout-ms ; exact-positive-integer?
         working-dir ; (or/c path-string? #f)
         capability ; symbol?
         schema-version ; exact-nonnegative-integer?
         trusted-working-dir) ; (or/c path-string? #f)
  #:transparent
  #:constructor-name construct-ipc-request)

;; Backward-compatible constructor: historical call sites use 7 positional
;; args; the coordinator trust channel is an optional 8th argument. Bound to
;; the name `ipc-request` so every pre-existing positional call site keeps
;; working; `make-ipc-request` is an alias for new code.
(define (make-ipc-request request-id
                          tool-name
                          arguments
                          timeout-ms
                          working-dir
                          capability
                          schema-version
                          [trusted-working-dir #f])
  (construct-ipc-request request-id
                         tool-name
                         arguments
                         timeout-ms
                         working-dir
                         capability
                         schema-version
                         trusted-working-dir))

(struct ipc-response
        (request-id ; string?
         status ; (or/c 'ok 'error 'timeout 'crashed)
         content ; any/c
         details ; hash?
         error-message ; (or/c string? #f)
         schema-version) ; exact-nonnegative-integer?
  #:transparent)

;; ── Serialization ───────────────────────────────────────────────

(define (ipc-request->jsexpr req)
  (define base
    (hasheq 'request-id
            (ipc-request-request-id req)
            'tool-name
            (ipc-request-tool-name req)
            'arguments
            (ipc-request-arguments req)
            'timeout-ms
            (ipc-request-timeout-ms req)
            'working-dir
            (ipc-request-working-dir req)
            'capability
            (symbol->string (ipc-request-capability req))
            'schema-version
            (ipc-request-schema-version req)))
  ;; Omit the trust channel when absent so old workers see an unchanged wire
  ;; format; only add it when the coordinator supplied one.
  (define twd (ipc-request-trusted-working-dir req))
  (if twd
      (hash-set base 'trusted-working-dir twd)
      base))

(define (jsexpr->ipc-request data)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (hash? data)
         (let ([request-id (hash-ref data 'request-id #f)]
               [tool-name (hash-ref data 'tool-name #f)]
               [arguments (hash-ref data 'arguments (hasheq))]
               [timeout-ms (hash-ref data 'timeout-ms IPC-DEFAULT-TIMEOUT-MS)]
               [working-dir (hash-ref data 'working-dir #f)]
               [trusted-working-dir (hash-ref data 'trusted-working-dir #f)]
               [capability (hash-ref data 'capability "any")]
               [schema-version (hash-ref data 'schema-version IPC-SCHEMA-VERSION)])
           (and (string? request-id)
                (string? tool-name)
                (hash? arguments)
                (positive-timeout? timeout-ms)
                (make-ipc-request request-id
                                  tool-name
                                  arguments
                                  (if (exact-positive-integer? timeout-ms)
                                      timeout-ms
                                      (inexact->exact (floor timeout-ms)))
                                  working-dir
                                  (if (symbol? capability)
                                      capability
                                      (string->symbol capability))
                                  (if (exact-nonnegative-integer? schema-version)
                                      schema-version
                                      IPC-SCHEMA-VERSION)))))))

(define (positive-timeout? v)
  (and (real? v) (positive? v)))

(define (ipc-response->jsexpr resp)
  (hasheq 'request-id
          (ipc-response-request-id resp)
          'status
          (symbol->string (ipc-response-status resp))
          'content
          (ipc-response-content resp)
          'details
          (ipc-response-details resp)
          'error-message
          (ipc-response-error-message resp)
          'schema-version
          (ipc-response-schema-version resp)))

(define (valid-status? s)
  (and (or (symbol? s) (string? s))
       (let ([sym (if (symbol? s)
                      s
                      (string->symbol s))])
         (or (eq? sym 'ok) (eq? sym 'error) (eq? sym 'timeout) (eq? sym 'crashed)))))

(define (jsexpr->ipc-response data)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (hash? data)
         (let ([request-id (hash-ref data 'request-id #f)]
               [status (hash-ref data 'status #f)]
               [content (hash-ref data 'content (void))]
               [details (hash-ref data 'details (hasheq))]
               [error-message (hash-ref data 'error-message #f)]
               [schema-version (hash-ref data 'schema-version IPC-SCHEMA-VERSION)])
           (and (string? request-id)
                (valid-status? status)
                (ipc-response request-id
                              (if (symbol? status)
                                  status
                                  (string->symbol status))
                              content
                              (if (hash? details)
                                  details
                                  (hasheq))
                              (if (or (string? error-message) (not error-message))
                                  error-message
                                  (format "~a" error-message))
                              (if (exact-nonnegative-integer? schema-version)
                                  schema-version
                                  IPC-SCHEMA-VERSION)))))))

;; ── Convenience Constructors ────────────────────────────────────

(define (make-error-response request-id [message "unknown error"]
                             #:details [extra-details #f]
                             #:error-class [error-class #f])
  ;; WP3.5 (BUG-0056): structured outcome classes. error-class is attached in
  ;; details so every execution-plane failure is distinguishable without
  ;; parsing message strings. Detail values are jsexpr-safe (symbols allowed).
  (define base (if (and (hash? extra-details) (not (hash-empty? extra-details)))
                   extra-details
                   (hasheq)))
  (define details (if error-class (hash-set base 'error-class error-class) base))
  (ipc-response (or request-id "") 'error (void) details message IPC-SCHEMA-VERSION))

(define (make-timeout-response request-id
                               [message "request timed out"]
                               #:details [extra-details #f])
  (define base (hasheq 'error-class 'command-timeout))
  (define details (if (and (hash? extra-details) (not (hash-empty? extra-details)))
                      (for/fold ([acc base])
                                ([k (in-hash-keys extra-details)])
                        (hash-set acc k (hash-ref extra-details k)))
                      base))
  (ipc-response (or request-id "") 'timeout (void) details message IPC-SCHEMA-VERSION))

;; WP3.5 (BUG-0056): structured worker-busy rejection. Carries owner metadata
;; (owner tool/request id and busy elapsed time — never command bodies) so a
;; caller can distinguish contention from a broken shell.
(define (make-busy-response request-id
                            #:requested-tool requested-tool
                            #:owner-tool owner-tool
                            #:owner-request-id owner-request-id
                            #:busy-elapsed-ms busy-elapsed-ms
                            [message #f])
  (define default-msg
    (format "worker busy: request held by another caller (tool '~a', request ~a, ~a ms elapsed); it will not be preempted — retry after it completes"
            owner-tool owner-request-id busy-elapsed-ms))
  (ipc-response (or request-id "") 'error (void)
                (hasheq 'error-class 'worker-busy
                        'requested-tool requested-tool
                        'owner-tool owner-tool
                        'owner-request-id owner-request-id
                        'busy-elapsed-ms busy-elapsed-ms)
                (or message default-msg)
                IPC-SCHEMA-VERSION))

;; ── Size Validation ─────────────────────────────────────────────

(define (ipc-request-too-large? req)
  (> (bytes-length (string->bytes/utf-8 (with-output-to-string
                                         (lambda ()
                                           (write-json-for-size (ipc-request->jsexpr req))))))
     IPC-MAX-REQUEST-BYTES))

;; Minimal JSON size estimation without requiring racket/json at module level
(define (write-json-for-size v)
  (cond
    [(hash? v)
     (display "{")
     (for ([pair (in-list (hash->list v))]
           [i (in-naturals)])
       (when (> i 0)
         (display ","))
       (display "\"")
       (display (car pair))
       (display "\":")
       (write-json-for-size (cdr pair)))
     (display "}")]
    [(list? v)
     (display "[")
     (for ([item (in-list v)]
           [i (in-naturals)])
       (when (> i 0)
         (display ","))
       (write-json-for-size item))
     (display "]")]
    [(string? v)
     (display "\"")
     (display v)
     (display "\"")]
    [(number? v) (display v)]
    [(boolean? v) (display (if v "true" "false"))]
    [(void? v) (display "null")]
    [(symbol? v)
     (display "\"")
     (display (symbol->string v))
     (display "\"")]
    [else
     (display "\"")
     (display v)
     (display "\"")]))

;; ── Provides ────────────────────────────────────────────────────

(provide IPC-SCHEMA-VERSION
         IPC-MAX-REQUEST-BYTES
         IPC-MAX-RESPONSE-BYTES
         IPC-DEFAULT-TIMEOUT-MS)

(provide make-ipc-request
         ipc-request
         ipc-request?
         ipc-request-request-id
         ipc-request-tool-name
         ipc-request-arguments
         ipc-request-timeout-ms
         ipc-request-working-dir
         ipc-request-capability
         ipc-request-schema-version
         ipc-request-trusted-working-dir)

(provide ipc-response
         ipc-response?
         ipc-response-request-id
         ipc-response-status
         ipc-response-content
         ipc-response-details
         ipc-response-error-message
         ipc-response-schema-version)

(provide (contract-out [ipc-request->jsexpr (-> ipc-request? hash?)]
                       [jsexpr->ipc-request (-> any/c (or/c ipc-request? #f))]
                       [ipc-response->jsexpr (-> ipc-response? hash?)]
                       [jsexpr->ipc-response (-> any/c (or/c ipc-response? #f))]
                       [make-error-response
                        (->* ((or/c string? #f))
                             (string?
                              #:details (or/c hash? #f)
                              #:error-class (or/c symbol? #f))
                             ipc-response?)]
                       [make-timeout-response
                        (->* ((or/c string? #f)) (string? #:details (or/c hash? #f)) ipc-response?)]
                       [make-busy-response
                        (->* ((or/c string? #f)
                              #:requested-tool string?
                              #:owner-tool string?
                              #:owner-request-id string?
                              #:busy-elapsed-ms real?)
                             (string?)
                             ipc-response?)]
                       [ipc-request-too-large? (-> ipc-request? boolean?)]))
