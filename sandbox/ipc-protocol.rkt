#lang racket/base

;; sandbox/ipc-protocol.rkt — Wire protocol for gateway ↔ worker IPC
;;
;; Defines request/response structs and JSON serialization for
;; the execution-plane JSON-RPC-over-stdio channel.
;; All values are jsexpr-serializable (strings, numbers, hashes, lists).

(require racket/contract
         racket/match
         racket/path
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
;; W2: `trusted-scratch-root` carries the single trusted session scratch
;; allocation (tools/scratch.rkt) on the same trusted producer channel. The
;; worker extends its request-scoped allowed roots and its bash redirection
;; policy from this field only; absence (#f) leaves behavior unchanged.
(struct ipc-request
        (request-id ; string?
         tool-name ; string?
         arguments ; hash?
         timeout-ms ; exact-positive-integer?
         working-dir ; (or/c path-string? #f)
         capability ; symbol?
         schema-version ; exact-nonnegative-integer?
         trusted-working-dir ; (or/c path-string? #f)
         trusted-scratch-root) ; (or/c path-string? #f)
  #:transparent
  #:constructor-name construct-ipc-request)

;; R91/BUG-0091 (frozen contract §2.2.4): JSON cannot encode path objects, so
;; trusted producers normalize path values to JSON strings at construction.
;; This is producer-side normalization only; it never widens trust.
(define (normalize-wire-path v)
  (if (path? v)
      (path->string v)
      v))

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
                          [trusted-working-dir #f]
                          [trusted-scratch-root #f])
  (construct-ipc-request request-id
                         tool-name
                         arguments
                         timeout-ms
                         (normalize-wire-path working-dir)
                         capability
                         schema-version
                         (normalize-wire-path trusted-working-dir)
                         (normalize-wire-path trusted-scratch-root)))

;; ── Trusted-working-dir validation (R91/BUG-0091) ───────────────

;; Frozen contract §2.2.4: the decoder validates twd when present — string,
;; absolute, no `up` component, no NUL, no symlink spelling. Invalid trust
;; makes the WHOLE request malformed (parser #f → error response); it is
;; never coerced to #f-and-continue and never widens roots.
(define (valid-trusted-working-dir? twd)
  (and (string? twd)
       (not (regexp-match? #rx"\u0000" twd))
       (absolute-path? twd)
       ;; `..` may not appear as a path component (explode-path spells it 'up).
       (not (member 'up (explode-path (string->path twd))))
       (trusted-working-dir-spelling-safe? twd)))

;; W2: the trusted scratch root shares the structural checks of the wire
;; (string, absolute, no NUL, no `up` component) but NOT the working-dir
;; symlink-spelling rule. The scratch producer (tools/scratch.rkt) hands over
;; the allocation below ITS OWN base spelling, and that spelling can legally
;; reach the directory through a symlinked temporary ancestor (a TMPDIR alias
;; such as /tmp -> /private/tmp, a test-scoped link). Applying the stricter
;; working-dir rule here voided those legitimate requests at decode time, so
;; the worker never saw the sanctioned root it was given.
;;
;; The trust boundary is enforced where it can be: the FINAL component of the
;; scratch root may not be a symbolic link (a link there would redirect every
;; write into the allocation), and the worker additionally demands an EXISTING
;; real, owner-only, canonically-contained allocation
;; (tools/scratch.rkt existing-trusted-session-scratch-root?), so an alias
;; spelling that does not resolve to the worker's own base is still refused.
;; Existence is deliberately NOT a decoder condition: an allocation may be
;; named here before it is created.
(define (valid-trusted-scratch-root? tsr)
  (and (string? tsr)
       (not (regexp-match? #rx"\u0000" tsr))
       (absolute-path? tsr)
       ;; `..` may not appear as a path component.
       (not (member 'up (explode-path (string->path tsr))))
       (trusted-scratch-root-spelling-safe? tsr)))

;; #t iff the final component of TSR-STR is not a symbolic link. `link-exists?`
;; examines the named entry itself (no final-component following), so symlinked
;; ANCESTORS — the platform aliases this check deliberately accepts — do not
;; affect the verdict, while a link at the allocation or a dangling link there
;; fails closed. Resolution errors fail closed too.
(define (trusted-scratch-root-spelling-safe? tsr-str)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (not (link-exists? (simplify-path (string->path tsr-str) #f)))))

;; No symlink spelling: walk the longest existing prefix and require every
;; existing component to resolve to its own lexical spelling. A symlinked
;; ancestor with a non-existent tail would pass a final-component-only
;; resolve-path check, so every existing component is resolved in turn.
;; Broken links and resolution failures fail closed (#f).
(define (trusted-working-dir-spelling-safe? twd-str)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (let loop ([remaining (explode-path (string->path twd-str))]
               [resolved-prefix #f])
      (cond
        [(null? remaining) #t]
        [else
         (define candidate
           (if resolved-prefix
               (build-path resolved-prefix (car remaining))
               (car remaining)))
         (cond
           [(or (file-exists? candidate) (directory-exists? candidate) (link-exists? candidate))
            (define resolved (resolve-path candidate))
            (define complete-target
              (if (complete-path? resolved)
                  resolved
                  (path->complete-path resolved (or (path-only candidate) (current-directory)))))
            (define simplified (simplify-path complete-target #f))
            (and (equal? simplified (simplify-path candidate #f)) (loop (cdr remaining) simplified))]
           ;; Non-existent tail: nothing further can be a link spelling.
           [else #t])]))))

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
  ;; Omit either trust channel when absent so old workers see an unchanged
  ;; wire format; only add a channel when the coordinator supplied one.
  (define twd (ipc-request-trusted-working-dir req))
  (define tsr (ipc-request-trusted-scratch-root req))
  (define with-twd
    (if twd
        (hash-set base 'trusted-working-dir twd)
        base))
  (if tsr
      (hash-set with-twd 'trusted-scratch-root tsr)
      with-twd))

(define (jsexpr->ipc-request data)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (hash? data)
         (let ([request-id (hash-ref data 'request-id #f)]
               [tool-name (hash-ref data 'tool-name #f)]
               [arguments (hash-ref data 'arguments (hasheq))]
               [timeout-ms (hash-ref data 'timeout-ms IPC-DEFAULT-TIMEOUT-MS)]
               [working-dir (hash-ref data 'working-dir #f)]
               [trusted-working-dir (hash-ref data 'trusted-working-dir #f)]
               [trusted-scratch-root (hash-ref data 'trusted-scratch-root #f)]
               [capability (hash-ref data 'capability "any")]
               [schema-version (hash-ref data 'schema-version IPC-SCHEMA-VERSION)])
           (and (string? request-id)
                (string? tool-name)
                (hash? arguments)
                (positive-timeout? timeout-ms)
                ;; R91/BUG-0091 defect fix: the parsed `trusted-working-dir`
                ;; is now validated and passed into the 8th constructor
                ;; argument instead of being silently discarded. Legacy
                ;; absence (#f / field omitted) keeps byte-identical
                ;; behavior (§2.2.5); invalid trust fails closed and
                ;; refuses the whole request (§2.2.4).
                (or (not trusted-working-dir) (valid-trusted-working-dir? trusted-working-dir))
                (or (not trusted-scratch-root) (valid-trusted-scratch-root? trusted-scratch-root))
                (make-ipc-request
                 request-id
                 tool-name
                 arguments
                 (if (exact-positive-integer? timeout-ms)
                     timeout-ms
                     (inexact->exact (floor timeout-ms)))
                 working-dir
                 (if (symbol? capability)
                     capability
                     (string->symbol capability))
                 (if (exact-nonnegative-integer? schema-version) schema-version IPC-SCHEMA-VERSION)
                 trusted-working-dir
                 trusted-scratch-root))))))

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

(define (make-error-response request-id
                             [message "unknown error"]
                             #:details [extra-details #f]
                             #:error-class [error-class #f])
  ;; WP3.5 (BUG-0056): structured outcome classes. error-class is attached in
  ;; details so every execution-plane failure is distinguishable without
  ;; parsing message strings. Detail values are jsexpr-safe (symbols allowed
  ;; for in-process consumers; wire-serialized responses should use a string
  ;; class because racket/json rejects symbols).
  (define base
    (if (and (hash? extra-details) (not (hash-empty? extra-details)))
        extra-details
        (hasheq)))
  (define details
    (if error-class
        (hash-set base 'error-class error-class)
        base))
  (ipc-response (or request-id "") 'error (void) details message IPC-SCHEMA-VERSION))

(define (make-timeout-response request-id [message "request timed out"] #:details [extra-details #f])
  (define base (hasheq 'error-class 'command-timeout))
  (define details
    (if (and (hash? extra-details) (not (hash-empty? extra-details)))
        (for/fold ([acc base]) ([k (in-hash-keys extra-details)])
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
    (format
     "worker busy: request held by another caller (tool '~a', request ~a, ~a ms elapsed); it will not be preempted — retry after it completes"
     owner-tool
     owner-request-id
     busy-elapsed-ms))
  (ipc-response (or request-id "")
                'error
                (void)
                (hasheq 'error-class
                        'worker-busy
                        'requested-tool
                        requested-tool
                        'owner-tool
                        owner-tool
                        'owner-request-id
                        owner-request-id
                        'busy-elapsed-ms
                        busy-elapsed-ms)
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
         ipc-request-trusted-working-dir
         ipc-request-trusted-scratch-root
         valid-trusted-working-dir?
         valid-trusted-scratch-root?)

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
                             (string? #:details (or/c hash? #f)
                                      #:error-class (or/c symbol? string? #f))
                             ipc-response?)]
                       [make-timeout-response
                        (->* ((or/c string? #f)) (string? #:details (or/c hash? #f)) ipc-response?)]
                       [make-busy-response
                        (->* ((or/c string? #f) #:requested-tool string?
                                                #:owner-tool string?
                                                #:owner-request-id string?
                                                #:busy-elapsed-ms real?)
                             (string?)
                             ipc-response?)]
                       [ipc-request-too-large? (-> ipc-request? boolean?)]))
