#lang racket/base

;; sandbox/worker-dispatch.rkt — Reusable tool dispatch for workers
;; STABILITY: evolving
;;
;; W2 (v0.99.12): Extracted from worker-main.rkt so that both the stdio
;; worker (worker-main.rkt) and the TLS executor server (executor-server.rkt)
;; share the same dispatch logic.
;;
;; process-ipc-request: (-> ipc-request? ipc-response?)
;;   Takes a parsed ipc-request, dispatches to the worker tool registry,
;;   and returns an ipc-response with the request-id stamped in.

(require racket/match
         racket/path
         (only-in json string->jsexpr jsexpr->string)
         "ipc-protocol.rkt"
         "worker-tools.rkt")

;; ── IPC Request Processing ──────────────────────────────────────

;; Dispatch an ipc-request to the worker tool registry.
;; The working-dir field is honored (parameterized current-directory).
;; The request-id from the request is stamped into the response.
;;
;; BUG-0028 core fix (v1.00.19 W1): the request working-dir parameterizes
;; current-directory. R91/BUG-0091 (frozen contract §2.2.1–§2.2.2): the
;; request-scoped allowed roots extend if and only if the decoded
;; trusted-working-dir is a valid absolute directory string. Model
;; `working-dir`/`working-directory` NEVER extends roots — it keeps its plain
;; cwd semantics. Roots are request-dynamic-extent only: the parameterize
;; below restores the captured roots after success AND after any exception.
;; §2.2.2: when twd is present but wd is absent, roots extend and the worker
;; does NOT chdir to twd — trust is never a chdir channel.
;; §2.2.4: an invalid trust value (possible only via a directly constructed
;; struct, since the decoder refuses invalid trust) fails closed: the whole
;; request is refused, never dispatched with silently dropped trust.
(define (process-ipc-request request)
  (define req-id (ipc-request-request-id request))
  (define tool-name (ipc-request-tool-name request))
  (define arguments (ipc-request-arguments request))
  (define response
    (let ([wd (ipc-request-working-dir request)]
          [twd (ipc-request-trusted-working-dir request)])
      (cond
        [(and twd (not (valid-trusted-working-dir? twd)))
         (make-error-response req-id "refused: invalid trusted working directory")]
        [else
         (define roots+
           (if twd
               (cons (simplify-path (path->complete-path twd)) (current-allowed-roots))
               (current-allowed-roots)))
         (cond
           [wd
            (parameterize ([current-directory wd]
                           [current-allowed-roots roots+])
              (dispatch-tool tool-name arguments))]
           ;; Trusted-only request: extend roots only; never chdir to twd.
           [twd
            (parameterize ([current-allowed-roots roots+])
              (dispatch-tool tool-name arguments))]
           [else (dispatch-tool tool-name arguments)])])))
  ;; Stamp the request-id into the response
  (ipc-response req-id
                (ipc-response-status response)
                (ipc-response-content response)
                (ipc-response-details response)
                (ipc-response-error-message response)
                (ipc-response-schema-version response)))

;; ── JSON Line Processing ────────────────────────────────────────

;; Parse a JSON line → ipc-request → dispatch → ipc-response
;; Returns ipc-response.
;; On malformed JSON or parse failure, returns an error response.
(define (process-request-line line)
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-response #f (format "worker error: ~a" (exn-message e))))])
    (define req-data
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (string->jsexpr line)))
    (define request (and req-data (jsexpr->ipc-request req-data)))
    (cond
      [(not request) (make-error-response #f "malformed request")]
      [else (process-ipc-request request)])))

;; ── Response Serialization ──────────────────────────────────────

;; Serialize an ipc-response to a JSON string, enforcing IPC-MAX-RESPONSE-BYTES.
;; Returns the JSON string (possibly with a truncated error response).
(define (serialize-response resp)
  ;; Normalize void content to #f for JSON serialization
  (define clean-response
    (if (void? (ipc-response-content resp))
        (ipc-response (ipc-response-request-id resp)
                      (ipc-response-status resp)
                      #f
                      (ipc-response-details resp)
                      (ipc-response-error-message resp)
                      (ipc-response-schema-version resp))
        resp))
  ;; Enforce IPC-MAX-RESPONSE-BYTES
  (define json-str (jsexpr->string (ipc-response->jsexpr clean-response)))
  (if (> (string-length json-str) IPC-MAX-RESPONSE-BYTES)
      (let ([truncated (ipc-response (ipc-response-request-id clean-response)
                                     'error
                                     (format "response too large (~a bytes, max ~a)"
                                             (string-length json-str)
                                             IPC-MAX-RESPONSE-BYTES)
                                     (hasheq 'original-bytes (string-length json-str))
                                     "response exceeded size limit"
                                     (ipc-response-schema-version clean-response))])
        (jsexpr->string (ipc-response->jsexpr truncated)))
      json-str))

;; ── Provides ────────────────────────────────────────────────────

(provide process-ipc-request
         process-request-line
         serialize-response)
