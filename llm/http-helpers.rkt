#lang racket/base

;; llm/http-helpers.rkt — Shared HTTP utilities for LLM providers
;;
;; Common helpers for:
;;   - HTTP request execution (make-provider-http-request)
;;   - Status checking with provider-specific error messages
;;   - Parsing HTTP status lines and raising structured errors
;;
;; Used by anthropic.rkt, gemini.rkt, openai-compatible.rkt, azure-openai.rkt.

(require racket/contract
         racket/math
         racket/match
         racket/string
         racket/port
         racket/date
         racket/hash
         json
         net/url
         net/http-client
         "provider-errors.rkt"
         "stream.rkt")

(provide (contract-out
          [extract-status-code (-> (or/c bytes? string?) exact-nonnegative-integer?)]
          [http-error? (-> exact-nonnegative-integer? boolean?)]
          [raise-http-error! (->* (string?) ((or/c exact-nonnegative-integer? #f)) any)]
          [parse-provider-url
           (-> string? (values string? string? exact-nonnegative-integer? boolean?))]
          [make-provider-http-request
           (->* (string? (listof string?) bytes?)
                (#:timeout (or/c positive? #f)
                           ;; v1.00.13 W4 (#9478): dedicated connect/TTFB bound
                           #:connect-timeout (or/c positive? #f)
                           ;; v1.00.13 W3 (#9473): duration contracts widened
                           ;; from exact-positive-integer? to positive? to
                           ;; match the downstream wrappers (call-with-request-
                           ;; timeout / read-response-body/timeout both accept
                           ;; positive reals); sub-second durations are required
                           ;; by the cleanup-contract tests.
                           #:read-timeout (or/c positive? #f)
                           #:status-checker (or/c procedure? #f))
                jsexpr?)]
          [check-provider-status! (-> string? (or/c bytes? string?) (or/c bytes? string?) void?)]
          [extract-error-message (-> jsexpr? (or/c string? #f))]
          [translate-stop-reason (-> (or/c symbol? #f) any/c symbol?)]
          ;; v1.00.13 W3 (#9473): structured failure context (RL-5/RL-7)
          [parse-retry-after-header
           (->* ((or/c string? #f))
                (#:now-ms (-> exact-nonnegative-integer?))
                (or/c exact-nonnegative-integer? #f))]
          [build-network-failure-context
           (->* (#:kind symbol?)
                (#:status (or/c exact-nonnegative-integer? #f) #:headers (listof string?))
                hash?)])
         ;; v1.00.13 W3 (#9473): injectable HTTP boundary (RL-6) — the
         ;; cleanup-contract tests substitute a local socket pair through it.
         current-provider-http-sendrecv
         ;; v1.00.13 W4 (#9478): connect/TTFB-bounded sendrecv (RL-4)
         (contract-out
          [provider-sendrecv/ttfb-bounded
           (->* (positive? procedure?) (#:cleanup procedure?) (values any/c any/c any/c))]))

;; ============================================================
;; Contracts
;; ============================================================

(define status-line/c (or/c bytes? string?))

(define response-body/c (or/c bytes? string?))

;; ============================================================
;; Existing helpers
;; ============================================================

;; Parse "HTTP/N.N NNN ..." → integer status code.
;; Returns 0 if the pattern cannot be matched.
(define (extract-status-code status-line)
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (define m (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" status-str))
  (if m
      (string->number (cadr m))
      0))

;; Returns #t when status-code indicates a client or server error (>= 400).
(define (http-error? status-code)
  (>= status-code 400))

;; Raise provider-error with category inferred from HTTP status code.
;; Falls back to exn:fail if no status code is provided.
(define (raise-http-error! message [status-code #f])
  (define category (classify-http-status status-code))
  (if category
      (raise (provider-error message (current-continuation-marks) (hash) category status-code))
      (raise (exn:fail message (current-continuation-marks)))))

;; ============================================================
;; Consolidated HTTP request (QUAL-02)
;; ============================================================

;; parse-provider-url : string? -> (values string? string? exact-nonnegative-integer? boolean?)
;;
;; Shared URL parser for LLM provider HTTP requests. Returns:
;;   - host (string)
;;   - path-str (string, starts with /)
;;   - port (exact-nonnegative-integer, 443 for https / 80 for http if not in URL)
;;   - ssl? (boolean, true for https)
;;
;; Eliminates duplicated string->url/url-host/url-path/url-port/url-scheme logic
;; across anthropic.rkt, gemini.rkt, openai-compatible.rkt (v0.99.58 W1-1 / P1-S).
(define (parse-provider-url url-str)
  (define uri (string->url url-str))
  (define host (url-host uri))
  (define path-str
    (string-append "/"
                   (string-join (for/list ([p (in-list (url-path uri))])
                                  (path/param-path p))
                                "/")))
  (define ssl? (and (url-scheme uri) (not (equal? (url-scheme uri) "http"))))
  (define port (or (url-port uri) (if ssl? 443 80)))
  (values host path-str port ssl?))

;; ============================================================
;; Structured failure context (v1.00.13 W3, RL-5/RL-7)
;; ============================================================

;; HTTP-date month table for the Retry-After HTTP-date form (RFC 1123).
(define retry-after-months
  '(("Jan" . 1) ("Feb" . 2)
                ("Mar" . 3)
                ("Apr" . 4)
                ("May" . 5)
                ("Jun" . 6)
                ("Jul" . 7)
                ("Aug" . 8)
                ("Sep" . 9)
                ("Oct" . 10)
                ("Nov" . 11)
                ("Dec" . 12)))

;; Days since 1970-01-01 for a civil (proleptic Gregorian) date —
;; timezone-free, so HTTP-date (always GMT) parses deterministically on
;; every host locale (Hinnant's days_from_civil).
(define (days-from-civil y m d)
  (define y*
    (if (<= m 2)
        (sub1 y)
        y))
  (define era (quotient y* 400))
  (define yoe (- y* (* era 400)))
  (define mp
    (if (<= m 2)
        (+ m 9)
        (- m 3)))
  (define doy (quotient (+ (* 153 mp) 2) 5))
  (define doe (+ (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100)) doy d -1))
  (+ (* era 146097) doe -719468))

(define (http-date->ms v)
  (define m
    (regexp-match
     #px"^[A-Za-z]{3}, *([0-9]{1,2}) +([A-Za-z]{3}) +([0-9]{4}) +([0-9]{2}):([0-9]{2}):([0-9]{2}) +GMT$"
     (string-trim v)))
  (and m
       (let* ([day (string->number (list-ref m 1))]
              [mon (cdr (assoc (list-ref m 2) retry-after-months string-ci=?))]
              [year (string->number (list-ref m 3))]
              [hour (string->number (list-ref m 4))]
              [minute (string->number (list-ref m 5))]
              [sec (string->number (list-ref m 6))])
         (and day
              mon
              year
              hour
              minute
              sec
              (* 1000
                 (+ (* (days-from-civil year mon day) 86400) (* hour 3600) (* minute 60) sec))))))

;; parse-retry-after-header : (or/c string? #f) [#:now-ms thunk] ->
;;   (or/c exact-nonnegative-integer? #f)
;;
;; Parse a Retry-After header value into milliseconds. Supports:
;;   - delta-seconds (integer or float, e.g. "17", "2.5")
;;   - HTTP-date (e.g. "Wed, 21 Oct 2015 07:28:00 GMT") relative to a
;;     deterministic clock seam (#:now-ms thunk, default wall clock); past
;;     dates clamp to 0, never negative.
;; Returns #f for missing/unparseable values.
(define (parse-retry-after-header header-val #:now-ms [now-ms current-inexact-milliseconds])
  (and header-val
       (string? header-val)
       (let ([trimmed (string-trim header-val)])
         (and (> (string-length trimmed) 0)
              ;; delta-seconds form
              (or (with-handlers ([exn:fail? (lambda (_) #f)])
                    (define secs (string->number trimmed))
                    (and (real? secs)
                         (positive-integer-millis? (* secs 1000))
                         (inexact->exact (max 0 (exact-floor (* secs 1000))))))
                  ;; HTTP-date form (clock seam keeps tests deterministic)
                  (cond
                    [(http-date->ms trimmed)
                     =>
                     (lambda (target-ms)
                       (inexact->exact (max 0 (exact-floor (- target-ms (now-ms))))))]
                    [else #f]))))))

(define (positive-integer-millis? v)
  (and (real? v) (>= v 0)))

;; Header names whose values may be retained in a structured failure context.
;; Everything else (Authorization, Set-Cookie, provider tokens, ...) is
;; dropped: failure metadata must never leak credential material into logs.
(define retry-relevant-header-rx #rx"(?i:^retry-after:|^x-ratelimit)")

(define (retry-relevant-headers headers)
  (filter (lambda (h) (regexp-match retry-relevant-header-rx (string-trim h))) headers))

(define (header-ref headers name-rx)
  (for/or ([h (in-list headers)])
    (define m (regexp-match #rx"^([^:]+):[ \t]*(.*)$" h))
    (and m (regexp-match name-rx (string-trim (cadr m))) (caddr m))))

;; build-network-failure-context : #:kind symbol? [#:status N] [#:headers list]
;;   -> hash?
;;
;; One machine-readable failure context (PLAN-v1.00.13 §3.5). Human error
;; text is rendered elsewhere; retry policy consumes these fields only
;; (RL-7: no string-as-protocol). Response headers are REDACTED to the
;; retry-relevant subset before retention.
(define (build-network-failure-context #:kind kind #:status [status #f] #:headers [headers '()])
  (define retry-after-raw (header-ref headers #rx"(?i:^retry-after$)"))
  (hasheq 'kind
          kind
          'http-status
          status
          'response-headers
          (retry-relevant-headers headers)
          'retry-after-ms
          (and retry-after-raw (parse-retry-after-header retry-after-raw))))

;; ============================================================
;; Connect/TTFB-bounded sendrecv (v1.00.13 W4, RL-4)
;; ============================================================

;; Run the (already injected or real) HTTP send phase under the policy
;; connect/TTFB bound: connect + TLS + status + headers. An established
;; connection that produces no response-head progress within the bound fires
;; a structured exn:fail:network:timeout:stream with phase 'connect/ttfb —
;; it can never silently consume the full request budget (NP-4/RL-4).
;; #:cleanup runs BEFORE the worker thread is killed (W4: cleanup before
;; retry).
(define (provider-sendrecv/ttfb-bounded ttfb-secs
                                        send-proc
                                        #:cleanup [cleanup-thunk (lambda () (void))])
  (define ch (make-channel))
  (define th
    (thread (lambda ()
              (with-handlers ([exn:break? (lambda (e) (void))]
                              [exn:fail? (lambda (e) (channel-put ch (cons 'exn e)))])
                ;; send-proc returns multiple values (http-sendrecv: status,
                ;; headers, port) — package them for the channel.
                (channel-put ch (cons 'val (call-with-values send-proc vector)))))))
  (define result (sync/timeout ttfb-secs ch))
  (match result
    [#f
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "llm/http-helpers: connect cleanup error: ~a"
                                                       (exn-message e))))])
       (cleanup-thunk))
     (kill-thread th)
     (raise (exn:fail:network:timeout:stream
             (format "connect/TTFB timeout (~a seconds) awaiting response head" ttfb-secs)
             (current-continuation-marks)
             #f ; received heartbeats? — no stream yet
             #f ; received any data? — no bytes of the response head
             'connect/ttfb
             0))]
    [_
     (define tag (car result))
     (define payload (cdr result))
     (match tag
       ['exn (raise payload)]
       [_ (vector->values payload)])]))

;; ============================================================
;; Injectable HTTP boundary (v1.00.13 W3, RL-6)
;; ============================================================

(define (header-line->string h)
  (if (bytes? h)
      (bytes->string/utf-8 h #\?)
      h))

;; Default: the real http-sendrecv. The cleanup-contract tests substitute a
;; local socket pair so port ownership is directly observable.
(define current-provider-http-sendrecv
  (make-parameter (lambda (host path #:port p #:ssl? ssl? #:method m #:headers hs #:data d)
                    (http-sendrecv host path #:port p #:ssl? ssl? #:method m #:headers hs #:data d))))

;; make-provider-http-request : string? (listof string?) bytes?
;;   [#:timeout exact-positive-integer?]
;;   [#:status-checker (bytes? bytes? -> void?)]
;;   -> jsexpr?
;;
;; Shared HTTP POST helper for LLM providers. This boundary OWNS every
;; response port it opens (v1.00.13 W3, RL-6): normal completion, status
;; failure, read timeout, request timeout, and cancellation each close the
;; port exactly once; GC remains only a safety net. HTTP status + response
;; headers survive long enough to populate a structured failure context
;; (RL-5), which is attached to provider errors raised by the status checker.
(define (make-provider-http-request url-str
                                    headers
                                    body-bytes
                                    #:timeout [timeout-secs #f]
                                    #:connect-timeout [connect-timeout-secs #f]
                                    #:read-timeout [read-timeout-secs #f]
                                    #:status-checker [status-checker #f])
  ;; v1.00.13 W2 (#9466): body-read budget observation for the cross-adapter
  ;; conformance harness (RL-10/AC-3). Zero overhead when unset.
  (define observer (current-request-mechanism-observer))
  (when observer
    (observer
     (hasheq 'kind 'body-read 'read-timeout (or read-timeout-secs http-read-timeout-default))))
  (define-values (host path-str port ssl?) (parse-provider-url url-str))
  (define effective-timeout (or timeout-secs (current-http-request-timeout)))
  (define sendrecv (current-provider-http-sendrecv))
  ;; Single-owner, idempotent close for the response port this request opens.
  (define resp-port-box (box #f))
  (define close-lock (make-semaphore 1))
  (define closed? (box #f))
  (define (close-response-once!)
    (call-with-semaphore
     close-lock
     (lambda ()
       (define p (unbox resp-port-box))
       (when (and p (not (unbox closed?)))
         (set-box! closed? #t)
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning
                                       (format "llm/http-helpers: response close error: ~a"
                                               (exn-message e))))])
           (unless (port-closed? p)
             (close-input-port p)))))))
  (dynamic-wind
   void
   (lambda ()
     (call-with-request-timeout
      (lambda ()
        (define-values (status-line response-headers response-port)
          ;; v1.00.13 W4 (#9478, RL-4): connect+TLS+status+headers runs under
          ;; the dedicated policy bound when the caller provides it — an
          ;; established-but-silent peer cannot consume the request budget.
          ((if connect-timeout-secs
               (lambda (proc)
                 (provider-sendrecv/ttfb-bounded connect-timeout-secs
                                                 proc
                                                 #:cleanup close-response-once!))
               (lambda (proc) (proc)))
           (lambda ()
             (sendrecv host
                       path-str
                       #:port port
                       #:ssl? ssl?
                       #:method #"POST"
                       #:headers headers
                       #:data body-bytes))))
        (set-box! resp-port-box response-port)
        ;; v1.00.05 W1 (#9393): honor a per-model read timeout (policy
        ;; body-read budget since v1.00.13) for the body read; the outer
        ;; #:timeout remains the wall-clock request cap.
        (define response-body
          (with-handlers ([exn:fail? (lambda (e)
                                       (close-response-once!)
                                       (raise e))])
            (if read-timeout-secs
                (read-response-body/timeout response-port #:timeout read-timeout-secs)
                (read-response-body/timeout response-port))))
        (define failure-ctx
          (build-network-failure-context #:kind 'http-status
                                         #:status (extract-status-code status-line)
                                         #:headers (map header-line->string response-headers)))
        (with-handlers ([provider-error?
                         (lambda (e)
                           (close-response-once!)
                           (raise (provider-error (exn-message e)
                                                  (exn-continuation-marks e)
                                                  (hash-union (provider-error-context e) failure-ctx)
                                                  (provider-error-category e)
                                                  (provider-error-status-code e))))]
                        [exn:fail? (lambda (e)
                                     (close-response-once!)
                                     (raise e))])
          (when status-checker
            (status-checker status-line response-body))
          (bytes->jsexpr response-body)))
      #:timeout effective-timeout
      ;; Request-timeout cleanup fires BEFORE the worker thread is killed.
      #:cleanup close-response-once!))
   ;; Post-thunk: covers success (idempotent), cancellation/break during the
   ;; body read, and any other unwind path.
   (lambda () (close-response-once!))))

;; ============================================================
;; Consolidated status checker (QUAL-02)
;; ============================================================

;; check-provider-status! : string? bytes? bytes? -> void?
;;
;; Generic HTTP status checker with provider name for error messages.
;; Handles common status codes (301-303 redirects, 401, 403, 429, 5xx)
;; with provider-specific messages.
;;
;; Example:
;;   (check-provider-status! "Anthropic" status-line response-body)
(define (check-provider-status! provider-name status-line response-body)
  (define status-code (extract-status-code status-line))
  (define response-bytes
    (if (bytes? response-body)
        response-body
        (string->bytes/utf-8 response-body)))
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (cond
    ;; Redirects
    [(and (>= status-code 300) (< status-code 400))
     (raise-http-error!
      (format "~a API request redirected (~a: ~a). Check your base-url in config.json."
              provider-name
              status-code
              status-str))]
    ;; Authentication
    [(= status-code 401)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API authentication failed (401): ~a" provider-name error-text)
                        status-code)]
    ;; Forbidden
    [(= status-code 403)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API forbidden (403): ~a" provider-name error-text) status-code)]
    ;; Rate limited
    [(= status-code 429)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error!
      (format "~a API rate limited (429). Please wait and try again.\n~a" provider-name error-text)
      status-code)]
    ;; Bad request
    [(= status-code 400)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API bad request (400): ~a" provider-name error-text) status-code)]
    ;; Server errors
    [(>= status-code 500)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API server error (~a): ~a" provider-name status-code error-text)
                        status-code)]
    ;; Other client errors
    [(http-error? status-code)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API error (~a): ~a" provider-name status-code error-text)
                        status-code)]))

;; Safe extraction of error text from response body bytes.
;; Returns a readable string or a fallback message.
(define (safe-extract-error-text response-bytes)
  (with-handlers ([exn:fail? (lambda (_)
                               (format "<binary body ~a bytes>" (bytes-length response-bytes)))])
    (define jsexpr (bytes->jsexpr response-bytes))
    (or (extract-error-message jsexpr) (format "~a" jsexpr))))

;; Extract a readable error message from a JSON error response.
(define (extract-error-message jsexpr)
  (cond
    [(not (hash? jsexpr)) #f]
    [(hash-has-key? jsexpr 'error)
     (define err (hash-ref jsexpr 'error))
     (cond
       [(hash? err)
        (cond
          [(hash-has-key? err 'message)
           (define msg (hash-ref err 'message))
           (if (string? msg) msg #f)]
          [(hash-has-key? err 'code) (format "Error code: ~a" (hash-ref err 'code))]
          [else #f])]
       [(string? err) err]
       [else #f])]
    [(hash-has-key? jsexpr 'message)
     (define msg (hash-ref jsexpr 'message))
     (if (string? msg) msg #f)]
    [else #f]))

;; ============================================================
;; Provider stop-reason dispatch (Finding A9)
;; ============================================================

;; Provider-specific stop reason mappings.
(define stop-reason-table
  (hash
   'anthropic
   '(("end_turn" . stop) ("max_tokens" . length) ("stop_sequence" . stop) ("tool_use" . tool-calls))
   'gemini
   '(("STOP" . stop) ("MAX_TOKENS" . length)
                     ("SAFETY" . content-filtered)
                     ("RECITATION" . content-filtered))))

;; translate-stop-reason : (or/c 'anthropic 'gemini 'openai #f)
;;   (or/c string? symbol?) -> symbol?
;;
;; Translate a provider-specific stop/finish reason to a canonical symbol.
;; For providers not in the table (e.g. openai-family), applies
;; string->symbol with underscore→hyphen replacement.
(define (translate-stop-reason provider reason)
  (define r
    (cond
      [(string? reason) (string-trim reason)]
      [(symbol? reason) reason]
      [else 'stop]))
  (cond
    [(symbol? r) r]
    [(and provider (hash-has-key? stop-reason-table provider))
     (let* ([mapping (hash-ref stop-reason-table provider)]
            [entry (assoc r mapping)])
       (if entry
           (cdr entry)
           (string->symbol r)))]
    [else (string->symbol (string-replace r "_" "-"))]))
