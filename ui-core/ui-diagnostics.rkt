#lang racket

;;; ui-diagnostics.rkt — structured, rate-limited UI diagnostics.
;;;
;;; W5 (v0.99.96): shared observability surface for both frontends.
;;;
;;; Design constraints:
;;;   - PURE bookkeeping: this module never performs I/O and never
;;;     mutates any frontend state. Emitting a diagnostic only counts
;;;     it (and exposes the most recent payload) so callers can decide
;;;     how to surface it (log, stderr, telemetry).
;;;   - RATE-LIMITED: each category is keyed by `key` (usually a
;;;     turn-id, session-id, chord, or widget id). A repeat of the
;;;     same (category . key) pair within the window is suppressed but
;;;     still counted, and `rate-limit-window` can be reconfigured.
;;;   - SAFE FOR LOGS: payloads are truncated (`max-detail-length`)
;;;     and rendered via `~s` so newlines/control chars cannot forge
;;;     log lines.
;;;
;;; Diagnostic categories (see W5 action 3):
;;;   stream.ordering          — malformed stream ordering
;;;   reducer.unknown-turn     — unknown turn id in reducer
;;;   input.unsupported-chord  — key sequence decoder could not resolve
;;;   composer.clamped         — composer hit max rows / viewport engaged
;;;   persistence.dropped      — persistence dropped due to policy
;;;   renderer.cursor-clamped  — cursor clamped into assigned region

(require racket/contract racket/string racket/list)

(provide
 (contract-out
  [make-diagnostic-store (-> diagnostic-store?)]
  [diagnostic-store? (-> any/c boolean?)]
  [current-ui-diagnostics (parameter/c diagnostic-store?)]
  [rate-limit-window (parameter/c (and/c real? (not/c negative?)))]
  [max-detail-length (parameter/c (and/c exact-nonnegative-integer? (<=/c 4096)))]
  [ui-diagnostic! (->* (symbol?) (string? any/c) boolean?)]
  [ui-diagnostic-format (-> diagnostic? string?)]
  [diagnostic? (-> any/c boolean?)]
  [diagnostic-category (-> diagnostic? symbol?)]
  [diagnostic-key (-> diagnostic? (or/c #f string?))]
  [diagnostic-detail (-> diagnostic? any/c)]
  [diagnostic-timestamp (-> diagnostic? real?)]
  [diagnostic-count (-> diagnostic? exact-positive-integer?)]
  [diagnostic-suppressed (-> diagnostic? exact-nonnegative-integer?)]
  [ui-diagnostics-recent (->* () (diagnostic-store? exact-nonnegative-integer?) (listof diagnostic?))]
  [ui-diagnostics-summary (->* () (diagnostic-store?) (hash/c symbol? exact-nonnegative-integer?))]
  [ui-diagnostics-reset! (->* () (diagnostic-store?) void?)]))

;; ------------------------------------------------------------
;; Data types
;; ------------------------------------------------------------

(struct diagnostic (category key detail timestamp count suppressed) #:transparent)

(struct diagnostic-store
  (lock                 ; semaphore guarding the mutable window/counters
   [window #:mutable]   ; hash: (cons cat key) -> (cons last-ts diagnostic)
   [counters #:mutable] ; hash: category -> total occurrences
   [recent #:mutable]   ; list of most recent emitted diagnostics
   )
  #:transparent)

;; Like take, but tolerates short lists.
(define (take-at-most lst n) (take lst (min n (length lst))))

(define (make-diagnostic-store)
  (diagnostic-store (make-semaphore 1) (hash) (hash) '()))

(define current-ui-diagnostics (make-parameter (make-diagnostic-store)))
(define rate-limit-window (make-parameter 5.0))          ; seconds
(define max-detail-length (make-parameter 240))           ; rendered chars

;; ------------------------------------------------------------
;; Sanitising helper — safe for logs
;; ------------------------------------------------------------

;; Truncate the rendered detail so diagnostics cannot flood logs or
;; forge extra log lines. Rendering goes through ~s, so embedded
;; newlines appear escaped as \n inside the Racket representation.
(define (sanitize-detail detail)
  (define rendered
    (with-handlers ([exn:fail? (lambda (e) "<unprintable>")])
      (format "~s" detail)))
  (if (> (string-length rendered) (max-detail-length))
      (string-append (substring rendered 0 (max-detail-length)) "…")
      rendered))

;; ------------------------------------------------------------
;; Core emit
;; ------------------------------------------------------------

;; Record a diagnostic. Returns #t when it was EMITTED (i.e. not
;; rate-limit-suppressed), #f when suppressed by the per-key window.
;; Suppressed repeats still increment the occurrence counters and the
;; suppressed tally attached to the original diagnostic.
(define (ui-diagnostic! category [key #f] [detail #f])
  (define store (current-ui-diagnostics))
  (define now (current-inexact-milliseconds))
  (define window (* (rate-limit-window) 1000.0))
  (call-with-semaphore (diagnostic-store-lock store)
    (lambda ()
      (define wkey (cons category (and key (if (string? key) key (format "~a" key)))))
      (define w (diagnostic-store-window store))
      (define prior (hash-ref w wkey #f))
      (define total (add1 (hash-ref (diagnostic-store-counters store) category 0)))
      (set-diagnostic-store-counters!
       store (hash-set (diagnostic-store-counters store) category total))
      (cond
        [(and prior (< (- now (diagnostic-timestamp prior)) window))
         ;; suppressed repeat: bump counters, keep the first payload
         (set-diagnostic-store-window!
          store (hash-set w wkey
                          (struct-copy diagnostic prior
                                       [count total]
                                       [suppressed (add1 (diagnostic-suppressed prior))])))
         #f]
        [else
         (define d (diagnostic category
                               (and key (if (string? key) key (format "~a" key)))
                               (sanitize-detail detail)
                               now total 0))
         (set-diagnostic-store-window! store (hash-set w wkey d))
         (set-diagnostic-store-recent!
          store (cons d (take-at-most (diagnostic-store-recent store) 256)))
         #t]))))

;; Human-readable, single-line rendering suitable for logs.
(define (ui-diagnostic-format d)
  (format "ui-diagnostic[~a]~a count=~a suppressed=~a ~a"
          (diagnostic-category d)
          (if (diagnostic-key d)
              (format " key=~s" (diagnostic-key d))
              "")
          (diagnostic-count d)
          (diagnostic-suppressed d)
          (diagnostic-detail d)))

;; ------------------------------------------------------------
;; Introspection / tests
;; ------------------------------------------------------------

(define (ui-diagnostics-recent [store (current-ui-diagnostics)] [limit 50])
  (call-with-semaphore (diagnostic-store-lock store)
    (lambda () (take-at-most (diagnostic-store-recent store) limit))))

(define (ui-diagnostics-summary [store (current-ui-diagnostics)])
  (call-with-semaphore (diagnostic-store-lock store)
    (lambda () (diagnostic-store-counters store))))

(define (ui-diagnostics-reset! [store (current-ui-diagnostics)])
  (call-with-semaphore (diagnostic-store-lock store)
    (lambda ()
      (set-diagnostic-store-window! store (hash))
      (set-diagnostic-store-counters! store (hash))
      (set-diagnostic-store-recent! store '()))))

;; ------------------------------------------------------------
;; Module tests
;; ------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "emit is recorded and rate-limited per key"
    (define store (make-diagnostic-store))
    (parameterize ([current-ui-diagnostics store]
                   [rate-limit-window 10.0])
      (check-true (ui-diagnostic! 'renderer.cursor-clamped "turn-1" "row 3 -> 2"))
      ;; immediate repeat is suppressed
      (check-false (ui-diagnostic! 'renderer.cursor-clamped "turn-1" "row 3 -> 2"))
      ;; different key is emitted
      (check-true (ui-diagnostic! 'renderer.cursor-clamped "turn-2" "col 40 -> 39"))
      (define recent (ui-diagnostics-recent store))
      (check-equal? (length recent) 2)
      (check-equal? (diagnostic-category (car recent)) 'renderer.cursor-clamped)
      (check-equal? (hash-ref (ui-diagnostics-summary store) 'renderer.cursor-clamped) 3)))

  (test-case "window expiry re-emits"
    (define store (make-diagnostic-store))
    (parameterize ([current-ui-diagnostics store]
                   [rate-limit-window 0.0])
      (check-true (ui-diagnostic! 'stream.ordering "t" "completed without delta"))
      (check-true (ui-diagnostic! 'stream.ordering "t" "completed without delta"))
      (check-equal? (length (ui-diagnostics-recent store)) 2)))

  (test-case "detail is sanitized and truncated"
    (define store (make-diagnostic-store))
    (parameterize ([current-ui-diagnostics store]
                   [max-detail-length 16])
      (ui-diagnostic! 'input.unsupported-chord "ctrl-alt-x" "very long detail string")
      (define d (car (ui-diagnostics-recent store)))
      ;; truncated to the limit plus ellipsis
      (check-true (<= (string-length (diagnostic-detail d)) 17))
      ;; formatted output must be a single line (safe for logs)
      (check-false (string-contains? (ui-diagnostic-format d) "\n"))
      ;; embedded newlines are escaped by ~s, never literal
      (ui-diagnostic! 'stream.ordering "t" "a\nb")
      (define d2 (car (ui-diagnostics-recent store)))
      (check-false (string-contains? (ui-diagnostic-format d2) "\n"))))

  (test-case "reset clears state"
    (define store (make-diagnostic-store))
    (parameterize ([current-ui-diagnostics store])
      (ui-diagnostic! 'composer.clamped "session" "max rows 6")
      (ui-diagnostics-reset! store)
      (check-equal? (ui-diagnostics-recent store) '())
      (check-equal? (hash-count (ui-diagnostics-summary store)) 0))))

(module+ test
  ;; key sanitising: non-string keys are stringified, never crash
  (require rackunit)
  (define store (make-diagnostic-store))
  (parameterize ([current-ui-diagnostics store])
    (check-true (ui-diagnostic! 'reducer.unknown-turn 42 #f))
    (check-equal? (diagnostic-key (car (ui-diagnostics-recent store))) "42")))
