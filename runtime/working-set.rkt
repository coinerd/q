#lang racket/base

;; runtime/working-set.rkt — Working Set Memory for Context Assembly
;; STABILITY: testing
;;
;; Tracks recently-read file contents that the agent still needs for
;; subsequent edits. Prevents read spirals by pinning read results in
;; the assembled context.
;;
;; Lifecycle rules:
;;   read  → add/refresh entry for path
;;   edit  → remove entry for path (content consumed)
;;   write → remove entry for path (file overwritten)
;;   bash  → no change (transient output)
;;   reset → clear all entries (new user message)

(require racket/contract
         racket/list
         (only-in "../util/errors.rkt" raise-session-error))

;; ────────────────────────────────────────────────────────────
;; Data structures
;; ────────────────────────────────────────────────────────────

;; A working set entry tracks a file read that's still "active"
(struct ws-entry (path message-id token-estimate timestamp) #:transparent)

;; Working set state (mutable)
(struct working-set ([entries #:mutable] [max-entries #:mutable] [max-tokens #:mutable])
  #:transparent)

;; ────────────────────────────────────────────────────────────
;; Constructor
;; ────────────────────────────────────────────────────────────

(define (make-working-set #:max-entries [max-entries 30] #:max-tokens [max-tokens 15000])
  (working-set '() max-entries max-tokens))

;; ────────────────────────────────────────────────────────────
;; Queries (use struct accessors directly for entries)
;; ────────────────────────────────────────────────────────────

(define (working-set-entry-count ws)
  (length (working-set-entries ws)))

(define (working-set-token-count ws)
  (for/sum ([e (in-list (working-set-entries ws))]) (ws-entry-token-estimate e)))

;; ────────────────────────────────────────────────────────────
;; Mutations
;; ────────────────────────────────────────────────────────────

(define (working-set-reset! ws)
  (set-working-set-entries! ws '()))

;; Add or refresh an entry. If path already exists, update it (move to front).
;; After adding, enforce max-entries and max-tokens via LRU eviction.
(define (working-set-add! ws path message-id token-estimate)
  (define now (current-seconds))
  (define new-entry (ws-entry path message-id token-estimate now))
  (define existing (working-set-entries ws))
  ;; Remove any existing entry for the same path
  (define without-path (filter (lambda (e) (not (equal? (ws-entry-path e) path))) existing))
  ;; Add new entry at front (most recent)
  (set-working-set-entries! ws (cons new-entry without-path))
  ;; Enforce constraints
  (working-set-enforce-budget! ws))

;; Remove entry for a path (edit/write consumed the content)
(define (working-set-remove! ws path)
  (define existing (working-set-entries ws))
  (define filtered (filter (lambda (e) (not (equal? (ws-entry-path e) path))) existing))
  (set-working-set-entries! ws filtered))

;; Enforce max-entries and max-tokens by evicting least-recently used entries.
;; Entries are ordered newest-first, so we evict from the tail (oldest).
(define (working-set-enforce-budget! ws)
  (define max-e (working-set-max-entries ws))
  (define max-t (working-set-max-tokens ws))
  ;; Single-pass: find the largest prefix that fits both budgets.
  ;; Previous version used drop-right in a loop — O(n²).
  (let loop ([entries (working-set-entries ws)]
             [acc '()]
             [count 0]
             [tokens 0])
    (cond
      [(null? entries) (set-working-set-entries! ws (reverse acc))]
      [else
       (define e (car entries))
       (define new-count (add1 count))
       (define new-tokens (+ tokens (ws-entry-token-estimate e)))
       (cond
         [(and (<= new-count max-e) (<= new-tokens max-t))
          (loop (cdr entries) (cons e acc) new-count new-tokens)]
         ;; Budget would be exceeded — stop here
         [else (set-working-set-entries! ws (reverse acc))])])))

;; Remove entries NOT matching a predicate (keeps selective entries).
(define (working-set-selective-remove! ws keep-pred?)
  (define existing (working-set-entries ws))
  (define filtered (filter keep-pred? existing))
  (set-working-set-entries! ws filtered)
  (working-set-enforce-budget! ws))

;; ────────────────────────────────────────────────────────────
;; Tool call processing
;; ────────────────────────────────────────────────────────────

;; Process a batch of tool calls and their result messages.
;; Each tool-call is a hash with 'name and 'arguments keys.
;; Each result-msg is a message struct (or any value with extractable id).
;; msg-id-fn: function to extract message-id from a result-msg
;; token-fn: function to estimate tokens from a result-msg
;; Returns ws (for convenience)
(define (working-set-update! ws tool-calls result-msgs msg-id-fn token-fn)
  (for ([tc (in-list tool-calls)]
        [rm (in-list result-msgs)])
    (define name
      (if (hash? tc)
          (hash-ref tc 'name "")
          ""))
    (define args
      (if (hash? tc)
          (hash-ref tc 'arguments (hasheq))
          (hasheq)))
    (define path
      (if (hash? args)
          (hash-ref args 'path "")
          ""))
    (cond
      [(equal? name "read") (working-set-add! ws path (msg-id-fn rm) (token-fn rm))]
      [(or (equal? name "edit") (equal? name "write"))
       (when (and (string? path) (positive? (string-length path)))
         (working-set-remove! ws path))]
      ;; bash and other tools: no change
      [else (void)]))
  ws)

;; ────────────────────────────────────────────────────────────
;; Message resolution
;; ────────────────────────────────────────────────────────────

;; Find the message structs corresponding to working set entries.
;; msg-id-fn: function to extract id from a message struct
;; Returns messages in the same order as ws entries (newest first).
;; Missing messages are silently skipped.
(define (working-set-resolve-messages ws messages msg-id-fn)
  (define msg-by-id
    (for/hash ([m (in-list messages)])
      (values (msg-id-fn m) m)))
  (for/list ([e (in-list (working-set-entries ws))]
             #:when (hash-has-key? msg-by-id (ws-entry-message-id e)))
    (hash-ref msg-by-id (ws-entry-message-id e))))

;; ────────────────────────────────────────────────────────────
;; Closure factory (v0.29.4 W2)
;; ────────────────────────────────────────────────────────────

;; Create an isolated working-set context using closures.
;; Returns a dispatch procedure that accepts actions:
;;   'entries, 'entry-count, 'token-count,
;;   'max-entries, 'max-tokens,
;;   'add!, 'remove!, 'reset!
;; Thread-safe via internal semaphore.
(define (make-ws-context #:max-entries [max-entries 30] #:max-tokens [max-tokens 15000])
  (let ([entries '()]
        [sem (make-semaphore 1)])
    (lambda (action . args)
      (call-with-semaphore
       sem
       (lambda ()
         (case action
           [(entries) entries]
           [(entry-count) (length entries)]
           [(token-count) (for/sum ([e (in-list entries)]) (ws-entry-token-estimate e))]
           [(max-entries) max-entries]
           [(max-tokens) max-tokens]
           [(reset!) (set! entries '())]
           [(add!)
            (define path (car args))
            (define msg-id (cadr args))
            (define tokens (caddr args))
            (unless (string? path)
              (raise-argument-error 'ws-context "string" path))
            (unless (number? tokens)
              (raise-argument-error 'ws-context "number" tokens))
            (define new-entry (ws-entry path msg-id tokens (current-seconds)))
            (define without-existing
              (filter (lambda (e) (not (equal? (ws-entry-path e) path))) entries))
            (set! entries (cons new-entry without-existing))
            ;; Enforce max-entries
            (when (> (length entries) max-entries)
              (set! entries (take entries max-entries)))]
           [(remove!)
            (define path (car args))
            (set! entries (filter (lambda (e) (not (equal? (ws-entry-path e) path))) entries))]
           [else (raise-session-error (format "unknown action: ~a" action) #f)]))))))

(provide (contract-out
          [working-set? (-> any/c boolean?)]
          [make-working-set
           (->* ()
                (#:max-entries exact-nonnegative-integer? #:max-tokens exact-nonnegative-integer?)
                working-set?)]
          [make-ws-context
           (->* ()
                (#:max-entries exact-nonnegative-integer? #:max-tokens exact-nonnegative-integer?)
                procedure?)]
          [working-set-entries (-> working-set? list?)]
          [working-set-entry-count (-> working-set? exact-nonnegative-integer?)]
          [working-set-token-count (-> working-set? exact-nonnegative-integer?)]
          [working-set-reset! (-> working-set? void?)]
          [working-set-update! (-> working-set? list? list? procedure? procedure? working-set?)]
          [working-set-resolve-messages (-> working-set? list? procedure? list?)]
          [ws-entry
           (-> string? any/c exact-nonnegative-integer? exact-nonnegative-integer? ws-entry?)]
          [ws-entry? (-> any/c boolean?)]
          [ws-entry-path (-> ws-entry? string?)]
          [ws-entry-message-id (-> ws-entry? any/c)]
          [ws-entry-token-estimate (-> ws-entry? exact-nonnegative-integer?)]
          [ws-entry-timestamp (-> ws-entry? exact-nonnegative-integer?)]
          [working-set-selective-remove! (-> working-set? procedure? void?)]
          [working-set-add! (-> working-set? string? any/c exact-nonnegative-integer? void?)]
          [working-set-remove! (-> working-set? string? void?)]))
