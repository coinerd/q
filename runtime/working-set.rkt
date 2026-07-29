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
         (only-in "../util/error/errors.rkt" raise-session-error)
         (only-in "../util/tool/tool-types.rkt"
                  tool-call?
                  tool-call-id
                  tool-call-name
                  tool-call-arguments)
         (only-in "../util/message/message.rkt" message? message-content message-meta-safe)
         (only-in "../util/content/content-parts.rkt"
                  tool-result-part?
                  tool-result-part-tool-call-id
                  tool-result-part-is-error?))

;; ────────────────────────────────────────────────────────────
;; Data structures
;; ────────────────────────────────────────────────────────────

;; A working set entry tracks a file read that's still "active"
(struct ws-entry (path message-id token-estimate timestamp budget-action) #:transparent)

;; Default budget-action values
(define WS-ACTION-KEPT 'kept)
(define WS-ACTION-SUMMARIZED 'summarized)
(define WS-ACTION-EVICTED 'evicted)
(define WS-ACTION-SUPERSEDED 'superseded)

;; v0.96.13 W4: Extract text summary from a ws-entry for distillation input
(define (ws-entry->text entry)
  (format "[~a] ~a (~a tokens, ~a)"
          (ws-entry-timestamp entry)
          (ws-entry-path entry)
          (ws-entry-token-estimate entry)
          (ws-entry-budget-action entry)))

;; Working set state (mutable)
(struct working-set ([entries #:mutable] [max-entries #:mutable] [max-tokens #:mutable])
  #:transparent)

;; ────────────────────────────────────────────────────────────
;; Constructor
;; ────────────────────────────────────────────────────────────

(define (compute-working-set-budget context-budget)
  (min 8192 (quotient (* context-budget 3) 10)))

(define (make-working-set #:max-entries [max-entries 20] #:max-tokens [max-tokens 8192])
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
;; Returns (list-of ws-entry) of evicted entries for telemetry.
(define (working-set-add! ws path message-id token-estimate)
  (define now (current-seconds))
  (define new-entry (ws-entry path message-id token-estimate now 'kept))
  (define existing (working-set-entries ws))
  ;; Check if this supersedes an existing entry
  (define superseded
    (for/list ([e (in-list existing)]
               #:when (equal? (ws-entry-path e) path))
      (struct-copy ws-entry e [budget-action 'superseded])))
  ;; Remove any existing entry for the same path
  (define without-path (filter (lambda (e) (not (equal? (ws-entry-path e) path))) existing))
  ;; Add new entry at front (most recent)
  (set-working-set-entries! ws (cons new-entry without-path))
  ;; Enforce constraints
  (define evicted (working-set-enforce-budget! ws))
  ;; Return evicted + superseded for telemetry
  (append evicted superseded))

;; Remove entry for a path (edit/write consumed the content)
(define (working-set-remove! ws path)
  (define existing (working-set-entries ws))
  (define filtered (filter (lambda (e) (not (equal? (ws-entry-path e) path))) existing))
  (set-working-set-entries! ws filtered))

;; Enforce max-entries and max-tokens by evicting least-recently used entries.
;; Entries are ordered newest-first, so we evict from the tail (oldest).
;; Returns (list-of ws-entry) of evicted entries for telemetry.
(define (working-set-enforce-budget! ws)
  (define max-e (working-set-max-entries ws))
  (define max-t (working-set-max-tokens ws))
  ;; Single-pass: find the largest prefix that fits both budgets.
  (let loop ([entries (working-set-entries ws)]
             [acc '()]
             [evicted '()]
             [count 0]
             [tokens 0])
    (cond
      [(null? entries)
       (set-working-set-entries! ws (reverse acc))
       (reverse evicted)]
      [else
       (define e (car entries))
       (define new-count (add1 count))
       (define new-tokens (+ tokens (ws-entry-token-estimate e)))
       (cond
         [(and (<= new-count max-e) (<= new-tokens max-t))
          (loop (cdr entries) (cons e acc) evicted new-count new-tokens)]
         ;; Budget would be exceeded — evict this entry
         [else
          (define evicted-entry (struct-copy ws-entry e [budget-action 'evicted]))
          (loop (cdr entries) acc (cons evicted-entry evicted) count tokens)])])))

;; Remove entries NOT matching a predicate (keeps selective entries).
(define (working-set-selective-remove! ws keep-pred?)
  (define existing (working-set-entries ws))
  (define filtered (filter keep-pred? existing))
  (set-working-set-entries! ws filtered)
  (working-set-enforce-budget! ws)
  (void))

;; ────────────────────────────────────────────────────────────
;; Tool call processing
;; ────────────────────────────────────────────────────────────

;; Process a batch of tool calls and their result messages. Calls may be
;; canonical tool-call structs or legacy hashes. Protocol-aware batches are
;; joined by tool-call ID, so scheduler completion order cannot associate a
;; read result with the wrong path.
(define (hash-ref/keys h keys [default #f])
  (or (for/or ([key (in-list keys)])
        (hash-ref h key #f))
      default))

(define (call-fields tc)
  (cond
    [(tool-call? tc) (values (tool-call-id tc) (tool-call-name tc) (tool-call-arguments tc))]
    [(hash? tc)
     (values (hash-ref/keys tc '(id tool-call-id toolCallId "id" "tool-call-id" "toolCallId"))
             (hash-ref/keys tc '(name "name") "")
             (hash-ref/keys tc '(arguments "arguments") (hasheq)))]
    [else (values #f "" (hasheq))]))

;; Return a result's protocol correlation ID and error status. Plain legacy
;; messages deliberately return no ID and are eligible for positional pairing.
(define (result-protocol-fields rm)
  (cond
    [(message? rm)
     (define part
       (for/or ([p (in-list (message-content rm))]
                #:when (tool-result-part? p))
         p))
     (define meta (message-meta-safe rm))
     (define protocol-id
       (or (and part (tool-result-part-tool-call-id part))
           (hash-ref/keys meta '(toolCallId tool-call-id "toolCallId" "tool-call-id"))))
     (define error?
       (or (and part (tool-result-part-is-error? part))
           (hash-ref/keys meta '(isError is-error? "isError" "is-error?") #f)))
     (values protocol-id (and error? #t))]
    [else (values #f #f)]))

(define (working-set-update!/actions ws tool-calls result-msgs msg-id-fn token-fn)
  (define actions '())
  (define result-by-call-id
    (for/fold ([by-id (hash)]) ([rm (in-list result-msgs)])
      (define-values (result-id _error?) (result-protocol-fields rm))
      (if result-id
          (hash-set by-id result-id rm)
          by-id)))
  ;; Hash-only legacy callers predate call IDs. Preserve their positional API;
  ;; once any call carries an ID, only ID-less results receive that fallback.
  (define protocol-call-ids?
    (for/or ([tc (in-list tool-calls)])
      (define-values (call-id _name _args) (call-fields tc))
      call-id))
  (for ([tc (in-list tool-calls)]
        [index (in-naturals)])
    (define-values (call-id name args) (call-fields tc))
    (define path
      (if (hash? args)
          (hash-ref/keys args '(path "path") "")
          ""))
    (define positional-rm (and (< index (length result-msgs)) (list-ref result-msgs index)))
    (define-values (positional-id _positional-error?)
      (if positional-rm
          (result-protocol-fields positional-rm)
          (values #f #f)))
    (define rm
      (or (and call-id (hash-ref result-by-call-id call-id #f))
          (and positional-rm (or (not protocol-call-ids?) (not positional-id)) positional-rm)))
    (define-values (_result-id result-error?)
      (if rm
          (result-protocol-fields rm)
          (values #f #f)))
    (cond
      ;; Failed reads neither add nor supersede an existing successful entry.
      [(and (equal? name "read") rm (not result-error?))
       (set! actions (append actions (working-set-add! ws path (msg-id-fn rm) (token-fn rm))))]
      [(and rm (not result-error?) (or (equal? name "edit") (equal? name "write")))
       (when (and (string? path) (positive? (string-length path)))
         (working-set-remove! ws path))]
      ;; bash, failed/unmatched mutations, and other tools: no change
      [else (void)]))
  actions)

(define (working-set-update! ws tool-calls result-msgs msg-id-fn token-fn)
  (working-set-update!/actions ws tool-calls result-msgs msg-id-fn token-fn)
  ws)

;; Tighten active state to the frozen provider-bound context share.
(define (working-set-enforce-context-share! ws assembled-context-tokens)
  (set-working-set-max-tokens! ws
                               (min (working-set-max-tokens ws)
                                    (compute-working-set-budget assembled-context-tokens)))
  (working-set-enforce-budget! ws))

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
;; GAP-N v0.97.12: Token-budget eviction helper for ws-context closure.
(define (evict-to-token-budget entries max-tokens)
  (let loop ([es entries]
             [evicted '()])
    (cond
      [(and (pair? es) (> (for/sum ([e (in-list es)]) (ws-entry-token-estimate e)) max-tokens))
       (define evicted-entry
         (ws-entry (ws-entry-path (car (reverse es)))
                   (ws-entry-message-id (car (reverse es)))
                   (ws-entry-token-estimate (car (reverse es)))
                   (ws-entry-timestamp (car (reverse es)))
                   'evicted))
       (loop (take es (sub1 (length es))) (cons evicted-entry evicted))]
      [else es])))

(define (make-ws-context #:max-entries [max-entries 20] #:max-tokens [max-tokens 8192])
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
            (define new-entry (ws-entry path msg-id tokens (current-seconds) 'kept))
            (define existing-superseded
              (for/list ([e (in-list entries)]
                         #:when (equal? (ws-entry-path e) path))
                (ws-entry (ws-entry-path e)
                          (ws-entry-message-id e)
                          (ws-entry-token-estimate e)
                          (ws-entry-timestamp e)
                          'superseded)))
            (define without-existing
              (filter (lambda (e) (not (equal? (ws-entry-path e) path))) entries))
            (set! entries (cons new-entry without-existing))
            ;; Enforce max-entries
            (when (> (length entries) max-entries)
              (set! entries (take entries max-entries))
              ;; Tag evicted entries with budget-action
              (define kept-count (min max-entries (length entries)))
              (set! entries
                    (append (take entries kept-count)
                            (for/list ([e (in-list (drop entries kept-count))])
                              (ws-entry (ws-entry-path e)
                                        (ws-entry-message-id e)
                                        (ws-entry-token-estimate e)
                                        (ws-entry-timestamp e)
                                        'evicted)))))
            ;; GAP-N v0.97.12: Enforce max-tokens (evict from tail)
            (set! entries (evict-to-token-budget entries max-tokens))]
           [(remove!)
            (define path (car args))
            (set! entries (filter (lambda (e) (not (equal? (ws-entry-path e) path))) entries))]
           [else (raise-session-error (format "unknown action: ~a" action) #f)]))))))

(provide (contract-out
          [working-set? (-> any/c boolean?)]
          [compute-working-set-budget (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
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
          [working-set-update!/actions
           (-> working-set? list? list? procedure? procedure? (listof ws-entry?))]
          [working-set-enforce-context-share!
           (-> working-set? exact-nonnegative-integer? (listof ws-entry?))]
          [working-set-resolve-messages (-> working-set? list? procedure? list?)]
          [ws-entry
           (-> string? any/c exact-nonnegative-integer? exact-nonnegative-integer? symbol? ws-entry?)]
          [ws-entry? (-> any/c boolean?)]
          [ws-entry-path (-> ws-entry? string?)]
          [ws-entry-message-id (-> ws-entry? any/c)]
          [ws-entry-token-estimate (-> ws-entry? exact-nonnegative-integer?)]
          [ws-entry-timestamp (-> ws-entry? exact-nonnegative-integer?)]
          [ws-entry->text (-> ws-entry? string?)]
          [working-set-selective-remove! (-> working-set? procedure? void?)]
          [working-set-add!
           (-> working-set? string? any/c exact-nonnegative-integer? (listof ws-entry?))]
          [working-set-remove! (-> working-set? string? void?)]
          [ws-entry-budget-action (-> ws-entry? symbol?)]
          [working-set-enforce-budget! (-> working-set? (listof ws-entry?))]
          [WS-ACTION-KEPT symbol?]
          [WS-ACTION-SUMMARIZED symbol?]
          [WS-ACTION-EVICTED symbol?]
          [WS-ACTION-SUPERSEDED symbol?]))
