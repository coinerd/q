#lang racket/base

;; util/protocol-types.rkt — canonical runtime structs and enums
;;
;; Defines the core data types for q:
;;   - content parts (text, tool-call, tool-result)
;;   - messages
;;   - event envelopes
;;   - tool-call / tool-result (standalone — canonical definitions)
;;   - loop-result
;;
;; All structs provide JSON serialization/deserialization via
;; message->jsexpr / jsexpr->message and event->jsexpr / jsexpr->event.
;;
;; ARCH-01/04/05: extracted from agent/types.rkt to eliminate layer violations.
;; Re-exported from agent/types.rkt for backward compatibility.

(require racket/contract)

;; Content parts
(provide (struct-out text-part)
         (struct-out tool-call-part)
         (struct-out tool-result-part)
         make-text-part
         make-tool-call-part
         make-tool-result-part
         content-part->jsexpr
         jsexpr->content-part
         content-part-type

         ;; Message
         (struct-out message)
         make-message
         message->jsexpr
         jsexpr->message

         ;; Custom entry for extension state persistence (#1147)
         make-custom-entry
         custom-entry?
         custom-entry-extension
         custom-entry-key
         custom-entry-data
         ;; Entry kind predicates (#497)
         message-entry?
         model-change-entry?
         thinking-level-change-entry?
         branch-summary-entry?
         custom-message-entry?
         session-info-entry?
         compaction-summary-entry?
         tool-result-entry?
         bash-execution-entry?
         any-tool-result-entry?
         ;; Tree entry types (#1314)
         make-branch-entry
         branch-entry?
         branch-entry-parent-entry-id
         branch-entry-name
         make-tree-navigation-entry
         tree-navigation-entry?
         tree-navigation-entry-target-entry-id
         tree-navigation-entry-from-entry-id
         make-branch-summary-entry
         branch-summary-entry-summary
         branch-summary-entry-entry-range
         branch-summary-entry-token-count
         tree-entry?
         ;; Event envelope
         (struct-out event)
         event-event
         make-event
         event->jsexpr
         jsexpr->event

         ;; Versioning
         CURRENT-EVENT-VERSION

         ;; Tool-call (standalone — canonical definition)
         (struct-out tool-call)

         ;; Tool-result (standalone — canonical definition)
         (struct-out tool-result)

         ;; Loop-result
         (struct-out loop-result)
         make-loop-result)

;; ============================================================
;; Content parts
;; ============================================================

;; Base struct — not exported directly; use type-specific constructors.
(struct content-part (type) #:transparent)

;; Text content part
(struct text-part content-part (text) #:transparent)

;; Tool-call content part
(struct tool-call-part content-part (id name arguments) #:transparent)

;; Tool-result content part
(struct tool-result-part content-part (tool-call-id content is-error?) #:transparent)

;; Convenience constructors
(define (make-text-part text)
  (text-part "text" text))

(define (make-tool-call-part id name arguments)
  (tool-call-part "tool-call" id name arguments))

(define (make-tool-result-part tool-call-id content is-error?)
  (tool-result-part "tool-result" tool-call-id content is-error?))

;; Serialization
(define (content-part->jsexpr cp)
  (cond
    [(text-part? cp) (hasheq 'type "text" 'text (text-part-text cp))]
    [(tool-call-part? cp)
     (hasheq 'type
             "tool-call"
             'id
             (tool-call-part-id cp)
             'name
             (tool-call-part-name cp)
             'arguments
             (tool-call-part-arguments cp))]
    [(tool-result-part? cp)
     (hasheq 'type
             "tool-result"
             'toolCallId
             (tool-result-part-tool-call-id cp)
             'content
             (tool-result-part-content cp)
             'isError
             (tool-result-part-is-error? cp))]
    [else (error 'content-part->jsexpr "unknown content part type: ~a" cp)]))

;; Deserialization
(define (jsexpr->content-part h)
  (define tp (hash-ref h 'type))
  (case tp
    [("text") (make-text-part (hash-ref h 'text))]
    [("tool-call") (make-tool-call-part (hash-ref h 'id) (hash-ref h 'name) (hash-ref h 'arguments))]
    [("tool-result")
     (make-tool-result-part (hash-ref h 'toolCallId) (hash-ref h 'content) (hash-ref h 'isError))]
    [else (error 'jsexpr->content-part "unknown content part type: ~a" tp)]))

;; ============================================================
;; Message struct
;; ============================================================

(struct message (id parent-id role kind content timestamp meta) #:transparent)

(define (make-message id parent-id role kind content timestamp meta)
  (message id parent-id role kind content timestamp meta))

;; Symbol -> string for JSON
(define (symbol->string* s)
  (if (symbol? s)
      (symbol->string s)
      s))

;; String -> symbol from JSON
(define (string->symbol* s)
  (if (string? s)
      (string->symbol s)
      s))

;; Serialize message to jsexpr (hash)
(define (message->jsexpr msg)
  (hasheq 'id
          (message-id msg)
          'parentId
          (message-parent-id msg) ; #f -> JSON null
          'role
          (symbol->string* (message-role msg))
          'kind
          (symbol->string* (message-kind msg))
          'content
          (map content-part->jsexpr (message-content msg))
          'timestamp
          (message-timestamp msg)
          'meta
          (message-meta msg)))

;; Deserialize jsexpr (hash) to message
(define (jsexpr->message h)
  (make-message (hash-ref h 'id)
                (hash-ref h 'parentId) ; JSON null -> #f
                (string->symbol* (hash-ref h 'role))
                (string->symbol* (hash-ref h 'kind))
                (map jsexpr->content-part (hash-ref h 'content))
                (hash-ref h 'timestamp)
                (hash-ref h 'meta)))

;; ============================================================
;; Custom entry for extension state persistence (#1147)
;; ============================================================

;; Creates a message struct with kind 'custom-message and extension
;; metadata in the meta field. Used by extensions to persist state.
(define (make-custom-entry extension-name key data)
  (make-message (format "custom-~a-~a-~a" extension-name key (current-inexact-milliseconds))
                #f
                'system
                'custom-message
                '()
                (current-seconds)
                (hasheq 'extension extension-name 'key key 'data data)))

;; Predicate: is this message a custom entry?
(define (custom-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'custom-message)))

;; Accessor: get extension name from a custom entry
(define (custom-entry-extension msg)
  (hash-ref (message-meta msg) 'extension #f))

;; Accessor: get key from a custom entry
(define (custom-entry-key msg)
  (hash-ref (message-meta msg) 'key #f))

;; Accessor: get data from a custom entry
(define (custom-entry-data msg)
  (hash-ref (message-meta msg) 'data #f))

;; ============================================================
;; Entry kind predicates (#497)
;; ============================================================
;; Entry kinds extend the message struct's `kind` field to support
;; session metadata and special entries beyond user/assistant messages.

(define (message-entry? msg)
  ;; Standard message entry (user or assistant)
  (and (message? msg) (memq (message-kind msg) '(message)) #t))

(define (model-change-entry? msg)
  ;; Records a model change mid-session
  (and (message? msg) (eq? (message-kind msg) 'model-change)))

(define (thinking-level-change-entry? msg)
  ;; Records a thinking level change mid-session
  (and (message? msg) (eq? (message-kind msg) 'thinking-level-change)))

(define (branch-summary-entry? msg)
  ;; Summary of a branched path (used in context assembly)
  (and (message? msg) (eq? (message-kind msg) 'branch-summary)))

(define (custom-message-entry? msg)
  ;; Custom/labeled message for extensions
  (and (message? msg) (eq? (message-kind msg) 'custom-message)))

(define (session-info-entry? msg)
  ;; Session metadata (version, settings, etc.)
  (and (message? msg) (eq? (message-kind msg) 'session-info)))

(define (compaction-summary-entry? msg)
  ;; Compaction summary entry
  (and (message? msg) (eq? (message-kind msg) 'compaction-summary)))

(define (tool-result-entry? msg)
  ;; Tool result entry
  (and (message? msg) (eq? (message-kind msg) 'tool-result)))

(define (bash-execution-entry? msg)
  ;; Bash execution entry — preserves command metadata (exit-code, duration, etc.)
  (and (message? msg) (eq? (message-kind msg) 'bash-execution)))

(define (any-tool-result-entry? msg)
  ;; Matches both generic tool-result and bash-execution entries
  (and (message? msg) (memq (message-kind msg) '(tool-result bash-execution))))

;; ============================================================
;; Tree entry types (#1314)
;; ============================================================
;; Tree entries are message structs with specific kinds and metadata.
;; They support session branching and navigation within the JSONL log.

;; branch-entry: marks where a branch was created
;;   - id: unique entry id
;;   - parent-id: the entry being branched from
;;   - branch-name: human-readable branch name
(define (make-branch-entry id parent-entry-id branch-name)
  (make-message id
                parent-entry-id
                'system
                'branch
                '()
                (current-seconds)
                (hasheq 'branchName branch-name 'parentEntryId parent-entry-id)))

(define (branch-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'branch)))

(define (branch-entry-parent-entry-id msg)
  (hash-ref (message-meta msg) 'parentEntryId #f))

(define (branch-entry-name msg)
  (hash-ref (message-meta msg) 'branchName #f))

;; tree-navigation-entry: marks navigation to a different branch
(define (make-tree-navigation-entry id from-entry-id target-entry-id)
  (make-message id
                from-entry-id
                'system
                'tree-navigation
                '()
                (current-seconds)
                (hasheq 'targetEntryId target-entry-id 'fromEntryId from-entry-id)))

(define (tree-navigation-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'tree-navigation)))

(define (tree-navigation-entry-target-entry-id msg)
  (hash-ref (message-meta msg) 'targetEntryId #f))

(define (tree-navigation-entry-from-entry-id msg)
  (hash-ref (message-meta msg) 'fromEntryId #f))

;; branch-summary-entry: LLM-generated summary of a branch
;; Note: branch-summary-entry? predicate already exists above, but
;; this constructor adds structured fields (summary, entry-range, token-count).
(define (make-branch-summary-entry id parent-id summary entry-range token-count)
  (make-message id
                parent-id
                'system
                'branch-summary
                '()
                (current-seconds)
                (hasheq 'summary summary 'entryRange entry-range 'tokenCount token-count)))

(define (branch-summary-entry-summary msg)
  (hash-ref (message-meta msg) 'summary #f))

(define (branch-summary-entry-entry-range msg)
  (hash-ref (message-meta msg) 'entryRange #f))

(define (branch-summary-entry-token-count msg)
  (hash-ref (message-meta msg) 'tokenCount #f))

;; tree-entry? — matches any tree entry type
(define (tree-entry? msg)
  (and (message? msg) (memq (message-kind msg) '(branch tree-navigation branch-summary)) #t))

;; ============================================================
;; Event envelope struct
;; ============================================================

(struct event (version ev time session-id turn-id payload) #:transparent)

(define (make-event ev time session-id turn-id payload #:version [version 1])
  (event version ev time session-id turn-id payload))

;; Accessor alias: the field is called `ev` internally to avoid
;; name collision with the struct, but the logical name is `event`.
;; Racket generates `event-ev`; we re-export as a procedure.
(define event-event event-ev)

;; Serialize event to jsexpr (hash)
(define (event->jsexpr evt)
  (hasheq 'version
          (event-version evt)
          'event
          (event-ev evt)
          'time
          (event-time evt)
          'sessionId
          (event-session-id evt)
          'turnId
          (event-turn-id evt) ; #f -> JSON null
          'payload
          (event-payload evt)))

;; Current event schema version produced by this runtime.
(define CURRENT-EVENT-VERSION 1)

;; Deserialize jsexpr (hash) to event.
(define (jsexpr->event h)
  (define ver (hash-ref h 'version 1))
  (when (> ver CURRENT-EVENT-VERSION)
    (log-warning "jsexpr->event: event version ~a exceeds current ~a (event: ~a)"
                 ver
                 CURRENT-EVENT-VERSION
                 (hash-ref h 'event "<unknown>")))
  (event ver
         (hash-ref h 'event)
         (hash-ref h 'time)
         (hash-ref h 'sessionId)
         (hash-ref h 'turnId)
         (hash-ref h 'payload)))

;; ============================================================
;; Loop-result struct
;; ============================================================

(struct loop-result (messages termination-reason metadata) #:transparent)

(define (make-loop-result messages termination-reason metadata)
  (loop-result messages termination-reason metadata))

;; ============================================================
;; Tool-call struct (standalone — canonical definition)
;; ============================================================

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

;; ============================================================
;; Tool-result struct (standalone — canonical definition)
;; ============================================================

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
