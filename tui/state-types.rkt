#lang racket/base

;; tui/state-types.rkt — UI state structs, constructors, entry/cache helpers
;;
;; TUI state types, render cache management, and entry helpers.
;; Contains both data definitions and cache/entry utility logic.
;; Split from state.rkt (v0.22.6 W2) to keep each module under 400 lines.

(require racket/contract
         racket/set
         racket/string
         racket/list
         json
         "../util/cost-tracker.rkt"
         "../util/string-helpers.rkt")

;; Structs that need struct-copy support in consumers must use struct-out.
;; We add contracts on functions only.
(provide transcript-entry
         transcript-entry?
         transcript-entry-kind
         transcript-entry-text
         transcript-entry-timestamp
         transcript-entry-meta
         transcript-entry-id
         ui-state
         ui-state?
         ui-state-transcript
         ui-state-scroll-offset
         ui-state-session-id
         ui-state-model-name
         ui-state-mode
         ui-state-streaming
         ui-state-current-branch
         ui-state-visible-branches
         ui-state-selection
         ui-state-cache
         ui-state-next-entry-id
         ui-state-active-overlay
         ui-state-queue-counts
         ui-state-extension-widgets
         ui-state-custom-header
         ui-state-custom-footer
         ui-state-mock-provider?
         ui-state-focused-component
         ui-state-editor-component
         ui-state-context-tokens
         ui-state-cost-tracker
         streaming-state
         streaming-state?
         streaming-state-busy?
         streaming-state-status-message
         streaming-state-pending-tool-name
         streaming-state-streaming-text
         streaming-state-streaming-thinking
         streaming-state-busy-since
         overlay-state
         overlay-state?
         overlay-state-type
         overlay-state-content
         overlay-state-input
         overlay-state-anchor
         overlay-state-width
         overlay-state-height
         overlay-state-margin
         overlay-state-extra
         branch-info
         branch-info?
         branch-info-id
         branch-info-parent-id
         branch-info-role
         branch-info-leaf?
         branch-info-active?

         ;; These structs are not used with struct-copy externally,
         ;; so we export via contract-out for stronger guarantees.
         (contract-out
          [selection-state? (-> any/c boolean?)]
          [selection-state
           (-> (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f)
               (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f)
               selection-state?)]
          [selection-state-anchor
           (-> selection-state?
               (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f))]
          [selection-state-end
           (-> selection-state?
               (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f))]
          [cache-state? (-> any/c boolean?)]
          [cache-state
           (-> hash?
               (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f)
               cache-state?)]
          [cache-state-entries (-> cache-state? hash?)]
          [cache-state-width
           (-> cache-state? (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f))]
          [tree-browser-state? (-> any/c boolean?)]
          [tree-browser-state
           (->* ((listof (list/c any/c any/c symbol? string? number?)) exact-integer?
                                                                       set?
                                                                       (listof string?))
                ()
                tree-browser-state?)]
          [tree-browser-state-nodes
           (-> tree-browser-state? (listof (list/c any/c any/c symbol? string? number?)))]
          [tree-browser-state-selected-idx (-> tree-browser-state? exact-integer?)]
          [tree-browser-state-folded-set (-> tree-browser-state? set?)]
          [tree-browser-state-rendered-lines (-> tree-browser-state? (listof string?))]
          ;; Constructors
          [initial-ui-state
           (->* ()
                (#:session-id (or/c string? #f) #:model-name (or/c string? #f) #:mode symbol?)
                ui-state?)]
          ;; Entry helpers
          [make-entry (-> symbol? string? number? hash? transcript-entry?)]
          [make-system-entry (-> string? transcript-entry?)]
          [make-error-entry (-> string? transcript-entry?)]
          [assign-entry-id (-> transcript-entry? ui-state? (values transcript-entry? ui-state?))]
          [next-entry-id (-> ui-state? exact-nonnegative-integer?)]
          ;; Render cache helpers
          [rendered-cache-ref (-> ui-state? any/c (or/c (listof styled-line?) #f))]
          [rendered-cache-set (-> ui-state? any/c (listof styled-line?) ui-state?)]
          [rendered-cache-clear (-> ui-state? ui-state?)]
          [rendered-cache-invalidate-entry (-> ui-state? any/c ui-state?)]
          [rendered-cache-width-valid? (-> ui-state? exact-nonnegative-integer? boolean?)]
          [rendered-cache-set-width (-> ui-state? exact-nonnegative-integer? ui-state?)]
          [ui-state-rendered-cache (-> ui-state? hash?)]
          [ui-state-rendered-cache-width
           (-> ui-state? (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f))]
          ;; Backward-compatible streaming accessors (v0.38.6 migration)
          [ui-state-busy? (-> ui-state? boolean?)]
          [ui-state-status-message (-> ui-state? (or/c string? #f))]
          [ui-state-pending-tool-name (-> ui-state? (or/c string? #f))]
          [ui-state-streaming-text (-> ui-state? (or/c string? #f))]
          [ui-state-streaming-thinking (-> ui-state? (or/c string? #f))]
          [ui-state-streaming-phase (-> ui-state? symbol?)]
          [ui-state-busy-since (-> ui-state? any/c)]
          ;; Streaming update helpers
          [update-streaming (-> ui-state? (-> streaming-state? streaming-state?) ui-state?)]
          [set-busy (-> ui-state? boolean? ui-state?)]
          [set-status-message (-> ui-state? (or/c string? #f) ui-state?)]
          [set-pending-tool-name (-> ui-state? (or/c string? #f) ui-state?)]
          [set-streaming-text (-> ui-state? (or/c string? #f) ui-state?)]
          [set-streaming-thinking (-> ui-state? (or/c string? #f) ui-state?)]
          [set-streaming-phase (-> ui-state? symbol? ui-state?)]
          [set-busy-since (-> ui-state? any/c ui-state?)]
          [clear-streaming (-> ui-state? ui-state?)]
          ;; String helpers
          [truncate-string (-> string? exact-nonnegative-integer? string?)]
          [extract-arg-summary (-> (or/c string? hash?) string?)])
         ui-state-context-pressure-level
         ui-state-context-pressure-percent)

;; Forward declarations for types referenced in contracts but defined elsewhere
;; (These are provided by other modules and re-exported through state.rkt)
(define styled-line? any/c) ;; placeholder — actual definition is in render/message-layout.rkt
(define q-component? any/c) ;; placeholder — actual definition is in component.rkt

;; A single line in the transcript display
(struct transcript-entry
        (kind ; symbol: 'assistant | 'tool-start | 'tool-end | 'tool-fail | 'system | 'error | 'user | 'thinking
         text ; string — the display text
         timestamp ; number — epoch seconds (or 0)
         meta ; hash — extra data (tool name, error message, etc.)
         id ; integer or #f — unique entry id for render cache
         )
  #:transparent)

;; Selection state -- cursor position and selection range
(struct selection-state (anchor end) #:transparent)

;; Cache state -- rendered content cache
(struct cache-state (entries width) #:transparent)

;; Streaming state — turn activity / partial output group
(struct streaming-state
        (busy? ; boolean — is the agent currently processing?
         status-message ; string or #f — temporary status
         pending-tool-name ; string or #f — name of tool currently executing
         streaming-text ; string or #f — partial streaming text
         streaming-thinking ; string or #f — accumulated thinking text
         busy-since ; any/c — timestamp when busy started
         streaming-phase) ; symbol: idle | thinking | streaming | tool-pending (T2-8)
  #:transparent)

;; The complete UI state (21 fields, grouped by domain)
;;
;; Field groups (v0.38.6 documentation):
;;   Transcript:  transcript, scroll-offset, next-entry-id
;;   Streaming:   streaming (streaming-state)
;;   Selection:   selection (selection-state)
;;   Branch:      current-branch, visible-branches
;;   Cache:       cache (cache-state)
;;   Overlay:     active-overlay
;;   Extension:   extension-widgets, custom-header, custom-footer
;;   Input:       focused-component, editor-component
;;   Session:     session-id, model-name, mode, mock-provider?
;;   Queue:       queue-counts
;;   Budget:      context-tokens, cost-tracker
;;
;; --- Transcript group ---
(struct ui-state
        (transcript ; (listof transcript-entry) — newest FIRST (cons front)
         scroll-offset ; integer — 0 = bottom, positive = scrolled up
         ;; --- Session group ---
         session-id ; string or #f
         model-name ; string or #f
         mode ; symbol: 'chat | 'single | etc.
         ;; --- Streaming group ---
         streaming ; streaming-state — turn activity / partial output
         ;; --- Branch group ---
         current-branch ; string or #f — current branch node id
         visible-branches ; (listof branch-info) — cached branch list for display
         ;; --- Selection group ---
         selection ; selection-state -- cursor position and selection range
         ;; --- Cache group ---
         cache ; cache-state -- rendered content cache
         ;; --- Transcript group (cont.) ---
         next-entry-id ; integer — monotonic counter
         ;; --- Overlay group ---
         active-overlay ; (or/c #f overlay-state) — currently displayed overlay
         ;; --- Queue group ---
         queue-counts ; hash or #f — steering/followup counts from queue.status-update
         ;; --- Extension group ---
         extension-widgets ; hash — maps (cons ext-name key) → (listof styled-line)
         custom-header ; (or/c #f (listof styled-line)) — extension-provided header
         custom-footer ; (or/c #f (listof styled-line)) — extension-provided footer
         ;; --- Session group (cont.) ---
         mock-provider? ; boolean — #t when using mock/fallback provider (BUG-55)
         ;; --- Input group ---
         focused-component ; (or/c #f symbol?) — id of component with focus
         editor-component ; (or/c #f q-component?) — custom editor for input area (#1150)
         ;; --- Budget group ---
         context-tokens ; (or/c #f integer?) — estimated token count from context events (v0.19.12 W1)
         cost-tracker
         context-pressure-level
         context-pressure-percent) ; (or/c #f cost-tracker?) — mutable cost accumulator (G8.4)
  #:transparent)

;; Overlay state for modal/popup UI elements (command palette, etc.)
(struct overlay-state
        (type ; symbol — 'command-palette | other overlay types
         content ; (listof styled-line) — overlay render content
         input ; string — current input for the overlay
         anchor ; symbol — 'top-left | 'center | 'bottom-right (default 'top-left)
         width ; (or/c integer? #f) — explicit width override
         height ; (or/c integer? #f) — explicit height override
         margin ; integer — margin around overlay (default 0)
         extra)
  #:transparent)

;; Tree browser state for interactive tree navigation
(struct tree-browser-state
        (nodes ; (listof (list id parent-id role text timestamp)) — flat entry list
         selected-idx ; integer — currently selected node index in rendered lines
         folded-set ; set? — set of node IDs that are collapsed
         rendered-lines ; (listof string) — cached rendered lines
         )
  #:transparent)

;; Branch info struct for displaying branches
(struct branch-info
        (id ; string — branch node id
         parent-id ; string or #f — parent node id
         role ; symbol — 'user | 'assistant | 'system | 'tool
         leaf? ; boolean — is this a leaf node?
         active? ; boolean — is this the current active branch?
         )
  #:transparent)

;; ============================================================
;; Entry construction helpers
;; ============================================================

(define (make-entry kind text timestamp meta)
  (transcript-entry kind text timestamp meta #f))

;; L-01: Convenience constructors for common entry patterns
(define (make-system-entry text)
  (make-entry 'system text (current-inexact-milliseconds) (hash)))

(define (make-error-entry text)
  (make-entry 'error text (current-inexact-milliseconds) (hash)))

(define (assign-entry-id entry state)
  (define id (ui-state-next-entry-id state))
  (values (struct-copy transcript-entry entry [id id])
          (struct-copy ui-state state [next-entry-id (add1 id)])))

(define (next-entry-id state)
  (ui-state-next-entry-id state))

;; ============================================================
;; Render cache helpers
;; ============================================================

(define RENDER-CACHE-MAX-SIZE 100)

(define (rendered-cache-ref state entry-id)
  (hash-ref (cache-state-entries (ui-state-cache state)) entry-id #f))

(define (rendered-cache-set state entry-id lines)
  (define old-cache (cache-state-entries (ui-state-cache state)))
  (define new-cache (hash-set old-cache entry-id lines))
  (define pruned-cache
    (if (<= (hash-count new-cache) RENDER-CACHE-MAX-SIZE)
        new-cache
        (let ([sorted-keys (sort (hash-keys new-cache) <)])
          (for/fold ([c new-cache])
                    ([k (in-list (take sorted-keys
                                       (- (hash-count new-cache) RENDER-CACHE-MAX-SIZE)))])
            (hash-remove c k)))))
  (struct-copy ui-state
               state
               [cache (cache-state pruned-cache (cache-state-width (ui-state-cache state)))]))

(define (rendered-cache-clear state)
  (struct-copy ui-state state [cache (cache-state (hash) #f)]))

(define (rendered-cache-invalidate-entry state entry-id)
  (struct-copy ui-state
               state
               [cache
                (cache-state (hash-remove (cache-state-entries (ui-state-cache state)) entry-id)
                             (cache-state-width (ui-state-cache state)))]))

(define (rendered-cache-width-valid? state width)
  (equal? (cache-state-width (ui-state-cache state)) width))

(define (rendered-cache-set-width state width)
  (struct-copy ui-state
               state
               [cache (cache-state (cache-state-entries (ui-state-cache state)) width)]))

;; Backward-compatible cache accessors (v0.38.6: cache-state sub-struct)
(define (ui-state-rendered-cache state)
  (cache-state-entries (ui-state-cache state)))

(define (ui-state-rendered-cache-width state)
  (cache-state-width (ui-state-cache state)))

;; ============================================================
;; Constructor with defaults
;; ============================================================

(define (initial-ui-state #:session-id [session-id #f]
                          #:model-name [model-name #f]
                          #:mode [mode 'chat])
  (ui-state '() ; transcript
            0 ; scroll-offset
            session-id
            model-name
            mode
            (streaming-state #f #f #f #f #f #f 'idle) ; streaming
            #f ; current-branch
            '() ; visible-branches
            (selection-state #f #f)
            (cache-state (hash) #f)
            0 ; next-entry-id
            #f ; active-overlay
            #f ; queue-counts
            (hash) ; extension-widgets
            #f ; custom-header
            #f ; custom-footer
            #f ; mock-provider?
            #f ; focused-component
            #f ; editor-component
            #f ; context-tokens
            (make-cost-tracker)
            #f
            #f))

;; ============================================================
;; Backward-compatible streaming accessors (v0.38.6)
;; ============================================================

(define (ui-state-busy? state)
  (streaming-state-busy? (ui-state-streaming state)))

(define (ui-state-status-message state)
  (streaming-state-status-message (ui-state-streaming state)))

(define (ui-state-pending-tool-name state)
  (streaming-state-pending-tool-name (ui-state-streaming state)))

(define (ui-state-streaming-text state)
  (streaming-state-streaming-text (ui-state-streaming state)))

(define (ui-state-streaming-thinking state)
  (streaming-state-streaming-thinking (ui-state-streaming state)))

(define (ui-state-busy-since state)
  (streaming-state-busy-since (ui-state-streaming state)))

(define (ui-state-streaming-phase state)
  (streaming-state-streaming-phase (ui-state-streaming state)))

;; ============================================================
;; Streaming update helpers
;; ============================================================

(define (update-streaming state f)
  (struct-copy ui-state state [streaming (f (ui-state-streaming state))]))

(define (set-streaming-phase state phase)
  (struct-copy ui-state
               state
               [streaming
                (struct-copy streaming-state (ui-state-streaming state) [streaming-phase phase])]))

(define (set-busy state busy?)
  (update-streaming state (lambda (s) (struct-copy streaming-state s [busy? busy?]))))

(define (set-status-message state msg)
  (update-streaming state (lambda (s) (struct-copy streaming-state s [status-message msg]))))

(define (set-pending-tool-name state name)
  (update-streaming state (lambda (s) (struct-copy streaming-state s [pending-tool-name name]))))

(define (set-streaming-text state text)
  (update-streaming state (lambda (s) (struct-copy streaming-state s [streaming-text text]))))

(define (set-streaming-thinking state thinking)
  (update-streaming state (lambda (s) (struct-copy streaming-state s [streaming-thinking thinking]))))

(define (set-busy-since state since)
  (update-streaming state (lambda (s) (struct-copy streaming-state s [busy-since since]))))

(define (clear-streaming state)
  (update-streaming state
                    (lambda (s)
                      (struct-copy streaming-state s [streaming-text #f] [streaming-thinking #f]))))

;; ============================================================
;; String helpers
;; ============================================================

;; truncate-string — now canonical in util/string-helpers.rkt

(define (extract-arg-summary args-str)
  (define (summarize-hash h)
    (define vals (hash-values h))
    (if (null? vals)
        "(no args)"
        (truncate-string (format "~a" (car vals)) 60)))
  (cond
    [(hash? args-str) (summarize-hash args-str)]
    [(string? args-str)
     (with-handlers ([exn:fail? (lambda (_) (truncate-string args-str 60))])
       (define h (read-json (open-input-string args-str)))
       (cond
         [(hash? h) (summarize-hash h)]
         [else (truncate-string (format "~a" h) 60)]))]
    [else (truncate-string (format "~a" args-str) 60)]))
