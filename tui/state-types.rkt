#lang racket/base

;; tui/state-types.rkt — UI state structs, constructors, entry/cache helpers
;;
;; TUI state types, render cache management, and entry helpers.
;; Contains both data definitions and cache/entry utility logic.
;; Split from state.rkt (v0.22.6 W2) to keep each module under 400 lines.

(require racket/string
         racket/list
         json
         "../util/cost-tracker.rkt")

(provide (struct-out transcript-entry)
         (struct-out ui-state)
         (struct-out branch-info)
         (struct-out overlay-state)
         (struct-out tree-browser-state)

         ;; Constructors
         initial-ui-state

         ;; Entry helpers
         make-entry
         assign-entry-id
         next-entry-id

         ;; Render cache helpers
         rendered-cache-ref
         rendered-cache-set
         rendered-cache-clear
         rendered-cache-invalidate-entry
         rendered-cache-width-valid?
         rendered-cache-set-width

         ;; String helpers
         truncate-string
         extract-arg-summary)

;; A single line in the transcript display
(struct transcript-entry
        (kind ; symbol: 'assistant | 'tool-start | 'tool-end | 'tool-fail | 'system | 'error | 'user | 'thinking
         text ; string — the display text
         timestamp ; number — epoch seconds (or 0)
         meta ; hash — extra data (tool name, error message, etc.)
         id ; integer or #f — unique entry id for render cache
         )
  #:transparent)

;; The complete UI state
(struct ui-state
        (transcript ; (listof transcript-entry) — newest LAST
         scroll-offset ; integer — 0 = bottom, positive = scrolled up
         busy? ; boolean — is the agent currently processing?
         session-id ; string or #f
         model-name ; string or #f
         mode ; symbol: 'chat | 'single | etc.
         status-message ; string or #f — temporary status
         pending-tool-name ; string or #f — name of tool currently executing
         streaming-text ; string or #f — partial streaming text (during model.stream.delta)
         streaming-thinking ; string or #f — accumulated thinking text (during model.stream.thinking)
         current-branch ; string or #f — current branch node id
         visible-branches ; (listof branch-info) — cached branch list for display
         sel-anchor ; (cons col row) or #f — mouse selection start
         sel-end ; (cons col row) or #f — mouse selection end
         rendered-cache ; hash — maps entry-id → (listof styled-line)
         rendered-cache-width ; integer or #f — width used for cache
         next-entry-id ; integer — monotonic counter
         active-overlay ; (or/c #f overlay-state) — currently displayed overlay
         queue-counts ; hash or #f — steering/followup counts from queue.status-update
         extension-widgets ; hash — maps (cons ext-name key) → (listof styled-line)
         custom-header ; (or/c #f (listof styled-line)) — extension-provided header
         custom-footer ; (or/c #f (listof styled-line)) — extension-provided footer
         mock-provider? ; boolean — #t when using mock/fallback provider (BUG-55)
         focused-component ; (or/c #f symbol?) — id of component with focus
         editor-component ; (or/c #f q-component?) — custom editor for input area (#1150)
         context-tokens ; (or/c #f integer?) — estimated token count from context events (v0.19.12 W1)
         cost-tracker ; (or/c #f cost-tracker?) — mutable cost accumulator (G8.4)
         busy-since)
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
  (hash-ref (ui-state-rendered-cache state) entry-id #f))

(define (rendered-cache-set state entry-id lines)
  (define old-cache (ui-state-rendered-cache state))
  (define new-cache (hash-set old-cache entry-id lines))
  (define pruned-cache
    (if (<= (hash-count new-cache) RENDER-CACHE-MAX-SIZE)
        new-cache
        (let ([sorted-keys (sort (hash-keys new-cache) <)])
          (for/fold ([c new-cache])
                    ([k (in-list (take sorted-keys
                                       (- (hash-count new-cache) RENDER-CACHE-MAX-SIZE)))])
            (hash-remove c k)))))
  (struct-copy ui-state state [rendered-cache pruned-cache]))

(define (rendered-cache-clear state)
  (struct-copy ui-state state [rendered-cache (hash)] [rendered-cache-width #f]))

(define (rendered-cache-invalidate-entry state entry-id)
  (struct-copy ui-state
               state
               [rendered-cache (hash-remove (ui-state-rendered-cache state) entry-id)]))

(define (rendered-cache-width-valid? state width)
  (equal? (ui-state-rendered-cache-width state) width))

(define (rendered-cache-set-width state width)
  (struct-copy ui-state state [rendered-cache-width width]))

;; ============================================================
;; Constructor with defaults
;; ============================================================

(define (initial-ui-state #:session-id [session-id #f]
                          #:model-name [model-name #f]
                          #:mode [mode 'chat])
  (ui-state '() ; transcript
            0 ; scroll-offset
            #f ; busy?
            session-id
            model-name
            mode
            #f ; status-message
            #f ; pending-tool-name
            #f ; streaming-text
            #f ; streaming-thinking
            #f ; current-branch
            '() ; visible-branches
            #f ; sel-anchor
            #f ; sel-end
            (hash) ; rendered-cache
            #f ; rendered-cache-width
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
            (make-cost-tracker) ; cost-tracker
            #f))

;; ============================================================
;; String helpers
;; ============================================================

(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 1)) "…")))

(define (extract-arg-summary args-str)
  (with-handlers ([exn:fail? (lambda (_) (truncate-string args-str 60))])
    (define h (read-json (open-input-string args-str)))
    (cond
      [(hash? h)
       (define vals (hash-values h))
       (if (null? vals)
           "(no args)"
           (truncate-string (format "~a" (car vals)) 60))]
      [else (truncate-string (format "~a" h) 60)])))
