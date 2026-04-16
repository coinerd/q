#lang racket/base

;; tui/state.rkt — UI state and event→state reduction
;;
;; Pure functions only. No terminal I/O. No side effects.
;; The ui-state struct captures everything the TUI needs to render.

(require racket/string
         racket/list
         json
         "../util/protocol-types.rkt")

;; Structs
(provide (struct-out transcript-entry)
         (struct-out ui-state)
         (struct-out branch-info)
         (struct-out overlay-state)

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

         ;; Branch management
         set-current-branch
         set-visible-branches
         clear-visible-branches

         ;; Overlay management (#643)
         show-overlay
         update-overlay-input
         dismiss-overlay
         overlay-active?
         ;; Overlay anchors (#725)
         ANCHOR-TOP-LEFT
         ANCHOR-CENTER
         ANCHOR-BOTTOM-RIGHT
         anchor?

         ;; Event reduction (pure)
         apply-event-to-state

         ;; Transcript helpers
         add-transcript-entry
         visible-entries
         scroll-up
         scroll-down
         scroll-to-bottom
         scroll-to-top

         ;; State queries
         ui-busy?
         ui-session-label
         ui-model-label
         ui-status-text

         ;; String helpers
         truncate-string
         extract-arg-summary

         ;; Widget management (#714)
         set-extension-widget
         remove-extension-widget
         remove-all-extension-widgets
         get-widget-lines-above
         get-widget-lines-below

         ;; Custom header/footer (#717)
         set-custom-header
         set-custom-footer
         clear-custom-header
         clear-custom-footer

         ;; Selection
         set-selection-anchor
         set-selection-end
         clear-selection
         has-selection?)

;; A single line in the transcript display
(struct transcript-entry
        (kind ; symbol: 'assistant | 'tool-start | 'tool-end | 'tool-fail | 'system | 'error | 'user
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
         )
  #:transparent)

;; Overlay state for modal/popup UI elements (command palette, etc.)
(struct overlay-state
        (type      ; symbol — 'command-palette | other overlay types
         content   ; (listof styled-line) — overlay render content
         input     ; string — current input for the overlay
         anchor    ; symbol — 'top-left | 'center | 'bottom-right (default 'top-left)
         width     ; (or/c integer? #f) — explicit width override
         height    ; (or/c integer? #f) — explicit height override
         margin    ; integer — margin around overlay (default 0)
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

;; Create a transcript-entry with id=#f (not yet assigned)
(define (make-entry kind text timestamp meta)
  (transcript-entry kind text timestamp meta #f))

;; Assign a unique id to an entry, returning (values new-entry new-state)
(define (assign-entry-id entry state)
  (define id (ui-state-next-entry-id state))
  (values (struct-copy transcript-entry entry [id id])
          (struct-copy ui-state state [next-entry-id (add1 id)])))

;; Get the current next-entry-id (without incrementing)
(define (next-entry-id state)
  (ui-state-next-entry-id state))

;; ============================================================
;; Render cache helpers
;; ============================================================

;; Look up cached rendered lines for an entry id
(define (rendered-cache-ref state entry-id)
  (hash-ref (ui-state-rendered-cache state) entry-id #f))

;; Store cached rendered lines for an entry id, returning new state
;; Maximum number of entries in the render cache
(define RENDER-CACHE-MAX-SIZE 100)

(define (rendered-cache-set state entry-id lines)
  (define old-cache (ui-state-rendered-cache state))
  (define new-cache (hash-set old-cache entry-id lines))
  ;; BUG-35 fix: evict oldest entries when cache exceeds limit
  (define pruned-cache
    (if (<= (hash-count new-cache) RENDER-CACHE-MAX-SIZE)
        new-cache
        ;; Remove oldest entries (lowest keys)
        (let ([sorted-keys (sort (hash-keys new-cache) <)])
          (for/fold ([c new-cache])
                    ([k (in-list (take sorted-keys (- (hash-count new-cache) RENDER-CACHE-MAX-SIZE)))])
            (hash-remove c k)))))
  (struct-copy ui-state state [rendered-cache pruned-cache]))

;; Clear the entire render cache and reset width
(define (rendered-cache-clear state)
  (struct-copy ui-state state [rendered-cache (hash)] [rendered-cache-width #f]))

;; Remove a single entry from the render cache
(define (rendered-cache-invalidate-entry state entry-id)
  (struct-copy ui-state
               state
               [rendered-cache (hash-remove (ui-state-rendered-cache state) entry-id)]))

;; Check if the cache width matches the given width
(define (rendered-cache-width-valid? state width)
  (equal? (ui-state-rendered-cache-width state) width))

;; Set the cache width (typically after clearing cache on resize)
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
            ))

;; ============================================================
;; Event reduction
;; ============================================================

;; Apply a runtime event to the UI state.
;; Returns a new ui-state (immutable update).
;; This is the core event→state reduction.
(define (apply-event-to-state state evt)
  ;; evt is an event struct from util/protocol-types.rkt
  ;; (event version ev time session-id turn-id payload)
  (define ev (event-ev evt))
  (define payload (event-payload evt))

  ;; Helper: create entry, assign id, append to transcript, return new state
  (define (append-entry st entry)
    (define-values (id-entry st1) (assign-entry-id entry st))
    (struct-copy ui-state st1 [transcript (append (ui-state-transcript st1) (list id-entry))]))

  (case ev
    [("assistant.message.completed")
     ;; Add assistant text to transcript, mark not busy
     ;; If we were streaming, use the streamed text as the transcript entry
     ;; (it's more complete than the payload content in some providers)
     (define streamed (ui-state-streaming-text state))
     (define content (or streamed (hash-ref payload 'content "")))
     (define ts (event-time evt))
     (struct-copy ui-state
                  (append-entry state (make-entry 'assistant content ts (hash)))
                  [busy? #f]
                  [pending-tool-name #f]
                  [streaming-text #f])]

    [("tool.call.started")
     ;; Show tool started with arguments, mark busy
     (let* ([name (hash-ref payload 'name "?")]
            [args-raw (hash-ref payload 'arguments #f)]
            [arg-summary (if args-raw (extract-arg-summary args-raw) "")]
            [text (if (string=? arg-summary "")
                      (format "[TOOL: ~a]" name)
                      (format "[TOOL: ~a] ~a" name arg-summary))]
            [ts (event-time evt)]
            [meta (hasheq 'name name 'arguments (or args-raw ""))]
            [new-state (append-entry state (make-entry 'tool-start text ts meta))])
       ;; BUG-38 fix: don't overwrite pending-tool-name if one is already set
       (if (ui-state-pending-tool-name state)
           (struct-copy ui-state new-state (busy? #t))
           (struct-copy ui-state new-state (busy? #t) (pending-tool-name name))))]

    [("tool.call.completed")
     ;; Show tool completed with result summary
     (define name (hash-ref payload 'name "?"))
     (define result-raw (hash-ref payload 'result #f))
     (define result-summary
       (if result-raw
           (truncate-string (format "~a" result-raw) 80)
           ""))
     (define text
       (if (string=? result-summary "")
           (format "[OK: ~a]" name)
           (format "[OK: ~a] ~a" name result-summary)))
     (define ts (event-time evt))
     (define meta (hasheq 'name name 'result (or result-raw "")))
     (struct-copy ui-state
                  (append-entry state (make-entry 'tool-end text ts meta))
                  [pending-tool-name #f])]

    [("tool.call.failed")
     ;; Show tool error with error message
     (define name (hash-ref payload 'name "?"))
     (define err (hash-ref payload 'error "unknown"))
     (define ts (event-time evt))
     (struct-copy
      ui-state
      (append-entry
       state
       (make-entry 'tool-fail (format "[FAIL: ~a] ~a" name err) ts (hasheq 'name name 'error err)))
      [pending-tool-name #f])]

    [("runtime.error")
     (define err (hash-ref payload 'error "unknown error"))
     (define ts (event-time evt))
     ;; BUG-29 fix: clear pending-tool-name and streaming-text on error
     (struct-copy ui-state
                  (append-entry state (make-entry 'error (format "Error: ~a" err) ts (hash)))
                  [busy? #f]
                  [pending-tool-name #f]
                  [streaming-text #f])]

    [("session.started")
     (define sid (hash-ref payload 'sessionId ""))
     (define s1 (struct-copy ui-state state [session-id sid]))
     (append-entry s1
                   (make-entry 'system (format "Session started: ~a" sid) (event-time evt) (hash)))]

    [("session.resumed")
     (define sid (hash-ref payload 'sessionId ""))
     (define s1 (struct-copy ui-state state [session-id sid]))
     (append-entry s1
                   (make-entry 'system (format "Session resumed: ~a" sid) (event-time evt) (hash)))]

    [("model.stream.delta")
     ;; Accumulate streaming text from the model
     (define delta (hash-ref payload 'delta ""))
     (define current-streaming (ui-state-streaming-text state))
     (define new-streaming (string-append (or current-streaming "") delta))
     ;; Invalidate cache for the streaming entry (if re-rendering)
     (struct-copy ui-state state [streaming-text new-streaming] [busy? #t])]

    ;; Agent starts processing — mark busy
    ;; BUG-30 fix: clear stale state from previous turn
    [("turn.started") (struct-copy ui-state state [busy? #t] [pending-tool-name #f] [streaming-text #f])]

    ;; Agent done processing — mark not busy, clear streaming-text
    ;; Bug B2 fix: clear streaming-text on turn.completed as defense-in-depth
    ;; so stale streaming text doesn't contaminate next turn.
    ;; BUG-31 fix: also clear pending-tool-name as defense-in-depth
    [("turn.completed") (struct-copy ui-state state [busy? #f] [streaming-text #f] [pending-tool-name #f])]

    [("turn.cancelled")
     ;; Agent turn was cancelled — clear busy state
     (struct-copy ui-state state [busy? #f] [streaming-text #f] [pending-tool-name #f])]

    [("compaction.warning")
     (define tc (hash-ref payload 'tokenCount "?"))
     (append-entry
      state
      (make-entry 'system (format "[compaction warning: ~a tokens]" tc) (event-time evt) (hash)))]

    [("session.forked")
     (define new-sid (hash-ref payload 'newSessionId ""))
     (append-entry
      state
      (make-entry 'system (format "[session forked: ~a]" new-sid) (event-time evt) (hash)))]

    [("compaction.started") (struct-copy ui-state state [status-message "Compacting..."])]
    ;; BUG-32 fix: runtime emits compaction.start (not compaction.started)
    [("compaction.start") (struct-copy ui-state state [status-message "Compacting..."])]

    [("compaction.completed") (struct-copy ui-state state [status-message #f])]
    ;; BUG-32 fix: runtime emits compaction.end (not compaction.completed)
    [("compaction.end") (struct-copy ui-state state [status-message #f])]

    ;; Model request initiated — mark busy if not already
    [("model.request.started") (struct-copy ui-state state [busy? #t])]

    ;; Context was assembled — informational, keep state
    [("context.built") state]

    [("tool.call.blocked")
     (define name (hash-ref payload 'name "?"))
     (define reason (hash-ref payload 'reason "blocked by extension"))
     (struct-copy ui-state
                  (append-entry state
                                (make-entry 'system
                                            (format "[tool blocked: ~a — ~a]" name reason)
                                            (event-time evt)
                                            (hasheq 'name name)))
                  [pending-tool-name #f])]

    [("queue.status-update")
     (struct-copy ui-state state [queue-counts payload])]

    ;; BUG-33 fix: auto-retry event from runtime/iteration.rkt
    [("auto-retry.start")
     (define attempt (hash-ref payload 'attempt "?"))
     (define max-attempts (hash-ref payload 'maxAttempts "?"))
     (append-entry
      state
      (make-entry 'system
                  (format "[retry: attempt ~a/~a]" attempt max-attempts)
                  (event-time evt) (hash)))]

    ;; BUG-34 fix: model.stream.completed clears streaming text
    [("model.stream.completed")
     (struct-copy ui-state state [streaming-text #f])]

    [else state])) ;; Ignore unknown events

;; ============================================================
;; Transcript helpers
;; ============================================================

;; Add a user message to transcript (called when user submits input)
;; Assigns an entry id if the entry doesn't already have one.
(define (add-transcript-entry state entry)
  (define-values (id-entry state1)
    (if (transcript-entry-id entry)
        (values entry state)
        (assign-entry-id entry state)))
  (struct-copy ui-state
               state1
               [transcript (append (ui-state-transcript state1) (list id-entry))]
               [scroll-offset 0])) ;; Reset scroll to bottom

;; Get visible entries — returns all transcript entries.
;; Line-based slicing is done by the renderer (render.rkt)
;; which knows the width and can compute actual rendered line counts.
;; scroll-offset is now line-based (not entry-based).
(define (visible-entries state transcript-height)
  (ui-state-transcript state))

;; Scroll up (see older entries)
;; scroll-offset counts rendered LINES from the bottom.
;; No upper clamp here — render-transcript clamps via max(0, ...).
;; Selection is screen-relative — clear it when scrolling to avoid stale highlights.
(define (scroll-up state [amount 1])
  (define actual (+ (ui-state-scroll-offset state) amount))
  (define next (struct-copy ui-state state [scroll-offset actual]))
  (if (has-selection? next) (clear-selection next) next))

;; Scroll down (see newer entries)
(define (scroll-down state [amount 1])
  (define new-offset (max 0 (- (ui-state-scroll-offset state) amount)))
  (define next (struct-copy ui-state state [scroll-offset new-offset]))
  (if (has-selection? next) (clear-selection next) next))

;; Scroll to bottom (latest entries)
(define (scroll-to-bottom state)
  (struct-copy ui-state state [scroll-offset 0]))

;; Scroll to top (oldest entries)
;; Set a large scroll offset — the renderer clamps to max
(define (scroll-to-top state)
  (struct-copy ui-state state [scroll-offset 999999]))

;; ============================================================
;; Selection
;; ============================================================

;; Set mouse selection anchor (start of drag)
(define (set-selection-anchor state col row)
  (struct-copy ui-state state [sel-anchor (cons col row)] [sel-end (cons col row)]))

;; Update mouse selection end (during drag)
(define (set-selection-end state col row)
  (struct-copy ui-state state [sel-end (cons col row)]))

;; Clear mouse selection
(define (clear-selection state)
  (struct-copy ui-state state [sel-anchor #f] [sel-end #f]))

;; Check if there is an active selection
(define (has-selection? state)
  (and (ui-state-sel-anchor state) (ui-state-sel-end state)))

;; ============================================================
;; Queries
;; ============================================================

(define (ui-busy? state)
  (ui-state-busy? state))

(define (ui-session-label state)
  (or (ui-state-session-id state) "no session"))

(define (ui-model-label state)
  (or (ui-state-model-name state) "no model"))

(define (ui-status-text state)
  (or (ui-state-status-message state)
      (if (ui-state-busy? state)
          (format "busy~a...~a"
                  (if (ui-state-pending-tool-name state)
                      (format " (~a)" (ui-state-pending-tool-name state))
                      "")
                  (queue-status-suffix (ui-state-queue-counts state)))
          (format "idle~a" (queue-status-suffix (ui-state-queue-counts state))))))

;; Format queue counts as status suffix
(define (queue-status-suffix counts)
  (if (not counts)
      ""
      (let ([steering (hash-ref counts 'steering 0)]
            [followup (hash-ref counts 'followup 0)])
        (cond
          [(and (> steering 0) (> followup 0))
           (format " | \u2691~a steer, ~a follow" steering followup)]
          [(> steering 0)
           (format " | \u2691~a steer" steering)]
          [(> followup 0)
           (format " | \u2691~a follow" followup)]
          [else ""]))))

;; ============================================================
;; Branch management helpers
;; ============================================================

(define (set-current-branch state branch-id)
  ;; Set the current branch node id
  (struct-copy ui-state state [current-branch branch-id]))

(define (set-visible-branches state branches)
  ;; Set the cached list of branch-info structs
  (struct-copy ui-state state [visible-branches branches]))

(define (clear-visible-branches state)
  ;; Clear the cached branch list
  (struct-copy ui-state state [visible-branches '()]))

;; ============================================================
;; Overlay anchors (#725)
;; ============================================================

(define ANCHOR-TOP-LEFT 'top-left)
(define ANCHOR-CENTER 'center)
(define ANCHOR-BOTTOM-RIGHT 'bottom-right)

(define (anchor? v)
  (and (symbol? v)
       (or (eq? v ANCHOR-TOP-LEFT)
           (eq? v ANCHOR-CENTER)
           (eq? v ANCHOR-BOTTOM-RIGHT))))

;; ============================================================
;; Overlay helpers (#643)
;; ============================================================

(define (show-overlay state type content [input ""] #:anchor [anchor ANCHOR-TOP-LEFT]
                                                          #:width [width #f]
                                                          #:height [height #f]
                                                          #:margin [margin 0])
  ;; Show an overlay with the given type, content lines, input, and positioning
  (struct-copy ui-state state
               [active-overlay (overlay-state type content input anchor width height margin)]))

(define (update-overlay-input state input)
  ;; Update the overlay input text (e.g., as user types in palette)
  (define ov (ui-state-active-overlay state))
  (if ov
      (struct-copy ui-state state
                   [active-overlay (struct-copy overlay-state ov [input input])])
      state))

(define (dismiss-overlay state)
  ;; Dismiss the active overlay
  (struct-copy ui-state state [active-overlay #f]))

(define (overlay-active? state)
  ;; Check if an overlay is currently active
  (and (ui-state-active-overlay state) #t))

;; ============================================================
;; Extension widget helpers (#714)
;; ============================================================

;; Set a widget for an extension. Key is a string identifier.
;; lines is (listof styled-line). Returns new ui-state.
(define (set-extension-widget state ext-name key lines)
  (define widgets (ui-state-extension-widgets state))
  (define widget-key (cons ext-name key))
  (struct-copy ui-state state
               [extension-widgets (hash-set widgets widget-key lines)]))

;; Remove a specific widget by extension name and key.
(define (remove-extension-widget state ext-name key)
  (define widgets (ui-state-extension-widgets state))
  (define widget-key (cons ext-name key))
  (struct-copy ui-state state
               [extension-widgets (hash-remove widgets widget-key)]))

;; Remove all widgets for a given extension (for unload/disposal).
(define (remove-all-extension-widgets state ext-name)
  (define widgets (ui-state-extension-widgets state))
  (define filtered
    (for/hash ([(k v) (in-hash widgets)]
               #:unless (equal? (car k) ext-name))
      (values k v)))
  (struct-copy ui-state state [extension-widgets filtered]))

;; Get all widget lines for placement above input.
;; Returns (listof styled-line) combining all widgets.
(define (get-widget-lines-above state)
  (define widgets (ui-state-extension-widgets state))
  (apply append (hash-values widgets)))

;; Get all widget lines for placement below input (placeholder for future).
(define (get-widget-lines-below state)
  ;; Currently all widgets render above input.
  ;; Future: support #:placement 'below per widget.
  '())

;; ============================================================
;; Custom header/footer (#717)
;; ============================================================

(define (set-custom-header state lines)
  (struct-copy ui-state state [custom-header lines]))

(define (set-custom-footer state lines)
  (struct-copy ui-state state [custom-footer lines]))

(define (clear-custom-header state)
  (struct-copy ui-state state [custom-header #f]))

(define (clear-custom-footer state)
  (struct-copy ui-state state [custom-footer #f]))

;; ============================================================
;; String helpers
;; ============================================================

;; Truncate a string to max-len characters, appending … if truncated
(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 1)) "…")))

;; Extract a readable summary from tool arguments JSON string
;; Try to parse JSON and show the first value, fallback to first 60 chars
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
