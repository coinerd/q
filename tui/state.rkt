#lang racket/base

;; tui/state.rkt — UI state and event→state reduction
;;
;; Pure functions only. No terminal I/O. No side effects.
;; The ui-state struct captures everything the TUI needs to render.

(require racket/string
         json
         "../util/protocol-types.rkt")

;; Structs
(provide (struct-out transcript-entry)
         (struct-out ui-state)
         (struct-out branch-info)

         ;; Constructors
         initial-ui-state

         ;; Branch management
         set-current-branch
         set-visible-branches
         clear-visible-branches

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

;; Constructor with defaults
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
            #f)) ; sel-end

;; Apply a runtime event to the UI state.
;; Returns a new ui-state (immutable update).
;; This is the core event→state reduction.
(define (apply-event-to-state state evt)
  ;; evt is an event struct from agent/types.rkt
  ;; (event version ev time session-id turn-id payload)
  (define ev (event-ev evt))
  (define payload (event-payload evt))
  (case ev
    [("assistant.message.completed")
     ;; Add assistant text to transcript, mark not busy
     ;; If we were streaming, use the streamed text as the transcript entry
     ;; (it's more complete than the payload content in some providers)
     (define streamed (ui-state-streaming-text state))
     (define content (or streamed (hash-ref payload 'content "")))
     (define ts (event-time evt))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'assistant content ts (hash))))]
                  [busy? #f]
                  [pending-tool-name #f]
                  [streaming-text #f])]

    [("tool.call.started")
     ;; Show tool started with arguments, mark busy
     (define name (hash-ref payload 'name "?"))
     (define args-raw (hash-ref payload 'arguments #f))
     (define arg-summary
       (if args-raw
           (extract-arg-summary args-raw)
           ""))
     (define text
       (if (string=? arg-summary "")
           (format "[TOOL: ~a]" name)
           (format "[TOOL: ~a] ~a" name arg-summary)))
     (define ts (event-time evt))
     (define meta (hasheq 'name name 'arguments (or args-raw "")))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'tool-start text ts meta)))]
                  [busy? #t]
                  [pending-tool-name name])]

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
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'tool-end text ts meta)))]
                  [pending-tool-name #f])]

    [("tool.call.failed")
     ;; Show tool error with error message
     (define name (hash-ref payload 'name "?"))
     (define err (hash-ref payload 'error "unknown"))
     (define ts (event-time evt))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'tool-fail
                                                   (format "[FAIL: ~a] ~a" name err)
                                                   ts
                                                   (hasheq 'name name 'error err))))]
                  [pending-tool-name #f])]

    [("runtime.error")
     (define err (hash-ref payload 'error "unknown error"))
     (define ts (event-time evt))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'error (format "Error: ~a" err) ts (hash))))]
                  [busy? #f])]

    [("session.started")
     (define sid (hash-ref payload 'sessionId ""))
     (struct-copy ui-state
                  state
                  [session-id sid]
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'system
                                                   (format "Session started: ~a" sid)
                                                   (event-time evt)
                                                   (hash))))])]

    [("session.resumed")
     (define sid (hash-ref payload 'sessionId ""))
     (struct-copy ui-state
                  state
                  [session-id sid]
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'system
                                                   (format "Session resumed: ~a" sid)
                                                   (event-time evt)
                                                   (hash))))])]

    [("model.stream.delta")
     ;; Accumulate streaming text from the model
     (define delta (hash-ref payload 'delta ""))
     (define current-streaming (ui-state-streaming-text state))
     (define new-streaming (string-append (or current-streaming "") delta))
     (struct-copy ui-state state [streaming-text new-streaming] [busy? #t])]

    ;; Agent starts processing — mark busy
    [("turn.started") (struct-copy ui-state state [busy? #t])]

    ;; Agent done processing — mark not busy
    [("turn.completed") (struct-copy ui-state state [busy? #f])]

    [("turn.cancelled")
     ;; Agent turn was cancelled — clear busy state
     (struct-copy ui-state state [busy? #f] [streaming-text #f] [pending-tool-name #f])]

    [("compaction.warning")
     (define tc (hash-ref payload 'tokenCount "?"))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'system
                                                   (format "[compaction warning: ~a tokens]" tc)
                                                   (event-time evt)
                                                   (hash))))])]

    [("session.forked")
     (define new-sid (hash-ref payload 'newSessionId ""))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'system
                                                   (format "[session forked: ~a]" new-sid)
                                                   (event-time evt)
                                                   (hash))))])]

    [("compaction.started") (struct-copy ui-state state [status-message "Compacting..."])]

    [("compaction.completed") (struct-copy ui-state state [status-message #f])]

    ;; Model request initiated — mark busy if not already
    [("model.request.started") (struct-copy ui-state state [busy? #t])]

    ;; Context was assembled — informational, keep state
    [("context.built") state]

    [("tool.call.blocked")
     (define name (hash-ref payload 'name "?"))
     (define reason (hash-ref payload 'reason "blocked by extension"))
     (struct-copy ui-state
                  state
                  [transcript
                   (append (ui-state-transcript state)
                           (list (transcript-entry 'system
                                                   (format "[tool blocked: ~a — ~a]" name reason)
                                                   (event-time evt)
                                                   (hasheq 'name name))))]
                  [pending-tool-name #f])]

    [else state])) ;; Ignore unknown events

;; Add a user message to transcript (called when user submits input)
(define (add-transcript-entry state entry)
  (struct-copy ui-state
               state
               [transcript (append (ui-state-transcript state) (list entry))]
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
(define (scroll-up state [amount 1])
  (define actual (+ (ui-state-scroll-offset state) amount))
  (struct-copy ui-state state [scroll-offset actual]))

;; Scroll down (see newer entries)
(define (scroll-down state [amount 1])
  (define new-offset (max 0 (- (ui-state-scroll-offset state) amount)))
  (struct-copy ui-state state [scroll-offset new-offset]))

;; Scroll to bottom (latest entries)
(define (scroll-to-bottom state)
  (struct-copy ui-state state [scroll-offset 0]))

;; Scroll to top (oldest entries)
;; Set a large scroll offset — the renderer clamps to max
(define (scroll-to-top state)
  (struct-copy ui-state state [scroll-offset 999999]))

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

;; Queries
(define (ui-busy? state)
  (ui-state-busy? state))

(define (ui-session-label state)
  (or (ui-state-session-id state) "no session"))

(define (ui-model-label state)
  (or (ui-state-model-name state) "no model"))

(define (ui-status-text state)
  (or (ui-state-status-message state)
      (if (ui-state-busy? state)
          (format "busy~a..."
                  (if (ui-state-pending-tool-name state)
                      (format " (~a)" (ui-state-pending-tool-name state))
                      ""))
          "idle")))

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
