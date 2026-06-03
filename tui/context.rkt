#lang racket/base

;; q/tui/context.rkt — TUI context struct, constructor, and dirty flag
;;
;; Extracted from tui-keybindings.rkt (W15) to reduce hotspot complexity.
;; This is a leaf module with minimal dependencies.

(require racket/contract
         racket/async-channel
         "state.rkt"
         "input.rkt"
         "../agent/event-bus.rkt"
         (only-in "../runtime/provider/model-registry.rkt" model-registry?)
         (only-in "../extensions/api.rkt" extension-registry?)
         (only-in "../agent/queue.rkt" queue?))

;; ── tui-ctx struct ──
;; Holds mutable references for the running TUI
(struct tui-ctx
        (ui-state-box ; (boxof ui-state)
         input-state-box ; (boxof input-state)
         event-bus ; event-bus? or #f
         session-runner ; (string -> void) — called with user prompts
         running-box ; (boxof boolean) — set to #f to exit
         event-ch ; async-channel — serializes runtime events into main loop (unbounded buffer)
         session-dir ; (or/c path-string? #f) — session directory for index loading
         needs-redraw-box ; (boxof boolean) — #t when state changed and frame needs redraw
         term-box ; (boxof any) — terminal instance
         ubuf-box ; (boxof any) — ubuf buffer for output
         model-registry-box ; (boxof (or/c model-registry? #f)) — model registry for /model
         previous-frame-box ; (boxof (or/c (listof string) #f)) — last rendered frame for diffing
         prev-ubuf-box ; (boxof (or/c any/c #f)) — previous cell buffer for cell-level diffing
         last-prompt-box ; (boxof (or/c string? #f)) — last user prompt for /retry
         extension-registry-box ; (or/c (boxof (or/c extension-registry? #f)) #f)
         session-queue-box ; (boxof (or/c queue? #f)) — agent queue for followup during streaming (G3.1)
         session-factory-runner ; (or/c (string -> void) #f) — creates fresh session for /go
         component-registry-box ; (boxof (or/c (hash/c symbol? q-component?) #f)) — persistent components
         focused-component-id-box ; (boxof (or/c symbol? #f)) — currently focused component
         agent-session-box ; (boxof (or/c any/c #f)) — live agent session for goal-runner
         goal-cancel-box) ; (boxof boolean?) — #t signals goal thread to stop
  #:transparent)

(define (make-tui-ctx #:event-bus [bus #f]
                      #:session-runner [runner (lambda (prompt) (void))]
                      #:session-dir [sess-dir #f]
                      #:model-registry [reg #f]
                      #:extension-registry [ext-reg #f]
                      #:session-queue [sess-queue #f]
                      #:session-factory-runner [factory #f]
                      #:agent-session-box [sess-box #f])
  (tui-ctx (box (initial-ui-state))
           (box (initial-input-state))
           bus
           runner
           (box #t)
           (make-async-channel)
           sess-dir
           (box #t) ; needs-redraw: #t for first frame
           (box #f) ; term-box - set when terminal opened
           (box #f) ; ubuf-box - set when buffer created
           (box reg) ; model-registry-box
           (box #f) ; previous-frame-box - #f means no previous frame
           (box #f) ; prev-ubuf-box - previous cell buffer for cell-diff
           (box #f) ; last-prompt-box - #f until first submit
           (box ext-reg) ; extension-registry-box
           (box sess-queue) ; session-queue-box
           factory
           (box #f) ; component-registry-box - lazy init on first render
           (box #f) ; focused-component-id-box - no component focused initially
           (or sess-box (box #f)) ; agent-session-box
           (box #f))) ; goal-cancel-box

;; Mark that the frame needs redraw.
(define (mark-dirty! ctx)
  (set-box! (tui-ctx-needs-redraw-box ctx) #t))

;; Get the currently focused component id (or #f if none).
(define (tui-ctx-focused-component-id ctx)
  (unbox (tui-ctx-focused-component-id-box ctx)))

;; Set the focused component id.
(define (tui-ctx-set-focused-component! ctx id)
  (set-box! (tui-ctx-focused-component-id-box ctx) id))

(provide tui-ctx
         tui-ctx?
         tui-ctx-ui-state-box
         tui-ctx-input-state-box
         tui-ctx-event-bus
         tui-ctx-session-runner
         tui-ctx-running-box
         tui-ctx-event-ch
         tui-ctx-session-dir
         tui-ctx-needs-redraw-box
         tui-ctx-term-box
         tui-ctx-ubuf-box
         tui-ctx-model-registry-box
         tui-ctx-previous-frame-box
         tui-ctx-prev-ubuf-box
         tui-ctx-last-prompt-box
         tui-ctx-extension-registry-box
         tui-ctx-session-queue-box
         tui-ctx-session-factory-runner
         tui-ctx-component-registry-box
         tui-ctx-focused-component-id-box
         tui-ctx-agent-session-box
         tui-ctx-goal-cancel-box
         (contract-out [make-tui-ctx
                        (->* ()
                             (#:event-bus (or/c event-bus? #f)
                                          #:session-runner procedure?
                                          #:session-dir (or/c path-string? #f)
                                          #:model-registry (or/c model-registry? #f)
                                          #:extension-registry (or/c extension-registry? #f)
                                          #:session-queue (or/c queue? #f)
                                          #:session-factory-runner (or/c procedure? #f)
                                          #:agent-session-box (or/c box? #f))
                             tui-ctx?)]
                       [mark-dirty! (-> tui-ctx? void?)]
                       [tui-ctx-focused-component-id (-> tui-ctx? (or/c symbol? #f))]
                       [tui-ctx-set-focused-component! (-> tui-ctx? (or/c symbol? #f) void?)]))
