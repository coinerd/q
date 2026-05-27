#lang racket/base

;; q/tui/context.rkt — TUI context struct, constructor, and dirty flag
;;
;; Extracted from tui-keybindings.rkt (W15) to reduce hotspot complexity.
;; This is a leaf module with minimal dependencies.

(require racket/contract
         racket/async-channel
         "state.rkt"
         "input.rkt"
         "../agent/event-bus.rkt")

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
         last-prompt-box ; (boxof (or/c string? #f)) — last user prompt for /retry
         extension-registry-box ; (or/c (boxof (or/c extension-registry? #f)) #f)
         session-queue-box ; (boxof (or/c queue? #f)) — agent queue for followup during streaming (G3.1)
         session-factory-runner) ; (or/c (string -> void) #f) — creates fresh session for /go
  #:transparent)

(define (make-tui-ctx #:event-bus [bus #f]
                      #:session-runner [runner (lambda (prompt) (void))]
                      #:session-dir [sess-dir #f]
                      #:model-registry [reg #f]
                      #:extension-registry [ext-reg #f]
                      #:session-queue [sess-queue #f]
                      #:session-factory-runner [factory #f])
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
           (box #f) ; last-prompt-box - #f until first submit
           (box ext-reg) ; extension-registry-box
           (box sess-queue) ; session-queue-box
           factory))

;; Mark that the frame needs redraw.
(define (mark-dirty! ctx)
  (set-box! (tui-ctx-needs-redraw-box ctx) #t))

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
         tui-ctx-last-prompt-box
         tui-ctx-extension-registry-box
         tui-ctx-session-queue-box
         tui-ctx-session-factory-runner
         (contract-out [make-tui-ctx
                        (->* ()
                             (#:event-bus (or/c event-bus? #f)
                                          #:session-runner procedure?
                                          #:session-dir (or/c path-string? #f)
                                          #:model-registry any/c
                                          #:extension-registry any/c
                                          #:session-queue any/c
                                          #:session-factory-runner any/c)
                             tui-ctx?)]
                       [mark-dirty! (-> tui-ctx? void?)]))
