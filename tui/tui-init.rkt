#lang racket/base
(require racket/contract)

(require racket/dict)

(require racket/logging)
(define-logger q-tui-init)

;; q/tui/tui-init.rkt -- TUI initialization, teardown, and entry points
;;
;; v0.35.4 (W-19): Extracted phases from run-tui-with-runtime.
;;   create-tui-session  -- session + context creation
;;   load-tui-scrollback -- scrollback loading + welcome messages
;;   init-tui-terminal   -- terminal setup + event subscription
;;   run-tui-loop        -- main render loop with cleanup

(require racket/async-channel
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/scrollback.rkt"
         "../tui/render.rkt"
         (only-in "../util/message/message.rkt" message)
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/provider/provider-factory.rkt"
         "../runtime/session-index.rkt"
         "../runtime/session/session-switch.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/tui-render-loop.rkt"
         "../cli/args.rkt"
         "../extensions/ui-surface.rkt")

(provide (contract-out [run-tui (->* () () any)]
                       [run-tui-with-runtime (-> any/c any/c any)]
                       [subscribe-runtime-events! (-> any/c void?)]
                       [create-tui-session (-> any/c any/c any)]
                       [load-tui-scrollback (-> any/c any/c any/c any/c any)]
                       [init-tui-terminal (-> any/c any)]
                       [run-tui-loop (-> any/c any/c any)]))

;; ============================================================
;; Runtime event subscription
;; ============================================================

(define (subscribe-runtime-events! ctx)
  (define bus (tui-ctx-event-bus ctx))
  (define ch (tui-ctx-event-ch ctx))
  (when bus
    (subscribe! bus (lambda (evt) (async-channel-put ch evt))))
  (void))

;; ============================================================
;; Phase 1: Create session + context (W-19)
;; ============================================================

(define (create-tui-session rt-config cli-cfg)
  ;; Create agent session and TUI context from runtime config.
  ;; Returns (values ctx sess scrollback-path)
  (define bus (dict-ref rt-config 'event-bus #f))
  (define sess (make-agent-session rt-config))

  ;; Wire custom keybindings path if specified
  (define kb-path (and (cli-config? cli-cfg) (cli-config-keybindings-path cli-cfg)))
  (when kb-path
    (current-keybindings-path kb-path)
    (reload-keymap!))

  ;; Determine session dir
  (define sess-dir (or (dict-ref rt-config 'session-dir #f) (dict-ref rt-config 'store-dir #f)))

  ;; Wire GSD mode query callback — defaults to 'idle
  ;; Caller (main.rkt) sets current-gsd-mode-query to gsm-current if GSD is loaded

  (define ctx
    (make-tui-ctx
     #:event-bus bus
     #:session-runner (lambda (prompt) (run-prompt! sess prompt))
     #:session-dir sess-dir
     #:model-registry (dict-ref rt-config 'model-registry #f)
     #:extension-registry (dict-ref rt-config 'extension-registry #f)
     #:session-queue (agent-session-queue sess)
     #:session-factory-runner
     (lambda (prompt)
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (define err-msg (format "[ERROR] /go failed: ~a" (exn-message e)))
                          (define entry
                            (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                          (set-box! (tui-ctx-ui-state-box ctx)
                                    (add-transcript-entry (unbox (tui-ctx-ui-state-box ctx)) entry))
                          (set-box! (tui-ctx-needs-redraw-box ctx) #t))])
         (define new-sess (make-agent-session rt-config))
         (define new-sid (session-id new-sess))
         (define new-dir (or (dict-ref rt-config 'session-dir #f) (dict-ref rt-config 'store-dir #f)))
         (switch-session! #:old-session-id (session-id sess)
                          #:old-bus bus
                          #:old-extension-registry (dict-ref rt-config 'extension-registry #f)
                          #:new-session-id new-sid
                          #:new-session-dir new-dir
                          #:new-bus bus
                          #:new-extension-registry (dict-ref rt-config 'extension-registry #f)
                          #:reason 'fork)
         ;; Update agent-session-box with new session for goal-runner
         (set-box! (tui-ctx-agent-session-box ctx) new-sess)
         (set-box! (tui-ctx-ui-state-box ctx)
                   (initial-ui-state #:session-id new-sid
                                     #:model-name (dict-ref rt-config 'model-name #f)))
         (set-box! (tui-ctx-needs-redraw-box ctx) #t)
         (run-prompt! new-sess prompt)))
     #:agent-session-box (box sess)))

  ;; Scrollback path
  (define scrollback-path
    (cond
      [(and sess-dir (directory-exists? sess-dir)) (build-path sess-dir "scrollback.jsonl")]
      [else (build-path "/tmp" (format "q-scrollback-~a.jsonl" (current-seconds)))]))

  (values ctx sess scrollback-path))

;; ============================================================
;; Phase 2: Load scrollback + welcome (W-19)
;; ============================================================

(define (load-tui-scrollback ctx sess rt-config scrollback-path)
  ;; Load scrollback and set initial UI state.
  ;; Returns updated state.
  (define prov (dict-ref rt-config 'provider #f))
  (define mock? (provider-is-mock? prov))
  (define q-config-dir (build-path (find-system-path 'home-dir) ".q"))
  (define first-run?
    (not (or (directory-exists? q-config-dir)
             (file-exists? (build-path q-config-dir "config.json")))))

  (define base-state
    (initial-ui-state #:session-id (session-id sess)
                      #:model-name (dict-ref rt-config 'model-name #f)))
  (define base-state-with-mock
    (if mock?
        (struct-copy ui-state base-state [mock-provider? #t])
        base-state))

  ;; Load scrollback entries
  (define state-with-scrollback
    (if scrollback-path
        (let ([loaded (load-scrollback scrollback-path)])
          (if (null? loaded)
              base-state-with-mock
              (let ([max-id (for/fold ([m -1]) ([e (in-list loaded)])
                              (max m (or (transcript-entry-id e) -1)))])
                (struct-copy ui-state
                             base-state-with-mock
                             [transcript loaded]
                             [next-entry-id (add1 max-id)]))))
        base-state-with-mock))

  ;; Add welcome messages for first-run users
  (define init-state
    (let ([welcome-entries (if (and first-run? (null? (ui-state-transcript state-with-scrollback)))
                               (list (make-entry 'system
                                                 "Welcome to q! Type a message to get started."
                                                 (current-inexact-milliseconds)
                                                 (hash))
                                     (make-entry 'system
                                                 "Commands: /help for reference, /quit to exit."
                                                 (current-inexact-milliseconds)
                                                 (hash)))
                               '())])
      (if (not (null? welcome-entries))
          (struct-copy ui-state
                       state-with-scrollback
                       [transcript
                        (append welcome-entries (ui-state-transcript state-with-scrollback))])
          state-with-scrollback)))

  ;; Show provider info message
  (cond
    [(and prov (not (provider-is-mock? prov)))
     (define prov-info-entry
       (make-entry 'system
                   (format "Provider: ~a | Model: ~a"
                           (provider-name prov)
                           (or (dict-ref rt-config 'model-name #f) "unknown"))
                   (current-inexact-milliseconds)
                   (hash)))
     (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry init-state prov-info-entry))]
    [else (set-box! (tui-ctx-ui-state-box ctx) init-state)]))

;; ============================================================
;; Phase 3: Install UI callbacks + subscribe events (W-19)
;; ============================================================

(define (init-tui-terminal ctx)
  ;; Install UI callbacks and subscribe to runtime events.
  (install-ui-callbacks!
   (hasheq 'set-footer
           (lambda (box lines) (set-box! box (set-custom-footer (unbox box) lines)))
           'set-header
           (lambda (box lines) (set-box! box (set-custom-header (unbox box) lines)))
           'clear-footer
           (lambda (box) (set-box! box (clear-custom-footer (unbox box))))
           'clear-header
           (lambda (box) (set-box! box (clear-custom-header (unbox box))))
           'make-styled-line
           styled-line
           'make-styled-segment
           styled-segment
           'set-status-message
           (lambda (box msg) (set-box! box (set-status-message (unbox box) msg)))
           'set-extension-widget
           set-extension-widget
           'remove-extension-widget
           remove-extension-widget
           'remove-all-extension-widgets
           remove-all-extension-widgets))
  (subscribe-runtime-events! ctx)
  (tui-ctx-init-terminal! ctx))

;; ============================================================
;; Phase 4: Run main loop with cleanup (W-19)
;; ============================================================

(define (run-tui-loop ctx scrollback-path)
  ;; Run the main TUI loop with crash handling and cleanup.
  (with-handlers
      ([exn:break? (lambda (e) (void))]
       [exn:fail?
        (lambda (e)
          ;; Save scrollback BEFORE re-raising
          (with-handlers
              ([exn:fail? (lambda (_) (log-q-tui-init-warning "cleanup failed: ~a" (exn-message _)))])
            (when scrollback-path
              (let* ([state (unbox (tui-ctx-ui-state-box ctx))]
                     [transcript (ui-state-transcript state)])
                (when (not (null? transcript))
                  (save-scrollback transcript scrollback-path)))))
          (with-handlers
              ([exn:fail? (lambda (_) (log-q-tui-init-warning "cleanup failed: ~a" (exn-message _)))])
            (disable-mouse-tracking)
            (tui-term-close (unbox (tui-ctx-term-box ctx))))
          (raise e))])
    (tui-main-loop ctx))
  ;; Cleanup terminal
  (with-handlers ([exn:fail? (lambda (e)
                               (log-q-tui-init-warning "cleanup failed: ~a" (exn-message e)))])
    (disable-mouse-tracking)
    (tui-term-close (unbox (tui-ctx-term-box ctx))))
  ;; Save scrollback
  (when scrollback-path
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-q-tui-init-warning "cleanup failed: ~a" (exn-message e)))])
      (let* ([state (unbox (tui-ctx-ui-state-box ctx))]
             [transcript (transcript-entries state)])
        (when (not (null? transcript))
          (save-scrollback transcript scrollback-path))))))
;; After cleanup, display goodbye

;; ============================================================
;; Public entry points (orchestrators)
;; ============================================================

;; Full TUI run with runtime config -- 4-phase orchestrator (W-19)
(define (run-tui-with-runtime rt-config cli-cfg)
  (define-values (ctx sess scrollback-path) (create-tui-session rt-config cli-cfg))
  (load-tui-scrollback ctx sess rt-config scrollback-path)
  (init-tui-terminal ctx)
  (run-tui-loop ctx scrollback-path)
  (displayln "Goodbye."))

;; Simple TUI run (for testing without full runtime)
(define (run-tui #:session-runner [session-runner (lambda (prompt) (void))] #:event-bus [bus #f])
  (define ctx (make-tui-ctx #:event-bus bus #:session-runner session-runner))
  (tui-ctx-init-terminal! ctx)
  (with-handlers ([exn:break? (lambda (e) (void))]
                  [exn:fail?
                   (lambda (e)
                     (with-handlers ([exn:fail? (lambda (_)
                                                  (log-q-tui-init-warning "cleanup failed: ~a"
                                                                          (exn-message _)))])
                       (disable-mouse-tracking)
                       (tui-term-close (unbox (tui-ctx-term-box ctx))))
                     (raise e))])
    (tui-main-loop ctx))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-q-tui-init-warning "cleanup failed: ~a" (exn-message e)))])
    (disable-mouse-tracking)
    (tui-term-close (unbox (tui-ctx-term-box ctx))))
  (displayln "Goodbye."))
