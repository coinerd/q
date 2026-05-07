#lang racket/base

(require racket/logging)
(define-logger q-tui-init)

;; q/tui/tui-init.rkt — TUI initialization, teardown, and entry points
;;
;; Extracted from interfaces/tui.rkt for modularity (Issue #194).
;;
;; Dependency chain: tui-init.rkt → tui-render-loop.rkt → tui-keybindings.rkt
;;
;; Provides:
;;   run-tui                — simple TUI run (testing)
;;   run-tui-with-runtime   — full TUI run with runtime config
;;   subscribe-runtime-events! — internal helper (used by entry points)

(require racket/async-channel
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/scrollback.rkt"
         "../tui/render.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/provider-factory.rkt"
         "../runtime/session-index.rkt"
         "../runtime/session-switch.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/tui-render-loop.rkt"
         "../cli/args.rkt"
         "../extensions/ui-surface.rkt"
         (only-in "../extensions/gsd/state-machine.rkt" gsm-current))

(provide run-tui
         run-tui-with-runtime
         subscribe-runtime-events!)

;; ============================================================
;; Runtime event subscription
;; ============================================================

(define (subscribe-runtime-events! ctx)
  (define bus (tui-ctx-event-bus ctx))
  (define ch (tui-ctx-event-ch ctx))
  (when bus
    (subscribe! bus
                (lambda (evt)
                  ;; async-channel-put is non-blocking (unbounded buffer)
                  (async-channel-put ch evt)))))

;; ============================================================
;; Public entry points
;; ============================================================

;; Run TUI with a runtime config hash and cli-config
(define (run-tui-with-runtime rt-config cli-cfg)
  ;; rt-config is a hash from build-runtime-from-cli
  ;; cli-cfg is a cli-config struct
  (define bus (hash-ref rt-config 'event-bus #f))
  (define sess (make-agent-session rt-config))

  ;; Wire custom keybindings path if specified (#1118)
  (define kb-path (and (cli-config? cli-cfg) (cli-config-keybindings-path cli-cfg)))
  (when kb-path
    (current-keybindings-path kb-path)
    (reload-keymap!))

  ;; Determine session dir before creating TUI context (Fix #513)
  (define sess-dir (or (hash-ref rt-config 'session-dir #f) (hash-ref rt-config 'store-dir #f)))

  ;; Wire GSD mode query callback to avoid TUI→extensions circular import (v0.32.6)
  (current-gsd-mode-query (lambda () (gsm-current)))

  (define ctx
    (make-tui-ctx
     #:event-bus bus
     #:session-runner (lambda (prompt) (run-prompt! sess prompt))
     #:session-dir sess-dir
     #:model-registry (hash-ref rt-config 'model-registry #f)
     #:extension-registry (hash-ref rt-config 'extension-registry #f)
     #:session-queue (agent-session-queue sess)
     #:session-factory-runner
     (lambda (prompt)
       (with-handlers ([exn:fail?
                        (lambda (e)
                          ;; Display error as system message in TUI transcript
                          (define err-msg (format "[ERROR] /go failed: ~a" (exn-message e)))
                          (define entry
                            (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                          (set-box! (tui-ctx-ui-state-box ctx)
                                    (add-transcript-entry (unbox (tui-ctx-ui-state-box ctx)) entry))
                          (set-box! (tui-ctx-needs-redraw-box ctx) #t))])
         ;; v0.21.4: /go creates fresh session with no planning history
         (define new-sess (make-agent-session rt-config))
         (define new-sid (session-id new-sess))
         (define new-dir (or (hash-ref rt-config 'session-dir #f) (hash-ref rt-config 'store-dir #f)))
         ;; Switch extensions: teardown old, rebind to new
         (switch-session! #:old-session-id (session-id sess)
                          #:old-bus bus
                          #:old-extension-registry (hash-ref rt-config 'extension-registry #f)
                          #:new-session-id new-sid
                          #:new-session-dir new-dir
                          #:new-bus bus
                          #:new-extension-registry (hash-ref rt-config 'extension-registry #f)
                          #:reason 'fork)
         ;; Clear TUI transcript for new session
         (set-box! (tui-ctx-ui-state-box ctx)
                   (initial-ui-state #:session-id new-sid
                                     #:model-name (hash-ref rt-config 'model-name #f)))
         (set-box! (tui-ctx-needs-redraw-box ctx) #t)
         ;; Run prompt in new session
         (run-prompt! new-sess prompt)))))

  ;; Install UI callbacks for extensions (breaks extensions→TUI upward import)
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
           ;; LAYER-01: status message callback for dialog-api
           'set-status-message
           (lambda (box msg) (set-box! box (struct-copy ui-state (unbox box) [status-message msg])))
           ;; LAYER-02: widget callbacks for widget-api
           'set-extension-widget
           set-extension-widget
           'remove-extension-widget
           remove-extension-widget
           'remove-all-extension-widgets
           remove-all-extension-widgets))

  ;; Subscribe to events
  (subscribe-runtime-events! ctx)

  ;; Determine scrollback file path from session dir
  ;; B3-C: Fallback scrollback — never silently lose transcript
  (define scrollback-path
    (cond
      [(and sess-dir (directory-exists? sess-dir)) (build-path sess-dir "scrollback.jsonl")]
      [else (build-path "/tmp" (format "q-scrollback-~a.jsonl" (current-seconds)))]))

  ;; First-run welcome detection
  (define q-config-dir (build-path (find-system-path 'home-dir) ".q"))
  (define first-run?
    (not (or (directory-exists? q-config-dir)
             (file-exists? (build-path q-config-dir "config.json")))))

  ;; Set initial session info, loading scrollback if available
  (define base-state
    (initial-ui-state #:session-id (session-id sess)
                      #:model-name (hash-ref rt-config 'model-name #f)))
  ;; BUG-55: Detect mock provider and set warning flag
  (define prov (hash-ref rt-config 'provider #f))
  (define mock? (provider-is-mock? prov))
  (define base-state-with-mock
    (if mock?
        (struct-copy ui-state base-state [mock-provider? #t])
        base-state))

  ;; Add welcome message for first-run users
  (define init-state
    (let* ([state (if scrollback-path
                      (let ([loaded (load-scrollback scrollback-path)])
                        (if (null? loaded)
                            base-state-with-mock
                            (let ([max-id (for/fold ([m -1]) ([e (in-list loaded)])
                                            (max m (or (transcript-entry-id e) -1)))])
                              (struct-copy ui-state
                                           base-state-with-mock
                                           [transcript loaded]
                                           [next-entry-id (add1 max-id)]))))
                      base-state-with-mock)]
           [welcome-entries (if (and first-run? (null? (ui-state-transcript state)))
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
                       state
                       [transcript (append welcome-entries (ui-state-transcript state))])
          state)))
  ;; B2-C: Show provider info message on startup
  (when (and prov (not (provider-is-mock? prov)))
    (define prov-info-entry
      (make-entry 'system
                  (format "Provider: ~a | Model: ~a"
                          (provider-name prov)
                          (or (hash-ref rt-config 'model-name #f) "unknown"))
                  (current-inexact-milliseconds)
                  (hash)))
    (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry init-state prov-info-entry)))
  (unless (and prov (not (provider-is-mock? prov)))
    (set-box! (tui-ctx-ui-state-box ctx) init-state))

  ;; Initialize terminal, run main loop, cleanup on exit
  (tui-ctx-init-terminal! ctx)
  (with-handlers
      ([exn:break? (lambda (e) (void))]
       [exn:fail?
        (lambda (e)
          ;; BUG-27 fix: Save scrollback BEFORE re-raising
          ;; so that crash doesn't lose the transcript.
          (with-handlers
              ([exn:fail? (lambda (_) (log-q-tui-init-warning "cleanup failed: ~a" (exn-message _)))])
            (when scrollback-path
              (let* ([state (unbox (tui-ctx-ui-state-box ctx))]
                     [transcript (ui-state-transcript state)])
                (when (not (null? transcript))
                  (save-scrollback transcript scrollback-path)))))
          ;; Cleanup terminal (BUG-56: mouse disable in close)
          (with-handlers
              ([exn:fail? (lambda (_) (log-q-tui-init-warning "cleanup failed: ~a" (exn-message _)))])
            (disable-mouse-tracking)
            (tui-term-close (unbox (tui-ctx-term-box ctx))))
          (raise e))])
    (tui-main-loop ctx))
  ;; Cleanup terminal BEFORE printing Goodbye
  ;; (so newline works correctly and we're on normal screen)
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
          (save-scrollback transcript scrollback-path)))))
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
