#lang racket/base

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
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../llm/provider.rkt"
         "../runtime/session-index.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/tui-render-loop.rkt"
         "../cli/args.rkt")

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

  (define ctx
    (make-tui-ctx #:event-bus bus
                  #:session-runner (lambda (prompt) (run-prompt! sess prompt))
                  #:session-dir sess-dir
                  #:model-registry (hash-ref rt-config 'model-registry #f)))

  ;; Subscribe to events
  (subscribe-runtime-events! ctx)

  ;; Determine scrollback file path from session dir
  (define scrollback-path (and sess-dir (build-path sess-dir "scrollback.jsonl")))

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
  (define mock? (and prov (provider? prov) (equal? (provider-name prov) "mock")))
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
  (set-box! (tui-ctx-ui-state-box ctx) init-state)

  ;; Initialize terminal, run main loop, cleanup on exit
  (tui-ctx-init-terminal! ctx)
  (with-handlers ([exn:break? (lambda (e) (void))]
                  [exn:fail? (lambda (e)
                               ;; BUG-27 fix: Save scrollback BEFORE re-raising
                               ;; so that crash doesn't lose the transcript.
                               (with-handlers ([exn:fail? (lambda (_) (void))])
                                 (when scrollback-path
                                   (let* ([state (unbox (tui-ctx-ui-state-box ctx))]
                                          [transcript (ui-state-transcript state)])
                                     (when (not (null? transcript))
                                       (save-scrollback transcript scrollback-path)))))
                               ;; Cleanup terminal (BUG-56: mouse disable in close)
                               (with-handlers ([exn:fail? (lambda (_) (void))])
                                 (disable-mouse-tracking)
                                 (tui-term-close (unbox (tui-ctx-term-box ctx))))
                               (raise e))])
    (tui-main-loop ctx))
  ;; Cleanup terminal BEFORE printing Goodbye
  ;; (so newline works correctly and we're on normal screen)
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (disable-mouse-tracking)
    (tui-term-close (unbox (tui-ctx-term-box ctx))))
  ;; Save scrollback
  (when scrollback-path
    (with-handlers ([exn:fail? (lambda (e) (void))])
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
                  [exn:fail? (lambda (e)
                               (with-handlers ([exn:fail? (lambda (_) (void))])
                                 (disable-mouse-tracking)
                                 (tui-term-close (unbox (tui-ctx-term-box ctx))))
                               (raise e))])
    (tui-main-loop ctx))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (disable-mouse-tracking)
    (tui-term-close (unbox (tui-ctx-term-box ctx))))
  (displayln "Goodbye."))
