#lang racket/base

;; tui/commands/model.rkt — /model command handler
;;
;; Extracted from commands.rkt (ARCH-06).
;; Handles /model [list|<name>] for model listing and switching.

(require "../state.rkt"
         "../../util/protocol-types.rkt"
         "../../agent/event-bus.rkt"
         "../../runtime/model-registry.rkt"
         "context.rkt")

(provide handle-model-command)

;; ============================================================
;; /model command handler
;; ============================================================

;; Handle /model command — list or switch models
(define (handle-model-command cctx [arg #f])
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define reg-box (cmd-ctx-model-registry-box cctx))
  (define reg (and reg-box (unbox reg-box)))
  (cond
    ;; No registry available
    [(not reg)
     (define entry (make-entry 'error "[no model registry available]" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; No argument — list available models
    [(not arg)
     (define models (available-models reg))
     (define default (default-model reg))
     (define header (make-entry 'system "Available models:" 0 (hash)))
     (define model-entries
       (for/list ([m (in-list models)])
         (define marker (if (equal? (model-entry-name m) default) " *" "  "))
         (make-entry 'system
                     (format "~a ~a (~a)" marker (model-entry-name m) (model-entry-provider-name m))
                     0
                     (hash))))
     (define all-entries (cons header model-entries))
     (define new-state
       (for/fold ([s state]) ([e (in-list all-entries)])
         (add-transcript-entry s e)))
     (set-box! (cmd-ctx-state-box cctx) new-state)
     'continue]
    ;; Argument provided — switch model
    [else
     (define resolution (resolve-model reg arg))
     (cond
       [(not resolution)
        (define entry
          (make-entry 'error
                      (format "Model not found: ~a. Use /model to list available models." arg)
                      0
                      (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else
        ;; Publish model.switched event
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "model.switched"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hasheq 'model
                                        (model-resolution-model-name resolution)
                                        'provider
                                        (model-resolution-provider-name resolution)))))
        (define entry
          (make-entry 'system
                      (format "[switched to model: ~a (provider: ~a)]"
                              (model-resolution-model-name resolution)
                              (model-resolution-provider-name resolution))
                      0
                      (hasheq 'model
                              (model-resolution-model-name resolution)
                              'provider
                              (model-resolution-provider-name resolution))))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue])]))
