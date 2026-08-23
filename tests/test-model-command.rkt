#lang racket

;; @speed fast
;; @suite default
;; @boundary unit
;; BOUNDARY: integration

;; test-model-command.rkt — tests for /model command in TUI and CLI
;;
;; Covers:
;;   - cmd-ctx construction with model-registry-box field
;;   - handle-model-command: no registry, list models, switch, not found
;;   - CLI parse-slash-command for /model
;;   - Model registry integration

(require rackunit
         racket/file
         "../tui/commands.rkt"
         "../tui/command-parse.rkt"
         "../tui/commands/context.rkt"
         "../tui/state.rkt"
         "../tools/tool.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         "../runtime/session/session-config.rkt"
         "../runtime/provider/model-registry.rkt"
         "../interfaces/cli.rkt"
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

(define (make-test-config)
  ;; 'api-key inline so switch-model! can create providers without env creds
  ;; (BUG-0018 W2 tests exercise the live-session switch path).
  (hasheq 'providers
          (hasheq 'openai
                  (hasheq 'base-url
                          "https://api.openai.com/v1"
                          'api-key
                          "test-key-openai"
                          'default-model
                          "gpt-4o"
                          'models
                          '("gpt-4" "gpt-4o" "gpt-3.5-turbo"))
                  'anthropic
                  (hasheq 'base-url
                          "https://api.anthropic.com/v1"
                          'api-key
                          "test-key-anthropic"
                          'default-model
                          "claude-3-sonnet"
                          'models
                          '("claude-3-opus" "claude-3-sonnet" "claude-3-haiku")))
          'default-provider
          "openai"
          'default-model
          "gpt-4o"))

(define (make-test-cctx #:model-registry [reg #f] #:event-bus [bus #f])
  ;; BUG-0018 W2: switch tests need a live agent session — the handler now
  ;; refuses UI-only switches when no live session exists.
  (define sess
    (and reg
         (make-agent-session (hasheq 'model-name
                                     "gpt-4o"
                                     'event-bus
                                     (make-event-bus)
                                     'tool-registry
                                     (make-tool-registry)
                                     'session-dir
                                     (path->string (make-temporary-file "q-model-cmd-~a"
                                                                        'directory))))))
  (cmd-ctx (box (initial-ui-state)) ; state-box
           (box #t) ; running-box
           bus ; event-bus
           #f ; session-dir
           (box #f) ; needs-redraw-box
           (and reg (box reg)) ; model-registry-box
           (box #f) ; last-prompt-box
           #f ; session-runner
           (box "") ; input-text-box
           (box #f)
           #f ; session-factory-runner
           (box sess) ; agent-session-box
           (box #f))) ; goal-cancel-box

;; Extract transcript text from a cmd-ctx
(define (cctx-transcript-text cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (for/list ([e (in-list (ui-state-transcript state))])
    (transcript-entry-text e)))

;; ============================================================
;; 1. cmd-ctx construction with new field (3 tests)
;; ============================================================

(test-case "test-model-command: checks block 4"
  (check-true (cmd-ctx? (make-test-cctx))
              "cmd-ctx accepts arguments including last-prompt-box and session-runner")

  (check-false (cmd-ctx-model-registry-box (make-test-cctx))
               "cmd-ctx-model-registry-box returns #f when not provided")

  (check-true (box? (cmd-ctx-model-registry-box (make-test-cctx #:model-registry 'something)))
              "cmd-ctx-model-registry-box returns a box when registry provided"))

;; ============================================================
;; 2. handle-model-command — no registry (2 tests)
;; ============================================================

(let ([cctx (make-test-cctx)]) ; no model registry
  (check-equal? (process-slash-command cctx 'model)
                'continue
                "/model with no registry returns 'continue")
  (check-not-false (member "[no model registry available]" (cctx-transcript-text cctx))
                   "Transcript contains 'no model registry' error message"))

;; ============================================================
;; 3. handle-model-command — list models (5 tests)
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [cctx (make-test-cctx #:model-registry reg)])
  (check-equal? (process-slash-command cctx 'model) 'continue "/model list returns 'continue")
  (define text (cctx-transcript-text cctx))
  (check-not-false (member "Available models:" text) "Transcript contains 'Available models:' header")
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t "gpt-4o"))
                     #t)
                   "Transcript contains 'gpt-4o'")
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t " * "))
                     #t)
                   "Transcript contains default marker '*'")
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t "anthropic"))
                     #t)
                   "Transcript contains provider name 'anthropic'"))

;; ============================================================
;; 4. handle-model-command — switch model (4 tests)
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [cctx (make-test-cctx #:model-registry reg)])
  (check-equal? (process-slash-command cctx '(model "gpt-3.5-turbo"))
                'continue
                "/model switch returns 'continue")
  (define text (cctx-transcript-text cctx))
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t "switched to model"))
                     #t)
                   "Transcript contains 'switched to model'")
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t "gpt-3.5-turbo"))
                     #t)
                   "Transcript contains 'gpt-3.5-turbo'")
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t "openai"))
                     #t)
                   "Transcript contains provider 'openai'"))

(test-case "parsed /model dispatch forwards its model argument"
  (define reg (make-model-registry-from-config (make-test-config)))
  (define cctx (make-test-cctx #:model-registry reg))
  (define parsed (parse-command-name "/model gpt-3.5-turbo"))
  (check-true (parsed-command? parsed))
  (check-equal? (process-slash-command cctx parsed) 'continue)
  (check-not-false (for/or ([t (in-list (cctx-transcript-text cctx))])
                     (and (string-contains? t "switched to model")
                          (string-contains? t "gpt-3.5-turbo")))))

;; ============================================================
;; 5. handle-model-command — model not found (2 tests)
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [cctx (make-test-cctx #:model-registry reg)])
  (check-equal? (process-slash-command cctx '(model "nonexistent"))
                'continue
                "/model with unknown model returns 'continue")
  (define text (cctx-transcript-text cctx))
  (check-not-false (for/or ([t (in-list text)]
                            #:when (string-contains? t "not found"))
                     #t)
                   "Transcript contains 'not found' error"))

;; ============================================================
;; 6. CLI parse-slash-command (4 tests)
;; ============================================================

(test-case "test-model-command: checks block 3"
  (check-equal? (parse-slash-command "/model") '(model) "parse-slash-command /model → (model)")
  (check-equal? (parse-slash-command "/model gpt-4")
                '(model "gpt-4")
                "parse-slash-command /model gpt-4 → (model \"gpt-4\")"))

(test-case "test-model-command: checks block 2"
  (check-equal? (parse-slash-command "/model ")
                '(model)
                "parse-slash-command /model (trailing space) → (model)"))

(test-case "test-model-command: checks block 1"
  (check-false (parse-slash-command "/models")
               "parse-slash-command /models → #f (different command)"))

;; ============================================================
;; 7. Model registry integration (5 tests)
;; ============================================================

(let ([reg (make-model-registry-from-config (make-test-config))])
  (check-not-false reg "make-model-registry-from-config returns non-#f")
  (check-not-false (and (list? (available-models reg)) (> (length (available-models reg)) 0))
                   "available-models returns non-empty list")
  (check-not-false (resolve-model reg "gpt-4") "resolve-model finds gpt-4")
  (let ([r (resolve-model reg "gpt-4")])
    (check-equal? (model-resolution-provider-name r)
                  "openai"
                  "resolve-model returns correct provider name")
    (check-equal? (model-resolution-model-name r) "gpt-4" "resolve-model returns correct model name"))
  (check-equal? (default-model reg) "gpt-4o" "default-model returns gpt-4o"))

;; ============================================================
;; BUG-0018 W2: /model switch reaches the request path
;; ============================================================
(let* ([reg (make-model-registry-from-config (make-test-config))]
       [bus (make-event-bus)]
       [events (box '())]
       [_ (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt)))))]
       [cctx (make-test-cctx #:model-registry reg #:event-bus bus)])
  (check-equal? (process-slash-command cctx '(model "gpt-3.5-turbo")) 'continue)
  (define sess (unbox (cmd-ctx-agent-session-box cctx)))
  ;; The live session actually switched.
  (check-equal? (agent-session-model-name sess)
                "gpt-3.5-turbo"
                "session model-name updated by /model switch")
  ;; The explicit override marker is set, so path-derived resolution cannot
  ;; clobber the switch on the next prompt (BUG-0018 R-B1).
  (check-true (config-model-override (agent-session-config sess))
              "explicit model override marker recorded")
  ;; A guaranteed model.switched event was published (BUG-0018 R-B2).
  (define switched
    (for/first ([e (in-list (unbox events))]
                #:when (equal? (event-ev e) "model.switched"))
      e))
  (check-not-false switched "model.switched event published")
  (when switched
    (check-equal? (hash-ref (event-payload switched) 'model) "gpt-3.5-turbo")
    (check-equal? (hash-ref (event-payload switched) 'provider) "openai")))

(test-case "BUG-0018: next constructed provider-settings carries the switched model"
  ;; Mirrors turn-orchestrator's request-path resolution: the config hash is
  ;; the single source of truth for the settings 'model value, and it must
  ;; reflect the switched model after handle-model-command.
  (define reg (make-model-registry-from-config (make-test-config)))
  (define cctx (make-test-cctx #:model-registry reg))
  (process-slash-command cctx '(model "gpt-3.5-turbo"))
  (define sess (unbox (cmd-ctx-agent-session-box cctx)))
  (define cfg (agent-session-config sess))
  (check-equal? (config-model-name cfg)
                "gpt-3.5-turbo"
                "config model-name (request-path source) reflects the switch"))
