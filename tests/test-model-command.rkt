#lang racket
;; test-model-command.rkt — tests for /model command in TUI and CLI
;;
;; Covers:
;;   - cmd-ctx construction with model-registry-box field
;;   - handle-model-command: no registry, list models, switch, not found
;;   - CLI parse-slash-command for /model
;;   - Model registry integration

(require rackunit
         "../tui/commands.rkt"
         "../tui/state.rkt"
         "../runtime/model-registry.rkt"
         "../interfaces/cli.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

(define (make-test-config)
  (hasheq 'providers
          (hasheq 'openai
                  (hasheq 'base-url
                          "https://api.openai.com/v1"
                          'api-key-env
                          "OPENAI_API_KEY"
                          'default-model
                          "gpt-4o"
                          'models
                          '("gpt-4" "gpt-4o" "gpt-3.5-turbo"))
                  'anthropic
                  (hasheq 'base-url
                          "https://api.anthropic.com/v1"
                          'api-key-env
                          "ANTHROPIC_API_KEY"
                          'default-model
                          "claude-3-sonnet"
                          'models
                          '("claude-3-opus" "claude-3-sonnet" "claude-3-haiku")))
          'default-provider
          "openai"
          'default-model
          "gpt-4o"))

(define (make-test-cctx #:model-registry [reg #f])
  (cmd-ctx (box (initial-ui-state)) ; state-box
           (box #t) ; running-box
           #f ; event-bus
           #f ; session-dir
           (box #f) ; needs-redraw-box
           (and reg (box reg)) ; model-registry-box
           (box #f) ; last-prompt-box
           #f)) ; session-runner

;; Extract transcript text from a cmd-ctx
(define (cctx-transcript-text cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (for/list ([e (in-list (ui-state-transcript state))])
    (transcript-entry-text e)))

;; ============================================================
;; 1. cmd-ctx construction with new field (3 tests)
;; ============================================================

(check-true (cmd-ctx? (make-test-cctx))
            "cmd-ctx accepts 8 arguments including last-prompt-box and session-runner")

(check-false (cmd-ctx-model-registry-box (make-test-cctx))
             "cmd-ctx-model-registry-box returns #f when not provided")

(check-true (box? (cmd-ctx-model-registry-box (make-test-cctx #:model-registry 'something)))
            "cmd-ctx-model-registry-box returns a box when registry provided")

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

(check-equal? (parse-slash-command "/model") '(model) "parse-slash-command /model → (model)")

(check-equal? (parse-slash-command "/model gpt-4")
              '(model "gpt-4")
              "parse-slash-command /model gpt-4 → (model \"gpt-4\")")

(check-equal? (parse-slash-command "/model ")
              '(model)
              "parse-slash-command /model (trailing space) → (model)")

(check-false (parse-slash-command "/models") "parse-slash-command /models → #f (different command)")

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
