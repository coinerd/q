#lang racket/base

;; tests/workflows/fixtures/workflow-runner.rkt — full workflow orchestration
;;
;; Creates a complete SDK runtime, session, and runs prompts through it.
;; Returns structured results for assertion.

(require racket/file
         (prefix-in sdk: "../../../interfaces/sdk.rkt")
         "../../../agent/event-bus.rkt"
         (only-in "../../../util/protocol-types.rkt"
                  message?
                  message-role
                  message-content
                  message-id
                  message-parent-id
                  message-timestamp
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-name)
         (only-in "../../../tools/tool.rkt" make-tool-registry)
         (only-in "../../../extensions/api.rkt" make-extension-registry extension-registry?)
         (only-in "../../../extensions/loader.rkt" load-extension!)
         (only-in "../../../wiring/run-modes.rkt" load-extensions-from-dir!)
         "../../../util/jsonl.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/session-assert.rkt")

(provide (struct-out workflow-result)
         run-workflow
         run-workflow-multi-turn
         workflow-session-log-path
         workflow-session-entries)

;; ============================================================
;; Result struct
;; ============================================================

(struct workflow-result
        (output ; loop-result? from SDK
         events ; event-recorder? with captured events
         session-log ; path? to session.jsonl
         session-id ; string?
         project-dir ; path? (or #f if no project)
         session-dir ; path? to session base dir
         runtime ; sdk:runtime? (for further operations)
         side-effects) ; (listof string) — descriptions of side effects
  #:transparent)

;; ============================================================
;; run-workflow
;; ============================================================

;; run-workflow : provider? string?
;;               #:tools (or/c tool-registry? #f)
;;               #:files (listof (cons path-string? string?))
;;               #:max-iterations exact-positive-integer?
;;               #:system-instructions (listof string?)
;;               #:extensions (listof path-string?)
;;               #:extension-registry (or/c extension-registry? #f)
;;            -> workflow-result?
;;
;; Creates a full SDK runtime with the given provider, runs a single prompt,
;; and returns a structured result for assertion.
(define (run-workflow provider
                      prompt
                      #:tools [tool-reg #f]
                      #:files [files '()]
                      #:max-iterations [max-iter 10]
                      #:system-instructions [system-instrs '()]
                      #:extensions [ext-paths '()]
                      #:extension-registry [ext-reg #f])
  (define-values (project-dir session-dir)
    (if (null? files)
        (values #f (make-temp-session-dir))
        (make-temp-project files)))
  (define reg (or tool-reg (make-tool-registry)))
  (define bus (make-event-bus))
  (define recorder (make-event-recorder bus))

  ;; Build extension registry: use provided one, or create and load from paths
  (define effective-ext-reg
    (cond
      [ext-reg ext-reg]
      [(null? ext-paths) #f]
      [else
       (define er (make-extension-registry))
       (for ([p (in-list ext-paths)])
         (load-extension! er p #:event-bus bus))
       er]))

  (define rt
    (sdk:make-runtime #:provider provider
                      #:session-dir session-dir
                      #:tool-registry reg
                      #:event-bus bus
                      #:max-iterations max-iter
                      #:system-instructions system-instrs
                      #:extension-registry effective-ext-reg))
  (define rt2 (sdk:open-session rt))
  (define sid (hash-ref (sdk:session-info rt2) 'session-id))
  (define-values (rt3 result)
    (parameterize ([current-directory (or project-dir (current-directory))])
      (sdk:run-prompt! rt2 prompt)))

  (define log-path (build-path session-dir sid "session.jsonl"))

  (workflow-result result recorder log-path sid project-dir session-dir rt3 '()))

;; ============================================================
;; run-workflow-multi-turn
;; ============================================================

;; run-workflow-multi-turn : provider? (listof string?)
;;                          #:tools (or/c tool-registry? #f)
;;                          #:files (listof (cons path-string? string?))
;;                          #:max-iterations exact-positive-integer?
;;                       -> workflow-result?
;;
;; Runs multiple prompts through the same session.
(define (run-workflow-multi-turn provider
                                 prompts
                                 #:tools [tool-reg #f]
                                 #:files [files '()]
                                 #:max-iterations [max-iter 10]
                                 #:extensions [ext-paths '()]
                                 #:extension-registry [ext-reg #f])
  (define-values (project-dir session-dir)
    (if (null? files)
        (values #f (make-temp-session-dir))
        (make-temp-project files)))
  (define reg (or tool-reg (make-tool-registry)))
  (define bus (make-event-bus))
  (define recorder (make-event-recorder bus))

  ;; Build extension registry
  (define effective-ext-reg
    (cond
      [ext-reg ext-reg]
      [(null? ext-paths) #f]
      [else
       (define er (make-extension-registry))
       (for ([p (in-list ext-paths)])
         (load-extension! er p #:event-bus bus))
       er]))

  (define rt
    (sdk:make-runtime #:provider provider
                      #:session-dir session-dir
                      #:tool-registry reg
                      #:event-bus bus
                      #:max-iterations max-iter
                      #:extension-registry effective-ext-reg))
  (define rt2 (sdk:open-session rt))
  (define sid (hash-ref (sdk:session-info rt2) 'session-id))

  ;; Run each prompt sequentially, accumulating results
  (define final-rt
    (parameterize ([current-directory (or project-dir (current-directory))])
      (for/fold ([current-rt rt2]) ([prompt (in-list prompts)])
        (define-values (next-rt _result) (sdk:run-prompt! current-rt prompt))
        next-rt)))

  (define log-path (build-path session-dir sid "session.jsonl"))

  (workflow-result #f ; no single output for multi-turn
                   recorder
                   log-path
                   sid
                   project-dir
                   session-dir
                   final-rt
                   '()))

;; ============================================================
;; Helpers
;; ============================================================

;; workflow-session-log-path : workflow-result? -> path?
(define (workflow-session-log-path wr)
  (workflow-result-session-log wr))

;; workflow-session-entries : workflow-result? -> (listof message?)
(define (workflow-session-entries wr)
  (session-log-entries (workflow-session-log-path wr)))
