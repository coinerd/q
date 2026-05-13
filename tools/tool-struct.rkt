#lang racket/base
;; tools/tool-struct.rkt -- Tool struct definition
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(provide tool?
         tool-name
         tool-description
         tool-schema
         ;; NOTE: tool-execute is exported for test/internal use only.
         ;;       Production code should use tools/scheduler.rkt for invocation.
         ;;       A future breaking change may gate this behind a test-only module.
         tool-execute
         tool-prompt-snippet
         tool-prompt-guidelines
         tool-dangerous?
         tool-render-call
         tool-render-result
         ;; Keyword constructor with validation (S3-F2c)
         make-tool)

(struct tool
        (name description
              schema
              execute
              prompt-snippet
              prompt-guidelines
              render-call
              render-result
              dangerous?)
  #:transparent)

;; make-tool : keyword constructor with validation
(define (make-tool name
                   description
                   schema
                   execute
                   #:prompt-snippet [prompt-snippet #f]
                   #:render-call [render-call #f]
                   #:render-result [render-result #f]
                   #:prompt-guidelines [prompt-guidelines #f]
                   #:dangerous? [dangerous? #f])
  (unless (string? name)
    (raise-argument-error 'make-tool "string?" name))
  (unless (string? description)
    (raise-argument-error 'make-tool "string?" description))
  (unless (hash? schema)
    (raise-argument-error 'make-tool "hash?" schema))
  (unless (procedure? execute)
    (raise-argument-error 'make-tool "procedure?" execute))
  ;; W-16: Arity check -- validate handler accepts 1 or 2 args (args [exec-ctx])
  (unless (or (procedure-arity-includes? execute 1) (procedure-arity-includes? execute 2))
    (raise-arguments-error 'make-tool
                           (format "tool '~a' handler does not accept 1 or 2 args (args [exec-ctx])"
                                   name)))
  (tool name
        description
        schema
        execute
        prompt-snippet
        prompt-guidelines
        render-call
        render-result
        dangerous?))
