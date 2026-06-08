#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; Tests for provider scenario DSL (v0.83.5 W0)

(require racket/sequence
         rackunit
         racket/list
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "helpers/provider-scenarios.rkt")

;; ---------------------------------------------------------------------------
;; scenario-text
;; ---------------------------------------------------------------------------

(test-case "scenario-text: produces text response"
  (define resp (scenario-text "Hello world"))
  (check-equal? (model-response-stop-reason resp) 'stop)
  (check-equal? (length (model-response-content resp)) 1)
  (check-equal? (hash-ref (car (model-response-content resp)) 'type) "text")
  (check-equal? (hash-ref (car (model-response-content resp)) 'text) "Hello world"))

(test-case "scenario-text: custom usage"
  (define resp (scenario-text "hi" #:usage (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)))
  (check-equal? (hash-ref (model-response-usage resp) 'total-tokens) 7))

;; ---------------------------------------------------------------------------
;; scenario-tool-call
;; ---------------------------------------------------------------------------

(test-case "scenario-tool-call: produces tool-call response"
  (define resp (scenario-tool-call "bash" #:arguments (hash 'command "ls")))
  (check-equal? (model-response-stop-reason resp) 'tool-calls)
  (define tc (car (model-response-content resp)))
  (check-equal? (hash-ref tc 'type) "tool-call")
  (check-equal? (hash-ref tc 'name) "bash")
  (check-equal? (hash-ref tc 'id) "tc-1"))

(test-case "scenario-tool-call: defaults"
  (define resp (scenario-tool-call "read"))
  (define tc (car (model-response-content resp)))
  (check-equal? (hash-ref tc 'arguments) (hash)))

;; ---------------------------------------------------------------------------
;; scenario-multi-tool
;; ---------------------------------------------------------------------------

(test-case "scenario-multi-tool: multiple tool calls"
  (define resp (scenario-multi-tool '(("bash" (command . "ls")) ("read" (path . "foo.rkt")))))
  (check-equal? (model-response-stop-reason resp) 'tool-calls)
  (check-equal? (length (model-response-content resp)) 2)
  (check-equal? (hash-ref (car (model-response-content resp)) 'name) "bash")
  (check-equal? (hash-ref (cadr (model-response-content resp)) 'name) "read"))

;; ---------------------------------------------------------------------------
;; scenario-finish-length
;; ---------------------------------------------------------------------------

(test-case "scenario-finish-length: truncated response"
  (define resp (scenario-finish-length "truncated output..."))
  (check-equal? (model-response-stop-reason resp) 'length)
  (check-equal? (hash-ref (car (model-response-content resp)) 'text) "truncated output..."))

;; ---------------------------------------------------------------------------
;; scenario-streaming
;; ---------------------------------------------------------------------------

(test-case "scenario-streaming: produces chunk list"
  (define chunks (scenario-streaming '("Hello" " world" "!")))
  (check-equal? (length chunks) 4)
  (check-equal? (stream-chunk-delta-text (car chunks)) "Hello")
  (check-true (stream-chunk-done? (last chunks))))

(test-case "scenario-streaming-with-tools: produces tool chunks"
  (define chunks (scenario-streaming-with-tools '(("bash" "{\"command\":\"ls\"}"))))
  (check-equal? (length chunks) 2)
  (check-true (stream-chunk-done? (last chunks))))

;; ---------------------------------------------------------------------------
;; make-scenario-provider -- text response
;; ---------------------------------------------------------------------------

(test-case "provider: returns text response via send"
  (define-values (prov cap) (make-scenario-provider (list (scenario-text "Hello"))))
  (define req (make-model-request '() '() (hash)))
  (define resp (provider-send prov req))
  (check-equal? (hash-ref (car (model-response-content resp)) 'text) "Hello")
  (check-equal? (length (captured-requests cap)) 1))

(test-case "provider: returns text response via stream"
  (define-values (prov cap) (make-scenario-provider (list (scenario-text "Stream test"))))
  (define req (make-model-request '() '() (hash)))
  (define chunks (provider-stream prov req))
  (define chunk-list (for/list ([v (in-producer chunks #f)]) v))
  (check-true (>= (length chunk-list) 1))
  (check-true (stream-chunk-done? (last chunk-list))))

;; ---------------------------------------------------------------------------
;; make-scenario-provider -- tool-call response
;; ---------------------------------------------------------------------------

(test-case "provider: returns tool-call response"
  (define-values (prov cap) (make-scenario-provider (list (scenario-tool-call "grep"))))
  (define resp (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp) 'tool-calls)
  (check-equal? (hash-ref (car (model-response-content resp)) 'name) "grep"))

;; ---------------------------------------------------------------------------
;; make-scenario-provider -- multi-turn
;; ---------------------------------------------------------------------------

(test-case "provider: multi-turn sequence"
  (define-values (prov cap)
    (make-scenario-provider
     (list (scenario-text "first") (scenario-text "second") (scenario-text "third"))))
  (define r1 (provider-send prov (make-model-request '() '() (hash))))
  (define r2 (provider-send prov (make-model-request '() '() (hash))))
  (define r3 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (hash-ref (car (model-response-content r1)) 'text) "first")
  (check-equal? (hash-ref (car (model-response-content r2)) 'text) "second")
  (check-equal? (hash-ref (car (model-response-content r3)) 'text) "third"))

(test-case "provider: repeats last response after exhaustion"
  (define-values (prov cap)
    (make-scenario-provider (list (scenario-text "only"))))
  (provider-send prov (make-model-request '() '() (hash)))
  (define r2 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (hash-ref (car (model-response-content r2)) 'text) "only"))

;; ---------------------------------------------------------------------------
;; make-scenario-provider -- error
;; ---------------------------------------------------------------------------

(test-case "provider: raises error on send"
  (define-values (prov cap)
    (make-scenario-provider (list (scenario-error "boom" #:category 'server-error #:status 500))))
  (check-exn
   exn:fail?
   (lambda ()
     (provider-send prov (make-model-request '() '() (hash))))))

(test-case "provider: raises rate-limit error"
  (define-values (prov cap)
    (make-scenario-provider (list (scenario-rate-limit))))
  (check-exn
   exn:fail?
   (lambda ()
     (provider-send prov (make-model-request '() '() (hash))))))

;; ---------------------------------------------------------------------------
;; Request body capture
;; ---------------------------------------------------------------------------

(test-case "provider: captures request bodies"
  (define-values (prov cap)
    (make-scenario-provider
     (list (scenario-text "a") (scenario-text "b"))))
  (provider-send prov (make-model-request (list (hash 'role 'user)) '() (hash)))
  (provider-send prov (make-model-request (list (hash 'role 'assistant)) '() (hash)))
  (define caps (captured-requests cap))
  (check-equal? (length caps) 2)
  (clear-captured! cap)
  (check-equal? (length (captured-requests cap)) 0))

;; ---------------------------------------------------------------------------
;; scenario-eof (F2)
;; ---------------------------------------------------------------------------

(test-case "scenario-eof: send returns empty response"
  (define-values (prov cap)
    (make-scenario-provider (list (scenario-eof))))
  (define resp (provider-send prov (make-model-request '() '() (hash))))
  (check-true (model-response? resp))
  (check-equal? (model-response-content resp) '())
  (check-equal? (model-response-stop-reason resp) 'stop))

(test-case "scenario-eof: stream returns empty list"
  (define-values (prov cap)
    (make-scenario-provider (list (scenario-eof))))
  (define gen (provider-stream prov (make-model-request '() '() (hash))))
  ;; Generator yields #f immediately (no chunks)
  (define chunks (for/list ([v (in-producer gen #f)]) v))
  (check-true (null? chunks)))
