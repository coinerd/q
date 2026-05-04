#lang racket/base

;; tests/test-typed-model.rkt — type-verification and regression tests for llm/model.rkt
;; v0.29.3 W0: Verify model-request, model-response, stream-chunk struct behavior
;; before Typed Racket migration.

(require rackunit
         (only-in "../llm/model.rkt"
                  model-request
                  model-request?
                  model-request-messages
                  model-request-tools
                  model-request-settings
                  make-model-request
                  model-request->jsexpr
                  jsexpr->model-request

                  model-response
                  model-response?
                  model-response-content
                  model-response-usage
                  model-response-model
                  model-response-stop-reason
                  make-model-response
                  model-response->jsexpr
                  jsexpr->model-response

                  stream-chunk
                  stream-chunk?
                  stream-chunk-delta-text
                  stream-chunk-delta-tool-call
                  stream-chunk-delta-thinking
                  stream-chunk-usage
                  stream-chunk-done?
                  stream-chunk-finish-reason
                  make-stream-chunk))

;; ============================================================
;; model-request tests
;; ============================================================

(check-true (model-request?
             (make-model-request
              (list (hasheq 'role "user" 'content "hello"))
              #f
              (hasheq 'model "gpt-4")))
            "model-request: basic construction")

(check-equal? (model-request-messages
               (make-model-request
                (list (hasheq 'role "user" 'content "hi"))
                #f
                (hasheq)))
              (list (hasheq 'role "user" 'content "hi"))
              "model-request: messages accessor")

(check-false (model-request-tools
              (make-model-request '() #f (hasheq)))
             "model-request: tools is #f when not provided")

(check-equal? (model-request-settings
               (make-model-request '() #f (hasheq 'temperature 0.7)))
              (hasheq 'temperature 0.7)
              "model-request: settings accessor")

;; ============================================================
;; model-request serialization roundtrip
;; ============================================================

(let* ([msg (hasheq 'role "user" 'content "test")]
       [tools (list (hasheq 'type "function" 'name "bash"))]
       [req (make-model-request (list msg) tools (hasheq 'model "gpt-4"))]
       [js (model-request->jsexpr req)]
       [req2 (jsexpr->model-request js)])
  (check-equal? (model-request-messages req2) (list msg)
                "model-request roundtrip: messages preserved")
  (check-not-false (model-request-tools req2)
                   "model-request roundtrip: tools preserved")
  (check-equal? (hash-ref (model-request-settings req2) 'model) "gpt-4"
                "model-request roundtrip: settings preserved"))

;; Roundtrip without tools
(let* ([req (make-model-request '() #f (hasheq))]
       [js (model-request->jsexpr req)]
       [req2 (jsexpr->model-request js)])
  (check-false (model-request-tools req2)
               "model-request roundtrip: tools #f preserved"))

;; ============================================================
;; model-response tests
;; ============================================================

(check-true (model-response?
             (make-model-response
              (list (hasheq 'type "text" 'text "Hello"))
              (hasheq 'prompt-tokens 10 'completion-tokens 5)
              "gpt-4"
              'stop))
            "model-response: basic construction")

(check-equal? (model-response-stop-reason
               (make-model-response '() (hasheq) "model" 'length))
              'length
              "model-response: stop-reason accessor")

;; ============================================================
;; model-response serialization roundtrip
;; ============================================================

(let* ([content (list (hasheq 'type "text" 'text "world"))]
       [usage (hasheq 'prompt-tokens 20 'completion-tokens 10)]
       [resp (make-model-response content usage "gpt-4" 'stop)]
       [js (model-response->jsexpr resp)]
       [resp2 (jsexpr->model-response js)])
  (check-equal? (model-response-content resp2) content
                "model-response roundtrip: content preserved")
  (check-equal? (model-response-model resp2) "gpt-4"
                "model-response roundtrip: model preserved")
  (check-equal? (model-response-stop-reason resp2) 'stop
                "model-response roundtrip: stop-reason preserved as symbol"))

;; ============================================================
;; stream-chunk tests
;; ============================================================

(check-true (stream-chunk? (make-stream-chunk "hello" #f #f #f))
            "stream-chunk: basic construction")

(check-equal? (stream-chunk-delta-text (make-stream-chunk "hi" #f #f #f))
              "hi"
              "stream-chunk: delta-text accessor")

(check-false (stream-chunk-delta-tool-call (make-stream-chunk "hi" #f #f #f))
             "stream-chunk: delta-tool-call is #f")

;; Optional keyword args
(let ([chunk (make-stream-chunk "text" #f #f #f
                                #:delta-thinking "thinking..."
                                #:finish-reason 'stop)])
  (check-equal? (stream-chunk-delta-thinking chunk) "thinking..."
                "stream-chunk: delta-thinking keyword")
  (check-equal? (stream-chunk-finish-reason chunk) 'stop
                "stream-chunk: finish-reason keyword"))

;; Done chunk
(let ([chunk (make-stream-chunk #f #f (hasheq 'total-tokens 100) #t)])
  (check-true (stream-chunk-done? chunk) "stream-chunk: done? is #t")
  (check-false (stream-chunk-delta-text chunk) "stream-chunk: done has no delta-text"))

;; ============================================================
;; Edge cases
;; ============================================================

;; Empty messages list
(check-not-exn
 (lambda () (make-model-request '() #f (hasheq)))
 "edge: empty messages list")

;; Empty response content
(check-not-exn
 (lambda () (make-model-response '() (hasheq) "model" 'stop))
 "edge: empty response content")

;; model-response with tool_calls stop reason
(check-equal? (model-response-stop-reason
               (make-model-response '() (hasheq) "model" 'tool-calls))
              'tool-calls
              "edge: tool-calls stop reason")
