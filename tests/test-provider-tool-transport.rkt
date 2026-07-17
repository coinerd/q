#lang racket

;; @speed fast
;; @suite security
;; BOUNDARY: integration

(require rackunit
         json
         "../agent/loop-messages.rkt"
         "../tools/builtins/provider-hash-bridge.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt"
         (only-in "../util/message/provider-transport.rkt"
                  make-provider-tool-call
                  make-provider-assistant-message
                  make-provider-tool-result-message
                  provider-tool-call-type?
                  provider-tool-stop-reason?
                  provider-completion-stop-reason?)
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../runtime/settings.rkt"
         (only-in "../runtime/context-assembly/session-walk.rkt" summarize-tool-result))

(define (msg id role parts)
  (make-message id #f role 'message parts 0 (hasheq)))

(define canonical-history
  (list (msg "s" 'system (list (make-text-part "system")))
        (msg "u" 'user (list (make-text-part "use tool")))
        (msg "a"
             'assistant
             (list (make-text-part "checking")
                   (make-tool-call-part "call-42" "read" (hasheq 'path "README.md"))))
        (msg "t" 'tool (list (make-tool-result-part "call-42" "contents" #f)))
        (msg "u2" 'user (list (make-text-part "continue")))))

(define canonical-wire (build-raw-messages canonical-history))

(define (message-parts message)
  (define content (hash-ref message 'content '()))
  (if (list? content)
      content
      '()))

(define production-shaped-tool-responses
  (list
   (cons
    'openai
    (openai-parse-response
     (string->jsexpr
      (string-append
       "{\"model\":\"gpt-test\",\"choices\":[{\"finish_reason\":\"tool_calls\","
       "\"message\":{\"content\":\"checking\",\"tool_calls\":[{\"id\":\"openai-call\","
       "\"function\":{\"name\":\"read\",\"arguments\":\"{\\\"path\\\":\\\"README.md\\\"}\"}}]}}]}"))))
   (cons 'anthropic
         (anthropic-parse-response
          (string->jsexpr
           (string-append
            "{\"model\":\"claude-test\",\"stop_reason\":\"tool_use\",\"content\":["
            "{\"type\":\"text\",\"text\":\"checking\"},{\"type\":\"tool_use\","
            "\"id\":\"anthropic-call\",\"name\":\"read\",\"input\":{\"path\":\"README.md\"}}]}"))))
   (cons
    'gemini
    (gemini-parse-response
     (string->jsexpr
      (string-append
       "{\"modelVersion\":\"gemini-test\",\"candidates\":[{\"finishReason\":\"STOP\","
       "\"content\":{\"parts\":[{\"text\":\"checking\"},{\"functionCall\":{\"id\":\"gemini-call\",\"name\":\"read\","
       "\"args\":{\"path\":\"README.md\"}}}]}}]}"))))))

(define (run-parser-response-through-child initial-response)
  (define requests (box '()))
  (define sends (box 0))
  (define provider
    (make-provider
     (lambda () "production-parser")
     (lambda () (hasheq))
     (lambda (request)
       (set-box! requests (append (unbox requests) (list request)))
       (set-box! sends (add1 (unbox sends)))
       (if (= (unbox sends) 1)
           initial-response
           (make-model-response (list (hasheq 'type "text" 'text "done")) (hasheq) "test" 'stop)))
     (lambda (_request) '())))
  (define ctx
    (make-exec-context #:runtime-settings
                       (q-settings (hash) (hash) (hasheq 'provider provider 'model "test"))))
  (values
   (tool-spawn-subagent (hasheq 'task "read README" 'tools '("read") 'capabilities '(read-only)) ctx)
   (unbox requests)))

(test-case "actual adapter parser output completes correlated child request two"
  (for ([fixture (in-list production-shaped-tool-responses)])
    (define-values (result requests) (run-parser-response-through-child (cdr fixture)))
    (check-false (tool-result-is-error? result) (format "~a child failed" (car fixture)))
    (check-equal? (length requests) 2 (format "~a must send request two" (car fixture)))
    (define second-messages (model-request-messages (cadr requests)))
    (define assistant
      (findf (lambda (message) (pair? (hash-ref message 'tool_calls '()))) second-messages))
    (define tool
      (findf (lambda (message) (equal? (hash-ref message 'role #f) "tool")) second-messages))
    (check-not-false assistant)
    (check-not-false tool)
    (check-equal? (hash-ref (car (hash-ref assistant 'tool_calls)) 'id) (hash-ref tool 'tool_call_id))
    (check-equal? (hash-ref assistant 'content #f) "checking")
    (define native-body
      (case (car fixture)
        [(openai) (openai-build-request-body (cadr requests))]
        [(anthropic) (anthropic-build-request-body (cadr requests))]
        [(gemini) (gemini-build-request-body (cadr requests))]))
    (define native-json (jsexpr->string native-body))
    (case (car fixture)
      [(openai)
       (check-true (string-contains? native-json (hash-ref tool 'tool_call_id)))
       (check-true (string-contains? native-json "tool_call_id"))]
      [(anthropic)
       (check-true (string-contains? native-json (hash-ref tool 'tool_call_id)))
       (check-true (string-contains? native-json "tool_result"))]
      [(gemini)
       (check-true (string-contains? native-json "functionCall"))
       (check-true (string-contains? native-json "functionResponse"))
       (check-true (string-contains? native-json "read"))
       (check-true (string-contains? native-json (hash-ref tool 'tool_call_id)))])))

(test-case "canonical transport emits hyphenated protocol and accepts legacy only at ingress"
  (define call (make-provider-tool-call "id-1" "read" (hasheq 'path "x")))
  (define assistant (make-provider-assistant-message "" (list call)))
  (define result (make-provider-tool-result-message "id-1" "ok"))
  (check-equal? (hash-ref (car (hash-ref assistant 'tool_calls)) 'type) "function")
  (check-equal? (hash-ref result 'tool_call_id) "id-1")
  (check-true (provider-tool-call-type? "tool-call"))
  (check-true (provider-tool-call-type? "tool_call"))
  (check-true (provider-tool-stop-reason? 'tool-calls))
  (check-true (provider-tool-stop-reason? 'tool_calls))
  (check-true (provider-completion-stop-reason? 'stop))
  (check-false (provider-completion-stop-reason? 'length)))

(test-case "subagent bridge delegates to canonical parent message transport"
  (check-equal? (messages->provider-hashes canonical-history) canonical-wire)
  (define assistant (list-ref canonical-wire 2))
  (define tool (list-ref canonical-wire 3))
  (check-equal? (hash-ref (car (hash-ref assistant 'tool_calls)) 'id) "call-42")
  (check-equal? (hash-ref tool 'tool_call_id) "call-42")
  (check-false (regexp-match? #rx"tool_call|tool_result"
                              (jsexpr->string (hash-ref assistant 'content "")))))

(test-case "role merging never discards assistant tool calls"
  (define first
    (make-provider-assistant-message "first" (list (make-provider-tool-call "c1" "read" (hasheq)))))
  (define second
    (make-provider-assistant-message "second" (list (make-provider-tool-call "c2" "read" (hasheq)))))
  (define merged (merge-consecutive-roles (list first second)))
  (check-equal? (length merged) 2)
  (check-equal? (map (lambda (message) (hash-ref (car (hash-ref message 'tool_calls)) 'id)) merged)
                '("c1" "c2")))

(test-case "summarized large tool result preserves correlation through all native builders"
  (define large-result
    (make-message "large-tool"
                  "a"
                  'tool
                  'tool-result
                  (list (make-tool-result-part "large-call" (make-string 9000 #\X) #f))
                  0
                  (hasheq)))
  (define summarized (summarize-tool-result large-result))
  (check-true (tool-result-part? (car (message-content summarized))))
  (check-equal? (tool-result-part-tool-call-id (car (message-content summarized))) "large-call")
  (define history
    (list (msg "a" 'assistant (list (make-tool-call-part "large-call" "read" (hasheq 'path "large"))))
          summarized
          (msg "u" 'user (list (make-text-part "continue")))))
  (define wire (build-raw-messages history))
  (define openai-body
    (openai-build-request-body (make-model-request wire '() (hasheq 'model "gpt-test"))))
  (check-equal? (hash-ref (list-ref (hash-ref openai-body 'messages) 1) 'tool_call_id) "large-call")
  (define anthropic-body
    (anthropic-build-request-body (make-model-request wire '() (hasheq 'model "claude-test"))))
  (check-true (string-contains? (jsexpr->string anthropic-body) "large-call"))
  (define gemini-body
    (gemini-build-request-body (make-model-request wire '() (hasheq 'model "gemini-test"))))
  (check-true (string-contains? (jsexpr->string gemini-body) "functionResponse"))
  (check-true (string-contains? (jsexpr->string gemini-body) "read")))

(test-case "OpenAI request body emits stable top-level call/result IDs and JSON arguments"
  (define body
    (openai-build-request-body (make-model-request canonical-wire '() (hasheq 'model "gpt-test"))))
  (define messages (hash-ref body 'messages))
  (define assistant (list-ref messages 2))
  (define tool (list-ref messages 3))
  (define call (car (hash-ref assistant 'tool_calls)))
  (check-equal? (hash-ref call 'id) "call-42")
  (check-true (string? (hash-ref (hash-ref call 'function) 'arguments)))
  (check-equal? (hash-ref tool 'tool_call_id) "call-42")
  (check-not-exn (lambda () (jsexpr->bytes body))))

(test-case "Anthropic request body correlates tool_use and tool_result"
  (define body
    (anthropic-build-request-body
     (make-model-request canonical-wire '() (hasheq 'model "claude-test"))))
  (define messages (hash-ref body 'messages))
  (check-equal? (hash-ref body 'system #f) "system")
  (check-false (ormap (lambda (message) (equal? (hash-ref message 'role #f) "system")) messages))
  (define tool-use
    (for*/first ([message (in-list messages)]
                 [part (in-list (message-parts message))]
                 #:when (equal? (hash-ref part 'type #f) "tool_use"))
      part))
  (define tool-result
    (for*/first ([message (in-list messages)]
                 [part (in-list (message-parts message))]
                 #:when (equal? (hash-ref part 'type #f) "tool_result"))
      part))
  (check-equal? (hash-ref tool-use 'id) "call-42")
  (check-equal? (hash-ref tool-result 'tool_use_id) "call-42")
  (check-not-exn (lambda () (jsexpr->bytes body))))

(test-case "Gemini request body correlates native function names from stable internal ID"
  (define body
    (gemini-build-request-body (make-model-request canonical-wire '() (hasheq 'model "gemini-test"))))
  (define contents (hash-ref body 'contents))
  (check-equal? (hash-ref (hash-ref body 'systemInstruction) 'parts) (list (hasheq 'text "system")))
  (check-false (ormap (lambda (content)
                        (equal? (hash-ref content 'parts '()) (list (hasheq 'text "system"))))
                      contents))
  (define call
    (for*/first ([content (in-list contents)]
                 [part (in-list (hash-ref content 'parts '()))]
                 #:when (hash-has-key? part 'functionCall))
      (hash-ref part 'functionCall)))
  (define result
    (for*/first ([content (in-list contents)]
                 [part (in-list (hash-ref content 'parts '()))]
                 #:when (hash-has-key? part 'functionResponse))
      (hash-ref part 'functionResponse)))
  (check-equal? (hash-ref call 'name) "read")
  (check-equal? (hash-ref result 'name) "read")
  (check-not-exn (lambda () (jsexpr->bytes body))))

(test-case "Gemini preserves IDs for duplicate native function names"
  (define duplicate-wire
    (list (make-provider-assistant-message
           ""
           (list (make-provider-tool-call "same-1" "read" (hasheq 'path "a"))
                 (make-provider-tool-call "same-2" "read" (hasheq 'path "b"))))
          (make-provider-tool-result-message "same-1" "A")
          (make-provider-tool-result-message "same-2" "B")
          (hasheq 'role "user" 'content "continue")))
  (define body
    (gemini-build-request-body (make-model-request duplicate-wire '() (hasheq 'model "gemini-test"))))
  (define calls
    (for*/list ([content (in-list (hash-ref body 'contents))]
                [part (in-list (hash-ref content 'parts '()))]
                #:when (hash-has-key? part 'functionCall))
      (hash-ref part 'functionCall)))
  (define responses
    (for*/list ([content (in-list (hash-ref body 'contents))]
                [part (in-list (hash-ref content 'parts '()))]
                #:when (hash-has-key? part 'functionResponse))
      (hash-ref part 'functionResponse)))
  (check-equal? (map (lambda (call) (hash-ref call 'name)) calls) '("read" "read"))
  (check-equal? (map (lambda (call) (hash-ref call 'id)) calls) '("same-1" "same-2"))
  (check-equal? (map (lambda (response) (hash-ref response 'id)) responses) '("same-1" "same-2")))

(test-case "Gemini parser marks functionCall response as canonical tool-calls stop"
  (gemini-reset-tool-id-counter!)
  (define parsed
    (gemini-parse-response
     (hasheq
      'candidates
      (list (hasheq
             'finishReason
             "STOP"
             'content
             (hasheq 'parts
                     (list (hasheq 'text "checking")
                           (hasheq 'functionCall
                                   (hasheq 'name "read" 'args (hasheq 'path "README.md"))))))))))
  (check-equal? (model-response-stop-reason parsed) 'tool-calls)
  (check-equal? (map (lambda (part) (hash-ref part 'type)) (model-response-content parsed))
                '("text" "tool-call")))
