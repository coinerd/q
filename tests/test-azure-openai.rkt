#lang racket

;; test-azure-openai.rkt — tests for llm/azure-openai.rkt
;; v0.16.0 Wave 2: provider construction, response parsing, streaming

(require rackunit
         rackunit/text-ui
         "../llm/azure-openai.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt")

(define-test-suite
 azure-openai-tests
 (test-case "provider construction requires api-key"
   (check-exn exn:fail? (lambda () (make-azure-openai-provider (hasheq)))))
 (test-case "provider construction succeeds with required config"
   (define p
     (make-azure-openai-provider (hasheq 'api-key
                                         "test-key"
                                         'model
                                         "gpt-4"
                                         'base-url
                                         "https://test.openai.azure.com"
                                         'api-version
                                         "2024-02-15-preview")))
   (check-equal? (provider-name p) "Azure OpenAI"))
 (test-case "openai-parse-response-from-jsexpr: injects model name"
   (define js
     (hasheq 'choices
             (list (hasheq 'message (hasheq 'content "Hello") 'finish_reason "stop"))
             'usage
             (hasheq 'prompt_tokens 5 'completion_tokens 3 'total_tokens 8)))
   (define resp (openai-parse-response-from-jsexpr js "my-model"))
   (check-equal? (model-response-model resp) "my-model")
   (check-equal? (model-response-stop-reason resp) 'stop))
 (test-case "openai-parse-response-from-jsexpr: preserves existing model"
   (define js
     (hasheq 'model
             "existing"
             'choices
             (list (hasheq 'message (hasheq 'content "Hi") 'finish_reason "stop"))
             'usage
             (hasheq 'prompt_tokens 1 'completion_tokens 1 'total_tokens 2)))
   (define resp (openai-parse-response-from-jsexpr js "override"))
   (check-equal? (model-response-model resp) "existing"))
 (test-case "openai-parse-response-from-jsexpr: handles empty choices"
   (define js
     (hasheq 'choices '() 'usage (hasheq 'prompt_tokens 0 'completion_tokens 0 'total_tokens 0)))
   (define resp (openai-parse-response-from-jsexpr js "test"))
   (check-equal? (model-response-content resp) '()))
 (test-case "check-azure-status! raises on non-200"
   (check-exn exn:fail? (lambda () (check-azure-status! #"HTTP/1.1 401 Unauthorized" #"{}"))))
 (test-case "check-azure-status! passes on 200"
   (check-not-exn (lambda () (check-azure-status! #"HTTP/1.1 200 OK" #"{}")))))

(run-tests azure-openai-tests)
