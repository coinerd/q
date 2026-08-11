#lang racket/base

;; @speed fast
;; @suite arch
;; BOUNDARY: architecture

(require rackunit
         racket/list
         racket/runtime-path
         racket/string
         "helpers/provider-change-locality.rkt")

(define-runtime-path repo-root "..")
(define-runtime-path policy-path "../docs/architecture/provider-change-locality-policy.rktd")

(define policy (load-provider-locality-policy policy-path))

(define expected-protocol-policy
  '((anthropic ("llm/anthropic.rkt" "llm/anthropic-helpers.rkt"
                                    "llm/anthropic/format.rkt"
                                    "llm/anthropic/sse.rkt")
               ("content_block_delta" "input_json_delta" "message_start"))
    (gemini ("llm/gemini.rkt") ("usageMetadata" "functionCall" "candidates"))
    (openai-compatible ("llm/openai-compatible.rkt") ("reasoning_content" "finish_reason"))
    (azure-openai ("llm/azure-openai.rkt") ("api-version" "api-version="))))

(define expected-neutral-policy
  '(("llm/http-helpers.rkt" (make-provider-http-request check-provider-status! translate-stop-reason)
                            (C1 C5 C6))
    ("llm/stream.rkt" (stream-sse-events parse-sse-line
                                         parse-sse-lines
                                         sse-done?
                                         close-port-after-stream
                                         accumulate-tool-call-deltas
                                         call-with-request-timeout
                                         read-line/timeout
                                         read-response-body/timeout
                                         current-http-request-timeout)
                      (C2 C3 C7 C8))
    ("llm/provider-errors.rkt" (provider-error classify-http-status raise-provider-error) (C4))))

(test-case "W3-B1: policy is versioned, complete, and pins only W0 C1-C8 neutral helpers"
  (check-equal? (provider-locality-policy-version policy) 1)
  (check-equal? (map provider-protocol-name (provider-locality-policy-protocols policy))
                '(anthropic gemini openai-compatible azure-openai))
  (check-equal? (for/list ([protocol (in-list (provider-locality-policy-protocols policy))])
                  (list (provider-protocol-name protocol)
                        (provider-protocol-owners protocol)
                        (provider-protocol-markers protocol)))
                expected-protocol-policy)
  (check-equal? (for/list ([helper (in-list (provider-locality-policy-neutral-helpers policy))])
                  (list (neutral-helper-module helper)
                        (neutral-helper-primitives helper)
                        (neutral-helper-evidence helper)))
                expected-neutral-policy)
  (check-equal?
   (sort (remove-duplicates (append-map neutral-helper-evidence
                                        (provider-locality-policy-neutral-helpers policy)))
         symbol<?)
   '(C1 C2 C3 C4 C5 C6 C7 C8))
  (check-equal? (check-provider-locality-policy policy repo-root) '()))

(test-case "W3-B2: current production LLM sources respect provider ownership and neutrality"
  (check-equal? (check-provider-change-locality policy (production-llm-source-units repo-root)) '()))

(test-case "W3-B3: positive probes accept every marker in an allowed ownership path"
  (for* ([protocol (in-list (provider-locality-policy-protocols policy))]
         [marker (in-list (provider-protocol-markers protocol))])
    (define owner (car (provider-protocol-owners protocol)))
    (check-equal?
     (check-provider-change-locality policy
                                     (list (source-unit owner (format "(define probe ~s)" marker))))
     '()
     (format "allowed owner rejected for ~a/~a" (provider-protocol-name protocol) marker))))

(test-case "W3-B4: negative probes reject protocol logic in a generic streaming module"
  (for ([protocol (in-list (provider-locality-policy-protocols policy))])
    (define marker (car (provider-protocol-markers protocol)))
    (define violations
      (check-provider-change-locality policy
                                      (list (source-unit "llm/stream.rkt"
                                                         (format "(define leaked ~s)" marker)))))
    (check-equal? (length violations) 1)
    (define violation (car violations))
    (check-equal? (locality-violation-reason violation) 'generic-stream-protocol)
    (define message (locality-violation->string violation))
    (check-true (string-contains? message "llm/stream.rkt"))
    (check-true (string-contains? message marker))
    (check-true (string-contains? message (symbol->string (provider-protocol-name protocol))))
    (for ([owner (in-list (provider-protocol-owners protocol))])
      (check-true (string-contains? message owner)
                  (format "developer message omitted allowed owner ~a" owner)))))

(test-case "W3-B5: negative probe rejects a marker in another provider adapter"
  (define anthropic (first (provider-locality-policy-protocols policy)))
  (define marker (car (provider-protocol-markers anthropic)))
  (define violations
    (check-provider-change-locality policy
                                    (list (source-unit "llm/gemini.rkt"
                                                       (format "(define leaked ~s)" marker)))))
  (check-equal? (length violations) 1)
  (check-equal? (locality-violation-reason (car violations)) 'wrong-provider-owner)
  (check-true (string-contains? (locality-violation->string (car violations))
                                "allowed ownership path")))

(test-case "W3-B6: source parsing ignores comments and requires a real datum marker"
  (check-equal?
   (check-provider-change-locality
    policy
    (list (source-unit "llm/stream.rkt"
                       ";; content_block_delta is documentation only\n(define neutral #t)")))
   '()))
