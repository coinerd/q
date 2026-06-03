#lang racket/base

;; q/runtime/goal-evaluator.rkt — Transcript evaluation via LLM
;;
;; Evaluates whether a goal has been achieved by sending the recent
;; conversation transcript to a cheap LLM and parsing its JSON response.
;; Returns an evaluation-result struct.

(require racket/contract
         racket/match
         racket/format
         racket/string
         json
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         "../../util/message/message.rkt"
         "../../util/content/content-parts.rkt"
         "goal-state.rkt"
         "goal-checks.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide EVALUATOR-SYSTEM-PROMPT
         evaluate-transcript
         parse-evaluator-response)

(define EVALUATOR-SYSTEM-PROMPT
  "You are a goal achievement evaluator. Given a goal condition and a conversation
transcript, determine if the goal has been achieved. Be strict: only return true if
there is clear evidence in the transcript that the goal condition is met. If evidence
is ambiguous or missing, return false with a specific reason explaining what is missing.

You MUST respond with valid JSON:
{\"ok\": true/false, \"reason\": \"specific explanation\"}
Do NOT include any text outside the JSON object.")

;; ============================================================
;; Evaluator
;; ============================================================

(define/contract (evaluate-transcript goal-text
                                      transcript
                                      provider
                                      evaluator-model
                                      #:max-tokens [max-tokens 256]
                                      #:check-results [check-results '()])
  (->* (string? list? provider? string?)
       (#:max-tokens exact-nonnegative-integer? #:check-results (listof check-result?))
       evaluation-result?)
  (define messages (build-evaluator-messages goal-text transcript check-results))
  (define req (make-model-request messages #f (hasheq 'model evaluator-model 'max_tokens max-tokens)))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-evaluation-result #:achieved? #f
                                                       #:reason (~a "Evaluator error: "
                                                                    (exn-message e))
                                                       #:model-used evaluator-model))])
    (define resp (provider-send provider req))
    (define content (model-response-content resp))
    (define usage (model-response-usage resp))
    (define token-cost (or (and usage (hash-ref usage 'total_tokens #f)) 0))
    (define text-content (extract-text-from-content content))
    (parse-evaluator-response text-content evaluator-model token-cost)))

;; Build the message list for the evaluator LLM call
(define (build-evaluator-messages goal-text transcript [check-results '()])
  (define sys-msg (hasheq 'role "system" 'content EVALUATOR-SYSTEM-PROMPT))
  (define check-text
    (if (null? check-results)
        ""
        (format "\n\nDeterministic check results:\n~a"
                (string-join (for/list ([cr (in-list check-results)])
                               (format "- ~a: ~a (exit ~a)~a"
                                       (check-result-label cr)
                                       (if (= (check-result-exit-code cr) 0) "PASS" "FAIL")
                                       (check-result-exit-code cr)
                                       (if (check-result-timed-out? cr) " [TIMED OUT]" "")))
                             "\n"))))
  (define goal-msg
    (hasheq 'role
            "user"
            'content
            (~a "Goal: " goal-text check-text "\n\nEvaluate the transcript below:")))
  (define transcript-text (format-transcript transcript))
  (define transcript-msg (hasheq 'role "assistant" 'content transcript-text))
  (list sys-msg goal-msg transcript-msg))

;; Format messages into readable transcript string
(define (format-transcript messages)
  (string-join (for/list ([msg (in-list messages)])
                 (define role
                   (cond
                     [(message? msg) (message-role msg)]
                     [(hash? msg) (hash-ref msg 'role "unknown")]
                     [else "unknown"]))
                 (define content
                   (cond
                     [(message? msg)
                      (string-join (for/list ([part (in-list (message-content msg))])
                                     (cond
                                       [(text-part? part) (text-part-text part)]
                                       [(string? part) part]
                                       [else (~a part)]))
                                   "")]
                     [(hash? msg) (hash-ref msg 'content "")]
                     [else ""]))
                 (format "[~a]: ~a" role content))
               "\n\n"))

;; Extract text from model response content list
(define (extract-text-from-content content)
  (string-join (for/list ([part (in-list content)])
                 (cond
                   [(string? part) part]
                   [(hash? part) (or (hash-ref part 'text #f) (hash-ref part 'content #f) (~a part))]
                   [else (~a part)]))
               ""))

;; Parse the evaluator's JSON response
(define/contract (parse-evaluator-response text model-used [token-cost 0])
  (->* (string? string?) (exact-nonnegative-integer?) evaluation-result?)
  (with-handlers ([exn:fail? (lambda (e)
                               (make-evaluation-result #:achieved? #f
                                                       #:reason (~a "Evaluator returned non-JSON: "
                                                                    (exn-message e))
                                                       #:model-used model-used
                                                       #:token-cost token-cost))])
    (define jx (string->jsexpr text))
    (define ok (hash-ref jx 'ok #f))
    (define reason (hash-ref jx 'reason "no reason provided"))
    (make-evaluation-result #:achieved? (and ok #t)
                            #:reason (if (string? reason)
                                         reason
                                         (~a reason))
                            #:model-used model-used
                            #:token-cost token-cost)))
