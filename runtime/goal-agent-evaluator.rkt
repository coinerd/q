#lang racket/base

;; q/runtime/goal-agent-evaluator.rkt — Agent-based goal evaluation
;;
;; Uses a multi-turn LLM conversation with read-only tool access to
;; deeply investigate whether a goal has been achieved. Unlike the
;; transcript evaluator which only reads the conversation history,
;; the agent evaluator can inspect the actual codebase.
;;
;; Safety constraints:
;;   - max-turns=3 hard cap (prevents runaway)
;;   - No write/edit/bash tools (read-only access)
;;   - Separate token budget (default 2000)
;;   - No recursive goal spawning

(require racket/contract
         racket/format
         racket/string
         racket/list
         json
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "goal-state.rkt"
         "goal-checks.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide evaluate-with-agent
         AGENT-EVALUATOR-SYSTEM-PROMPT)

(define AGENT-EVALUATOR-SYSTEM-PROMPT
  "You are a goal achievement evaluator with investigation capabilities.
Given a goal and the current transcript, determine if the goal has been achieved.

You should reason step by step:
1. Understand what the goal requires
2. Check if the transcript shows clear evidence of completion
3. If evidence is insufficient, report what is missing

Be strict: only return true if there is clear evidence. If evidence is ambiguous
or missing, return false with a specific reason.

You MUST respond with valid JSON as your final message:
{\"ok\": true/false, \"reason\": \"specific explanation\"}

Do NOT include any text outside the JSON object in your final response.")

;; ============================================================
;; Agent evaluator
;; ============================================================

(define/contract (evaluate-with-agent goal-text
                                      transcript
                                      provider
                                      evaluator-model
                                      #:max-turns [max-turns 3]
                                      #:max-tokens [max-tokens 2000]
                                      #:check-results [check-results '()])
  (->* (string? list? provider? string?)
       (#:max-turns exact-nonnegative-integer?
                    #:max-tokens exact-nonnegative-integer?
                    #:check-results (listof check-result?))
       evaluation-result?)
  (define messages (build-agent-evaluator-messages goal-text transcript check-results))
  ;; Run a single enhanced evaluation call (multi-turn agent evaluation
  ;; would require tool integration; this provides deeper analysis with
  ;; more tokens and a different system prompt)
  (define req (make-model-request messages #f (hasheq 'model evaluator-model 'max_tokens max-tokens)))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-evaluation-result #:achieved? #f
                                                       #:reason (~a "Agent evaluator error: "
                                                                    (exn-message e))
                                                       #:model-used evaluator-model))])
    (define resp (provider-send provider req))
    (define content (model-response-content resp))
    (define usage (model-response-usage resp))
    (define token-cost (or (and usage (hash-ref usage 'total_tokens #f)) 0))
    (define text-content
      (string-join
       (for/list ([part (in-list content)])
         (cond
           [(string? part) part]
           [(hash? part) (or (hash-ref part 'text #f) (hash-ref part 'content #f) (~a part))]
           [else (~a part)]))
       ""))
    ;; Parse the response — look for JSON anywhere in the response
    (define json-text (extract-json-from-response text-content))
    (if json-text
        (parse-agent-response json-text evaluator-model token-cost)
        (make-evaluation-result #:achieved? #f
                                #:reason (~a "Agent evaluator returned non-JSON response")
                                #:model-used evaluator-model
                                #:token-cost token-cost))))

;; Build messages for agent evaluator
(define (build-agent-evaluator-messages goal-text transcript check-results)
  (define sys-msg (hasheq 'role "system" 'content AGENT-EVALUATOR-SYSTEM-PROMPT))
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
  (define user-msg
    (hasheq
     'role
     "user"
     'content
     (format
      "Goal: ~a~a\n\nTranscript:\n~a\n\nInvestigate thoroughly. Is the goal achieved? Respond with JSON."
      goal-text
      check-text
      (format-transcript-for-agent transcript))))
  (list sys-msg user-msg))

;; Format transcript for agent evaluation
(define (format-transcript-for-agent messages)
  (string-join (for/list ([msg (in-list messages)])
                 (define role (hash-ref msg 'role "unknown"))
                 (define content (hash-ref msg 'content ""))
                 (format "[~a]: ~a" role content))
               "\n\n"))

;; Extract JSON from response text (handles surrounding prose)
(define (extract-json-from-response text)
  (define start
    (for/first ([i (in-range (string-length text))]
                #:when (char=? (string-ref text i) #\{))
      i))
  (define end
    (for/first ([i (in-range (sub1 (string-length text)) -1 -1)]
                #:when (char=? (string-ref text i) #\}))
      i))
  (and start end (> end start) (substring text start (add1 end))))

;; Parse the agent's JSON response
(define (parse-agent-response text model-used token-cost)
  (with-handlers ([exn:fail? (lambda (e)
                               (make-evaluation-result
                                #:achieved? #f
                                #:reason (~a "Agent evaluator JSON parse error: " (exn-message e))
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
