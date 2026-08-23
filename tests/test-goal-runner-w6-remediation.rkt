#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
(require rackunit
         racket/file
         racket/path
         racket/port
         racket/string
         racket/hash
         racket/system
         json
         (only-in "../runtime/goal/goal-runner.rkt" current-simulated-prompt-sink)
         (only-in "../util/message/message.rkt" message? message-kind message-content)
         (only-in "../util/content/content-parts.rkt" text-part-text)
         (only-in "../llm/model.rkt" make-model-response)
         (only-in "../llm/provider.rkt" make-provider)
         "../runtime/goal/goal-state.rkt"
         "../runtime/goal/goal-evidence.rkt"
         "../runtime/goal/goal-runner.rkt"
         "../runtime/session/session-store-goal-task.rkt")

(define (ok-provider [achieved? #t])
  (make-provider
   (lambda () "w6-eval")
   (lambda () (hash 'streaming #f 'token-counting #t))
   (lambda (_req)
     (make-model-response (list (hasheq 'type
                                        'text
                                        'text
                                        (if achieved?
                                            "{\"ok\": true, \"reason\": \"met\"}"
                                            "{\"ok\": false, \"reason\": \"missing evidence\"}")))
                          (hasheq 'total_tokens 1)
                          "w6-eval"
                          'stop))
   (lambda (_req)
     (make-model-response (list (hasheq 'type
                                        'text
                                        'text
                                        (if achieved?
                                            "{\"ok\": true, \"reason\": \"met\"}"
                                            "{\"ok\": false, \"reason\": \"missing evidence\"}")))
                          (hasheq 'total_tokens 1)
                          "w6-eval"
                          'stop))))

(define (one-response [text "worker evidence"])
  (list (hash 'messages (list (hasheq 'role 'assistant 'content text)))))

(module+ test
  (test-case "goal-run-simulated! rejects an empty turn response list"
    (check-exn #rx"non-empty"
               (lambda () (goal-run-simulated! "empty" (ok-provider) "w6-eval" '() #:max-turns 1))))

  (test-case "goal-run-simulated! exposes worker exceptions instead of hanging to timeout"
    (check-exn #rx"worker blew up"
               (lambda ()
                 (goal-run-simulated! "worker error"
                                      (ok-provider)
                                      "w6-eval"
                                      (list (hash 'explode "worker blew up"))
                                      #:max-turns 1))))

  (test-case "goal-loop prompts include mandatory operating rules"
    (define captured '())
    (define st
      (parameterize ([current-simulated-prompt-sink (lambda (prompt) (set! captured prompt))])
        (goal-run-simulated! "rules" (ok-provider) "w6-eval" (one-response) #:max-turns 1)))
    (check-true (string-contains? captured "SEQUENTIAL WAVES"))
    (check-true (string-contains? captured "BACKGROUND GATES"))
    (check-true (string-contains? captured "TURN CAP"))
    (check-true (string-contains? captured "RE-VERIFY AFTER BASE CHANGE")))

  (test-case "failed deterministic checks force not-achieved despite optimistic evaluator"
    (define check (make-goal-check #:label "false" #:command "false"))
    (define st
      (goal-run! "checks"
                 (ok-provider #t)
                 "w6-eval"
                 (lambda (_prompt)
                   (values #f (hash 'messages (list (hasheq 'role 'assistant 'content "done")))))
                 #:checks (list check)
                 #:max-turns 1))
    (check-eq? (goal-state-status st) 'failed)
    (check-true (string-contains? (goal-state-reason-text st) "check false failed")))

  (test-case "check completion appends evidence with provenance when session log is set"
    (define appended (box '()))
    (parameterize ([current-goal-session-log-path "w6-goal.jsonl"]
                   [current-append-entry!-gt (lambda (_path entry)
                                               (set-box! appended (cons entry (unbox appended))))]
                   [current-load-session-log-gt (lambda (_path) '())])
      (define check (make-goal-check #:label "true" #:command "true"))
      (define st
        (goal-run! "evidence"
                   (ok-provider #t)
                   "w6-eval"
                   (lambda (_prompt)
                     (values #f (hash 'messages (list (hasheq 'role 'assistant 'content "done")))))
                   #:checks (list check)
                   #:max-turns 1))
      (check-eq? (goal-state-status st) 'achieved)
      (define evidence-entries
        (filter (lambda (e) (and (message? e) (eq? (message-kind e) 'goal.evidence)))
                (reverse (unbox appended))))
      (check-equal? (length evidence-entries) 1)
      (define prov
        (hash->evidence-provenance
         (string->jsexpr (text-part-text (car (message-content (car evidence-entries)))))))
      (check-equal? (evidence-provenance-kind prov) 'check)
      (check-true (non-empty-string? (evidence-provenance-base-sha prov)))
      (check-true (non-empty-string? (evidence-provenance-tree-hash prov)))))

  (test-case "legacy last-evaluation only state is not double-counted"
    (define eval
      (make-evaluation-result #:achieved? #f #:reason "same" #:model-used "w6-eval" #:token-cost 0))
    (define st (make-goal-state #:goal-text "legacy" #:last-evaluation eval))
    (check-equal? (length (collect-evaluations st)) 1)))

(define (goal-state-reason-text st)
  (define er (goal-state-last-evaluation st))
  (if er
      (evaluation-result-reason er)
      ""))
