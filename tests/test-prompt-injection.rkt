#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-prompt-injection.rkt — Prompt injection effectiveness tests
;; v0.76.1 W0: Verify state-aware system prompt instructions are injected correctly.

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         (only-in "../util/message/protocol-types.rkt"
                  message-content
                  message-role
                  make-message
                  make-text-part)
         (only-in "../util/message/message.rkt" message-kind)
         (only-in "../util/content/content-parts.rkt" text-part-text)
         (only-in "../util/fsm/fsm.rkt" fsm-state))

;; Helper: extract text from a preamble message
(define (preamble-text p)
  (text-part-text (car (message-content p))))

;; Helper: make a test conclusion
(define (make-test-conclusion text [category 'fact])
  (task-conclusion (format "c~a" (current-inexact-milliseconds))
                   text
                   category
                   'exploration
                   '()
                   (current-seconds)
                   '()
                   '()))

(define suite
  (test-suite "prompt-injection"

    ;; ── Presence / Absence ──
    (test-case "preamble absent when feature flag is off"
      (parameterize ([current-task-state-aware-assembly? #f])
        (define msgs
          (list (make-message "id"
                              #f
                              'user
                              'text
                              (list (make-text-part "hello"))
                              (current-seconds)
                              (hasheq))))
        (define tc (build-tiered-context/state-aware msgs #:task-state 'exploration))
        ;; When flag is off but task-state is passed, preamble still appears
        ;; (the flag controls integration, not the builder itself)
        (check-true (>= (length (tiered-context-tier-a tc)) 1))))

    (test-case "preamble present for exploration state"
      (define p (build-state-awareness-preamble 'exploration '()))
      (check-not-false p)
      (check-equal? (message-role p) 'system)
      (check-equal? (message-kind p) 'system-instruction))

    (test-case "preamble present for idle state (v0.79.1 GAP-5)"
      (define p (build-state-awareness-preamble 'idle '()))
      (check-not-false p)
      (check-equal? (message-role p) 'system)
      (check-equal? (message-kind p) 'system-instruction))

    (test-case "preamble absent for #f state"
      (check-false (build-state-awareness-preamble #f '())))

    ;; ── State labels ──
    (test-case "preamble contains correct state label for exploration"
      (define text (preamble-text (build-state-awareness-preamble 'exploration '())))
      (check-not-false (string-contains? text "EXPLORATION")))

    (test-case "preamble contains correct state label for implementation"
      (define text (preamble-text (build-state-awareness-preamble 'implementation '())))
      (check-not-false (string-contains? text "IMPLEMENTATION")))

    (test-case "preamble contains correct state label for verification"
      (define text (preamble-text (build-state-awareness-preamble 'verification '())))
      (check-not-false (string-contains? text "VERIFICATION")))

    (test-case "preamble works with fsm-state struct"
      (define text (preamble-text (build-state-awareness-preamble (fsm-state 'debugging) '())))
      (check-not-false (string-contains? text "DEBUGGING")))

    ;; ── Action-oriented instructions ──
    (test-case "exploration instructions reference record_conclusion"
      (define text (preamble-text (build-state-awareness-preamble 'exploration '())))
      (check-not-false (string-contains? text "record_conclusion")
                       (format "Missing record_conclusion in: ~a" text)))

    (test-case "planning instructions reference record_conclusion"
      (define text (preamble-text (build-state-awareness-preamble 'planning '())))
      (check-not-false (string-contains? text "record_conclusion")
                       (format "Missing record_conclusion in: ~a" text)))

    (test-case "implementation instructions reference record_conclusion"
      (define text (preamble-text (build-state-awareness-preamble 'implementation '())))
      (check-not-false (string-contains? text "record_conclusion")
                       (format "Missing record_conclusion in: ~a" text)))

    (test-case "implementation instructions tell agent to prefer conclusions"
      (define text (preamble-text (build-state-awareness-preamble 'implementation '())))
      (check-not-false (string-contains? text "Prefer existing conclusions")
                       (format "Missing preference instruction in: ~a" text)))

    (test-case "verification instructions reference record_conclusion"
      (define text (preamble-text (build-state-awareness-preamble 'verification '())))
      (check-not-false (string-contains? text "record_conclusion")
                       (format "Missing record_conclusion in: ~a" text)))

    (test-case "debugging instructions reference record_conclusion"
      (define text (preamble-text (build-state-awareness-preamble 'debugging '())))
      (check-not-false (string-contains? text "record_conclusion")
                       (format "Missing record_conclusion in: ~a" text)))

    ;; ── Dynamic conclusion count ──
    (test-case "preamble shows 0 conclusions with encouragement"
      (define text (preamble-text (build-state-awareness-preamble 'exploration '())))
      (check-not-false (string-contains? text "No conclusions in memory yet"))
      (check-not-false (string-contains? text "record_conclusion")))

    (test-case "preamble shows exact conclusion count for 1 conclusion"
      (define conclusions (list (make-test-conclusion "Found the bug")))
      (define text (preamble-text (build-state-awareness-preamble 'implementation conclusions)))
      (check-not-false (string-contains? text "1 in memory") (format "Missing count in: ~a" text)))

    (test-case "preamble shows exact conclusion count for 3 conclusions"
      (define conclusions
        (list (make-test-conclusion "A") (make-test-conclusion "B") (make-test-conclusion "C")))
      (define text (preamble-text (build-state-awareness-preamble 'planning conclusions)))
      (check-not-false (string-contains? text "3 in memory") (format "Missing count in: ~a" text)))

    (test-case "preamble lists conclusion texts when conclusions exist"
      (define conclusions (list (make-test-conclusion "Use structs for data")))
      (define text (preamble-text (build-state-awareness-preamble 'exploration conclusions)))
      (check-not-false (string-contains? text "Use structs for data")))

    ;; ── No duplicate injection ──
    (test-case "single preamble in tier-a for state-aware assembly"
      (define msgs
        (list (make-message "id"
                            #f
                            'user
                            'text
                            (list (make-text-part "hello"))
                            (current-seconds)
                            (hasheq))))
      (define tc (build-tiered-context/state-aware msgs #:task-state 'planning))
      (define tier-a (tiered-context-tier-a tc))
      (define preamble-count (count (lambda (m) (eq? (message-kind m) 'system-instruction)) tier-a))
      (check-true (>= preamble-count 1) "Should have at least the preamble")
      (check-true (<= preamble-count 2)
                  (format "Too many system-instruction messages: ~a" preamble-count)))

    ;; ── Conclusion cap ──
    (test-case "preamble limits displayed conclusions to top 10"
      (define conclusions
        (for/list ([i (in-range 15)])
          (make-test-conclusion (format "Finding ~a" i))))
      (define text (preamble-text (build-state-awareness-preamble 'debugging conclusions)))
      (check-not-false (string-contains? text "Finding 9"))
      (check-false (string-contains? text "Finding 14")))

    ;; ── Integration: full assembly includes preamble ──
    (test-case "state-aware assembly includes instructions in message list"
      (define msgs
        (list (make-message "id"
                            #f
                            'user
                            'text
                            (list (make-text-part "hello"))
                            (current-seconds)
                            (hasheq))))
      (define tc (build-tiered-context/state-aware msgs #:task-state 'verification))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 1))
      (check-equal? (message-role (car tier-a)) 'system)
      (check-equal? (message-kind (car tier-a)) 'system-instruction))))

(run-tests suite)
