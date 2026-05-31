#lang racket/base

;; tests/test-state-aware-assembly.rkt — tests for state-aware context assembly
;; v0.75.3 W0: Verify build-tiered-context/state-aware respects state relevance.

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion task-conclusion?)
         (only-in "../runtime/context-assembly/serialization.rkt"
                  build-tiered-context
                  build-tiered-context/state-aware
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c
                  tiered-context->message-list
                  current-task-state-aware-assembly?)
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-meta-safe
                  message-content)
         (only-in "../util/content-parts.rkt" text-part? text-part-text))

;; Helper: create a simple test message
(define (test-msg role text)
  (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) (hasheq)))

;; Helper: create a working-set message
(define (ws-msg text)
  (make-message "ws-id"
                #f
                'system-instruction
                'text
                (list (make-text-part text))
                (current-seconds)
                (hasheq 'working-set #t)))

;; Test data
(define base-messages
  (list (test-msg 'user "hello")
        (test-msg 'assistant "hi there")
        (test-msg 'user "do something")
        (test-msg 'assistant "done")))

(define ws-messages (list (ws-msg "ws-entry-1") (ws-msg "ws-entry-2") (ws-msg "ws-entry-3")))

(define conclusions
  (list (task-conclusion "c1" "Files are organized by layer" 'fact 'idle '() (current-seconds) '())
        (task-conclusion "c2" "Tests use rackunit" 'fact 'idle '() (current-seconds) '())))

(define (count-ws tier-a)
  (length (filter (lambda (m) (hash-ref (message-meta-safe m) 'working-set #f)) tier-a)))

(define suite
  (test-suite "state-aware-assembly"

    ;; ── Feature flag default ──
    (test-case "feature flag defaults to #f"
      (check-false (current-task-state-aware-assembly?)))

    ;; ── No state = standard behavior ──
    (test-case "no task-state produces same message count as standard"
      (define sa (build-tiered-context/state-aware base-messages #:working-set-messages ws-messages))
      (define std (build-tiered-context base-messages #:working-set-messages ws-messages))
      (check-equal? (length (tiered-context->message-list sa))
                    (length (tiered-context->message-list std))))

    ;; ── idle state: working-set excluded ──
    (test-case "idle state excludes working-set"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-idle))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (count-ws tier-a) 0 "idle should exclude working-set"))

    ;; ── exploration state: full working-set ──
    (test-case "exploration state keeps full working-set"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-exploration))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (count-ws tier-a) 3 "exploration keeps all working-set entries"))

    ;; ── planning state: working-set excluded, conclusions included ──
    (test-case "planning state excludes working-set and includes conclusions"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-planning
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (count-ws tier-a) 0 "planning excludes working-set")
      (check-true (>= (length tier-a) 2) "planning includes conclusions in tier-a"))

    ;; ── implementation state: filtered working-set ──
    (test-case "implementation state filters working-set"
      (define many-ws
        (for/list ([i (in-range 10)])
          (ws-msg (format "ws-~a" i))))
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages many-ws
                                          #:task-state task-implementation
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (<= (count-ws tier-a) 3) "implementation filters working-set to at most 3"))

    ;; ── verification state: conclusions included ──
    (test-case "verification state includes conclusions"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-verification
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 2) "verification includes conclusions"))

    ;; ── debugging state: full working-set ──
    (test-case "debugging state keeps full working-set"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-debugging
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (count-ws tier-a) 3 "debugging keeps all working-set"))

    ;; ── W1: Comparison tests ──

    (test-case "exploration state behaves identically to standard"
      (define sa
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-exploration))
      (define std (build-tiered-context base-messages #:working-set-messages ws-messages))
      ;; Exploration has 'full for everything → should be same
      (check-equal? (length (tiered-context->message-list sa))
                    (length (tiered-context->message-list std))))

    (test-case "implementation filters working-set entries"
      (define many-ws
        (for/list ([i (in-range 10)])
          (ws-msg (format "ws-~a" i))))
      (define sa
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages many-ws
                                          #:task-state task-implementation
                                          #:conclusions conclusions))
      (define sa-ws (count-ws (tiered-context-tier-a sa)))
      (check-true (<= sa-ws 3) (format "sa-ws=~a should be <= 3" sa-ws)))

    (test-case "feature flag can be enabled"
      (parameterize ([current-task-state-aware-assembly? #t])
        (check-true (current-task-state-aware-assembly?))))

    (test-case "conclusions appear as system-instruction messages"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages '()
                                          #:task-state task-debugging
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (define conclusion-texts
        (filter-map (lambda (m)
                      (define content (message-content m))
                      (and (pair? content)
                           (let ([t (text-part-text (car content))])
                             (and (string-contains? t "[Conclusion]") t))))
                    tier-a))
      (check-true (>= (length conclusion-texts) 1) "At least one conclusion should appear in tier-a"))
    ;; ── Empty conclusions ──
    (test-case "empty conclusions with planning state"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-planning
                                          #:conclusions '()))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (count-ws tier-a) 0 "planning excludes ws even without conclusions"))))

(run-tests suite)
