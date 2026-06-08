#lang racket/base

;; @speed fast
;; @suite default

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
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion)
         (only-in "../util/message/protocol-types.rkt" message-content)
         (only-in "../util/content/content-parts.rkt" text-part-text)
         (only-in "../runtime/context-assembly/serialization.rkt"
                  build-tiered-context
                  build-tiered-context/state-aware
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c
                  tiered-context->message-list
                  current-task-state-aware-assembly?
                  build-state-awareness-preamble)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-meta-safe
                  message-content)
         (only-in "../util/content/content-parts.rkt" text-part? text-part-text))

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
  (list
   (task-conclusion "c1" "Files are organized by layer" 'fact 'idle '() (current-seconds) '() '())
   (task-conclusion "c2" "Tests use rackunit" 'fact 'idle '() (current-seconds) '() '())))

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

    (test-case "exploration state includes preamble (1 more than standard)"
      (define sa
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-exploration))
      (define std (build-tiered-context base-messages #:working-set-messages ws-messages))
      ;; Exploration has preamble injected → 1 more than standard
      (check-equal? (length (tiered-context->message-list sa))
                    (add1 (length (tiered-context->message-list std)))))

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

    (test-case "state-aware-assembly: feature flag can be enabled"
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

    ;; ── Empty conclusions ──
    (test-case "empty conclusions with planning state"
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages ws-messages
                                          #:task-state task-planning
                                          #:conclusions '()))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (count-ws tier-a) 0 "planning excludes ws even without conclusions"))

    ;; ── v0.75.5: System prompt preamble tests ──

    (test-case "preamble returns message for idle state (GAP-5 fix)"
      (define p (build-state-awareness-preamble task-idle '()))
      (check-not-false p)
      (define text (text-part-text (car (message-content p))))
      (check-not-false (string-contains? text "IDLE")))

    (test-case "preamble returns idle with conclusions (GAP-5 fix)"
      (define conclusions
        (list (task-conclusion "c1" "Found X" 'fact 'idle '() (current-seconds) '() '())))
      (define p (build-state-awareness-preamble task-idle conclusions))
      (check-not-false p)
      (define text (text-part-text (car (message-content p))))
      (check-not-false (string-contains? text "Found X")))

    (test-case "state-aware-assembly: preamble returns #f for #f state"
      (define p (build-state-awareness-preamble #f '()))
      (check-false p))

    (test-case "preamble contains correct state label"
      (define p (build-state-awareness-preamble task-implementation '()))
      (check-not-false p)
      (define text-part (car (message-content p)))
      (define text (text-part-text text-part))
      (check-not-false (string-contains? text "IMPLEMENTATION") (format "text=~a" text)))

    (test-case "preamble contains state-specific guidance"
      (define p (build-state-awareness-preamble task-implementation '()))
      (define text (text-part-text (car (message-content p))))
      (check-not-false (string-contains? text "Focus on editing files")
                       (format "missing guidance in: ~a" text)))

    (test-case "preamble includes conclusions summary"
      (define conclusions
        (list (task-conclusion "c1" "Use struct for data" 'fact 'idle '() (current-seconds) '() '())
              (task-conclusion "c2" "Tests in tests/" 'fact 'idle '() (current-seconds) '() '())))
      (define p (build-state-awareness-preamble task-exploration conclusions))
      (define text (text-part-text (car (message-content p))))
      (check-not-false (string-contains? text "Use struct for data")
                       (format "missing conclusion in: ~a" text)))

    (test-case "preamble limits conclusions to top 10"
      (define conclusions
        (for/list ([i (in-range 15)])
          (task-conclusion (format "c~a" i)
                           (format "Finding ~a" i)
                           'fact
                           'idle
                           '()
                           (current-seconds)
                           '()
                           '())))
      (define p (build-state-awareness-preamble task-planning conclusions))
      (define text (text-part-text (car (message-content p))))
      (check-not-false (string-contains? text "Finding 9"))
      (check-false (string-contains? text "Finding 14")))))

(run-tests suite)
