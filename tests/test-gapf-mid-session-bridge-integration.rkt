#lang racket/base

;; tests/test-gapf-mid-session-bridge-integration.rkt
;; v0.97.6 LF4: Integration test for maybe-persist-mid-session!
;; Tests the actual function body, not just the gate condition.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/session/session-events.rkt"
                  maybe-persist-mid-session!
                  current-mid-session-bridge-enabled)
         (only-in "../runtime/memory/conclusion-bridge.rkt"
                  current-conclusion-to-memory-bridge-enabled)
         (only-in "../runtime/memory/service.rkt"
                  current-memory-backend)
         (only-in "../runtime/memory/protocol.rkt"
                  memory-backend)
         (only-in "../runtime/memory/types.rkt"
                  memory-item-id)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion)
         ;; Access internal setter for test setup (seeding conclusions)
         (only-in (submod "../runtime/session/session-types.rkt" internal)
                  set-agent-session-task-conclusions!)
         (only-in "helpers/session-fixture.rkt"
                  make-test-session))

;; Recording mock backend — stores items in a box for assertion
(define (make-recording-backend box)
  (memory-backend "recording"
                  (lambda (item) (set-box! box (cons item (unbox box))) (hasheq 'ok #t 'id (memory-item-id item)))
                  (lambda (q) '())
                  (lambda (id p) (hasheq 'ok #f))
                  (lambda (id s) (hasheq 'ok #f))
                  (lambda (q) '())
                  (lambda () #t)
                  (lambda (p) (hasheq 'ok #f))))

;; Helper: create a high-value conclusion (decision category)
(define (make-test-conclusion text)
  (task-conclusion "test-id" text 'decision 'active '()
                   (current-seconds) '() (hasheq)))

(define suite
  (test-suite
   "gapf-mid-session-bridge-integration"

   ;; maybe-persist-mid-session! persists on forward transition when all flags on
   (test-case "bridge persists high-value conclusions on forward transition"
     (define recorded (box '()))
     (define mock-backend (make-recording-backend recorded))
     (define sess (make-test-session #:dir "/tmp/q-gapf-integ-1"))
     ;; Seed a high-value conclusion
     (set-agent-session-task-conclusions! sess (list (make-test-conclusion "test-decision")))
     (parameterize ([current-mid-session-bridge-enabled #t]
                    [current-conclusion-to-memory-bridge-enabled #t]
                    [current-memory-backend mock-backend])
       (maybe-persist-mid-session! sess 'exploration 'planning)
       (check-equal? (length (unbox recorded)) 1
                     "one conclusion should be persisted on forward transition")))

   ;; maybe-persist-mid-session! does NOT persist when bridge disabled
   (test-case "bridge does not persist when disabled"
     (define recorded (box '()))
     (define mock-backend (make-recording-backend recorded))
     (define sess (make-test-session #:dir "/tmp/q-gapf-integ-2"))
     (set-agent-session-task-conclusions! sess (list (make-test-conclusion "test")))
     (parameterize ([current-mid-session-bridge-enabled #f]
                    [current-conclusion-to-memory-bridge-enabled #t]
                    [current-memory-backend mock-backend])
       (maybe-persist-mid-session! sess 'exploration 'planning)
       (check-equal? (unbox recorded) '()
                     "no conclusions should be persisted when bridge disabled")))

   ;; maybe-persist-mid-session! does NOT persist on non-forward transition
   (test-case "bridge does not persist on non-forward transition"
     (define recorded (box '()))
     (define mock-backend (make-recording-backend recorded))
     (define sess (make-test-session #:dir "/tmp/q-gapf-integ-3"))
     (set-agent-session-task-conclusions! sess (list (make-test-conclusion "test")))
     (parameterize ([current-mid-session-bridge-enabled #t]
                    [current-conclusion-to-memory-bridge-enabled #t]
                    [current-memory-backend mock-backend])
       (maybe-persist-mid-session! sess 'planning 'exploration)
       (check-equal? (unbox recorded) '()
                     "no conclusions should be persisted on reverse transition")))

   ;; maybe-persist-mid-session! handles backend error gracefully
   (test-case "bridge handles backend error gracefully"
     (define error-backend
       (memory-backend "failing"
                       (lambda (item) (error "backend-fail"))
                       (lambda (q) '())
                       (lambda (id p) (hasheq 'ok #f))
                       (lambda (id s) (hasheq 'ok #f))
                       (lambda (q) '())
                       (lambda () #t)
                       (lambda (p) (hasheq 'ok #f))))
     (define sess (make-test-session #:dir "/tmp/q-gapf-integ-4"))
     (set-agent-session-task-conclusions! sess (list (make-test-conclusion "error-test")))
     (parameterize ([current-mid-session-bridge-enabled #t]
                    [current-conclusion-to-memory-bridge-enabled #t]
                    [current-memory-backend error-backend])
       (check-not-exn
        (lambda () (maybe-persist-mid-session! sess 'exploration 'planning))
        "bridge should not throw on backend error")))))

(run-tests suite)
