#lang racket/base
;;; test-memory-lifecycle-w3.rkt — W3 tests for post-turn auto-extraction lifecycle hook
(require rackunit
         racket/string
         racket/file
         "../runtime/memory/auto-extraction.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/service.rkt"
         "../runtime/memory/reflection.rkt")

;; Helper: create a hash backend for testing
(define (make-test-backend)
  (make-memory-hash-backend))

(test-case "W3: maybe-auto-extract-after-response! does nothing when disabled"
  (parameterize ([current-auto-extraction-enabled #f]
                 [current-memory-backend (make-test-backend)]
                 [current-memory-policy default-memory-policy])
    (define events '())
    (maybe-auto-extract-after-response! "This is a factual statement about the project."
                                        #:session-id "test-session"
                                        #:on-typed-event (lambda (e) (set! events (cons e events))))
    (check-equal? events '())))

(test-case "W3: maybe-auto-extract-after-response! extracts when enabled"
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.3]
                 [current-memory-backend (make-test-backend)]
                 [current-memory-policy default-memory-policy])
    (define events '())
    (maybe-auto-extract-after-response!
     "The project uses Racket for the agent core. Config files are in the project root."
     #:session-id "test-session"
     #:project-root "/tmp/test"
     #:on-typed-event (lambda (e) (set! events (cons e events))))
    ;; Should produce at least one event (store-requested, possibly stored)
    (check-true (> (length events) 0) "Should produce events when enabled")))

(test-case "W3: maybe-auto-extract-after-response! blocks secrets"
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.1]
                 [current-memory-backend (make-test-backend)]
                 [current-memory-policy default-memory-policy])
    (define blocked '())
    (maybe-auto-extract-after-response!
     "The API key is sk-1234567890abcdef1234567890abcdef for the service."
     #:session-id "test-session"
     #:on-event (lambda (action item reason)
                  (when (eq? action 'blocked)
                    (set! blocked (cons reason blocked))))
     #:on-typed-event void)
    (check-true (> (length blocked) 0) "Secret content should be blocked")))

(test-case "W3: maybe-auto-extract-after-response! does not fail on backend error"
  (define failing-backend
    (memory-backend "failing"
                    (lambda (item) (error "Backend unavailable"))
                    (lambda (query) (memory-result #t '() #f #f))
                    (lambda (id item) (error "Update failed"))
                    (lambda (id scope) (memory-result #t #f #f #f))
                    (lambda (query) (memory-result #t '() #f #f))
                    (lambda () #t)
                    (lambda (query) (memory-result #t '() #f #f))))
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.1]
                 [current-memory-backend failing-backend]
                 [current-memory-policy default-memory-policy])
    ;; Should NOT raise an exception
    (maybe-auto-extract-after-response! "The project uses Racket for the agent core module."
                                        #:session-id "test-session")
    (check-true #t "Should not raise on backend failure")))

(test-case "W3: maybe-auto-extract-after-response! skips short response"
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-memory-backend (make-test-backend)]
                 [current-memory-policy default-memory-policy])
    (define events '())
    (maybe-auto-extract-after-response! "OK"
                                        #:session-id "test-session"
                                        #:on-typed-event (lambda (e) (set! events (cons e events))))
    ;; Short response should produce skipped result, no stored events
    (check-equal?
     (length (filter (lambda (e) (and (hash? e) (eq? (hash-ref e 'type #f) 'memory.stored))) events))
     0)))

(test-case "W3: maybe-auto-extract-after-response! skips empty response"
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-memory-backend (make-test-backend)]
                 [current-memory-policy default-memory-policy])
    (define events '())
    (maybe-auto-extract-after-response! ""
                                        #:session-id "test-session"
                                        #:on-typed-event (lambda (e) (set! events (cons e events))))
    (check-equal? events '())))

;; ---------------------------------------------------------------------------
;; v0.95.21 W0: G1 — Auto-reflection gap verification
;; ---------------------------------------------------------------------------

(test-case "W0 G1: current-auto-reflection-enabled does not exist in service.rkt"
  ;; Verify the parameter doesn't exist yet — W2 will add it
  (define src (file->string (build-path (current-directory) ".." "runtime" "memory" "service.rkt")))
  (check-false (string-contains? src "current-auto-reflection-enabled")
               "W0 baseline: current-auto-reflection-enabled should not exist yet"))

(test-case "W0 G1: maybe-reflect-session-memories! does not exist in reflection.rkt"
  ;; Verify the wrapper doesn't exist yet — W3 will add it
  (define src (file->string (build-path (current-directory) ".." "runtime" "memory" "reflection.rkt")))
  (check-false (string-contains? src "maybe-reflect-session-memories!")
               "W0 baseline: maybe-reflect-session-memories! should not exist yet"))

(test-case "W0 G1: auto-reflection not hooked in loop-stream.rkt"
  ;; Verify loop-stream.rkt doesn't call reflection yet — W3 will add it
  (define src (file->string (build-path (current-directory) ".." "agent" "loop-stream.rkt")))
  (check-false (string-contains? src "reflect-session")
               "W0 baseline: auto-reflection should not be in loop-stream.rkt yet"))

(test-case "W0 G1: reflect-session-memories! exists and works (existing behavior)"
  ;; Verify the underlying reflection function exists and works
  (define backend (make-test-backend))
  (parameterize ([current-memory-backend backend]
                 [current-reflection-min-group-size 2])
    ;; Store 3 groupable items
    (for ([i (in-range 3)])
      ((memory-backend-store! backend)
       (memory-item (format "refl-base-~a" i)
                    'semantic 'session
                    (format "shared topic ~a alpha beta gamma" i)
                    (hasheq 'project-root "/test" 'session-id "sess-w0" 'tags '("shared" "topic")
                            'source 'test 'origin-message-id (format "m~a" i))
                    (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                    "2026-06-07T12:00:00Z"
                    "2026-06-07T12:00:00Z")))
    (define results
      (reflect-session-memories! backend #:session-id "sess-w0" #:project-root "/test"
                                 #:min-group-size 2))
    (check-true (and (list? results) (>= (length results) 1))
                "Reflection should produce at least one merged item")))
