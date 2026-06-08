#lang racket/base

;; @speed fast
;; @suite default
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

(test-case "W1 G2: current-auto-reflection-enabled now exists in service.rkt"
  (define src (file->string (build-path (current-directory) ".." "runtime" "memory" "service.rkt")))
  (check-true (string-contains? src "current-auto-reflection-enabled")
              "W1: current-auto-reflection-enabled should exist"))

(test-case "W3 G1: maybe-reflect-session-memories! now exists in reflection.rkt"
  (define src (file->string (build-path (current-directory) ".." "runtime" "memory" "reflection.rkt")))
  (check-true (string-contains? src "maybe-reflect-session-memories!")
              "W3: maybe-reflect-session-memories! should exist"))

(test-case "W3 G1: auto-reflection now hooked in loop-stream.rkt"
  (define src (file->string (build-path (current-directory) ".." "agent" "loop-stream.rkt")))
  (check-true (string-contains? src "reflect-session")
              "W3: auto-reflection should be in loop-stream.rkt"))

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

;; ---------------------------------------------------------------------------
;; v0.95.21 W2: G1 — Auto-reflection settings functional tests
;; ---------------------------------------------------------------------------

(test-case "W2 G1: current-auto-reflection-enabled defaults to #f"
  (check-false (current-auto-reflection-enabled)))

(test-case "W2 G1: current-auto-reflection-min-items defaults to 5"
  (check-equal? (current-auto-reflection-min-items) 5))

(test-case "W2 G1: current-auto-reflection-enabled can be set"
  (parameterize ([current-auto-reflection-enabled #t])
    (check-true (current-auto-reflection-enabled))))

(test-case "W2 G1: current-auto-reflection-min-items can be set"
  (parameterize ([current-auto-reflection-min-items 3])
    (check-equal? (current-auto-reflection-min-items) 3)))

;; ---------------------------------------------------------------------------
;; v0.95.21 W3: G1 — maybe-reflect-session-memories! functional tests
;; ---------------------------------------------------------------------------

(test-case "W3: maybe-reflect-session-memories! no-ops when disabled"
  (define backend (make-test-backend))
  (parameterize ([current-auto-reflection-enabled #f]
                 [current-memory-backend backend]
                 [current-memory-policy default-memory-policy]
                 [current-reflection-min-group-size 2])
    ;; Store enough items for reflection
    (for ([i (in-range 3)])
      ((memory-backend-store! backend)
       (memory-item (format "no-op-~a" i)
                    'semantic 'session
                    (format "shared topic ~a alpha beta gamma" i)
                    (hasheq 'project-root "/test" 'session-id "sess-w3-noop"
                            'tags '("shared" "topic") 'source 'test
                            'origin-message-id (format "m~a" i))
                    (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                    "2026-06-07T12:00:00Z"
                    "2026-06-07T12:00:00Z")))
    (maybe-reflect-session-memories! #:session-id "sess-w3-noop")
    ;; No reflection items created at project scope
    (define result ((memory-backend-retrieve backend)
                    (memory-query "" 'project "/test" #f #f #f 100 #f)))
    (when (memory-result-ok? result)
      (define items (memory-result-value result))
      (check-equal? (length items) 0
                    "Should not reflect when disabled"))))

(test-case "W3: maybe-reflect-session-memories! no-ops when no backend"
  (parameterize ([current-auto-reflection-enabled #t]
                 [current-memory-backend #f])
    ;; Should not raise
    (maybe-reflect-session-memories! #:session-id "sess-w3-nobackend")
    (check-true #t "Should not raise when no backend")))

(test-case "W3: maybe-reflect-session-memories! non-fatal on backend error"
  (define failing-backend
    (memory-backend "failing"
                    (lambda (item) (error "store failed"))
                    (lambda (q) (error "retrieve failed"))
                    (lambda (id patch) (error "update failed"))
                    (lambda (id scope) (error "delete failed"))
                    (lambda () (error "list failed"))
                    (lambda () #t)
                    (lambda (action) (error "manage failed"))))
  (parameterize ([current-auto-reflection-enabled #t]
                 [current-memory-backend failing-backend]
                 [current-auto-reflection-min-items 2])
    ;; Should not raise despite backend errors
    (maybe-reflect-session-memories! #:session-id "sess-w3-fail")
    (check-true #t "Should not raise on backend failure")))

(test-case "W3: maybe-reflect-session-memories! fires when enabled with backend"
  (define backend (make-test-backend))
  (parameterize ([current-auto-reflection-enabled #t]
                 [current-memory-backend backend]
                 [current-memory-policy default-memory-policy]
                 [current-auto-reflection-min-items 2]
                 [current-reflection-min-group-size 2])
    ;; Store 3 groupable items in session scope
    (for ([i (in-range 3)])
      ((memory-backend-store! backend)
       (memory-item (format "fire-~a" i)
                    'semantic 'session
                    (format "shared topic ~a alpha beta gamma" i)
                    (hasheq 'project-root "/test" 'session-id "sess-w3-fire"
                            'tags '("shared" "topic") 'source 'test
                            'origin-message-id (format "m~a" i))
                    (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                    "2026-06-07T12:00:00Z"
                    "2026-06-07T12:00:00Z")))
    (maybe-reflect-session-memories! #:session-id "sess-w3-fire" #:project-root "/test")
    ;; Check that a reflection item was created at project scope
    (define result ((memory-backend-retrieve backend)
                    (memory-query "" 'project "/test" #f #f #f 100 #f)))
    (when (memory-result-ok? result)
      (define items (memory-result-value result))
      (check-true (>= (length items) 1)
                  "Should have created at least one reflection item"))))
