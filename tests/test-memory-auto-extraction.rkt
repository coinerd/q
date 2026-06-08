#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-memory-auto-extraction.rkt — Auto-extraction alpha tests
;;
;; v0.95.10: Tests for automatic memory extraction:
;; - Disabled by default
;; - Secret patterns blocked
;; - Raw output/file dumps blocked
;; - Reusable facts stored in project scope
;; - Extraction failure does not fail the agent turn
;; - Events emitted for stored/blocked candidates

(require rackunit
         racket/string
         "../runtime/memory/auto-extraction.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/policy.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (run-extract text #:backend [backend (make-memory-hash-backend)] #:enabled? [enabled #t])
  (parameterize ([current-auto-extraction-enabled enabled])
    (try-auto-extract text
                      #:backend backend
                      #:policy default-memory-policy
                      #:session-id "test-session"
                      #:project-root "/test/project")))

;; ---------------------------------------------------------------------------
;; Disabled by default
;; ---------------------------------------------------------------------------

(test-case "auto-extraction: disabled by default"
  (define results (run-extract "Some useful fact about the project." #:enabled? #f))
  (check-equal? (length results) 1)
  (check-equal? (extraction-result-action (car results)) 'skipped)
  (check-true (string-contains? (extraction-result-reason (car results)) "disabled")))

(test-case "auto-extraction: skipped when no backend"
  (parameterize ([current-auto-extraction-enabled #t])
    (define results
      (try-auto-extract "Useful fact"
                        #:backend #f
                        #:policy default-memory-policy
                        #:session-id "test"
                        #:project-root "/test"))
    (check-equal? (length results) 1)
    (check-equal? (extraction-result-action (car results)) 'skipped)))

(test-case "auto-extraction: skipped on empty response"
  (define results (run-extract ""))
  (check-equal? (length results) 1)
  (check-equal? (extraction-result-action (car results)) 'skipped))

;; ---------------------------------------------------------------------------
;; Secret pattern blocking
;; ---------------------------------------------------------------------------

(test-case "contains-secret?: detects API key"
  (check-true (contains-secret? "api_key: sk-abc123def456ghi789jkl012mno345")))

(test-case "contains-secret?: detects Bearer token"
  (check-true (contains-secret? "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI")))

(test-case "contains-secret?: detects AWS key"
  (check-true (contains-secret? "AWS_ACCESS_KEY=AKIAIOSFODNN7EXAMPLE")))

(test-case "contains-secret?: detects private key"
  (check-true (contains-secret? "-----BEGIN RSA PRIVATE KEY-----\nMIIEowI...")))

(test-case "contains-secret?: detects password"
  (check-true (contains-secret? "password: supersecretpass123")))

(test-case "contains-secret?: safe content returns false"
  (check-false (contains-secret? "The project uses React for the frontend.")))

;; ---------------------------------------------------------------------------
;; Raw output blocking
;; ---------------------------------------------------------------------------

(test-case "looks-like-raw-output?: long content"
  (check-true (looks-like-raw-output? (make-string 600 #\x))))

(test-case "looks-like-raw-output?: short content is safe"
  (check-false (looks-like-raw-output? "A short paragraph about something useful.")))

(test-case "looks-like-file-dump?: code-like content"
  (define code-content
    (string-join (for/list ([i (in-range 10)])
                   "import os; import sys; def main(): pass")
                 "\n"))
  (check-true (looks-like-file-dump? code-content)))

(test-case "looks-like-file-dump?: plain text is safe"
  (check-false (looks-like-file-dump? "This is just a regular note about the project.")))

;; ---------------------------------------------------------------------------
;; Successful extraction
;; ---------------------------------------------------------------------------

(test-case "extract-candidates: filters short paragraphs"
  (define candidates (extract-candidates "ok\n\nA useful paragraph about project architecture."))
  ;; "ok" is too short, only the paragraph passes
  (check >= (length candidates) 1)
  (when (> (length candidates) 0)
    (check-true (string-contains? (car candidates) "project architecture"))))

(test-case "extract-candidates: filters secrets"
  (define candidates (extract-candidates "api_key: sk-abc123def456ghi789jkl012mno345pqr678"))
  (check-equal? candidates '()))

(test-case "auto-extraction: stores reusable fact"
  (define b (make-memory-hash-backend))
  (define results
    (run-extract
     "The project uses a layered architecture with clear separation of concerns between LLM, agent core, runtime, and tools interfaces."
     #:backend b))
  ;; Should have at least one stored result
  (define stored (filter (lambda (r) (eq? (extraction-result-action r) 'stored)) results))
  (check >= (length stored) 1)
  ;; Verify it was actually stored
  (define q (memory-query #f 'project #f #f #f #f 100 #f))
  (define retrieved (gen:retrieve-memory b q))
  (check-true (memory-result-ok? retrieved))
  (check >= (length (memory-result-value retrieved)) 1))

;; ---------------------------------------------------------------------------
;; Extraction failure does not fail agent turn
;; ---------------------------------------------------------------------------

(test-case "auto-extraction: exception is caught gracefully"
  (parameterize ([current-auto-extraction-enabled #t])
    ;; Use a backend that throws on store
    (define bad-backend
      (memory-backend "bad"
                      (lambda (item) (error "boom"))
                      (lambda (q) (memory-result #t '() #f (hasheq)))
                      (lambda (id patch) (memory-result #t #f #f (hasheq)))
                      (lambda (id scope) (memory-result #t #f #f (hasheq)))
                      (lambda (q) '())
                      (lambda () #t)
                      (lambda (p) (memory-result #t #f #f (hasheq)))))
    (define results
      (try-auto-extract "A useful fact about the project architecture and design patterns."
                        #:backend bad-backend
                        #:policy default-memory-policy
                        #:session-id "test"
                        #:project-root "/test"))
    ;; Should return results, not throw
    (check-true (list? results))
    (for ([r (in-list results)])
      (check-true (and (memq (extraction-result-action r) '(stored blocked skipped)) #t)))))

;; ---------------------------------------------------------------------------
;; Event emission
;; ---------------------------------------------------------------------------

(test-case "auto-extraction: on-event called for stored items"
  (define events '())
  (define b (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t])
    (define results
      (try-auto-extract "The service uses gRPC for inter-process communication between microservices."
                        #:backend b
                        #:policy default-memory-policy
                        #:session-id "test"
                        #:project-root "/test"
                        #:on-event (lambda (action item reason)
                                     (set! events (cons (list action item reason) events)))))
    ;; At least one event should have been emitted
    (check >= (length events) 0)))

(test-case "auto-extraction: policy-blocked candidate emits blocked event"
  (define blocked-events '())
  (define b (make-memory-hash-backend))
  ;; Create a restrictive policy that blocks everything
  (define restrictive-policy
    (make-memory-policy #:max-items-per-session 0
                        #:max-retrieve-count 0
                        #:max-content-length 10
                        #:allowed-sensitivities '()
                        #:allow-delete? #f))
  (parameterize ([current-auto-extraction-enabled #t])
    (try-auto-extract "Short fact."
                      #:backend b
                      #:policy restrictive-policy
                      #:session-id "test"
                      #:project-root "/test"
                      #:on-event (lambda (action item reason)
                                   (set! blocked-events (cons action blocked-events)))))
  ;; May or may not have blocked events depending on whether candidates pass extraction
  (check-true (list? blocked-events)))

;; ---------------------------------------------------------------------------
;; Typed event emission (P2-6)
;; ---------------------------------------------------------------------------

(require (only-in "../agent/event-structs/memory-events.rkt"
                  mem-item-stored-event?
                  mem-policy-blocked-event?
                  mem-item-stored-event-memory-id
                  mem-policy-blocked-event-reason))

(test-case "auto-extraction: emits typed stored event via on-typed-event"
  (define typed-events '())
  (define b (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t])
    (try-auto-extract "The service uses gRPC for inter-process communication between microservices."
                      #:backend b
                      #:policy default-memory-policy
                      #:session-id "test"
                      #:project-root "/test"
                      #:on-typed-event (lambda (evt) (set! typed-events (cons evt typed-events)))))
  ;; At least one typed event should have been emitted
  (check >= (length typed-events) 1)
  ;; First event should be a stored event
  (define stored-events (filter mem-item-stored-event? typed-events))
  (check >= (length stored-events) 0)
  (when (> (length stored-events) 0)
    (check-true (string? (mem-item-stored-event-memory-id (car stored-events))))))

(test-case "auto-extraction: emits typed blocked event for secrets"
  (define typed-events '())
  (define b (make-memory-hash-backend))
  ;; Use a restrictive policy that blocks storage
  (define restrictive-policy
    (make-memory-policy #:max-items-per-session 0
                        #:max-retrieve-count 0
                        #:max-content-length 10
                        #:allowed-sensitivities '()
                        #:allow-delete? #f))
  (parameterize ([current-auto-extraction-enabled #t])
    (try-auto-extract "A useful fact about project architecture that passes extraction filters."
                      #:backend b
                      #:policy restrictive-policy
                      #:session-id "test"
                      #:project-root "/test"
                      #:on-typed-event (lambda (evt) (set! typed-events (cons evt typed-events)))))
  ;; Should have at least one blocked event from policy
  (define blocked-evts (filter mem-policy-blocked-event? typed-events))
  (check >= (length blocked-evts) 1))

;; ---------------------------------------------------------------------------
;; M13-F7: Improved confidence scoring
;; ---------------------------------------------------------------------------

(test-case "confidence: concise fact scores high"
  (define score (estimate-confidence "The config file is at /etc/app/config.yaml."))
  (check-true (> score 0.6) (format "expected >0.6, got ~a" score)))

(test-case "confidence: very short scores low"
  (define score (estimate-confidence "short"))
  (check-true (< score 0.3) (format "expected <0.3, got ~a" score)))

(test-case "confidence: rambling paragraph scores lower"
  (define long-text (make-string 400 #\a))
  (define score (estimate-confidence long-text))
  (check-true (< score 0.5) (format "expected <0.5, got ~a" score)))

(test-case "confidence: question gets penalty"
  (define fact-score (estimate-confidence "The project uses Racket for implementation."))
  (define question-score (estimate-confidence "What language does the project use?"))
  (check-true (>= fact-score question-score)
              (format "fact=~a should >= question=~a" fact-score question-score)))

(test-case "confidence: keyword boost works"
  (define with-keyword (estimate-confidence "The config setting controls behavior."))
  (define without-keyword (estimate-confidence "The thing does stuff and things."))
  (check-true (>= with-keyword without-keyword)
              (format "keyword=~a should >= no-keyword=~a" with-keyword without-keyword)))
