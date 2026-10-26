#lang racket/base
;; @covers runtime/memory/conclusion-bridge.rkt
;; @speed fast @suite default
;; @boundary unit
;; tests/test-gap5-conclusion-bridge.rkt — GAP-5 TDD tests
;; Validates conclusion bridge activation in profiles

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/session/session-config.rkt"
                  apply-context-assembly-profile!
                  current-context-assembly-profile)
         (only-in "../runtime/memory/conclusion-bridge.rkt"
                  current-conclusion-to-memory-bridge-enabled
                  persist-high-value-conclusions!
                  high-value-categories))

(define-test-suite gap-5-tests
                   (test-case "GAP-5: self-healing profile enables conclusion bridge"
                     (apply-context-assembly-profile! 'self-healing)
                     (check-true (current-conclusion-to-memory-bridge-enabled)))
                   (test-case "GAP-5: full profile enables conclusion bridge"
                     (apply-context-assembly-profile! 'full)
                     (check-true (current-conclusion-to-memory-bridge-enabled)))
                   (test-case "GAP-5: observe profile does NOT enable bridge"
                     (current-conclusion-to-memory-bridge-enabled #f)
                     (apply-context-assembly-profile! 'observe)
                     (check-false (current-conclusion-to-memory-bridge-enabled)))
                   (test-case "GAP-5: bounded profile does NOT enable bridge"
                     (current-conclusion-to-memory-bridge-enabled #f)
                     (apply-context-assembly-profile! 'bounded)
                     (check-false (current-conclusion-to-memory-bridge-enabled)))
                   (test-case "GAP-5: high-value-categories includes fact"
                     (check-not-false (memq 'fact high-value-categories)
                                      "fact category must be in high-value-categories")))

(run-tests gap-5-tests)

;; ---------------------------------------------------------------------------
;; R99a (BUG-0099): bridge no-promotion — recognized legacy deterministic
;; fallback AND future provenance-category records are excluded from
;; high-value semantic export; valid LLM facts still export.
;; ---------------------------------------------------------------------------

(require (only-in "../runtime/memory/backends/memory-hash.rkt"
                  make-memory-hash-backend
                  memory-hash-backend-items)
         (only-in "../runtime/memory/types.rkt" memory-item-content)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  legacy-deterministic-fallback?
                  provenance-kind?)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  generate-fallback-conclusions))

(define (r99a-fallback-conclusion id origin-id)
  (task-conclusion (format "auto-~a" origin-id)
                   (format "[Auto] Previously read file (origin: ~a)" origin-id)
                   'fact
                   'implementation
                   (list origin-id)
                   (current-seconds)
                   '(auto-distilled)
                   '()))

(define-test-suite r99a-bridge-no-promotion-tests
  (test-case "R99a: recognized legacy fallback is NOT promoted to memory"
    (define backend (make-memory-hash-backend))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions!
       (list (r99a-fallback-conclusion "auto-ws-1" "ws-1"))
       #:backend backend
       #:session-id "s"))
    (check-equal? (length (memory-hash-backend-items backend)) 0))

  (test-case "R99a: provenance-category record is NOT promoted to memory"
    (define backend (make-memory-hash-backend))
    (define prov
      (task-conclusion "p1" "[Auto] provenance record" 'provenance 'implementation
                       '("m1") (current-seconds) '(auto-distilled) '()))
    (check-true (provenance-kind? prov))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions! (list prov) #:backend backend #:session-id "s"))
    (check-equal? (length (memory-hash-backend-items backend)) 0))

  (test-case "R99a: valid LLM fact still exports (no over-suppression)"
    (define backend (make-memory-hash-backend))
    (define fact
      (task-conclusion "c-llm" "Genuine LLM fact" 'fact 'implementation
                       '("m2") (current-seconds) '() '()))
    (check-false (provenance-kind? fact))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions! (list fact) #:backend backend #:session-id "s"))
    (check-equal? (length (memory-hash-backend-items backend)) 1))

  (test-case "R99a: mixed batch — fallback excluded, genuine decision exported"
    (define backend (make-memory-hash-backend))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions!
       (list (r99a-fallback-conclusion "auto-ws-2" "ws-2")
             (task-conclusion "c-dec" "Real decision" 'decision 'planning '() (current-seconds) '() '()))
       #:backend backend
       #:session-id "s"))
    (define items (memory-hash-backend-items backend))
    (check-equal? (length items) 1)
    (check-equal? (memory-item-content (car items)) "Real decision"))

  (test-case "R99a: recognizer frozen — tag-only variant would NOT be excluded"
    ;; Guard against over-broad exclusion: an LLM conclusion carrying the
    ;; auto-distilled tag but lacking the full fallback signature is still a
    ;; genuine fact and must export.
    (define ambiguous
      (task-conclusion "c-tagged" "[Auto] shaped but LLM-authored" 'fact 'implementation
                       '("m3") (current-seconds) '(auto-distilled) '()))
    (check-false (legacy-deterministic-fallback? ambiguous))
    (define backend (make-memory-hash-backend))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions! (list ambiguous) #:backend backend #:session-id "s"))
    (check-equal? (length (memory-hash-backend-items backend)) 1)))

(run-tests r99a-bridge-no-promotion-tests)

;; ---------------------------------------------------------------------------
;; R99b (BUG-0099): the ACTIVATED writer's provenance records are excluded
;; from high-value semantic export (no promotion), without suppressing
;; genuine insights in a mixed batch.
;; ---------------------------------------------------------------------------
(define-test-suite r99b-bridge-writer-record-tests
  (test-case "R99b: writer-produced provenance record is NOT promoted to memory"
    (define record
      (car (generate-fallback-conclusions (list "ws-9") 'implementation)))
    (check-true (provenance-kind? record))
    (define backend (make-memory-hash-backend))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions! (list record) #:backend backend #:session-id "s"))
    (check-equal? (length (memory-hash-backend-items backend)) 0))

  (test-case "R99b: mixed batch — writer record excluded, genuine decision exported"
    (define record
      (car (generate-fallback-conclusions (list "ws-10") 'implementation)))
    (define backend (make-memory-hash-backend))
    (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
      (persist-high-value-conclusions!
       (list record
             (task-conclusion "c-dec2" "Real decision from LLM" 'decision 'planning '() (current-seconds) '() '()))
       #:backend backend
       #:session-id "s"))
    (define items (memory-hash-backend-items backend))
    (check-equal? (length items) 1)
    (check-equal? (memory-item-content (car items)) "Real decision from LLM")))

(run-tests r99b-bridge-writer-record-tests)
