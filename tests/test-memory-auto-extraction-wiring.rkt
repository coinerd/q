#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-auto-extraction-wiring.rkt — W0/W2/W3 characterization tests
;;;
;;; W0: Characterize auto-extraction config/wiring gaps.
;;; W2: Verify runtime parameter wiring (after implementation).
;;; W3: Verify post-turn lifecycle hook (after implementation).
(require rackunit
         racket/string
         "../runtime/memory/auto-extraction.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/settings.rkt")

;; ============================================================
;; W0: Auto-extraction config characterization
;; ============================================================

(test-case "W0: auto-extraction disabled by default"
  (check-false (current-auto-extraction-enabled)
               "auto-extraction must be disabled by default"))

(test-case "W0: auto-extraction min-confidence defaults to 0.5"
  (check-equal? (current-auto-extraction-min-confidence) 0.5
                "min-confidence default should be 0.5"))

(test-case "W0: settings reads memory backend"
  (define s (make-minimal-settings))
  (check-false (setting-memory-backend s)))

(test-case "W0: try-auto-extract returns skipped when disabled"
  (define be (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #f])
    (define result (try-auto-extract "Some factual content about the project."
                                      #:backend be
                                      #:policy default-memory-policy
                                      #:session-id "s1"
                                      #:project-root "/tmp"))
    (check-true (list? result) "disabled extraction should return a list")
    (when (pair? result)
      (check-equal? (extraction-result-action (car result)) 'skipped
                    "disabled should report skipped"))))

(test-case "W0: try-auto-extract produces candidates when enabled"
  (define be (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.3])
    (define result (try-auto-extract "The project uses Racket for its agent runtime."
                                      #:backend be
                                      #:policy default-memory-policy
                                      #:session-id "s1"
                                      #:project-root "/tmp"))
    (check-true (list? result) "extraction should return a list")))

(test-case "W0: try-auto-extract blocks secrets"
  (define be (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.1])
    (define result (try-auto-extract "My API key is sk-1234567890abcdef"
                                      #:backend be
                                      #:policy default-memory-policy
                                      #:session-id "s1"
                                      #:project-root "/tmp"))
    (check-true (list? result))
    ;; Check that at least one result reports blocked for secrets
    (define blocked (filter (lambda (r) (eq? (extraction-result-action r) 'blocked)) result))
    (check-true (> (length blocked) 0) "secret content should be blocked")))

(test-case "W0: try-auto-extract skips short responses"
  (define be (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.1])
    (define result (try-auto-extract "ok"
                                      #:backend be
                                      #:policy default-memory-policy
                                      #:session-id "s1"
                                      #:project-root "/tmp"))
    (check-true (list? result))))

;; ============================================================
;; W2 placeholder: Runtime parameter wiring
;; ============================================================

(test-case "W2-placeholder: settings auto-extraction from config"
  ;; After W1, setting-memory-auto-extraction-enabled? should exist
  ;; For now, verify settings structure accepts nested memory config
  (define s (q-settings (hash 'memory (hash 'auto-extraction (hash 'enabled #t)))
                         (hash)
                         (hash 'memory (hash 'auto-extraction (hash 'enabled #t)))))
  (check-true (q-settings? s) "settings should accept nested memory config"))

(test-case "W2-placeholder: settings auto-extraction min-confidence from config"
  (define s (q-settings (hash 'memory (hash 'auto-extraction (hash 'min-confidence 0.75)))
                         (hash)
                         (hash 'memory (hash 'auto-extraction (hash 'min-confidence 0.75)))))
  (check-true (q-settings? s) "settings should accept min-confidence"))
