#lang racket/base
;;; test-memory-backend-config.rkt — W0/W4 characterization tests
;;;
;;; W0: Characterize complex backend config gaps.
;;; W4: Verify recursive backend factory (after implementation).
(require rackunit
         "../runtime/memory/service.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/settings.rkt")

;; ============================================================
;; W0: Current backend config characterization
;; ============================================================

(test-case "W0: simple 'hash backend spec works"
  (parameterize ([current-memory-backend #f])
    (define cfg (hash 'memory-backend 'hash))
    (define be (initialize-memory-backend! cfg))
    (check-true (memory-backend? be) "hash backend should be created")
    (check-true (memory-service-available?) "service should be available")
    (current-memory-backend #f)))

(test-case "W0: simple 'file-jsonl backend spec works"
  (parameterize ([current-memory-backend #f])
    (define cfg (hash 'memory-backend 'file-jsonl 'session-dir "/tmp"))
    (define be (initialize-memory-backend! cfg))
    (check-true (memory-backend? be) "file-jsonl backend should be created")
    (current-memory-backend #f)))

(test-case "W0: #f backend spec disables memory"
  (parameterize ([current-memory-backend #f])
    (define cfg (hash 'memory-backend #f))
    (define be (initialize-memory-backend! cfg))
    (check-false be "no backend should be created for #f")
    (check-false (memory-service-available?) "service should not be available")))

(test-case "W0: unknown backend spec disables memory"
  (parameterize ([current-memory-backend #f])
    (define cfg (hash 'memory-backend 'unknown))
    (define be (initialize-memory-backend! cfg))
    (check-false be "unknown spec should produce no backend")))

(test-case "W0: complex chained backend spec not yet handled"
  (parameterize ([current-memory-backend #f])
    (define cfg (hash 'memory-backend (hash 'type 'chained 'l1 'hash 'l2 'file-jsonl)))
    ;; Currently falls through to 'else clause → returns #f
    (define be (initialize-memory-backend! cfg))
    (check-false be "chained backend not yet supported — expected red")))

(test-case "W0: complex external backend spec not yet handled"
  (parameterize ([current-memory-backend #f])
    (define cfg
      (hash 'memory-backend
            (hash 'type
                  'external
                  'provider
                  'mem0
                  'api-key-env
                  "MEM0_API_KEY"
                  'base-url
                  "https://api.mem0.ai")))
    (define be (initialize-memory-backend! cfg))
    (check-false be "external backend not yet supported — expected red")))

(test-case "W0: settings returns symbol backend only"
  (define s (make-minimal-settings #:overrides (hash 'memory (hash 'backend 'hash))))
  (check-equal? (setting-memory-backend s) 'hash)
  (define s2 (make-minimal-settings))
  (check-false (setting-memory-backend s2)))

(test-case "W0: settings does not return complex backend spec"
  (define s
    (make-minimal-settings #:overrides (hash 'memory (hash 'backend (hash 'type 'chained 'l1 'hash 'l2 'file-jsonl)))))
  ;; Currently setting-memory-backend only handles symbol/string
  ;; After W4 this should return the hash spec
  (define result (setting-memory-backend s))
  ;; Either #f or the hash — characterize current behavior
  (check-true (or (eq? result #f) (hash? result)) "complex spec should either be #f or pass through"))
