#lang racket/base

;; BOUNDARY: integration

;; tests/test-preflight-entry.rkt — Preflight entry struct tests (W-10)

(require rackunit
         "../tools/scheduler.rkt"
         "../util/tool-types.rkt")

;; ============================================================
;; W-10: preflight-entry struct
;; ============================================================

(test-case "preflight-entry construction - ready"
  (define tc (tool-call "read" (hash 'path "/tmp/test.rkt") "tc-1"))
  (define entry (preflight-entry 'ready tc 'some-tool #f))
  (check-eq? (preflight-entry-status entry) 'ready)
  (check-equal? (preflight-entry-tool-call entry) tc)
  (check-eq? (preflight-entry-tool entry) 'some-tool)
  (check-false (preflight-entry-error-message entry)))

(test-case "preflight-entry construction - blocked"
  (define tc (tool-call "edit" (hash) "tc-2"))
  (define entry (preflight-entry 'blocked tc #f "blocked by safe-mode"))
  (check-eq? (preflight-entry-status entry) 'blocked)
  (check-equal? (preflight-entry-error-message entry) "blocked by safe-mode"))

(test-case "preflight-entry construction - error"
  (define tc (tool-call "unknown" (hash) "tc-3"))
  (define entry (preflight-entry 'error tc #f "unknown tool: 'unknown'"))
  (check-eq? (preflight-entry-status entry) 'error)
  (check-false (preflight-entry-tool entry)))

(test-case "preflight-entry is transparent"
  (define tc (tool-call "read" (hash) "tc-4"))
  (define entry (preflight-entry 'ready tc #f #f))
  (check-true (preflight-entry? entry))
  ;; Transparent struct: can destructure
  (check-equal? (preflight-entry-tool-call entry) tc))

(test-case "preflight-entry predicate rejects non-entries"
  (check-false (preflight-entry? 'ready))
  (check-false (preflight-entry? (hash 'status 'ready)))
  (check-false (preflight-entry? #f)))
