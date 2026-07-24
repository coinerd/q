#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-preflight-entry.rkt — Preflight entry struct tests (W-10)

(require rackunit
         "../tools/scheduler-preflight.rkt"
         "../tools/tool.rkt"
         "../util/capability.rkt")

;; A no-op hook dispatcher (no transformations, nothing blocked).
;; Signature: (hook-dispatcher event-symbol tool-call) -> tool-call | #f | hook-result
(define (identity-hook-dispatcher event tc) #f)

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

;; ============================================================
;; W3 (v0.99.66 Finding #3): capability denial produces a 'blocked entry
;; ============================================================

;; A dummy tool that requires the 'shell-exec capability.
(define (make-shell-tool)
  (make-tool "bash"
             "dummy bash tool"
             (hasheq 'type "object"
                     'properties (hasheq)
                     'required '())
             (lambda (args) (make-tool-result "ok" #f #f))
             #:required-capability 'shell-exec))

(test-case "W3: preflight blocks tool when required capability is missing"
  ;; Build a registry containing a shell-exec-only tool, then run preflight
  ;; with a session that only grants 'read-only. The capability check must
  ;; produce a 'blocked preflight-entry with a descriptive message.
  (define reg (make-tool-registry))
  (register-tool! reg (make-shell-tool))
  (define tc (tool-call #f "bash" (hasheq)))
  (define result
    (parameterize ([current-session-capabilities '(read-only)])
      (run-preflight (list tc) reg identity-hook-dispatcher)))
  (define entry (car result))
  (check-eq? (preflight-entry-status entry) 'blocked)
  (check-false (preflight-entry-tool entry))
  (check-regexp-match #rx"requires capability 'shell-exec"
                      (preflight-entry-error-message entry)))
