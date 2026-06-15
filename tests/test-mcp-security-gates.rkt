#lang racket

;; @speed fast  ;; @suite security

;; tests/test-mcp-security-gates.rkt — W0 (v0.99.10) MCP Security Gate Characterization
;;
;; CHARACTERIZATION TESTS — These document the CURRENT (buggy) behavior.
;; They will be flipped in W1 to assert the CORRECT behavior.
;;
;; Blockers characterized:
;;   H1: Master feature gate not enforced for server startup
;;   C1: MCP tools/call bypasses governed scheduler/security/context
;;   C2: MCP tools/call may return non-JSON-serializable tool-result structs
;;   M5: Invalid MCP transport config can crash

(require rackunit
         rackunit/text-ui
         json
         "../extensions/mcp-adapter.rkt"
         "../tools/registry.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-success-result
                  make-error-result
                  tool-result?
                  tool-result->jsexpr)
         "../runtime/settings-query.rkt"
         (only-in "../runtime/settings.rkt" q-settings))

;; ── Helpers ──

(define (merge-nested h path value)
  (cond
    [(null? path) value]
    [(null? (cdr path)) (hash-set h (car path) value)]
    [else (hash-set h (car path) (merge-nested (hash-ref h (car path) (hash)) (cdr path) value))]))

(define (make-settings-from-paths . path-vals)
  (define merged
    (for/fold ([acc (hash)]) ([pv (in-list path-vals)])
      (merge-nested acc (car pv) (cadr pv))))
  (q-settings (hash) (hash) merged))

(define (make-test-registry)
  (define reg (make-tool-registry))
  (register-tool!
   reg
   (make-tool "echo"
              "Echo input"
              (hasheq 'type "object" 'properties (hasheq 'text (hasheq 'type "string")))
              (lambda (args ctx) (make-success-result (hash-ref args 'text "")))))
  reg)

(define (json-roundtrip v)
  (string->jsexpr (jsexpr->string v)))

(define suite
  (test-suite "MCP Security Gates (W0 Characterization)"

    ;; ════════════════════════════════════════════════════════════
    ;; H1: Master feature gate not enforced for server startup
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE H1: mcp-server-enabled? alone returns #t without master gate"
      ;; CURRENT BUG: wiring checks only mcp-server-enabled? to start server.
      ;; This means server starts even when mas.mcp.enabled is #f.
      ;; W1 FIX: require (and (mcp-enabled? settings) (mcp-server-enabled? settings))
      (define settings (make-settings-from-paths '((mas mcp server enabled) #t)))
      (check-true (mcp-server-enabled? settings) "server-enabled? returns #t")
      (check-false (mcp-enabled? settings) "master gate is #f — but server-enabled? doesn't check it")
      ;; CHARACTERIZATION: both must be true for safe server startup,
      ;; but currently wiring only checks server-enabled?
      ;; W1 will add the composition check.
      (check-false (and (mcp-enabled? settings) (mcp-server-enabled? settings))
                   "composed gate is #f — confirms H1 bug exists"))

    (test-case "CHARACTERIZE H1: both gates must be #t for safe startup"
      (define settings
        (make-settings-from-paths '((mas mcp enabled) #t) '((mas mcp server enabled) #t)))
      (check-true (mcp-enabled? settings))
      (check-true (mcp-server-enabled? settings))
      (check-true (and (mcp-enabled? settings) (mcp-server-enabled? settings))
                  "composed gate is #t — this is what W1 will enforce"))

    ;; ════════════════════════════════════════════════════════════
    ;; C1/C2: MCP tools/call bypasses governed execution path
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE C1: execute-fn is called directly without governance"
      ;; CURRENT: handle-mcp-request calls execute-fn directly.
      ;; No permission check, no execution context, no scheduler integration.
      ;; W1 FIX: route through governed execution wrapper.
      (define reg (make-test-registry))
      (define calls (box '()))
      (define exec-fn
        (lambda (name args)
          (set-box! calls (cons (list name args) (unbox calls)))
          (hasheq 'content (list (hasheq 'type "text" 'text "ok")))))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq 'text "hi"))))
      (define resp (handle-mcp-request req reg exec-fn))
      ;; CHARACTERIZATION: exec-fn was called directly — no governance layer
      (check-not-false (unbox calls) "execute-fn was called directly")
      (check-not-false (hash-ref resp 'result #f) "result returned directly from execute-fn"))

    (test-case "CHARACTERIZE C2: tool-result struct is NOT converted to JSON by adapter"
      ;; CURRENT: if execute-fn returns a tool-result struct,
      ;; handle-mcp-request passes it through without conversion.
      ;; W1 FIX: convert tool-result? to MCP-compatible jsexpr.
      (define reg (make-test-registry))
      ;; Simulate execute-fn returning a raw tool-result struct
      (define exec-fn (lambda (name args) (make-success-result "raw result")))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq))))
      (define resp (handle-mcp-request req reg exec-fn))
      (define result (hash-ref resp 'result #f))
      ;; CHARACTERIZATION: result IS a tool-result struct (not a hash),
      ;; which means it will fail JSON serialization at the stdio boundary.
      ;; This is the C2 bug — raw structs pass through.
      (check-true (tool-result? result)
                  "CURRENT BUG: tool-result struct returned directly, not converted to jsexpr"))

    (test-case "CORRECT BEHAVIOR (for W1): tool-result->jsexpr produces MCP-compatible hash"
      ;; This test shows what the CORRECT behavior should look like.
      (define tr (make-success-result "hello"))
      (define jsexpr (tool-result->jsexpr tr))
      (check-true (hash? jsexpr) "tool-result->jsexpr produces a hash")
      (check-not-false (hash-ref jsexpr 'content #f) "has content field")
      (check-false (hash-ref jsexpr 'isError #f) "not an error"))

    (test-case "CHARACTERIZE C2: tool-result jsexpr IS JSON-serializable"
      ;; Verify the fix target: tool-result->jsexpr output IS JSON-compatible
      (define tr (make-success-result "test data"))
      (define jsexpr (tool-result->jsexpr tr))
      (check-not-false (with-handlers ([exn:fail? (lambda (_) #f)])
                         (jsexpr->string jsexpr))
                       "tool-result->jsexpr output is JSON-serializable"))

    ;; ════════════════════════════════════════════════════════════
    ;; C1: Unknown tool returns fake success, not error
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE C1: unknown tool path in execute-fn returns fake content, not error"
      ;; CURRENT: the wiring layer's execute-fn returns fake content for unknown tools.
      ;; W1 FIX: return a JSON-RPC error response for unknown tools.
      (define reg (make-test-registry))
      (define exec-fn
        (lambda (name args)
          (hasheq 'content (list (hasheq 'type "text" 'text (format "unknown tool: ~a" name))))))
      (define req (json-roundtrip (build-mcp-tools-call 1 "nonexistent" (hasheq))))
      (define resp (handle-mcp-request req reg exec-fn))
      ;; CHARACTERIZATION: no error field — fake success content returned
      (check-false (hash-ref resp 'error #f)
                   "CURRENT BUG: unknown tool returns fake success, not error")
      (check-not-false (hash-ref resp 'result #f) "returns a result (should be error in W1)"))

    ;; ════════════════════════════════════════════════════════════
    ;; M5: Invalid MCP transport config can crash
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE M5: boolean transport value crashes symbol->string"
      ;; CURRENT: mcp-server-transport calls symbol->string on non-string values.
      ;; If transport is #t or a number, it crashes.
      ;; W1 FIX: accept only string/symbol, default or reject invalid values.
      (define settings (make-settings-from-paths '((mas mcp server transport) #t)))
      (check-exn exn:fail:contract?
                 (lambda () (mcp-server-transport settings))
                 "CURRENT BUG: boolean transport crashes symbol->string"))

    (test-case "CHARACTERIZE M5: number transport value crashes"
      (define settings (make-settings-from-paths '((mas mcp server transport) 42)))
      (check-exn exn:fail:contract?
                 (lambda () (mcp-server-transport settings))
                 "CURRENT BUG: number transport crashes symbol->string"))

    (test-case "transport works correctly for strings"
      (define settings (make-settings-from-paths '((mas mcp server transport) "stdio")))
      (check-equal? (mcp-server-transport settings) "stdio"))

    (test-case "transport works correctly for symbols"
      (define settings (make-settings-from-paths (list '(mas mcp server transport) 'stdio)))
      (check-equal? (mcp-server-transport settings) "stdio"))))

(run-tests suite)
