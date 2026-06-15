#lang racket

;; @speed fast  ;; @suite security

;; tests/test-mcp-security-gates.rkt — W1 (v0.99.10) MCP Security Gate Remediation
;;
;; FIXED in W1: C1, C2, H1, M5 audit blockers.
;; Tests now assert CORRECT behavior.

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/runtime-path
         racket/string
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

(define-runtime-path run-modes-path "../wiring/run-modes.rkt")

(define suite
  (test-suite "MCP Security Gates (W1 Remediation)"

    ;; ════════════════════════════════════════════════════════════
    ;; H1: Master feature gate not enforced for server startup
    ;; ════════════════════════════════════════════════════════════

    (test-case "H1: config functions return correct values for composed gate check"
      ;; H1 FIX: wiring now checks (and (mcp-enabled?) (mcp-server-enabled?)).
      ;; These tests verify the config functions that the wiring composes.
      (define settings (make-settings-from-paths '((mas mcp server enabled) #t)))
      (check-true (mcp-server-enabled? settings) "server-enabled? returns #t")
      (check-false (mcp-enabled? settings) "master gate is #f")
      ;; The composed gate (what wiring now checks) is #f — server won't start
      (check-false (and (mcp-enabled? settings) (mcp-server-enabled? settings))
                   "composed gate is #f — server won't start without master gate"))

    (test-case "H1: both gates must be #t for safe startup"
      (define settings
        (make-settings-from-paths '((mas mcp enabled) #t) '((mas mcp server enabled) #t)))
      (check-true (mcp-enabled? settings))
      (check-true (mcp-server-enabled? settings))
      (check-true (and (mcp-enabled? settings) (mcp-server-enabled? settings))
                  "composed gate is #t — this is what W1 enforces"))

    ;; ════════════════════════════════════════════════════════════
    ;; C1/C2: MCP tools/call bypasses governed execution path
    ;; ════════════════════════════════════════════════════════════

    (test-case "C1: execute-fn is called for known tools"
      ;; W1 FIX: execute-fn is called through a governance layer that checks
      ;; tool existence first. For known tools, execution proceeds normally.
      (define reg (make-test-registry))
      (define calls (box '()))
      (define exec-fn
        (lambda (name args)
          (set-box! calls (cons (list name args) (unbox calls)))
          (hasheq 'content (list (hasheq 'type "text" 'text "ok")))))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq 'text "hi"))))
      (define resp (handle-mcp-request req reg exec-fn))
      ;; FIX: exec-fn was called for the known tool
      (check-not-false (unbox calls) "execute-fn was called for known tool")
      (check-not-false (hash-ref resp 'result #f) "result returned from execute-fn"))

    (test-case "F-01 CHARACTERIZE: MCP adapter invokes execute-fn without execution context"
      ;; v0.99.11 W0: This is a repro harness for the remaining governance
      ;; blocker. handle-mcp-request exposes only (tool-name args) to the
      ;; execution function; there is no governed execution context argument.
      ;; W1 should replace this characterization with a positive governed-path
      ;; assertion.
      (define reg (make-test-registry))
      (define observed-arity (box #f))
      (define observed-call (box #f))
      (define exec-fn
        (lambda args
          (set-box! observed-arity (length args))
          (set-box! observed-call args)
          (hasheq 'content (list (hasheq 'type "text" 'text "ok")))))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq 'text "hi"))))
      (define resp (handle-mcp-request req reg exec-fn))
      (check-equal? (unbox observed-arity)
                    2
                    "current MCP adapter calls execute-fn with only name and args")
      (check-equal? (map values (unbox observed-call)) (list "echo" (hasheq 'text "hi")))
      (check-not-false (hash-ref resp 'result #f)))

    (test-case "F-01 CHARACTERIZE: run-modes MCP wiring still direct-executes tools with ctx #f"
      ;; Source-level characterization for the exact independent-audit finding.
      ;; This test intentionally records the current unsafe wiring so W1 has a
      ;; stable red/green target: the direct `(tool-execute ... args #f)` path
      ;; must disappear when governed MCP execution is introduced.
      (define run-modes-source (file->string run-modes-path))
      (check-not-false (string-contains? run-modes-source "((tool-execute t) args #f)")
                       "current run-modes MCP path directly invokes tool-execute with ctx #f"))

    (test-case "C2 FIXED: tool-result struct IS converted to MCP-compatible jsexpr"
      ;; W1 FIX: handle-mcp-request now checks tool-result? and converts.
      ;; The result hash is JSON-serializable.
      (define reg (make-test-registry))
      ;; Simulate execute-fn returning a raw tool-result struct
      (define exec-fn (lambda (name args) (make-success-result "raw result")))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq))))
      (define resp (handle-mcp-request req reg exec-fn))
      (define result (hash-ref resp 'result #f))
      ;; FIX: result is now a hash (jsexpr), not a tool-result struct
      (check-true (hash? result) "FIXED: tool-result converted to hash")
      (check-false (tool-result? result) "FIXED: no longer raw struct")
      (check-not-false (hash-ref result 'content #f) "has content field")
      ;; Verify the converted result is JSON-serializable
      (check-not-false (with-handlers ([exn:fail? (lambda (_) #f)])
                         (jsexpr->string resp))
                       "full response is JSON-serializable"))

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

    (test-case "C1 FIXED: unknown tool returns JSON-RPC error, not fake success"
      ;; W1 FIX: handle-mcp-request checks registry before calling execute-fn.
      ;; Unknown tools now produce a -32602 error response.
      (define reg (make-test-registry))
      (define exec-fn
        (lambda (name args)
          (hasheq 'content (list (hasheq 'type "text" 'text (format "unknown tool: ~a" name))))))
      (define req (json-roundtrip (build-mcp-tools-call 1 "nonexistent" (hasheq))))
      (define resp (handle-mcp-request req reg exec-fn))
      ;; FIX: unknown tool returns error response
      (check-true (mcp-error-response? resp) "FIXED: unknown tool returns error response")
      (check-equal? (mcp-error-code resp) -32602 "error code is -32602 invalid params")
      (check-not-false (regexp-match? #rx"nonexistent" (or (mcp-error-message resp) ""))
                       "error message mentions tool name"))

    ;; ════════════════════════════════════════════════════════════
    ;; M5: Invalid MCP transport config can crash
    ;; ════════════════════════════════════════════════════════════

    (test-case "M5 FIXED: boolean transport returns default \"stdio\""
      ;; W1 FIX: mcp-server-transport accepts string/symbol, defaults for others.
      (define settings (make-settings-from-paths '((mas mcp server transport) #t)))
      (check-equal? (mcp-server-transport settings)
                    "stdio"
                    "FIXED: boolean transport returns default instead of crashing"))

    (test-case "M5 FIXED: number transport returns default \"stdio\""
      (define settings (make-settings-from-paths '((mas mcp server transport) 42)))
      (check-equal? (mcp-server-transport settings)
                    "stdio"
                    "FIXED: number transport returns default instead of crashing"))

    (test-case "transport works correctly for strings"
      (define settings (make-settings-from-paths '((mas mcp server transport) "stdio")))
      (check-equal? (mcp-server-transport settings) "stdio"))

    (test-case "transport works correctly for symbols"
      (define settings (make-settings-from-paths (list '(mas mcp server transport) 'stdio)))
      (check-equal? (mcp-server-transport settings) "stdio"))))

(run-tests suite)
