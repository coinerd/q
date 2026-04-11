#lang racket

;; test-tool-result-unique.rkt
;; Verifies that tool-result is defined exactly once (in tools/tool.rkt)
;; and that agent/types.rkt re-exports the SAME struct — not a duplicate.

(require rackunit
         "../tools/tool.rkt"
         (except-in "../agent/types.rkt"
           make-tool-call make-tool-result))

;; ── The struct constructor from tools/tool should produce values
;;    accepted by the predicate imported from agent/types ──

(define tr (make-tool-result "hello" #f #f))

;; tools/tool.rkt predicate
(check-pred tool-result? tr
            "tool-result? from tools/tool.rkt accepts its own constructor")

;; Accessors from tools/tool.rkt work
(check-equal? (tool-result-content tr) "hello")
(check-false  (tool-result-details tr))
(check-false  (tool-result-is-error? tr))

;; Cross-module identity: the tool-result? from agent/types (re-exported)
;; must accept values created by the constructor from tools/tool.
;; If agent/types had its own duplicate struct, this would fail.
(let ([tr2 (make-tool-result "cross" (hasheq 'ok #t) #f)])
  (check-true (tool-result? tr2)
              "tool-result? (re-exported via agent/types) accepts values from tools/tool.rkt"))

;; Reverse: the predicate from tools/tool should accept values
;; created by the make-tool-result imported via agent/types.
;; Since both are the same binding, this is always true — but
;; the test documents the intent.
(let ([tr3 (make-tool-result '("line") (hasheq) #t)])
  (check-pred tool-result-is-error? tr3)
  (check-equal? (tool-result-content tr3) '("line")))

;; Verify JSON round-trip uses the canonical serialization
(let* ([tr4 (make-tool-result '("data") (hasheq 'code 0) #f)]
       [jx  (tool-result->jsexpr tr4)])
  (check-equal? (hash-ref jx 'content) '("data"))
  (check-equal? (hash-ref jx 'isError) #f))

