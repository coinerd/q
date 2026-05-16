#lang racket

;; BOUNDARY: pure

;; tests/test-session-lifecycle-pure.rkt -- W0: Pure helper tests for session-lifecycle.rkt
;;
;; Tests the extracted pure helpers:
;;   - compute-parent-id
;;   - inject-system-instructions

(require rackunit
         rackunit/text-ui
         "../runtime/session-lifecycle.rkt"
         "../runtime/context-assembly.rkt"
         "../util/protocol-types.rkt")

;; Build a minimal message for testing
(define (test-msg id kind role content-text [parent-id #f])
  (make-message id parent-id role kind (list (make-text-part content-text)) 0 (hasheq)))

(define pure-suite
  (test-suite "session-lifecycle pure helpers"

    ;; -- compute-parent-id: empty entries, no index --
    (test-case "compute-parent-id with empty entries returns #f"
      (check-equal? (compute-parent-id '() #f) #f))

    ;; -- compute-parent-id: entries with session-info skipped --
    (test-case "compute-parent-id skips session-info and returns last real message id"
      (define m1 (test-msg "msg-1" 'message 'user "hello"))
      (define m2 (test-msg "msg-2" 'message 'assistant "hi"))
      (define info (test-msg "info-1" 'session-info 'system "{}"))
      (check-equal? (compute-parent-id (list m1 info m2) #f) "msg-2"))

    ;; -- compute-parent-id: single entry --
    (test-case "compute-parent-id with single entry returns its id"
      (define m1 (test-msg "msg-1" 'message 'user "hello"))
      (check-equal? (compute-parent-id (list m1) #f) "msg-1"))

    ;; -- compute-parent-id: all session-info returns #f --
    (test-case "compute-parent-id with only session-info returns #f"
      (define info (test-msg "info-1" 'session-info 'system "{}"))
      (check-equal? (compute-parent-id (list info) #f) #f))

    ;; -- inject-system-instructions: empty instructions --
    (test-case "inject-system-instructions with empty list returns context unchanged"
      (define ctx (list (test-msg "m1" 'message 'user "hello")))
      (check-equal? (inject-system-instructions ctx '()) ctx))

    ;; -- inject-system-instructions: non-empty instructions --
    (test-case "inject-system-instructions prepends system message"
      (define ctx (list (test-msg "m1" 'message 'user "hello")))
      (define result (inject-system-instructions ctx '("Be helpful")))
      (check-equal? (length result) 2)
      (check-equal? (message-role (car result)) 'system)
      (check-equal? (message-kind (car result)) 'system-instruction))

    ;; -- inject-system-instructions: multi-instruction joins with double newline --
    (test-case "inject-system-instructions joins multiple instructions"
      (define ctx (list (test-msg "m1" 'message 'user "hello")))
      (define result (inject-system-instructions ctx '("Be helpful" "Be concise")))
      (check-equal? (length result) 2)
      (define sys-msg (car result))
      (check-equal? (message-role sys-msg) 'system)
      ;; Content should contain both instructions separated by double newline
      (define content (hash-ref (content-part->jsexpr (car (message-content sys-msg))) 'text))
      (check-true (string-contains? content "Be helpful"))
      (check-true (string-contains? content "Be concise")))

    ;; ── v0.45.16 regression: build-session-context shadowing ──
    ;; The bug: local 4-arg build-session-context shadowed imported 1-arg version,
    ;; causing arity crash on 2nd prompt when session has an index.
    ;; Fix: renamed import → build-session-context/from-index (internal),
    ;;       local → build-session-context-for-prompt (exported).
    ;; This test verifies the two functions remain distinct.
    (test-case "v0.45.16 regression: imported build-session-context distinct from for-prompt"
      ;; build-session-context from context-assembly (1-arg, the original import)
      (check-pred procedure? build-session-context)
      ;; build-session-context-for-prompt from session-lifecycle (4-arg, the local)
      (check-pred procedure? build-session-context-for-prompt)
      ;; They must NOT be the same procedure
      (check-false (eq? build-session-context build-session-context-for-prompt))
      ;; Verify arities: 1-arg import vs 4-arg local
      (check-equal? (procedure-arity build-session-context) 1)
      (check-equal? (procedure-arity build-session-context-for-prompt) 4))))

(run-tests pure-suite 'verbose)
