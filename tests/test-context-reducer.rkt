#lang racket

;; tests/test-context-reducer.rkt — Pair-aware context reducer (#1329)
;;
;; Tests that trim-context-pair-aware keeps tool_call/result pairs atomic
;; and ensures the first non-system message is always 'user'.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../runtime/context-reducer.rkt")

;; Helper: create a simple message
(define (make-simple-msg id role [content '()])
  (make-message id #f role 'message content 0 (hasheq)))

;; Helper: create a tool_call assistant message
(define (make-tool-call-msg id tool-call-id tool-name)
  (make-message id
                #f
                'assistant
                'message
                (list (make-tool-call-part tool-call-id tool-name (hasheq)))
                0
                (hasheq)))

;; Helper: create a tool result message
(define (make-tool-result-msg id tool-call-id [content "ok"])
  (make-message id
                #f
                'tool
                'message
                (list (make-tool-result-part tool-call-id content #f))
                0
                (hasheq)))

;; Helper: create a system message
(define (make-system-msg id text)
  (make-message id #f 'system 'message (list (make-text-part text)) 0 (hasheq)))

(define context-reducer-suite
  (test-suite "context-reducer: pair-aware trim"

    ;; ============================================================
    ;; Basic trimming
    ;; ============================================================

    (test-case "trim keeps all messages when under keep-count"
      (define msgs
        (list (make-system-msg "sys" "system")
              (make-simple-msg "u1" 'user)
              (make-simple-msg "a1" 'assistant)))
      (define result (trim-context-pair-aware msgs 10))
      (check-equal? (length result) 3)
      (check-equal? (message-role (list-ref result 0)) 'system)
      (check-equal? (message-role (list-ref result 1)) 'user))

    (test-case "trim reduces to keep-count when messages are simple"
      (define msgs
        (append (list (make-system-msg "sys" "system"))
                (for/list ([i (in-range 10)])
                  (make-simple-msg (format "u~a" i)
                                   (if (even? i) 'user 'assistant)))))
      (define result (trim-context-pair-aware msgs 4))
      ;; system + 4 non-system = 5
      (check-equal? (length result) 5))

    ;; ============================================================
    ;; Tool call/result pairing
    ;; ============================================================

    (test-case "tool_call/result pair kept together after trim"
      (define msgs
        (list (make-system-msg "sys" "system")
              (make-simple-msg "u1" 'user)
              (make-tool-call-msg "a1" "tc1" "bash")
              (make-tool-result-msg "t1" "tc1")
              (make-simple-msg "u2" 'user)
              (make-tool-call-msg "a2" "tc2" "edit")
              (make-tool-result-msg "t2" "tc2")))
      ;; Trim to 4 non-system msgs — should keep both pairs intact
      (define result (trim-context-pair-aware msgs 4))
      (define non-system (filter (lambda (m) (not (eq? (message-role m) 'system))) result))
      ;; Every tool result must be preceded by its tool_call assistant
      (for ([m (in-list non-system)]
            [i (in-naturals)])
        (when (eq? (message-role m) 'tool)
          (check-true (> i 0) "tool msg has preceding msg")
          (when (> i 0)
            (define prev (list-ref non-system (sub1 i)))
            (check-equal? (message-role prev) 'assistant
                          "tool msg preceded by assistant")))))

    (test-case "trim never splits a tool_call from its result"
      (define msgs
        (list (make-system-msg "sys" "system")
              (make-simple-msg "u1" 'user)
              (make-simple-msg "a1" 'assistant)
              (make-simple-msg "u2" 'user)
              (make-tool-call-msg "a2" "tc1" "bash")
              (make-tool-result-msg "t1" "tc1")
              (make-tool-call-msg "a3" "tc2" "edit")
              (make-tool-result-msg "t2" "tc2")))
      ;; Trim to 3 — should not split any pair
      (define result (trim-context-pair-aware msgs 3))
      (define non-system (filter (lambda (m) (not (eq? (message-role m) 'system))) result))
      ;; Check no trailing tool_call without result
      (define last-idx (sub1 (length non-system)))
      (when (> (length non-system) 0)
        (define last-msg (list-ref non-system last-idx))
        ;; Last message should not be an assistant with tool_calls
        ;; (unless followed by tool results)
        (check-false
         (and (eq? (message-role last-msg) 'assistant)
              (for/or ([cp (in-list (message-content last-msg))])
                (tool-call-part? cp)))
         "last non-system msg should not be a dangling tool_call")))

    (test-case "orphan tool result removed at trim boundary"
      (define msgs
        (list (make-system-msg "sys" "system")
              (make-simple-msg "u1" 'user)
              (make-tool-call-msg "a1" "tc1" "bash")
              (make-tool-result-msg "t1" "tc1")
              (make-simple-msg "u2" 'user)
              (make-tool-result-msg "t2" "orphan-tc")))
      ;; The orphan tool result for "orphan-tc" has no matching tool_call
      ;; Trim should drop it
      (define result (trim-context-pair-aware msgs 3))
      (define non-system (filter (lambda (m) (not (eq? (message-role m) 'system))) result))
      ;; No tool msg should reference an id not in our kept messages
      (define kept-ids (for/set ([m (in-list non-system)]) (message-id m)))
      (for ([m (in-list non-system)])
        (when (eq? (message-role m) 'tool)
          ;; Find its tool-call-id in content
          (for ([cp (in-list (message-content m))])
            (when (tool-result-part? cp)
              (check-true
               (for/or ([other (in-list non-system)])
                 (for/or ([cp2 (in-list (message-content other))])
                   (and (tool-call-part? cp2)
                        (equal? (tool-call-part-id cp2)
                                (tool-result-part-tool-call-id cp)))))
               "tool result has matching tool call"))))))

    ;; ============================================================
    ;; First-message guarantee
    ;; ============================================================

    (test-case "first non-system message is always user"
      (define msgs
        (list (make-system-msg "sys" "system")
              (make-simple-msg "u1" 'user)
              (make-tool-call-msg "a1" "tc1" "bash")
              (make-tool-result-msg "t1" "tc1")
              (make-tool-call-msg "a2" "tc2" "edit")
              (make-tool-result-msg "t2" "tc2")))
      ;; Trim to 2 — would start with tool if not fixed
      (define result (trim-context-pair-aware msgs 2))
      (define non-system (filter (lambda (m) (not (eq? (message-role m) 'system))) result))
      (when (> (length non-system) 0)
        (check-equal? (message-role (car non-system)) 'user
                      "first non-system message is user")))

    (test-case "synthetic user message when all user msgs trimmed"
      (define msgs
        (list (make-system-msg "sys" "system")
              (make-tool-call-msg "a1" "tc1" "bash")
              (make-tool-result-msg "t1" "tc1")
              (make-tool-call-msg "a2" "tc2" "edit")
              (make-tool-result-msg "t2" "tc2")))
      ;; No user messages at all — trim should add synthetic
      (define result (trim-context-pair-aware msgs 2))
      (define non-system (filter (lambda (m) (not (eq? (message-role m) 'system))) result))
      (when (> (length non-system) 0)
        (check-equal? (message-role (car non-system)) 'user
                      "first non-system is user (synthetic if needed)")))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================

    (test-case "empty context returns empty"
      (define result (trim-context-pair-aware '() 5))
      (check-equal? result '()))

    (test-case "only system messages returns system messages"
      (define msgs (list (make-system-msg "sys" "system")))
      (define result (trim-context-pair-aware msgs 5))
      (check-equal? (length result) 1)
      (check-equal? (message-role (car result)) 'system))))

(run-tests context-reducer-suite)
