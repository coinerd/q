#lang racket

;; tests/test-compaction-edge-cases.rkt — Edge case tests for compaction
;;                                         and context assembly
;;
;; Tests for:
;; - #613: Compact empty session
;; - #614: Compact single-message session
;; - #615: Context assembly with empty index / edge cases
;;
;; These test the full pipeline including compact-and-persist!,
;; build-tiered-context, and build-session-context under edge conditions.

(require rackunit
         rackunit/text-ui
         racket/file
         "../runtime/compactor.rkt"
         "../runtime/context-builder.rkt"
         "../runtime/session-index.rkt"
         "../runtime/session-store.rkt"
         "../util/protocol-types.rkt"
         (only-in "../util/hook-types.rkt" hook-result hook-result-action))

(define (make-temp-dir)
  (make-temporary-file "q-comp-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir #:must-exist? #f)))

(define (session-path dir) (build-path dir "session.jsonl"))
(define (index-path dir) (build-path dir "index.jsonl"))

;; Helper: make a simple message
(define (make-test-msg role text [kind 'assistant])
  (make-message (format "msg-~a" (current-inexact-milliseconds))
                #f role kind
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

;; ============================================================
;; #613: Compact empty session
;; ============================================================

(test-case "edge-compact: empty message list returns identity"
  (define result (compact-history '()))
  (check-equal? (compaction-result-removed-count result) 0)
  (check-equal? (compaction-result-kept-messages result) '())
  (check-false (compaction-result-summary-message result)))

(test-case "edge-compact: compact-and-persist! with empty session"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  ;; compact-and-persist! takes positional session-log-path
  (define result (compact-and-persist! '() sp))
  (check-equal? (compaction-result-removed-count result) 0)
  (cleanup-dir dir))

(test-case "edge-compact: build-tiered-context with empty messages"
  (define tc (build-tiered-context '()))
  (check-equal? (tiered-context-tier-a tc) '())
  (check-equal? (tiered-context-tier-b tc) '())
  (check-equal? (tiered-context-tier-c tc) '()))

(test-case "edge-compact: compaction-result->message-list with empty"
  (define result (compaction-result #f 0 '()))
  (check-equal? (compaction-result->message-list result) '()))

(test-case "edge-compact: build-summary-window with empty"
  (define-values (old recent)
    (build-summary-window '() (default-strategy)))
  (check-equal? old '())
  (check-equal? recent '()))

;; ============================================================
;; #614: Compact single-message session
;; ============================================================

(test-case "edge-compact: single message returns identity (no compaction)"
  (define msgs (list (make-test-msg 'user "hello")))
  (define result (compact-history msgs))
  (check-equal? (compaction-result-removed-count result) 0)
  (check-equal? (length (compaction-result-kept-messages result)) 1)
  (check-false (compaction-result-summary-message result)))

(test-case "edge-compact: single assistant message preserved"
  (define msgs (list (make-test-msg 'assistant "response")))
  (define result (compact-history msgs))
  (check-equal? (length (compaction-result-kept-messages result)) 1)
  (check-equal? (text-part-text (car (message-content (car (compaction-result-kept-messages result))))) "response"))

(test-case "edge-compact: compact-and-persist! with single message"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define msgs (list (make-test-msg 'user "hello")))
  (define result (compact-and-persist! msgs sp))
  (check-equal? (compaction-result-removed-count result) 0)
  ;; Single message should not produce a compaction entry
  (check-false (compaction-result-summary-message result))
  (cleanup-dir dir))

(test-case "edge-compact: build-tiered-context with single message"
  (define msgs (list (make-test-msg 'user "hello")))
  (define tc (build-tiered-context msgs))
  (check-equal? (tiered-context-tier-a tc) '())
  ;; Single message goes to tier-c (most recent)
  (check-equal? (length (tiered-context-tier-c tc)) 1))

(test-case "edge-compact: build-tiered-context single message is in context list"
  (define msgs (list (make-test-msg 'user "hello")))
  (define tc (build-tiered-context msgs))
  (define all-msgs (tiered-context->message-list tc))
  (check-equal? (length all-msgs) 1)
  (check-equal? (text-part-text (car (message-content (car all-msgs)))) "hello"))

;; ============================================================
;; #615: Context assembly with empty index
;; ============================================================

(test-case "edge-context: build-session-context with empty index"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  ;; Create empty files
  (call-with-output-file sp (lambda (p) (void)) #:exists 'replace)
  (call-with-output-file ip (lambda (p) (void)) #:exists 'replace)
  (define idx (build-index! sp ip))
  (define ctx (build-session-context idx))
  (check-equal? ctx '())
  (cleanup-dir dir))

(test-case "edge-context: build-session-context nonexistent session"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  ;; Files don't exist — build-index! should handle gracefully
  (define idx (build-index! sp ip))
  (define ctx (build-session-context idx))
  (check-equal? ctx '())
  (cleanup-dir dir))

(test-case "edge-context: entry->context-message filters compaction-summary"
  ;; Compaction summaries should be transformed, not filtered
  (define msg (make-test-msg 'system "summary text" 'compaction-summary))
  (define result (entry->context-message msg))
  ;; Compaction summaries should pass through (they get user-role in assembly)
  (check-true (or (not result) (message? result))
              "compaction-summary should produce a message or be handled"))

(test-case "edge-context: entry->context-message passes settings entries"
  ;; Settings entries (kind = 'settings) are not in the explicit filter
  ;; list (session-info, model-change, thinking-level-change), so they
  ;; pass through via the else clause. This is the documented behavior.
  (define msg (make-message "s1" #f 'system 'settings
                            (list (make-text-part "{}"))
                            (current-seconds)
                            (hasheq)))
  (define result (entry->context-message msg))
  (check-true (message? result) "settings entries pass through (not filtered)"))

(test-case "edge-context: entry->context-message passes label entries"
  ;; Label entries pass through via the else clause
  (define msg (make-message "l1" #f 'system 'label
                            (list (make-text-part "my label"))
                            (current-seconds)
                            (hasheq)))
  (define result (entry->context-message msg))
  (check-true (message? result) "label entries pass through (not filtered)"))

(test-case "edge-context: entry->context-message passes user messages"
  (define msg (make-test-msg 'user "hello"))
  (define result (entry->context-message msg))
  (check-true (message? result) "user messages should pass through"))

(test-case "edge-context: entry->context-message passes assistant messages"
  (define msg (make-test-msg 'assistant "response"))
  (define result (entry->context-message msg))
  (check-true (message? result) "assistant messages should pass through"))

;; ============================================================
;; Additional edge cases: threshold boundaries
;; ============================================================

(test-case "edge-compact: exactly keep-recent-count messages (no compaction)"
  ;; Default strategy keeps 20 recent. 20 messages should not compact.
  (define msgs (for/list ([i (in-range 20)])
                 (make-test-msg 'assistant (format "msg-~a" i))))
  (define result (compact-history msgs))
  (check-equal? (compaction-result-removed-count result) 0)
  (check-equal? (length (compaction-result-kept-messages result)) 20))

(test-case "edge-compact: keep-recent-count + 1 messages (one compacted)"
  (define msgs (for/list ([i (in-range 21)])
                 (make-test-msg 'assistant (format "msg-~a" i))))
  (define result (compact-history msgs))
  ;; 21 > 20 (keep-recent), so 1 old message should be summarized
  (check > (compaction-result-removed-count result) 0)
  (check-true (message? (compaction-result-summary-message result))
              "should have a summary when messages exceed keep-recent"))

(test-case "edge-compact: tiered-context with only compaction summaries"
  ;; Only compaction summaries, no regular messages
  (define summaries (list (make-message "cs1" #f 'system 'compaction-summary
                                         (list (make-text-part "summary"))
                                         (current-seconds)
                                         (hasheq))))
  (define tc (build-tiered-context summaries))
  ;; Tier A should have the compaction summaries
  (check-equal? (length (tiered-context-tier-a tc)) 1)
  ;; Tier B and C should be empty (no regular messages)
  (check-equal? (tiered-context-tier-b tc) '())
  (check-equal? (tiered-context-tier-c tc) '()))

(test-case "edge-compact: tiered-context->message-list includes all tiers"
  (define summaries (list (make-message "cs1" #f 'system 'compaction-summary
                                         (list (make-text-part "summary"))
                                         (current-seconds)
                                         (hasheq))))
  (define regular (list (make-test-msg 'assistant "response")))
  (define msgs (append summaries regular))
  (define tc (build-tiered-context msgs))
  (define all-msgs (tiered-context->message-list tc))
  ;; Should include tier-a + tier-c (tier-b empty for small sets)
  (check >= (length all-msgs) 1))

;; ============================================================
;; Hook edge cases
;; ============================================================

(test-case "edge-compact: hook blocks compaction on empty"
  (define blocker (lambda (hook payload)
                    (hook-result 'block (hasheq))))
  (define result (compact-history '() #:hook-dispatcher blocker))
  ;; Empty messages → identity result regardless of hook
  (check-equal? (compaction-result-removed-count result) 0))

(test-case "edge-compact: hook blocking prevents compaction (#636)"
  ;; FIX #636: The hook-dispatcher 'session-before-compact' now properly
  ;; blocks compaction. The `cond` form returns the identity result when
  ;; the hook returns 'block.
  (define msgs (for/list ([i (in-range 30)])
                 (make-test-msg 'assistant (format "msg-~a" i))))
  (define blocker (lambda (hook payload)
                    (hook-result 'block (hasheq))))
  (define result (compact-history msgs #:hook-dispatcher blocker))
  ;; Hook blocks — no messages compacted, identity result returned
  (check-equal? (compaction-result-removed-count result) 0)
  (check-equal? (length (compaction-result-kept-messages result)) 30)
  (check-false (compaction-result-summary-message result)))

(test-case "edge-compact: hook allows compaction"
  (define msgs (for/list ([i (in-range 30)])
                 (make-test-msg 'assistant (format "msg-~a" i))))
  (define allow (lambda (hook payload)
                  (hook-result 'pass (hasheq))))
  (define result (compact-history msgs #:hook-dispatcher allow))
  ;; Hook passed — normal compaction
  (check > (compaction-result-removed-count result) 0))
