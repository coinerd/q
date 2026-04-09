#lang racket

;;; tests/test-compactor.rkt — tests for runtime/compactor.rkt
;;;
;;; TDD test suite for the context compactor module.
;;; Covers:
;;;   - build-summary-window splitting logic
;;;   - compact-history producing shorter message lists
;;;   - Custom summarize function injection
;;;   - Summary message containing information from summarized messages
;;;   - Original messages never modified or deleted
;;;   - Compaction entry written to session store
;;;   - Edge cases: empty, too few messages, all kept

(require rackunit
         rackunit/text-ui
         racket/file
         "../agent/types.rkt"
         "../runtime/compactor.rkt"
         "../runtime/session-store.rkt"
         ;; R2-6: Import hooks for context-assembly testing
         "../extensions/hooks.rkt")

;; ── Helpers ──

(define (make-temp-dir)
  (make-temporary-file "q-compactor-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (make-test-message id role text)
  (make-message id #f role 'message
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define (msg-text msg)
  (define content (message-content msg))
  (if (and (pair? content) (text-part? (car content)))
      (text-part-text (car content))
      ""))

(define (msg-role msg)
  (message-role msg))

(define (msg-id msg)
  (message-id msg))

;; Build a list of N simple messages for testing
(define (make-n-messages n)
  (for/list ([i (in-range n)])
    (make-test-message (format "msg~a" i) 'user (format "Message ~a content" i))))

;; Simple test summarize function that concatenates texts
(define (test-summarize messages)
  (define texts (map msg-text messages))
  (string-join texts " | "))

;; ── Test suite ──

(define-test-suite test-compactor

  ;; ══════════════════════════════════════════════════════════════
  ;; build-summary-window
  ;; ══════════════════════════════════════════════════════════════

  (test-case "build-summary-window splits into old and recent"
    (define msgs (make-n-messages 10))
    (define strategy (compaction-strategy 6 4)) ; summarize up to 6, keep last 4
    (define-values (old recent) (build-summary-window msgs strategy))
    ;; 10 messages, keep-recent-count=4 → recent = last 4, old = first 6
    (check-equal? (length old) 6)
    (check-equal? (length recent) 4)
    (check-equal? (msg-id (first old)) "msg0")
    (check-equal? (msg-id (last old)) "msg5")
    (check-equal? (msg-id (first recent)) "msg6")
    (check-equal? (msg-id (last recent)) "msg9"))

  (test-case "build-summary-window keeps all messages when fewer than keep-recent"
    (define msgs (make-n-messages 3))
    (define strategy (compaction-strategy 50 20))
    (define-values (old recent) (build-summary-window msgs strategy))
    ;; 3 messages, keep-recent-count=20 → all are "recent", old is empty
    (check-equal? (length old) 0)
    (check-equal? (length recent) 3))

  (test-case "build-summary-window with empty messages returns both empty"
    (define msgs '())
    (define strategy (compaction-strategy 50 20))
    (define-values (old recent) (build-summary-window msgs strategy))
    (check-equal? old '())
    (check-equal? recent '()))

  (test-case "build-summary-window respects summary-window-size cap"
    ;; summary-window-size caps how many old messages to consider summarizing
    (define msgs (make-n-messages 30))
    (define strategy (compaction-strategy 10 5)) ; cap summarize to 10, keep 5
    (define-values (old recent) (build-summary-window msgs strategy))
    ;; 30 total, keep-recent=5 → recent=last 5, old=first 25
    ;; BUT summary-window-size=10 → only 10 from old are the summarize window
    ;; The remaining 15 in the middle are also kept (they fall through)
    ;; Wait — re-read the design: old = messages[0..total-keep], recent = messages[total-keep..end]
    ;; Then old is further trimmed to summary-window-size
    ;; Actually the simplest interpretation: split at total-keep-recent-count,
    ;; old = first (total - keep-recent-count), but capped at summary-window-size
    (check-equal? (length recent) 5)
    (check-equal? (length old) 10))

  (test-case "build-summary-window with equal messages to keep-recent"
    (define msgs (make-n-messages 20))
    (define strategy (compaction-strategy 50 20))
    (define-values (old recent) (build-summary-window msgs strategy))
    (check-equal? (length old) 0)
    (check-equal? (length recent) 20))

  ;; ══════════════════════════════════════════════════════════════
  ;; compact-history — core functionality
  ;; ══════════════════════════════════════════════════════════════

  (test-case "compact-history reduces message count"
    (define msgs (make-n-messages 30))
    (define strategy (compaction-strategy 20 5))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    ;; Should have: 1 summary + 5 recent = 6
    (check-equal? (length (compaction-result-kept-messages result)) 5)
    (check-true (< (+ 1 (length (compaction-result-kept-messages result))) ; summary + kept
                    (length msgs)))
    (check-equal? (compaction-result-removed-count result) 20))

  (test-case "compact-history summary message is a system message"
    (define msgs (make-n-messages 10))
    (define strategy (compaction-strategy 8 2))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (define summary (compaction-result-summary-message result))
    (check-equal? (message-role summary) 'system)
    (check-equal? (message-kind summary) 'compaction-summary))

  (test-case "compact-history summary contains information from summarized messages"
    (define msgs (make-n-messages 10))
    (define strategy (compaction-strategy 8 2))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (define summary (compaction-result-summary-message result))
    (define summary-text (msg-text summary))
    ;; The summary should contain text from the first 8 messages
    (check-true (string-contains? summary-text "Message 0"))
    (check-true (string-contains? summary-text "Message 7"))
    ;; The summary should NOT contain text from the kept messages
    (check-false (string-contains? summary-text "Message 8"))
    (check-false (string-contains? summary-text "Message 9")))

  (test-case "compact-history preserves recent messages verbatim"
    (define msgs (make-n-messages 10))
    (define strategy (compaction-strategy 8 2))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (define kept (compaction-result-kept-messages result))
    (check-equal? (length kept) 2)
    (check-equal? (msg-id (first kept)) "msg8")
    (check-equal? (msg-id (second kept)) "msg9")
    (check-equal? (msg-text (first kept)) "Message 8 content")
    (check-equal? (msg-text (second kept)) "Message 9 content"))

  (test-case "compact-history does not modify original messages"
    (define msgs (make-n-messages 10))
    (define msgs-before (map msg-id msgs))
    (define strategy (compaction-strategy 8 2))
    (compact-history msgs
                     #:strategy strategy
                     #:summarize-fn test-summarize)
    ;; Original list unchanged
    (define msgs-after (map msg-id msgs))
    (check-equal? msgs-before msgs-after)
    (check-equal? (length msgs) 10))

  (test-case "compact-history with too few messages returns identity"
    ;; If total messages <= keep-recent-count, nothing to compact
    (define msgs (make-n-messages 5))
    (define strategy (compaction-strategy 50 20))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    ;; No compaction happens — all messages are "recent"
    (check-equal? (compaction-result-removed-count result) 0)
    (check-equal? (length (compaction-result-kept-messages result)) 5)
    ;; Summary message is #f when nothing was summarized
    (check-false (compaction-result-summary-message result)))

  (test-case "compact-history with empty messages returns empty result"
    (define result (compact-history '()
                                     #:strategy (compaction-strategy 50 20)
                                     #:summarize-fn test-summarize))
    (check-equal? (compaction-result-removed-count result) 0)
    (check-equal? (compaction-result-kept-messages result) '())
    (check-false (compaction-result-summary-message result)))

  (test-case "compact-history produces valid shorter message list"
    (define msgs (make-n-messages 50))
    (define strategy (compaction-strategy 30 10))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (define compacted-list (compaction-result->message-list result))
    ;; 1 summary + 10 recent = 11 total
    (check-equal? (length compacted-list) 11)
    ;; First element should be the summary
    (check-equal? (message-kind (first compacted-list)) 'compaction-summary)
    ;; Remaining should be the kept messages
    (check-equal? (msg-id (second compacted-list)) "msg40")
    (check-equal? (msg-id (last compacted-list)) "msg49"))

  ;; ══════════════════════════════════════════════════════════════
  ;; Custom summarize function
  ;; ══════════════════════════════════════════════════════════════

  (test-case "custom summarize function is used"
    (define msgs (make-n-messages 10))
    (define strategy (compaction-strategy 8 2))
    (define (custom-summarize ms)
      (format "CUSTOM: ~a messages summarized" (length ms)))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn custom-summarize))
    (define summary (compaction-result-summary-message result))
    (check-true (string-contains? (msg-text summary) "CUSTOM: 8 messages")))

  ;; ══════════════════════════════════════════════════════════════
  ;; compaction-result->message-list
  ;; ══════════════════════════════════════════════════════════════

  (test-case "compaction-result->message-list returns summary + kept"
    (define msgs (make-n-messages 15))
    (define strategy (compaction-strategy 10 5))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (define merged (compaction-result->message-list result))
    (check-equal? (length merged) 6)  ; 1 summary + 5 kept
    (check-equal? (message-kind (first merged)) 'compaction-summary)
    (for ([m (in-list (drop merged 1))])
      (check-equal? (message-kind m) 'message)))

  (test-case "compaction-result->message-list with no compaction returns kept only"
    (define msgs (make-n-messages 5))
    (define strategy (compaction-strategy 50 20))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (define merged (compaction-result->message-list result))
    (check-equal? (length merged) 5)
    (check-false (findf (lambda (m) (eq? (message-kind m) 'compaction-summary)) merged)))

  ;; ══════════════════════════════════════════════════════════════
  ;; write-compaction-entry!
  ;; ══════════════════════════════════════════════════════════════

  (test-case "write-compaction-entry! writes to session log"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Pre-populate session log
    (for ([i (in-range 10)])
      (append-entry! path (make-test-message (format "orig~a" i) 'user
                                              (format "Original ~a" i))))
    ;; Run compaction
    (define msgs (load-session-log path))
    (define strategy (compaction-strategy 6 4))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    ;; Write compaction entry
    (write-compaction-entry! path result)
    ;; The session log should now have original 10 + compaction entry = 11
    (define all-entries (load-session-log path))
    (check-equal? (length all-entries) 11)
    ;; The last entry should be the compaction-summary
    (define last-entry (last all-entries))
    (check-equal? (message-kind last-entry) 'compaction-summary)
    (check-equal? (message-role last-entry) 'system)
    (delete-directory/files dir #:must-exist? #f))

  (test-case "write-compaction-entry! preserves original history"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define original-msgs (for/list ([i (in-range 10)])
                            (make-test-message (format "orig~a" i) 'user
                                               (format "Original ~a" i))))
    (append-entries! path original-msgs)
    ;; Compact
    (define msgs (load-session-log path))
    (define strategy (compaction-strategy 6 4))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (write-compaction-entry! path result)
    ;; Reload — original entries must all still be there
    (define all-entries (load-session-log path))
    (check-equal? (length all-entries) 11)
    (for ([i (in-range 10)])
      (check-equal? (message-id (list-ref all-entries i))
                    (format "orig~a" i)))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "write-compaction-entry! records removed-count in meta"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path (make-n-messages 10))
    (define msgs (load-session-log path))
    (define strategy (compaction-strategy 6 4))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (write-compaction-entry! path result)
    (define all-entries (load-session-log path))
    (define compaction-entry (last all-entries))
    (check-equal? (hash-ref (message-meta compaction-entry) 'removedCount) 6)
    (check-equal? (hash-ref (message-meta compaction-entry) 'type) "compaction")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "write-compaction-entry! with no compaction does nothing"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path (make-n-messages 3))
    (define msgs (load-session-log path))
    (define strategy (compaction-strategy 50 20))
    (define result (compact-history msgs
                                     #:strategy strategy
                                     #:summarize-fn test-summarize))
    (write-compaction-entry! path result)
    ;; No compaction happened — log should still have 3 entries
    (define all-entries (load-session-log path))
    (check-equal? (length all-entries) 3)
    (delete-directory/files dir #:must-exist? #f))

  ;; ══════════════════════════════════════════════════════════════
  ;; default-strategy
  ;; ══════════════════════════════════════════════════════════════

  (test-case "default-strategy returns sensible defaults"
    (define s (default-strategy))
    (check-true (compaction-strategy? s))
    (check-true (positive? (compaction-strategy-summary-window-size s)))
    (check-true (positive? (compaction-strategy-keep-recent-count s))))

  ;; ══════════════════════════════════════════════════════════════
  ;; default-summarize
  ;; ══════════════════════════════════════════════════════════════

  (test-case "default-summarize produces structured text"
    (define msgs (make-n-messages 5))
    (define summary-text (default-summarize msgs))
    (check-true (string? summary-text))
    (check-true (> (string-length summary-text) 0)))

  ;; ══════════════════════════════════════════════════════════════
  ;; compact-and-persist!
  ;; ══════════════════════════════════════════════════════════════

  (test-case "compact-and-persist! writes summary entry to session log"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Pre-populate with messages
    (define msgs (make-n-messages 30))
    (for ([m (in-list msgs)])
      (append-entry! path m))
    (define loaded (load-session-log path))
    (check-equal? (length loaded) 30)
    ;; Compact and persist
    (define strategy (compaction-strategy 20 5))
    (define result (compact-and-persist! loaded path
                                          #:strategy strategy
                                          #:summarize-fn test-summarize))
    ;; Log should now have 30 originals + 1 compaction summary = 31
    (define all-entries (load-session-log path))
    (check-equal? (length all-entries) 31)
    ;; Last entry is the compaction summary
    (define last-entry (last all-entries))
    (check-equal? (message-kind last-entry) 'compaction-summary)
    (check-equal? (message-role last-entry) 'system)
    ;; Original messages are still intact
    (for ([i (in-range 30)])
      (check-equal? (message-id (list-ref all-entries i))
                    (format "msg~a" i)))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "compact-and-persist! returns compaction-result"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-n-messages 30))
    (for ([m (in-list msgs)])
      (append-entry! path m))
    (define loaded (load-session-log path))
    (define strategy (compaction-strategy 20 5))
    (define result (compact-and-persist! loaded path
                                          #:strategy strategy
                                          #:summarize-fn test-summarize))
    (check-true (compaction-result? result))
    (check-equal? (compaction-result-removed-count result) 20)
    (check-equal? (length (compaction-result-kept-messages result)) 5)
    (check-true (message? (compaction-result-summary-message result)))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "compact-and-persist! is no-op when nothing to compact"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Few messages — less than keep-recent-count
    (define msgs (make-n-messages 3))
    (for ([m (in-list msgs)])
      (append-entry! path m))
    (define loaded (load-session-log path))
    (define strategy (compaction-strategy 50 20))
    (define result (compact-and-persist! loaded path
                                          #:strategy strategy
                                          #:summarize-fn test-summarize))
    ;; removed-count is 0, no summary entry written
    (check-equal? (compaction-result-removed-count result) 0)
    (check-false (compaction-result-summary-message result))
    ;; Log should still have exactly 3 entries — no summary appended
    (define all-entries (load-session-log path))
    (check-equal? (length all-entries) 3)
    (delete-directory/files dir #:must-exist? #f))

  ;; ══════════════════════════════════════════════════════════════
  ;; compact-history-advisory
  ;; ══════════════════════════════════════════════════════════════

  (test-case "compact-history-advisory is advisory-only (alias for compact-history)"
    (define msgs (make-n-messages 30))
    (define strategy (compaction-strategy 20 5))
    (define r1 (compact-history msgs
                                #:strategy strategy
                                #:summarize-fn test-summarize))
    (define r2 (compact-history-advisory msgs
                                          #:strategy strategy
                                          #:summarize-fn test-summarize))
    ;; Both return the same removed-count and kept-count
    (check-equal? (compaction-result-removed-count r1)
                  (compaction-result-removed-count r2))
    (check-equal? (length (compaction-result-kept-messages r1))
                  (length (compaction-result-kept-messages r2)))
    ;; Both produce a summary message
    (check-true (message? (compaction-result-summary-message r1)))
    (check-true (message? (compaction-result-summary-message r2)))
    ;; Summary texts are identical
    (check-equal? (msg-text (compaction-result-summary-message r1))
                  (msg-text (compaction-result-summary-message r2))))

  (test-case "compact-history contract: does not modify session log"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Write 10 messages to the log
    (define msgs (make-n-messages 10))
    (for ([m (in-list msgs)])
      (append-entry! path m))
    (define entries-before (load-session-log path))
    (check-equal? (length entries-before) 10)
    ;; Run advisory compaction
    (define loaded (load-session-log path))
    (define strategy (compaction-strategy 6 4))
    (compact-history loaded
                     #:strategy strategy
                     #:summarize-fn test-summarize)
    ;; Verify log is unchanged
    (define entries-after (load-session-log path))
    (check-equal? (length entries-after) 10)
    (for ([i (in-range 10)])
      (check-equal? (message-id (list-ref entries-after i))
                    (format "msg~a" i)))
    (delete-directory/files dir #:must-exist? #f))

  ;; ══════════════════════════════════════════════════════════════
  ;; R2-6: Context Assembly Hooks
  ;; ══════════════════════════════════════════════════════════════

  (test-case "build-tiered-context-with-hooks: no dispatcher returns base context"
    ;; When no hook-dispatcher is provided, should return base tiered context
    (define msgs (make-n-messages 15))
    (define-values (tc hook-result) 
      (build-tiered-context-with-hooks msgs
                                       #:hook-dispatcher #f
                                       #:tier-b-count 5
                                       #:tier-c-count 3))
    ;; Should return valid tiered-context
    (check-true (tiered-context? tc))
    ;; Tier C should have 3 messages
    (check-equal? (length (tiered-context-tier-c tc)) 3)
    ;; Tier B should have 5 messages
    (check-equal? (length (tiered-context-tier-b tc)) 5)
    ;; Tier A should be empty (no compaction summaries)
    (check-equal? (length (tiered-context-tier-a tc)) 0)
    ;; No hook result when no dispatcher
    (check-false hook-result))

  (test-case "build-tiered-context-with-hooks: hook returns 'pass"
    ;; Hook that returns 'pass should not modify context
    (define msgs (make-n-messages 15))
    (define pass-hook
      (lambda (hook-point payload)
        (check-equal? hook-point 'context-assembly)
        (check-true (context-assembly-payload? payload))
        (hook-pass payload)))
    
    (define-values (tc hook-result)
      (build-tiered-context-with-hooks msgs
                                       #:hook-dispatcher pass-hook
                                       #:tier-b-count 5
                                       #:tier-c-count 3
                                       #:max-tokens 4096))
    
    ;; Should return valid tiered-context
    (check-true (tiered-context? tc))
    ;; Hook result should indicate pass
    (check-true (hook-result? hook-result))
    (check-equal? (hook-result-action hook-result) 'pass)
    ;; Tiers should be unchanged
    (check-equal? (length (tiered-context-tier-c tc)) 3)
    (check-equal? (length (tiered-context-tier-b tc)) 5))

  (test-case "build-tiered-context-with-hooks: hook returns 'amend"
    ;; Hook that modifies tier composition via amend
    (define msgs (make-n-messages 20))
    (define amend-hook
      (lambda (hook-point payload)
        (check-equal? hook-point 'context-assembly)
        ;; Create modified payload with swapped tiers
        (define original-tc (payload->tiered-context payload))
        (define modified-payload
          (context-assembly-payload
           (context-assembly-payload-tier-a-messages payload)
           ;; Move one message from tier-c to tier-b
           (append (tiered-context-tier-b original-tc)
                   (list (first (tiered-context-tier-c original-tc))))
           ;; Remove first from tier-c
           (rest (tiered-context-tier-c original-tc))
           (context-assembly-payload-max-tokens payload)
           (context-assembly-payload-metadata payload)))
        (hook-amend modified-payload)))
    
    (define-values (tc hook-result)
      (build-tiered-context-with-hooks msgs
                                       #:hook-dispatcher amend-hook
                                       #:tier-b-count 5
                                       #:tier-c-count 4
                                       #:max-tokens 8192))
    
    ;; Should return amended tiered-context
    (check-true (tiered-context? tc))
    ;; Hook result should indicate amend
    (check-true (hook-result? hook-result))
    (check-equal? (hook-result-action hook-result) 'amend)
    ;; Tier B should now have 6 messages (5 original + 1 moved from C)
    (check-equal? (length (tiered-context-tier-b tc)) 6)
    ;; Tier C should now have 3 messages (4 original - 1 moved to B)
    (check-equal? (length (tiered-context-tier-c tc)) 3))

  (test-case "build-tiered-context-with-hooks: hook returns 'block raises error"
    ;; Hook that blocks context assembly
    (define msgs (make-n-messages 10))
    (define block-hook
      (lambda (hook-point payload)
        (check-equal? hook-point 'context-assembly)
        (hook-block "Context assembly blocked for testing")))
    
    ;; Should raise exn:fail when hook blocks
    (check-exn
     exn:fail?
     (lambda ()
       (build-tiered-context-with-hooks msgs
                                        #:hook-dispatcher block-hook
                                        #:tier-b-count 5
                                        #:tier-c-count 3)))
    
    ;; Verify the error message contains the block reason
    (check-exn
     (lambda (e)
       (and (exn:fail? e)
            (string-contains? (exn-message e) "blocked")))
     (lambda ()
       (build-tiered-context-with-hooks msgs
                                        #:hook-dispatcher block-hook
                                        #:tier-b-count 5
                                        #:tier-c-count 3))))

  (test-case "context-assembly-payload is serializable"
    ;; Payload should be serializable for logging
    (define msgs (make-n-messages 10))
    (define tc (build-tiered-context msgs #:tier-b-count 5 #:tier-c-count 3))
    (define payload (tiered-context->payload tc 8192 (hasheq 'test-key "test-value")))
    
    ;; Verify payload struct fields
    (check-true (context-assembly-payload? payload))
    (check-equal? (context-assembly-payload-max-tokens payload) 8192)
    (check-equal? (hash-ref (context-assembly-payload-metadata payload) 'test-key) "test-value")
    
    ;; Verify payload can be converted back to tiered-context
    (define tc-from-payload (payload->tiered-context payload))
    (check-true (tiered-context? tc-from-payload))
    (check-equal? (length (tiered-context-tier-a tc-from-payload))
                  (length (tiered-context-tier-a tc)))
    (check-equal? (length (tiered-context-tier-b tc-from-payload))
                  (length (tiered-context-tier-b tc)))
    (check-equal? (length (tiered-context-tier-c tc-from-payload))
                  (length (tiered-context-tier-c tc))))

  (test-case "build-tiered-context-with-hooks: payload contains correct tier messages"
    ;; Verify payload contains the actual tier message lists
    (define msgs (make-n-messages 12))
    (define received-payload #f)
    
    (define capture-hook
      (lambda (hook-point payload)
        (set! received-payload payload)
        (hook-pass payload)))
    
    (define-values (tc hook-result)
      (build-tiered-context-with-hooks msgs
                                       #:hook-dispatcher capture-hook
                                       #:tier-b-count 5
                                       #:tier-c-count 3
                                       #:max-tokens 4096))
    
    ;; Verify payload was received and contains correct data
    (check-true (context-assembly-payload? received-payload))
    ;; Tier C should have 3 messages (the most recent)
    (check-equal? (length (context-assembly-payload-tier-c-messages received-payload)) 3)
    ;; Check that tier-c contains the most recent messages
    (define tier-c-ids (map message-id (context-assembly-payload-tier-c-messages received-payload)))
    (check-equal? tier-c-ids '("msg9" "msg10" "msg11"))
    ;; Tier B should have 5 messages
    (check-equal? (length (context-assembly-payload-tier-b-messages received-payload)) 5)
    ;; Check that tier-b contains messages before tier-c
    (define tier-b-ids (map message-id (context-assembly-payload-tier-b-messages received-payload)))
    (check-equal? tier-b-ids '("msg4" "msg5" "msg6" "msg7" "msg8")))

  (test-case "build-tiered-context-with-hooks: multiple hooks chain correctly"
    ;; Simulate multiple extensions by having hook that tracks call count
    (define call-count 0)
    (define first-hook-amends #t)
    
    (define chained-hook
      (lambda (hook-point payload)
        (set! call-count (add1 call-count))
        (cond
          ;; First call: amend the payload
          [first-hook-amends
           (set! first-hook-amends #f)
           (define tc (payload->tiered-context payload))
           (define modified-payload
             (context-assembly-payload
              (context-assembly-payload-tier-a-messages payload)
              (tiered-context-tier-b tc)
              ;; Add a marker to tier-c metadata
              (tiered-context-tier-c tc)
              (context-assembly-payload-max-tokens payload)
              (hash-set (context-assembly-payload-metadata payload) 'amended #t)))
           (hook-amend modified-payload)]
          ;; Second call: pass
          [else
           (hook-pass payload)])))
    
    (define msgs (make-n-messages 10))
    (define-values (tc hook-result)
      (build-tiered-context-with-hooks msgs
                                       #:hook-dispatcher chained-hook
                                       #:tier-b-count 4
                                       #:tier-c-count 3))
    
    ;; Hook should have been called once (single hook simulating chain)
    (check-equal? call-count 1)
    ;; Result should be amend
    (check-equal? (hook-result-action hook-result) 'amend))

  (test-case "tiered-context->message-list after hook amendment"
    ;; Verify that tiered-context->message-list works correctly on amended context
    (define msgs (make-n-messages 10))
    (define amend-hook
      (lambda (hook-point payload)
        ;; Swap tier B and C for testing
        (define original-tc (payload->tiered-context payload))
        (define modified-payload
          (context-assembly-payload
           (context-assembly-payload-tier-a-messages payload)
           (tiered-context-tier-c original-tc)  ; swap
           (tiered-context-tier-b original-tc)  ; swap
           (context-assembly-payload-max-tokens payload)
           (context-assembly-payload-metadata payload)))
        (hook-amend modified-payload)))
    
    (define-values (tc hook-result)
      (build-tiered-context-with-hooks msgs
                                       #:hook-dispatcher amend-hook
                                       #:tier-b-count 4
                                       #:tier-c-count 3))
    
    ;; Convert to message list
    (define msg-list (tiered-context->message-list tc))
    
    ;; Order should be: Tier A (empty) -> Tier B (was 3 msgs) -> Tier C (was 4 msgs)
    (check-equal? (length msg-list) 7)
    ;; First 3 should be what was originally tier-c
    (check-equal? (message-id (first msg-list)) "msg7")
    (check-equal? (message-id (third msg-list)) "msg9")
    ;; Next 4 should be what was originally tier-b
    (check-equal? (message-id (fourth msg-list)) "msg3")
    (check-equal? (message-id (seventh msg-list)) "msg6"))
  )

;; Run
(run-tests test-compactor)
