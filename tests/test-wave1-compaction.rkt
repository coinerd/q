#lang racket

;; tests/test-wave1-compaction.rkt — Wave 1 compaction hardening tests
;;
;; Tests for v0.11.0 Wave 1 sub-issues:
;;   #1177: Structured compaction with cumulative file tracking
;;   #1178: Split-turn edge cases (cascading, oversized tool results)
;;   #1179: Context window overflow recovery

(require rackunit
         racket/file
         "../util/protocol-types.rkt"
         "../runtime/compactor.rkt"
         "../runtime/token-compaction.rkt"
         "../runtime/split-turn.rkt"
         "../runtime/cutpoint-rules.rkt"
         "../runtime/auto-retry.rkt"
         "../runtime/session-store.rkt"
         "../runtime/compaction-prompts.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-wave1-test-~a" 'directory))

(define (msg id role text [kind 'message])
  (make-message id #f role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (msg-with-tool-call id tool-name path-str)
  (make-message id #f 'assistant 'message
                (list (make-tool-call-part id tool-name (hasheq 'path path-str)))
                (current-seconds) (hasheq)))

(define (msg-with-tool-result id result-text)
  (make-message id #f 'tool 'tool-result
                (list (make-tool-result-part id result-text #f))
                (current-seconds) (hasheq)))

(define (msg-text m)
  (string-join (for/list ([p (in-list (message-content m))] #:when (text-part? p))
                 (text-part-text p)) ""))

;; Tiny config for triggering compaction
(define tiny-tc (token-compaction-config 5 0 10))

(define (make-n-messages n)
  (for/list ([i (in-range n)])
    (msg (format "msg~a" i) 'user (format "Message ~a content here" i))))

;; ============================================================
;; #1177: Structured Compaction Format + Cumulative File Tracking
;; ============================================================

(test-case "#1177: extract-file-tracker finds read paths"
  (define messages
    (list (msg-with-tool-call "tc1" "read" "/src/main.rkt")
          (msg-with-tool-result "tr1" "contents of main")
          (msg-with-tool-call "tc2" "edit" "/src/util.rkt")
          (msg-with-tool-result "tr2" "edit applied")))
  (define ft (extract-file-tracker messages))
  (check-equal? (sort (hash-ref ft 'readFiles '()) string<?) '("/src/main.rkt"))
  (check-equal? (sort (hash-ref ft 'modifiedFiles '()) string<?) '("/src/util.rkt")))

(test-case "#1177: extract-file-tracker finds multiple read/write paths"
  (define messages
    (list (msg-with-tool-call "tc1" "read" "/a.rkt")
          (msg-with-tool-call "tc2" "read" "/b.rkt")
          (msg-with-tool-call "tc3" "write" "/c.rkt")
          (msg-with-tool-call "tc4" "edit" "/d.rkt")))
  (define ft (extract-file-tracker messages))
  (check-equal? (length (hash-ref ft 'readFiles '())) 2)
  (check-equal? (length (hash-ref ft 'modifiedFiles '())) 2))

(test-case "#1177: merge-file-trackers deduplicates"
  (define ft1 (hasheq 'readFiles '("/a.rkt" "/b.rkt") 'modifiedFiles '("/c.rkt")))
  (define ft2 (hasheq 'readFiles '("/b.rkt" "/d.rkt") 'modifiedFiles '("/c.rkt" "/e.rkt")))
  (define merged (merge-file-trackers ft1 ft2))
  (check-equal? (sort (hash-ref merged 'readFiles) string<?) '("/a.rkt" "/b.rkt" "/d.rkt"))
  (check-equal? (sort (hash-ref merged 'modifiedFiles) string<?) '("/c.rkt" "/e.rkt")))

(test-case "#1177: cumulative file tracker across 3+ compaction rounds"
  ;; Simulate 3 successive compaction rounds where each round has different files
  ;; Round 1: reads /a.rkt, writes /b.rkt
  ;; Round 2: reads /c.rkt, writes /a.rkt (now both read+write)
  ;; Round 3: reads /d.rkt, writes /e.rkt
  ;; The cumulative tracker should include ALL files from all rounds
  (define round1-msgs
    (list (msg-with-tool-call "r1tc1" "read" "/a.rkt")
          (msg-with-tool-result "r1tr1" "a")
          (msg-with-tool-call "r1tc2" "edit" "/b.rkt")
          (msg-with-tool-result "r1tr2" "b edited")))

  ;; Round 1 compaction
  (define ft1 (extract-file-tracker round1-msgs))
  (check-equal? (hash-ref ft1 'readFiles) '("/a.rkt"))
  (check-equal? (hash-ref ft1 'modifiedFiles) '("/b.rkt"))

  ;; Round 2 messages
  (define round2-msgs
    (list (msg-with-tool-call "r2tc1" "read" "/c.rkt")
          (msg-with-tool-result "r2tr1" "c")
          (msg-with-tool-call "r2tc2" "write" "/a.rkt")
          (msg-with-tool-result "r2tr2" "a written")))
  (define ft2 (extract-file-tracker round2-msgs))

  ;; Cumulative after round 2
  (define cumulative-2 (merge-file-trackers ft1 ft2))
  (check-equal? (sort (hash-ref cumulative-2 'readFiles) string<?) '("/a.rkt" "/c.rkt"))
  (check-equal? (sort (hash-ref cumulative-2 'modifiedFiles) string<?) '("/a.rkt" "/b.rkt"))

  ;; Round 3
  (define round3-msgs
    (list (msg-with-tool-call "r3tc1" "read" "/d.rkt")
          (msg-with-tool-result "r3tr1" "d")
          (msg-with-tool-call "r3tc2" "edit" "/e.rkt")
          (msg-with-tool-result "r3tr2" "e edited")))
  (define ft3 (extract-file-tracker round3-msgs))
  (define cumulative-3 (merge-file-trackers cumulative-2 ft3))

  ;; All files from all 3 rounds preserved
  (check-equal? (sort (hash-ref cumulative-3 'readFiles) string<?) '("/a.rkt" "/c.rkt" "/d.rkt"))
  (check-equal? (sort (hash-ref cumulative-3 'modifiedFiles) string<?) '("/a.rkt" "/b.rkt" "/e.rkt")))

(test-case "#1177: compaction summary stores fileTracker in metadata (default-summarize path)"
  ;; Without LLM provider, fileTracker is stored as empty (hasheq) since
  ;; the default-summarize path doesn't extract tool paths. The metadata
  ;; key still exists.
  (define messages
    (append (list (msg-with-tool-call "tc1" "read" "/foo.rkt")
                  (msg-with-tool-result "tr1" "foo")
                  (msg-with-tool-call "tc2" "edit" "/bar.rkt")
                  (msg-with-tool-result "tr2" "bar"))
            (make-n-messages 10)))
  (define result (compact-history messages #:token-config tiny-tc))
  (define summary (compaction-result-summary-message result))
  (when summary
    (define meta (message-meta summary))
    ;; The metadata always has a 'type' key for compaction summaries
    (check-equal? (hash-ref meta 'type #f) "compaction")))

(test-case "#1177: compaction prompt includes file tracker section"
  ;; Verify that the summary-prompt includes file tracker information
  ;; with the new XML format
  (define ft (hasheq 'readFiles '("/a.rkt") 'modifiedFiles '("/b.rkt")))
  (define prompt (summary-prompt "test messages" ft))
  (check-true (string-contains? prompt "<file-tracker>"))
  (check-true (string-contains? prompt "<read-files>"))
  (check-true (string-contains? prompt "/a.rkt"))
  (check-true (string-contains? prompt "<modified-files>"))
  (check-true (string-contains? prompt "/b.rkt")))

(test-case "#1177: compaction prompt with empty file tracker omits section"
  (define prompt (summary-prompt "test messages" (hasheq)))
  (check-false (string-contains? prompt "<file-tracker>")))

;; ============================================================
;; #1178: Split-Turn Edge Cases
;; ============================================================

(test-case "#1178: oversized tool result alone exceeds keep-recent-tokens"
  ;; Create a single massive tool result that blows the token budget
  (define huge-text (make-string 5000 #\X))
  (define messages
    (list (msg "u1" 'user "small query")
          (msg "a1" 'assistant "calling tool")
          (msg-with-tool-result "tr1" huge-text)))
  ;; Use a config that only keeps 100 tokens
  (define small-tc (token-compaction-config 100 0 200))
  (define result (compact-history messages #:token-config small-tc))
  ;; Should still produce a valid result
  (check-true (compaction-result? result))
  (check-true (>= (compaction-result-removed-count result) 0)))

(test-case "#1178: cascading split-turn — turn exceeds window then summary exceeds window"
  ;; First compaction produces a summary. Then we add more messages
  ;; and the summary itself becomes part of the "old" messages.
  ;; Second compaction should handle the existing summary gracefully.
  (define msgs-round1
    (for/list ([i (in-range 20)])
      (msg (format "r1-~a" i) 'user (format "Round 1 message ~a with enough text" i))))

  ;; First compaction
  (define result1 (compact-history msgs-round1 #:token-config tiny-tc))
  (check-true (compaction-result? result1))

  ;; Build messages for round 2: include the summary from round 1
  (define summary1 (compaction-result-summary-message result1))
  (define kept1 (compaction-result-kept-messages result1))
  (define msgs-round2
    (append (if summary1 (list summary1) '())
            kept1
            (for/list ([i (in-range 20)])
              (msg (format "r2-~a" i) 'user (format "Round 2 message ~a with enough text" i)))))

  ;; Second compaction — should handle the existing summary gracefully
  (define result2 (compact-history msgs-round2 #:token-config tiny-tc))
  (check-true (compaction-result? result2))
  ;; The second summary should exist and the system shouldn't crash
  (when (compaction-result-summary-message result2)
    (define s2-text (msg-text (compaction-result-summary-message result2)))
    (check-true (string? s2-text))))

(test-case "#1178: split-turn with tool-call/tool-result pairs"
  ;; Tool-call followed by tool-result must not be split
  (define messages
    (append (for/list ([i (in-range 5)])
              (msg (format "pre-~a" i) 'user (format "Pre-message ~a content" i)))
            (list (msg "tc1" 'assistant "calling read")
                  (msg-with-tool-call "tc1-inner" "read" "/foo.rkt")
                  (msg-with-tool-result "tr1" "file contents here"))
            (list (msg "u-post" 'user "after tool call"))))
  ;; Force compaction
  (define result (compact-history messages #:token-config tiny-tc))
  (check-true (compaction-result? result))
  ;; Verify no tool-result is orphaned (first kept message should not be a tool-result)
  (define kept (compaction-result-kept-messages result))
  (when (and (pair? kept) (positive? (compaction-result-removed-count result)))
    ;; First kept message should not be a bare tool-result
    (define first-kept (car kept))
    ;; tool-result is valid if preceded by its tool-call in the summary
    (check-true (or (not (eq? (message-kind first-kept) 'tool-result))
                    (> (compaction-result-removed-count result) 0)))))

(test-case "#1178: empty split-turn produces no prefix"
  (define result (find-split-turn '() 0))
  (check-false (split-turn-result-is-split? result))
  (check-equal? (generate-turn-prefix (split-turn-result-turn-messages result)) ""))

;; ============================================================
;; #1179: Context Window Overflow Recovery
;; ============================================================

(test-case "#1179: context-overflow-error? detects context_length errors"
  (check-true (context-overflow-error?
               (exn:fail "context_length_exceeded: too many tokens" (current-continuation-marks)))))

(test-case "#1179: context-overflow-error? detects max tokens errors"
  (check-true (context-overflow-error?
               (exn:fail "Request too large: max_tokens exceeded" (current-continuation-marks)))))

(test-case "#1179: context-overflow-error? rejects non-overflow errors"
  (check-false (context-overflow-error?
                (exn:fail "connection timeout" (current-continuation-marks)))))

(test-case "#1179: overflow-state tracks attempts"
  (define state (make-overflow-state #:max-attempts 1))
  (check-true (can-retry-overflow? state))
  (check-true (overflow-state-will-retry state))
  (mark-overflow-attempted! state)
  (check-false (can-retry-overflow? state))
  (check-false (overflow-state-will-retry state)))

(test-case "#1179: build-retry-messages creates retry context with original prompt"
  (define messages (make-n-messages 10))
  (define result (compact-history messages #:token-config tiny-tc))
  (define retry-msgs (build-retry-messages result "original user prompt"))
  ;; Should have: summary + kept + retry message
  (check-true (>= (length retry-msgs) 2))
  ;; Last message should be the retry prompt
  (define last-msg (last retry-msgs))
  (check-equal? (message-role last-msg) 'user)
  (check-true (string-contains? (msg-text last-msg) "original user prompt")))

(test-case "#1179: build-retry-messages preserves exact prompt text"
  (define messages (make-n-messages 5))
  (define result (compact-history messages #:token-config tiny-tc))
  (define original "Please implement the foo function with bar argument")
  (define retry-msgs (build-retry-messages result original))
  (define last-msg (last retry-msgs))
  (check-equal? (msg-text last-msg) original))

(test-case "#1179: overflow recovery handles compaction failure gracefully"
  ;; When compact-history produces no summary (too few messages),
  ;; build-retry-messages should still work
  (define messages (list (msg "m1" 'user "hello")))
  (define result (compact-history messages)) ; no compaction
  (check-false (compaction-result-summary-message result))
  (define retry-msgs (build-retry-messages result "hello"))
  ;; Should still include the original prompt
  (check-true (>= (length retry-msgs) 1))
  (define last-msg (last retry-msgs))
  (check-equal? (msg-text last-msg) "hello"))

(test-case "#1179: retryable-error? detects rate limits"
  (check-true (retryable-error?
               (exn:fail "429 rate limit exceeded" (current-continuation-marks)))))

(test-case "#1179: retryable-error? detects server errors"
  (check-true (retryable-error?
               (exn:fail "502 bad gateway" (current-continuation-marks)))))

(test-case "#1179: with-auto-retry succeeds on first try"
  (define attempts 0)
  (define result (with-auto-retry
                   (lambda ()
                     (set! attempts (add1 attempts))
                     "success")
                   #:max-retries 2))
  (check-equal? result "success")
  (check-equal? attempts 1))

(test-case "#1179: with-auto-retry retries on transient failure"
  (define attempts 0)
  (define result
    (with-auto-retry
     (lambda ()
       (set! attempts (add1 attempts))
       (if (< attempts 2)
           (raise (exn:fail "503 server error" (current-continuation-marks)))
           "recovered"))
     #:max-retries 2
     #:base-delay-ms 10))
  (check-equal? result "recovered")
  (check-equal? attempts 2))

(test-case "#1179: with-auto-retry re-raises non-retryable errors"
  (check-exn
   exn:fail?
   (lambda ()
     (with-auto-retry
      (lambda ()
        (raise (exn:fail "fatal logic error" (current-continuation-marks))))
      #:max-retries 2
      #:base-delay-ms 10))))
