#lang racket

;;; tests/test-session-replay.rkt — tests for q/cli/replay.rkt
;;;
;;; Covers:
;;;   1. Matched tool call + result → no drift
;;;   2. Unmatched tool call (missing result) → drift detected
;;;   3. Orphan tool result (no matching call) → drift detected
;;;   4. Non-deterministic tools flagged in report
;;;   5. Empty session → valid report with 0 counts
;;;   6. Corrupted/missing file → safe wrapper returns error hash
;;;   7. format-replay-report produces non-empty output with expected keywords
;;;   8. Session with multiple tool calls → correct count

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../cli/replay.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-session-replay-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (write-session! path messages)
  (append-entries! path messages))

;; ============================================================
;; Test Data Builders
;; ============================================================

(define (make-simple-session)
  ;; user → assistant (tool-call bash "ls -la") → tool (tool-result for tc1)
  (list
   (make-message "m1" #f 'user 'message
                 (list (make-text-part "list files")) 1000 (hasheq))
   (make-message "m2" "m1" 'assistant 'message
                 (list (make-tool-call-part "tc1" "bash" "ls -la")) 1001 (hasheq))
   (make-message "m3" "m2" 'tool 'message
                 (list (make-tool-result-part "tc1" "file1.txt\nfile2.txt" #f)) 1002 (hasheq))))

(define (make-unmatched-call-session)
  ;; Tool call with NO matching result → drift
  (list
   (make-message "m1" #f 'user 'message
                 (list (make-text-part "do something")) 1000 (hasheq))
   (make-message "m2" "m1" 'assistant 'message
                 (list (make-tool-call-part "tc-orphan" "bash" "rm -rf /")) 1001 (hasheq))))

(define (make-orphan-result-session)
  ;; Tool result referencing a non-existent tool-call-id → drift
  (list
   (make-message "m1" #f 'user 'message
                 (list (make-text-part "hello")) 1000 (hasheq))
   (make-message "m2" "m1" 'tool 'message
                 (list (make-tool-result-part "ghost-tc" "some output" #f)) 1001 (hasheq))))

(define (make-mixed-session)
  ;; Multiple tool calls: read (deterministic), bash (non-deterministic),
  ;; one unmatched call, one orphan result
  (list
   (make-message "m1" #f 'user 'message
                 (list (make-text-part "do stuff")) 1000 (hasheq))
   ;; Matched read call
   (make-message "m2" "m1" 'assistant 'message
                 (list (make-tool-call-part "tc-read-1" "read" "/etc/hosts")) 1001 (hasheq))
   (make-message "m3" "m2" 'tool 'message
                 (list (make-tool-result-part "tc-read-1" "127.0.0.1 localhost" #f)) 1002 (hasheq))
   ;; Matched bash call
   (make-message "m4" "m3" 'assistant 'message
                 (list (make-tool-call-part "tc-bash-1" "bash" "date")) 1003 (hasheq))
   (make-message "m5" "m4" 'tool 'message
                 (list (make-tool-result-part "tc-bash-1" "Fri Apr 10 12:00:00 UTC 2026" #f)) 1004 (hasheq))
   ;; Unmatched call (no result)
   (make-message "m6" "m5" 'assistant 'message
                 (list (make-tool-call-part "tc-unmatched" "bash" "ls /tmp")) 1005 (hasheq))
   ;; Orphan result (no call)
   (make-message "m7" "m6" 'tool 'message
                 (list (make-tool-result-part "tc-ghost" "phantom data" #f)) 1006 (hasheq))))

(define (make-multi-call-session)
  ;; 3 matched tool calls across multiple messages
  (list
   (make-message "m1" #f 'user 'message
                 (list (make-text-part "go")) 1000 (hasheq))
   (make-message "m2" "m1" 'assistant 'message
                 (list (make-tool-call-part "tc1" "read" "a.txt")
                       (make-tool-call-part "tc2" "bash" "ls")) 1001 (hasheq))
   (make-message "m3" "m2" 'tool 'message
                 (list (make-tool-result-part "tc1" "content of a" #f)) 1002 (hasheq))
   (make-message "m4" "m3" 'tool 'message
                 (list (make-tool-result-part "tc2" "file1\nfile2" #f)) 1003 (hasheq))
   (make-message "m5" "m4" 'assistant 'message
                 (list (make-tool-call-part "tc3" "edit" "fix typo")) 1004 (hasheq))
   (make-message "m6" "m5" 'tool 'message
                 (list (make-tool-result-part "tc3" "ok" #f)) 1005 (hasheq))))

;; ============================================================
;; Test Suite
;; ============================================================

(define-test-suite test-session-replay

  ;; ── Test 1: Matched tool call + result → no drift ──
  (test-case "matched tool call and result: no drift"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-simple-session))
    (write-session! path msgs)

    (define report (replay-session-report path))
    (define results (hash-ref report 'replay-results))

    (check-equal? (length results) 1 "one tool call/result pair")
    (define r (first results))
    (check-false (replay-result-drifted? r) "no drift for matched pair")
    (check-equal? (replay-result-tool-name r) "bash")
    (check-equal? (replay-result-reason r) #f "no reason when no drift")
    (check-equal? (hash-ref report 'tool-call-count) 1)
    (check-equal? (hash-ref report 'drift-count) 0)

    (delete-directory/files dir))

  ;; ── Test 2: Unmatched tool call (missing result) → drift ──
  (test-case "unmatched tool call: drift detected"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-unmatched-call-session))
    (write-session! path msgs)

    (define report (replay-session-report path))
    (define results (hash-ref report 'replay-results))

    (check-equal? (length results) 1)
    (define r (first results))
    (check-true (replay-result-drifted? r) "drift for unmatched call")
    (check-equal? (replay-result-reason r) "missing result")

    (delete-directory/files dir))

  ;; ── Test 3: Orphan tool result → drift ──
  (test-case "orphan tool result: drift detected"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-orphan-result-session))
    (write-session! path msgs)

    (define report (replay-session-report path))
    (define results (hash-ref report 'replay-results))

    (check-equal? (length results) 1)
    (define r (first results))
    (check-true (replay-result-drifted? r) "drift for orphan result")
    (check-equal? (replay-result-reason r) "orphan result")

    (delete-directory/files dir))

  ;; ── Test 4: Non-deterministic tools flagged ──
  (test-case "non-deterministic tools flagged in report"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-multi-call-session))
    (write-session! path msgs)

    (define report (replay-session-report path))

    ;; bash is non-deterministic
    (define nd-tools (hash-ref report 'non-deterministic))
    (check-not-false (member "bash" nd-tools) "bash is non-deterministic")
    (check-false (member "read" nd-tools) "read is deterministic")

    ;; 1 non-deterministic tool call (bash) should be skipped
    (check-equal? (hash-ref report 'skipped) 1 "one non-deterministic call skipped")

    (delete-directory/files dir))

  ;; ── Test 5: Empty session ──
  (test-case "empty session: valid report with zero counts"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Don't write anything — file doesn't exist

    (define report (replay-session-report path))

    (check-equal? (hash-ref report 'total-entries) 0)
    (check-equal? (hash-ref report 'tool-call-count) 0)
    (check-equal? (hash-ref report 'drift-count) 0)
    (check-equal? (hash-ref report 'skipped) 0)
    (check-equal? (hash-ref report 'replay-results) '())
    (check-equal? (hash-ref report 'drifts) '())

    (delete-directory/files dir))

  ;; ── Test 6: Corrupted/missing file → safe wrapper ──
  (test-case "safe wrapper: missing file returns error hash"
    (define dir (make-temp-dir))
    (define path (build-path dir "nonexistent.jsonl"))

    (define report (replay-session-safe path))

    (check-not-false (hash-ref report 'error #f)
                     "error key present for missing file")
    (check-equal? (hash-ref report 'total-entries) 0)

    (delete-directory/files dir))

  (test-case "safe wrapper: corrupted file returns partial results"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Write corrupted JSONL (not valid JSON)
    (call-with-output-file path
      (lambda (out)
        (write-string "NOT VALID JSON\n" out))
      #:mode 'text
      #:exists 'replace)

    (define report (replay-session-safe path))
    ;; Should succeed — corrupted entries are skipped by load-session-log
    (check-equal? (hash-ref report 'total-entries) 0 "corrupted entries skipped")

    (delete-directory/files dir))

  ;; ── Test 7: format-replay-report ──
  (test-case "format-replay-report: produces expected output"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-multi-call-session))
    (write-session! path msgs)

    (define report (replay-session-report path))
    (define output (format-replay-report report))

    (check-true (non-empty-string? output) "output is non-empty")
    (check-true (string-contains? output "Tool calls") "mentions tool calls")
    (check-true (string-contains? output "Drift") "mentions drift")
    (check-true (string-contains? output "Entries") "mentions entries")

    (delete-directory/files dir))

  ;; ── Test 8: Multiple tool calls → correct count ──
  (test-case "multiple tool calls: correct count and pairing"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-multi-call-session))
    (write-session! path msgs)

    (define report (replay-session-report path))

    (check-equal? (hash-ref report 'tool-call-count) 3 "three tool calls")
    (check-equal? (hash-ref report 'total-entries) 6 "six messages")
    (check-equal? (hash-ref report 'drift-count) 0 "all matched")

    (define results (hash-ref report 'replay-results))
    (check-equal? (length results) 3 "three replay results")

    (delete-directory/files dir))

  ;; ── Test 9: Mixed session with drift and non-deterministic ──
  (test-case "mixed session: drift and non-deterministic detection"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (make-mixed-session))
    (write-session! path msgs)

    (define report (replay-session-report path))

    ;; 4 tool calls total (tc-read-1, tc-bash-1, tc-unmatched, orphan ghost)
    (check-equal? (hash-ref report 'tool-call-count) 4)
    ;; 2 drifts: unmatched call + orphan result
    (check-equal? (hash-ref report 'drift-count) 2)

    (define results (hash-ref report 'replay-results))
    (check-equal? (length results) 4)

    ;; Check drift descriptions
    (define drifts (hash-ref report 'drifts))
    (check-equal? (length drifts) 2)

    ;; bash is non-deterministic, 1 bash call (tc-bash-1) + 1 unmatched bash call = 2 non-det
    (check-equal? (hash-ref report 'skipped) 2 "two non-deterministic calls skipped")

    (delete-directory/files dir))

  ;; ── Test 10: replay-tool-calls works on raw messages ──
  (test-case "replay-tool-calls on raw message list"
    (define msgs (make-simple-session))
    (define results (replay-tool-calls msgs))

    (check-equal? (length results) 1)
    (check-false (replay-result-drifted? (first results)))
    (check-equal? (replay-result-tool-name (first results)) "bash"))

  ;; ── Test 11: Message with multiple tool-call parts ──
  (test-case "message with multiple tool calls in single message"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs
      (list
       (make-message "m1" #f 'user 'message
                     (list (make-text-part "go")) 1000 (hasheq))
       ;; Two tool calls in one message
       (make-message "m2" "m1" 'assistant 'message
                     (list (make-tool-call-part "tc-a" "read" "file1")
                           (make-tool-call-part "tc-b" "read" "file2")) 1001 (hasheq))
       ;; Results in separate messages
       (make-message "m3" "m2" 'tool 'message
                     (list (make-tool-result-part "tc-a" "content1" #f)) 1002 (hasheq))
       (make-message "m4" "m2" 'tool 'message
                     (list (make-tool-result-part "tc-b" "content2" #f)) 1003 (hasheq))))
    (write-session! path msgs)

    (define report (replay-session-report path))
    (check-equal? (hash-ref report 'tool-call-count) 2)
    (check-equal? (hash-ref report 'drift-count) 0)

    (delete-directory/files dir))
  )

;; Run
(run-tests test-session-replay)
