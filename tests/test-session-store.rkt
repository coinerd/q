#lang racket

;;; tests/test-session-store.rkt — tests for runtime/session-store.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         "../agent/types.rkt"
         "../runtime/session-store.rkt")

;; ── Helpers ──

(define (make-temp-dir)
  (make-temporary-file "q-session-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (make-test-message id parent-id role kind)
  (make-message id parent-id role kind
                (list (make-text-part "hello"))
                (current-seconds)
                (hasheq)))

(define (make-timestamped-message id parent-id role kind ts)
  (make-message id parent-id role kind
                (list (make-text-part "hello"))
                ts
                (hasheq)))

(define (msg-id msg)
  (message-id msg))

;; ── Test suite ──

(define-test-suite test-session-store

  ;; ── append-entry! ──

  (test-case "append-entry! creates file and writes one entry"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define m (make-test-message "id1" #f 'user 'message))
    (append-entry! path m)
    (check-true (file-exists? path))
    (define entries (load-session-log path))
    (check-equal? (length entries) 1)
    (check-equal? (message-id (car entries)) "id1")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "append-entry! appends multiple entries sequentially"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-test-message "id1" #f 'user 'message))
    (append-entry! path (make-test-message "id2" "id1" 'assistant 'message))
    (define entries (load-session-log path))
    (check-equal? (length entries) 2)
    (check-equal? (message-id (first entries)) "id1")
    (check-equal? (message-id (second entries)) "id2")
    (delete-directory/files dir #:must-exist? #f))

  ;; ── append-entries! ──

  (test-case "append-entries! writes multiple entries atomically"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define msgs (list (make-test-message "id1" #f 'user 'message)
                       (make-test-message "id2" "id1" 'assistant 'message)
                       (make-test-message "id3" "id2" 'tool 'tool-result)))
    (append-entries! path msgs)
    (define entries (load-session-log path))
    (check-equal? (length entries) 3)
    (check-equal? (map message-id entries) '("id1" "id2" "id3"))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "append-entries! with empty list does not create file"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path '())
    (check-false (file-exists? path))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "append-entries! can append after existing entries"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-test-message "id1" #f 'user 'message))
    (append-entries! path (list (make-test-message "id2" "id1" 'assistant 'message)
                                (make-test-message "id3" "id2" 'tool 'tool-result)))
    (define entries (load-session-log path))
    (check-equal? (length entries) 3)
    (check-equal? (map message-id entries) '("id1" "id2" "id3"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── load-session-log ──

  (test-case "load-session-log returns empty list for nonexistent file"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (check-equal? (load-session-log path) '())
    (delete-directory/files dir #:must-exist? #f))

  (test-case "load-session-log skips corrupted lines"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Write a valid entry, then a corrupted line, then another valid entry
    (append-entry! path (make-test-message "id1" #f 'user 'message))
    ;; Append a bad line manually
    (call-with-output-file path
      (lambda (out) (displayln "THIS IS NOT JSON!!!" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! path (make-test-message "id2" "id1" 'assistant 'message))
    ;; load-session-log (valid-only) should skip the bad line
    (define entries (load-session-log path))
    (check-equal? (length entries) 2)
    (check-equal? (map message-id entries) '("id1" "id2"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── replay-session ──

  (test-case "replay-session returns messages in order"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path (list (make-test-message "id1" #f 'user 'message)
                                (make-test-message "id2" "id1" 'assistant 'message)))
    (define replayed (replay-session path))
    (check-equal? (length replayed) 2)
    (check-equal? (message-id (first replayed)) "id1")
    (check-equal? (message-id (second replayed)) "id2")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "replay-session preserves tree structure (parentId)"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path (list (make-test-message "root" #f 'system 'message)
                                (make-test-message "child1" "root" 'user 'message)
                                (make-test-message "child2" "root" 'assistant 'message)))
    (define replayed (replay-session path))
    (check-equal? (message-parent-id (first replayed)) #f)
    (check-equal? (message-parent-id (second replayed)) "root")
    (check-equal? (message-parent-id (third replayed)) "root")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "replay-session preserves all message fields"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define m (make-message "id1" #f 'user 'message
                            (list (make-text-part "hello world"))
                            12345
                            (hasheq 'model "test-model")))
    (append-entry! path m)
    (define replayed (replay-session path))
    (check-equal? (length replayed) 1)
    (define r (first replayed))
    (check-equal? (message-id r) "id1")
    (check-equal? (message-parent-id r) #f)
    (check-equal? (message-role r) 'user)
    (check-equal? (message-kind r) 'message)
    (check-equal? (text-part-text (first (message-content r))) "hello world")
    (check-equal? (message-timestamp r) 12345)
    (check-equal? (hash-ref (message-meta r) 'model) "test-model")
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Roundtrip with different content types ──

  (test-case "roundtrip with tool-call and tool-result content parts"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define m1 (make-message "id1" #f 'assistant 'message
                             (list (make-tool-call-part "tc1" "read"
                                      (hasheq 'path "/tmp/x.txt")))
                             1000 (hasheq)))
    (define m2 (make-message "id2" "id1" 'tool 'tool-result
                             (list (make-tool-result-part "tc1"
                                      (list (hasheq 'type "text" 'text "file contents"))
                                      #f))
                             1001 (hasheq)))
    (append-entries! path (list m1 m2))
    (define replayed (replay-session path))
    (check-equal? (length replayed) 2)
    ;; Check tool-call part
    (define tc (first (message-content (first replayed))))
    (check-equal? (tool-call-part-id tc) "tc1")
    (check-equal? (tool-call-part-name tc) "read")
    ;; Check tool-result part
    (define tr (first (message-content (second replayed))))
    (check-equal? (tool-result-part-tool-call-id tr) "tc1")
    (check-equal? (tool-result-part-is-error? tr) #f)
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Fork detection ──

  (test-case "two entries with same parentId form a fork"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "branch-a" "root" 'user 'message)
                           (make-test-message "branch-b" "root" 'assistant 'message)))
    (define replayed (replay-session path))
    (check-equal? (message-parent-id (second replayed)) "root")
    (check-equal? (message-parent-id (third replayed)) "root")
    ;; Both share same parent — that's a fork
    (check-equal? (message-parent-id (second replayed))
                  (message-parent-id (third replayed)))
    (delete-directory/files dir #:must-exist? #f))

  ;; ══════════════════════════════════════════════════════════
  ;; ── Crash Recovery / Replay Hardening (WP-36 Part A) ──
  ;; ══════════════════════════════════════════════════════════

  ;; ── 1. Verify integrity of a clean session log ──

  (test-case "verify-session-integrity: clean log passes all checks"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (make-timestamped-message "id1" #f 'user 'message 1000)
                           (make-timestamped-message "id2" "id1" 'assistant 'message 1100)
                           (make-timestamped-message "id3" "id2" 'tool 'tool-result 1200)))
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 3)
    (check-equal? (hash-ref report 'valid-entries) 3)
    (check-equal? (hash-ref report 'invalid-entries) '())
    (check-false (hash-ref report 'truncated-at-end?))
    (check-true (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 2. Verify integrity detects truncated entry at end ──

  (test-case "verify-session-integrity: detects truncated entry at end"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-timestamped-message "id1" #f 'user 'message 1000))
    ;; Manually append a partial JSON line (no closing brace, no newline)
    (call-with-output-file path
      (lambda (out) (display "{\"id\":\"truncated" out))
      #:mode 'text
      #:exists 'append)
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 2)
    (check-equal? (hash-ref report 'valid-entries) 1)
    ;; The truncated line should be in invalid-entries
    (check-equal? (length (hash-ref report 'invalid-entries)) 1)
    (check-equal? (hash-ref (first (hash-ref report 'invalid-entries)) 'line-number) 2)
    (check-true (hash-ref report 'truncated-at-end?))
    (check-true (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 3. Verify integrity detects invalid JSON line ──

  (test-case "verify-session-integrity: detects invalid JSON line"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-timestamped-message "id1" #f 'user 'message 1000))
    ;; Append an invalid JSON line (with newline, so file still ends with \n)
    (call-with-output-file path
      (lambda (out) (displayln "NOT VALID JSON!!!" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! path (make-timestamped-message "id2" "id1" 'assistant 'message 1100))
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 3)
    (check-equal? (hash-ref report 'valid-entries) 2)
    (check-equal? (length (hash-ref report 'invalid-entries)) 1)
    (check-equal? (hash-ref (first (hash-ref report 'invalid-entries)) 'line-number) 2)
    (check-false (hash-ref report 'truncated-at-end?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 4. Verify integrity detects out-of-order entries ──

  (test-case "verify-session-integrity: detects out-of-order entries"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Write entries with timestamps in decreasing order
    (append-entries! path
                     (list (make-timestamped-message "id1" #f 'user 'message 2000)
                           (make-timestamped-message "id2" "id1" 'assistant 'message 1000)))
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 2)
    (check-equal? (hash-ref report 'valid-entries) 2)
    (check-false (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 5. Verify integrity detects duplicate IDs ──

  (test-case "verify-session-integrity: detects duplicate entry IDs"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (make-timestamped-message "dup-id" #f 'user 'message 1000)
                           (make-timestamped-message "dup-id" #f 'assistant 'message 1100)))
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 2)
    (check-equal? (hash-ref report 'valid-entries) 1)
    (define invalids (hash-ref report 'invalid-entries))
    (check-equal? (length invalids) 1)
    (check-equal? (hash-ref (first invalids) 'line-number) 2)
    (check-true (string-contains? (hash-ref (first invalids) 'reason) "duplicate"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 6. Repair removes truncated entries ──

  (test-case "repair-session-log! removes truncated entries"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-timestamped-message "id1" #f 'user 'message 1000))
    ;; Manually append a truncated partial line (no newline)
    (call-with-output-file path
      (lambda (out) (display "{\"id\":\"truncated" out))
      #:mode 'text
      #:exists 'append)
    ;; Verify before repair: 1 valid, 1 invalid, truncated
    (define before (verify-session-integrity path))
    (check-equal? (hash-ref before 'valid-entries) 1)
    (check-true (hash-ref before 'truncated-at-end?))
    ;; Repair
    (define report (repair-session-log! path))
    (check-equal? (hash-ref report 'entries-kept) 1)
    (check-equal? (hash-ref report 'entries-removed) 1)
    ;; After repair: clean log
    (define after (verify-session-integrity path))
    (check-equal? (hash-ref after 'total-entries) 1)
    (check-equal? (hash-ref after 'valid-entries) 1)
    (check-false (hash-ref after 'truncated-at-end?))
    ;; Content preserved
    (define entries (load-session-log path))
    (check-equal? (message-id (first entries)) "id1")
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 7. Repair removes invalid entries and keeps valid ones ──

  (test-case "repair-session-log! removes invalid entries and keeps valid ones"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-timestamped-message "id1" #f 'user 'message 1000))
    (call-with-output-file path
      (lambda (out) (displayln "BROKEN LINE" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! path (make-timestamped-message "id2" "id1" 'assistant 'message 1100))
    (call-with-output-file path
      (lambda (out) (displayln "ANOTHER BAD LINE" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! path (make-timestamped-message "id3" "id2" 'tool 'tool-result 1200))
    ;; Repair
    (define report (repair-session-log! path))
    (check-equal? (hash-ref report 'entries-kept) 3)
    (check-equal? (hash-ref report 'entries-removed) 2)
    ;; After repair: 3 valid entries
    (define after (verify-session-integrity path))
    (check-equal? (hash-ref after 'total-entries) 3)
    (check-equal? (hash-ref after 'valid-entries) 3)
    (check-equal? (hash-ref after 'invalid-entries) '())
    ;; Content preserved
    (define entries (load-session-log path))
    (check-equal? (map message-id entries) '("id1" "id2" "id3"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 8. Repair of clean log is a no-op ──

  (test-case "repair-session-log! on clean log is a no-op"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (make-timestamped-message "id1" #f 'user 'message 1000)
                           (make-timestamped-message "id2" "id1" 'assistant 'message 1100)))
    ;; Read file content before repair
    (define content-before (file->string path))
    ;; Repair should be a no-op
    (define report (repair-session-log! path))
    (check-equal? (hash-ref report 'entries-kept) 2)
    (check-equal? (hash-ref report 'entries-removed) 0)
    ;; File should be unchanged
    (check-equal? (file->string path) content-before)
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 9. Write-ahead marker created before append ──

  (test-case "write-ahead marker mechanism: marker path and creation"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define marker (pending-marker-path path))
    ;; Marker path should end with .pending
    (check-true (string-suffix? (path->string marker) ".pending"))
    ;; Before any append, no marker
    (check-false (has-pending-marker? path))
    ;; Simulate crash: manually create marker
    (call-with-output-file marker
      (lambda (out) (display 1 out))
      #:mode 'text
      #:exists 'truncate)
    ;; Now marker exists — simulates "created before append completed"
    (check-true (has-pending-marker? path))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 10. Write-ahead marker removed after successful append ──

  (test-case "write-ahead marker removed after successful append"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; No marker before append
    (check-false (has-pending-marker? path))
    ;; Append an entry
    (append-entry! path (make-test-message "id1" #f 'user 'message))
    ;; After successful append, marker should be gone
    (check-false (has-pending-marker? path))
    ;; Also for append-entries!
    (append-entries! path (list (make-test-message "id2" "id1" 'assistant 'message)))
    (check-false (has-pending-marker? path))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 11. Write-ahead marker detected on load indicates potential truncation ──

  (test-case "write-ahead marker detected indicates potential truncation"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define marker (pending-marker-path path))
    ;; Create a session log with valid entries
    (append-entries! path
                     (list (make-timestamped-message "id1" #f 'user 'message 1000)
                           (make-timestamped-message "id2" "id1" 'assistant 'message 1100)))
    ;; No marker initially
    (check-false (has-pending-marker? path))
    ;; Simulate crash: create marker
    (call-with-output-file marker
      (lambda (out) (display 1 out))
      #:mode 'text
      #:exists 'truncate)
    ;; has-pending-marker? should detect it
    (check-true (has-pending-marker? path))
    ;; load-session-log should still work (advisory, not a hard gate)
    (define entries (load-session-log path))
    (check-equal? (length entries) 2)
    (check-equal? (message-id (first entries)) "id1")
    (check-equal? (message-id (second entries)) "id2")
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 12. Repair preserves entry order ──

  (test-case "repair-session-log! preserves entry order"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Write entries with specific timestamps and IDs
    (append-entry! path (make-timestamped-message "first"  #f 'user 'message 1000))
    ;; Insert a bad line between valid entries
    (call-with-output-file path
      (lambda (out) (displayln "GARBAGE" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! path (make-timestamped-message "second" "first" 'assistant 'message 2000))
    (call-with-output-file path
      (lambda (out) (displayln "MORE GARBAGE" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! path (make-timestamped-message "third"  "second" 'tool 'tool-result 3000))
    ;; Repair
    (define report (repair-session-log! path))
    (check-equal? (hash-ref report 'entries-kept) 3)
    (check-equal? (hash-ref report 'entries-removed) 2)
    ;; Verify order is preserved
    (define entries (load-session-log path))
    (check-equal? (map message-id entries) '("first" "second" "third"))
    (check-equal? (map message-timestamp entries) '(1000 2000 3000))
    ;; Verify integrity after repair
    (define after (verify-session-integrity path))
    (check-true (hash-ref after 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 13. Verify integrity on empty file ──

  (test-case "verify-session-integrity on empty file"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Create an empty file
    (close-output-port (open-output-file path))
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 0)
    (check-equal? (hash-ref report 'valid-entries) 0)
    (check-equal? (hash-ref report 'invalid-entries) '())
    (check-false (hash-ref report 'truncated-at-end?))
    (check-true (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── 14. Repair on empty file is a no-op ──

  (test-case "repair-session-log! on empty file is a no-op"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Create an empty file
    (close-output-port (open-output-file path))
    (check-true (file-exists? path))
    ;; Repair should be a no-op
    (define report (repair-session-log! path))
    (check-equal? (hash-ref report 'entries-kept) 0)
    (check-equal? (hash-ref report 'entries-removed) 0)
    ;; File should still exist and be empty
    (check-true (file-exists? path))
    (check-equal? (file-size path) 0)
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Verify integrity on nonexistent file ──

  (test-case "verify-session-integrity on nonexistent file"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 0)
    (check-equal? (hash-ref report 'valid-entries) 0)
    (check-equal? (hash-ref report 'invalid-entries) '())
    (check-false (hash-ref report 'truncated-at-end?))
    (check-true (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Repair on nonexistent file is a no-op ──

  (test-case "repair-session-log! on nonexistent file is a no-op"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define report (repair-session-log! path))
    (check-equal? (hash-ref report 'entries-kept) 0)
    (check-equal? (hash-ref report 'entries-removed) 0)
    (check-false (file-exists? path))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Repair also removes pending marker ──

  (test-case "repair-session-log! cleans up pending marker"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define marker (pending-marker-path path))
    (append-entry! path (make-test-message "id1" #f 'user 'message))
    ;; Simulate crash: create marker and append a bad line
    (call-with-output-file marker
      (lambda (out) (display 1 out))
      #:mode 'text
      #:exists 'truncate)
    (call-with-output-file path
      (lambda (out) (displayln "BAD" out))
      #:mode 'text
      #:exists 'append)
    (check-true (has-pending-marker? path))
    ;; Repair should clean up marker
    (repair-session-log! path)
    (check-false (has-pending-marker? path))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Verify integrity detects missing required fields ──

  (test-case "verify-session-integrity: detects missing required fields"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-timestamped-message "id1" #f 'user 'message 1000))
    ;; Append valid JSON but missing required fields (no 'role', 'kind', etc.)
    (call-with-output-file path
      (lambda (out)
        (displayln "{\"id\":\"incomplete\",\"text\":\"missing fields\"}" out))
      #:mode 'text
      #:exists 'append)
    (define report (verify-session-integrity path))
    (check-equal? (hash-ref report 'total-entries) 2)
    (check-equal? (hash-ref report 'valid-entries) 1)
    (define invalids (hash-ref report 'invalid-entries))
    (check-equal? (length invalids) 1)
    (check-equal? (hash-ref (first invalids) 'line-number) 2)
    (check-true (string-contains? (hash-ref (first invalids) 'reason) "missing required fields"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Single-entry order is always valid ──

  (test-case "verify-session-integrity: single entry has valid order"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entry! path (make-timestamped-message "id1" #f 'user 'message 1000))
    (define report (verify-session-integrity path))
    (check-true (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Equal timestamps are valid (non-decreasing) ──

  (test-case "verify-session-integrity: equal timestamps are valid order"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (make-timestamped-message "id1" #f 'user 'message 1000)
                           (make-timestamped-message "id2" "id1" 'assistant 'message 1000)))
    (define report (verify-session-integrity path))
    (check-true (hash-ref report 'entry-order-valid?))
    (delete-directory/files dir #:must-exist? #f))
  )

;; Run
(run-tests test-session-store)
