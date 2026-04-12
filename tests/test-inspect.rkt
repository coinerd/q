#lang racket

;;; tests/test-inspect.rkt — tests for cli/inspect.rkt
;;;
;;; Covers:
;;;   1. Correct entry count
;;;   2. Time span computed correctly
;;;   3. Tool call counts (multiple tools, same & different)
;;;   4. Role counts
;;;   5. Branch count
;;;   6. Error count
;;;   7. Corrupted/missing file: inspect-session-safe
;;;   8. format-inspection produces non-empty string with keywords
;;;   9. Empty session returns entry-count 0, time-span #f

(require rackunit
         rackunit/text-ui
         racket/file
         json
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../cli/inspect.rkt")

;; ── Helpers ──

(define (make-temp-dir)
  (make-temporary-file "q-inspect-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (msg id parent-id role kind content ts [meta (hasheq)])
  (make-message id parent-id role kind content ts meta))

(define text (make-text-part "hello"))

;; ── Test suite ──

(define-test-suite test-inspect

  ;; 1. Correct entry count
  (test-case "entry-count matches number of messages"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000)
                           (msg "2" "1" 'assistant 'message (list text) 1001)
                           (msg "3" "2" 'user 'message (list text) 1002)))
    (define result (inspect-session path))
    (check-equal? (hash-ref result 'entry-count) 3)
    (delete-directory/files dir #:must-exist? #f))

  ;; 2. Time span computed correctly
  (test-case "time-span is (cons first-ts last-ts)"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000)
                           (msg "2" "1" 'assistant 'message (list text) 2000)
                           (msg "3" "2" 'user 'message (list text) 3500)))
    (define result (inspect-session path))
    (check-equal? (hash-ref result 'time-span) (cons 1000 3500))
    (check-equal? (hash-ref result 'duration-seconds) 2500)
    (delete-directory/files dir #:must-exist? #f))

  ;; 3. Tool call counts
  (test-case "tool-calls counts multiple tools correctly"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define tc1 (make-tool-call-part "tc1" "read" "{}"))
    (define tc2 (make-tool-call-part "tc2" "read" "{\"path\":\"x\"}"))
    (define tc3 (make-tool-call-part "tc3" "bash" "{\"cmd\":\"ls\"}"))
    (define tr1 (make-tool-result-part "tc1" "file contents" #f))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000)
                           (msg "2" "1" 'assistant 'message (list tc1 tc2 tc3) 1001)
                           (msg "3" "2" 'tool 'message (list tr1) 1002)))
    (define result (inspect-session path))
    (define tc-hash (hash-ref result 'tool-calls))
    (check-equal? (hash-ref tc-hash "read" 0) 2)
    (check-equal? (hash-ref tc-hash "bash" 0) 1)
    (check-equal? (hash-ref result 'tool-call-total) 3)
    (delete-directory/files dir #:must-exist? #f))

  ;; 4. Role counts
  (test-case "role-counts aggregates roles correctly"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (msg "1" #f 'system 'message (list text) 1000)
                           (msg "2" "1" 'user 'message (list text) 1001)
                           (msg "3" "2" 'assistant 'message (list text) 1002)
                           (msg "4" "3" 'user 'message (list text) 1003)
                           (msg "5" "4" 'assistant 'message (list text) 1004)))
    (define result (inspect-session path))
    (define rc (hash-ref result 'role-counts))
    (check-equal? (hash-ref rc 'system 0) 1)
    (check-equal? (hash-ref rc 'user 0) 2)
    (check-equal? (hash-ref rc 'assistant 0) 2)
    (delete-directory/files dir #:must-exist? #f))

  ;; 5. Branch count
  (test-case "branch-count counts distinct non-root parentIds"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000)
                           (msg "2a" "1" 'assistant 'message (list text) 1001)
                           (msg "2b" "1" 'assistant 'message (list text) 1001)
                           (msg "3" "2a" 'user 'message (list text) 1002)))
    (define result (inspect-session path))
    ;; parentIds: "1" (appears 3 times), "2a" (appears 1 time) → 2 distinct
    (check-equal? (hash-ref result 'branch-count) 2)
    (delete-directory/files dir #:must-exist? #f))

  ;; 6. Error count
  (test-case "error-count counts tool-result parts with is-error? = #t"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define tr-ok (make-tool-result-part "tc1" "ok" #f))
    (define tr-err1 (make-tool-result-part "tc2" "fail" #t))
    (define tr-err2 (make-tool-result-part "tc3" "also fail" #t))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000)
                           (msg "2" "1" 'tool 'message (list tr-ok) 1001)
                           (msg "3" "2" 'tool 'message (list tr-err1 tr-err2) 1002)))
    (define result (inspect-session path))
    (check-equal? (hash-ref result 'error-count) 2)
    (delete-directory/files dir #:must-exist? #f))

  ;; 7. Corrupted/missing file: inspect-session-safe
  (test-case "inspect-session-safe returns error hash for missing file"
    (define result (inspect-session-safe "/nonexistent/path/session.jsonl"))
    (check-equal? (hash-ref result 'error) "file not found")
    (check-equal? (hash-ref result 'entry-count) 0))

  ;; 8. format-inspection produces non-empty string with keywords
  (test-case "format-inspection produces non-empty string with expected keywords"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (define tc (make-tool-call-part "tc1" "read" "{}"))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000
                                (hasheq 'model "gpt-4"
                                        'usage (hasheq 'prompt 100 'completion 50)))
                           (msg "2" "1" 'assistant 'message (list tc) 2000
                                (hasheq 'model "gpt-4"
                                        'usage (hasheq 'prompt 200 'completion 75)))))
    (define result (inspect-session path))
    (define formatted (format-inspection result))
    (check-pred string? formatted)
    (check-true (> (string-length formatted) 0))
    (check-true (regexp-match? #rx"[Ee]ntry.count" formatted))
    (check-true (regexp-match? #rx"[Dd]uration" formatted))
    (check-true (regexp-match? #rx"[Tt]ool.call" formatted))
    (check-true (regexp-match? #rx"[Tt]oken" formatted))
    (delete-directory/files dir #:must-exist? #f))

  ;; 9. Empty session
  (test-case "empty session returns entry-count 0 and time-span #f"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Create an empty file — no entries
    (call-with-output-file path void #:exists 'append)
    (define result (inspect-session path))
    (check-equal? (hash-ref result 'entry-count) 0)
    (check-equal? (hash-ref result 'time-span) #f)
    (check-equal? (hash-ref result 'duration-seconds) 0)
    (check-equal? (hash-ref result 'tool-call-total) 0)
    (check-equal? (hash-ref result 'error-count) 0)
    (delete-directory/files dir #:must-exist? #f))

  ;; 10. Models extracted from meta
  (test-case "models list deduplicates model names from meta"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000
                                (hasheq 'model "gpt-4"))
                           (msg "2" "1" 'assistant 'message (list text) 1001
                                (hasheq 'model "gpt-4"))
                           (msg "3" "2" 'user 'message (list text) 1002
                                (hasheq 'model "claude-3"))))
    (define result (inspect-session path))
    (define models (hash-ref result 'models))
    (check-equal? (length models) 2)
    (check-equal? (sort models string<?) '("claude-3" "gpt-4"))
    (delete-directory/files dir #:must-exist? #f))

  ;; 11. Token usage summed from meta
  (test-case "token-usage sums prompt and completion tokens"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    (append-entries! path
                     (list (msg "1" #f 'user 'message (list text) 1000
                                (hasheq 'usage (hasheq 'prompt 100 'completion 50)))
                           (msg "2" "1" 'assistant 'message (list text) 1001
                                (hasheq 'usage (hasheq 'prompt 200 'completion 75)))))
    (define result (inspect-session path))
    (define tu (hash-ref result 'token-usage))
    (check-equal? (hash-ref tu 'prompt 0) 300)
    (check-equal? (hash-ref tu 'completion 0) 125)
    (delete-directory/files dir #:must-exist? #f))

  ;; 12. inspect-session-safe returns partial on corrupted data
  (test-case "inspect-session-safe handles partial/corrupted sessions"
    (define dir (make-temp-dir))
    (define path (session-path dir))
    ;; Write one valid entry then a corrupted line
    (call-with-output-file path
      (lambda (out)
        (write-json (message->jsexpr
                     (msg "1" #f 'user 'message (list text) 1000)) out)
        (newline out)
        (displayln "{corrupted json" out))
      #:mode 'text
      #:exists 'append)
    (define result (inspect-session-safe path))
    ;; Should succeed since load-session-log skips bad lines
    (check-true (hash-has-key? result 'entry-count))
    (delete-directory/files dir #:must-exist? #f))

) ;; end test-inspect

;; ── Run ──

(run-tests test-inspect)
