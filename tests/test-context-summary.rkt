#lang racket

;; q/tests/test-context-summary.rkt — Tests for context-manager LLM summary (Wave 2A #1395)
;;
;; Tests the summary generation pipeline:
;;   - Summary prompt template
;;   - Summary cache (regenerate only when new entries)
;;   - generate-context-summary
;;   - First-turn shortcut (no old context → no summary)

(require rackunit
         rackunit/text-ui
         racket/list
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-id
                  message-role
                  message-content)
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-manager.rkt")

(define summary-tests
  (test-suite "context-summary"

    ;; Summary prompt template tests
    (test-case "summary-prompt-template produces structured prompt"
      (define msgs
        (list (make-message "m1"
                            #f
                            'user
                            'message
                            (list (make-text-part "Hello world"))
                            (current-seconds)
                            (hasheq))))
      (define prompt (context-summary-prompt msgs))
      (check-true (string-contains? prompt "## Goal"))
      (check-true (string-contains? prompt "## Progress"))
      (check-true (string-contains? prompt "## Critical Context"))
      (check-true (string-contains? prompt "Hello world")))

    (test-case "summary-prompt with previous summary uses iterative template"
      (define msgs
        (list (make-message "m1"
                            #f
                            'assistant
                            'message
                            (list (make-text-part "Did something"))
                            (current-seconds)
                            (hasheq))))
      (define prev "## Goal\nFix the bug\n## Progress\n### Done\n- [x] nothing yet")
      (define prompt (context-summary-prompt msgs #:previous-summary prev))
      (check-true (string-contains? prompt "EXISTING SUMMARY"))
      (check-true (string-contains? prompt "Fix the bug"))
      (check-true (string-contains? prompt "UPDATED summary")))

    ;; Summary cache tests
    (test-case "summary-cache starts empty"
      (define cache (make-summary-cache))
      (check-false (summary-cache-lookup cache "m1" "m5")))

    (test-case "summary-cache stores and retrieves"
      (define cache (make-summary-cache))
      (summary-cache-store! cache "m1" "m5" "Summary of m1-m5")
      (check-equal? (summary-cache-lookup cache "m1" "m5") "Summary of m1-m5"))

    (test-case "summary-cache invalidates on different range"
      (define cache (make-summary-cache))
      (summary-cache-store! cache "m1" "m5" "Old summary")
      (check-false (summary-cache-lookup cache "m1" "m6"))
      (check-equal? (summary-cache-lookup cache "m1" "m5") "Old summary"))

    ;; context-summary struct
    (test-case "context-summary struct has required fields"
      (define s (context-summary "m1" "m5" "Summary text" 500))
      (check-equal? (context-summary-from-id s) "m1")
      (check-equal? (context-summary-to-id s) "m5")
      (check-equal? (context-summary-text s) "Summary text")
      (check-equal? (context-summary-entry-count s) 500))

    ;; generate-context-summary returns #f when no entries
    (test-case "generate-context-summary returns #f for empty entries"
      (check-false (generate-context-summary '() #f #f)))

    ;; generate-context-summary with entries returns a summary
    (test-case "generate-context-summary with entries returns summary"
      (define entries
        (list (make-message "m1"
                            #f
                            'user
                            'message
                            (list (make-text-part "Fix the bug in foo.rkt"))
                            (current-seconds)
                            (hasheq))
              (make-message "m2"
                            "m1"
                            'assistant
                            'message
                            (list (make-text-part "I found the issue. It's on line 42."))
                            (current-seconds)
                            (hasheq))))
      (define result (generate-context-summary entries #f #f))
      (check-true (context-summary? result))
      (check-equal? (context-summary-from-id result) "m1")
      (check-equal? (context-summary-to-id result) "m2")
      (check-true (string-contains? (context-summary-text result) "m1"))
      (check-true (string-contains? (context-summary-text result) "foo.rkt")))

    ;; generate-context-summary uses cache
    (test-case "generate-context-summary uses cache on second call"
      (define entries
        (list (make-message "m1"
                            #f
                            'user
                            'message
                            (list (make-text-part "Hello"))
                            (current-seconds)
                            (hasheq))))
      (define cache (make-summary-cache))
      (define result1 (generate-context-summary entries #f #f #:cache cache))
      ;; Store a different value via cache directly
      (summary-cache-store! cache "m1" "m1" "Cached summary")
      (define result2 (generate-context-summary entries #f #f #:cache cache))
      (check-equal? (context-summary-text result2) "Cached summary"))

    ;; Small session: assemble-context works with no excluded entries
    (test-case "assemble-context for small session has no excluded entries"
      (define dir (make-temporary-file "q-test-~a" 'directory))
      (define sp (build-path dir "session.jsonl"))
      (define ip (build-path dir "session.index"))
      (define msgs
        (list (make-message "sys"
                            #f
                            'system
                            'system-instruction
                            (list (make-text-part "You are helpful."))
                            (current-seconds)
                            (hasheq))
              (make-message "u1"
                            "sys"
                            'user
                            'message
                            (list (make-text-part "Hello"))
                            (current-seconds)
                            (hasheq))
              (make-message "a1"
                            "u1"
                            'assistant
                            'message
                            (list (make-text-part "Hi there"))
                            (current-seconds)
                            (hasheq))))
      (append-entries! sp msgs)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 10000))
      (define-values (result catalog) (assemble-context idx cfg))
      ;; Small session fits entirely — no summary needed
      (check-equal? (length result) 3)
      (check-equal? (length catalog) 0)
      (delete-directory/files dir #:must-exist? #f))))

(run-tests summary-tests)
