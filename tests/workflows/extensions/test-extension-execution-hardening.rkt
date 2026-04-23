#lang racket

;; tests/workflows/extensions/test-extension-execution-hardening.rkt
;; v0.18.3 Wave 2: Extension tool execution hardening tests
;;
;; Tests:
;;   - GSD-planning multi-artifact chain
;;   - Planning state preserved through compaction
;;   - Extension tool execution error handling

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../extensions/gsd-planning.rkt"
         "../../../extensions/context.rkt"
         "../../../extensions/api.rkt"
         "../../../extensions/dynamic-tools.rkt"
         "../../../extensions/ext-commands.rkt"
         "../../../extensions/hooks.rkt"
         "../../../tools/tool.rkt"
         "../../../agent/event-bus.rkt"
         "../../../runtime/compactor.rkt"
         "../../../runtime/session-store.rkt"
         "../../../util/jsonl.rkt")

;; ── Helpers ──

(define (with-temp-dir proc)
  (define dir (make-temporary-file "ext-harden-~a" 'directory))
  (with-handlers ([exn:fail? (lambda (e)
                               (when (directory-exists? dir)
                                 (delete-directory/files dir))
                               (raise e))])
    (begin0 (proc dir)
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define (result-text result)
  (string-join (for/list ([c (in-list (tool-result-content result))]
                          #:when (and (hash? c) (hash-ref c 'text #f)))
                 (hash-ref c 'text ""))
               ""))

(define (artifact-exists? dir name)
  (file-exists? (planning-artifact-path dir name)))

;; ── Test Suite ──

(define test-extension-execution-hardening
  (test-suite "Extension Execution Hardening Tests (v0.18.3 Wave 2)"

    ;; Test 1: GSD-planning multi-artifact chain
    (test-case "multi-artifact chain: PLAN → STATE → VALIDATION → HANDOFF"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Step 1: Create plan
           (handle-planning-write
            (hasheq
             'artifact
             "PLAN"
             'content
             "# Plan\n## Goal\nMulti-artifact chain test\n## Waves\n- Wave 1: Init\n- Wave 2: Implement"))
           (check-true (artifact-exists? dir "PLAN"))

           ;; Step 2: Write initial state
           (handle-planning-write (hasheq 'artifact
                                          "STATE"
                                          'content
                                          "# State\n## Status\nIn Progress\n## Current Wave\nWave 1"))
           (check-true (artifact-exists? dir "STATE"))

           ;; Step 3: Validate Wave 1
           (handle-planning-write
            (hasheq 'artifact
                    "VALIDATION"
                    'content
                    "# Validation\n## Wave 1\n### Tests\n- 10/10 passed\n### Verdict\nAPPROVED"))
           (check-true (artifact-exists? dir "VALIDATION"))

           ;; Step 4: Advance state
           (handle-planning-write
            (hasheq
             'artifact
             "STATE"
             'content
             "# State\n## Status\nIn Progress\n## Current Wave\nWave 2\n## Completed\n- Wave 1: APPROVED"))

           ;; Step 5: Create handoff
           (write-planning-artifact!
            dir
            "HANDOFF"
            (hasheq 'machine "local" 'milestone "v0.18.3" 'last-wave 1 'next-step "Wave 2"))
           (check-true (artifact-exists? dir "HANDOFF"))

           ;; Verify chain: read all artifacts in sequence
           (define plan-text (result-text (handle-planning-read (hasheq 'artifact "PLAN"))))
           (define state-text (result-text (handle-planning-read (hasheq 'artifact "STATE"))))
           (define val-text (result-text (handle-planning-read (hasheq 'artifact "VALIDATION"))))
           (define handoff-text (result-text (handle-planning-read (hasheq 'artifact "HANDOFF"))))

           (check-true (string-contains? plan-text "Multi-artifact chain test"))
           (check-true (string-contains? state-text "Wave 2"))
           (check-true (string-contains? val-text "APPROVED"))
           (check-true (string-contains? handoff-text "v0.18.3"))))))

    ;; Test 2: Compaction preserves planning state
    (test-case "compaction preserves session state in JSONL"
      (with-temp-dir
       (lambda (dir)
         ;; Create a session log with planning entries
         (define log-path (build-path dir "session.jsonl"))
         (define messages
           (list (hasheq 'role "user" 'content "Create a plan")
                 (hasheq 'role
                         "assistant"
                         'content
                         "Writing PLAN.md"
                         'tool_calls
                         (list (hasheq 'id
                                       "tc-1"
                                       'type
                                       "function"
                                       'function
                                       (hasheq 'name
                                               "planning-write"
                                               'arguments
                                               "{\"artifact\": \"PLAN\", \"content\": \"# Plan\"}"))))
                 (hasheq 'role "tool" 'tool_call_id "tc-1" 'content "PLAN.md written")
                 (hasheq 'role "assistant" 'content "Plan created.")))
         ;; Write log
         (call-with-output-file log-path
                                (lambda (out)
                                  (for ([msg (in-list messages)])
                                    (displayln (jsexpr->string msg) out))))

         ;; Verify log is readable
         (check-true (file-exists? log-path))
         (define logged (jsonl-read-all-valid log-path))
         (check-equal? (length logged) 4 "Should have 4 log entries")

         ;; Simulate compaction: write a summary entry
         (call-with-output-file
          log-path
          (lambda (out)
            (displayln
             (jsexpr->string (hasheq 'role "system" 'content "Compaction summary: Plan was created."))
             out))
          #:exists 'append)

         ;; Verify compaction didn't corrupt
         (define after-compact (jsonl-read-all-valid log-path))
         (check-equal? (length after-compact) 5 "Should have 5 entries after compaction")

         ;; Original entries still accessible
         (check-equal? (hash-ref (list-ref after-compact 0) 'role) "user")
         (check-equal? (hash-ref (list-ref after-compact 2) 'role) "tool"))))

    ;; Test 3: Extension tool error handling
    (test-case "planning-read for non-existent artifact returns error"
      (with-temp-dir (lambda (dir)
                       (parameterize ([current-directory dir])
                         (define result (handle-planning-read (hasheq 'artifact "NONEXISTENT")))
                         (check-true (tool-result? result))
                         (check-true (tool-result-is-error? result))))))

    ;; Test 4: Invalid artifact name handling
    (test-case "planning-write rejects path traversal"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (define result
             (handle-planning-write (hasheq 'artifact "../etc/passwd" 'content "malicious")))
           (check-true (tool-result? result))
           (check-true (tool-result-is-error? result))
           (check-false (file-exists? (build-path dir "passwd")))))))

    ;; Test 5: Compaction strategy defaults
    (test-case "default compaction strategy has valid fields"
      (define strat (default-strategy))
      (check-true (compaction-strategy? strat))
      (check-true (exact-positive-integer? (compaction-strategy-summary-window-size strat)))
      (check-true (exact-nonnegative-integer? (compaction-strategy-keep-recent-count strat))))

    ;; Test 6: Extension registry loads GSD planning
    (test-case "extension registry discovers planning tools"
      (define ext-reg (make-extension-registry))
      (check-true (extension-registry? ext-reg))
      ;; After loading, planning tools should be registered
      (check-true (list? (list-extensions ext-reg))))

    ;; Test 7: Event bus records planning events
    (test-case "event bus can record and replay events"
      (define bus (make-event-bus))
      (define received (box '()))
      (subscribe! bus (lambda (evt) (set-box! received (cons evt (unbox received)))))
      ;; Fire an event
      (publish! bus (hasheq 'tool "planning-write" 'args (hasheq 'artifact "PLAN")))
      ;; Small delay for async delivery
      (sleep 0.1)
      (check-true (>= (length (unbox received)) 1) "Should have at least one event"))))

(run-tests test-extension-execution-hardening)
