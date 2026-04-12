#lang racket

;;; tests/test-scheduler-safe-mode.rkt — TDD tests for scheduler-level safe-mode
;;;
;;; Covers (ARCH-02/QUAL-07, #191):
;;;   - Tool is blocked in safe mode (bash, edit, write, firecrawl)
;;;   - Tool is allowed in safe mode (read, ls, find, grep)
;;;   - Path is blocked in safe mode (outside project root)
;;;   - Path is allowed in safe mode (inside project root)
;;;   - Extension tool is blocked in safe mode
;;;   - Safe mode inactive — all tools and paths allowed
;;;   - Multiple tool calls in batch — some blocked, some allowed

(require rackunit
         (only-in "../tools/tool.rkt"
                  tool? tool-name tool-schema tool-execute
                  make-tool make-tool-registry make-exec-context
                  register-tool! lookup-tool
                  tool-result? tool-result-content
                  tool-result-details tool-result-is-error?
                  make-error-result make-success-result)
         (only-in "../tools/scheduler.rkt"
                  run-tool-batch scheduler-result
                  scheduler-result-results scheduler-result-metadata)
         (only-in "../util/protocol-types.rkt"
                  tool-call tool-call?
                  tool-call-id tool-call-name tool-call-arguments)
         (only-in "../runtime/safe-mode.rkt"
                  current-safe-mode project-root)
         (only-in "../util/safe-mode-predicates.rkt"
                  safe-mode? allowed-tool? allowed-path?
                  safe-mode-project-root trust-level blocked-tools))

;; ============================================================
;; Helper: build a registry with various tool types
;; ============================================================

(define (make-test-registry)
  (define reg (make-tool-registry))

  ;; read tool — allowed in safe mode, takes 'path
  (register-tool!
   reg
   (make-tool "read"
              "Read tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "read: ~a" (hash-ref args 'path))))))))

  ;; ls tool — allowed in safe mode, takes 'path
  (register-tool!
   reg
   (make-tool "ls"
              "List tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "ls: ~a" (hash-ref args 'path))))))))

  ;; find tool — allowed in safe mode, takes 'path
  (register-tool!
   reg
   (make-tool "find"
              "Find tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "find: ~a" (hash-ref args 'path))))))))

  ;; grep tool — allowed in safe mode, takes 'path
  (register-tool!
   reg
   (make-tool "grep"
              "Grep tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "grep: ~a" (hash-ref args 'path))))))))

  ;; bash tool — BLOCKED in safe mode
  (register-tool!
   reg
   (make-tool "bash"
              "Bash tool"
              (hasheq 'type "object"
                      'properties (hasheq 'command (hasheq 'type "string"))
                      'required '("command"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "bash: ~a" (hash-ref args 'command))))))))

  ;; edit tool — BLOCKED in safe mode, takes 'path
  (register-tool!
   reg
   (make-tool "edit"
              "Edit tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "edit: ~a" (hash-ref args 'path))))))))

  ;; write tool — BLOCKED in safe mode, takes 'path
  (register-tool!
   reg
   (make-tool "write"
              "Write tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "write: ~a" (hash-ref args 'path))))))))

  ;; firecrawl tool — BLOCKED in safe mode
  (register-tool!
   reg
   (make-tool "firecrawl"
              "Firecrawl tool"
              (hasheq 'type "object"
                      'properties (hasheq 'url (hasheq 'type "string"))
                      'required '("url"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "firecrawl: ~a" (hash-ref args 'url))))))))

  ;; extension:custom tool — BLOCKED in safe mode
  (register-tool!
   reg
   (make-tool "extension:custom"
              "Extension custom tool"
              (hasheq 'type "object"
                      'properties (hasheq 'path (hasheq 'type "string"))
                      'required '("path"))
              (lambda (args ctx)
                (make-success-result
                 (list (hasheq 'type "text"
                               'text (format "ext: ~a" (hash-ref args 'path))))))))

  reg)

;; Helper: extract text from a tool-result's content
(define (result-text tr)
  (define content (tool-result-content tr))
  (if (and (list? content) (pair? content))
      (hash-ref (car content) 'text "")
      ""))

;; Helper: test project root
(define test-project-root (build-path "/tmp/test-scheduler-safe-mode"))

;; ============================================================
;; 1. Blocked tools are blocked in safe mode
;; ============================================================

(test-case
 "bash tool blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "bash" (hasheq 'command "ls"))))
   (define sr (run-tool-batch tcs reg))
   (define results (scheduler-result-results sr))
   (check-equal? (length results) 1)
   (check-true (tool-result-is-error? (first results)))
   (check-true (string-contains? (result-text (first results)) "blocked by safe-mode"))))

(test-case
 "edit tool blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "edit" (hasheq 'path "/tmp/test-scheduler-safe-mode/foo.rkt"))))
   (define sr (run-tool-batch tcs reg))
   (check-true (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "blocked by safe-mode"))))

(test-case
 "write tool blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "write" (hasheq 'path "/tmp/test-scheduler-safe-mode/foo.rkt"))))
   (define sr (run-tool-batch tcs reg))
   (check-true (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "blocked by safe-mode"))))

(test-case
 "firecrawl tool blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "firecrawl" (hasheq 'url "https://example.com"))))
   (define sr (run-tool-batch tcs reg))
   (check-true (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "blocked by safe-mode"))))

;; ============================================================
;; 2. Allowed tools work in safe mode (with path inside root)
;; ============================================================

(test-case
 "read tool allowed in safe mode with path inside root"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "read" (hasheq 'path "/tmp/test-scheduler-safe-mode/src/foo.rkt"))))
   (define sr (run-tool-batch tcs reg))
   (check-false (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "read:"))))

(test-case
 "ls tool allowed in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "ls" (hasheq 'path "/tmp/test-scheduler-safe-mode/"))))
   (define sr (run-tool-batch tcs reg))
   (check-false (tool-result-is-error? (first (scheduler-result-results sr))))))

(test-case
 "find tool allowed in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "find" (hasheq 'path "/tmp/test-scheduler-safe-mode/"))))
   (define sr (run-tool-batch tcs reg))
   (check-false (tool-result-is-error? (first (scheduler-result-results sr))))))

(test-case
 "grep tool allowed in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "grep" (hasheq 'path "/tmp/test-scheduler-safe-mode/"))))
   (define sr (run-tool-batch tcs reg))
   (check-false (tool-result-is-error? (first (scheduler-result-results sr))))))

;; ============================================================
;; 3. Path is blocked in safe mode (outside project root)
;; ============================================================

(test-case
 "read tool with path outside project root blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "read" (hasheq 'path "/etc/passwd"))))
   (define sr (run-tool-batch tcs reg))
   (check-true (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "Access denied"))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "outside project root"))))

(test-case
 "ls tool with path outside project root blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "ls" (hasheq 'path "/etc"))))
   (define sr (run-tool-batch tcs reg))
   (check-true (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "Access denied"))))

;; ============================================================
;; 4. Path is allowed in safe mode (inside project root)
;; ============================================================

(test-case
 "read tool with path inside project root allowed in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "read" (hasheq 'path "/tmp/test-scheduler-safe-mode/src/bar.rkt"))))
   (define sr (run-tool-batch tcs reg))
   (check-false (tool-result-is-error? (first (scheduler-result-results sr))))))

;; ============================================================
;; 5. Extension tool blocked in safe mode
;; ============================================================

(test-case
 "extension:custom tool blocked in safe mode"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "extension:custom" (hasheq 'path "/tmp/test-scheduler-safe-mode/foo"))))
   (define sr (run-tool-batch tcs reg))
   (check-true (tool-result-is-error? (first (scheduler-result-results sr))))
   (check-true (string-contains? (result-text (first (scheduler-result-results sr))) "blocked by safe-mode"))))

;; ============================================================
;; 6. Safe mode inactive — all tools and paths allowed
;; ============================================================

(test-case
 "all tools allowed when safe mode inactive"
 (parameterize ([current-safe-mode #f])
   (define reg (make-test-registry))
   (define tcs (list (tool-call "tc-1" "bash" (hasheq 'command "ls"))
                     (tool-call "tc-2" "read" (hasheq 'path "/etc/passwd"))
                     (tool-call "tc-3" "edit" (hasheq 'path "/tmp/foo"))
                     (tool-call "tc-4" "firecrawl" (hasheq 'url "https://example.com"))))
   (define sr (run-tool-batch tcs reg))
   (define results (scheduler-result-results sr))
   (for ([r (in-list results)])
     (check-false (tool-result-is-error? r) "tool should succeed when safe mode off"))))

;; ============================================================
;; 7. Multiple tool calls in batch — some blocked, some allowed
;; ============================================================

(test-case
 "mixed batch in safe mode: blocked tools and paths produce errors"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (define reg (make-test-registry))
   ;; bash → blocked tool, read inside root → ok, read outside root → path blocked,
   ;; grep inside root → ok, edit → blocked tool
   (define tcs (list (tool-call "tc-1" "bash" (hasheq 'command "ls"))
                     (tool-call "tc-2" "read" (hasheq 'path "/tmp/test-scheduler-safe-mode/foo.rkt"))
                     (tool-call "tc-3" "read" (hasheq 'path "/etc/passwd"))
                     (tool-call "tc-4" "grep" (hasheq 'path "/tmp/test-scheduler-safe-mode/"))
                     (tool-call "tc-5" "edit" (hasheq 'path "/tmp/test-scheduler-safe-mode/bar.rkt"))))
   (define sr (run-tool-batch tcs reg))
   (define results (scheduler-result-results sr))
   (check-equal? (length results) 5)
   ;; tc-1: bash → blocked tool
   (check-true (tool-result-is-error? (first results)) "bash blocked")
   (check-true (string-contains? (result-text (first results)) "blocked by safe-mode"))
   ;; tc-2: read inside root → ok
   (check-false (tool-result-is-error? (second results)) "read inside root ok")
   ;; tc-3: read outside root → path blocked
   (check-true (tool-result-is-error? (third results)) "read outside root blocked")
   (check-true (string-contains? (result-text (third results)) "Access denied"))
   ;; tc-4: grep inside root → ok
   (check-false (tool-result-is-error? (fourth results)) "grep inside root ok")
   ;; tc-5: edit → blocked tool
   (check-true (tool-result-is-error? (fifth results)) "edit blocked")
   (check-true (string-contains? (result-text (fifth results)) "blocked by safe-mode"))
   ;; Check metadata
   (define meta (scheduler-result-metadata sr))
   (check-equal? (hash-ref meta 'total) 5)
   (check-equal? (hash-ref meta 'blocked) 3)))  ;; bash + path-blocked read + edit

;; ============================================================
;; 8. Predicates re-exported correctly from util/safe-mode-predicates
;; ============================================================

(test-case
 "safe-mode? predicate works from util import"
 (parameterize ([current-safe-mode #t])
   (check-true (safe-mode?)))
 (parameterize ([current-safe-mode #f])
   (check-false (safe-mode?))))

(test-case
 "allowed-tool? predicate works from util import"
 (parameterize ([current-safe-mode #t])
   (check-false (allowed-tool? "bash"))
   (check-true (allowed-tool? "read"))))

(test-case
 "allowed-path? predicate works from util import"
 (parameterize ([current-safe-mode #t]
                [project-root test-project-root])
   (check-false (allowed-path? "/etc/passwd"))
   (check-true (allowed-path? "/tmp/test-scheduler-safe-mode/foo.rkt"))))

(test-case
 "blocked-tools constant available"
 (check-equal? blocked-tools '("bash" "edit" "write" "firecrawl")))

(test-case
 "trust-level predicate works from util import"
 (parameterize ([current-safe-mode #t])
   (check-equal? (trust-level) 'restricted))
 (parameterize ([current-safe-mode #f])
   (check-equal? (trust-level) 'full)))

;; ============================================================
;; Summary
;; ============================================================

(displayln "All scheduler safe-mode tests passed.")
