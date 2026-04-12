#lang racket

;; tests/workflows/safety/test-safe-mode-boundary.rkt — Safe-mode boundary workflow test (#176)
;;
;; Verifies that out-of-bounds file operations are blocked by mock tools
;; simulating safe-mode enforcement. Tests outcome (tool errors),
;; side-effects (events), and durable state (session log records).

(require rackunit
         rackunit/text-ui
         json
         racket/file
         (prefix-in sdk: "../../../interfaces/sdk.rkt")
         "../../../agent/event-bus.rkt"
         "../../../agent/types.rkt"
         (only-in "../../../tools/tool.rkt"
                  make-tool-registry register-tool!
                  make-tool make-error-result make-success-result)
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (absolute-path? s)
  (and (string? s)
       (> (string-length s) 0)
       (char=? (string-ref s 0) #\/)))

(define (string-prefix? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))

;; Safe project setup: use make-temp-project '() then write files manually
(define (setup-bounded-project files)
  (define-values (project-dir session-dir) (make-temp-project '()))
  (for ([f (in-list files)])
    (define full-path (build-path project-dir (car f)))
    (make-directory* (path-only full-path))
    (call-with-output-file full-path
                           (lambda (out) (display (cdr f) out))
                           #:exists 'replace))
  (values project-dir session-dir))

;; ============================================================
;; Boundary-enforcing mock tools
;; ============================================================

(define (make-bounded-read-tool project-root)
  (make-tool
   "read"
   "Read a file within the project"
   (hasheq 'type "object"
           'properties (hasheq 'path (hasheq 'type "string"))
           'required '("path"))
   (lambda (args ctx)
     (define path (hash-ref args 'path (hash-ref args "path" #f)))
     (cond
       [(not path)
        (make-error-result "missing path argument")]
       [(and (string? path)
             (or (not (absolute-path? path))
                 (string-prefix? path (path->string project-root))))
        (make-success-result
         (list (hasheq 'type "text" 'text (format "Contents of ~a" path))))]
       [else
        (make-error-result
         (format "SAFE-MODE: path '~a' is outside project root '~a'"
                 path (path->string project-root)))]))))

(define (make-bounded-write-tool project-root)
  (make-tool
   "write"
   "Write a file within the project"
   (hasheq 'type "object"
           'properties (hasheq 'path (hasheq 'type "string")
                               'content (hasheq 'type "string"))
           'required '("path" "content"))
   (lambda (args ctx)
     (define path (hash-ref args 'path (hash-ref args "path" #f)))
     (cond
       [(not path)
        (make-error-result "missing path argument")]
       [(and (string? path)
             (or (not (absolute-path? path))
                 (string-prefix? path (path->string project-root))))
        (make-success-result
         (list (hasheq 'type "text" 'text (format "Wrote to ~a" path))))]
       [else
        (make-error-result
         (format "SAFE-MODE: path '~a' is outside project root '~a'"
                 path (path->string project-root)))]))))

;; ============================================================
;; Assertion helpers
;; ============================================================

(define (has-error-result? msg)
  (for/or ([part (in-list (message-content msg))])
    (and (tool-result-part? part)
         (tool-result-part-is-error? part))))

(define (has-success-result? msg)
  (for/or ([part (in-list (message-content msg))])
    (and (tool-result-part? part)
         (not (tool-result-part-is-error? part)))))

;; ============================================================
;; Test suite
;; ============================================================

(define safe-mode-boundary-tests
  (test-suite
   "Safe-Mode Boundary Workflow"

   ;; ── Outcome: out-of-bounds read is rejected ──
   (test-case "out-of-bounds read tool-call returns error result"

     (define-values (project-dir session-dir)
       (setup-bounded-project (list (cons "hello.rkt" "#lang racket"))))

     (with-handlers ([exn:fail? (lambda (e)
                                  (cleanup-temp-project! project-dir session-dir)
                                  (raise e))])
       (define reg (make-tool-registry))
       (register-tool! reg (make-bounded-read-tool project-dir))

       ;; Provider script: attempt to read /etc/passwd (out of bounds)
       (define provider
         (make-scripted-provider
          (list (tool-call-response "tc-1" "read"
                                    (hasheq 'path "/etc/passwd"))
                (text-response "I tried to read the file"))))

       (define bus (make-event-bus))
       (define recorder (make-event-recorder bus))

       (define rt
         (sdk:make-runtime #:provider provider
                           #:session-dir session-dir
                           #:tool-registry reg
                           #:event-bus bus
                           #:max-iterations 10))
       (define rt-open (sdk:open-session rt))
       (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

       (define-values (rt-final result) (sdk:run-prompt! rt-open "Read the password file"))

       (define log-path (build-path session-dir sid "session.jsonl"))
       (define entries (session-log-entries log-path))

       ;; ── Outcome: session log is valid ──
       (check-eq? (check-session-jsonl-valid log-path) #t
                  "Session log should be valid JSONL")

       ;; ── Outcome: tool results contain errors for out-of-bounds ──
       (define tool-result-entries (entries-with-role entries 'tool))
       (check-equal? (length tool-result-entries) 1
                     "Should have 1 tool-result entry")
       (when (= (length tool-result-entries) 1)
         (check-true (has-error-result? (car tool-result-entries))
                     "Tool result should be an error"))

       ;; ── Durable state: session log records tool-call + error result ──
       (check >= (length (entries-with-role entries 'assistant)) 1
              "Should have at least 1 assistant entry")

       ;; ── Side-effects: tool.call events emitted ──
       (define names (event-names recorder))
       (check-not-false (or (member "tool.call.completed" names)
                            (member "tool.call.failed" names))
                        "Should emit tool.call event for read attempt")

       (cleanup-temp-project! project-dir session-dir)))

   ;; ── Outcome: out-of-bounds write is rejected ──
   (test-case "out-of-bounds write tool-call returns error result"

     (define-values (project-dir session-dir)
       (setup-bounded-project (list (cons "main.rkt" "#lang racket"))))

     (with-handlers ([exn:fail? (lambda (e)
                                  (cleanup-temp-project! project-dir session-dir)
                                  (raise e))])
       (define reg (make-tool-registry))
       (register-tool! reg (make-bounded-write-tool project-dir))

       ;; Provider script: attempt to write /tmp/malicious.rkt
       (define provider
         (make-scripted-provider
          (list (tool-call-response "tc-1" "write"
                                    (hasheq 'path "/tmp/malicious.rkt"
                                            'content "(rm -rf /)"))
                (text-response "I tried to write outside the project"))))

       (define bus (make-event-bus))
       (define recorder (make-event-recorder bus))

       (define rt
         (sdk:make-runtime #:provider provider
                           #:session-dir session-dir
                           #:tool-registry reg
                           #:event-bus bus
                           #:max-iterations 10))
       (define rt-open (sdk:open-session rt))
       (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

       (define-values (rt-final result) (sdk:run-prompt! rt-open "Write a malicious file"))

       (define log-path (build-path session-dir sid "session.jsonl"))
       (define entries (session-log-entries log-path))

       ;; ── Outcome: tool result is an error ──
       (define tool-result-entries (entries-with-role entries 'tool))
       (check-equal? (length tool-result-entries) 1
                     "Should have 1 tool-result entry")
       (when (= (length tool-result-entries) 1)
         (check-true (has-error-result? (car tool-result-entries))
                     "Write tool result should be an error"))

       ;; ── Side-effects: events for tool call ──
       (define names (event-names recorder))
       (check-not-false (or (member "tool.call.completed" names)
                            (member "tool.call.failed" names))
                        "Should emit tool.call event for write attempt")

       (cleanup-temp-project! project-dir session-dir)))

   ;; ── Outcome: in-bounds read succeeds while out-of-bounds fails ──
   (test-case "in-bounds access succeeds while out-of-bounds fails"

     (define-values (project-dir session-dir)
       (setup-bounded-project (list (cons "safe.txt" "safe content"))))

     (with-handlers ([exn:fail? (lambda (e)
                                  (cleanup-temp-project! project-dir session-dir)
                                  (raise e))])
       (define reg (make-tool-registry))
       (register-tool! reg (make-bounded-read-tool project-dir))

       ;; Provider: first read inside project (success), then read outside (error)
       (define provider
         (make-scripted-provider
          (list (tool-call-response "tc-1" "read"
                                    (hasheq 'path "safe.txt"))
                (tool-call-response "tc-2" "read"
                                    (hasheq 'path "/etc/shadow"))
                (text-response "Done"))))

       (define bus (make-event-bus))
       (define recorder (make-event-recorder bus))

       (define rt
         (sdk:make-runtime #:provider provider
                           #:session-dir session-dir
                           #:tool-registry reg
                           #:event-bus bus
                           #:max-iterations 10))
       (define rt-open (sdk:open-session rt))
       (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

       (define-values (rt-final result) (sdk:run-prompt! rt-open "Read safe then shadow"))

       (define log-path (build-path session-dir sid "session.jsonl"))
       (define entries (session-log-entries log-path))

       (check-eq? (check-session-jsonl-valid log-path) #t
                  "Session log should be valid")

       ;; 2 tool-result entries: first success, second error
       (define tool-result-entries (entries-with-role entries 'tool))
       (check >= (length tool-result-entries) 2
              "Should have at least 2 tool-result entries")

       (when (>= (length tool-result-entries) 2)
         ;; First result: NOT an error (in-bounds read)
         (check-not-false (has-success-result? (car tool-result-entries))
                          "First read (in-bounds) should succeed")
         ;; Second result: IS an error (out-of-bounds read)
         (check-true (has-error-result? (cadr tool-result-entries))
                     "Second read (out-of-bounds) should fail"))

       ;; ── Side-effects: events for both tool calls ──
       (define names (event-names recorder))
       (define tool-events
         (filter (lambda (n) (or (string=? n "tool.call.completed")
                                 (string=? n "tool.call.failed")))
                 names))
       (check >= (length tool-events) 2
              "Should have events for both tool calls")

       (cleanup-temp-project! project-dir session-dir)))))

;; ============================================================
;; Run
;; ============================================================

(module+ main
  (run-tests safe-mode-boundary-tests))

(module+ test
  (run-tests safe-mode-boundary-tests))
