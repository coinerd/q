#lang racket

;; @speed fast  ;; @suite runtime
;; @boundary integration

;; BOUNDARY: integration

;; test-tool-coordinator-cwd.rkt — BUG-4 regression tests
;; Validates that tool execution CWD prefers project dir over session dir.
;; Tests via bash tool which respects exec-ctx working-directory.
;;
;; R97 (BUG-0097) additions: scheduler-bound find -> read roundtrip with
;; CONFLICTING cwd/contracts files — the actual scheduler find must return
;; absolute paths whose read returns the docs sentinel bytes, even when the
;; process CWD and exec-context working directory differ (never just direct
;; handler calls).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-call
                  make-success-result
                  tool-result-content
                  tool-result-is-error?)
         (only-in "../tools/exec-context.rkt" make-exec-context)
         (only-in "../tools/permission-gate.rkt" make-permissive-permission-config)
         (only-in "../tools/registry.rkt" make-tool-registry register-tool!)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result-results)
         (only-in "../tools/builtins/bash.rkt" tool-bash)
         (only-in "../tools/builtins/find.rkt" tool-find)
         (only-in "../tools/builtins/read.rkt" tool-read)
         "helpers/fixtures.rkt")

(define (result-content-text r)
  (define parts (tool-result-content r))
  (string-join (for/list ([part (in-list parts)])
                 (cond
                   [(string? part) part]
                   [(hash? part) (hash-ref part 'text "")]
                   [else (~a part)]))
               ""))

        (define (result-content-strings r)
          (for/list ([part (in-list (tool-result-content r))])
            (cond
              [(string? part) part]
              [(hash? part) (hash-ref part 'text "")]
              [else (~a part)])))

(define (make-bash-tool)
  (make-tool "bash"
             "Execute a shell command"
             (hasheq 'type
                     "object"
                     'required
                     '("command")
                     'properties
                     (hasheq 'command
                             (hasheq 'type "string" 'description "Shell command to run")
                             'working-directory
                             (hasheq 'type "string" 'description "Explicit command directory")))
             tool-bash))

(define (make-find-tool)
  (make-tool "find"
             "Find files and directories by name or type"
             (hasheq 'type
                     "object"
                     'required
                     '("path")
                     'properties
                     (hasheq 'path (hasheq 'type "string" 'description "Root directory")
                             'name (hasheq 'type "string" 'description "Glob")))
             tool-find))

(define (make-real-read-tool)
  (make-tool "read"
             "Read file contents"
             (hasheq 'type
                     "object"
                     'required
                     '("path")
                     'properties
                     (hasheq 'path (hasheq 'type "string" 'description "File to read")))
             tool-read))

(define coordinator-cwd-tests
  (test-suite "tool-coordinator-cwd"

    (test-case "bash tool uses exec-ctx working-directory"
      (with-temp-dir
       (lambda (dir)
         ;; Create a unique file in temp dir
         (call-with-output-file (build-path dir "unique-marker-cwd.txt")
                                (lambda (out) (display "test" out))
                                #:exists 'replace)
         ;; Register bash tool
         (define reg (make-tool-registry))
         (register-tool! reg (make-bash-tool))
         ;; Create exec-ctx pointing to our temp dir
         (define ctx
           (make-exec-context #:working-directory dir
                              #:permission-config (make-permissive-permission-config)))
         ;; Run ls via bash — should list from our temp dir
         (define tc (make-tool-call "bash-1" "bash" (hasheq 'command "ls unique-marker-cwd.txt")))
         (define result (run-tool-batch (list tc) reg #:exec-context ctx))
         (define results (scheduler-result-results result))
         (define content (result-content-text (car results)))
         (check-true (string-contains? content "unique-marker-cwd.txt")
                     (format "Expected 'unique-marker-cwd.txt' in bash ls output, got: ~a"
                             content)))))

    (test-case "explicit invocation cwd takes precedence over exec-context fallback"
      (with-temp-dir
       (lambda (outer)
         (define inner (build-path outer "directory with spaces"))
         (make-directory inner)
         (call-with-output-file (build-path inner "cwd.sentinel")
                                (lambda (out) (display "explicit-cwd-honored" out)))
         (define reg (make-tool-registry))
         (register-tool! reg (make-bash-tool))
         (define ctx
           (make-exec-context #:working-directory outer
                              #:permission-config (make-permissive-permission-config)))
         (define tc
           (make-tool-call
            "bash-explicit-cwd"
            "bash"
            (hasheq 'command "pwd && cat cwd.sentinel" 'working-directory (path->string inner))))
         (define result (run-tool-batch (list tc) reg #:exec-context ctx))
         (define tool-result (car (scheduler-result-results result)))
         (define content (result-content-text tool-result))
         (check-false (tool-result-is-error? tool-result) content)
         (check-true (string-contains? content (path->string inner)) content)
         (check-true (string-contains? content "explicit-cwd-honored") content))))

    (test-case "bash tool with current-directory succeeds"
      (define reg (make-tool-registry))
      (register-tool! reg (make-bash-tool))
      (define ctx
        (make-exec-context #:working-directory (current-directory)
                           #:permission-config (make-permissive-permission-config)))
      (define tc (make-tool-call "bash-2" "bash" (hasheq 'command "echo ok")))
      (define result (run-tool-batch (list tc) reg #:exec-context ctx))
      (define results (scheduler-result-results result))
      (check-false (tool-result-is-error? (car results))))

    ;; ============================================================
    ;; R97 (BUG-0097): scheduler-bound find -> read roundtrip with
    ;; CONFLICTING cwd/contracts files. Actual scheduler execution (never
    ;; direct handler calls), with differing process/exec-context CWDs.
    ;; ============================================================

    (test-case "R97: scheduler find->read returns docs sentinel with conflicting cwd/contracts"
      (with-temp-dir
       (lambda (root)
         ;; Canonical docs/contracts fixture ...
         (make-directory* (build-path root "docs" "contracts"))
         (call-with-output-file (build-path root "docs" "contracts" "spec.txt")
                                (lambda (out) (display "docs-contracts-sentinel" out))
                                #:exists 'replace)
         ;; ... plus a CONFLICTING cwd/contracts file with a different sentinel.
         (make-directory* (build-path root "contracts"))
         (call-with-output-file (build-path root "contracts" "spec.txt")
                                (lambda (out) (display "cwd-conflict-sentinel" out))
                                #:exists 'replace)
         (define reg (make-tool-registry))
         (register-tool! reg (make-find-tool))
         (register-tool! reg (make-real-read-tool))
         ;; exec-context working directory deliberately differs from root
         (define unrelated (make-temporary-file "q97-unrelated-~a" 'directory))
         (dynamic-wind
           (lambda () (current-directory root))
           (lambda ()
             (define ctx
               (make-exec-context
                #:working-directory unrelated
                #:permission-config (make-permissive-permission-config)))
             ;; find via the ACTUAL scheduler with a relative docs root
             (define find-call (make-tool-call "tc-find" "find" (hasheq 'path "docs")))
             (define find-result
               (car (scheduler-result-results
                     (run-tool-batch (list find-call) reg #:exec-context ctx))))
             (check-false (tool-result-is-error? find-result)
                          (result-content-text find-result))
             (define found
               (findf (lambda (s) (string-suffix? s "docs/contracts/spec.txt"))
                      (result-content-strings find-result)))
             ;; R97: result must be an absolute path ...
             (check-not-false found (format "expected absolute docs result, got: ~a"
                                            (result-content-strings find-result)))
             (when found
               (check-true (string-prefix? found (path->string root))
                           (format "result must live under the searched root: ~a" found))
               ;; ... whose read returns the DOCS sentinel bytes, never the
               ;; cwd-conflict sentinel from root/contracts/spec.txt.
               (define read-call (make-tool-call "tc-read" "read" (hasheq 'path found)))
               (define read-result
                 (car (scheduler-result-results
                       (run-tool-batch (list read-call) reg #:exec-context ctx))))
               (check-false (tool-result-is-error? read-result)
                            (result-content-text read-result))
               (check-true (string-contains? (result-content-text read-result)
                                             "docs-contracts-sentinel")
                           (format "expected docs sentinel, got: ~a"
                                   (result-content-text read-result)))
               (check-false (string-contains? (result-content-text read-result)
                                              "cwd-conflict-sentinel"))))
           (lambda ()
             (delete-directory/files unrelated #:must-exist? #f)))))

    ;; CWD switched BETWEEN find and read, both through the scheduler:
    ;; absolute results must resolve independent of CWD state changes.
    (test-case "R97: CWD switched between scheduler find and scheduler read"
      (with-temp-dir
       (lambda (root)
         (make-directory* (build-path root "docs" "contracts"))
         (call-with-output-file (build-path root "docs" "contracts" "spec.txt")
                                (lambda (out) (display "docs-between-sentinel" out))
                                #:exists 'replace)
         (define reg (make-tool-registry))
         (register-tool! reg (make-find-tool))
         (register-tool! reg (make-real-read-tool))
         (define ctx (make-exec-context
                      #:permission-config (make-permissive-permission-config)))
         (define orig-cd (current-directory))
         (dynamic-wind
           (lambda () (current-directory root))
           (lambda ()
             (define find-result
               (car (scheduler-result-results
                     (run-tool-batch
                      (list (make-tool-call "tc-find" "find" (hasheq 'path "docs")))
                      reg #:exec-context ctx))))
             (check-false (tool-result-is-error? find-result))
             (define found
               (findf (lambda (s) (string-suffix? s "docs/contracts/spec.txt"))
                      (result-content-strings find-result)))
             (check-not-false found)
             ;; now switch the process CWD to a hostile decoy layout
             (define decoy (make-temporary-file "q97-decoy-~a" 'directory))
             (dynamic-wind
               (lambda ()
                 (make-directory* (build-path decoy "docs" "contracts"))
                 (call-with-output-file (build-path decoy "docs" "contracts" "spec.txt")
                                        (lambda (out) (display "decoy-sentinel" out))
                                        #:exists 'replace)
                 (current-directory decoy))
               (lambda ()
                 (define read-result
                   (car (scheduler-result-results
                         (run-tool-batch
                          (list (make-tool-call "tc-read" "read"
                                                (hasheq 'path found)))
                          reg #:exec-context ctx))))
                 (check-false (tool-result-is-error? read-result)
                              (result-content-text read-result))
                 (check-true (string-contains? (result-content-text read-result)
                                               "docs-between-sentinel")
                             (format "expected docs sentinel after CWD switch, got: ~a"
                                     (result-content-text read-result)))
                 (check-false (string-contains? (result-content-text read-result)
                                                "decoy-sentinel")))
               (lambda ()
                 (current-directory orig-cd)
                 (delete-directory/files decoy #:must-exist? #f)))))))))
    ))

(run-tests coordinator-cwd-tests)
