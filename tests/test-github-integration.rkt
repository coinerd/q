#lang racket

;; @speed slow
;; @suite default

;; BOUNDARY: integration

;; tests/test-github-integration.rkt — tests for GitHub integration extension
;;
;; Covers:
;;   - Extension loads and registers 6 tools
;;   - gh-binary-path parameter for test injection
;;   - Shell quoting
;;   - gh-unavailable when gh not found
;;   - Mock gh CLI: issue CRUD, PR CRUD, milestone ops, board ops
;;   - Wave start/finish flow
;;   - Error handling for malformed args

(require rackunit
         racket/file
         racket/port
         racket/string
         json
         "../extensions/github-integration.rkt"
         "../extensions/github/tool-schemas.rkt"
         "../extensions/context.rkt"
         "../extensions/dynamic-tools.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../tools/tool.rkt"
         "../util/event/event-bus.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (result-text result)
  (string-join (for/list ([item (in-list (tool-result-content result))]
                          #:when (hash? item))
                 (hash-ref item 'text ""))))

(define (make-test-ctx #:tool-registry [reg #f])
  (make-extension-ctx #:session-id "test-gh"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:tool-registry (or reg (make-tool-registry))
                      #:command-registry (box (hash))))

;; Create a temporary mock gh script
(define (make-mock-gh-script responses)
  ;; responses: hash from command-pattern -> exit-code + stdout + stderr
  (define tmp-dir (make-temporary-file "mock-gh-~a" 'directory))
  (define script-path (build-path tmp-dir "gh"))
  (define response-json (jsexpr->string responses))
  (call-with-output-file script-path
                         (lambda (out)
                           (displayln "#!/bin/sh" out)
                           (displayln (format "echo '~a'" (shell-quote response-json)) out)
                           (displayln "exit 0" out)))
  (file-or-directory-permissions script-path #o755)
  tmp-dir)

(define (cleanup-mock-gh dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; Create a mock gh binary that echoes JSON responses
(define (make-simple-mock-gh response-text)
  (define tmp-dir (make-temporary-file "mock-gh-~a" 'directory))
  (define script-path (build-path tmp-dir "gh"))
  (call-with-output-file
   script-path
   (lambda (out)
     (displayln "#!/bin/sh" out)
     ;; Capture arguments to decide what to output
     (displayln "case \"$*\" in" out)
     (displayln "  *issue*create*)" out)
     (displayln
      "    echo '{\"number\":42,\"title\":\"test\",\"url\":\"https://github.com/test/test/issues/42\"}'"
      out)
     (displayln "    ;;" out)
     (displayln "  *issue*close*)" out)
     (displayln "    echo 'Closed'" out)
     (displayln "    ;;" out)
     (displayln "  *issue*view*)" out)
     (displayln "    echo '{\"number\":1,\"title\":\"Test\",\"state\":\"open\",\"body\":\"body\"}'"
                out)
     (displayln "    ;;" out)
     (displayln "  *issue*edit*)" out)
     (displayln "    echo '{\"number\":1,\"title\":\"Updated\",\"state\":\"open\"}'" out)
     (displayln "    ;;" out)
     (displayln "  *issue*list*)" out)
     (displayln "    echo '[{\"number\":1,\"title\":\"Test\",\"state\":\"open\"}]'" out)
     (displayln "    ;;" out)
     (displayln "  *pr*create*)" out)
     (displayln
      "    echo '{\"number\":7,\"title\":\"PR\",\"url\":\"https://github.com/test/test/pull/7\"}'"
      out)
     (displayln "    ;;" out)
     (displayln "  *pr*merge*)" out)
     (displayln "    echo '{\"url\":\"https://github.com/test/test/pull/7\"}'" out)
     (displayln "    ;;" out)
     (displayln "  *pr*list*)" out)
     (displayln "    echo '[{\"number\":7,\"title\":\"PR\",\"state\":\"open\"}]'" out)
     (displayln "    ;;" out)
     (displayln "  *pr*view*)" out)
     (displayln "    echo '{\"number\":7,\"title\":\"PR\",\"state\":\"open\"}'" out)
     (displayln "    ;;" out)
     (displayln "  *api*milestones*POST*)" out)
     (displayln "    echo '{\"number\":1,\"title\":\"v1\",\"state\":\"open\"}'" out)
     (displayln "    ;;" out)
     (displayln "  *api*milestones*PATCH*)" out)
     (displayln "    echo '{\"number\":1,\"title\":\"v1\",\"state\":\"closed\"}'" out)
     (displayln "    ;;" out)
     (displayln "  *api*milestones*)" out)
     (displayln "    echo '[{\"number\":1,\"title\":\"v1\",\"state\":\"open\"}]'" out)
     (displayln "    ;;" out)
     (displayln "  *)" out)
     (displayln (format "    echo '~a'" response-text) out)
     (displayln "    ;;" out)
     (displayln "esac" out)
     (displayln "exit 0" out)))
  (file-or-directory-permissions script-path #o755)
  tmp-dir)

;; ============================================================
;; Test: Shell quoting
;; ============================================================

(check-equal? (shell-quote "hello") "'hello'" "shell-quote wraps in single quotes")
(check-equal? (shell-quote "it's") "'it'\\''s'" "shell-quote escapes single quotes")
(check-equal? (shell-quote 42) "'42'" "shell-quote converts numbers to strings")

;; ============================================================
;; Test: gh-binary-path parameter
;; ============================================================

(check-false (gh-binary-path) "gh-binary-path defaults to #f")

;; ============================================================
;; Test: gh-unavailable error (via tool handler directly)
;; ============================================================

(let ()
  (parameterize ([gh-binary-path 'disabled])
    (define result (handle-gh-issue (hasheq 'action "create" 'title "Test")))
    (check-pred tool-result? result "gh-unavailable returns tool-result")
    (check-true (tool-result-is-error? result) "gh-unavailable is error")))

;; ============================================================
;; Test: Extension registers 6 tools
;; ============================================================

(let ()
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  ;; Register extension tools
  (register-github-tools ctx (hasheq))
  ;; Check all 6 tools are registered
  (for ([name '("gh-issue" "gh-pr" "gh-milestone" "gh-board" "gh-wave-start" "gh-wave-finish")])
    (check-not-false (lookup-tool reg name) (format "Tool ~a should be registered" name)))
  (let ([finish-tool (lookup-tool reg "gh-wave-finish")])
    (check-true (string-contains? (tool-description finish-tool) "always fails before mutation"))
    (check-true (string-contains? (tool-description finish-tool)
                                  "external authenticated PR workflow"))
    (check-true (string-contains? (tool-prompt-guidelines finish-tool)
                                  "always fails before mutation"))
    (check-true (string-contains? (tool-prompt-guidelines finish-tool)
                                  "external authenticated PR workflow"))))

;; ============================================================
;; Test: gh-issue handler with mock gh (create)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    ;; Create issue
    (define result (handle-gh-issue (hasheq 'action "create" 'title "Test issue" 'body "Test body")))
    (check-pred tool-result? result "gh-issue create returns tool-result")
    (check-false (tool-result-is-error? result) "gh-issue create succeeds")
    ;; Verify response contains issue number
    (define content (tool-result-content result))
    (define text (hash-ref (car content) 'text ""))
    (check-not-false (string-contains? text "42") "Created issue #42"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue handler with mock gh (close)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "close" 'number 42)))
    (check-pred tool-result? result "gh-issue close returns tool-result")
    (check-false (tool-result-is-error? result) "gh-issue close succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue handler with mock gh (list)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "list" 'state "open")))
    (check-pred tool-result? result "gh-issue list returns tool-result")
    (check-false (tool-result-is-error? result) "gh-issue list succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue handler with mock gh (get)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "get" 'number 1)))
    (check-pred tool-result? result "gh-issue get returns tool-result")
    (check-false (tool-result-is-error? result) "gh-issue get succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue handler with missing action
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq)))
    (check-pred tool-result? result "missing action returns tool-result")
    (check-true (tool-result-is-error? result) "missing action is error"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue handler with unknown action
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "nonexistent")))
    (check-pred tool-result? result "unknown action returns tool-result")
    (check-true (tool-result-is-error? result) "unknown action is error"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-pr handler with mock gh (create)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result
      (handle-gh-pr (hasheq 'action "create" 'title "Test PR" 'head "feature/test" 'base "main")))
    (check-pred tool-result? result "gh-pr create returns tool-result")
    (check-false (tool-result-is-error? result) "gh-pr create succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-pr handler with mock gh (list)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-pr (hasheq 'action "list" 'state "open")))
    (check-pred tool-result? result "gh-pr list returns tool-result")
    (check-false (tool-result-is-error? result) "gh-pr list succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-milestone handler with mock gh (create)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result
      (handle-gh-milestone (hasheq 'action "create" 'title "v1.0" 'description "First release")))
    (check-pred tool-result? result "gh-milestone create returns tool-result")
    (check-false (tool-result-is-error? result) "gh-milestone create succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-milestone handler with mock gh (list)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-milestone (hasheq 'action "list" 'state "open")))
    (check-pred tool-result? result "gh-milestone list returns tool-result")
    (check-false (tool-result-is-error? result) "gh-milestone list succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-board handler with mock gh (status)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-board (hasheq 'action "status" 'milestone_number 1)))
    (check-pred tool-result? result "gh-board status returns tool-result")
    (check-false (tool-result-is-error? result) "gh-board status succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-board handler with mock gh (verify)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-board (hasheq 'action "verify" 'milestone_number 1)))
    (check-pred tool-result? result "gh-board verify returns tool-result")
    (check-false (tool-result-is-error? result) "gh-board verify succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-board handler with mock gh (batch_set)
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-board (hasheq 'action "batch_set" 'issue_numbers (list 1 2 3))))
    (check-pred tool-result? result "gh-board batch_set returns tool-result")
    (check-false (tool-result-is-error? result) "gh-board batch_set succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue create without title returns error
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "create")))
    (check-pred tool-result? result "create without title returns tool-result")
    (check-true (tool-result-is-error? result) "create without title is error"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-pr merge without number returns error
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-pr (hasheq 'action "merge")))
    (check-pred tool-result? result "merge without number returns tool-result")
    (check-true (tool-result-is-error? result) "merge without number is error"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-milestone create_from_spec with missing file
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result
      (handle-gh-milestone
       (hasheq 'action "create_from_spec" 'spec_file "/tmp/nonexistent-spec.json")))
    (check-pred tool-result? result "missing spec file returns tool-result")
    (check-true (tool-result-is-error? result) "missing spec file is error"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-wave-start without gh returns error
;; ============================================================

(let ()
  (parameterize ([gh-binary-path 'disabled])
    (define result (handle-gh-wave-start (hasheq 'issue_number 42)))
    (check-pred tool-result? result "wave-start without gh returns tool-result")
    (check-true (tool-result-is-error? result) "wave-start without gh is error")))

;; ============================================================
;; Tests: gh-wave-finish schema parity and validation
;; ============================================================

(test-case "gh-wave-finish schema has exactly the quarantined handler contract"
  (define properties (hash-ref gh-wave-finish-schema 'properties))
  (check-equal? (sort (hash-keys properties) symbol<?) '(commit_msg files issue_number))
  (check-equal? (hash-ref gh-wave-finish-schema 'required) '("issue_number" "files" "commit_msg"))
  (check-false (hash-ref gh-wave-finish-schema 'additionalProperties)))

(test-case "gh-wave-finish rejects unknown properties before quarantine"
  (define result
    (handle-gh-wave-finish
     (hasheq 'issue_number 42 'files '("test.rkt") 'commit_msg "test" 'branch "unsafe")))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "unknown property")))

(test-case "gh-wave-finish rejects issue numbers that do not match its positive-integer schema"
  (parameterize ([gh-binary-path 'disabled])
    (for ([issue-number (in-list '(0 "42"))])
      (define result
        (handle-gh-wave-finish
         (hasheq 'issue_number issue-number 'files '("test.rkt") 'commit_msg "test")))
      (check-true (tool-result-is-error? result))
      (check-true (string-contains? (result-text result) "issue_number")))))

(test-case "gh-wave-finish requires a non-empty safe relative files allowlist"
  (parameterize ([gh-binary-path 'disabled])
    (define tmp-root (make-temporary-file "gh-finish-root-~a" 'directory))
    (define abs-path (path->string (build-path tmp-root "absolute.rkt")))
    (define escape-path (path->string (build-path tmp-root "escape.rkt")))
    (dynamic-wind
     void
     (lambda ()
       (for ([files (in-list (list '() (list abs-path) (list escape-path) '("")))])
         (define result
           (handle-gh-wave-finish (hasheq 'issue_number 42 'files files 'commit_msg "test")))
         (check-true (tool-result-is-error? result))
         (check-true (string-contains? (result-text result) "files"))))
     (lambda () (delete-directory/files tmp-root #:must-exist? #f)))))

(test-case "gh-wave-finish requires a non-empty canonical commit_msg"
  (parameterize ([gh-binary-path 'disabled])
    (for ([message (in-list (list #f "" "   "))])
      (define args
        (if message
            (hasheq 'issue_number 42 'files '("test.rkt") 'commit_msg message)
            (hasheq 'issue_number 42 'files '("test.rkt"))))
      (define result (handle-gh-wave-finish args))
      (check-true (tool-result-is-error? result))
      (check-true (string-contains? (result-text result) "commit_msg")))))

;; ============================================================
;; Test: Extension definition
;; ============================================================

(check-not-false github-integration-extension "extension definition exists")

;; ============================================================
;; Test: gh-issue close_tree
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "close_tree" 'number 1)))
    (check-pred tool-result? result "close_tree returns tool-result")
    (check-false (tool-result-is-error? result) "close_tree succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-issue update
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-issue (hasheq 'action "update" 'number 1 'title "Updated title")))
    (check-pred tool-result? result "update returns tool-result")
    (check-false (tool-result-is-error? result) "update succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-board reconfigure
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-board (hasheq 'action "reconfigure" 'issue_number 1)))
    (check-pred tool-result? result "reconfigure returns tool-result")
    (check-false (tool-result-is-error? result) "reconfigure succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Test: gh-board stale
;; ============================================================

(let ()
  (define mock-dir (make-simple-mock-gh ""))
  (parameterize ([gh-binary-path (build-path mock-dir "gh")])
    (define result (handle-gh-board (hasheq 'action "stale" 'milestone_number 1)))
    (check-pred tool-result? result "stale returns tool-result")
    (check-false (tool-result-is-error? result) "stale succeeds"))
  (cleanup-mock-gh mock-dir))

;; ============================================================
;; Regression tests (Wave 3 — C3-C5, M1 fixes)
;; Validates: shell injection prevention, regex fix, input validation
;; ============================================================

(require (only-in "../extensions/github-integration.rkt"
                  valid-identifier?
                  valid-number?
                  valid-state?
                  valid-method?))

(test-case "valid-identifier? rejects shell metacharacters"
  (check-true (valid-identifier? "hello-world_1.0"))
  (check-false (valid-identifier? "; rm -rf /"))
  (check-false (valid-identifier? "$(pwned)"))
  (check-false (valid-identifier? "`id`")))

(test-case "valid-number? accepts integers and numeric strings only"
  (check-true (valid-number? 42))
  (check-true (valid-number? "123"))
  (check-false (valid-number? -1))
  (check-false (valid-number? "12; echo pwned"))
  (check-false (valid-number? "abc")))

(test-case "valid-state? only allows known states"
  (check-true (valid-state? "open"))
  (check-true (valid-state? "closed"))
  (check-true (valid-state? "all"))
  (check-false (valid-state? "injection; rm -rf"))
  (check-false (valid-state? "invalid")))

(test-case "valid-method? only allows known merge methods"
  (check-true (valid-method? "squash"))
  (check-true (valid-method? "merge"))
  (check-true (valid-method? "rebase"))
  (check-false (valid-method? "injection; DROP TABLE"))
  (check-false (valid-method? "invalid")))

(test-case "close_tree regex correctly matches digit issue numbers"
  ;; M1 fix: regex was ((0-9)+) which matched literal "0-9" chars
  ;; Now uses ([0-9]+) which matches actual digit sequences
  (define test-body "fixes #42, closes #100, resolves #999")
  (define matches
    (regexp-match* #px"(?:closes?|fixes?|resolves?)\\s+#(\\d+)" test-body #:match-select cadr))
  (check-equal? matches '("42" "100" "999")))
