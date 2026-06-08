#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-planning.rkt — tests for GSD planning extension
;;
;; Covers:
;;   - Path resolution for canonical and custom artifacts
;;   - Artifact name validation
;;   - Reading and writing planning artifacts
;;   - Tool handlers (planning-read, planning-write)
;;   - Extension registration (tools + commands)

(require rackunit
         racket/port
         racket/file
         racket/string
         json
         "../extensions/gsd-planning.rkt"
         "../extensions/context.rkt"
         "../extensions/dynamic-tools.rkt"
         "../extensions/ext-commands.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         (only-in "../extensions/gsd/core.rkt"
                  cmd-wave-done
                  gsd-command-result-success
                  gsd-command-result-message
                  gsd-command-result-data
                  gsd-success?
                  gsd-failed?)
         "../tools/tool.rkt"
         "../tools/tool-struct.rkt"
         "../agent/event-bus.rkt"
         "../util/event/event.rkt"
         (prefix-in events: "../extensions/gsd/events.rkt"))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-planning-dir)
  (define dir (make-temporary-file "gsd-test-~a" 'directory))
  dir)

(define (cleanup-temp-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define (with-temp-dir proc)
  (define dir (make-temp-planning-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-temp-dir dir)
                               (raise e))])
    (begin0 (proc dir)
      (cleanup-temp-dir dir))))

(define (make-test-ctx #:tool-registry [reg #f])
  (make-extension-ctx #:session-id "test-gsd"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:tool-registry (or reg (make-tool-registry))
                      #:command-registry (box (hash))))

(define (result-text result)
  (string-join (for/list ([c (in-list (tool-result-content result))]
                          #:when (and (hash? c) (hash-ref c 'text #f)))
                 (hash-ref c 'text ""))
               ""))

;; TH-02: Dynamic-wind wrapper ensuring GSD state cleanup even on test failure
(define (with-gsd-cleanup thunk)
  (dynamic-wind (lambda () (void)) thunk (lambda () (reset-all-gsd-state!))))

;; ============================================================
;; Path resolution tests
;; ============================================================

(test-case "planning-artifact-path resolves canonical PLAN"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "PLAN"))
                   (check-true (path? path))
                   (check-true (string-suffix? (path->string path) ".planning/PLAN.md")))))

(test-case "planning-artifact-path resolves canonical HANDOFF"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "HANDOFF"))
                   (check-true (path? path))
                   (check-true (string-suffix? (path->string path) ".planning/HANDOFF.json")))))

(test-case "planning-artifact-path resolves custom .md filename"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "CUSTOM_PLAN.md"))
                   (check-true (path? path))
                   (check-true (string-suffix? (path->string path) ".planning/CUSTOM_PLAN.md")))))

(test-case "planning-artifact-path resolves custom .json filename"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "wave-state.json"))
                   (check-true (path? path))
                   (check-true (string-suffix? (path->string path) ".planning/wave-state.json")))))

(test-case "planning-artifact-path returns #f for invalid name"
  (with-temp-dir (lambda (dir) (check-false (planning-artifact-path dir "SOMETHING")))))

;; ============================================================
;; Artifact name validation tests
;; ============================================================

(test-case "valid-artifact-name? accepts canonical names"
  (for ([name '("PLAN" "STATE"
                       "HANDOFF"
                       "VALIDATION"
                       "BUG_REPORT"
                       "BUG_PLAN"
                       "BUG_STATE"
                       "BUG_VALIDATION"
                       "SUMMARY")])
    (check-true (valid-artifact-name? name) name)))

(test-case "valid-artifact-name? accepts .md files"
  (check-true (valid-artifact-name? "CUSTOM.md"))
  (check-true (valid-artifact-name? "wave-plan.md")))

(test-case "valid-artifact-name? accepts .json files"
  (check-true (valid-artifact-name? "state.json"))
  (check-true (valid-artifact-name? "data.json")))

(test-case "valid-artifact-name? accepts wave document names"
  (check-true (valid-artifact-name? "waves/W0-fix-bug.md"))
  (check-true (valid-artifact-name? "waves/W1-add-tests.md"))
  (check-false (valid-artifact-name? "waves/"))
  (check-false (valid-artifact-name? "waves/no-md"))
  (check-false (valid-artifact-name? "waves/sub/dir.md"))
  (check-false (valid-artifact-name? "waves/../etc.md")))

(test-case "valid-artifact-name? rejects bare names without extension"
  (check-false (valid-artifact-name? "RANDOM"))
  (check-false (valid-artifact-name? "foo.txt")))

;; ============================================================
;; Read artifact tests
;; ============================================================

(test-case "read-planning-artifact returns #f for missing file"
  (with-temp-dir (lambda (dir) (check-false (read-planning-artifact dir "PLAN")))))

(test-case "read-planning-artifact reads markdown content"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "PLAN" "# Test Plan\nContent here")
                   (define content (read-planning-artifact dir "PLAN"))
                   (check-true (string? content))
                   (check-true (string-contains? content "# Test Plan")))))

(test-case "read-planning-artifact reads JSON content as hash"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "HANDOFF" (hasheq 'machine "local" 'wave "A2"))
                   (define content (read-planning-artifact dir "HANDOFF"))
                   (check-true (hash? content))
                   (check-true (hash? content))
                   (check-equal? (hash-ref content 'machine) "local"))))

(test-case "read-planning-artifact returns #f for invalid name"
  (with-temp-dir (lambda (dir) (check-false (read-planning-artifact dir "INVALID")))))

;; ============================================================
;; Write artifact tests
;; ============================================================

(test-case "write-planning-artifact! creates .planning dir and writes md"
  (with-temp-dir (lambda (dir)
                   (define result (write-planning-artifact! dir "STATE" "# State\nAll good"))
                   (check-true (path? result))
                   (check-true (file-exists? (build-path dir ".planning" "STATE.md")))
                   (define content (read-planning-artifact dir "STATE"))
                   (check-true (string-contains? content "All good")))))

(test-case "write-planning-artifact! writes JSON hash"
  (with-temp-dir (lambda (dir)
                   (define result
                     (write-planning-artifact! dir "HANDOFF" (hasheq 'machine "vps" 'wave "A2")))
                   (check-true (path? result))
                   (check-true (file-exists? (build-path dir ".planning" "HANDOFF.json")))
                   (define content (read-planning-artifact dir "HANDOFF"))
                   (check-true (hash? content))
                   (check-equal? (hash-ref content 'machine) "vps"))))

(test-case "write-planning-artifact! overwrites existing content"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "PLAN" "Version 1")
                   (write-planning-artifact! dir "PLAN" "Version 2")
                   (check-equal? (read-planning-artifact dir "PLAN") "Version 2"))))

(test-case "write-planning-artifact! returns #f for invalid name"
  (with-temp-dir (lambda (dir) (check-false (write-planning-artifact! dir "BADNAME" "content")))))

(test-case "write-planning-artifact! auto-creates waves/ subdirectory"
  (with-temp-dir (lambda (dir)
                   ;; The waves/ subdir doesn't exist yet — write-planning-artifact! must create it
                   (define result
                     (write-planning-artifact! dir "waves/W0-fix-bug.md" "# Wave 0\nFix the bug."))
                   (check-true (path? result))
                   (check-true (file-exists? (build-path dir ".planning" "waves" "W0-fix-bug.md")))
                   (define content (read-planning-artifact dir "waves/W0-fix-bug.md"))
                   (check-true (string-contains? content "Fix the bug")))))

(test-case "write-planning-artifact! writes multiple wave docs to new waves/ dir"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "waves/W0-first.md" "# W0")
                   (write-planning-artifact! dir "waves/W1-second.md" "# W1")
                   (check-true (file-exists? (build-path dir ".planning" "waves" "W0-first.md")))
                   (check-true (file-exists? (build-path dir ".planning" "waves" "W1-second.md"))))))

;; ============================================================
;; planning-read tool handler tests
;; ============================================================

(test-case "handle-planning-read returns error for missing artifact"
  (with-temp-dir (lambda (dir)
                   (define result (handle-planning-read (hasheq 'artifact "PLAN" 'base_dir dir)))
                   (check-true (tool-result-is-error? result))
                   (check-true (string-contains? (result-text result) "not found")))))

(test-case "handle-planning-read reads markdown artifact"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "PLAN" "# Plan\nWave A2")
                   (define result (handle-planning-read (hasheq 'artifact "PLAN" 'base_dir dir)))
                   (check-false (tool-result-is-error? result))
                   (check-true (string-contains? (result-text result) "Wave A2")))))

(test-case "handle-planning-read reads JSON artifact"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "HANDOFF" (hasheq 'status "active" 'machine "local"))
                   (define result (handle-planning-read (hasheq 'artifact "HANDOFF" 'base_dir dir)))
                   (check-false (tool-result-is-error? result))
                   (check-true (string-contains? (result-text result) "active")))))

(test-case "handle-planning-read returns error for empty artifact name"
  (define result (handle-planning-read (hasheq 'artifact "")))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "Missing")))

(test-case "handle-planning-read returns error for invalid artifact name"
  (define result (handle-planning-read (hasheq 'artifact "INVALID")))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "Invalid")))

;; ============================================================
;; planning-write tool handler tests
;; ============================================================

(test-case "handle-planning-write creates markdown artifact"
  (with-temp-dir
   (lambda (dir)
     (define result
       (handle-planning-write (hasheq 'artifact "PLAN" 'content "# New Plan" 'base_dir dir)))
     (check-false (tool-result-is-error? result))
     (check-true (string-contains? (result-text result) "Written"))
     ;; Verify file was actually created
     (check-equal? (read-planning-artifact dir "PLAN") "# New Plan"))))

(test-case "handle-planning-write creates JSON artifact from string"
  (with-temp-dir (lambda (dir)
                   (define result
                     (handle-planning-write (hasheq 'artifact
                                                    "HANDOFF"
                                                    'content
                                                    "{\"machine\":\"local\",\"wave\":\"A2\"}"
                                                    'base_dir
                                                    dir)))
                   (check-false (tool-result-is-error? result))
                   ;; Verify JSON was written correctly
                   (define content (read-planning-artifact dir "HANDOFF"))
                   (check-true (hash? content))
                   (check-equal? (hash-ref content 'machine) "local"))))

(test-case "handle-planning-write returns error for invalid JSON"
  (with-temp-dir
   (lambda (dir)
     (define result
       (handle-planning-write (hasheq 'artifact "HANDOFF" 'content "not-valid-json{" 'base_dir dir)))
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (result-text result) "Invalid JSON")))))

(test-case "handle-planning-write returns error for missing artifact"
  (define result (handle-planning-write (hasheq 'content "text")))
  (check-true (tool-result-is-error? result)))

(test-case "handle-planning-write returns error for missing content"
  (define result (handle-planning-write (hasheq 'artifact "PLAN")))
  (check-true (tool-result-is-error? result)))

(test-case "handle-planning-write returns error for invalid artifact name"
  (define result (handle-planning-write (hasheq 'artifact "INVALID" 'content "text")))
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Extension registration tests
;; ============================================================

(test-case "gsd-planning-extension has correct name and version"
  (check-equal? (extension-name gsd-planning-extension) "gsd-planning-extension")
  (check-equal? (extension-version gsd-planning-extension) "1.1.0"))

(test-case "gsd-planning-extension has register-tools hook"
  (define hooks (extension-hooks gsd-planning-extension))
  (check-true (hash-has-key? hooks 'register-tools)))

(test-case "gsd-planning-extension registers tools via hook"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'register-tools))
  (handler ctx (hasheq))
  (check-true (tool? (lookup-tool reg "planning-read")))
  (check-true (tool? (lookup-tool reg "planning-write")))
  ;; Check tools have proper schemas
  (define pr (lookup-tool reg "planning-read"))
  (check-true (hash? (tool-schema pr)))
  (check-equal? (hash-ref (tool-schema pr) 'type) "object"))

(test-case "gsd-planning-extension registers commands via hook"
  (define ctx (make-test-ctx))
  (define hooks (extension-hooks gsd-planning-extension))
  (when (hash-has-key? hooks 'register-shortcuts)
    (define handler (hash-ref hooks 'register-shortcuts))
    (handler ctx)
    ;; Commands should be registered
    (define cmd (ext-lookup-command ctx "/plan"))
    (check-pred values cmd)
    (check-pred values (ext-lookup-command ctx "/state"))
    (check-pred values (ext-lookup-command ctx "/handoff"))))

(test-case "registered planning-read tool is callable"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "STATE" "# State\nActive")
                   (define reg (make-tool-registry))
                   (define ctx (make-test-ctx #:tool-registry reg))
                   (define handler
                     (hash-ref (extension-hooks gsd-planning-extension) 'register-tools))
                   (handler ctx (hasheq))
                   (define pr (lookup-tool reg "planning-read"))
                   (define result ((tool-execute pr) (hasheq 'artifact "STATE" 'base_dir dir) #f))
                   (check-false (tool-result-is-error? result))
                   (check-true (string-contains? (result-text result) "Active")))))

(test-case "registered planning-write tool is callable"
  (with-temp-dir
   (lambda (dir)
     (define reg (make-tool-registry))
     (define ctx (make-test-ctx #:tool-registry reg))
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'register-tools))
     (handler ctx (hasheq))
     (define pw (lookup-tool reg "planning-write"))
     (define result
       ((tool-execute pw) (hasheq 'artifact "VALIDATION" 'content "## Tests pass" 'base_dir dir) #f))
     (check-false (tool-result-is-error? result))
     ;; Verify content was written
     (check-equal? (read-planning-artifact dir "VALIDATION") "## Tests pass"))))

;; ============================================================
;; M2 regression: Path traversal in artifact names
;; ============================================================

(test-case "valid-artifact-name? rejects path traversal"
  (check-true (valid-artifact-name? "PLAN"))
  (check-true (valid-artifact-name? "custom.md"))
  (check-true (valid-artifact-name? "data.json"))
  (check-false (valid-artifact-name? "../../etc/crontab.md"))
  (check-false (valid-artifact-name? "foo/bar.md"))
  (check-false (valid-artifact-name? "..\x00PLAN")))

(test-case "write-planning-artifact rejects path traversal"
  (define dir (make-temporary-file "q-test-gsd-~a" 'directory))
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'register-tools))
  (handler ctx (hasheq))
  (define pw (lookup-tool reg "planning-write"))
  (define result
    ((tool-execute pw) (hasheq 'artifact "../../etc/crontab.md" 'content "pwned" 'base_dir dir) #f))
  ;; Should fail gracefully (artifact name rejected)
  (check-true (or (tool-result-is-error? result)
                  (not (file-exists? (build-path dir ".planning" "../../etc/crontab.md")))))
  (delete-directory/files dir))

;; ============================================================
;; execute-command handler tests
;; ============================================================

(test-case "execute-command handler returns plan content"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-directory dir])
       ;; Write a plan
       (make-directory* (build-path dir ".planning"))
       (call-with-output-file (build-path dir ".planning" "PLAN.md")
                              (lambda (out) (display "# Plan\nWave 1: test" out))
                              #:exists 'truncate)
       ;; Get the handler from the extension hooks
       (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
       (define result (handler (hasheq 'command "/plan" 'input "/plan")))
       (check-equal? (hook-result-action result) 'amend)
       (check-equal? (hash-ref (hook-result-payload result) 'text) "# Plan\nWave 1: test")))))

(test-case "execute-command handler returns state content"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-directory dir])
       (make-directory* (build-path dir ".planning"))
       (call-with-output-file (build-path dir ".planning" "STATE.md")
                              (lambda (out) (display "Status: in progress" out))
                              #:exists 'truncate)
       (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
       (define result (handler (hasheq 'command "/state" 'input "/state")))
       (check-equal? (hook-result-action result) 'amend)
       (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "in progress"))))))

(test-case "execute-command handler passes unknown commands"
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
  (define result (handler (hasheq 'command "/unknown" 'input "/unknown")))
  (check-equal? (hook-result-action result) 'pass))

(test-case "execute-command handler reports missing artifact"
  (with-temp-dir (lambda (dir)
                   (parameterize ([current-directory dir])
                     ;; No .planning/ dir
                     (define handler
                       (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                     (define result (handler (hasheq 'command "/plan" 'input "/plan")))
                     (check-equal? (hook-result-action result) 'amend)
                     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text)
                                                   "No PLAN found"))))))

(test-case "planning-system-prompt has GSD planning instructions"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "planning-write"))
  (check-true (string-contains? p "PLAN.md"))
  (check-true (string-contains? p "Do NOT implement")))

;; ============================================================
;; W0 (#1864): /plan prompt content tests
;; ============================================================

(test-case "planning-system-prompt limits exploration to 8 file reads"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "Do NOT read more than 8 files")))

(test-case "planning-system-prompt requires root cause identification"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "Root Cause"))
  (check-true (string-contains? p "old-text")))

(test-case "planning-system-prompt requires old-text for edit matching"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "old-text")))

(test-case "planning-system-prompt specifies actionable plan format"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "old-text"))
  (check-true (string-contains? p "new-text"))
  (check-true (string-contains? p "Verify")))

(test-case "/plan <text> returns augmented submit payload"
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
  (define result (handler (hasheq 'command "/plan" 'input "/plan refactor the module")))
  (check-equal? (hook-result-action result) 'amend)
  (define payload (hook-result-payload result))
  ;; Submit text is augmented with planning preamble
  (define submit-text (hash-ref payload 'submit))
  (check-true (string-contains? submit-text "GSD Planning"))
  (check-true (string-contains? submit-text "planning-write"))
  (check-true (string-contains? submit-text "refactor the module"))
  (check-true (string-contains? (hash-ref payload 'text) "refactor the module")))

(test-case "/p <text> returns augmented submit payload (shortcut)"
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
  (define result (handler (hasheq 'command "/p" 'input "/p quick fix")))
  (check-equal? (hook-result-action result) 'amend)
  (define payload (hook-result-payload result))
  (define submit-text (hash-ref payload 'submit))
  (check-true (string-contains? submit-text "GSD Planning"))
  (check-true (string-contains? submit-text "quick fix")))

(test-case "/plan (no args) returns display text"
  (with-temp-dir (lambda (dir)
                   (parameterize ([current-directory dir])
                     (make-directory* (build-path dir ".planning"))
                     (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                            (lambda (out) (display "# Test Plan" out))
                                            #:exists 'truncate)
                     (define handler
                       (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                     (define result (handler (hasheq 'command "/plan" 'input "/plan")))
                     (check-equal? (hook-result-action result) 'amend)
                     (define payload (hook-result-payload result))
                     ;; No submit key — display mode
                     (check-false (hash-ref payload 'submit #f))
                     (check-equal? (hash-ref payload 'text) "# Test Plan")))))

(test-case "/state <text> ignores args and displays artifact"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-directory dir])
       (make-directory* (build-path dir ".planning"))
       (call-with-output-file (build-path dir ".planning" "STATE.md")
                              (lambda (out) (display "Status: active" out))
                              #:exists 'truncate)
       (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
       (define result (handler (hasheq 'command "/state" 'input "/state some extra text")))
       (check-equal? (hook-result-action result) 'amend)
       (define payload (hook-result-payload result))
       ;; No submit key — /state always displays
       (check-false (hash-ref payload 'submit #f))
       (check-equal? (hash-ref payload 'text) "Status: active")))))

(test-case "/handoff <text> ignores args and displays artifact"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-directory dir])
       (make-directory* (build-path dir ".planning"))
       (call-with-output-file (build-path dir ".planning" "HANDOFF.json")
                              (lambda (out) (write-json (hasheq 'machine "local") out))
                              #:exists 'truncate)
       (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
       (define result (handler (hasheq 'command "/handoff" 'input "/handoff do something")))
       (check-equal? (hook-result-action result) 'amend)
       (define payload (hook-result-payload result))
       ;; No submit key — /handoff always displays
       (check-false (hash-ref payload 'submit #f))
       (check-true (string-contains? (hash-ref payload 'text) "local"))))))

;; ============================================================
;; /go command tests
;; ============================================================

(test-case "/go reports error when no plan exists"
  (with-temp-dir (lambda (dir)
                   (parameterize ([current-directory dir])
                     (define handler
                       (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                     (define result (handler (hasheq 'command "/go" 'input "/go")))
                     (check-equal? (hook-result-action result) 'amend)
                     (define payload (hook-result-payload result))
                     (check-false (hash-ref payload 'submit #f) "no submit without plan")
                     (check-true (string-contains? (hash-ref payload 'text) "No PLAN found"))))))

(test-case "/go returns new-session payload with plan content"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir
      (lambda (dir)
        (parameterize ([current-directory dir])
          (set-pinned-planning-dir! dir)
          (make-directory* (build-path dir ".planning"))
          (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                 (lambda (out)
                                   (display "# Plan\n## Wave 0: Fix bug\n- File: foo.rkt" out))
                                 #:exists 'truncate)
          (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
          (define result (handler (hasheq 'command "/go" 'input "/go")))
          (check-equal? (hook-result-action result) 'amend)
          (define payload (hook-result-payload result))
          (define new-session-text (hash-ref payload 'new-session))
          (check-true (string-contains? new-session-text "[gsd-planning]"))
          (check-true (string-contains? new-session-text "IMPLEMENT NOW"))
          (check-true (string-contains? new-session-text "Wave 0"))
          (check-true (string-contains? (hash-ref payload 'text) "Implementing"))))))))

;; ============================================================
;; W1 (#1867): /go anti-exploration tests
;; ============================================================

(test-case "planning-implement-prompt forbids re-reading the plan"
  (check-true (string-contains? planning-implement-prompt "Do NOT re-read the plan")))

(test-case "planning-implement-prompt instructs to read target files before editing"
  (check-true (string-contains? planning-implement-prompt "Read each target file BEFORE editing")))

(test-case "planning-implement-prompt forbids writing a new plan"
  (check-true (string-contains? planning-implement-prompt "Do NOT write a new plan")))

(test-case "planning-implement-prompt allows planning-read but blocks planning-write"
  (check-false (string-contains? planning-implement-prompt
                                 "Do NOT use planning-read or planning-write")
               "prompt should NOT say planning-read is prohibited")
  (check-true (string-contains? planning-implement-prompt "Do NOT use planning-write")
              "prompt should say planning-write is prohibited"))

(test-case "planning-implement-prompt does NOT tell agent to use planning-read"
  (check-false (string-contains? planning-implement-prompt "Use planning-read")))

(test-case "/go includes state when available"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir
      (lambda (dir)
        (parameterize ([current-directory dir])
          (set-pinned-planning-dir! dir)
          (make-directory* (build-path dir ".planning"))
          (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                 (lambda (out)
                                   (display "# Plan\n## Wave 0: Fix\n- File: foo.rkt" out))
                                 #:exists 'truncate)
          (call-with-output-file (build-path dir ".planning" "STATE.md")
                                 (lambda (out) (display "W0: done" out))
                                 #:exists 'truncate)
          (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
          (define result (handler (hasheq 'command "/go" 'input "/go")))
          (define submit-text (hash-ref (hook-result-payload result) 'new-session))
          (check-true (string-contains? submit-text "W0: done"))))))))

(test-case "/go N starts at specified wave"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir
      (lambda (dir)
        (parameterize ([current-directory dir])
          (set-pinned-planning-dir! dir)
          (make-directory* (build-path dir ".planning"))
          (call-with-output-file
           (build-path dir ".planning" "PLAN.md")
           (lambda (out)
             (display
              "# Plan\n## Wave 0: Fix\n- File: foo.rkt\n## Wave 1: Core\n- File: bar.rkt\n## Wave 2: Test\n- File: baz.rkt"
              out))
           #:exists 'truncate)
          (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
          (define result (handler (hasheq 'command "/go" 'input "/go 2")))
          (define submit-text (hash-ref (hook-result-payload result) 'new-session))
          (check-true (string-contains? submit-text "wave 2"))))))))

(test-case "/go emits normalized pipeline events via events.rkt bus"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir
      (lambda (dir)
        (parameterize ([current-directory dir])
          (set-pinned-planning-dir! dir)
          (make-directory* (build-path dir ".planning"))
          (call-with-output-file
           (build-path dir ".planning" "PLAN.md")
           (lambda (out)
             (display "# Plan\n## Wave 0: Fix\n- File: foo.rkt\n## Wave 1: Polish\n- File: bar.rkt"
                      out))
           #:exists 'truncate)
          ;; Set up events.rkt event collector
          (define-values (collector get-events) (events:make-event-collector))
          (events:set-gsd-event-bus! collector)
          (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
          (define result (handler (hasheq 'command "/go" 'input "/go")))
          (check-equal? (hook-result-action result) 'amend)
          ;; Check that pipeline events were emitted
          (define evts (events:collector-events get-events))
          (define event-names (map (lambda (e) (hash-ref e 'event #f)) evts))
          (check-not-false (member 'gsd.plan.parsed event-names) "should emit gsd.plan.parsed")
          (check-not-false (member 'gsd.plan.normalized event-names)
                           "should emit gsd.plan.normalized")
          (check-not-false (member 'gsd.plan.validated event-names) "should emit gsd.plan.validated")
          (check-not-false (member 'gsd.mode.changed event-names) "should emit gsd.mode.changed")
          (delete-directory/files dir)))))))

(test-case "/implement alias works"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir
      (lambda (dir)
        (parameterize ([current-directory dir])
          (set-pinned-planning-dir! dir)
          (make-directory* (build-path dir ".planning"))
          (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                 (lambda (out)
                                   (display "# Plan\n## Wave 0: Fix\n- File: foo.rkt" out))
                                 #:exists 'truncate)
          (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
          (define result (handler (hasheq 'command "/implement" 'input "/implement")))
          (check-equal? (hook-result-action result) 'amend)
          (define submit-text (hash-ref (hook-result-payload result) 'new-session))
          (check-true (string-contains? submit-text "[gsd-planning]"))))))))

;; ============================================================
;; W0: /plan Exploration Cap Tests
;; ============================================================

(test-case "planning-system-prompt-no-unlimited-exploration"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "Do NOT read more than 8 files")
              "prompt should limit exploration to 8 file reads"))

;; ============================================================
;; W2: /plan Overwrite Stale Plans Tests
;; ============================================================

(test-case "planning-system-prompt-references-wave-docs"
  (define p (planning-system-prompt ""))
  (check-true (string-contains? p "waves/") "prompt should reference wave documents")
  (check-true (string-contains? p "planning-write") "prompt should mention planning-write tool"))

(test-case "/plan-with-text-injects-stale-warning-when-plan-exists"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir
      (lambda (dir)
        (parameterize ([current-directory dir])
          (set-pinned-planning-dir! dir)
          (make-directory* (build-path dir ".planning"))
          (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                 (lambda (out) (display "# Old Plan\nWave 0: old stuff" out))
                                 #:exists 'truncate)
          (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
          (define result (handler (hasheq 'command "/plan" 'input "/plan fix the bug")))
          (define submit-text (hash-ref (hook-result-payload result) 'submit))
          (check-true (string-contains? submit-text "existing PLAN.md was found")
                      "should inject stale warning when PLAN.md exists")
          (check-true (string-contains? submit-text "OVERWRITE")
                      "stale warning should mention OVERWRITE")))))))

(test-case "/plan-with-text-no-warning-when-no-plan"
  (with-gsd-cleanup
   (lambda ()
     (with-temp-dir (lambda (dir)
                      (parameterize ([current-directory dir])
                        (set-pinned-planning-dir! dir)
                        (make-directory* (build-path dir ".planning"))
                        (define handler
                          (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                        (define result (handler (hasheq 'command "/plan" 'input "/plan fix the bug")))
                        (define submit-text (hash-ref (hook-result-payload result) 'submit))
                        (check-false (string-contains? submit-text "existing PLAN.md was found")
                                     "should NOT inject stale warning when no PLAN.md exists")))))))

;; ============================================================
;; Wave 3 tests: Prompt constants, artifact registry, I1 fix
;; ============================================================

(test-case "planning-implement-prompt allows planning-read during execution (I1 fix)"
  (check-true (string-contains? planning-implement-prompt "planning-read is allowed")
              "prompt should say planning-read is allowed during /go")
  (check-false (string-contains? planning-implement-prompt
                                 "Do NOT use planning-read or planning-write")
               "prompt should NOT say planning-read is prohibited"))

(test-case "valid-artifact-name? accepts BUG_REPORT"
  (check-true (valid-artifact-name? "BUG_REPORT")))

(test-case "valid-artifact-name? accepts REVIEW"
  (check-true (valid-artifact-name? "REVIEW")))

(test-case "valid-artifact-name? accepts ANALYSIS"
  (check-true (valid-artifact-name? "ANALYSIS")))

(test-case "gsd-tool-guard allows planning-read during executing (I1 fix)"
  (with-gsd-cleanup (lambda ()
                      (set-gsd-mode! 'executing)
                      (define res (gsd-tool-guard (hasheq 'tool-name "planning-read" 'args (hasheq))))
                      (check-eq? (hook-result-action res) 'pass))))

;; ============================================================
;; W1 (v0.20.4): parse-wave-headers tests
;; ============================================================

(test-case "parse-wave-headers extracts wave indices from plan text"
  (define plan-text
    "## Wave 0: Foundation\nSome content\n## Wave 1: Core\nMore content\n## Wave 2: Tests")
  (define indices (parse-wave-headers plan-text))
  (check-equal? indices '(0 1 2)))

(test-case "parse-wave-headers returns empty list for plan with no waves"
  (define indices (parse-wave-headers "# Plan\nJust text"))
  (check-equal? indices '()))

(test-case "parse-wave-headers handles single wave"
  (define indices (parse-wave-headers "## Wave 3: Only Wave"))
  (check-equal? indices '(3)))

(test-case "parse-wave-headers handles non-consecutive waves"
  (define indices (parse-wave-headers "## Wave 0: Start\n## Wave 5: End"))
  (check-equal? indices '(0 5)))

;; ============================================================
;; W1 (v0.20.4): /go sets total waves from plan
;; ============================================================

;; ============================================================
;; W1 (v0.20.4): /plan initializes plan budget
;; ============================================================

;; ============================================================
;; W1 (v0.20.4): /gsd status command tests
;; ============================================================

(test-case "/gsd shows status when inactive"
  (with-gsd-cleanup (lambda ()
                      (define handler
                        (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                      (define result (handler (hasheq 'command "/gsd" 'input "/gsd")))
                      (check-eq? (hook-result-action result) 'amend)
                      (define text (hash-ref (hook-result-payload result) 'text))
                      (check-true (string-contains? text "Mode: inactive")))))

(test-case "/gsd shows wave progress during execution"
  (with-gsd-cleanup (lambda ()
                      (set-gsd-mode! 'executing)
                      (set-total-waves! 4)
                      (mark-wave-complete! 0)
                      (mark-wave-complete! 1)
                      (define handler
                        (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                      (define result (handler (hasheq 'command "/gsd" 'input "/gsd")))
                      (define text (hash-ref (hook-result-payload result) 'text))
                      (check-true (string-contains? text "Mode: executing"))
                      (check-true (string-contains? text "Waves: 2/4 complete")))))

;; ============================================================
;; W1 (v0.20.4): /plan budget enforcement
;; ============================================================

;; ============================================================
;; v0.20.5 W1: Event bus — parameter → registry-stored
;; ============================================================

(test-case "W1: set-gsd-event-bus! and gsd-event-bus roundtrip"
  (with-gsd-cleanup (lambda ()
                      (check-false (gsd-event-bus) "event bus should start as #f")
                      (define bus (make-event-bus))
                      (set-gsd-event-bus! bus)
                      (check-eq? (gsd-event-bus) bus "event bus should be the one we set"))))

(test-case "W1: emit-gsd-event! publishes when bus is set"
  (with-gsd-cleanup (lambda ()
                      (define-values (collector query) (events:make-event-collector))
                      (events:set-gsd-event-bus! collector)
                      ;; emit-gsd-event! is defined in gsd-planning.rkt
                      (emit-gsd-event! 'gsd.mode.changed (hasheq 'key 'value))
                      (define events (events:collector-events query))
                      (check-equal? (length events) 1 "should receive one event")
                      (define evt (car events))
                      (check-equal? (hash-ref evt 'event) 'gsd.mode.changed)
                      (check-equal? (hash-ref (hash-ref evt 'data) 'key) 'value))))

(test-case "W1: emit-gsd-event! is no-op when bus is #f"
  (with-gsd-cleanup (lambda ()
                      ;; Should not raise an error
                      (emit-gsd-event! 'gsd.mode.changed (hasheq 'key 'value))
                      (check-false (gsd-event-bus) "bus should still be #f"))))

;; ============================================================
;; Wave progress tracking tests (v0.21.2 W1)
;; ============================================================

;; ============================================================
;; W1: Command Unification — wired dispatch tests (v0.21.6)
;; ============================================================

(test-case "W1: /replan wired through execute-command from plan-written"
  (with-gsd-cleanup
   (lambda ()
     (set-gsd-mode! 'planning)
     (set-gsd-mode! 'plan-written)
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
     (define result (handler (hasheq 'command "/replan" 'input "/replan")))
     (check-equal? (hook-result-action result) 'amend)
     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "Re-planning")))))

(test-case "W1: /replan from idle returns error through execute-command"
  (with-gsd-cleanup
   (lambda ()
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
     (define result (handler (hasheq 'command "/replan" 'input "/replan")))
     (check-equal? (hook-result-action result) 'amend)
     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "Cannot re-plan")))))

(test-case "W1: /skip wired through execute-command during executing"
  (with-gsd-cleanup
   (lambda ()
     (set-gsd-mode! 'planning)
     (set-gsd-mode! 'plan-written)
     (set-gsd-mode! 'executing)
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
     (define result (handler (hasheq 'command "/skip" 'input "/skip 1")))
     (check-equal? (hook-result-action result) 'amend)
     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "skipped")))))

(test-case "W1: /skip without arg returns usage through execute-command"
  (with-gsd-cleanup
   (lambda ()
     (set-gsd-mode! 'planning)
     (set-gsd-mode! 'plan-written)
     (set-gsd-mode! 'executing)
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
     (define result (handler (hasheq 'command "/skip" 'input "/skip")))
     (check-equal? (hook-result-action result) 'amend)
     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "Usage")))))

(test-case "W1: /reset wired through execute-command"
  (with-gsd-cleanup
   (lambda ()
     (set-gsd-mode! 'planning)
     (set-gsd-mode! 'plan-written)
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
     (define result (handler (hasheq 'command "/reset" 'input "/reset")))
     (check-equal? (hook-result-action result) 'amend)
     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "reset")))))

(test-case "W1: unknown command returns hook-pass through execute-command"
  (with-gsd-cleanup (lambda ()
                      (define handler
                        (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                      (define result (handler (hasheq 'command "/unknown-cmd" 'input "/unknown-cmd")))
                      (check-equal? (hook-result-action result) 'pass))))

;; ============================================================
;; /wave-done command tests (v0.21.10)
;; ============================================================

(test-case "wave-done: marks wave complete with valid index"
  (with-gsd-cleanup
   (lambda ()
     (define tmp-dir (make-temporary-file "q-test-wave-done-~a" 'directory))
     (define planning-dir (build-path tmp-dir ".planning"))
     (make-directory planning-dir)
     ;; Create a minimal PLAN.md with wave index
     (display-to-file "# Test Plan\n\n- [Inbox] W0: First wave\n- [Inbox] W1: Second wave\n"
                      (build-path planning-dir "PLAN.md")
                      #:exists 'replace)
     ;; Create STATE.md
     (display-to-file "# Project State\n\nStatus: Active\n"
                      (build-path planning-dir "STATE.md")
                      #:exists 'replace)
     ;; Set up GSD state
     (set-pinned-planning-dir! tmp-dir)
     (set-gsd-mode! 'planning)
     (set-gsd-mode! 'plan-written)
     (set-gsd-mode! 'executing)
     (set-total-waves! 2)
     ;; Call cmd-wave-done
     (define result (cmd-wave-done tmp-dir "0"))
     (check-true (gsd-command-result-success result))
     (define data (gsd-command-result-data result))
     (check-equal? (and (hash? data) (hash-ref data 'wave #f)) 0)
     (check-true (string-contains? (gsd-command-result-message result) "Wave 0"))
     ;; Verify PLAN.md updated
     (define plan-content (file->string (build-path planning-dir "PLAN.md")))
     (check-true (string-contains? plan-content "[DONE] W0:"))
     (check-true (string-contains? plan-content "[Inbox] W1:"))
     ;; Verify STATE.md updated
     (define state-content (file->string (build-path planning-dir "STATE.md")))
     (check-true (string-contains? state-content "W0: completed"))
     ;; Cleanup
     (delete-directory/files tmp-dir))))

(test-case "wave-done: without index returns usage"
  (with-gsd-cleanup (lambda ()
                      (define result (cmd-wave-done #f ""))
                      (check-false (gsd-command-result-success result))
                      (check-true (string-contains? (gsd-command-result-message result) "Usage")))))

(test-case "wave-done: non-numeric index returns error"
  (with-gsd-cleanup (lambda ()
                      (define result (cmd-wave-done #f "abc"))
                      (check-false (gsd-command-result-success result))
                      (check-true (string-contains? (gsd-command-result-message result) "Invalid")))))

(test-case "wave-done: negative index returns error"
  (with-gsd-cleanup (lambda ()
                      (define result (cmd-wave-done #f "-1"))
                      (check-false (gsd-command-result-success result))
                      (check-true (string-contains? (gsd-command-result-message result)
                                                    "non-negative")))))

(test-case "wave-done: multiple waves can be marked"
  (with-gsd-cleanup (lambda ()
                      (define tmp-dir (make-temporary-file "q-test-wave-done-~a" 'directory))
                      (define planning-dir (build-path tmp-dir ".planning"))
                      (make-directory planning-dir)
                      (display-to-file "# Test Plan\n\n- [Inbox] W0: First\n- [Inbox] W1: Second\n"
                                       (build-path planning-dir "PLAN.md")
                                       #:exists 'replace)
                      (display-to-file "# Project State\n\nStatus: Active\n"
                                       (build-path planning-dir "STATE.md")
                                       #:exists 'replace)
                      (set-pinned-planning-dir! tmp-dir)
                      (set-gsd-mode! 'planning)
                      (set-gsd-mode! 'plan-written)
                      (set-gsd-mode! 'executing)
                      (set-total-waves! 2)
                      ;; Mark W0
                      (define r0 (cmd-wave-done tmp-dir "0"))
                      (check-true (gsd-command-result-success r0))
                      ;; Mark W1
                      (define r1 (cmd-wave-done tmp-dir "1"))
                      (check-true (gsd-command-result-success r1))
                      ;; Both marked in PLAN.md
                      (define plan-content (file->string (build-path planning-dir "PLAN.md")))
                      (check-true (string-contains? plan-content "[DONE] W0:"))
                      (check-true (string-contains? plan-content "[DONE] W1:"))
                      ;; Both in STATE.md
                      (define state-content (file->string (build-path planning-dir "STATE.md")))
                      (check-true (string-contains? state-content "W0: completed"))
                      (check-true (string-contains? state-content "W1: completed"))
                      (delete-directory/files tmp-dir))))

(test-case "wave-done: works through execute-command handler"
  (with-gsd-cleanup
   (lambda ()
     (define tmp-dir (make-temporary-file "q-test-wave-done-~a" 'directory))
     (define planning-dir (build-path tmp-dir ".planning"))
     (make-directory planning-dir)
     (display-to-file "# Test Plan\n\n- [Inbox] W0: First\n"
                      (build-path planning-dir "PLAN.md")
                      #:exists 'replace)
     (display-to-file "# Project State\n\nStatus: Active\n"
                      (build-path planning-dir "STATE.md")
                      #:exists 'replace)
     (set-pinned-planning-dir! tmp-dir)
     (set-gsd-mode! 'planning)
     (set-gsd-mode! 'plan-written)
     (set-gsd-mode! 'executing)
     (set-total-waves! 1)
     (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
     (define result (handler (hasheq 'command "/wave-done" 'input "/wave-done 0")))
     (check-equal? (hook-result-action result) 'amend)
     (check-true (string-contains? (hash-ref (hook-result-payload result) 'text) "Wave 0"))
     (delete-directory/files tmp-dir))))
