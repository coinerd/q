#lang racket

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
         "../tools/tool.rkt"
         "../agent/event-bus.rkt")

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

;; ============================================================
;; Path resolution tests
;; ============================================================

(test-case "planning-artifact-path resolves canonical PLAN"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "PLAN"))
                   (check-not-false path)
                   (check-true (string-suffix? (path->string path) ".planning/PLAN.md")))))

(test-case "planning-artifact-path resolves canonical HANDOFF"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "HANDOFF"))
                   (check-not-false path)
                   (check-true (string-suffix? (path->string path) ".planning/HANDOFF.json")))))

(test-case "planning-artifact-path resolves custom .md filename"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "CUSTOM_PLAN.md"))
                   (check-not-false path)
                   (check-true (string-suffix? (path->string path) ".planning/CUSTOM_PLAN.md")))))

(test-case "planning-artifact-path resolves custom .json filename"
  (with-temp-dir (lambda (dir)
                   (define path (planning-artifact-path dir "wave-state.json"))
                   (check-not-false path)
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
    (check-not-false (valid-artifact-name? name) name)))

(test-case "valid-artifact-name? accepts .md files"
  (check-true (valid-artifact-name? "CUSTOM.md"))
  (check-true (valid-artifact-name? "wave-plan.md")))

(test-case "valid-artifact-name? accepts .json files"
  (check-true (valid-artifact-name? "state.json"))
  (check-true (valid-artifact-name? "data.json")))

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
                   (check-not-false content)
                   (check-true (string-contains? content "# Test Plan")))))

(test-case "read-planning-artifact reads JSON content as hash"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "HANDOFF" (hasheq 'machine "local" 'wave "A2"))
                   (define content (read-planning-artifact dir "HANDOFF"))
                   (check-not-false content)
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
                   (check-not-false result)
                   (check-true (file-exists? (build-path dir ".planning" "STATE.md")))
                   (define content (read-planning-artifact dir "STATE"))
                   (check-true (string-contains? content "All good")))))

(test-case "write-planning-artifact! writes JSON hash"
  (with-temp-dir (lambda (dir)
                   (define result
                     (write-planning-artifact! dir "HANDOFF" (hasheq 'machine "vps" 'wave "A2")))
                   (check-not-false result)
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
  (check-equal? (extension-version gsd-planning-extension) "1.0.0"))

(test-case "gsd-planning-extension has register-tools hook"
  (define hooks (extension-hooks gsd-planning-extension))
  (check-true (hash-has-key? hooks 'register-tools)))

(test-case "gsd-planning-extension registers tools via hook"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'register-tools))
  (handler ctx (hasheq))
  (check-not-false (lookup-tool reg "planning-read"))
  (check-not-false (lookup-tool reg "planning-write"))
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
    (check-not-false cmd)
    (check-not-false (ext-lookup-command ctx "/state"))
    (check-not-false (ext-lookup-command ctx "/handoff"))))

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
  (check-true (string-contains? planning-system-prompt "[gsd-planning]"))
  (check-true (string-contains? planning-system-prompt "planning-write"))
  (check-true (string-contains? planning-system-prompt "PLAN.md"))
  (check-true (string-contains? planning-system-prompt "Do NOT implement")))

(test-case "/plan <text> returns augmented submit payload"
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
  (define result (handler (hasheq 'command "/plan" 'input "/plan refactor the module")))
  (check-equal? (hook-result-action result) 'amend)
  (define payload (hook-result-payload result))
  ;; Submit text is augmented with planning preamble
  (define submit-text (hash-ref payload 'submit))
  (check-true (string-contains? submit-text "[gsd-planning]"))
  (check-true (string-contains? submit-text "planning-write"))
  (check-true (string-contains? submit-text "refactor the module"))
  (check-true (string-contains? (hash-ref payload 'text) "refactor the module")))

(test-case "/p <text> returns augmented submit payload (shortcut)"
  (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
  (define result (handler (hasheq 'command "/p" 'input "/p quick fix")))
  (check-equal? (hook-result-action result) 'amend)
  (define payload (hook-result-payload result))
  (define submit-text (hash-ref payload 'submit))
  (check-true (string-contains? submit-text "[gsd-planning]"))
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

(test-case "/go submits implementation prompt with plan content"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-directory dir])
       (make-directory* (build-path dir ".planning"))
       (call-with-output-file (build-path dir ".planning" "PLAN.md")
                              (lambda (out) (display "# Plan\n## Wave 0\n- Fix bug" out))
                              #:exists 'truncate)
       (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
       (define result (handler (hasheq 'command "/go" 'input "/go")))
       (check-equal? (hook-result-action result) 'amend)
       (define payload (hook-result-payload result))
       (define submit-text (hash-ref payload 'submit))
       (check-true (string-contains? submit-text "[gsd-planning]"))
       (check-true (string-contains? submit-text "planning-read"))
       (check-true (string-contains? submit-text "Wave 0"))
       (check-true (string-contains? (hash-ref payload 'text) "Implementing"))))))

(test-case "/go includes state when available"
  (with-temp-dir (lambda (dir)
                   (parameterize ([current-directory dir])
                     (make-directory* (build-path dir ".planning"))
                     (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                            (lambda (out) (display "# Plan\nWave 0" out))
                                            #:exists 'truncate)
                     (call-with-output-file (build-path dir ".planning" "STATE.md")
                                            (lambda (out) (display "W0: done" out))
                                            #:exists 'truncate)
                     (define handler
                       (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                     (define result (handler (hasheq 'command "/go" 'input "/go")))
                     (define submit-text (hash-ref (hook-result-payload result) 'submit))
                     (check-true (string-contains? submit-text "W0: done"))))))

(test-case "/go N starts at specified wave"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-directory dir])
       (make-directory* (build-path dir ".planning"))
       (call-with-output-file (build-path dir ".planning" "PLAN.md")
                              (lambda (out) (display "# Plan\nWave 0\nWave 1\nWave 2" out))
                              #:exists 'truncate)
       (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
       (define result (handler (hasheq 'command "/go" 'input "/go 2")))
       (define submit-text (hash-ref (hook-result-payload result) 'submit))
       (check-true (string-contains? submit-text "wave 2"))))))

(test-case "/implement alias works"
  (with-temp-dir (lambda (dir)
                   (parameterize ([current-directory dir])
                     (make-directory* (build-path dir ".planning"))
                     (call-with-output-file (build-path dir ".planning" "PLAN.md")
                                            (lambda (out) (display "# Plan\nWave 0" out))
                                            #:exists 'truncate)
                     (define handler
                       (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
                     (define result (handler (hasheq 'command "/implement" 'input "/implement")))
                     (check-equal? (hook-result-action result) 'amend)
                     (define submit-text (hash-ref (hook-result-payload result) 'submit))
                     (check-true (string-contains? submit-text "[gsd-planning]"))))))
