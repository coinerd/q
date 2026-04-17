#lang racket

;; tests/test-context-files.rkt — tests for skills/context-files.rkt (GC-26)
;;
;; Tests:
;; 1. parse-agent-file — markdown parsing
;; 2. load-agent-context — loading from directory
;; 3. discover-agents-files — walk-up-from-cwd discovery
;; 4. merge-agent-contexts — merging multiple contexts
;; 5. find-git-root — git root detection

(require rackunit
         rackunit/text-ui
         racket/file
         "../skills/context-files.rkt")

(define context-files-tests
  (test-suite "Context Files (skills/context-files.rkt)"

    ;; -------------------------------------------------------
    ;; parse-agent-file
    ;; -------------------------------------------------------
    (test-case "parse-agent-file extracts name from h1"
      (define ctx (parse-agent-file "# My Agent\nSome description"))
      (check-equal? (agent-context-name ctx) "My Agent"))

    (test-case "parse-agent-file extracts description"
      (define ctx (parse-agent-file "# Agent\nThis is a test agent.\n\n## System Instructions"))
      (check-equal? (agent-context-description ctx) "This is a test agent."))

    (test-case "parse-agent-file extracts system instructions"
      (define ctx
        (parse-agent-file "# Agent\n\nDesc\n\n## System Instructions\nDo the thing.\nBe careful.\n"))
      (check-equal? (agent-context-instructions ctx) "Do the thing.\nBe careful."))

    (test-case "parse-agent-file returns default name for empty content"
      (define ctx (parse-agent-file ""))
      (check-equal? (agent-context-name ctx) "Unnamed Agent"))

    (test-case "parse-agent-file handles content with no sections"
      (define ctx (parse-agent-file "# Basic Agent\nJust a description"))
      (check-equal? (agent-context-instructions ctx) "")
      (check-equal? (agent-context-examples ctx) '())
      (check-equal? (agent-context-tool-preferences ctx) '()))

    ;; -------------------------------------------------------
    ;; load-agent-context
    ;; -------------------------------------------------------
    (test-case "load-agent-context returns #f when no AGENTS.md"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      (check-false (load-agent-context tmp-dir))
      (delete-directory/files tmp-dir))

    (test-case "load-agent-context loads from directory"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      (call-with-output-file
       (build-path tmp-dir "AGENTS.md")
       (lambda (out)
         (display "# Test Agent\n\nA test description.\n\n## System Instructions\nDo stuff." out))
       #:exists 'replace)
      (define ctx (load-agent-context tmp-dir))
      (check-not-false ctx)
      (when ctx
        (check-equal? (agent-context-name ctx) "Test Agent")
        (check-equal? (agent-context-description ctx) "A test description.")
        (check-equal? (agent-context-instructions ctx) "Do stuff."))
      (delete-directory/files tmp-dir))

    ;; -------------------------------------------------------
    ;; find-git-root
    ;; -------------------------------------------------------
    (test-case "find-git-root returns #f when no .git directory"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      (check-false (find-git-root tmp-dir))
      (delete-directory/files tmp-dir))

    (test-case "find-git-root finds .git in current dir"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      (make-directory (build-path tmp-dir ".git"))
      (check-equal? (path->string (find-git-root tmp-dir)) (path->string tmp-dir))
      (delete-directory/files tmp-dir))

    (test-case "find-git-root walks up to parent"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      (define sub-dir (build-path tmp-dir "sub"))
      (make-directory sub-dir)
      (make-directory (build-path tmp-dir ".git"))
      (check-equal? (path->string (find-git-root sub-dir)) (path->string tmp-dir))
      (delete-directory/files tmp-dir))

    ;; -------------------------------------------------------
    ;; discover-agents-files
    ;; -------------------------------------------------------
    (test-case "discover-agents-files finds AGENTS.md in start dir"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      (call-with-output-file (build-path tmp-dir "AGENTS.md")
                             (lambda (out) (display "# Root Agent\n\nRoot instructions." out))
                             #:exists 'replace)
      (define paths (discover-agents-files tmp-dir))
      (check >= (length paths) 1)
      (check-not-false (member (build-path tmp-dir "AGENTS.md") paths))
      (delete-directory/files tmp-dir))

    (test-case "discover-agents-files walks up directory tree"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      ;; Root AGENTS.md
      (call-with-output-file (build-path tmp-dir "AGENTS.md")
                             (lambda (out) (display "# Root Agent" out))
                             #:exists 'replace)
      ;; Sub dir AGENTS.md
      (define sub-dir (build-path tmp-dir "project"))
      (make-directory sub-dir)
      (call-with-output-file (build-path sub-dir "AGENTS.md")
                             (lambda (out) (display "# Project Agent" out))
                             #:exists 'replace)
      ;; .git to mark root
      (make-directory (build-path tmp-dir ".git"))
      (define paths (discover-agents-files sub-dir))
      ;; Should find both
      (check = (length paths) 2)
      (delete-directory/files tmp-dir))

    (test-case "discover-agents-files stops at git root"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      ;; Parent dir with AGENTS.md (outside git)
      (define parent-file (build-path tmp-dir "AGENTS.md"))
      (call-with-output-file parent-file
                             (lambda (out) (display "# Outside Agent" out))
                             #:exists 'replace)
      ;; Git repo root
      (define repo-dir (build-path tmp-dir "repo"))
      (make-directory repo-dir)
      (make-directory (build-path repo-dir ".git"))
      ;; Sub dir inside repo
      (define sub-dir (build-path repo-dir "src"))
      (make-directory sub-dir)
      (call-with-output-file (build-path repo-dir "AGENTS.md")
                             (lambda (out) (display "# Repo Agent" out))
                             #:exists 'replace)
      (define paths (discover-agents-files sub-dir))
      ;; Should NOT include the outside AGENTS.md
      (check-false (member parent-file paths))
      ;; Should include repo root AGENTS.md
      (check-not-false (member (build-path repo-dir "AGENTS.md") paths))
      (delete-directory/files tmp-dir))

    ;; -------------------------------------------------------
    ;; merge-agent-contexts
    ;; -------------------------------------------------------
    (test-case "merge-agent-contexts combines instructions"
      (define ctx1 (agent-context "Root" "Root desc" "Root instructions" '() '()))
      (define ctx2 (agent-context "Local" "Local desc" "Local instructions" '() '()))
      (define merged (merge-agent-contexts (list ctx1 ctx2)))
      (check-equal? (agent-context-instructions merged) "Root instructions\n\nLocal instructions"))

    (test-case "merge-agent-contexts uses last name and description"
      (define ctx1 (agent-context "Root" "Root desc" "Root instructions" '() '()))
      (define ctx2 (agent-context "Local" "Local desc" "Local instructions" '() '()))
      (define merged (merge-agent-contexts (list ctx1 ctx2)))
      (check-equal? (agent-context-name merged) "Local")
      (check-equal? (agent-context-description merged) "Local desc"))

    (test-case "merge-agent-contexts with empty list returns default"
      (define merged (merge-agent-contexts '()))
      (check-equal? (agent-context-name merged) "Default")
      (check-equal? (agent-context-instructions merged) ""))

    (test-case "merge-agent-contexts merges examples"
      (define ex1 (list (hasheq 'title "Ex1" 'user "hi" 'agent "hello" 'raw "hi")))
      (define ex2 (list (hasheq 'title "Ex2" 'user "bye" 'agent "goodbye" 'raw "bye")))
      (define ctx1 (agent-context "A" "" "" ex1 '()))
      (define ctx2 (agent-context "B" "" "" ex2 '()))
      (define merged (merge-agent-contexts (list ctx1 ctx2)))
      (check = (length (agent-context-examples merged)) 2))

    ;; -------------------------------------------------------
    ;; Integration: discover + load + merge
    ;; -------------------------------------------------------
    (test-case "integration: discover, load, merge from temp directory tree"
      (define tmp-dir (make-temporary-file "ctx-test-~a" 'directory))
      ;; Root level
      (make-directory (build-path tmp-dir ".git"))
      (call-with-output-file
       (build-path tmp-dir "AGENTS.md")
       (lambda (out)
         (display "# Root Agent\n\nRoot desc.\n\n## System Instructions\nAlways be helpful." out))
       #:exists 'replace)
      ;; Sub project
      (define sub (build-path tmp-dir "project"))
      (make-directory sub)
      (call-with-output-file
       (build-path sub "AGENTS.md")
       (lambda (out)
         (display "# Project Agent\n\nProject desc.\n\n## System Instructions\nFollow project rules."
                  out))
       #:exists 'replace)
      ;; Discover
      (define paths (discover-agents-files sub))
      (check = (length paths) 2)
      ;; Load each
      (define contexts
        (filter-map (lambda (p)
                      (define content (file->string p))
                      (parse-agent-file content))
                    paths))
      (check = (length contexts) 2)
      ;; Merge
      (define merged (merge-agent-contexts contexts))
      (check-equal? (agent-context-name merged) "Project Agent")
      (check-true (string-contains? (agent-context-instructions merged) "Always be helpful."))
      (check-true (string-contains? (agent-context-instructions merged) "Follow project rules."))
      (delete-directory/files tmp-dir))))

(run-tests context-files-tests)
