#lang racket

;; tests/test-context-builder-agents.rkt — Tests for AGENTS.md context wiring (G2.3)
;;
;; Tests:
;; 1. load-agents-context returns empty string when no AGENTS.md
;; 2. load-agents-context finds AGENTS.md in current dir
;; 3. load-agents-context merges instructions from parent + child AGENTS.md
;; 4. build-system-preamble returns formatted preamble

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         "../runtime/context-builder.rkt")

;; Helpers

(define (make-temp-dir)
  (make-temporary-file "q-agents-test-~a" 'directory))

(define (write-agents-md dir content)
  (define p (build-path dir "AGENTS.md"))
  (call-with-output-file p (lambda (out) (display content out)) #:exists 'replace)
  p)

(define (init-git-repo dir)
  (make-directory (build-path dir ".git")))

(define context-builder-agents-tests
  (test-suite "context-builder-agents (G2.3)"

    ;; -------------------------------------------------------
    ;; load-agents-context: no AGENTS.md → empty string
    ;; -------------------------------------------------------
    (test-case "load-agents-context returns empty string when no AGENTS.md"
      (define dir (make-temp-dir))
      (init-git-repo dir)
      (define result (load-agents-context dir))
      (check-equal? result "")
      (delete-directory/files dir))

    ;; -------------------------------------------------------
    ;; load-agents-context: finds AGENTS.md in current dir
    ;; -------------------------------------------------------
    (test-case "load-agents-context finds AGENTS.md in current dir"
      (define dir (make-temp-dir))
      (init-git-repo dir)
      (write-agents-md
       dir
       "# Test Agent\nDescription here.\n\n## System Instructions\nYou are a helpful assistant.\n")
      (define result (load-agents-context dir))
      (check-not-equal? result "")
      (check-true (string-contains? result "helpful assistant"))
      (delete-directory/files dir))

    ;; -------------------------------------------------------
    ;; load-agents-context: merges parent + child AGENTS.md
    ;; -------------------------------------------------------
    (test-case "load-agents-context merges instructions from parent + child AGENTS.md"
      (define parent-dir (make-temp-dir))
      (init-git-repo parent-dir)
      ;; Parent AGENTS.md
      (write-agents-md
       parent-dir
       "# Parent Agent\nParent description.\n\n## System Instructions\nAlways be polite.\n")
      ;; Child dir with its own AGENTS.md
      (define child-dir (build-path parent-dir "subdir"))
      (make-directory child-dir)
      (write-agents-md
       child-dir
       "# Child Agent\nChild description.\n\n## System Instructions\nAlways be concise.\n")
      (define result (load-agents-context child-dir))
      ;; Both instructions should be present (parent first, then child)
      (check-true (string-contains? result "polite") (format "Expected 'polite' in: ~a" result))
      (check-true (string-contains? result "concise") (format "Expected 'concise' in: ~a" result))
      (delete-directory/files parent-dir))

    ;; -------------------------------------------------------
    ;; build-system-preamble: returns formatted preamble
    ;; -------------------------------------------------------
    (test-case "build-system-preamble returns formatted preamble"
      (define dir (make-temp-dir))
      (init-git-repo dir)
      (write-agents-md
       dir
       "# Code Assistant\nA coding helper.\n\n## System Instructions\nWrite clean code.\n")
      (define result (build-system-preamble dir))
      (check-true (string-contains? result "Code Assistant"))
      (check-true (string-contains? result "coding helper"))
      (check-true (string-contains? result "Write clean code"))
      (delete-directory/files dir))

    ;; -------------------------------------------------------
    ;; build-system-preamble: returns empty string when no AGENTS.md
    ;; -------------------------------------------------------
    (test-case "build-system-preamble returns empty string when no AGENTS.md"
      (define dir (make-temp-dir))
      (init-git-repo dir)
      (define result (build-system-preamble dir))
      (check-equal? result "")
      (delete-directory/files dir))

    ;; -------------------------------------------------------
    ;; build-system-preamble: preamble with merged contexts includes all
    ;; -------------------------------------------------------
    (test-case "build-system-preamble merges parent + child into preamble"
      (define parent-dir (make-temp-dir))
      (init-git-repo parent-dir)
      (write-agents-md parent-dir "# Parent\nParent desc.\n\n## System Instructions\nRule one.\n")
      (define child-dir (build-path parent-dir "subdir"))
      (make-directory child-dir)
      (write-agents-md child-dir "# Child\nChild desc.\n\n## System Instructions\nRule two.\n")
      (define result (build-system-preamble child-dir))
      (check-true (string-contains? result "Rule one"))
      (check-true (string-contains? result "Rule two"))
      (delete-directory/files parent-dir))))

(run-tests context-builder-agents-tests)
