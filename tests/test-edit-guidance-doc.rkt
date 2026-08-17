#lang racket/base

;; @speed fast
;; @suite default

;; W2 regression tests for v0.99.79 agent editing guidance and recovery.

(require rackunit
         racket/file
         racket/port
         racket/string
         racket/system
         "../tools/builtins/edit.rkt"
         "../tools/tool.rkt"
         "../util/version.rkt"
         racket/path)

(define q-root (simplify-path (build-path (syntax-source #'here) ".." "..")))
(define runbook-path (build-path q-root "docs" "editing-rules.md"))

(define (runbook-text)
  (call-with-input-file runbook-path port->string))

(define (result-message result)
  (define content (car (tool-result-content result)))
  (if (hash? content)
      (hash-ref content 'text)
      content))

(test-case "editing runbook exists and carries a freshness marker"
  (check-true (file-exists? runbook-path))
  (check-true (string-contains? (runbook-text) (format "verified-against: ~a" q-version))))

(test-case "editing runbook documents safe whole-form workflow"
  (define text (runbook-text))
  (check-true (string-contains? text "raco make <file>"))
  (check-true (string-contains? text "whole form"))
  (check-true (string-contains? text "max-old-text-len"))
  (check-true (string-contains? text "structural edit tool"))
  (check-true (string-contains? text "Never split a form's head from its tail"))
  (check-true (string-contains? text "git restore --source=HEAD --staged --worktree -- <file>")))

(test-case "runbook and too-long error give consistent routing guidance"
  (define path (make-temporary-file "q-edit-guidance-~a.txt"))
  (dynamic-wind
   (lambda () (display-to-file (make-string 2100 #\x) path #:exists 'replace))
   (lambda ()
     (define result
       (tool-edit (hasheq 'path path 'old-text (make-string 2001 #\x) 'new-text "short")))
     (check-true (tool-result-is-error? result))
     (define message (result-message result))
     (define runbook (runbook-text))
     (for ([phrase (in-list '("whole-form replacement" "max-old-text-len"
                                                       "do not split"
                                                       "structural edit tool"))])
       (check-true (string-contains? message phrase) (format "tool message lacks ~s" phrase))
       (check-true (string-contains? runbook phrase) (format "runbook lacks ~s" phrase))))
   (lambda ()
     (when (file-exists? path)
       (delete-file path)))))

(test-case "doc-freshness lint registers the editing runbook"
  (define lint-source
    (call-with-input-file (build-path q-root "scripts" "lint-doc-freshness.rkt") port->string))
  (check-true (string-contains? lint-source "docs/editing-rules.md")))

(test-case "doc-freshness lint fails when a registered document is missing"
  (define root (make-temporary-file "q-doc-freshness-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory* (build-path root "scripts"))
                  (make-directory* (build-path root "util"))
                  (copy-file (build-path q-root "scripts" "lint-doc-freshness.rkt")
                             (build-path root "scripts" "lint-doc-freshness.rkt"))
                  (display-to-file (format "#lang racket/base\n(define q-version ~s)\n" q-version)
                                   (build-path root "util" "version.rkt"))
                  (for ([doc (in-list '("docs/install.md" "docs/getting-started/index.md"
                                                          "docs/extension-guide.md"
                                                          "docs/security-trust-model.md"
                                                          "docs/self-hosting.md"
                                                          "docs/style-guide.md"
                                                          "docs/trust-model.md"
                                                          "docs/workflow-testing.md"
                                                          "docs/architecture/overview.md"
                                                          "docs/event-taxonomy.md"
                                                          "docs/agent-harness-runbook.md"))])
                    (define target (build-path root doc))
                    (make-directory* (path-only target))
                    (display-to-file (format "<!-- verified-against: ~a -->\n" q-version) target))
                  (define racket (find-executable-path "racket"))
                  (define status
                    (parameterize ([current-directory root]
                                   [current-output-port (open-output-nowhere)]
                                   [current-error-port (open-output-nowhere)])
                      (system*/exit-code racket "scripts/lint-doc-freshness.rkt")))
                  (check-not-equal? status 0))
                (lambda () (delete-directory/files root))))

(test-case "documented git restore recovery restores staged and unstaged changes from HEAD"
  (define git (find-executable-path "git"))
  (check-not-false git)
  (define repo (make-temporary-file "q-edit-recovery-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (parameterize ([current-directory repo])
       (check-true (system* git "init" "--quiet"))
       (check-true (system* git "config" "user.email" "q-tests@example.invalid"))
       (check-true (system* git "config" "user.name" "q tests"))
       (display-to-file "#lang racket/base\n(define answer 42)\n" "sample.rkt")
       (check-true (system* git "add" "sample.rkt"))
       (check-true (system* git "commit" "--quiet" "-m" "baseline"))
       (display-to-file "#lang racket/base\n(define answer (begin 42)\n"
                        "sample.rkt"
                        #:exists 'replace)
       (check-true (system* git "add" "sample.rkt"))
       (display-to-file "#lang racket/base\n(define answer (case 42)\n"
                        "sample.rkt"
                        #:exists 'replace)
       (check-true (system* git "restore" "--source=HEAD" "--staged" "--worktree" "--" "sample.rkt"))
       (check-equal? (file->string "sample.rkt") "#lang racket/base\n(define answer 42)\n")
       (check-true (system* git "diff" "--quiet" "--" "sample.rkt"))
       (check-true (system* git "diff" "--cached" "--quiet" "--" "sample.rkt"))))
   (lambda () (delete-directory/files repo))))
