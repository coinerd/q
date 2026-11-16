#lang racket/base

;; @speed fast
;; @suite workflows
;; @boundary unit
;; @isolation temp-dir
;; @mutates temp-dir
;; @timeout 180

;; tests/test-ci-validate-workflows.rkt — testing-framework refactor §11.5.
;;
;; Red-first contract for scripts/ci/validate-workflows.rkt, which replaces the
;; `pip install pyyaml` + `python3 -c "import yaml"` step of the `lint` job:
;;   - structurally valid fixtures pass (and an opaque block scalar may contain
;;     anything — shell quotes, brackets, tabs — without a false positive);
;;   - every malformation class the old check rejected fails closed: duplicate
;;     top-level key, tab indentation, unclosed flow collection, unclosed
;;     quote, inconsistent indentation;
;;   - a read failure (or an empty declaration tree) is a failure, never a
;;     success line;
;;   - the real `.github/` tree validates, and the CLI exits 0 there.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../scripts/ci/validate-workflows.rkt")

(define-runtime-path here "test-ci-validate-workflows.rkt")
(define repo-root (simplify-path (build-path (path-only here) "..")))
(define script (build-path repo-root "scripts" "ci" "validate-workflows.rkt"))

(define (problem-matching problems rx)
  (for/first ([p (in-list problems)]
              #:when (regexp-match? rx p))
    p))

(define valid-workflow
  (string-append "name: CI\n"
                 "on:\n"
                 "  push:\n"
                 "    branches: [main]\n"
                 "jobs:\n"
                 "  build:\n"
                 "    runs-on: ubuntu-latest\n"
                 "    steps:\n"
                 "      - uses: actions/checkout@v7\n"
                 "      - name: run\n"
                 "        run: |\n"
                 "          if [ -n \"$FOO\" ]; then\n"
                 "            echo \"don't panic\" | sed 's/]/x/'\n"
                 "          fi\n"
                 "        with:\n"
                 "          a: [1, 2, {b: \"c\"}]\n"))

(define validate-suite
  (test-suite "scripts/ci/validate-workflows.rkt"

    (test-case "a structurally valid workflow has no problems"
      (check-equal? (yaml-problems valid-workflow) '()))

    (test-case "a duplicate top-level key fails closed"
      (define problems (yaml-problems "name: CI\nname: CI again\njobs:\n  a: 1\n"))
      (check-not-false (problem-matching problems #rx"duplicate top-level key")))

    (test-case "tab indentation fails closed"
      (define problems (yaml-problems "jobs:\n\truns-on: ubuntu-latest\n"))
      (check-not-false (problem-matching problems #rx"tab indentation")))

    (test-case "an unclosed flow collection fails closed"
      (define problems (yaml-problems "jobs:\n  a:\n    branches: [main, dev\n"))
      (check-not-false (problem-matching problems #rx"unclosed flow collection")))

    (test-case "an unmatched closing bracket fails closed"
      (define problems (yaml-problems "jobs:\n  a: 1]\n"))
      (check-not-false (problem-matching problems #rx"unmatched closing")))

    (test-case "an unclosed quote fails closed"
      (define problems (yaml-problems "jobs:\n  a:\n    description: \"unclosed\n"))
      (check-not-false (problem-matching problems #rx"unclosed double quote")))

    (test-case "inconsistent indentation fails closed"
      (define problems (yaml-problems "jobs:\n  a:\n      x: 1\n    y: 2\n"))
      (check-not-false (problem-matching problems #rx"inconsistent indentation")))

    (test-case "a top-level line that is not a mapping entry fails closed"
      (define problems (yaml-problems "name: CI\njust some words\n"))
      (check-not-false (problem-matching problems #rx"not a mapping entry")))

    (test-case "indented top-level content fails closed"
      (define problems (yaml-problems "  name: CI\n"))
      (check-not-false (problem-matching problems #rx"must not be indented")))

    (test-case "a block scalar is opaque: shell quotes, brackets and tabs are not YAML"
      ;; The F1 class of false positive: `run: |` bodies are programs. Their
      ;; unbalanced brackets, apostrophes and tabs must never be reported.
      (define text
        (string-append "jobs:\n"
                       "  a:\n"
                       "    steps:\n"
                       "      - run: |\n"
                       "          if [ -n \"$X\" ]; then echo \"it's ]]\"; fi\n"
                       "          \tprintf '%s\\n' \"{unbalanced\"\n"
                       "        continue-on-error: false\n"))
      (check-equal? (yaml-problems text) '()))

    (test-case "a realistic workflow-shaped document is accepted"
      (check-equal?
       (yaml-problems (string-append "name: CI\n"
                                     "on:\n"
                                     "  push:\n"
                                     "    branches: [main]\n"
                                     "  workflow_call:\n"
                                     "    inputs:\n"
                                     "      trial-id:\n"
                                     "        description: 'Coordinator-chosen id'\n"
                                     "        required: false\n"
                                     "        type: string\n"
                                     "concurrency:\n"
                                     "  group: ci-${{ github.ref }}\n"
                                     "jobs:\n"
                                     "  lint:\n"
                                     "    runs-on: ubuntu-latest\n"
                                     "    steps:\n"
                                     "      - uses: actions/checkout@v7\n"
                                     "      - name: check\n"
                                     "        env:\n"
                                     "          A: ${{ inputs.trial-id != '' && 'x' || 'y' }}\n"
                                     "        run: racket scripts/ci/validate-workflows.rkt\n"))
       '()))

    (test-case "the declaration-file inventory covers workflows and actions"
      (define files (map path->string (workflow-declaration-files repo-root)))
      (check-not-false (member ".github/workflows/ci.yml" files))
      (check-not-false (member ".github/actions/setup-racket/action.yml" files))
      (check-equal? files
                    (map path->string (workflow-declaration-files repo-root))
                    "the inventory order is deterministic"))

    (test-case "the real .github tree validates"
      (define-values (problems count) (validate-workflow-files repo-root))
      (check-equal? problems '())
      (check-true (>= count 14) (format "only ~a files validated" count)))

    (test-case "no declaration file spawns an interpreter from outside the toolchain"
      ;; §11.5 / Phase B invariant: the CI setup and execution path is pure
      ;; Racket. This action-level guard covers `.github/actions/**` too (the
      ;; restore action's `restore-ms` stamps used to shell out to python3),
      ;; so a reintroduced interpreter fails this fast test instead of only a
      ;; grep in review. Comment lines may still discuss the retired Python
      ;; controllers; only an executable line counts as a reintroduced
      ;; dependency.
      (define (interpreter-lines path)
        (for/list ([ln (in-list (string-split (file->string path) "\n"))]
                   #:when (and (not (string-prefix? (string-trim ln) "#"))
                               (string-contains? (string-downcase ln) "python")))
          (string-trim ln)))
      (define offenders
        (for*/list ([f (in-list (workflow-declaration-files repo-root))]
                    [ln (in-list (interpreter-lines (build-path repo-root f)))])
          (format "~a: ~a" (path->string f) ln)))
      (check-equal? offenders '() "no .github declaration may invoke an interpreter"))

    (test-case "the CLI passes on the real tree and prints exactly one success line"
      (define-values (code out) (run-cli repo-root))
      (check-equal? code 0 out)
      (define success-lines
        (filter (lambda (l) (string-prefix? l "workflow-yaml-ok:"))
                (filter non-empty-string? (string-split out "\n"))))
      (check-equal? (length success-lines) 1 out))

    (test-case "the CLI fails closed on a malformed workflow and names file + problem"
      (define dir (make-temporary-file "q-validate-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define wf-dir (build-path dir ".github" "workflows"))
                      (make-directory* wf-dir)
                      (display-to-file "name: A\nname: B\n" (build-path wf-dir "broken.yml"))
                      (define-values (code out) (run-cli dir))
                      (check-equal? code 1 out)
                      (check-false (string-contains? out "workflow-yaml-ok:") out)
                      (check-true (string-contains? out "broken.yml") out)
                      (check-true (string-contains? out "duplicate top-level key") out))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "the CLI fails closed on a read failure (never prints success)"
      (define dir (make-temporary-file "q-validate-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define wf-dir (build-path dir ".github" "workflows"))
                      (make-directory* wf-dir)
                      ;; A directory where a workflow file is expected: it cannot be read as
                      ;; a file, so the validator must report it instead of skipping it.
                      (make-directory* (build-path wf-dir "not-a-file.yml"))
                      (define-values (code out) (run-cli dir))
                      (check-equal? code 1 out)
                      (check-false (string-contains? out "workflow-yaml-ok:") out)
                      (check-true (string-contains? out "not-a-file.yml") out)
                      (check-true (string-contains? out "cannot read") out))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "an empty declaration tree is a failure, not a vacuous success"
      (define dir (make-temporary-file "q-validate-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define-values (code out) (run-cli dir))
                      (check-equal? code 1 out)
                      (check-false (string-contains? out "workflow-yaml-ok:") out)
                      (check-true (string-contains? out "nothing was validated") out))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))))

;; The CLI is the CI surface, so the tests exercise the process, not only the
;; pure functions.
(define (run-cli root)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (system*/exit-code (find-executable-path "racket")
                         (path->string script)
                         "--root"
                         (path->string root))))
  (values code (string-append (get-output-string out) (get-output-string err))))

(module+ test
  (require rackunit/text-ui)
  (run-tests validate-suite))

(module+ main
  (require rackunit/text-ui)
  (exit (run-tests validate-suite)))
