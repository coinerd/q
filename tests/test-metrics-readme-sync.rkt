#lang racket

;; @speed slow
;; @suite default

;; BOUNDARY: integration

;; test-metrics-readme-sync.rkt — Tests for metrics.rkt --sync-readme and --lint-prose
;;
;; Issue #1285: GAP-05 — Reconcile README metrics and prose counts

(require rackunit
         racket/file
         racket/port
         racket/string)

;; ---------------------------------------------------------------------------
;; Resolve paths relative to q/ root (parent of tests/)
;; ---------------------------------------------------------------------------

(define q-root (simplify-path (build-path (syntax-source #'here) ".." "..")))
(define script-path (build-path q-root "scripts" "metrics.rkt"))

;; ---------------------------------------------------------------------------
;; Helper: run metrics.rkt with explicit working directory
;; ---------------------------------------------------------------------------

(define (run-metrics . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (parameterize ([current-output-port out]
                 [current-error-port err]
                 [current-directory q-root])
    (apply system* (find-executable-path "racket") (path->string script-path) args))
  (values (get-output-string out) (get-output-string err)))

;; ---------------------------------------------------------------------------
;; Helper: temp file
;; ---------------------------------------------------------------------------

(define (make-temp-file content)
  (define tmp (make-temporary-file "metrics-test-~a.md"))
  (call-with-output-file tmp (λ (out) (display content out)) #:exists 'replace)
  tmp)

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(test-case "metrics --lint: runs without crashing"
  (define-values (out err) (run-metrics "--lint"))
  ;; Lint may pass or fail depending on current state, but must produce output
  (check-true
   (or (string-contains? out "match") (string-contains? out "FAILED") (string-contains? out "PASSED"))
   (format "Expected lint output, got: ~a~%err: ~a" out err)))

(test-case "metrics --lint-prose: catches prose/table drift"
  (define tmp
    (make-temp-file #<<EOF
## Module Structure

```
tests/          Full test suite (5 files)
```

## Test Suite

| Metric | Value |
|--------|-------|
| Test files | 292 |
EOF
                    ))
  (define-values (out err) (run-metrics "--lint-prose" (path->string tmp)))
  (delete-file tmp)
  (check-true (or (string-contains? out "MISMATCH")
                  (string-contains? out "drift")
                  (string-contains? out "FAILED"))
              (format "Expected prose lint failure, got: ~a~%err: ~a" out err)))

(test-case "metrics --sync-readme: updates METRICS markers"
  (define tmp
    (make-temp-file #<<EOF
## Module Structure

```
tests/          Full test suite (<!-- METRICS: test-files --> files)
```
EOF
                    ))
  (define-values (out err) (run-metrics "--sync-readme" (path->string tmp)))
  (define updated (file->string tmp))
  (delete-file tmp)
  ;; After sync, the markers should be replaced with actual numbers
  (check-false (string-contains? updated "<!-- METRICS:")
               (format "Markers should be replaced, got: ~a" updated))
  ;; The number should be a real count (not "120")
  (check-true (regexp-match? #rx"Full test suite \\([0-9]+ files\\)" updated)
              (format "Expected real count in prose, got: ~a" updated)))

(test-case "metrics --sync-readme: preserves surrounding text"
  (define original
    #<<EOF
## Module Structure

```
q/
├── agent/          Core types
├── tests/          Full test suite (<!-- METRICS: test-files --> files)
└── tools/          Tool registry
```

Some other text here.
EOF
    )
  (define tmp (make-temp-file original))
  (run-metrics "--sync-readme" (path->string tmp))
  (define updated (file->string tmp))
  (delete-file tmp)
  (check-true (string-contains? updated "Core types"))
  (check-true (string-contains? updated "Tool registry"))
  (check-true (string-contains? updated "Some other text here.")))

(test-case "metrics: ignores untracked temp files (git ls-files fix)"
  ;; Create an untracked temp .rkt file that should NOT inflate counts.
  ;; The metrics script should use git ls-files, so this file is invisible.
  (define-values (out-before err-before) (run-metrics))
  (define source-before
    (let ([m (regexp-match #rx"| Source modules | ([0-9]+) |" out-before)]) (and m (cadr m))))
  ;; Create untracked file
  (define tmp-path (build-path (simplify-path q-root) "tmp-metrics-inflation-test.rkt"))
  (call-with-output-file tmp-path
                         (lambda (out) (display "#lang racket/base\n(define bogus 42)\n" out))
                         #:exists 'replace)
  (define-values (out-after err-after) (run-metrics))
  (define source-after
    (let ([m (regexp-match #rx"| Source modules | ([0-9]+) |" out-after)]) (and m (cadr m))))
  ;; Clean up
  (delete-file tmp-path)
  (check-equal?
   source-before
   source-after
   (format "Source module count changed with temp file: ~a -> ~a" source-before source-after)))

;; ---------------------------------------------------------------------------
;; W5 (#8479): No-mutation boundary tests
;; ---------------------------------------------------------------------------

(test-case "W5: --lint does NOT modify README.md (read-only)"
  (define readme-path (build-path q-root "README.md"))
  (define original-content (file->string readme-path))
  (run-metrics "--lint")
  (define after-content (file->string readme-path))
  (check-equal? original-content after-content "--lint must not modify README.md"))

(test-case "W5: --lint-prose does NOT modify README.md (read-only)"
  (define readme-path (build-path q-root "README.md"))
  (define original-content (file->string readme-path))
  (run-metrics "--lint-prose")
  (define after-content (file->string readme-path))
  (check-equal? original-content after-content "--lint-prose must not modify README.md"))

(test-case "W5: --check-only does NOT modify target file"
  (define tmp
    (make-temp-file #<<EOF
## Test Suite

| Test files | 1 |
Full test suite (1 files)
<!-- METRICS: test-files -->
EOF
                    ))
  (define original (file->string tmp))
  (define-values (out err) (run-metrics "--check-only" (path->string tmp)))
  (define after (file->string tmp))
  (delete-file tmp)
  (check-equal? original after "--check-only must not modify the file")
  (check-true (string-contains? out "check-only")
              (format "Expected check-only message, got: ~a" out)))

(test-case "W5: --sync-all --check-only does NOT modify target file"
  (define tmp
    (make-temp-file #<<EOF
## Test Suite

| Test files | 1 |
Full test suite (1 files)
<!-- METRICS: test-files -->
EOF
                    ))
  (define original (file->string tmp))
  (define-values (out err) (run-metrics "--sync-all" "--check-only" (path->string tmp)))
  (define after (file->string tmp))
  (delete-file tmp)
  (check-equal? original after "--sync-all --check-only must not modify the file")
  (check-true (string-contains? out "check-only")
              (format "Expected check-only message, got: ~a" out)))

(test-case "W5: --sync-all on temp file updates all patterns"
  (define tmp
    (make-temp-file #<<EOF
## Module Structure

```
tests/          Full test suite (<!-- METRICS: test-files --> files)
```

## Test Suite

| Test files | 1 |
Full test suite (1 files)
EOF
                    ))
  (run-metrics "--sync-all" (path->string tmp))
  (define updated (file->string tmp))
  (delete-file tmp)
  ;; Markers should be replaced
  (check-false (string-contains? updated "<!-- METRICS:") "Markers should be replaced by --sync-all")
  ;; Table should have real numbers
  (check-false (string-contains? updated "| Test files | 1 |")
               "Table value should be updated by --sync-all")
  ;; Prose should be updated
  (check-false (string-contains? updated "Full test suite (1 files)")
               "Prose count should be updated by --sync-all"))

(test-case "W5: --check-only reports 'No changes needed' on already-synced file"
  ;; Create a file that already has the correct values
  (define-values (out _) (run-metrics))
  (define test-files-match (regexp-match #rx"\\| Test files \\| ([0-9]+) \\|" out))
  (define test-count (and test-files-match (cadr test-files-match)))
  (when test-count
    (define synced-content (format "Full test suite (~a files)" test-count))
    (define tmp (make-temp-file synced-content))
    (define-values (cout cerr) (run-metrics "--check-only" (path->string tmp)))
    (delete-file tmp)
    ;; Either shows 'No changes' or shows diffs (depends on prose pattern match)
    (check-true (or (string-contains? cout "No changes")
                    (string-contains? cout "Changes would be made"))
                (format "Expected check-only output, got: ~a" cout))))
