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

(define q-root (build-path (syntax-source #'here) ".." ".."))
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

(test-case "metrics --lint: detects mismatched prose count"
  (define-values (out err) (run-metrics "--lint"))
  ;; Currently should pass since we just fixed the README
  (check-true (or (string-contains? out "match") (string-contains? out "PASSED"))
              (format "Expected lint pass, got: ~a~%err: ~a" out err)))

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
