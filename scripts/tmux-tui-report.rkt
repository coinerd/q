#!/usr/bin/env racket
#lang racket/base

;; scripts/tmux-tui-report.rkt — Generate diagnostic report from tmux TUI test runs
;;
;; Usage:
;;   racket scripts/tmux-tui-report.rkt                    # scan artifact dirs
;;   racket scripts/tmux-tui-report.rkt --dir /path/to/dir # scan specific dir
;;
;; This script:
;;   - Scans for failure artifacts (raw-capture.txt, normalized-capture.txt, etc.)
;;   - Classifies failures by category (timeout, content-mismatch, crash, orphan)
;;   - Reports redaction status
;;   - Distinguishes retained success dirs from incomplete failure bundles

(require racket/cmdline
         racket/string
         racket/date
         racket/format
         racket/file
         "../util/credential-redaction.rkt")

(provide path-basename
         classify-failure
         check-artifacts
         complete-artifact-bundle?
         check-redaction
         scan-artifact-dir
         render-report)

;; Helper: extract last path component (file name)
(define (path-basename p)
  (let-values ([(base name must-be-dir?) (split-path p)])
    (if name
        (path->string name)
        "")))

;; ============================================================
;; Failure classification
;; ============================================================

(define expected-artifact-files '("raw-capture.txt" "normalized-capture.txt" "env-summary.txt"))

;; Classify a failure based on reason text and artifacts. Directories without a
;; reason file and no artifacts are success dirs (candidate for cleanup).
;; Directories with a reason but incomplete artifacts are incomplete failures.
(define (classify-failure reason artifact-dir)
  (cond
    [(not reason)
     (if (complete-artifact-bundle? artifact-dir) 'failure-artifacts 'success-no-artifacts)]
    [(string-contains? (string-downcase reason) "timeout") 'timeout]
    [(string-contains? (string-downcase reason) "crash") 'crash]
    [(string-contains? (string-downcase reason) "orphan") 'orphan-cleanup]
    [(string-contains? (string-downcase reason) "skip") 'environmental-skip]
    [(string-contains? (string-downcase reason) "content") 'content-mismatch]
    [(string-contains? reason "Expected") 'content-mismatch]
    [(complete-artifact-bundle? artifact-dir) 'failure-artifacts]
    [else 'incomplete-failure]))

;; Check if artifact directory contains expected files.
(define (check-artifacts dir)
  (for/list ([f (in-list expected-artifact-files)])
    (cons f (file-exists? (build-path dir f)))))

(define (complete-artifact-bundle? dir)
  (andmap cdr (check-artifacts dir)))

;; Check for credential leakage in artifact files. Redacted key names such as
;; API_KEY=<REDACTED> are allowed; raw bearer/API key values are not.
(define (check-redaction dir)
  ;; v0.99.50 W3 (TMUX-07): Use the same precision-tuned policy as the
  ;; explorer and harness, avoiding benign Bearer/sk- false positives.
  (for*/fold ([leaked '()])
             ([f (in-list expected-artifact-files)]
              [path (in-value (build-path dir f))]
              #:when (file-exists? path)
              [leak (in-list (find-secret-leaks (file->string path)))])
    (cons (format "~a in ~a" leak f) leaked)))

;; Scan directory for artifact subdirs.
(define (scan-artifact-dir base-dir)
  (define results '())
  (when (directory-exists? base-dir)
    (for ([entry (in-list (directory-list base-dir))])
      (define full-path (build-path base-dir entry))
      (when (and (directory-exists? full-path) (string-prefix? (path-basename full-path) "q-tmux-"))
        (define reason-path (build-path full-path "reason.txt"))
        (define reason (and (file-exists? reason-path) (file->string reason-path)))
        (define category (classify-failure reason full-path))
        (define artifacts (check-artifacts full-path))
        (define leaks (check-redaction full-path))
        (set! results
              (cons (list (path->string entry) full-path category reason artifacts leaks) results)))))
  (sort results string<? #:key car))

(define (all-redaction-clean? results)
  (for/and ([r (in-list results)])
    (null? (list-ref r 5))))

(define (render-report scan-dir [out (current-output-port)])
  (fprintf out "=== q tmux TUI diagnostic report ===~n")
  (fprintf out
           "Generated: ~a~n"
           (parameterize ([date-display-format 'iso-8601])
             (date->string (current-date) #t)))
  (fprintf out "Scan dir: ~a~n~n" scan-dir)

  (define results
    (with-handlers ([exn:fail? (lambda (e) '())])
      (scan-artifact-dir scan-dir)))

  (if (null? results)
      (fprintf out "No q-tmux artifact directories found in ~a~n" scan-dir)
      (begin
        (fprintf out "Found ~a q-tmux director(y/ies):~n~n" (length results))
        (for ([r (in-list results)])
          (define dir-name (list-ref r 0))
          (define category (list-ref r 2))
          (define reason (list-ref r 3))
          (define artifacts (list-ref r 4))
          (define leaks (list-ref r 5))
          (fprintf out "--- ~a ---~n" dir-name)
          (fprintf out "  Category: ~a~n" category)
          (when reason
            (fprintf out "  Reason: ~a~n" (string-trim reason)))
          (cond
            [(or (eq? category 'success-no-artifacts) (eq? category 'retained-success-dir))
             (fprintf out "  Artifacts: not expected (success dir, no failure artifacts)~n")]
            [(eq? category 'incomplete-failure)
             (fprintf out "  Artifacts: INCOMPLETE (missing expected files)~n")
             (for ([a (in-list artifacts)])
               (fprintf out "    ~a: ~a~n" (car a) (if (cdr a) "✅" "❌ MISSING")))]
            [else
             (fprintf out "  Artifacts:~n")
             (for ([a (in-list artifacts)])
               (fprintf out "    ~a: ~a~n" (car a) (if (cdr a) "✅" "❌ MISSING")))])
          (if (null? leaks)
              (fprintf out "  Redaction: ✅ PASS (no credential leakage detected)~n")
              (begin
                (fprintf out "  Redaction: ❌ FAIL (credential patterns found!)~n")
                (for ([l (in-list leaks)])
                  (fprintf out "    LEAK: ~a~n" l))))
          (fprintf out "~n"))))

  (fprintf out "=== SUMMARY ===~n")
  (fprintf out "  q-tmux dirs found: ~a~n" (length results))
  (fprintf out
           "  Redaction status: ~a~n"
           (if (null? results)
               "N/A (no artifacts to check)"
               (if (all-redaction-clean? results) "✅ All clean" "❌ Leaks detected")))
  (fprintf out "~n"))

(module+ main
  (define custom-dir #f)
  (command-line #:program "tmux-tui-report"
                #:once-each
                [("-d" "--dir") dir "Directory to scan for artifacts" (set! custom-dir dir)]
                #:args ()
                (void))
  (render-report (or custom-dir (find-system-path 'temp-dir))))
