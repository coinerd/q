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
;;   - Provides skip/fail/pass summary

(require racket/cmdline
         racket/string
         racket/date
         racket/format
         racket/file)

;; Helper: extract last path component (file name)
(define (path-basename p)
  (let-values ([(base name must-be-dir?) (split-path p)])
    (if name
        (path->string name)
        "")))

;; ============================================================
;; Failure classification
;; ============================================================

;; Classify a failure based on reason text and artifacts
(define (classify-failure reason artifact-dir)
  (cond
    [(not reason) 'pass]
    [(string-contains? reason "timeout") 'timeout]
    [(string-contains? reason "crash") 'crash]
    [(string-contains? reason "orphan") 'orphan-cleanup]
    [(string-contains? reason "skip") 'environmental-skip]
    [(string-contains? reason "content") 'content-mismatch]
    [(string-contains? reason "Expected") 'content-mismatch]
    [else 'unknown]))

;; Check if artifact directory contains expected files
(define (check-artifacts dir)
  (define expected-files '("raw-capture.txt" "normalized-capture.txt" "env-summary.txt"))
  (for/list ([f (in-list expected-files)])
    (cons f (file-exists? (build-path dir f)))))

;; Check for credential leakage in artifact files
(define (check-redaction dir)
  (define patterns '("sk-ant-" "Bearer " "api_key" "API_KEY" "ANTHROPIC_API_KEY"))
  (define leaked '())
  (define files-to-check '("raw-capture.txt" "normalized-capture.txt" "env-summary.txt"))
  (for ([f (in-list files-to-check)])
    (define path (build-path dir f))
    (when (file-exists? path)
      (define content (file->string path))
      (for ([pat (in-list patterns)])
        (when (string-contains? content pat)
          (set! leaked (cons (format "~a in ~a" pat f) leaked))))))
  leaked)

;; Scan directory for artifact subdirs
(define (scan-artifact-dir base-dir)
  (define results '())
  (when (directory-exists? base-dir)
    (for ([entry (in-list (directory-list base-dir))])
      (define full-path (build-path base-dir entry))
      (when (directory-exists? full-path)
        (define artifacts (check-artifacts full-path))
        (define leaks (check-redaction full-path))
        (set! results (cons (list (path->string entry) full-path artifacts leaks) results)))))
  results)

;; ============================================================
;; Main
;; ============================================================

(define custom-dir #f)

(command-line #:program "tmux-tui-report"
              #:once-each [("-d" "--dir") dir "Directory to scan for artifacts" (set! custom-dir dir)]
              #:args ()
              (void))

(define scan-dir (or custom-dir (find-system-path 'temp-dir)))

(printf "=== q tmux TUI diagnostic report ===~n")
(printf "Generated: ~a~n"
        (parameterize ([date-display-format 'iso-8601])
          (date->string (current-date) #t)))
(printf "Scan dir: ~a~n~n" scan-dir)

;; Scan for q-tmux-* artifact directories
(define q-tmux-dirs
  (with-handlers ([exn:fail? (lambda (e) '())])
    (filter (lambda (p) (and (directory-exists? p) (string-prefix? (path-basename p) "q-tmux-")))
            (for/list ([e (in-list (directory-list scan-dir))])
              (build-path scan-dir e)))))

(if (null? q-tmux-dirs)
    (printf "No q-tmux artifact directories found in ~a~n" scan-dir)
    (begin
      (printf "Found ~a artifact director(y/ies):~n~n" (length q-tmux-dirs))
      (for ([dir (in-list q-tmux-dirs)])
        (define dir-name (path-basename dir))
        (printf "--- ~a ---~n" dir-name)
        ;; Check for reason file
        (define reason-path (build-path dir "reason.txt"))
        (define reason
          (if (file-exists? reason-path)
              (file->string reason-path)
              #f))
        (define category (classify-failure reason dir))
        (printf "  Category: ~a~n" category)
        (when reason
          (printf "  Reason: ~a~n" (string-trim reason)))
        ;; Check artifact completeness
        (define artifacts (check-artifacts dir))
        (printf "  Artifacts:~n")
        (for ([a (in-list artifacts)])
          (printf "    ~a: ~a~n" (car a) (if (cdr a) "✅" "❌ MISSING")))
        ;; Check redaction
        (define leaks (check-redaction dir))
        (if (null? leaks)
            (printf "  Redaction: ✅ PASS (no credential leakage detected)~n")
            (begin
              (printf "  Redaction: ❌ FAIL (credential patterns found!)~n")
              (for ([l (in-list leaks)])
                (printf "    LEAK: ~a~n" l))))
        (printf "~n"))))

;; Summary
(printf "=== SUMMARY ===~n")
(printf "  Artifact dirs found: ~a~n" (length q-tmux-dirs))
(printf "  Redaction status: ~a~n"
        (if (null? q-tmux-dirs)
            "N/A (no artifacts to check)"
            (let ([all-clean? #t])
              (for ([dir (in-list q-tmux-dirs)])
                (unless (null? (check-redaction dir))
                  (set! all-clean? #f)))
              (if all-clean? "✅ All clean" "❌ Leaks detected"))))
(printf "~n")
