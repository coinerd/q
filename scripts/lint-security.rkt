#!/usr/bin/env racket
#lang racket/base

;; scripts/lint-security.rkt — Security lint for hardcoded secrets and API keys
;;
;; Scans .rkt source files for patterns that look like hardcoded secrets:
;;   - API key patterns (sk-*, key-*, etc.)
;;   - Hardcoded bearer tokens
;;   - Hardcoded passwords/credentials in test fixtures beyond known exemptions
;;
;; Usage:
;;   cd q/ && racket scripts/lint-security.rkt
;;   cd q/ && racket scripts/lint-security.rkt --verbose
;;
;; Exit 0 if clean, 1 if secrets found.

(require racket/list
         racket/string
         racket/port
         racket/match
         racket/system
         racket/file)

(define verbose? #f)

(define (parse-flags!)
  (for ([arg (in-vector (current-command-line-arguments))])
    (cond
      [(string=? arg "--verbose") (set! verbose? #t)]
      [else (printf "Unknown flag: ~a~n" arg)])))

;; Patterns that look like hardcoded secrets
;; Each: (list name regex exemption-keywords)
;; exemption-keywords: if any of these appear on the same line, skip
(define secret-patterns
  (list (list "OpenAI API key"
              #rx"sk-[a-zA-Z0-9]{20,}"
              '("test-" "dummy-" "example-" "fake-" "***" "****"))
        (list "Generic API key assignment"
              #rx"[a-zA-Z_][a-zA-Z0-9_-]*api[_-]?key[^a-z]*\"[a-zA-Z0-9_-]{16,}\""
              '("test-" "example-" "dummy-"))
        (list "Bearer token literal" #rx"\"Bearer [a-zA-Z0-9._-]{20,}\"" '("test-" "example-"))
        (list "Password assignment"
              #rx"[a-zA-Z_][a-zA-Z0-9_-]*password[^a-z]*\"[^\"]{8,}\""
              '("test-" "example-" "dummy-"))
        (list "Client secret assignment"
              #rx"[a-zA-Z_][a-zA-Z0-9_-]*secret[^a-z]*\"[a-zA-Z0-9_-]{16,}\""
              '("test-" "example-" "dummy-"))))

;; Files to skip entirely
(define skip-dirs '("tests/" ".git/"))

;; Collect .rkt files, excluding test and git directories
(define (collect-files)
  (define output (open-output-string))
  (parameterize ([current-output-port output]
                 [current-error-port output])
    (system "find . -name '*.rkt' -not -path './tests/*' -not -path './.git/*' -print0 | sort -z"))
  (filter (λ (f) (not (string=? f ""))) (string-split (get-output-string output) "\0")))

(define (line-has-exemption? line exemptions)
  (for/or ([exc (in-list exemptions)])
    (string-contains? line exc)))

(define (check-file path)
  (define lines (file->lines path))
  (define findings '())
  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (for ([pat (in-list secret-patterns)])
      (match-define (list name regex exemptions) pat)
      (when (and (regexp-match? regex line) (not (line-has-exemption? line exemptions)))
        (set! findings (cons (list lineno name (string-trim line)) findings)))))
  (reverse findings))

(define (main)
  (parse-flags!)

  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (define files (collect-files))
  (when verbose?
    (printf "Scanning ~a .rkt files...~n" (length files)))

  (define all-findings
    (for/fold ([acc '()]) ([f (in-list files)])
      (define findings (check-file f))
      (when (and verbose? (not (null? findings)))
        (for ([fd (in-list findings)])
          (match-define (list lineno name line) fd)
          (printf "  ~a:~a [~a] ~a~n" f lineno name line)))
      (append acc
              (for/list ([fd (in-list findings)])
                (cons f fd)))))

  (if (null? all-findings)
      (begin
        (printf "Security lint: OK (no hardcoded secrets found)~n")
        (exit 0))
      (begin
        (printf "Security lint: FAILED (~a potential secrets found)~n" (length all-findings))
        (unless verbose?
          (for ([f (in-list all-findings)])
            (match-define (cons file (list lineno name line)) f)
            (printf "  ~a:~a [~a] ~a~n" file lineno name line)))
        (exit 1))))

(main)
