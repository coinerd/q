#!/usr/bin/env racket
#lang racket/base

;; gen-release-notes.rkt — Generate release notes from CHANGELOG.md.
;;
;; Usage:
;;   racket scripts/gen-release-notes.rkt [VERSION]
;;
;; If VERSION is not given, reads from util/version.rkt.
;; Outputs formatted Markdown to stdout.

(require racket/file
         racket/string
         racket/port)

(define (parse-q-version content)
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

(define (version-header? line version)
  ;; Match lines like:
  ;;   ## [0.10.2] — 2026-04-16
  ;;   ## v0.10.2
  ;;   ## 0.10.2
  (define trimmed (string-trim line))
  (or (and (string-prefix? trimmed (format "## [~a]" version)) #t)
      (and (string-prefix? trimmed (format "## v~a" version)) #t)
      (and (string-prefix? trimmed (format "## ~a " version)) #t)
      (string=? trimmed (format "## ~a" version))))

(define (section-header? line)
  ;; Match any ## heading that starts a new version section
  (define trimmed (string-trim line))
  (and (string-prefix? trimmed "## ") (regexp-match? #rx"^## (\\[|v?[0-9])" trimmed)))

(define (extract-changelog-section content version)
  (define lines (string-split content "\n"))
  (define-values (found collecting result-lines)
    (for/fold ([found #f]
               [collecting #f]
               [result '()])
              ([line (in-list lines)])
      (cond
        [(and (not found) (version-header? line version)) (values #t #t (cons line result))]
        [(and collecting (section-header? line)) (values found #f result)]
        [collecting (values found #t (cons line result))]
        [else (values found #f result)])))
  (if found
      (string-join (reverse result-lines) "\n")
      #f))

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define version
    (cond
      [(pair? args) (car args)]
      [else
       (define util-path (build-path (current-directory) "util" "version.rkt"))
       (unless (file-exists? util-path)
         (displayln "ERROR: util/version.rkt not found")
         (exit 1))
       (define v (parse-q-version (file->string util-path)))
       (unless v
         (displayln "ERROR: could not parse version")
         (exit 1))
       v]))

  (printf "Generating release notes for v~a~n" version)

  (define changelog-path (build-path (current-directory) "CHANGELOG.md"))
  (unless (file-exists? changelog-path)
    (displayln "ERROR: CHANGELOG.md not found")
    (exit 1))

  (define changelog-content (file->string changelog-path))
  (define section (extract-changelog-section changelog-content version))

  (cond
    [section
     (define cleaned (string-trim section))
     (printf "~n~a~n" cleaned)]
    [else
     (printf "WARNING: No CHANGELOG section found for v~a~n" version)
     (printf "Falling back to summary header.~n~n")
     (printf "## q v~a~n~n" version)
     (printf "See [CHANGELOG.md](CHANGELOG.md) for details.~n")])

  ;; Footer
  (printf "~n---~n")
  (printf
   "**Install**: `curl -fsSL https://raw.githubusercontent.com/coinerd/q/main/scripts/install.sh | bash`~n")
  (printf "**Verify**: `racket main.rkt --version`~n"))

(main)
