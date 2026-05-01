#!/usr/bin/env racket
#lang racket/base

;; sync-version.rkt — Synchronise version strings across the repo.
;;
;; Canonical source: util/version.rkt  (define q-version "X.Y.Z")
;; Targets synced:
;;   1. info.rkt           (define version "X.Y.Z")
;;   2. README.md          version badge + install/verify snippets
;;   3. All .md files      (--all mode: version refs in docs/)
;;
;; Usage:
;;   racket scripts/sync-version.rkt              # dry-run (report only)
;;   racket scripts/sync-version.rkt --write      # write info.rkt + README.md
;;   racket scripts/sync-version.rkt --write --all  # write all surfaces including docs/
;;
;; Exit 0 if all in sync, 1 if drift detected (or --write applied fixes).

(require racket/file
         racket/port
         racket/string
         racket/path)

;; ---------------------------------------------------------------------------
;; Parsing
;; ---------------------------------------------------------------------------

(define VERSION-PAT #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"")

;; Extract q-version from util/version.rkt content.
;; Handles both `#lang racket` and `#lang typed/racket` formats.
;; Typed Racket may split `(define q-version : String "X.Y.Z")` across lines.
(define (parse-q-version content)
  ;; Find the version string after `(define q-version` — handles multi-line Typed Racket format
  (define start (regexp-match-positions #rx"\\(define q-version" content))
  (cond
    [(not start) #f]
    [else
     (define after (substring content (cdar start)))
     (define m (regexp-match #rx"([0-9]+\\.[0-9]+\\.[0-9]+)" after))
     (and m (cadr m))]))

;; Extract version from info.rkt content.
(define (parse-info-version content)
  (define m (regexp-match #rx"\\(define version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; ---------------------------------------------------------------------------
;; Sync info.rkt
;; ---------------------------------------------------------------------------

(define (sync-info-rkt info-content version)
  (regexp-replace #rx"\\(define version \"[0-9]+\\.[0-9]+\\.[0-9]+\"\\)"
                  info-content
                  (format "(define version \"~a\")" version)))

;; ---------------------------------------------------------------------------
;; Sync README.md
;; ---------------------------------------------------------------------------

(define (sync-readme readme-content version)
  (define step1
    (regexp-replace #rx"badge/version-[0-9]+\\.[0-9]+\\.[0-9]+-blue"
                    readme-content
                    (format "badge/version-~a-blue" version)))
  (define step2
    (regexp-replace #rx"q version [0-9]+\\.[0-9]+\\.[0-9]+" step1 (format "q version ~a" version)))
  step2)

;; ---------------------------------------------------------------------------
;; Sync all .md files (--all mode)
;; ---------------------------------------------------------------------------

;; Files where version refs are historical / should NOT be overwritten
(define EXCLUDED-MD-FILES
  '("CHANGELOG.md" "releasing.md"
                   "why-q.md"
                   "api-stability.md"
                   "compatibility-matrix.md"
                   "package-registry-spec.md"
                   "publish-verify-workflow.md"
                   "sdk-rpc-catalog.md"
                   "EXTENSIONS_INVENTORY.md"))

(define (skip-md-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
      (string-contains? s "/.planning/")
      (string-contains? s "/.pi/")
      (string-contains? s "/examples/README.md")
      (string-contains? s "/docs/tutorials/")
      (string-contains? s "/docs/ecosystem/")
      (string-contains? s "/docs/demos/")
      (string-contains? s "/docs/adr/")
      (string-contains? s "/docs/security.md")))

(define (historical-line? line)
  (regexp-match? #rx"^\\*\\*v[0-9]" (string-trim line)))

;; Replace mismatched version strings in .md content, respecting context.
;; Returns (new-content . num-changes).
(define (sync-md-content content version filename)
  (define fname
    (if (path? filename)
        (path->string (file-name-from-path filename))
        filename))
  (define skip-file? (member fname EXCLUDED-MD-FILES))
  (define lines (string-split content "\n" #:trim? #f))
  (define in-code-block #f)
  (define changes 0)
  (define new-lines
    (for/list ([line (in-list lines)])
      (when (string-prefix? (string-trim line) "```")
        (set! in-code-block (not in-code-block)))
      (cond
        [skip-file? line]
        [(historical-line? line) line]
        [in-code-block line]
        [else
         (define new-line
           (regexp-replace* #rx"[0-9]+\\.[0-9]+\\.[0-9]+"
                            line
                            (lambda (v)
                              (if (equal? v version)
                                  v
                                  (begin
                                    (set! changes (add1 changes))
                                    version)))))
         new-line])))
  (cons (string-join new-lines "\n") changes))

(define (sync-all-md-files version write-mode?)
  (define changes 0)
  (for ([f (in-directory (current-directory))]
        #:when (and (not (skip-md-path? f))
                    (let ([ext (filename-extension f)])
                      (and ext (equal? (bytes->string/utf-8 ext) "md")))))
    (define fname (path->string f))
    (define content (file->string f))
    (define result (sync-md-content content version f))
    (define new-content (car result))
    (define n-changes (cdr result))
    (when (> n-changes 0)
      (printf "  ~a: ~a ref(s) outdated~n" (find-relative-path (current-directory) f) n-changes)
      (when write-mode?
        (call-with-output-file f (lambda (out) (display new-content out)) #:exists 'truncate)
        (printf "  ~a: FIXED~n" (find-relative-path (current-directory) f)))
      (set! changes (+ changes n-changes))))
  changes)

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define write-mode? (member "--write" args))
  (define all-mode? (member "--all" args))

  ;; --- Read canonical version from util/version.rkt ---
  (define util-path (build-path (current-directory) "util" "version.rkt"))
  (unless (file-exists? util-path)
    (displayln "ERROR: util/version.rkt not found")
    (exit 1))
  (define util-content (file->string util-path))
  (define version (parse-q-version util-content))
  (unless version
    (displayln "ERROR: could not parse q-version from util/version.rkt")
    (exit 1))
  (printf "Canonical version: ~a (from util/version.rkt)~n" version)

  (define changes 0)

  ;; --- Sync info.rkt ---
  (define info-path (build-path (current-directory) "info.rkt"))
  (when (file-exists? info-path)
    (define info-content (file->string info-path))
    (define info-version (parse-info-version info-content))
    (cond
      [(not info-version) (printf "  info.rkt: could not parse version~n")]
      [(equal? info-version version) (printf "  info.rkt: OK (~a)~n" info-version)]
      [else
       (printf "  info.rkt: DRIFT ~a → ~a~n" info-version version)
       (when write-mode?
         (define new-content (sync-info-rkt info-content version))
         (call-with-output-file info-path (lambda (out) (display new-content out)) #:exists 'truncate)
         (printf "  info.rkt: FIXED~n"))
       (set! changes (add1 changes))]))

  ;; --- Sync README.md ---
  (define readme-path (build-path (current-directory) "README.md"))
  (when (file-exists? readme-path)
    (define readme-content (file->string readme-path))
    (define synced (sync-readme readme-content version))
    (if (equal? synced readme-content)
        (printf "  README.md: OK~n")
        (begin
          (printf "  README.md: DRIFT detected~n")
          (when write-mode?
            (call-with-output-file readme-path (lambda (out) (display synced out)) #:exists 'truncate)
            (printf "  README.md: FIXED~n"))
          (set! changes (add1 changes)))))

  ;; --- Sync all .md files (--all mode) ---
  (when all-mode?
    (printf "~n--- Syncing docs (*.md) ---~n")
    (define md-changes (sync-all-md-files version write-mode?))
    (set! changes (+ changes md-changes)))

  ;; --- Summary ---
  (printf "~n---~n")
  (cond
    [(= changes 0)
     (displayln "All targets in sync. No changes needed.")
     (exit 0)]
    [write-mode?
     (printf "~a change(s) synced.~n" changes)
     (exit 0)]
    [else
     (printf "~a change(s) out of sync. Run with --write to fix.~n" changes)
     (exit 1)]))

(main)
