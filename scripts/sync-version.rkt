#!/usr/bin/env racket
#lang racket/base

;; sync-version.rkt — Synchronise version strings across the repo.
;;
;; Canonical source: util/version.rkt  (define q-version "X.Y.Z")
;; Targets synced:
;;   1. info.rkt           (define version "X.Y.Z")
;;   2. README.md          version badge + install/verify snippets
;;
;; Usage:
;;   racket scripts/sync-version.rkt          # dry-run (report only)
;;   racket scripts/sync-version.rkt --write  # write changes
;;
;; Exit 0 if all in sync, 1 if drift detected (or --write applied fixes).

(require racket/file
         racket/port
         racket/string)

;; ---------------------------------------------------------------------------
;; Parsing
;; ---------------------------------------------------------------------------

(define VERSION-PAT #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"")

;; Extract q-version from util/version.rkt content.
(define (parse-q-version content)
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; Extract version from info.rkt content.
(define (parse-info-version content)
  (define m (regexp-match #rx"\\(define version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; ---------------------------------------------------------------------------
;; Sync info.rkt
;; ---------------------------------------------------------------------------

(define (sync-info-rkt info-content version)
  ;; Replace (define version "OLD") with (define version "NEW")
  (regexp-replace #rx"\\(define version \"[0-9]+\\.[0-9]+\\.[0-9]+\"\\)"
                  info-content
                  (format "(define version \"~a\")" version)))

;; ---------------------------------------------------------------------------
;; Sync README.md
;; ---------------------------------------------------------------------------

(define (sync-readme readME-content version)
  (define step1
    ;; Version badge: badge/version-OLD-blue → badge/version-NEW-blue
    (regexp-replace #rx"badge/version-[0-9]+\\.[0-9]+\\.[0-9]+-blue"
                    readME-content
                    (format "badge/version-~a-blue" version)))
  (define step2
    ;; Verify snippet: "q version OLD" → "q version NEW"
    (regexp-replace #rx"q version [0-9]+\\.[0-9]+\\.[0-9]+" step1 (format "q version ~a" version)))
  step2)

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define write-mode? (member "--write" (vector->list (current-command-line-arguments))))

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

  ;; --- Summary ---
  (printf "~n---~n")
  (cond
    [(= changes 0)
     (displayln "All targets in sync. No changes needed.")
     (exit 0)]
    [write-mode?
     (printf "~a target(s) synced.~n" changes)
     (exit 0)]
    [else
     (printf "~a target(s) out of sync. Run with --write to fix.~n" changes)
     (exit 1)]))

(main)
