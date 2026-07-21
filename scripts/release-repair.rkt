#!/usr/bin/env racket
#lang racket/base

;; scripts/release-repair.rkt — Diagnostic-only release check.
;;
;; W9 (#8773): Existing-tag repair is diagnostic only.
;; For existing releases: no mutation is allowed.
;; For unpublished tags: user is directed to push the tag for normal pipeline.
;;
;; Modes:
;;   dry-run — verify readiness, no publication (always used)
;;
;; Safety:
;;   - Always dry-run (diagnostic only)
;;   - Refuses if tag version ≠ canonical version
;;   - Refuses if CHANGELOG entry missing
;;   - Never mutates git tags or releases
;;   - Never claims historical gates passed unless rerun
;;
;; Exit codes:
;;   0 — checks pass
;;   1 — checks failed

(provide validate-tag-format
         extract-tag-version
         validate-version-consistency
         validate-changelog-entry
         parse-repair-args
         repair-checks)

(require racket/string
         racket/port
         racket/file
         racket/list
         racket/match)

;; ---------------------------------------------------------------------------
;; Pure logic (no I/O)
;; ---------------------------------------------------------------------------

(define tag-format-rx #px"^v([0-9]+\\.[0-9]+\\.[0-9]+)$")

;; Validate tag format. Returns (list 'ok tag version) or (list 'invalid tag).
(define (validate-tag-format tag)
  (define m (regexp-match tag-format-rx tag))
  (if m
      (list 'ok tag (cadr m))
      (list 'invalid tag)))

;; Extract bare version from tag string (strips leading v).
(define (extract-tag-version tag)
  (define m (regexp-match #px"^v?(.+)$" tag))
  (and m (cadr m)))

;; Validate that tag version matches canonical version.
;; Returns (list 'match) or (list 'mismatch tag-version canonical).
(define (validate-version-consistency tag-version canonical)
  (if (equal? tag-version canonical)
      (list 'match)
      (list 'mismatch tag-version canonical)))

;; Validate CHANGELOG has an entry for the version.
;; Returns (list 'found) or (list 'missing version).
(define (validate-changelog-entry changelog-content version)
  (define pattern-v (format "## v~a" (regexp-quote version)))
  (define pattern-bare (format "## ~a" (regexp-quote version)))
  (if (or (regexp-match? (regexp pattern-v) changelog-content)
          (regexp-match? (regexp pattern-bare) changelog-content))
      (list 'found)
      (list 'missing version)))

;; Parse command-line arguments.
;; Returns (values tag mode) or #f on error.
(define (parse-repair-args args)
  (define tag #f)
  (define mode "dry-run")
  (let loop ([rest args])
    (match rest
      [(list "--tag" t more ...)
       (set! tag t)
       (loop more)]
      [(list "--help" _ ...)
       (displayln "Usage: release-repair.rkt --tag vX.Y.Z [--mode dry-run]")
       #f]
      [(list) (void)]
      [_ (loop (cdr rest))]))
  (if tag
      (values tag mode)
      (begin
        (displayln "ERROR: --tag is required")
        (displayln "Usage: release-repair.rkt --tag vX.Y.Z [--mode dry-run]")
        #f)))

;; Build the list of check descriptors for diagnostic repair.
;; Each check is (cons name thunk) where thunk returns (cons 'pass/'fail message).
;; Dependency-injected for testability: file->string, file-exists?.
(define (repair-checks tag mode file-exists? file->string)
  (define tag-result (validate-tag-format tag))
  (define tag-version (and (eq? (car tag-result) 'ok) (caddr tag-result)))

  (list
   ;; Check 1: Tag format
   (cons "tag-format"
         (lambda ()
           (match tag-result
             [(list 'ok t v) (cons 'pass (format "tag ~a is valid (version ~a)" t v))]
             [(list 'invalid t) (cons 'fail (format "tag ~a is not valid semver" t))])))
   ;; Check 2: Mode validity (must be dry-run)
   (cons "mode-valid"
         (lambda ()
           (if (equal? mode "dry-run")
               (cons 'pass "mode dry-run: diagnostic only, no mutation")
               (cons 'fail (format "mode ~a is invalid; only dry-run is supported" mode)))))
   ;; Check 3: Version consistency
   (cons
    "version-consistency"
    (lambda ()
      (if (not tag-version)
          (cons 'fail "cannot check version without valid tag")
          (let ()
            (define util-content (file->string "util/version.rkt"))
            (define m
              (regexp-match #px"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" util-content))
            (define canonical (and m (cadr m)))
            (match (validate-version-consistency tag-version canonical)
              [(list 'match) (cons 'pass (format "version ~a matches canonical" tag-version))]
              [(list 'mismatch tv c) (cons 'fail (format "tag version ~a ≠ canonical ~a" tv c))])))))
   ;; Check 4: CHANGELOG entry
   (cons "changelog-entry"
         (lambda ()
           (if (not tag-version)
               (cons 'fail "cannot check changelog without valid tag")
               (let ()
                 (define cl-content (file->string "CHANGELOG.md"))
                 (match (validate-changelog-entry cl-content tag-version)
                   [(list 'found) (cons 'pass (format "CHANGELOG has entry for v~a" tag-version))]
                   [(list 'missing v) (cons 'fail (format "CHANGELOG missing entry for v~a" v))])))))
   ;; Check 5: No-mutation safety (always passes in dry-run)
   (cons "no-mutation-safety"
         (lambda () (cons 'pass "dry-run: no release, tag, or asset mutation will occur")))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (define parsed (parse-repair-args argv))
  (when (not parsed)
    (exit 1))
  (define-values (tag mode) (values (car parsed) (cadr parsed)))

  (displayln "=== Release Repair (Diagnostic) ===")
  (printf "Tag:  ~a~n" tag)
  (printf "Mode: ~a (diagnostic only)~n" mode)
  (displayln "")

  (unless (file-exists? "util/version.rkt")
    (displayln "ERROR: Run from q/ project root (util/version.rkt not found)")
    (exit 1))

  (displayln "--- Checks ---")
  (define checks (repair-checks tag mode file-exists? file->string))

  (define results
    (for/list ([c (in-list checks)])
      (define name (car c))
      (define result ((cdr c)))
      (define status (car result))
      (define msg (cdr result))
      (printf "  [~a] ~a: ~a~n" (if (eq? status 'pass) "PASS" "FAIL") name msg)
      (cons name status)))

  (define failures (filter (lambda (r) (eq? (cdr r) 'fail)) results))
  (displayln "")
  (displayln "--- Summary ---")
  (printf "Checks: ~a total, ~a passed, ~a failed~n"
          (length results)
          (- (length results) (length failures))
          (length failures))
  (displayln "")
  (displayln "Diagnostic only. No release, tag, or asset mutation occurred.")
  (displayln "")
  (if (null? failures)
      (displayln "All checks passed. For existing releases, no action needed.")
      (displayln "Checks FAILED. See above for details."))

  (exit (if (null? failures) 0 1)))

(module+ main
  (main))
