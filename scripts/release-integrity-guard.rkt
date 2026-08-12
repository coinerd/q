#!/usr/bin/env racket
#lang racket/base

;; release-integrity-guard.rkt — Release integrity guard for architecture-policy
;; artifacts.
;;
;; Implements the snapshot → run → compare → fail-loud pattern:
;;   1. Snapshot all release-protected artifacts (SHA-256 of raw bytes).
;;   2. Run release automation (or any thunk).
;;   3. Re-snapshot and compare.
;;   4. If any BYTE_STABLE artifact changed, report a loud violation.
;;
;; CLI:
;;   racket scripts/release-integrity-guard.rkt --snapshot
;;     Emit JSON of {path, sha256} pairs for all protected artifacts.
;;
;;   racket scripts/release-integrity-guard.rkt --verify FILE
;;     Compare current artifact state against a saved snapshot JSON file.
;;     Exits 1 on any violation.
;;
;;   racket scripts/release-integrity-guard.rkt --validate-rktd
;;     Validate that all protected .rktd files parse successfully.

(require racket/file
         racket/match
         racket/port
         racket/string
         racket/list
         racket/system
         json
         openssl
         (only-in file/sha1 bytes->hex-string))

(provide load-protected-artifacts
         snapshot-artifacts
         compare-snapshots
         integrity-violations
         format-violation-report
         run-integrity-check
         validate-rktd-syntax
         current-file->bytes
         current-file-exists?
         current-git-diff
         (struct-out integrity-snapshot)
         (struct-out integrity-violation))

;; ═══════════════════════════════════════════════════════════════════════════
;; Dependency injection (for testability — no hardcoded I/O in core logic)
;; ═══════════════════════════════════════════════════════════════════════════

;; Default git-diff implementation: returns files already modified in the
;; working tree or staged (i.e., developer-authored changes before automation).
(define (default-git-diff project-root)
  (define (run-git . args)
    (parameterize ([current-directory project-root])
      (with-output-to-string (lambda () (apply system* (find-executable-path "git") args)))))
  (define working-tree (run-git "diff" "--name-only" "HEAD"))
  (define staged (run-git "diff" "--cached" "--name-only"))
  (define (parse-lines s)
    (filter (lambda (l) (and (string? l) (> (string-length l) 0))) (string-split s "\n" #:trim? #f)))
  (remove-duplicates (append (parse-lines working-tree) (parse-lines staged))))

;; Injectable parameters — tests replace these to simulate mutations.
(define current-file->bytes (make-parameter file->bytes))
(define current-file-exists? (make-parameter file-exists?))
(define current-git-diff (make-parameter default-git-diff))

;; ═══════════════════════════════════════════════════════════════════════════
;; Data structures
;; ═══════════════════════════════════════════════════════════════════════════

;; A point-in-time snapshot of a file's SHA-256 hash.
(struct integrity-snapshot (path sha256) #:transparent)

;; A detected mutation of a protected artifact during release automation.
;; reason is one of: 'byte-changed, 'file-removed, 'file-added
(struct integrity-violation (path before-sha after-sha reason) #:transparent)

;; ═══════════════════════════════════════════════════════════════════════════
;; Core logic
;; ═══════════════════════════════════════════════════════════════════════════

;; Path to the protected artifacts registry (relative to project root).
(define REGISTRY-PATH "docs/architecture/release-protected-artifacts.rktd")

;; load-protected-artifacts : path-string? . -> . (listof (cons/c string? symbol?))
;; Reads the W0 registry and returns (path . stability) pairs for BYTE_STABLE entries.
(define (load-protected-artifacts project-root)
  (define registry-path (build-path project-root REGISTRY-PATH))
  (define data (call-with-input-file registry-path read))
  (define artifacts-list (cdr (assoc 'artifacts (cdr data))))
  (for/list ([entry artifacts-list]
             #:when (eq? (cdr (assoc 'stability entry)) 'BYTE_STABLE))
    (cons (cdr (assoc 'path entry)) 'BYTE_STABLE)))

;; compute-sha256 : path-string? . -> . string?
;; Computes SHA-256 hex digest of a file's raw bytes (uses injected file->bytes).
(define (compute-sha256 abs-path)
  (define bytes ((current-file->bytes) abs-path))
  (bytes->hex-string (sha256-bytes (open-input-bytes bytes))))

;; snapshot-artifacts : path-string? . -> . (listof integrity-snapshot)
;; Takes project root, loads protected list, computes SHA-256 of each file.
(define (snapshot-artifacts project-root)
  (define protected (load-protected-artifacts project-root))
  (for/list ([entry protected])
    (define rel-path (car entry))
    (define abs-path (build-path project-root rel-path))
    (if ((current-file-exists?) abs-path)
        (integrity-snapshot rel-path (compute-sha256 abs-path))
        (integrity-snapshot rel-path #f))))

;; find-snapshot : (listof integrity-snapshot) string? . -> . (or/c integrity-snapshot #f)
(define (find-snapshot snapshots path)
  (for/or ([s snapshots])
    (and (equal? (integrity-snapshot-path s) path) s)))

;; compare-snapshots :
;;   (listof integrity-snapshot) (listof integrity-snapshot) . -> . (listof integrity-violation)
;; Compares before and after snapshots, returns violations for any file
;; whose SHA-256 differs (or that appeared/disappeared).
(define (compare-snapshots before after)
  (define violations '())
  ;; Check for changed or removed files (in before)
  (for ([s-before before])
    (define path (integrity-snapshot-path s-before))
    (define before-sha (integrity-snapshot-sha256 s-before))
    (define s-after (find-snapshot after path))
    (cond
      [(not s-after)
       (set! violations (cons (integrity-violation path before-sha #f 'file-removed) violations))]
      [(not (equal? before-sha (integrity-snapshot-sha256 s-after)))
       (set!
        violations
        (cons (integrity-violation path before-sha (integrity-snapshot-sha256 s-after) 'byte-changed)
              violations))]))
  ;; Check for added files (in after but not in before)
  (for ([s-after after])
    (define path (integrity-snapshot-path s-after))
    (unless (find-snapshot before path)
      (set! violations
            (cons (integrity-violation path #f (integrity-snapshot-sha256 s-after) 'file-added)
                  violations))))
  (reverse violations))

;; integrity-violations : alias for compare-snapshots
(define integrity-violations compare-snapshots)

;; format-violation-report : (listof integrity-violation) . -> . string?
;; Produces a loud, actionable multi-line error message.
(define (format-violation-report violations)
  (define parts
    (for/list ([v violations])
      (format-single-violation v)))
  (string-append "*** RELEASE INTEGRITY VIOLATION ***\n\n" (string-join parts "\n\n")))

;; format-single-violation : integrity-violation? . -> . string?
(define (format-single-violation v)
  (define path (integrity-violation-path v))
  (define before-sha (integrity-violation-before-sha v))
  (define after-sha (integrity-violation-after-sha v))
  (define reason (integrity-violation-reason v))
  (case reason
    [(byte-changed)
     (format (string-append "~a changed during release automation,\n"
                            "but it was not part of the declared release change set.\n\n"
                            "  SHA-256 before: ~a\n"
                            "  SHA-256 after:  ~a\n\n"
                            "Release automation must not mutate architecture policy artifacts.\n"
                            "Review the release step that modified this file.")
             path
             before-sha
             after-sha)]
    [(file-removed)
     (format
      "~a was removed during release automation.\n\n  SHA-256 before: ~a\n\nRelease automation must not delete architecture policy artifacts."
      path
      (or before-sha "unknown"))]
    [(file-added)
     (format
      (string-append
       "~a appeared during release automation.\n\n"
       "  SHA-256 after: ~a\n\n"
       "Release automation must not add new artifacts to the protected set without governance review.")
      path
      (or after-sha "unknown"))]))

;; run-integrity-check :
;;   path-string? (listof integrity-snapshot) procedure? . -> . (or/c #t (listof integrity-violation))
;; Runs the thunk (release automation), then re-snapshots and compares.
;; Returns #t if clean, or the list of violations if mutations detected.
(define (run-integrity-check project-root before-snapshot thunk)
  (thunk)
  (define after-snapshot (snapshot-artifacts project-root))
  (define violations (compare-snapshots before-snapshot after-snapshot))
  (if (null? violations) #t violations))

;; validate-rktd-syntax : path-string? . -> . (or/c #t string?)
;; Reads a .rktd file and attempts (read). Returns #t on success or an
;; error message string on parse failure.
(define (validate-rktd-syntax path)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (call-with-input-file path read)
    #t))

;; ═══════════════════════════════════════════════════════════════════════════
;; Release intent model
;; ═══════════════════════════════════════════════════════════════════════════

;; pre-release-changed-files : path-string? . -> . (listof string?)
;; Returns the set of files already modified before release automation
;; (i.e., developer-authored changes in the working tree or staged).
;; Uses: git diff --name-only HEAD  +  git diff --cached --name-only
;;
;; Intent model:
;;   - If a protected artifact is UNCHANGED at release start → it must be
;;     byte-identical after automation. Any change = VIOLATION.
;;   - If a protected artifact is ALREADY MODIFIED before release start →
;;     that modification is intentional (developer-authored). Automation may
;;     preserve it but must not introduce ADDITIONAL changes.
;;
;; The snapshot → compare pattern already enforces this: the before-snapshot
;; captures the state including developer modifications. If automation doesn't
;; touch the file, before == after (no violation). If automation changes it
;; further, before != after (violation).
(define (pre-release-changed-files project-root)
  ((current-git-diff) project-root))

;; ═══════════════════════════════════════════════════════════════════════════
;; CLI
;; ═══════════════════════════════════════════════════════════════════════════

;; snapshots->json : (listof integrity-snapshot) . -> . jsexpr?
(define (snapshots->json snapshots)
  (for/list ([s snapshots])
    (hasheq 'path (integrity-snapshot-path s) 'sha256 (integrity-snapshot-sha256 s))))

;; json->snapshots : jsexpr? . -> . (listof integrity-snapshot)
(define (json->snapshots jlist)
  (for/list ([j jlist])
    (integrity-snapshot (hash-ref j 'path) (hash-ref j 'sha256))))

(define (cli-snapshot)
  (define project-root (current-directory))
  (define snapshots (snapshot-artifacts project-root))
  (displayln (jsexpr->string (snapshots->json snapshots))))

(define (cli-verify snapshot-file)
  (define project-root (current-directory))
  (define before-snapshots (json->snapshots (string->jsexpr (file->string snapshot-file))))
  (define after-snapshots (snapshot-artifacts project-root))
  (define violations (compare-snapshots before-snapshots after-snapshots))
  (if (null? violations)
      (displayln "OK: All protected artifacts are byte-identical.")
      (begin
        (displayln (format-violation-report violations))
        (exit 1))))

(define (cli-validate-rktd)
  (define project-root (current-directory))
  (define protected (load-protected-artifacts project-root))
  (define all-ok? #t)
  (for ([entry protected])
    (define rel-path (car entry))
    (define abs-path (build-path project-root rel-path))
    (define result (validate-rktd-syntax abs-path))
    (if (eq? result #t)
        (printf "  OK: ~a~n" rel-path)
        (begin
          (set! all-ok? #f)
          (printf "  FAIL: ~a — ~a~n" rel-path result))))
  (if all-ok?
      (displayln "All protected .rktd files parse successfully.")
      (exit 1)))

(define (main)
  (match (vector->list (current-command-line-arguments))
    [(list "--snapshot") (cli-snapshot)]
    [(list "--verify" file) (cli-verify file)]
    [(list "--validate-rktd") (cli-validate-rktd)]
    [_
     (displayln
      "Usage: racket scripts/release-integrity-guard.rkt [--snapshot | --verify FILE | --validate-rktd]")
     (exit 1)]))

(module+ main
  (main))
