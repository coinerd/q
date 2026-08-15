#lang racket/base

;; scripts/release-preflight.rkt — Fail-fast release pre-flight (BUG-0007)
;;
;; Runs every cheap, structural release-surface invariant in seconds and exits
;; non-zero on the FIRST violation, so a malformed tag fails the Release
;; workflow (or a local check) before any expensive full-suite job starts.
;;
;; Invariants, in detection-cost order:
;;   1. tag-exists          — the named ref exists in this clone
;;   2. tag-object-type     — the tag object is ANNOTATED (cat-file -t == "tag"),
;;                            with the remediation command in the error message (S1)
;;   3. tag-version-consistency — version derived from the tag == q/info.rkt version
;;   4. manifest-dry-run    — q/scripts/gen-release-manifest.rkt --dry-run <tag>
;;                            renders the whole release surface without publishing (S2)
;;
;; Usage:
;;   racket scripts/release-preflight.rkt <tag>       # e.g. v1.00.00
;;
;; Exit codes: 0 = all invariants hold; 1 = first violated invariant (reported).

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/string
         racket/system)

;; ---------------------------------------------------------------------------
;; Exit protocol
;; ---------------------------------------------------------------------------

(define (usage!)
  (displayln "usage: racket scripts/release-preflight.rkt <tag>")
  (exit 1))

(define (stage-ok fmt . args)
  (printf "  ok    ")
  (apply printf fmt args)
  (newline))

(define (stage-fail stage message)
  (eprintf "  FAIL  [~a] ~a\n" stage message)
  (eprintf "\nrelease preflight: refusing to proceed (fix the invariant above and re-run).\n")
  (exit 1))

;; ---------------------------------------------------------------------------
;; Environment
;; ---------------------------------------------------------------------------

;; Repo root = parent of the directory containing this script (scripts/).
;; Robust across invocation styles: when this module is *run as a script* the
;; run-file path gives us scripts/ directly; when it is merely *required*
;; (e.g. from tests), 'run-file carries no usable path, so we ask git.
(define (find-repo-root)
  (define here (path-only (find-system-path 'run-file)))
  (cond
    [(and here (relative-path? here)) (simplify-path (path->complete-path (build-path here 'up)))]
    [here (simplify-path (build-path here 'up))]
    [else
     (define out (open-output-string))
     (define code
       (parameterize ([current-output-port out])
         (system*/exit-code (find-executable-path "git") "rev-parse" "--show-toplevel")))
     (if (zero? code)
         (string->path (string-trim (get-output-string out)))
         (current-directory))]))

(define repo-root (find-repo-root))

;; ---------------------------------------------------------------------------
;; Git helpers
;; ---------------------------------------------------------------------------

;; Run `git <args...>` in repo-root; return list of lines of stdout.
;; Errors surface as a failed invariant at the call site.
(define (git-lines . args)
  (define args/strings (map ~a args))
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory repo-root]
                   [current-output-port out])
      (apply system*/exit-code (find-executable-path "git") args/strings)))
  (if (zero? code)
      (string-split (string-trim (get-output-string out)) "\n")
      '()))

;; Run `git <args...>`; return (list exit-code stdout stderr) without dying.
(define (git-quiet . args)
  (define args/strings (map ~a args))
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory repo-root]
                   [current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") args/strings)))
  (list code (get-output-string out) (get-output-string err)))

;; ---------------------------------------------------------------------------
;; Invariant 1: tag exists
;; ---------------------------------------------------------------------------

(define (tag-exists? tag)
  (define code (car (git-quiet "rev-parse" "--verify" "--quiet" (string-append "refs/tags/" tag))))
  (zero? code))

(define (check-tag-exists! tag)
  (if (tag-exists? tag)
      (stage-ok "tag-exists: ~a resolves in this clone" tag)
      (stage-fail "tag-exists"
                  (format "tag ~a does not exist in this clone (fetch first, or check the tag name)"
                          tag))))

;; ---------------------------------------------------------------------------
;; Invariant 2: tag object type is annotated
;; ---------------------------------------------------------------------------

(define (git-tag-object-type tag)
  (define code+out (git-quiet "cat-file" "-t" tag))
  (match code+out
    [(list 0 s _) (string-trim s)]
    [_ ""]))

(define remediation-command
  ;; Single source of truth for how to repair a wrong tag object type.
  ;; Kept in sync with q/scripts/gen-release-manifest.rkt (S1).
  "git tag -fa ~a -m \"~a\" && git push origin ~a --force")

(define (check-tag-object-type! tag)
  (define type (git-tag-object-type tag))
  (cond
    [(string=? type "tag") (stage-ok "tag-object-type: ~a is an annotated tag object" tag)]
    [(string=? type "commit")
     (stage-fail "tag-object-type"
                 (string-append (format "~a is a LIGHTWEIGHT tag (points directly at a commit); " tag)
                                "release tags must be annotated so they carry the release message.\n"
                                "  fix: "
                                (format remediation-command tag tag tag)))]
    [else
     (stage-fail "tag-object-type"
                 (format "~a has unexpected object type \"~a\" (expected \"tag\")" tag type))]))

;; ---------------------------------------------------------------------------
;; Invariant 3: tag version == q/info.rkt version
;; ---------------------------------------------------------------------------

;; (define version "1.00.00") in q/info.rkt — same parse contract as
;; scripts/version-surface.rkt:parse-info-version-from-content.
(define (read-info-version)
  (define info-path (build-path repo-root "info.rkt"))
  (define content (file->string info-path))
  (define m
    (regexp-match #rx"\\(define version \"([0-9]+\\.[0-9]+\\.[0-9]+(?:-[A-Za-z0-9.-]+)?)\"" content))
  (and m (cadr m)))

(define (tag->version tag)
  (define m (regexp-match #rx"^v?([0-9]+\\.[0-9]+\\.[0-9]+(?:-[A-Za-z0-9.-]+)?)$" tag))
  (and m (cadr m)))

(define (check-tag-version-consistency! tag)
  (define tag-version (tag->version tag))
  (define info-version (read-info-version))
  (cond
    [(not tag-version)
     (stage-fail "tag-version-consistency"
                 (format "cannot parse a version out of tag name ~a (expected vMAJOR.MINOR.PATCH)"
                         tag))]
    [(not info-version)
     (stage-fail "tag-version-consistency" "cannot parse (define version ...) out of info.rkt")]
    [(string=? tag-version info-version)
     (stage-ok "tag-version-consistency: tag ~a == info.rkt version ~a" tag-version info-version)]
    [else
     (stage-fail
      "tag-version-consistency"
      (format "tag says ~a but info.rkt says ~a — bump info.rkt (and the version surface) or re-tag"
              tag-version
              info-version))]))

;; ---------------------------------------------------------------------------
;; Invariant 4: manifest dry-run renders the release surface
;; ---------------------------------------------------------------------------

(define (check-manifest-dry-run! tag)
  (define out (open-output-string))
  (define err (open-output-string))
  ;; Run from repo-root so the subprocess resolves util/version.rkt etc.
  (define code
    (parameterize ([current-directory repo-root]
                   [current-output-port out]
                   [current-error-port err])
      (system*/exit-code (find-executable-path "racket")
                         (path->string (build-path repo-root "scripts" "gen-release-manifest.rkt"))
                         "--dry-run"
                         tag)))
  (if (zero? code)
      (stage-ok "manifest dry-run: gen-release-manifest.rkt --dry-run ~a rendered the release surface"
                tag)
      (stage-fail "manifest-dry-run"
                  (format "gen-release-manifest.rkt --dry-run ~a failed (exit ~a): ~a"
                          tag
                          code
                          (get-output-string err)))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (preflight! tag)
  (printf "release preflight: ~a\n" tag)
  (check-tag-exists! tag)
  (check-tag-object-type! tag)
  (check-tag-version-consistency! tag)
  (check-manifest-dry-run! tag)
  (printf "release preflight: all invariants hold for ~a\n" tag))

(module+ main
  (match (current-command-line-arguments)
    [(vector tag) (preflight! tag)]
    [_ (usage!)]))

(module+ test
  (require rackunit)
  (test-case "tag->version parses plain and pre-release tags"
    (check-equal? (tag->version "v1.00.00") "1.00.00")
    (check-equal? (tag->version "1.00.00-PRE1") "1.00.00-PRE1")
    (check-false (tag->version "not-a-tag")))
  (test-case "read-info-version finds a version definition"
    (check-match (read-info-version) (? string? _))))
