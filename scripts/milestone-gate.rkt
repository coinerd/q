#!/usr/bin/env racket
#lang racket/base

;; scripts/milestone-gate.rkt — Milestone completion gate checklist.
;;
;; Checks 5 gate conditions for a milestone:
;;   1. CI green on main (queries GitHub Actions API)
;;   2. All milestone issues closed
;;   3. GitHub Release published for this version
;;   4. CHANGELOG.md has entry for this version
;;   5. Metrics synced (metrics.rkt --lint passes)
;;
;; W5 (#8522): Release check now verifies assets (tarball + manifest)
;; and provides structured JSON output with release fields.
;;
;; Usage:
;;   cd q/ && racket scripts/milestone-gate.rkt 65    # check milestone #65
;;   cd q/ && racket scripts/milestone-gate.rkt 65 --json  # JSON output

(provide extract-version-from-milestone-title
         validate-release-data
         release-has-asset?
         make-release-check-result)

(require racket/file
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         json)
(require (only-in "../util/error/error-helpers.rkt" with-safe-fallback))

;; ---------------------------------------------------------------------------
;; Pure logic (testable without network)
;; ---------------------------------------------------------------------------

;; Extract version (X.Y.Z) from milestone title like "v0.99.40 ..." or "0.99.40 ..."
;; Returns string or #f.
(define (extract-version-from-milestone-title title)
  (define m (regexp-match #rx"v?([0-9]+\\.[0-9]+\\.[0-9]+)" title))
  (and m (cadr m)))

;; Check if release data has an asset with the given name.
(define (release-has-asset? release-data asset-name)
  (and release-data
       (hash? release-data)
       (let ([assets (hash-ref release-data 'assets '())])
         (for/or ([a (in-list assets)])
           (and (hash? a) (equal? (hash-ref a 'name #f) asset-name))))))

;; Build a release check result hash for JSON output.
(define (make-release-check-result exists tag tarball manifest pass detail)
  (hasheq 'release
          (hasheq 'exists
                  exists
                  'tag
                  (or tag "")
                  'tarball_asset
                  tarball
                  'manifest_asset
                  manifest
                  'pass
                  pass
                  'detail
                  detail)))

;; Validate release data for completeness.
;; Returns (list pass? detail-string release-check-hash).
;; release-data: jsexpr from GitHub API or #f
;; version: string like "0.99.40"
(define (validate-release-data release-data version)
  (define expected-tag (format "v~a" version))
  (define expected-tarball (format "q-~a.tar.gz" version))
  (cond
    ;; No release at all
    [(not release-data)
     (list #f
           (format "No release for ~a" expected-tag)
           (make-release-check-result #f #f #f #f #f (format "No release for ~a" expected-tag)))]
    ;; Release exists but is draft
    [(hash-ref release-data 'draft #f)
     (list #f
           (format "Release ~a is draft" expected-tag)
           (make-release-check-result #t
                                      expected-tag
                                      #f
                                      #f
                                      #f
                                      (format "Release ~a is draft" expected-tag)))]
    ;; Tag mismatch
    [(not (equal? (hash-ref release-data 'tag_name #f) expected-tag))
     (define actual (hash-ref release-data 'tag_name "?"))
     (list #f
           (format "Release tag mismatch: expected ~a, got ~a" expected-tag actual)
           (make-release-check-result #t
                                      actual
                                      #f
                                      #f
                                      #f
                                      (format "Tag mismatch: ~a ≠ ~a" actual expected-tag)))]
    [else
     ;; Check assets
     (define has-tarball (release-has-asset? release-data expected-tarball))
     (define has-manifest (release-has-asset? release-data "release-manifest.json"))
     (define all-ok (and has-tarball has-manifest))
     (define detail
       (cond
         [(and has-tarball has-manifest)
          (format "Release ~a verified: tarball + manifest present" expected-tag)]
         [(and (not has-tarball) (not has-manifest))
          (format "Release ~a missing all assets" expected-tag)]
         [(not has-tarball) (format "Release ~a missing tarball (~a)" expected-tag expected-tarball)]
         [else (format "Release ~a missing manifest" expected-tag)]))
     (list all-ok
           detail
           (make-release-check-result #t expected-tag has-tarball has-manifest all-ok detail))]))

;; ---------------------------------------------------------------------------
;; CLI parsing
;; ---------------------------------------------------------------------------

(define args (vector->list (current-command-line-arguments)))
(define json-output? (member "--json" args))

(define milestone-number
  (for/first ([a (in-list args)]
              #:when (regexp-match? #rx"^[0-9]+$" a))
    (string->number a)))

;; --- GitHub API helpers ---

(define (gh-api path)
  (define token
    (with-handlers ([exn:fail? (λ (_) "")])
      (string-trim (file->string (build-path (find-system-path 'home-dir) "GH_PAT")))))
  (define cmd
    (format
     "curl -s -H \"Authorization: token ~a\" -H \"Accept: application/vnd.github+json\" \"https://api.github.com/repos/coinerd/q/~a\""
     token
     path))
  (with-output-to-string (λ () (system cmd))))

(define (gh-api-json path)
  (with-safe-fallback #f (define output (gh-api path)) (string->jsexpr output)))

;; --- Gate checks ---

(define (check-ci-green)
  ;; Check latest CI workflow run on main
  (define data (gh-api-json "actions/runs?branch=main&per_page=1"))
  (cond
    [(not data) (list #f "Could not query GitHub Actions")]
    [else
     (define runs (hash-ref data 'workflow_runs '()))
     (if (null? runs)
         (list #t "No CI runs found (pass)")
         (let* ([run (car runs)]
                [status (hash-ref run 'status "")]
                [conclusion (hash-ref run 'conclusion "")])
           (if (and (string=? status "completed") (string=? conclusion "success"))
               (list #t (format "CI green (run ~a)" (hash-ref run 'run_number "?")))
               (list #f (format "CI ~a/~a" status conclusion)))))]))

(define (check-milestone-issues-closed)
  ;; Get milestone issues
  (define data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not data) (list #f "Could not query milestone")]
    [else
     (define open (hash-ref data 'open_issues -1))
     (define closed (hash-ref data 'closed_issues -1))
     (if (= open 0)
         (list #t (format "~a issues, all closed" closed))
         (list #f (format "~a open, ~a closed" open closed)))]))

(define (check-release-published)
  ;; Get milestone title to find version tag
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone" #f)]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define version (extract-version-from-milestone-title title))
     (cond
       [(not version) (list #f (format "Cannot extract version from: ~a" title) #f)]
       [else
        (define tag (format "v~a" version))
        (define rel-data (gh-api-json (format "releases/tags/~a" tag)))
        (define result (validate-release-data rel-data version))
        ;; result is (list pass? detail release-hash)
        (list (car result) (cadr result) (caddr result))])]))

(define (check-changelog-entry)
  ;; Get milestone title to find version
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone")]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define version (extract-version-from-milestone-title title))
     (cond
       [(not version) (list #f "Cannot extract version")]
       [else
        (cond
          [(not (file-exists? "CHANGELOG.md")) (list #f "CHANGELOG.md not found")]
          [else
           (define content (file->string "CHANGELOG.md"))
           (if (regexp-match? (regexp (format "##.*~a" (regexp-quote version))) content)
               (list #t (format "CHANGELOG has ~a entry" version))
               (list #f (format "CHANGELOG missing ~a entry" version)))])])]))

(define (check-metrics-synced)
  (define exit-code (system/exit-code "racket scripts/metrics.rkt --lint 2>&1"))
  (if (= exit-code 0)
      (list #t "Metrics synced")
      (list #f "Metrics out of sync")))

;; --- Main ---

(define (main)
  (unless milestone-number
    (printf "Usage: racket scripts/milestone-gate.rkt <milestone-number> [--json]~n")
    (exit 0))
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory~n")
    (exit 1))

  (define checks
    (list (list "CI green" check-ci-green)
          (list "Issues closed" check-milestone-issues-closed)
          (list "Release published" check-release-published)
          (list "CHANGELOG entry" check-changelog-entry)
          (list "Metrics synced" check-metrics-synced)))

  (define results
    (for/list ([c (in-list checks)])
      (define name (car c))
      (define result ((cadr c)))
      (list name
            (car result)
            (cadr result)
            ;; 4th element: release hash for JSON output (or #f)
            (if (>= (length result) 3)
                (caddr result)
                #f))))

  (cond
    [json-output?
     ;; Build structured JSON with release fields
     (define release-info
       (for/first ([r (in-list results)]
                   #:when (and (>= (length r) 4) (hash? (list-ref r 3))))
         (list-ref r 3)))
     (printf "~a~n"
             (jsexpr->string (hasheq 'milestone
                                     milestone-number
                                     'checks
                                     (for/list ([r (in-list results)])
                                       (hasheq 'name (car r) 'pass (cadr r) 'detail (caddr r)))
                                     'release_info
                                     (or release-info (hasheq 'release (hasheq 'exists #f))))))]
    [else
     (printf "=== Milestone #~a Gate Check ===~n~n" milestone-number)
     (for ([r (in-list results)])
       (printf "  [~a] ~a: ~a~n" (if (cadr r) "✓" "✗") (car r) (caddr r)))
     (define all-pass (andmap cadr results))
     (printf "~n~a~n" (if all-pass "All gates passed. ✓" "Some gates failed. ✗"))])

  (exit (if (andmap cadr results) 0 1)))

(module+ main
  (main))
