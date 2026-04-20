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
;; Usage:
;;   cd q/ && racket scripts/milestone-gate.rkt 65    # check milestone #65
;;   cd q/ && racket scripts/milestone-gate.rkt 65 --json  # JSON output

(require racket/file
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         json)

(define args (vector->list (current-command-line-arguments)))
(define json-output? (member "--json" args))

(define milestone-number
  (for/first ([a (in-list args)]
              #:when (regexp-match? #rx"^[0-9]+$" a))
    (string->number a)))

(unless milestone-number
  (printf "Usage: racket scripts/milestone-gate.rkt <milestone-number> [--json]~n")
  (exit 0))

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
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define output (gh-api path))
    (string->jsexpr output)))

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
    [(not ms-data) (list #f "Could not query milestone")]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define ver-m (regexp-match #rx"v([0-9]+\\.[0-9]+\\.[0-9]+)" title))
     (cond
       [(not ver-m) (list #f (format "Cannot extract version from: ~a" title))]
       [else
        (define tag (format "v~a" (cadr ver-m)))
        (define rel-data (gh-api-json (format "releases/tags/~a" tag)))
        (if rel-data
            (list #t (format "Release ~a published" tag))
            (list #f (format "No release for ~a" tag)))])]))

(define (check-changelog-entry)
  ;; Get milestone title to find version
  (define ms-data (gh-api-json (format "milestones/~a" milestone-number)))
  (cond
    [(not ms-data) (list #f "Could not query milestone")]
    [else
     (define title (hash-ref ms-data 'title ""))
     (define ver-m (regexp-match #rx"([0-9]+\\.[0-9]+\\.[0-9]+)" title))
     (cond
       [(not ver-m) (list #f "Cannot extract version")]
       [else
        (define ver (cadr ver-m))
        (cond
          [(not (file-exists? "CHANGELOG.md")) (list #f "CHANGELOG.md not found")]
          [else
           (define content (file->string "CHANGELOG.md"))
           (if (regexp-match? (regexp (format "##.*~a" (regexp-quote ver))) content)
               (list #t (format "CHANGELOG has ~a entry" ver))
               (list #f (format "CHANGELOG missing ~a entry" ver)))])])]))

(define (check-metrics-synced)
  (define exit-code (system/exit-code "racket scripts/metrics.rkt --lint 2>&1"))
  (if (= exit-code 0)
      (list #t "Metrics synced")
      (list #f "Metrics out of sync")))

;; --- Main ---

(define (main)
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
      (list name (car result) (cadr result))))

  (cond
    [json-output?
     (printf "~a~n"
             (format "{~a}"
                     (string-join (for/list ([r (in-list results)])
                                    (format "\"~a\": {\"pass\": ~a, \"detail\": \"~a\"}"
                                            (car r)
                                            (if (cadr r) "true" "false")
                                            (caddr r)))
                                  ", ")))]
    [else
     (printf "=== Milestone #~a Gate Check ===~n~n" milestone-number)
     (for ([r (in-list results)])
       (printf "  [~a] ~a: ~a~n" (if (cadr r) "✓" "✗") (car r) (caddr r)))
     (define all-pass (andmap cadr results))
     (printf "~n~a~n" (if all-pass "All gates passed. ✓" "Some gates failed. ✗"))])

  (exit (if (andmap cadr results) 0 1)))

(main)
