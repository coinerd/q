#lang racket

;; scripts/co-change-report.rkt — Git co-change coupling analysis
;; STABILITY: internal
;;
;; v0.96.7 (F12): Analyze git history for co-change patterns between modules.
;; Reports pairs of files changed together ≥N times (configurable threshold).
;;
;; Usage:
;;   racket scripts/co-change-report.rkt              # last 200 commits, threshold 3
;;   racket scripts/co-change-report.rkt --last 100   # last 100 commits
;;   racket scripts/co-change-report.rkt --threshold 5 # only pairs ≥5 co-changes
;;   racket scripts/co-change-report.rkt --path q/    # restrict to subdirectory

(require racket/match
         racket/string
         racket/list
         racket/file
         racket/system
         racket/port)

;; ═══════════════════════════════════════════════════════════════════
;; CLI argument parsing
;; ═══════════════════════════════════════════════════════════════════

(define last-n 200)
(define threshold 3)
(define path-prefix "")

(for ([arg (in-list (vector->list (current-command-line-arguments)))])
  (cond
    [(string-prefix? arg "--last ")
     (set! last-n (string->number (substring arg (string-length "--last "))))]
    [(string-prefix? arg "--threshold ")
     (set! threshold (string->number (substring arg (string-length "--threshold "))))]
    [(string-prefix? arg "--path ") (set! path-prefix (substring arg (string-length "--path ")))]
    [(or (equal? arg "-h") (equal? arg "--help"))
     (displayln
      "Usage: racket scripts/co-change-report.rkt [--last N] [--threshold N] [--path prefix]")
     (exit 0)]))

;; ═══════════════════════════════════════════════════════════════════
;; Git log parsing
;; ═══════════════════════════════════════════════════════════════════

(define (get-git-log n)
  (define cmd (format "git log --name-only --pretty=format: -~a" n))
  (define output (with-output-to-string (lambda () (system cmd))))
  (for/list ([block (in-list (string-split output "\n\n"))]
             #:when (non-empty-string? (string-trim block)))
    (filter
     non-empty-string?
     (for/list ([line (in-list (string-split block "\n"))])
       (define trimmed (string-trim line))
       (if (or (non-empty-string? trimmed) (string-prefix? trimmed path-prefix)) trimmed "")))))

;; ═══════════════════════════════════════════════════════════════════
;; Co-change counting
;; ═══════════════════════════════════════════════════════════════════

(define (count-co-changes commits)
  (define pair-counts (make-hash))
  (for ([files (in-list commits)])
    ;; Filter to relevant files
    (define relevant
      (filter (lambda (f)
                (and (string-suffix? f ".rkt")
                     (or (string=? path-prefix "") (string-prefix? f path-prefix))))
              files))
    ;; Count each pair
    (for* ([a (in-list relevant)]
           [b (in-list relevant)]
           #:when (string<? a b))
      (hash-update! pair-counts (cons a b) add1 0)))
  pair-counts)

;; ═══════════════════════════════════════════════════════════════════
;; Reporting
;; ═══════════════════════════════════════════════════════════════════

(define (report-hotspot-pairs pair-counts threshold)
  (define sorted
    (sort (for/list ([(pair count) (in-hash pair-counts)]
                     #:when (>= count threshold))
            (list count (car pair) (cdr pair)))
          >
          #:key car))
  (printf "# Co-Change Report (last ~a commits, threshold ≥~a)~n~n" last-n threshold)
  (cond
    [(empty? sorted)
     (printf "✅ No hotspot pairs (all co-change counts < ~a). Healthy baseline.~n~n" threshold)]
    [else
     (printf "| Count | File A | File B |~n")
     (printf "|-------|--------|--------|~n")
     (for ([entry (in-list sorted)])
       (match-define (list count a b) entry)
       (printf "| ~a | `~a` | `~a` |~n" count a b))
     (printf "~n**~a hotspot pairs** found.~n~n" (length sorted))])
  ;; Summary stats
  (define all-counts (hash-values pair-counts))
  (printf "## Summary~n~n")
  (printf "- Commits analyzed: ~a~n" last-n)
  (printf "- Total unique pairs: ~a~n" (hash-count pair-counts))
  (printf "- Hotspot pairs (≥~a): ~a~n" threshold (length sorted))
  (when (not (empty? all-counts))
    (printf "- Max co-change count: ~a~n" (apply max all-counts))
    (printf "- Median co-change count: ~a~n"
            (list-ref (sort all-counts <) (quotient (length all-counts) 2))))
  (printf "~n"))

;; ═══════════════════════════════════════════════════════════════════
;; Main
;; ═══════════════════════════════════════════════════════════════════

(define commits (get-git-log last-n))
(define pair-counts (count-co-changes commits))
(report-hotspot-pairs pair-counts threshold)
