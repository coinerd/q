#lang racket

;; q/scripts/contract-metrics.rkt — Count any/c in contract-out across source tree
;;
;; Usage: racket q/scripts/contract-metrics.rkt [--json] [--summary]
;;
;; Scans all .rkt files under src-dirs (excluding tests/, scripts/)
;; and counts occurrences of `any/c` in files that contain `contract-out`.

(require racket/string
         racket/file)

;; ── File discovery ──────────────────────────────────────────

(define src-dirs '("extensions" "agent" "runtime" "tui" "tools" "util"
                   "interfaces" "cli" "llm" "sandbox" "skills" "wiring"))

(define (under-src-dir? p)
  (define str (path->string p))
  ;; Strip leading ./ if present
  (define clean (if (string-prefix? str "./") (substring str 2) str))
  (ormap (lambda (d) (string-prefix? clean (string-append d "/"))) src-dirs))

(define (find-src-files [root "."])
  (for/list ([f (in-directory root)]
             #:when (and (path-has-extension? f #".rkt")
                         (under-src-dir? f)))
    (path->string f)))

;; ── Counting ────────────────────────────────────────────────

(define (count-anyc-in-file content)
  ;; Count any/c occurrences in lines that are inside a contract-out form.
  ;; Simplified: if file contains contract-out, count all any/c.
  ;; This slightly overcounts (any/c in comments/docs) but is good enough
  ;; for tracking reduction progress.
  (if (regexp-match? #rx"contract-out" content)
      (length (regexp-match-positions* #rx"any/c" content))
      0))

;; ── Main ────────────────────────────────────────────────────

(define (run-metrics #:root [root "."] #:json? [json? #f] #:summary? [summary? #f])
  (define files (find-src-files root))
  (define results
    (sort
     (for/list ([f (in-list files)])
       (define content
         (with-handlers ([exn:fail? (lambda (_) "")])
           (file->string f)))
       (define n (count-anyc-in-file content))
       (cons f n))
     > #:key cdr))
  (define total (apply + (map cdr results)))
  (define files-with-anyc (filter (lambda (p) (> (cdr p) 0)) results))

  (cond
    [json?
     (displayln (format "{ \"total\": ~a, \"files\": ~a }"
                        total (length files-with-anyc)))]
    [summary?
     (printf "Total any/c in contract-out: ~a~n" total)
     (printf "Files with any/c: ~a~n" (length files-with-anyc))
     (printf "Total source files scanned: ~a~n" (length files))]
    [else
     (printf "~n── Contract Metrics: any/c in contract-out ──~n~n")
     (printf "Total: ~a any/c across ~a files~n~n" total (length files-with-anyc))
     (for ([p (in-list files-with-anyc)])
       (printf "  ~a  ~a~n" (cdr p) (car p)))
     (printf "~n── Total: ~a any/c ──~n" total)])

  total)

;; ── CLI ─────────────────────────────────────────────────────

(define json-mode #f)
(define summary-mode #f)

(command-line
 #:program "contract-metrics"
 #:once-each
 [("--json") "Output JSON" (set! json-mode #t)]
 [("--summary") "Summary only" (set! summary-mode #t)]
 #:args ()
 (run-metrics #:root "." #:json? json-mode #:summary? summary-mode))
