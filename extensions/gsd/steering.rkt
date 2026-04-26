#lang racket/base

;; extensions/gsd/steering.rkt — Mode-aware steering module
;;
;; Wave 1c of v0.21.0: Steering logic that only activates during
;; executing mode. Replaces the ~200 lines of steering in iteration.rkt.
;;
;; DD-2: No steering during exploring — LLMs self-regulate.
;; DD-3: Steering moved into GSD extension, mode-aware.

(require racket/string
         "state-machine.rkt")

(provide gsd-steering-check
         reset-steering-state!
         ;; Exposed for testing
         stall-threshold)

;; ============================================================
;; Configuration
;; ============================================================

;; Number of consecutive identical reads before stall detection
(define stall-threshold 3)

;; ============================================================
;; Mutable steering state
;; ============================================================

;; Track last N (tool-name . argument) pairs for stall detection
(define steering-history (make-parameter '()))
(define steering-history-max 10)

;; ============================================================
;; Public API
;; ============================================================

;; Called from tool-result-post hook.
;; Returns #f if no steering needed, or a steering prompt string.
(define (gsd-steering-check tool-name tool-args result)
  (define mode (gsm-current))
  (cond
    ;; No steering in these modes (DD-2, DD-3)
    [(eq? mode 'idle) #f]
    [(eq? mode 'exploring) #f]
    [(eq? mode 'plan-written) #f]
    [(eq? mode 'verifying) #f]
    ;; Only active during executing
    [(eq? mode 'executing)
     (update-steering-history! tool-name tool-args)
     (detect-stall)]
    [else #f]))

;; Reset steering state (called on mode transition)
(define (reset-steering-state!)
  (steering-history '()))

;; ============================================================
;; Internal: stall detection
;; ============================================================

(define (update-steering-history! tool-name tool-args)
  (cond
    [(string=? tool-name "read")
     (define entry (cons tool-name (simplify-args tool-name tool-args)))
     (define current (steering-history))
     (define new-history (take-at-most (cons entry current) steering-history-max))
     (steering-history new-history)]
    ;; Non-read tool: reset stall counter
    [else (steering-history '())]))

;; Simplify args: for read tool, extract file path; for others, empty
(define (simplify-args tool-name tool-args)
  (cond
    [(string=? tool-name "read")
     (cond
       [(hash? tool-args) (hash-ref tool-args 'path (hash-ref tool-args 'file ""))]
       [(string? tool-args) tool-args]
       [else ""])]
    [else ""]))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))

;; Check if the last stall-threshold reads are all the same file
(define (detect-stall)
  (define history (steering-history))
  (define reads (filter (lambda (e) (string=? (car e) "read")) history))
  (cond
    [(< (length reads) stall-threshold) #f]
    [else
     (define first-read (car reads))
     (define first-args (cdr first-read))
     (define consecutive-same?
       (for/and ([r (take reads stall-threshold)])
         (string=? (cdr r) first-args)))
     (if consecutive-same?
         (format-stall-prompt first-args)
         #f)]))

(define (format-stall-prompt file-path)
  (string-append "⚠️  Stall detected: You've read `"
                 file-path
                 "` "
                 (number->string stall-threshold)
                 "+ times consecutively.\n"
                 "Consider:\n"
                 "- You already have this file's contents — use what you know\n"
                 "- Proceed to the next task in the wave\n"
                 "- Use /skip to skip this wave if stuck\n"))

;; ============================================================
;; take helper (racket/base doesn't have take)
;; ============================================================

(define (take lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (sub1 n)))))
