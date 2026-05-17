#lang racket/base

;; extensions/gsd/wave-status.rkt — Canonical wave status constants and helpers
;;
;; v0.32.5 W0: Unified wave status enum for GSD modules.
;; Plain Racket module (not Typed Racket) so all GSD consumers can import it.
;;
;; Status mapping (internal symbol → PLAN.md display string):
;;   'inbox      → "Inbox"
;;   'in-progress → "In-Progress"
;;   'done       → "DONE"
;;   'deferred   → "DEFERRED"
;;   'failed     → "FAILED"
;;
;; Note: plan-types.rkt uses different internal symbols ('pending, 'completed, 'skipped)
;; for backward compatibility with its Typed Racket consumers. This module provides
;; the canonical string-based view used by archive.rkt, wave-docs.rkt, etc.

;; ============================================================
;; Status strings (canonical constants)
;; ============================================================

(define STATUS-INBOX "Inbox")
(define STATUS-IN-PROGRESS "In-Progress")
(define STATUS-DONE "DONE")
(define STATUS-DEFERRED "DEFERRED")
(define STATUS-FAILED "FAILED")

(define ALL-STATUSES (list STATUS-INBOX STATUS-IN-PROGRESS STATUS-DONE STATUS-DEFERRED STATUS-FAILED))

;; ============================================================
;; Predicates
;; ============================================================

(define (wave-status-string? s)
  "Is s a recognized wave status string?"
  (and (string? s) (member s ALL-STATUSES) #t))

(define (terminal-status? s)
  "Is s a terminal status (DONE or DEFERRED)? Case-insensitive."
  (define up (string-upcase s))
  (or (string=? up STATUS-DONE) (string=? up STATUS-DEFERRED)))

(define (done-or-deferred? s)
  "Is s DONE or DEFERRED? (alias for terminal-status?)"
  (terminal-status? s))

(define (active-status? s)
  "Is s a non-terminal status?"
  (not (terminal-status? s)))

(define (normalize-status! s)
  "Normalize a case-variant status string to canonical form.
   Returns the canonical string or #f if not recognized."
  (define up (string-upcase s))
  (cond
    [(string=? up "DONE") STATUS-DONE]
    [(string=? up "DEFERRED") STATUS-DEFERRED]
    [(string=? up "FAILED") STATUS-FAILED]
    [(string=? up "INBOX") STATUS-INBOX]
    [(string=? up "IN-PROGRESS") STATUS-IN-PROGRESS]
    [else #f]))

;; ============================================================
;; Provide
;; ============================================================

(provide STATUS-INBOX
         STATUS-IN-PROGRESS
         STATUS-DONE
         STATUS-DEFERRED
         STATUS-FAILED
         ALL-STATUSES
         wave-status-string?
         terminal-status?
         done-or-deferred?
         active-status?
         normalize-status!)
