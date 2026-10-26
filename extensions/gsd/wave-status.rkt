#lang racket/base

;; extensions/gsd/wave-status.rkt — THE canonical wave status + marker vocabulary
;;
;; v0.32.5 W0: Unified wave status enum for GSD modules.
;; Plain Racket module (not Typed Racket) so all GSD consumers can import it.
;;
;; GSD-SIMPLIFICATION W1: this module is the single owner of BOTH layers that
;; used to be duplicated in wave-docs.rkt (STATUS-CONSIDERED-EQUAL /
;; status->symbol) and campaign-state.rkt (CANONICAL-WAVE-STATUSES /
;; canonical-wave-status). Consumers delegate here; no second table exists.
;;
;; Layer 1 — display/string vocabulary (legacy, unchanged):
;;   'inbox       → "Inbox"
;;   'in-progress → "In-Progress"
;;   'done        → "DONE"
;;   'deferred    → "DEFERRED"
;;   'failed      → "FAILED"
;;
;; Layer 2 — canonical/durable vocabulary (campaign record statuses):
;;   pending in-progress verifying awaiting-delivery done failed interrupted
;;   deferred
;;   Readers accept the documented legacy spellings verbatim ("COMPLETED" →
;;   done, "PENDING"/"NOT STARTED" → pending, "IN PROGRESS" → in-progress).
;;   Writers emit one canonical symbol. Unknown input keeps the historical
;;   `pending` fallback (unchanged compatibility; a typed-error flip is a
;;   separate change).
;;
;; Layer 3 — index-marker vocabulary (PLAN.md `[MARKER]` spells):
;;   "[Inbox]" "[In-Progress]" "[DONE]" "[DEFERRED]" "[FAILED]", plus the
;;   legacy spellings "[COMPLETED]" and "[CANCELLED]" retained VERBATIM
;;   (they are neither rewritten to another status's marker nor dropped).
;;
;; DEFERRED IS DISJOINT: `deferred` is its own canonical status. It is never
;; simultaneously `done`/`completed`, never actionable, and never retryable.
;; `terminal-status?`/`done-or-deferred?` remain the *display/archive grouping*
;; of DONE+DEFERRED and must not be read as "deferred == done".
;;
;; Note: plan-types.rkt keeps its own Typed Racket spellings ('pending,
;; 'completed, 'skipped) for its legacy consumers; that copy is not this
;; module's vocabulary and is out of scope here.

(require racket/string)

;; ============================================================
;; Status strings (canonical display constants)
;; ============================================================

(define STATUS-INBOX "Inbox")
(define STATUS-IN-PROGRESS "In-Progress")
(define STATUS-DONE "DONE")
(define STATUS-DEFERRED "DEFERRED")
(define STATUS-FAILED "FAILED")

(define ALL-STATUSES (list STATUS-INBOX STATUS-IN-PROGRESS STATUS-DONE STATUS-DEFERRED STATUS-FAILED))

;; ============================================================
;; Display predicates (legacy string layer)
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
  "Normalize a case-variant status string to canonical display form.
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
;; Canonical/durable vocabulary (campaign record layer)
;; ============================================================

(define CANONICAL-WAVE-STATUSES
  '(pending in-progress verifying awaiting-delivery done failed interrupted deferred))

(define canonical-wave-statuses CANONICAL-WAVE-STATUSES)

;; canonical-status → accepted spellings (uppercased, whitespace-trimmed).
;; This is the union of the historical campaign-state and wave-docs tables;
;; every spelling below behaved exactly this way before the consolidation.
(define wave-status-spelling-groups
  (list (cons 'pending (list "INBOX" "PENDING" "NOT STARTED"))
        (cons 'in-progress (list "IN-PROGRESS" "IN PROGRESS"))
        (cons 'verifying (list "VERIFYING"))
        (cons 'awaiting-delivery (list "AWAITING-DELIVERY"))
        (cons 'done (list "DONE" "COMPLETED"))
        (cons 'failed (list "FAILED"))
        (cons 'interrupted (list "INTERRUPTED"))
        (cons 'deferred (list "DEFERRED"))))

;; wave-status-spelling : (or/c string? symbol?) -> string?
(define (wave-status-spelling s)
  (string-upcase (string-trim (if (symbol? s)
                                  (symbol->string s)
                                  s))))

;; wave-status-canonical : (or/c string? symbol?) -> (or/c symbol? #f)
;; Pure lookup with NO fallback: #f exactly when the spelling is not part of
;; the documented vocabulary (used by lint to keep flagging unknown headers).
(define (wave-status-canonical s)
  (define up (wave-status-spelling s))
  (for/first ([group (in-list wave-status-spelling-groups)]
              #:when (member up (cdr group)))
    (car group)))

;; wave-status-recognized? : (or/c string? symbol?) -> boolean?
(define (wave-status-recognized? s)
  (and (wave-status-canonical s) #t))

;; wave-status-symbol : (or/c string? symbol?) -> symbol?
;; Canonical comparison key for every reader. Unknown input keeps the
;; historical `pending` fallback so no existing plan changes meaning.
(define (wave-status-symbol s)
  (or (wave-status-canonical s) 'pending))

;; ============================================================
;; Durable status predicates (disjoint statuses, no overwrite)
;; ============================================================

(define (done-status? s)
  (eq? s 'done))
(define (deferred-status? s)
  (eq? s 'deferred))

;; DONE only: deferred/failed/interrupted/awaiting-delivery are NOT completed.
(define (completed-status? s)
  (done-status? s))

(define (retryable-status? s)
  (and (memq s '(failed interrupted)) #t))

(define (actionable-status? s)
  (and (memq s '(pending in-progress verifying failed interrupted)) #t))

;; DONE+DEFERRED grouping for campaign completion; membership does NOT merge
;; the two statuses (see deferred-status?/completed-status?).
(define (terminal-wave-status? s)
  (and (memq s '(done deferred)) #t))

;; ============================================================
;; Index-marker vocabulary (PLAN.md `[MARKER]` spellings)
;; ============================================================

(define WAVE-STATUS-MARKERS
  (list (cons STATUS-INBOX "[Inbox]")
        (cons STATUS-IN-PROGRESS "[In-Progress]")
        (cons STATUS-DONE "[DONE]")
        (cons STATUS-DEFERRED "[DEFERRED]")
        (cons STATUS-FAILED "[FAILED]")
        ;; Legacy spellings retained verbatim. Their marker text is exactly
        ;; the spelling's own bracket form (the same output the historical
        ;; fallback produced), never another status's canonical marker.
        (cons "COMPLETED" "[COMPLETED]")
        (cons "CANCELLED" "[CANCELLED]")))

;; wave-status-marker : any/c -> string?
;; Marker spelling for a status value. Exact legacy fallback retained:
;; an unknown string keeps its own bracket form, a symbol is stringified
;; first, and a non-string/non-symbol becomes "[Inbox]".
(define (wave-status-marker status)
  (cond
    [(string? status)
     (define entry (assoc status WAVE-STATUS-MARKERS))
     (if entry
         (cdr entry)
         (format "[~a]" status))]
    [(symbol? status) (wave-status-marker (symbol->string status))]
    [else "[Inbox]"]))

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
         normalize-status!
         CANONICAL-WAVE-STATUSES
         canonical-wave-statuses
         wave-status-spelling-groups
         wave-status-spelling
         wave-status-canonical
         wave-status-recognized?
         wave-status-symbol
         done-status?
         deferred-status?
         completed-status?
         retryable-status?
         actionable-status?
         terminal-wave-status?
         WAVE-STATUS-MARKERS
         wave-status-marker)
