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
;;   Writers emit one canonical symbol.
;;
;;   W5 (R-019): unknown input is a TYPED REJECTION, never the historical
;;   `pending` fallback. `wave-status-symbol` raises `exn:fail:wave-status`
;;   naming the offending value, the accepted spellings and (when a caller
;;   supplies one) the plan/wave context. Comparison-only call sites that must
;;   DESCRIBE an unknown status instead of rejecting it use
;;   `wave-status-comparison-key`, which gives the value an explicit unknown
;;   identity — it never pretends the value means `pending`.
;;   The durable record schema is unchanged: this is a reader contract, not a
;;   format change.
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
;; Note: plan-types.rkt keeps its own Typed Racket row vocabulary ('pending,
;; 'completed, 'skipped) for its legacy consumers, but it DELEGATES the
;; accepted spellings to this module (W5); it no longer owns a second table
;; or a `pending` fallback.

(require racket/string)

;; ============================================================
;; Typed rejection (W5, R-019)
;; ============================================================

;; One unknown status spelling. `value` is the offending value exactly as it
;; was read (string or symbol), `accepted` is the closed list of accepted
;; spellings, and `context` is an optional caller-supplied location (for
;; example "W2 declared in .planning/PLAN.md").
(struct exn:fail:wave-status exn:fail (value accepted context) #:transparent)

(define (accepted-wave-status-spellings)
  (sort (apply append (for/list ([group (in-list wave-status-spelling-groups)])
                        (cdr group)))
        string<?))

(define (wave-status-value->display v)
  (if (or (string? v) (symbol? v))
      (format "~s" v)
      (format "~a" v)))

(define (unknown-wave-status-message value accepted context)
  (string-append
   (format "unknown wave status ~a" (wave-status-value->display value))
   (if context (format " (~a)" context) "")
   ": accepted spellings are "
   (string-join accepted ", ")
   ". Unknown statuses are rejected instead of defaulting to `pending`; "
   "correct the plan/wave document (writers emit one canonical status)."))

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
;; Canonical durable symbol for every reader. Unknown input raises the typed
;; `exn:fail:wave-status` rejection (W5, R-019): a typo must never turn a wave
;; into an actionable `pending` row.
(define (wave-status-symbol s)
  (or (wave-status-canonical s)
      (let ([accepted (accepted-wave-status-spellings)])
        (raise (exn:fail:wave-status (unknown-wave-status-message s accepted #f)
                                     (current-continuation-marks)
                                     s
                                     accepted
                                     #f)))))

;; wave-status-comparison-key : (or/c string? symbol?) -> any/c
;; Comparison ONLY. Known spellings compare by canonical symbol; an unknown
;; spelling compares by its own explicit identity `(unknown <SPELLING>)`, so
;; two unknown values are equal only when they are literally the same claim
;; (never two different typos collapsing into one status).
(define (wave-status-comparison-key s)
  (or (wave-status-canonical s) (list 'unknown (wave-status-spelling s))))

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
;; Projection display spellings
;; ============================================================
;; The projection kernel writes these strings into PLAN.md markers, wave-doc
;; status headers and STATE.md table rows. The spellings are OWNED here (the
;; kernel holds no local table) and preserve the historical output exactly:
;; only done/deferred/failed have a terminal spelling of their own; every
;; other durable status keeps the initial row/header text, because the
;; projection shell must stay byte-identical to the legacy writers.

;; wave-status-display-spelling : any/c -> string?
(define (wave-status-display-spelling s)
  (cond
    [(eq? s 'done) STATUS-DONE]
    [(eq? s 'deferred) STATUS-DEFERRED]
    [(eq? s 'failed) STATUS-FAILED]
    [else STATUS-INBOX]))

;; wave-status-state-table-spelling : any/c -> string?
;; The legacy STATE.md row writer uses "PENDING" (not the "Inbox" display
;; spelling) for the initial state; that historical spelling is preserved.
(define (wave-status-state-table-spelling s)
  (cond
    [(eq? s 'done) STATUS-DONE]
    [(eq? s 'deferred) STATUS-DEFERRED]
    [(eq? s 'failed) STATUS-FAILED]
    [else "PENDING"]))

;; ============================================================
;; Provide
;; ============================================================

(provide exn:fail:wave-status
         exn:fail:wave-status?
         exn:fail:wave-status-value
         exn:fail:wave-status-accepted
         exn:fail:wave-status-context
         accepted-wave-status-spellings
         STATUS-INBOX
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
         wave-status-comparison-key
         done-status?
         deferred-status?
         completed-status?
         retryable-status?
         actionable-status?
         terminal-wave-status?
         WAVE-STATUS-MARKERS
         wave-status-marker
         wave-status-display-spelling
         wave-status-state-table-spelling)
