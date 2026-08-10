#lang racket/base

;; extensions/gsd/projection-kernel.rkt — PURE GSD projection kernel
;;
;; v0.99.89 W2 "Plan/State Projection Kernel": compute PLAN.md / wave-doc /
;; STATE.md projection changes PURELY, as neutral domain data, so the atomic
;; effect shell (projection-effects.rkt) can apply them without partial
;; tracking. Mirror of the W1 pure transition kernel discipline: no
;; filesystem, GitHub, runtime, or event-bus imports — only racket/base +
;; racket/string (both pure).
;;
;; A "projection set" is a list of neutral entries that describe every file
;; change a projection transition needs:
;;
;;   (cons 'plan-index   content)          → .planning/PLAN.md
;;   (cons 'wave-doc     (cons idx content)) → .planning/waves/W{idx}-{slug}.md
;;   (cons 'state-table  content)          → .planning/STATE.md
;;
;; The kernel never touches paths: path resolution and write atomicity live in
;; projection-effects.rkt (the shell). The kernel is the single source of
;; truth for the CONTENT of every projection transition, so mark-wave-status! /
;; update-state-table! / completion and the crash-repair reconciliation all
;; agree byte-for-byte.
;;
;; The transforms below are textually identical to the legacy inline logic
;; they replace (wave-docs.rkt update-index-line / write-wave-doc! header
;; rebuild, wave-completion.rkt update-state-table! row replace) — the W0
;; golden-trace oracle proves the equivalence.

(require racket/string)

;; ============================================================
;; Neutral status display data
;; ============================================================

;; Wave status (campaign symbol) → display string used in PLAN.md index
;; markers and wave-doc status headers (wave-status.rkt constants).
(define (wave-status->projection-string s)
  (cond
    [(eq? s 'done) "DONE"]
    [(eq? s 'deferred) "DEFERRED"]
    [(eq? s 'failed) "FAILED"]
    [else "Inbox"]))

;; Wave status → display string used in STATE.md table rows.
;; The legacy row writer writes "PENDING" for the initial state and
;; DONE/FAILED/DEFERRED for terminal states; reconcile maps every other
;; durable status back to the initial row text.
(define (wave-status->state-string s)
  (cond
    [(eq? s 'done) "DONE"]
    [(eq? s 'deferred) "DEFERRED"]
    [(eq? s 'failed) "FAILED"]
    [else "PENDING"]))

;; ============================================================
;; PLAN.md index marker data
;; ============================================================

(define PROJECTION-STATUS-MARKERS
  (list (cons "Inbox" "[Inbox]")
        (cons "In-Progress" "[In-Progress]")
        (cons "DONE" "[DONE]")
        (cons "DEFERRED" "[DEFERRED]")
        (cons "FAILED" "[FAILED]")))

(define (status->marker status)
  (cond
    [(string? status)
     (define entry (assoc status PROJECTION-STATUS-MARKERS))
     (if entry
         (cdr entry)
         (format "[~a]" status))]
    [(symbol? status) (status->marker (symbol->string status))]
    [else "[Inbox]"]))

;; ============================================================
;; Pure projection transforms (text → text)
;; ============================================================

;; PLAN.md: replace the status marker on the index line for one wave.
;; Byte-identical to wave-docs.rkt update-index-line.
(define (project-plan-index-update text wave-idx status)
  (define lines (string-split text "\n"))
  (define update-rx
    (regexp (string-append "^([-*] +)\\[([A-Za-z-]+)\\]( +W" (number->string wave-idx) ":.*)$")))
  (define new-lines
    (for/list ([line lines])
      (define m (regexp-match update-rx line))
      (if m
          (string-append (cadr m) (status->marker status) (list-ref m 3))
          line)))
  (string-join new-lines "\n"))

;; Wave doc: replace the status header, preserving the body.
;; Byte-identical to wave-docs.rkt write-wave-doc! (strip-status-header +
;; header rebuild).
(define wave-header-full-rx #rx"^# Wave [0-9]+\nStatus: [^\n]+\n\n(.*)$")

(define (project-wave-doc-update text wave-idx status)
  (define m (regexp-match wave-header-full-rx text))
  (define content
    (if m
        (cadr m)
        text))
  (string-append (format "# Wave ~a\nStatus: ~a\n\n" wave-idx status) content))

;; STATE.md: replace the status field of one wave's table row.
;; Byte-identical to wave-completion.rkt update-state-table!.
(define (project-state-row-update content wave-idx status)
  (define lines (string-split content "\n"))
  (define prefix (format "| W~a |" wave-idx))
  (define new-lines
    (for/list ([line lines])
      (if (string-prefix? line prefix)
          (let* ([parts (string-split line "|" #:trim? #f)])
            (if (>= (length parts) 5)
                (string-join (list (list-ref parts 0)
                                   (list-ref parts 1)
                                   (list-ref parts 2)
                                   (string-append " " status " ")
                                   (list-ref parts 4))
                             "|")
                line))
          line)))
  (string-join new-lines "\n"))

;; ============================================================
;; Projection sets (neutral effect plans)
;; ============================================================

(define PROJECTION-KINDS '(plan-index wave-doc state-table))

(define (projection-kinds)
  PROJECTION-KINDS)

(define (projection-file-kind? x)
  (and (symbol? x) (memq x PROJECTION-KINDS)))

;; Entry accessors. Entries are (cons 'plan-index content),
;; (cons 'wave-doc (cons idx content)) or (cons 'state-table content).
(define (projection-entry-kind e)
  (car e))

(define (projection-entry-content e)
  (if (eq? (car e) 'wave-doc)
      (cddr e)
      (cdr e)))

(define (projection-entry-wave-idx e)
  (if (eq? (car e) 'wave-doc)
      (cadr e)
      #f))

(define (projection-set? x)
  (and (list? x)
       (for/and ([e x])
         (and (pair? e) (projection-file-kind? (car e)) (string? (projection-entry-content e))))))

(define (projection-set-entries set)
  set)

;; ============================================================
;; Wave status transition projection
;; ============================================================

;; The complete projection plan for one wave status change: PLAN.md index,
;; wave doc header and STATE.md row. The wave-doc entry is only included when
;; doc-text is provided (a missing doc cannot be fabricated — matches
;; mark-wave-status!'s skip-missing-doc semantics).
(define (project-wave-status-set plan-text doc-text state-text wave-idx status)
  (define entries (list (cons 'plan-index (project-plan-index-update plan-text wave-idx status))))
  (define entries*
    (if doc-text
        (append entries
                (list (cons 'wave-doc
                            (cons wave-idx (project-wave-doc-update doc-text wave-idx status)))))
        entries))
  (if state-text
      (append entries*
              (list (cons 'state-table (project-state-row-update state-text wave-idx status))))
      entries*))

;; ============================================================
;; Full reconciliation projection
;; ============================================================

;; Recompute the complete projection set from the durable wave statuses.
;; waves: list of (idx . status-symbol) — the durable truth.
;; plan-text: current PLAN.md text ("" when missing).
;; doc-map: hash idx → current wave-doc text (only for existing docs).
;; state-text: current STATE.md text ("" when missing).
;;
;; All transforms are idempotent: a file whose markers already match the
;; durable statuses projects to itself, so the shell writes nothing for it.
(define (project-reconciliation-set waves plan-text doc-map state-text)
  (define-values (plan docs state)
    (for/fold ([p plan-text]
               [d doc-map]
               [s state-text])
              ([w waves])
      (define idx (car w))
      (define status (cdr w))
      (define p-status (wave-status->projection-string status))
      (define s-status (wave-status->state-string status))
      (define new-plan (project-plan-index-update p idx p-status))
      (define new-doc-map
        (let ([cur (hash-ref d idx #f)])
          (if (and cur (string? cur))
              (hash-set d idx (project-wave-doc-update cur idx p-status))
              d)))
      (define new-state
        (if s
            (project-state-row-update s idx s-status)
            s))
      (values new-plan new-doc-map new-state)))
  (define entries
    (cons (cons 'plan-index plan)
          (if state-text
              (list (cons 'state-table state))
              '())))
  (define doc-entries
    (for/list ([(idx doc) (in-hash docs)])
      (cons 'wave-doc (cons idx doc))))
  (append entries doc-entries))

;; ============================================================
;; Provide
;; ============================================================

(provide wave-status->projection-string
         wave-status->state-string
         PROJECTION-STATUS-MARKERS
         status->marker
         project-plan-index-update
         project-wave-doc-update
         project-state-row-update
         project-wave-status-set
         project-reconciliation-set
         projection-entry-kind
         projection-entry-content
         projection-entry-wave-idx
         projection-set?
         projection-set-entries
         PROJECTION-KINDS
         projection-kinds
         projection-file-kind?)
