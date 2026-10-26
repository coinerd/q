#lang racket/base

;; extensions/gsd/campaign-result.rkt — the campaign terminal result value.
;;
;; Extracted from go-orchestrator.rkt (which still re-provides it) so the
;; delivery checkpoint modules can construct and classify campaign results
;; without depending on the orchestrator. Keeping the struct here also keeps
;; the orchestrator under its documented size target: extract a module instead
;; of growing it.
;;
;; W0 (GSD-SIMPLIFICATION-PLAN) — additive structured campaign diagnostics.
;; A terminal result MAY carry an immutable diagnostics hash with a stable
;; machine `code`, operator-facing `summary` / `detail` / `next-action`,
;; machine-checkable `artifacts` references, and arbitrary extra structured
;; fields. Diagnostics are descriptive only: nothing routes on them, and no
;; human message is ever matched. The historical struct (and its 3-argument
;; constructor plus status / completed-waves / message accessors) is
;; unchanged, so no producer call site has to change; legacy results expose a
;; stable default code derived from their status. The historical struct line
;; is kept verbatim (characterization-pinned); the envelope rides a
;; transparent subtype that satisfies every legacy accessor and match
;; pattern. The lean backward-compatible constructor path for diagnostics is
;; campaign-result-with-diagnostics / campaign-result-with-diagnostic.

(struct campaign-result (status completed-waves message) #:transparent)

(require racket/string)

;; Diagnosed variant: the same three legacy fields plus an immutable
;; diagnostics hash. A diagnosed value satisfies campaign-result?, so
;; existing consumers (accessors, match patterns, predicates) keep working.
(struct diagnosed-campaign-result campaign-result (diagnostics) #:transparent)

;; Internal builder: a plain legacy result when no envelope is attached, so
;; legacy construction stays byte-identical in shape and equality.
(define (build-result status completed-waves message diagnostics)
  (if diagnostics
      (diagnosed-campaign-result status completed-waves message diagnostics)
      (campaign-result status completed-waves message)))

;; Canonical diagnostics envelope. Fixed documented keys (`code`, `summary`,
;; `detail`, `next-action`, `artifacts`) plus any extra structured fields
;; merged from `extra` (extra wins on collision; nothing is mutated). `code`
;; is the stable machine identifier; summary/detail/next-action are operator
;; text; artifacts lists machine-checkable references (paths, SHAs, receipts).
(define (campaign-diagnostics code
                              #:summary [summary #f]
                              #:detail [detail #f]
                              #:next-action [next-action #f]
                              #:artifacts [artifacts '()]
                              #:extra [extra (hasheq)])
  (define base
    (hasheq 'code code 'summary summary 'detail detail 'next-action next-action 'artifacts artifacts))
  (foldl (lambda (kv acc) (hash-set acc (car kv) (cdr kv))) base (hash->list extra)))

;; The diagnostics envelope of a result, or #f when none was attached.
(define (campaign-result-diagnostics result)
  (if (diagnosed-campaign-result? result)
      (diagnosed-campaign-result-diagnostics result)
      #f))

;; Stable machine code for a stop. An explicit diagnostic code wins; legacy
;; results derive theirs from the status — the durable vocabulary that already
;; identifies every terminal class — so no producer call site has to change.
(define (campaign-result-code result)
  (define diagnostics (campaign-result-diagnostics result))
  (or (and diagnostics (hash-ref diagnostics 'code #f)) (campaign-result-status result)))

;; Attach/replace a whole diagnostics envelope (the lean backward-compatible
;; diagnostics constructor). Pure: a NEW result value is returned; the
;; original (and its envelope) is never mutated.
(define (campaign-result-with-diagnostics result diagnostics)
  (build-result (campaign-result-status result)
                (campaign-result-completed-waves result)
                (campaign-result-message result)
                diagnostics))

;; Attach/replace ONE diagnostic key by merging into the existing envelope.
(define (campaign-result-with-diagnostic result key value)
  (define existing
    (or (campaign-result-diagnostics result) (campaign-diagnostics (campaign-result-status result))))
  (campaign-result-with-diagnostics result (hash-set existing key value)))

;; Coordinator-boundary rebuild (used by the run-campaign! loop): the loop
;; rewrites the campaign-level completed list when a wave-level terminal
;; result stops the campaign. Status, message and any structured diagnostics
;; are carried through instead of being dropped (W0 propagation contract).
(define (campaign-result-with-completed result
                                        completed
                                        #:status [status (campaign-result-status result)]
                                        #:message [message (campaign-result-message result)])
  (build-result status completed message (campaign-result-diagnostics result)))

;; The single pure /go stop-text renderer (CLI + TUI contract). Legacy
;; results keep their historical text. Explicit codes, details, artifacts,
;; and next actions are operator-visible; none participates in routing.
(define (render-campaign-stop-text result)
  (define status (campaign-result-status result))
  (define message (campaign-result-message result))
  (define diagnostics (campaign-result-diagnostics result))
  (define (present? value)
    (and (string? value) (not (string=? value ""))))
  (define summary (and diagnostics (hash-ref diagnostics 'summary #f)))
  (define detail (and diagnostics (hash-ref diagnostics 'detail #f)))
  (define next-action (and diagnostics (hash-ref diagnostics 'next-action #f)))
  (define artifacts (and diagnostics (hash-ref diagnostics 'artifacts '())))
  (define display-message (if (present? message) message summary))
  (define code (campaign-result-code result))
  (define code-text
    (if (and diagnostics (not (equal? code status)))
        (format " [~a]" code)
        ""))
  (define base
    (if (present? display-message)
        (format "[ERROR] /go campaign stopped: ~a~a (~a)" status code-text display-message)
        (format "[ERROR] /go campaign stopped: ~a~a" status code-text)))
  (define lines
    (list base
          (and (present? detail) (string-append "Detail: " detail))
          (and (pair? artifacts)
               (format "Artifacts: ~a"
                       (string-join (map (lambda (value) (format "~a" value)) artifacts) ", ")))
          (and (present? next-action) (string-append "Next action: " next-action))))
  (string-join (filter values lines) "\n"))

(provide campaign-result
         campaign-result?
         campaign-result-status
         campaign-result-completed-waves
         campaign-result-message
         campaign-result-diagnostics
         campaign-result-code
         campaign-diagnostics
         campaign-result-with-diagnostics
         campaign-result-with-diagnostic
         campaign-result-with-completed
         render-campaign-stop-text)
