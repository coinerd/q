#lang racket/base
;; Pure declaration canonicalization shared by execution and offline authority.
;; This is deliberately not a shell parser: ambiguous scripts fail closed.
;;
;; LAYOUT lives here (which declaration shape is being read: backticked
;; bullets, a legacy single-line shell contract, or an unfenced multiline
;; section) and nothing else. Every TOKEN-level decision — stage splitting,
;; first-token vocabulary, span classification, the operator-reason taxonomy
;; — is owned by `verify-command-grammar.rkt` and re-exported below, so live
;; execution, the frozen offline binding and the review-proof retention layer
;; cannot drift onto a second parser.
(require racket/string
         "verify-command-grammar.rkt")

;; The whole grammar kernel, re-exported unchanged for existing consumers
;; (`verify-command-stages`, `canonical-verify-command`,
;; `verify-normalizer-version`, the span vocabulary, operator reasons, ...).
(provide (all-from-out "verify-command-grammar.rkt")
         ;; Declaration-layout extraction (BUG-0075 / R75).
         verify-declaration-extraction
         ;; ADDITIVE operator diagnostics: the same facts, structured.
         verify-extraction-operator-reasons)

;; ============================================================
;; BUG-0075 (R75): complete declaration extraction
;; ============================================================
;; One stable named vocabulary, shared by live execution, the frozen
;; offline binding and the review-proof retention layer. Every authored
;; command-shaped span becomes an executed marker (a canonical && stage) or
;; a named rejection carrying the original span. A span is command-shaped
;; only when its FIRST token names a known tool: prose identifiers,
;; filenames and single-word mentions are ignored, and grep inside an
;; accepted span's quoted payload is never inspected — quoted grep-looking
;; prose cannot be false-banned.
;;
;; The token vocabulary, `verify-normalizer-version` and the closed
;; rejection/refusal reason taxonomy are defined by
;; `verify-command-grammar.rkt`; this module only decides which layout of
;; declaration it is reading.

;; Complete extraction of one Verify declaration string.
;;   commands   — the COMPLETE validated command list (document order; each
;;                entry is one executed marker)
;;   canonical  — the executable canonical chain, "" exactly when refused
;;   rejections — named rejections: (hasheq 'reason symbol 'span original)
;;   refusal    — structural refusal symbol (#f when the shape is valid):
;;                'empty-declaration 'no-executable-command 'ambiguous-grammar
(define (verify-declaration-extraction declaration)
  (define refused (hasheq 'commands '() 'canonical "" 'rejections '() 'refusal 'empty-declaration))
  (if (not (string? declaration))
      refused
      (let ([trimmed (string-trim declaration)])
        (cond
          [(string=? trimmed "") refused]
          [(verify-decorated? trimmed)
           ;; Markdown bullet criteria: only backticked spans can be gates.
           ;; Every span is classified; one rejection voids the whole
           ;; declaration (never a partial green), zero commands refuses.
           (define-values (commands rejections)
             (for/fold ([commands '()]
                        [rejections '()])
                       ([raw (in-list (regexp-match* #rx"`([^`]+)`" trimmed #:match-select cadr))])
               (define span (string-trim raw))
               (define verdict (verify-classify-span span))
               (cond
                 [(eq? verdict 'prose) (values commands rejections)]
                 [(eq? (car verdict) 'accept)
                  (values (append commands (list (cdr verdict))) rejections)]
                 [else
                  (values commands
                          (append rejections (list (hasheq 'reason (cdr verdict) 'span span))))])))
           (cond
             [(pair? rejections)
              ;; One rejection voids the declaration: nothing executes, and
              ;; the marker list is empty by construction (never partial).
              (hasheq 'commands '() 'canonical "" 'rejections rejections 'refusal #f)]
             [(null? commands)
              (hasheq 'commands '() 'canonical "" 'rejections '() 'refusal 'no-executable-command)]
             [else
              (hasheq 'commands
                      commands
                      'canonical
                      (string-join commands " && ")
                      'rejections
                      '()
                      'refusal
                      #f)])]
          ;; Legacy single-line shell contract (with or without &&):
          ;; byte-for-byte passthrough through the conservative canonical
          ;; grammar — exactly the pre-R75 behavior, including its refusal of
          ;; ambiguous && shapes.
          [(not (regexp-match? #rx"\n" trimmed))
           (define canonical (canonical-verify-command trimmed))
           (if (non-empty-string? canonical)
               (hasheq 'commands
                       (verify-command-stages canonical)
                       'canonical
                       canonical
                       'rejections
                       '()
                       'refusal
                       #f)
               (hasheq 'commands '() 'canonical "" 'rejections '() 'refusal 'ambiguous-grammar))]
          [else
           ;; Unfenced multiline: the audited fail-fast && grammar. Every
           ;; stage must be an executable command line — an English line is
           ;; a NAMED rejection, never a launch through /bin/sh.
           (define stages (verify-command-stages trimmed))
           (cond
             [(not stages)
              (hasheq 'commands '() 'canonical "" 'rejections '() 'refusal 'ambiguous-grammar)]
             [else
              (define bad
                (for/first ([stage (in-list stages)]
                            #:when (let ([v (verify-classify-span stage)])
                                     (not (and (pair? v) (eq? (car v) 'accept)))))
                  stage))
              (cond
                [bad
                 (hasheq 'commands
                         '()
                         'canonical
                         ""
                         'rejections
                         (list (hasheq 'reason 'prose-in-unfenced-multiline 'span bad))
                         'refusal
                         #f)]
                [else
                 (hasheq 'commands
                         stages
                         'canonical
                         (string-join stages " && ")
                         'rejections
                         '()
                         'refusal
                         #f)])])]))))

;; ============================================================
;; ADDITIVE structured operator reasons
;; ============================================================
;; The extraction hash shape above is FROZEN for its existing consumers
;; ('commands 'canonical 'rejections 'refusal). Operator-facing explanations
;; are derived, never stored back: one ordered reason hash per rejection
;; (document order) followed by the structural refusal, if any. Every hash is
;;   'code        — the stable named reason (closed vocabulary)
;;   'kind        — 'rejection | 'refusal (or 'unknown for an out-of-vocabulary
;;                  symbol, which callers must surface as a failure)
;;   'span        — the original authored span, or #f for shape refusals
;;   'summary     — one operator-facing line
;;   'detail      — why the grammar fails closed
;;   'next-action — the safe alternative (never a policy weakening)
;; An accepted declaration yields '().
(define (verify-extraction-operator-reasons extraction)
  (define (describe code span)
    (define entry (verify-operator-reason code))
    (if entry
        (hash-set entry 'span span)
        (hasheq 'code
                code
                'kind
                'unknown
                'span
                span
                'summary
                (format "unrecognized Verify rejection ~a" code)
                'detail
                (string-append "This reason is outside the closed grammar vocabulary, so its "
                               "meaning cannot be stated; the declaration is still refused.")
                'next-action
                (string-append "Report the unrecognized reason symbol; do not treat the "
                               "declaration as accepted."))))
  (define rejections (hash-ref extraction 'rejections '()))
  (define refusal (hash-ref extraction 'refusal #f))
  (append (for/list ([rj (in-list rejections)])
            (describe (hash-ref rj 'reason) (hash-ref rj 'span #f)))
          (if refusal
              (list (describe refusal #f))
              '())))
