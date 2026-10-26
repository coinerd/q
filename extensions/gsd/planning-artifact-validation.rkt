#lang racket/base

;; extensions/gsd/planning-artifact-validation.rkt — the ONE planning preflight kernel
;;
;; W1 (GSD-SIMPLIFICATION-PLAN): "One planning, path, Verify, and preflight
;; contract." This module is the single owner of plan preflight:
;;
;;   * the plan-artifact loader (PLAN.md index to gsd-plan, declared-doc
;;     identity preserved — moved here from wave-executor.rkt);
;;   * the shared `validate-plan-artifacts` kernel /go's validate-plan-for-go
;;     and scripts/validate-plan.rkt both call;
;;   * the ONE wave-document path grammar (used by planning-artifact
;;     normalization and by preflight; bound paths are never renamed);
;;   * structured findings: stable machine `code`, operator `message`,
;;     `next-action` recovery text, and machine-checkable `artifacts`
;;     references — the same envelope vocabulary W0 pinned for campaign
;;     results (`code` / `summary` / `detail` / `next-action` / `artifacts`).
;;
;; Nothing here grants authority over campaign state. Every check is
;; read-only; no finding repairs, renames, rebinds, or resnapshots anything.
;;
;; COMPATIBILITY CONTRACT (why this move is behaviour-preserving):
;;   * `validate-plan-artifacts` keeps its four historical result keys
;;     ('ok?, 'error-message, 'plan, 'findings) with the SAME error text the
;;     standalone CLI and /go already print, and gains additive structured
;;     keys ('diagnostics, 'codes, 'next-actions, 'artifacts, 'plan-path,
;;     'base-dir, 'campaign-id);
;;   * `plan-validation-finding` keeps its 3-positional-argument constructor
;;     (wave-idx kind message) and its accessors, and gains keyword-only
;;     `#:code`, `#:next-action`, `#:artifacts` plus matching accessors;
;;   * `load-plan-from-index` keeps its #f-on-no-plan / #f-on-empty-index
;;     contract and raises the SAME messages in the SAME priority order
;;     (unsafe arrow, then symlinked declared doc, then aggregated missing);
;;   * wave-executor.rkt re-provides every moved name, so legacy importers
;;     keep working unchanged.
;;
;; DELIBERATE NON-BLOCKING FINDINGS: newly surfaced issues (wave-doc lint
;; details, slug mismatches, Verify span rejections, orphan wave docs,
;; snapshot-reference bindability, snapshot drift) are advisory (`warning`
;; kind) unless an existing guard already refuses the plan. Preflight adds
;; named early diagnostics; it does not quietly tighten acceptance.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         "plan-types.rkt"
         ;; The shared strict lint /go and the CLI both run. Kept as the one
         ;; implementation: the kernel composes it, it does not re-implement.
         (only-in "plan-validator.rkt"
                  validate-plan-strict
                  no-waves-format-diagnostic
                  inline-format-rejection-diagnostic)
         ;; BUG-0081/BUG-0082 declared-path identity layer + doc lint surface.
         (only-in "wave-docs.rkt"
                  parse-plan-index
                  wave-index-entry-idx
                  wave-index-entry-title
                  wave-index-entry-status
                  wave-index-entry-ref-path
                  wave-entry-effective-doc-rel-path
                  unsafe-declared-path-refusal
                  symlinked-declared-doc-refusal
                  missing-index-doc-paths
                  read-declared-wave-doc
                  declared-mapping-refusals
                  declared-wave-doc-resolution
                  count-inline-wave-sections
                  lint-campaign-wave-docs
                  wave-doc-violation-wave-idx
                  wave-doc-violation-doc-path
                  format-wave-doc-lint-warning
                  check-slug-consistency
                  slug-mismatch-wave-idx
                  slug-mismatch-doc-path
                  format-slug-mismatch-warning)
         ;; The frozen-snapshot reference grammar: membership in this list is
         ;; what plan-snapshot.rkt will CAPTURE. Probing the exported helper
         ;; (instead of re-encoding its regexp) keeps one grammar.
         (only-in "plan-snapshot.rkt"
                  plan-references->wave-doc-paths
                  plan-body-normalized-hash
                  plan-snapshot-manifest-files
                  load-snapshot-manifest
                  snapshot-file-path
                  snapshot-file-sha256
                  snapshot-file-size
                  snapshot-manifest-digest
                  classify-snapshot-drift)
         ;; W1 canonical path vocabulary: the layout literals built below are
         ;; the SAME values as planning-paths.rkt (sibling W1 lane). They are
         ;; kept as four local helpers so this module stays independently
         ;; loadable inside a parallel-worktree lane; the merge can alias them
         ;; to planning-paths.rkt without changing a single behaviour.
         ;; The one Verify declaration grammar (no second parser here).
         (only-in "verify-command-format.rkt" verify-declaration-extraction)
         ;; Read-only campaign inspection (--explain): durable record identity.
         (only-in "campaign-state.rkt"
                  campaign-record-waves
                  campaign-record-manifest
                  campaign-manifest-hash
                  campaign-manifest-plan-body-hash
                  campaign-manifest-waves
                  campaign-manifest-title
                  campaign-wave-descriptor-index
                  campaign-wave-descriptor-doc-path
                  campaign-plan-id
                  campaign-wave-index
                  campaign-wave-status)
         (only-in "campaign-repository.rkt" load-campaign-record))

;; ============================================================
;; Layout helpers (same values as the canonical planning-paths.rkt)
;; ============================================================
;; Four literals only, kept local so this module loads independently in a
;; parallel-worktree lane. Nothing here is a second path GRAMMAR: the
;; wave-document grammar lives below and refuses unsafe spellings.

(define (plan-file base-dir)
  (build-path base-dir ".planning" "PLAN.md"))

(define (planning-path base-dir rel-path)
  (build-path base-dir ".planning" rel-path))

(define (waves-dir base-dir)
  (build-path base-dir ".planning" "waves"))

(define (campaigns-root base-dir)
  (build-path base-dir ".planning" "campaigns"))

;; ============================================================
;; Structured findings
;; ============================================================
;; One named finding.
;;   wave-idx    — #f for plan-global findings, the wave index otherwise
;;   kind        — 'error (blocks) or 'warning (advisory; 'attribution is
;;                 the legacy advisory label and reports as a warning)
;;   message     — the operator-facing text (byte-identical to the legacy
;;                 message wherever a legacy message already existed)
;;   code        — stable machine identifier ('plan-missing, 'wave-doc-missing,
;;                 'verify-declaration-rejected, ...); never parsed back into
;;                 control flow, only reported
;;   next-action — the recovery step an operator can execute, or #f
;;   artifacts   — machine-checkable references (paths) the finding is about
;;
;; The value gained three fields in W1. The struct itself is named
;; `plan-validation-finding-record` so `plan-validation-finding` below can
;; stay the legacy-compatible 3-positional-argument procedure (the rule
;; preserved in the compatibility contract above); the public predicate and
;; accessors keep the historical `plan-validation-finding-*` names as
;; aliases, so every legacy consumer is unchanged.
(struct plan-validation-finding-record (wave-idx kind message code next-action artifacts)
  #:transparent)

(define plan-validation-finding? plan-validation-finding-record?)
(define plan-validation-finding-wave-idx plan-validation-finding-record-wave-idx)
(define plan-validation-finding-kind plan-validation-finding-record-kind)
(define plan-validation-finding-message plan-validation-finding-record-message)
(define plan-validation-finding-code plan-validation-finding-record-code)
(define plan-validation-finding-next-action plan-validation-finding-record-next-action)
(define plan-validation-finding-artifacts plan-validation-finding-record-artifacts)

;; Legacy-compatible constructor: positional (wave-idx kind message) like the
;; pre-W1 struct, with the structured fields as keywords.
(define (plan-validation-finding wave-idx
                                 kind
                                 message
                                 #:code [code #f]
                                 #:next-action [next-action #f]
                                 #:artifacts [artifacts '()])
  (plan-validation-finding-record wave-idx
                                  kind
                                  message
                                  (or code (default-finding-code kind))
                                  next-action
                                  (if (list? artifacts)
                                      artifacts
                                      '())))

(define (default-finding-code kind)
  (case kind
    [(error) 'plan-error]
    [(attribution) 'file-attribution]
    [else 'plan-warning]))

(define (plan-validation-error-findings findings)
  (filter (lambda (f) (eq? (plan-validation-finding-kind f) 'error)) findings))

(define (plan-validation-warning-findings findings)
  (filter (lambda (f) (not (eq? (plan-validation-finding-kind f) 'error))) findings))

;; Stable machine codes of the blocking findings, in report order.
(define (plan-validation-error-codes findings)
  (for/list ([f (in-list (plan-validation-error-findings findings))])
    (plan-validation-finding-code f)))

;; Every distinct recovery action named by the findings, errors first.
(define (plan-validation-next-actions findings)
  (remove-duplicates (filter values
                             (for/list ([f (in-list findings)])
                               (plan-validation-finding-next-action f)))
                     string=?))

;; Every distinct artifact reference named by the findings.
(define (plan-validation-artifacts findings)
  (remove-duplicates (append* (for/list ([f (in-list findings)])
                                (plan-validation-finding-artifacts f)))
                     string=?))

;; The W0 diagnostics envelope, filled from the findings: one stable code,
;; summary/detail text, the first recovery action, and artifact references.
(define (plan-validation-diagnostics findings)
  (define errors (plan-validation-error-findings findings))
  (cond
    [(null? errors)
     (hasheq 'code
             'plan-ready
             'summary
             #f
             'detail
             #f
             'next-action
             #f
             'artifacts
             (plan-validation-artifacts findings))]
    [else
     (define first-error (car errors))
     (define detail
       (and (pair? (cdr errors))
            (string-join (for/list ([f (in-list (cdr errors))])
                           (plan-validation-finding-message f))
                         "\n")))
     (hasheq 'code
             (plan-validation-finding-code first-error)
             'summary
             (plan-validation-finding-message first-error)
             'detail
             detail
             'next-action
             (plan-validation-finding-next-action first-error)
             'artifacts
             (plan-validation-artifacts errors))]))

;; ============================================================
;; One wave-document path grammar
;; ============================================================
;; A wave document is declared `.planning`-relative as
;;   waves/W<idx>-<slug>.md      (the canonical authoring form)
;;   waves/WAVE-<idx>-<slug>.md  (the long spelling some campaigns use)
;; where <slug> may contain safe dots (e.g. racket-9.3), letters, digits,
;; `_` and `-`. Everything else is refused with a named reason:
;; absolute paths, parent-directory or empty or `.` segments, NUL bytes,
;; extra separators, a missing waves/ prefix, a missing .md suffix, an empty
;; slug, and any filesystem separator.
;;
;; The traversal/absolute/segment half is OWNED by
;; `unsafe-declared-path-refusal` (wave-docs.rkt, BUG-0082) and is called
;; first; this module owns only the filename half. Bound paths are NEVER
;; renamed: the grammar validates input.

;; Filename shape of a wave document below waves/.
(define wave-doc-filename-rx #px"^(?:W|WAVE-)([0-9]+)-([A-Za-z0-9][A-Za-z0-9._-]*)\\.md$")
;; Prefix that marks a filename as a WAVE document (vs. another waves/ doc).
(define wave-doc-prefix-rx #px"^(?:W|WAVE-)[0-9]+-")
;; A plan-id / hash: 64 lowercase hex characters.
(define sha256-hex-rx #px"^[0-9a-f]{64}$")

;; parse-wave-doc-rel-path : any/c -> (values idx refusal)
;; PURE. idx is the declared wave number when the path is a valid wave
;; document path; refusal is #f then, and a named string otherwise.
(define (parse-wave-doc-rel-path rel-path)
  (cond
    [(not (string? rel-path)) (values #f "wave document path must be a string")]
    [(zero? (string-length rel-path)) (values #f "wave document path is empty")]
    [(string-contains? rel-path "\x00")
     (values #f (format "wave document path ~a contains a NUL byte" rel-path))]
    [else
     ;; Existing helper first: absolute/traversal/`.`/empty-segment refusals
     ;; stay owned by wave-docs.rkt (BUG-0082) and are never re-implemented.
     (define unsafe (unsafe-declared-path-refusal rel-path))
     (cond
       [unsafe (values #f unsafe)]
       [else
        (define segs (string-split rel-path "/"))
        (cond
          [(and (= (length segs) 2) (string=? (car segs) "waves"))
           (define m (regexp-match wave-doc-filename-rx (cadr segs)))
           (cond
             [m (values (string->number (cadr m)) #f)]
             [else
              (values #f
                      (format (string-append
                               "wave document path ~a does not follow the wave-document grammar "
                               "waves/W<idx>-<slug>.md or waves/WAVE-<idx>-<slug>.md")
                              rel-path))])]
          [else
           (values #f
                   (format "wave document path ~a is not exactly one document under waves/"
                           rel-path))])])]))

;; wave-doc-rel-path-refusal : any/c -> (or/c string? #f)
(define (wave-doc-rel-path-refusal rel-path)
  (let-values ([(idx refusal) (parse-wave-doc-rel-path rel-path)])
    (void idx)
    refusal))

;; wave-doc-rel-path? : any/c -> boolean?
(define (wave-doc-rel-path? rel-path)
  (not (wave-doc-rel-path-refusal rel-path)))

;; wave-doc-rel-path-index : any/c -> (or/c exact-nonnegative-integer? #f)
(define (wave-doc-rel-path-index rel-path)
  (let-values ([(idx refusal) (parse-wave-doc-rel-path rel-path)])
    (and (not refusal) idx)))

;; wave-artifact-name-refusal : any/c -> (or/c string? #f)
;; PURE. The planning-artifact-name face of the SAME grammar: the waves/
;; branch of plan artifact names (waves/W0-slug.md as used by planning-read
;; and planning-write). Non-wave documents below waves/ keep their
;; historical acceptance (any single <name>.md segment); only wave-prefixed
;; names must satisfy the wave-document grammar. #f means valid.
(define (wave-artifact-name-refusal name)
  (cond
    [(not (string? name)) "artifact name must be a string"]
    [(not (string-prefix? name "waves/"))
     (format "artifact name ~a is not a wave document path (expected waves/W<idx>-<slug>.md)" name)]
    [(string-contains? name "\x00") (format "artifact name ~a contains a NUL byte" name)]
    [(string-contains? name "..")
     (format "artifact name ~a contains a parent-directory reference" name)]
    [else
     (define rest (substring name (string-length "waves/")))
     (cond
       [(string=? rest "") "artifact name waves/ names no document"]
       [(string-contains? rest "/") (format "artifact name ~a nests below waves/" name)]
       [(not (string-suffix? rest ".md")) (format "artifact name ~a is not a .md document" name)]
       [(regexp-match? wave-doc-prefix-rx rest) (wave-doc-rel-path-refusal name)]
       [else #f])]))

;; ============================================================
;; Plan loading (moved from wave-executor.rkt, behaviour preserved)
;; ============================================================

;; Authors annotate declared-new files with trailing bracketed notes
;; ("- File: q/scripts/x.rkt [MISSING]"). The annotation is metadata
;; for humans; the probe target is the bare path.
(define declared-file-annotation-rx #px"\\s*\\[[^]]*\\]\\s*$")

(define (declared-file-path raw)
  (string-trim (regexp-replace declared-file-annotation-rx
                               (if (path? raw)
                                   (path->string raw)
                                   (format "~a" raw))
                               "")))

;; Mechanical file-attribution sanity check (BUG-0048 action 3): every
;; declared File: path that does not exist under the project base is either a
;; file this wave will create (fine) or a MISATTRIBUTED path — the exact
;; class of error the v1.00.21 plan review caught twice by hand. Advisory
;; only: it never blocks, but it names the wave and both the declared and
;; the resolved path.
(define (file-attribution-findings base-dir plan)
  (for*/list ([w (in-list (gsd-plan-waves plan))]
              [f (in-list (gsd-wave-files w))]
              #:when (non-empty-string? (declared-file-path f))
              #:unless
              (file-exists? (simplify-path (path->complete-path (declared-file-path f) base-dir) #f)))
    (define rel (declared-file-path f))
    (define resolved (simplify-path (path->complete-path rel base-dir) #f))
    (plan-validation-finding
     (gsd-wave-index w)
     'attribution
     (format
      "W~a: declared file \"~a\" does not exist under ~a (resolved: ~a) — new file this wave will create, or a misattributed path?"
      (gsd-wave-index w)
      rel
      base-dir
      resolved)
     #:code 'file-attribution
     #:next-action
     (format "confirm W~a really creates \"~a\", or correct the declared path" (gsd-wave-index w) rel)
     #:artifacts (list (format "~a" resolved)))))

;; missing-wave-docs-error-text : (listof wave-index-entry?) (listof string?) -> string?
;; BUG-0023 (W2): error text naming each missing wave doc plus the expected
;; filename convention, so authors can comply immediately.
(define (missing-wave-docs-error-text entries missing)
  (format (string-append "Plan index has ~a wave entr~a but ~a wave doc~a missing on disk: ~a\n"
                         "Expected filename convention: waves/W<idx>-<slug>.md "
                         "(e.g. .planning/waves/W0-title-slug.md). "
                         "Create the missing file(s) or fix the index target path.")
          (length entries)
          (if (= (length entries) 1) "y" "ies")
          (length missing)
          (if (= (length missing) 1) " is" "s are")
          (string-join missing ", ")))

(define (string->wave-status-from-entry e)
  (define s (wave-index-entry-status e))
  (cond
    [(string=? s "DONE") 'completed]
    [(string=? s "FAILED") 'failed]
    [(string=? s "DEFERRED") 'skipped]
    [(string=? s "In-Progress") 'in-progress]
    [else 'pending]))

(define (extract-files-from-content content)
  (hash-ref (parse-wave-content content) 'files '()))

(define (extract-verify-from-content content)
  (hash-ref (parse-wave-content content) 'verify ""))

;; The .planning-relative display path an index row resolves to (declared
;; arrow path when present, synthesized twin for arrowless legacy rows).
(define (entry-doc-display-path e)
  (define-values (rel refusal) (wave-entry-effective-doc-rel-path e))
  (if rel
      (format ".planning/~a" rel)
      (format ".planning/~a" (wave-index-entry-ref-path e))))

(define (arrow-issue e refusal)
  (define idx (wave-index-entry-idx e))
  (hasheq 'idx
          idx
          'kind
          'arrow
          'code
          'wave-doc-path-invalid
          'message
          refusal
          'next-action
          (format (string-append
                   "declare W~a's document as a relative .planning path under waves/ "
                   "(waves/W<idx>-<slug>.md); absolute, traversal and empty segments are refused")
                  idx)
          'artifacts
          (list (entry-doc-display-path e))))

(define (link-issue e refusal)
  (define idx (wave-index-entry-idx e))
  (hasheq 'idx
          idx
          'kind
          'link
          'code
          'wave-doc-path-symlinked
          'message
          refusal
          'next-action
          (format "replace W~a's symlinked declared document with a real file inside .planning/waves"
                  idx)
          'artifacts
          (list (entry-doc-display-path e))))

(define (missing-issue e display)
  (define idx (wave-index-entry-idx e))
  (hasheq 'idx
          idx
          'kind
          'missing
          'code
          'wave-doc-missing
          'message
          (format "W~a: declared wave document ~a is missing on disk" idx display)
          'next-action
          (format "create ~a (or fix the PLAN.md row's target path) before running /go" display)
          'artifacts
          (list display)))

;; resolve-plan-index : path-string? string? (listof wave-index-entry?)
;;   -> (values plan row-issues legacy-error-message)
;; PURE read of the declared documents. row-issues is a list of hashes with
;; keys 'idx 'kind ('arrow | 'link | 'missing) 'code 'message 'next-action
;; 'artifacts. legacy-error-message is #f when the rows resolve, else the
;; EXACT text load-plan-from-index historically raised, selected in the
;; historical priority order: first unsafe declared arrow, then first
;; symlinked declared doc, then the aggregated missing-document text.
(define (resolve-plan-index base-dir text entries)
  (define arrow-issues
    (for/list ([e (in-list entries)]
               #:do [(define-values (_rel refusal) (wave-entry-effective-doc-rel-path e))]
               #:when refusal)
      (arrow-issue e refusal)))
  (define link-issues
    (for/list ([e (in-list entries)]
               #:do [(define-values (rel _refusal) (wave-entry-effective-doc-rel-path e))
                     (define link-refusal (and rel (symlinked-declared-doc-refusal base-dir rel)))]
               #:when link-refusal)
      (link-issue e link-refusal)))
  (define missing-displays (missing-index-doc-paths base-dir entries))
  (define missing-issues
    (for/list ([e (in-list entries)]
               #:do [(define display (entry-doc-display-path e))]
               #:when (member display missing-displays))
      (missing-issue e display)))
  (define (issue-message issues)
    (and (pair? issues) (hash-ref (car issues) 'message)))
  (cond
    [(pair? arrow-issues) (values #f arrow-issues (issue-message arrow-issues))]
    [(pair? link-issues) (values #f link-issues (issue-message link-issues))]
    [(pair? missing-issues)
     (values #f missing-issues (missing-wave-docs-error-text entries missing-displays))]
    [else
     (void text)
     (define waves
       (for/list ([e (in-list entries)])
         (define idx (wave-index-entry-idx e))
         ;; BUG-0082: the wave's content is the DECLARED document's content
         ;; — a stale synthesized twin never loads.
         (define wave-data (read-declared-wave-doc base-dir idx))
         (define wave-content
           (if wave-data
               (hash-ref wave-data 'content)
               ""))
         (gsd-wave idx
                   (wave-index-entry-title e)
                   (string->wave-status-from-entry e)
                   wave-content
                   (extract-files-from-content wave-content)
                   '()
                   (extract-verify-from-content wave-content)
                   '())))
     (values (gsd-plan waves #f '() '()) '() #f)]))

;; load-plan-from-index : path-string? -> (or/c gsd-plan? #f)
;; The legacy loader contract: #f when PLAN.md is absent or the index is
;; empty; otherwise the declared documents load, or an exn names the first
;; refusal in the historical priority order.
(define (load-plan-from-index base-dir)
  (define plan-path (plan-file base-dir))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (cond
       [(null? entries) #f]
       [else
        (define-values (plan _issues legacy-message) (resolve-plan-index base-dir text entries))
        (cond
          [plan plan]
          [else
           (raise (exn:fail (or legacy-message "plan index could not be resolved")
                            (current-continuation-marks)))])])]))

;; ============================================================
;; Advisory findings: wave-doc lint, slug, Verify, orphan, bindability
;; ============================================================

;; Group the BUG-0041 lint violations per document exactly like
;; wave-doc-lint-warning-lines does, but keep the wave index (the legacy
;; line-only surface loses it).
(define (wave-doc-lint-findings base-dir)
  (for/list ([group (in-list (group-by (lambda (v)
                                         (list (wave-doc-violation-wave-idx v)
                                               (wave-doc-violation-doc-path v)))
                                       (lint-campaign-wave-docs base-dir)))])
    (define first-violation (car group))
    (define idx (wave-doc-violation-wave-idx first-violation))
    (define doc-path (wave-doc-violation-doc-path first-violation))
    (plan-validation-finding
     idx
     'warning
     (format-wave-doc-lint-warning idx doc-path group)
     #:code 'wave-doc-missing-section
     #:next-action (format "add the missing or invalid executor-contract sections to ~a" doc-path)
     #:artifacts (list doc-path))))

(define (slug-mismatch-findings base-dir)
  (for/list ([m (in-list (check-slug-consistency base-dir))])
    (plan-validation-finding (slug-mismatch-wave-idx m)
                             'warning
                             (format-slug-mismatch-warning m)
                             #:code 'wave-doc-slug-mismatch
                             #:next-action
                             "align the PLAN.md arrow target with the wave document filename on disk"
                             #:artifacts (list (slug-mismatch-doc-path m)))))

;; Verify span findings: the ONE declaration grammar reports rejections with
;; the original span and a named reason; preflight surfaces the SAME
;; span/reason instead of discovering it at delivery time. Advisory: no new
;; gate is introduced, the wave-doc lint and the verifier stay authoritative.
(define (wave-verify-findings w)
  (define declaration (gsd-wave-verify w))
  (define idx (gsd-wave-index w))
  (cond
    [(or (not (string? declaration)) (string=? (string-trim declaration) "")) '()]
    [else
     (define extraction (verify-declaration-extraction declaration))
     (define rejections (hash-ref extraction 'rejections '()))
     (define refusal (hash-ref extraction 'refusal #f))
     (cond
       [(pair? rejections)
        (for/list ([r (in-list rejections)])
          (plan-validation-finding
           idx
           'warning
           (format
            (string-append "W~a: Verify declaration rejected — span ~s cannot be executed as a "
                           "gate (reason: ~a). Nothing from this declaration runs; frozen and "
                           "live verification refuse it identically.")
            idx
            (hash-ref r 'span)
            (hash-ref r 'reason))
           #:code 'verify-declaration-rejected
           #:next-action (format "replace W~a's rejected Verify span with an accepted command span"
                                 idx)
           #:artifacts (list (format ".planning/PLAN.md#W~a" idx))))]
       [(memq refusal '(no-executable-command ambiguous-grammar))
        (list
         (plan-validation-finding
          idx
          'warning
          (format (string-append "W~a: Verify declaration has no executable command (reason: ~a) — "
                                 "no gate can run for this wave.")
                  idx
                  refusal)
          #:code 'verify-declaration-no-executable-command
          #:next-action
          (format "declare an executable Verify command for W~a (for example: raco test tests/x.rkt)"
                  idx)
          #:artifacts (list (format ".planning/PLAN.md#W~a" idx))))]
       [else '()])]))

(define (verify-findings plan)
  (append* (for/list ([w (in-list (gsd-plan-waves plan))])
             (wave-verify-findings w))))

;; Wave documents on disk that no index row claims. .planning/waves/ is
;; shared across campaigns (v1.00.30), so this is advisory only and names
;; the exact file: it is the missing-index-row class.
(define (orphan-wave-doc-findings base-dir entries)
  (define waves-root (waves-dir base-dir))
  (cond
    [(not (directory-exists? waves-root)) '()]
    [else
     (define claimed
       (for/list ([e (in-list entries)])
         (define-values (rel _refusal) (wave-entry-effective-doc-rel-path e))
         (and rel (path->string (planning-path base-dir rel)))))
     (for/list ([f (in-list (sort (directory-list waves-root #:build? #t) path<?))]
                #:do [(define filename (path->string (file-name-from-path f)))
                      (define m (regexp-match wave-doc-filename-rx filename))]
                #:when (and m (file-exists? f) (not (member (path->string f) claimed))))
       (define idx (string->number (cadr m)))
       (plan-validation-finding
        idx
        'warning
        (format (string-append "W~a: wave document .planning/waves/~a exists on disk but no PLAN.md "
                               "index row declares it (missing index row).")
                idx
                filename)
        #:code 'wave-doc-without-index-row
        #:next-action
        (format
         (string-append "add a PLAN.md index row declaring waves/~a, or leave the file alone if it "
                        "belongs to another campaign (.planning/waves/ is shared)")
         filename)
        #:artifacts (list (format ".planning/waves/~a" filename))))]))

;; Declared wave paths that the frozen-snapshot reference grammar does NOT
;; capture. Membership is decided by the EXPORTED plan-snapshot helper, so
;; there is still exactly one reference grammar. Advisory: the snapshot
;; grammar owner widens it; preflight only refuses to stay silent about a
;; document that a campaign binding would omit.
(define (bindability-findings text entries)
  (define captured (plan-references->wave-doc-paths text))
  (for/list ([e (in-list entries)]
             #:do [(define declared (wave-index-entry-ref-path e))]
             #:when declared
             #:do [(define-values (rel refusal) (wave-entry-effective-doc-rel-path e))]
             #:when (and rel (not refusal) (not (member rel captured))))
    (define idx (wave-index-entry-idx e))
    (plan-validation-finding
     idx
     'warning
     (format (string-append "W~a: declared wave document .planning/~a is not captured by the "
                            "frozen-snapshot reference grammar (dotted or long-form names are not "
                            "bound); a campaign snapshot would omit it.")
             idx
             rel)
     #:code 'wave-doc-path-not-bindable
     #:next-action
     (format (string-append "rename W~a's document to a bindable waves/W<idx>-<slug>.md path before "
                            "binding (no bound path is ever renamed automatically)")
             idx)
     #:artifacts (list (format ".planning/~a" rel)))))

;; Snapshot drift of an EXPLICIT campaign: the existing classify-snapshot-drift
;; classes are surfaced with stable codes. Symlinks are errors (fail-closed
;; everywhere); missing/content drift stay advisory here because recovery is
;; an operator decision, never a preflight rewrite.
(define (snapshot-drift-class-code kind)
  (case kind
    [(symlink) 'snapshot-drift-symlink]
    [(missing) 'snapshot-drift-missing]
    [(content-drift) 'snapshot-drift-content-drift]
    [(frozen-contract-stale) 'snapshot-drift-frozen-contract-stale]
    [else 'snapshot-unreadable]))

(define (snapshot-drift-next-action kind rel campaign-id)
  (case kind
    [(symlink) (format "replace the symlinked .planning/~a with the real frozen file" rel)]
    [(missing)
     (format "restore .planning/~a from the verified snapshot, or re-plan deliberately" rel)]
    [(content-drift)
     (format "review the authored change to .planning/~a; an operator decides restore or re-plan"
             rel)]
    [(frozen-contract-stale)
     (string-append "the authored plan body changed after the snapshot froze: re-freeze under a "
                    "new campaign identity or restore the frozen content")]
    [else (format "inspect the campaign snapshot for ~a" campaign-id)]))

(define (snapshot-drift-findings base-dir campaign-id)
  (define classified
    (with-handlers ([exn:fail? (lambda (e) (list (list "PLAN.md" 'unreadable (exn-message e))))])
      (for/list ([item (in-list (classify-snapshot-drift base-dir campaign-id))])
        (list (car item) (cadr item) #f))))
  (for/list ([item (in-list classified)])
    (define rel (car item))
    (define kind (cadr item))
    (define detail (caddr item))
    (plan-validation-finding #f
                             (if (memq kind '(symlink unreadable)) 'error 'warning)
                             (format "campaign ~a snapshot drift: .planning/~a — class ~a~a"
                                     campaign-id
                                     rel
                                     kind
                                     (if detail
                                         (format " (~a)" detail)
                                         ""))
                             #:code (snapshot-drift-class-code kind)
                             #:next-action (snapshot-drift-next-action kind rel campaign-id)
                             #:artifacts (list (format ".planning/~a" rel)))))

;; ============================================================
;; The shared preflight kernel
;; ============================================================

(define (make-plan-validation-result plan-path base-dir campaign-id ok? error-message plan findings)
  (hasheq 'ok?
          ok?
          'error-message
          error-message
          'plan
          plan
          'findings
          findings
          'diagnostics
          (plan-validation-diagnostics findings)
          'codes
          (plan-validation-error-codes findings)
          'next-actions
          (plan-validation-next-actions findings)
          'artifacts
          (plan-validation-artifacts findings)
          'plan-path
          (path->string plan-path)
          'base-dir
          (path->string base-dir)
          'campaign-id
          campaign-id))

;; validate-plan-artifacts : path-string? [path-string?] -> hash?
;; Returns:
;;   'ok?            — no 'error finding (mirrors valid-plan->go?)
;;   'error-message  — #f or the joined error text (the EXACT text /go shows
;;                     when it rejects the same plan; for the structural
;;                     refusals it is byte-identical to the legacy text)
;;   'plan           — the loaded gsd-plan on success, #f otherwise
;;   'findings       — every named finding, plan-global (wave-idx #f) and
;;                     per-wave (wave-idx N)
;;   'diagnostics    — the W0 envelope (code/summary/detail/next-action/
;;                     artifacts) for the first blocking finding
;;   'codes          — the blocking findings' stable machine codes
;;   'next-actions   — every distinct recovery action the findings name
;;   'artifacts      — every distinct artifact reference the findings name
;;   'plan-path / 'base-dir / 'campaign-id — inspection context
;; Decision order matches /go: no PLAN, then inline-only rejection
;; (BUG-0023 residual), then the zero-waves diagnostic, then per-row
;; declared-doc refusals (BUG-0023 strict / BUG-0082 identity), then strict
;; lint, then mapping ambiguity, then the advisory classes (wave-doc lint,
;; slug mismatch, Verify span, orphan row, snapshot bindability, file
;; attribution, and snapshot drift when #:campaign-id names a campaign).
(define (validate-plan-artifacts base-dir
                                 [plan-path (plan-file base-dir)]
                                 #:campaign-id [campaign-id #f])
  (cond
    [(not (file-exists? plan-path))
     (define msg "No PLAN found in .planning/. Use /plan <task> to create one.")
     (make-plan-validation-result
      plan-path
      base-dir
      campaign-id
      #f
      msg
      #f
      (list
       (plan-validation-finding
        #f
        'error
        msg
        #:code 'plan-missing
        #:next-action
        "create .planning/PLAN.md with the index grammar: - [Inbox] W0: Title -> waves/W0-slug.md"
        #:artifacts (list (path->string plan-path)))))]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (cond
       [(null? entries)
        (define inline? (positive? (count-inline-wave-sections text)))
        (define msg
          (if inline?
              inline-format-rejection-diagnostic
              (string-append "Plan has no waves\n" no-waves-format-diagnostic)))
        (make-plan-validation-result
         plan-path
         base-dir
         campaign-id
         #f
         msg
         #f
         (list (plan-validation-finding
                #f
                'error
                msg
                #:code (if inline? 'plan-inline-format-deprecated 'plan-no-waves)
                #:next-action
                (string-append "rewrite PLAN.md as index rows: "
                               "- [Inbox] W0: Title -> waves/W0-slug.md (one row per wave)")
                #:artifacts (list (path->string plan-path)))))]
       [else
        ;; A read failure that the row checks could not classify (for example
        ;; a declared document that becomes unreadable between resolution and
        ;; the read) is a plan-global refusal, never a crash out of the
        ;; kernel: the legacy kernel caught it and reported the message.
        (define-values (plan row-issues legacy-message)
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (values
                              #f
                              (list (hasheq 'idx
                                            #f
                                            'code
                                            'plan-unreadable
                                            'message
                                            (exn-message e)
                                            'next-action
                                            "make the declared plan documents readable and retry"
                                            'artifacts
                                            (list (path->string plan-path))))
                              (exn-message e)))])
            (resolve-plan-index base-dir text entries)))
        (cond
          [(not plan)
           (define findings
             (for/list ([issue (in-list row-issues)])
               (plan-validation-finding (hash-ref issue 'idx)
                                        'error
                                        (hash-ref issue 'message)
                                        #:code (hash-ref issue 'code)
                                        #:next-action (hash-ref issue 'next-action)
                                        #:artifacts (hash-ref issue 'artifacts))))
           (make-plan-validation-result plan-path base-dir campaign-id #f legacy-message #f findings)]
          [else
           (define strict (validate-plan-strict plan))
           (define mapping-refusals (declared-mapping-refusals entries))
           (define findings
             (append (strict-error-findings strict)
                     (strict-warning-findings strict)
                     (mapping-refusal-findings mapping-refusals)
                     (wave-doc-lint-findings base-dir)
                     (slug-mismatch-findings base-dir)
                     (verify-findings plan)
                     (orphan-wave-doc-findings base-dir entries)
                     (bindability-findings text entries)
                     (file-attribution-findings base-dir plan)
                     (if campaign-id
                         (snapshot-drift-findings base-dir campaign-id)
                         '())))
           (define errors (plan-validation-error-findings findings))
           (make-plan-validation-result
            plan-path
            base-dir
            campaign-id
            (null? errors)
            (and (pair? errors) (string-join (map plan-validation-finding-message errors) "\n"))
            plan
            findings)])])]))

(define (strict-error-findings strict)
  (for/list ([e (in-list (validation-errors strict))])
    (plan-validation-finding #f
                             'error
                             e
                             #:code
                             (if (regexp-match? #rx"no waves" e) 'plan-no-waves 'plan-lint-error)
                             #:next-action "fix the named plan lint error before running /go")))

(define (strict-warning-findings strict)
  (for/list ([e (in-list (validation-warnings strict))])
    (plan-validation-finding #f
                             'warning
                             e
                             #:code 'plan-lint-warning
                             #:next-action "optional: address the named plan lint warning")))

(define (mapping-refusal-findings mapping-refusals)
  (for/list ([r (in-list mapping-refusals)])
    (plan-validation-finding
     #f
     'error
     r
     #:code 'wave-doc-mapping-ambiguous
     #:next-action "give every wave its own declared document and remove duplicate W-index rows")))

;; ============================================================
;; Human-facing rendering (standalone CLI + logs)
;; ============================================================

;; format-plan-validation-findings : hash? -> string?
;; Human-facing rendering for the standalone CLI: result line, one line
;; per finding labelled by kind and stable code, and the recovery action
;; named under each finding that has one.
(define (format-plan-validation-findings result)
  (define findings (hash-ref result 'findings '()))
  (define errors (plan-validation-error-findings findings))
  (define warnings (plan-validation-warning-findings findings))
  (define (label f)
    (case (plan-validation-finding-kind f)
      [(error) "ERROR"]
      [(attribution) "ATTRIBUTION-WARN"]
      [else "WARN"]))
  (define (finding-line f)
    (define head
      (format "  [~a] (~a) ~a"
              (label f)
              (plan-validation-finding-code f)
              (plan-validation-finding-message f)))
    (define next-action (plan-validation-finding-next-action f))
    (string-append head
                   (if next-action
                       (format "\n      -> next: ~a" next-action)
                       "")))
  (string-append
   (if (hash-ref result 'ok?)
       (format "OK: plan is /go-ready (~a error(s), ~a warning(s))" (length errors) (length warnings))
       (format "FAILED: ~a error(s), ~a warning(s)" (length errors) (length warnings)))
   (if (null? findings)
       ""
       (string-append "\n"
                      (string-join (for/list ([f (in-list findings)])
                                     (finding-line f))
                                   "\n")))))

;; ============================================================
;; Read-only campaign inspection (--explain)
;; ============================================================
;; Identity namespaces shown side by side (R-006/R-016 diagnostics): the
;; stored plan-id from the durable record, the hash RECOMPUTED from that
;; record's manifest, the frozen snapshot's PLAN.md entry hash, and the LIVE
;; normalized plan-body hash. Nothing is compared for authority here and
;; nothing is repaired; the CLI prints what is true.

;; newest-campaign-id : path-string? -> (or/c string? #f)
;; Most recently modified durable campaign record id under
;; .planning/campaigns/ (read-only). Corrupt records are skipped.
(define (newest-campaign-id base-dir)
  (define dir (campaigns-root base-dir))
  (cond
    [(not (directory-exists? dir)) #f]
    [else
     (define candidates
       (for/list ([p (in-list (directory-list dir #:build? #t))]
                  #:do [(define name (path->string (file-name-from-path p)))
                        (define id
                          (and (string-suffix? name ".rktd")
                               (substring name 0 (- (string-length name) 5))))]
                  #:when (and id (regexp-match? sha256-hex-rx id) (file-exists? p)))
         (cons id
               (with-handlers ([exn:fail? (lambda (_) 0)])
                 (file-or-directory-modify-seconds p)))))
     (for/first ([c (in-list (sort candidates (lambda (a b) (> (cdr a) (cdr b)))))]
                 #:do [(define rec
                         (with-handlers ([exn:fail? (lambda (_) #f)])
                           (load-campaign-record base-dir (car c))))]
                 #:when rec)
       (car c))]))

;; plan-artifact-explanation : path-string? [#:campaign-id string?] -> hash?
;; Read-only structured inspection: declared wave paths, identity namespaces,
;; bound/live hashes, and per-wave status projections. Never writes, never
;; rebinds, never touches a campaign record.
(define (plan-artifact-explanation base-dir #:campaign-id [campaign-id #f])
  (define plan-path (plan-file base-dir))
  (define text (and (file-exists? plan-path) (call-with-input-file plan-path port->string)))
  (define entries
    (if text
        (parse-plan-index text)
        '()))
  (define selected-campaign-id (or campaign-id (newest-campaign-id base-dir)))
  (define rec
    (and selected-campaign-id
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (load-campaign-record base-dir selected-campaign-id))))
  (define manifest (and rec (campaign-record-manifest rec)))
  (define snapshot
    (and selected-campaign-id
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (load-snapshot-manifest base-dir selected-campaign-id))))
  (define bound-plan-entry
    (and snapshot
         (for/first ([f (in-list (plan-snapshot-manifest-files snapshot))]
                     #:when (equal? (snapshot-file-path f) "PLAN.md"))
           f)))
  (define live-body-hash (and text (plan-body-normalized-hash text)))
  (define declared-wave-paths
    (for/list ([e (in-list entries)])
      (define-values (rel refusal) (wave-entry-effective-doc-rel-path e))
      (hasheq 'wave
              (wave-index-entry-idx e)
              'declared?
              (and (wave-index-entry-ref-path e) #t)
              'path
              (entry-doc-display-path e)
              'valid?
              (not refusal)
              'refusal
              refusal
              'rel-path
              rel)))
  (define bound-wave-paths
    (if manifest
        (for/list ([w (in-list (campaign-manifest-waves manifest))])
          (hasheq 'wave
                  (campaign-wave-descriptor-index w)
                  'path
                  (campaign-wave-descriptor-doc-path w)))
        '()))
  (define status-projections
    (for/list ([e (in-list entries)])
      (define idx (wave-index-entry-idx e))
      (define-values (doc refusal display) (declared-wave-doc-resolution base-dir idx entries))
      (define durable
        (and rec
             (for/first ([w (in-list (campaign-record-waves rec))]
                         #:when (= (campaign-wave-index w) idx))
               (campaign-wave-status w))))
      (hasheq 'wave
              idx
              'declared-path
              display
              'index-status
              (wave-index-entry-status e)
              'doc-status
              (and doc (not refusal) (hash-ref doc 'status #f))
              'durable-status
              durable
              'resolvable?
              (and doc (not refusal) #t))))
  (hasheq
   'plan-path
   (path->string plan-path)
   'base-dir
   (path->string base-dir)
   'plan-exists?
   (and text #t)
   'campaign-id
   selected-campaign-id
   'campaign-source
   (cond
     [campaign-id 'requested]
     [selected-campaign-id 'newest-record]
     [else #f])
   'stored-plan-id
   (and rec (campaign-plan-id rec))
   'recomputed-manifest-hash
   (and manifest (campaign-manifest-hash manifest))
   'identity-matches?
   (and rec (equal? (campaign-plan-id rec) (campaign-manifest-hash (campaign-record-manifest rec))))
   'manifest-title
   (and manifest (campaign-manifest-title manifest))
   'bound-plan-body-hash
   (and manifest (campaign-manifest-plan-body-hash manifest))
   'live-plan-body-hash
   live-body-hash
   'bound-body-matches-live?
   (and manifest
        (campaign-manifest-plan-body-hash manifest)
        live-body-hash
        (equal? (campaign-manifest-plan-body-hash manifest) live-body-hash))
   'snapshot-manifest-digest
   (and snapshot (snapshot-manifest-digest snapshot))
   'snapshot-plan-sha256
   (and bound-plan-entry (snapshot-file-sha256 bound-plan-entry))
   'snapshot-plan-size
   (and bound-plan-entry (snapshot-file-size bound-plan-entry))
   'snapshot-files
   (if snapshot
       (for/list ([f (in-list (plan-snapshot-manifest-files snapshot))])
         (hasheq 'path
                 (snapshot-file-path f)
                 'sha256
                 (snapshot-file-sha256 f)
                 'size
                 (snapshot-file-size f)))
       '())
   'drift
   (if (and selected-campaign-id snapshot)
       (with-handlers ([exn:fail? (lambda (_) '())])
         (for/list ([item (in-list (classify-snapshot-drift base-dir selected-campaign-id))])
           (hasheq 'path (car item) 'class (cadr item))))
       '())
   'declared-wave-paths
   declared-wave-paths
   'bound-wave-paths
   bound-wave-paths
   'status-projections
   status-projections))

;; format-plan-artifact-explanation : hash? -> string?
(define (format-plan-artifact-explanation explanation)
  (define (line label value)
    (format "~a: ~a\n" label (if value value "(none)")))
  (string-append
   "== plan inspection (read-only) ==\n"
   (line "plan" (hash-ref explanation 'plan-path))
   (line "base" (hash-ref explanation 'base-dir))
   (line "campaign" (hash-ref explanation 'campaign-id))
   (line "campaign source" (hash-ref explanation 'campaign-source))
   (line "stored plan-id" (hash-ref explanation 'stored-plan-id))
   (line "recomputed manifest hash" (hash-ref explanation 'recomputed-manifest-hash))
   (line "identity matches"
         (cond
           [(not (hash-ref explanation 'campaign-id)) "n/a (no bound campaign)"]
           [(hash-ref explanation 'identity-matches?) "yes"]
           [else "NO (stored plan-id differs from the recomputed manifest hash)"]))
   (line "bound plan-body hash" (hash-ref explanation 'bound-plan-body-hash))
   (line "live plan-body hash" (hash-ref explanation 'live-plan-body-hash))
   (line "bound body matches live"
         (cond
           [(hash-ref explanation 'campaign-id)
            (if (hash-ref explanation 'bound-body-matches-live?) "yes" "NO (drift)")]
           [else "n/a (no bound campaign)"]))
   (line "snapshot manifest digest" (hash-ref explanation 'snapshot-manifest-digest))
   (line "snapshot PLAN.md sha256" (hash-ref explanation 'snapshot-plan-sha256))
   (format "declared wave paths (~a):\n" (length (hash-ref explanation 'declared-wave-paths)))
   (string-join (for/list ([row (in-list (hash-ref explanation 'declared-wave-paths))])
                  (format "  W~a  ~a~a~a\n"
                          (hash-ref row 'wave)
                          (hash-ref row 'path)
                          (if (hash-ref row 'declared?) "  [declared]" "  [synthesized]")
                          (let ([refusal (hash-ref row 'refusal)])
                            (if refusal
                                (format "  [INVALID: ~a]" refusal)
                                ""))))
                "")
   (if (null? (hash-ref explanation 'bound-wave-paths))
       ""
       (string-append
        (format "bound wave paths (~a):\n" (length (hash-ref explanation 'bound-wave-paths)))
        (string-join (for/list ([row (in-list (hash-ref explanation 'bound-wave-paths))])
                       (format "  W~a  ~a\n" (hash-ref row 'wave) (hash-ref row 'path)))
                     "")))
   (format "status projections (~a waves):\n" (length (hash-ref explanation 'status-projections)))
   (string-join (for/list ([row (in-list (hash-ref explanation 'status-projections))])
                  (format "  W~a  index=~a  doc=~a  durable=~a~a\n"
                          (hash-ref row 'wave)
                          (or (hash-ref row 'index-status) "(none)")
                          (or (hash-ref row 'doc-status) "(none)")
                          (or (hash-ref row 'durable-status) "(no campaign)")
                          (if (hash-ref row 'resolvable?) "" "  [UNRESOLVABLE doc]")))
                "")
   (if (null? (hash-ref explanation 'drift))
       ""
       (string-append (format "snapshot drift (~a):\n" (length (hash-ref explanation 'drift)))
                      (string-join (for/list ([row (in-list (hash-ref explanation 'drift))])
                                     (format "  ~a  ~a\n" (hash-ref row 'class) (hash-ref row 'path)))
                                   "")))))

;; ============================================================
;; Provide
;; ============================================================

;; findings
(provide plan-validation-finding
         plan-validation-finding?
         plan-validation-finding-wave-idx
         plan-validation-finding-kind
         plan-validation-finding-message
         plan-validation-finding-code
         plan-validation-finding-next-action
         plan-validation-finding-artifacts
         plan-validation-error-findings
         plan-validation-warning-findings
         plan-validation-error-codes
         plan-validation-next-actions
         plan-validation-artifacts
         plan-validation-diagnostics
         ;; wave-document path grammar (one parser)
         parse-wave-doc-rel-path
         wave-doc-rel-path?
         wave-doc-rel-path-refusal
         wave-doc-rel-path-index
         wave-artifact-name-refusal
         ;; loader
         missing-wave-docs-error-text
         declared-file-path
         file-attribution-findings
         load-plan-from-index
         ;; preflight kernel
         validate-plan-artifacts
         format-plan-validation-findings
         ;; read-only inspection
         plan-artifact-explanation
         format-plan-artifact-explanation)
