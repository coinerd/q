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
;;   * the ONE preflight composition kernel (pure declared-path syntax and
;;     canonical paths live in planning-paths.rkt; readers own link safety);
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
;;     (unsafe arrow, then symlinked declared doc, then aggregated missing,
;;     then an unknown row status);
;;   * wave-executor.rkt re-provides every moved name, so legacy importers
;;     keep working unchanged.
;;
;; DELIBERATE NON-BLOCKING FINDINGS: newly surfaced wave-doc lint,
;; attribution, and snapshot-reference bindability issues are advisory for /go
;; compatibility. Unknown PLAN.md row statuses are different: they are
;; blocking findings with the stable code `wave-status-unknown` (W5, R-019),
;; because the historical `pending` fallback made a typo dispatchable.
;; Snapshot drift is different again: every actual drift class under an
;; explicitly inspected campaign is an error, matching
;; load-or-migrate-campaign!; absent/unknown campaign state stays advisory.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         "plan-types.rkt"
         ;; W5 (R-019): the ONE typed unknown-status rejection. Preflight turns
         ;; it into a stable finding; it is never allowed to escape as a crash
         ;; or degrade into a `pending` row.
         (only-in "wave-status.rkt" exn:fail:wave-status?)
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
                  classify-snapshot-drift
                  snapshot-dir
                  snapshot-drift-class-description)
         ;; W4 read-only doctor facts: ONE lease path, and the durable attempt
         ;; records. Inspection never acquires, releases, or rewrites any of them.
         (only-in "planning-paths.rkt" campaign-lock-file campaign-record-file)
         ;; W4: ownership is the ONE shared predicate (never a local mirror).
         (only-in "planning-artifact-ownership.rkt"
                  owned-planning-artifact-dir?)
         ;; W1 canonical path vocabulary and Verify declaration grammar.
         (only-in "planning-paths.rkt"
                  plan-file
                  planning-root
                  planning-path
                  waves-dir
                  campaigns-root
                  campaign-dir
                  parse-wave-doc-rel-path
                  wave-doc-rel-path?
                  wave-doc-rel-path-refusal
                  wave-doc-rel-path-index)
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
                  campaign-wave-status
                  campaign-wave-attempt-count
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  campaign-fence-token)
         (only-in "campaign-repository.rkt" load-campaign-record))

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
;; A plan-id / hash: 64 lowercase hex characters.
(define sha256-hex-rx #px"^[0-9a-f]{64}$")

;; wave-artifact-name-refusal preserves the historical planning-tool gate:
;; one real .md filename directly below waves/, with traversal/NUL refused.
;; Canonical W<n>/WAVE-<n> recognition is advisory, not a new write refusal.
(define (wave-artifact-name-refusal name)
  (cond
    [(not (string? name)) "artifact name must be a string"]
    [(not (string-prefix? name "waves/"))
     (format "artifact name ~a is not a wave document path (expected waves/W<idx>-<slug>.md)" name)]
    [(string-contains? name "\x00") (format "artifact name ~a contains a NUL byte" name)]
    [else
     (define rest (substring name (string-length "waves/")))
     (cond
       [(string=? rest "") "artifact name waves/ names no document"]
       [(string-contains? rest "/") (format "artifact name ~a nests below waves/" name)]
       [(not (string-suffix? rest ".md")) (format "artifact name ~a is not a .md document" name)]
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

;; string->wave-status-from-entry : wave-index-entry? -> plan-types WaveStatus
;; W5: delegate the spelling vocabulary to the shared status owner (via
;; plan-types). No local table and no `pending` fallback: an unknown spelling
;; raises the typed `exn:fail:wave-status` and entry-status-issue below turns
;; it into a stable blocking finding.
(define (string->wave-status-from-entry e)
  (string->wave-status (wave-index-entry-status e)))

;; entry-status-issue : wave-index-entry? -> (or/c hash? #f)
;; PURE. The stable row issue for an unknown PLAN.md bracket spelling, or #f
;; when the row's status is accepted. The message names the wave, declares
;; the offending spelling and lists the accepted spellings (from the shared
;; owner's typed exception); the code is stable for machine consumers.
(define (entry-status-issue e)
  (define (issue-for reason)
    (define idx (wave-index-entry-idx e))
    (hasheq 'idx
            idx
            'kind
            'status
            'code
            'wave-status-unknown
            'message
            (format "W~a: ~a" idx reason)
            'next-action
            (format
             (string-append
              "correct W~a's bracket status in .planning/PLAN.md to one "
              "accepted spelling (or fix the wave doc header); no wave is "
              "dispatched")
             idx)
            'artifacts
            (list (entry-doc-display-path e))))
  (with-handlers ([exn:fail:wave-status?
                   (lambda (ex) (issue-for (exn-message ex)))]
                  ;; A status the shared owner recognizes but the plan-row
                  ;; vocabulary cannot represent is refused the same stable
                  ;; way, never as an unreadable plan.
                  [exn:fail?
                   (lambda (ex) (issue-for (exn-message ex)))])
    (string->wave-status-from-entry e)
    #f))

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
    [(pair? missing-displays)
     (values #f missing-issues (missing-wave-docs-error-text entries missing-displays))]
    [else
     (void text)
     ;; W5: an unknown bracket status is a stable row refusal (never a
     ;; `pending` wave and never an uncaught typed exception). Path refusals
     ;; keep their historical priority above this check.
     (define status-issues
       (for/list ([e (in-list entries)]
                  #:do [(define issue (entry-status-issue e))]
                  #:when issue)
         issue))
     (cond
       [(pair? status-issues)
        (values #f status-issues (issue-message status-issues))]
       [else
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
        (values (gsd-plan waves #f '() '()) '() #f)])]))

;; load-plan-from-index : path-string? -> (or/c gsd-plan? #f)
;; The legacy loader contract: #f when PLAN.md is absent or the index is
;; empty; otherwise the declared documents load, or an exn names the first
;; refusal in the historical priority order (unsafe arrow, symlinked declared
;; doc, missing document, then an unknown row status).
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
                      (define-values (idx refusal)
                        (parse-wave-doc-rel-path (string-append "waves/" filename)))]
                #:when (and (not refusal) (file-exists? f) (not (member (path->string f) claimed))))
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
;; classes are surfaced with stable codes. Every actual drift or unreadable
;; snapshot is an error; only an explicitly absent/unknown campaign is
;; advisory. No inspection path rewrites or rebinds the snapshot.
(define (snapshot-drift-class-code kind)
  (case kind
    [(symlink) 'snapshot-drift-symlink]
    [(missing) 'snapshot-drift-missing]
    [(content-drift) 'snapshot-drift-content-drift]
    [(frozen-contract-stale) 'snapshot-drift-frozen-contract-stale]
    [(snapshot-absent) 'snapshot-absent]
    [(campaign-unknown) 'campaign-unknown]
    [(unreadable) 'snapshot-unreadable]
    [else 'snapshot-drift-unknown]))

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
    [(snapshot-absent)
     "inspect the durable campaign identity; no snapshot drift is claimed without a manifest"]
    [(campaign-unknown)
     "pass a 64-hex durable campaign id or omit --campaign to inspect the newest record"]
    [else (format "inspect the campaign snapshot for ~a" campaign-id)]))

(define (snapshot-drift-findings base-dir campaign-id)
  (define classified
    (cond
      [(not (and (string? campaign-id) (regexp-match? sha256-hex-rx campaign-id)))
       (list (list "PLAN.md" 'campaign-unknown #f))]
      [else
       (with-handlers ([exn:fail? (lambda (e) (list (list "PLAN.md" 'unreadable (exn-message e))))])
         (define manifest (load-snapshot-manifest base-dir campaign-id #:sweep-stale? #f))
         (if (not manifest)
             (list (list "PLAN.md" 'snapshot-absent #f))
             (for/list ([item (in-list
                               (classify-snapshot-drift base-dir campaign-id #:sweep-stale? #f))])
               (list (car item) (cadr item) #f))))]))
  (for/list ([item (in-list classified)])
    (define rel (car item))
    (define kind (cadr item))
    (define detail (caddr item))
    (plan-validation-finding
     #f
     (if (memq kind '(symlink missing content-drift frozen-contract-stale unreadable))
         'error
         'warning)
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
;; declared-doc refusals (BUG-0023 strict / BUG-0082 identity), then the
;; per-row status refusal (W5 `wave-status-unknown`), then strict lint, then
;; mapping ambiguity, then the advisory classes (wave-doc lint, slug mismatch,
;; Verify span, orphan row, snapshot bindability, file attribution, and
;; snapshot drift when #:campaign-id names a campaign).
(define (validate-plan-artifacts base-dir
                                 [plan-path (plan-file base-dir)]
                                 #:campaign-id [campaign-id #f]
                                 #:explanation [explanation #f])
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
           (define advisory-findings
             (with-handlers
                 ([exn:fail?
                   (lambda (e)
                     (list (plan-validation-finding
                            #f
                            'warning
                            (format "advisory plan inspection failed: ~a" (exn-message e))
                            #:code 'plan-advisory-unreadable
                            #:next-action
                            "inspect the named planning artifacts; no authority was changed")))])
               (append (wave-doc-lint-findings base-dir)
                       (slug-mismatch-findings base-dir)
                       (verify-findings plan)
                       (orphan-wave-doc-findings base-dir entries)
                       (bindability-findings text entries)
                       (file-attribution-findings base-dir plan)
                       (if campaign-id
                           (snapshot-drift-findings base-dir campaign-id)
                           '()))))
           (define findings
             (append (strict-error-findings strict)
                     (strict-warning-findings strict)
                     (mapping-refusal-findings mapping-refusals)
                     (inspection-error-findings (and explanation (hash-ref explanation 'errors '())))
                     advisory-findings))
           (define errors (plan-validation-error-findings findings))
           (make-plan-validation-result
            plan-path
            base-dir
            campaign-id
            (null? errors)
            (and (pair? errors) (string-join (map plan-validation-finding-message errors) "\n"))
            plan
            findings)])])]))

(define (inspection-error-findings errors)
  (for/list ([error (in-list (or errors '()))])
    (plan-validation-finding
     #f
     'error
     (hash-ref error 'message)
     #:code (hash-ref error 'code 'inspection-unreadable)
     #:next-action "inspect or repair the named durable state; --explain never repairs it")))

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

;; Return the newest readable id plus any unreadable candidates. A corrupt
;; newer record is never silently presented as absence or as the newest record.
(define (newest-campaign-for-inspection base-dir)
  (define dir (campaigns-root base-dir))
  (if (not (directory-exists? dir))
      (values #f '() "none")
      (let ()
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
        (let loop ([remaining (sort candidates (lambda (a b) (> (cdr a) (cdr b))))]
                   [errors '()]
                   [skipped? #f])
          (cond
            [(null? remaining) (values #f (reverse errors) (if (pair? errors) "unreadable" "none"))]
            [else
             (define id (caar remaining))
             (define-values (rec read-error)
               (with-handlers ([exn:fail? (lambda (e) (values #f e))])
                 (values (load-campaign-record base-dir id #:sweep-stale? #f) #f)))
             (cond
               [rec (values id (reverse errors) (if skipped? "newest-readable" "newest-record"))]
               [else
                (loop (cdr remaining)
                      (cons (hasheq 'code
                                    'campaign-candidate-unreadable
                                    'message
                                    (format "~a: ~a"
                                            id
                                            (if read-error
                                                (exn-message read-error)
                                                "record disappeared during inspection")))
                            errors)
                      #t)])])))))

;; plan-artifact-explanation : path-string? [#:campaign-id string?] -> hash?
;; Read-only structured inspection: declared wave paths, identity namespaces,
;; bound/live hashes, per-wave status projections, the authoritative record,
;; active attempt/fence, the lease facts, and the exact LEGAL operator actions.
;; Never writes, rebinds, repairs, sweeps, acquires, or releases campaign
;; state; it cannot bless drift and it offers no force release.

;; Read-only lease facts. Inspection NEVER probes the OS advisory lock (a probe
;; could momentarily deny a real /go acquire), so it reports the recorded
;; holder and states honestly that held-vs-stale is undetermined.
(define (campaign-lease-inspection base-dir plan-id)
  (define lock-path (campaign-lock-file base-dir plan-id))
  (define recorded
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (and (file-exists? lock-path)
           (call-with-input-file lock-path
             (lambda (in)
               (define datum (read in))
               (and (hash? datum) datum))))))
  (define present? (file-exists? lock-path))
  (hasheq 'path
          (path->string lock-path)
          'file-exists?
          present?
          'recorded-owner
          (and recorded (hash-ref recorded 'owner #f))
          'recorded-pid
          (and recorded (hash-ref recorded 'pid #f))
          'recorded-acquired
          (and recorded (hash-ref recorded 'acquired #f))
          'assessment
          (if present?
              'undetermined-read-only
              'no-lock-file)
          'note
          (string-append "a lock FILE is not proof of a held lease: only a live OS advisory "
                         "lock makes /go refuse lease-busy, and no force-release command exists")))

;; Per-wave durable attempt/fence facts (what recovery would resume).
(define (campaign-attempt-inspection rec)
  (if (not rec)
      '()
      (for/list ([w (in-list (campaign-record-waves rec))])
        (define attempt (campaign-wave-current-attempt w))
        (hasheq 'wave
                (campaign-wave-index w)
                'status
                (campaign-wave-status w)
                'attempt-count
                (campaign-wave-attempt-count w)
                'current-attempt-id
                (and attempt (campaign-attempt-id attempt))
                'current-attempt-fence
                (and attempt (campaign-attempt-fence-token attempt))))))

;; Read-only inventory of user temp/backup/editor artifacts in the planning
;; root, the campaigns root, and the selected campaign directory. It is
;; additive and NON-FAILING: the doctor reports what it saw and nothing is
;; deleted, moved, renamed, or "cleaned up". A candidate is labeled `unowned`
;; UNLESS it carries an AUTHENTIC ownership marker (the ONE predicate in
;; planning-artifact-ownership.rkt: real nofollow marker file, hash datum,
;; schema-version 1, matching kind) — the same predicate the snapshot sweep,
;; supersede rotation and archive cleanup use to authorize a delete. A
;; directory carrying a marker q itself writes (plan-snapshot staging,
;; plan-supersede wave backup) is KNOWN OURS and is listed separately (never as
;; an unknown artifact). Lock files are skipped entirely: inspection never
;; opens or probes a lease lock.
(define unknown-planning-artifact-name-rx
  #px"(?i:(?:\\.bak|\\.backup|\\.tmp|\\.temp|\\.orig|\\.rej|\\.swp|\\.save)(?:[._-]|$)|~$|^#.*#$|^\\.#|(?:^|[-_.])backup(?:[._-]|$))")

(define (unknown-planning-artifact-name? name)
  (and (string? name)
       (not (regexp-match? #rx"\\.lock$" name))
       (regexp-match? unknown-planning-artifact-name-rx name)))

(define (planning-artifact-kind p)
  (cond
    [(link-exists? p) 'symlink]
    [(directory-exists? p) 'directory]
    [(file-exists? p) 'file]
    [else 'other]))

;; Ownership is NOT decided here: the authentic-marker predicate is owned by
;; planning-artifact-ownership.rkt and shared with the destructive consumers
;; (snapshot sweep, supersede rotation, archive cleanup), so the doctor can
;; never bless an artifact those paths would refuse to delete.
(define (planning-artifact-display-path base-complete p)
  (with-handlers ([exn:fail? (lambda (_) (path->string p))])
    (path->string (find-relative-path base-complete p))))

(define (scan-planning-artifacts dir location base-complete)
  (if (not (directory-exists? dir))
      '()
      (with-handlers ([exn:fail? (lambda (_) '())])
        (for/list ([p (in-list (sort (directory-list dir #:build? #t) path<?))]
                   #:do [(define name (path->string (file-name-from-path p)))]
                   #:when (unknown-planning-artifact-name? name))
          (define kind (planning-artifact-kind p))
          (hasheq 'path
                  (planning-artifact-display-path base-complete p)
                  'location
                  location
                  'kind
                  kind
                  'ownership
                  (if (and (eq? kind 'directory) (owned-planning-artifact-dir? p))
                      'owned
                      'unowned))))))

;; The exact legal operator actions for the inspected state, in priority order.
;; Nothing here resets, resnapshots, deletes state, or force-releases a lease.
(define (plan-inspection-legal-actions rec snapshot drift
                                       #:record-unreadable? [record-unreadable? #f])
  (append
   (cond
     [record-unreadable?
      (list (string-append "the durable record could not be read: inspect the named path and its "
                           "error; do not delete or regenerate it"))]
     [(not rec)
      (list (string-append "no durable campaign is bound yet: finish the plan edits, then run "
                           "/go once to bind ONE immutable snapshot"))]
     [else '()])
   (for/list ([row (in-list drift)])
     (format "resolve ~a for .planning/~a — ~a"
             (hash-ref row 'class)
             (hash-ref row 'path)
             (snapshot-drift-class-description (hash-ref row 'class))))
   (if (and rec (not snapshot))
       (list (string-append "this durable record has no verified snapshot: inspect the record "
                            "itself; no drift class is claimed without a manifest"))
       '())
   (if (and rec snapshot (null? drift))
       (list "resume with /go: status-only projection edits are informational, not drift")
       '())
   (list (string-append "never delete campaign state, a `.bak` file, or a backup directory "
                        "to repair drift: restore from the snapshot, re-freeze under a new "
                        "campaign identity, or stop and correct the authored input"))))

(define (plan-artifact-explanation base-dir #:campaign-id [campaign-id #f])
  (define plan-path (plan-file base-dir))
  (define base-complete (simplify-path (path->complete-path base-dir)))
  (define (inspection-error code error)
    (hasheq 'code code 'message (exn-message error)))
  (define-values (text plan-read-error)
    (if (file-exists? plan-path)
        (with-handlers ([exn:fail? (lambda (e) (values #f (inspection-error 'plan-unreadable e)))])
          (values (call-with-input-file plan-path port->string) #f))
        (values #f #f)))
  (define entries
    (if text
        (parse-plan-index text)
        '()))
  (define-values (selected-campaign-id candidate-errors selected-campaign-source)
    (if campaign-id
        (values campaign-id '() 'requested)
        (newest-campaign-for-inspection base-dir)))
  (define-values (rec campaign-error)
    (cond
      [(not selected-campaign-id) (values #f #f)]
      [(not (regexp-match? sha256-hex-rx selected-campaign-id))
       (values
        #f
        (hasheq 'code 'campaign-unknown 'message "campaign id must be 64 lowercase hex characters"))]
      [else
       (with-handlers ([exn:fail? (lambda (e) (values #f (inspection-error 'campaign-unreadable e)))])
         (values (load-campaign-record base-dir selected-campaign-id #:sweep-stale? #f) #f))]))
  (define manifest (and rec (campaign-record-manifest rec)))
  (define-values (snapshot snapshot-error)
    (if (and selected-campaign-id (not campaign-error))
        (with-handlers ([exn:fail? (lambda (e)
                                     (values #f (inspection-error 'snapshot-unreadable e)))])
          (values (load-snapshot-manifest base-dir selected-campaign-id #:sweep-stale? #f) #f))
        (values #f #f)))
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
  ;; Additive, non-failing artifact inventory (see scan-planning-artifacts).
  ;; Only the selected campaign directory is inspected, never another
  ;; campaign's state.
  (define inspected-planning-artifacts
    (append
     (scan-planning-artifacts (planning-root base-dir) 'planning-root base-complete)
     (scan-planning-artifacts (campaigns-root base-dir) 'campaigns-root base-complete)
     (if (and selected-campaign-id (regexp-match? sha256-hex-rx selected-campaign-id))
         (scan-planning-artifacts (campaign-dir base-dir selected-campaign-id)
                                  'campaign-dir
                                  base-complete)
         '())))
  (define unknown-artifacts
    (for/list ([row (in-list inspected-planning-artifacts)]
               #:when (eq? (hash-ref row 'ownership) 'unowned))
      row))
  (define owned-planning-artifacts
    (for/list ([row (in-list inspected-planning-artifacts)]
               #:when (eq? (hash-ref row 'ownership) 'owned))
      row))
  (define-values (drift drift-error)
    (if (and selected-campaign-id snapshot)
        (with-handlers ([exn:fail? (lambda (e)
                                     (values '() (inspection-error 'snapshot-drift-unreadable e)))])
          (values (for/list ([item (in-list (classify-snapshot-drift base-dir
                                                                     selected-campaign-id
                                                                     #:sweep-stale? #f))])
                    (hasheq 'path (car item) 'class (cadr item)))
                  #f))
        (values '() #f)))
  (hasheq
   'plan-path
   (path->string plan-path)
   'base-dir
   (path->string base-dir)
   'plan-exists?
   (and text #t)
   'campaign-id
   selected-campaign-id
   'campaign-record-path
   (and selected-campaign-id
        (regexp-match? sha256-hex-rx selected-campaign-id)
        (path->string (campaign-record-file base-dir selected-campaign-id)))
   'snapshot-path
   (and selected-campaign-id
        (regexp-match? sha256-hex-rx selected-campaign-id)
        (path->string (snapshot-dir base-dir selected-campaign-id)))
   'authoritative
   (if rec 'durable-campaign-record 'live-plan-unbound)
   'campaign-fence-token
   (and rec (campaign-fence-token rec))
   'attempts
   (campaign-attempt-inspection rec)
   'lease
   (and selected-campaign-id
        (regexp-match? sha256-hex-rx selected-campaign-id)
        (campaign-lease-inspection base-dir selected-campaign-id))
   'unknown-artifacts
   unknown-artifacts
   'owned-planning-artifacts
   owned-planning-artifacts
   'legal-actions
   (plan-inspection-legal-actions rec
                                   snapshot
                                   drift
                                   #:record-unreadable?
                                   (or (and campaign-error #t)
                                       (and (not campaign-id)
                                            (pair? candidate-errors))))
   'campaign-source
   (cond
     [campaign-id 'requested]
     [selected-campaign-id (string->symbol selected-campaign-source)]
     [else (and (pair? candidate-errors) 'unreadable)])
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
   drift
   'errors
   (append (filter values (list plan-read-error))
           candidate-errors
           (filter values (list campaign-error snapshot-error drift-error)))
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
  (define (inspection-error? codes)
    (for/or ([error (in-list (hash-ref explanation 'errors))])
      (memq (hash-ref error 'code) codes)))
  (define record-inspection-blocked?
    (or (inspection-error? '(campaign-unreadable campaign-unknown))
        (and (not (hash-ref explanation 'campaign-id))
             (inspection-error? '(campaign-candidate-unreadable)))))
  (define snapshot-unreadable? (inspection-error? '(snapshot-unreadable)))
  (define (snapshot-value key)
    (cond
      [record-inspection-blocked? "(not inspected: campaign record unreadable)"]
      [snapshot-unreadable? "(unreadable)"]
      [else (hash-ref explanation key)]))
  (string-append
   "== plan inspection (read-only) ==\n"
   (line "plan" (hash-ref explanation 'plan-path))
   (line "base" (hash-ref explanation 'base-dir))
   (line "campaign"
         (or (hash-ref explanation 'campaign-id)
             (and (eq? (hash-ref explanation 'campaign-source) 'unreadable) "(unreadable)")))
   (line "campaign source" (hash-ref explanation 'campaign-source))
   (line "stored plan-id" (hash-ref explanation 'stored-plan-id))
   (line "recomputed manifest hash" (hash-ref explanation 'recomputed-manifest-hash))
   (line "identity matches"
         (cond
           [(not (hash-ref explanation 'stored-plan-id)) "n/a (no durable record)"]
           [(hash-ref explanation 'identity-matches?) "yes"]
           [else "NO (stored plan-id differs from the recomputed manifest hash)"]))
   (line "bound plan-body hash" (hash-ref explanation 'bound-plan-body-hash))
   (line "live plan-body hash" (hash-ref explanation 'live-plan-body-hash))
   (line "bound body matches live"
         (cond
           [(not (hash-ref explanation 'stored-plan-id)) "n/a (no durable record)"]
           [(not (hash-ref explanation 'bound-plan-body-hash)) "n/a (no bound plan-body hash)"]
           [(hash-ref explanation 'bound-body-matches-live?) "yes"]
           [else "NO (drift)"]))
   ;; W4 doctor: what is authoritative, what recovery would resume, and what
   ;; the lease facts are — all read-only, with no force release offered.
   "what is authoritative:\n"
   (line "  durable record"
         (or (hash-ref explanation 'campaign-record-path) "(none)"))
   (line "  bound snapshot" (or (hash-ref explanation 'snapshot-path) "(none)"))
   (line "  live plan (authored input)" (hash-ref explanation 'plan-path))
   (line "  authority used by /go" (format "~a" (hash-ref explanation 'authoritative)))
   (line "campaign fence token" (hash-ref explanation 'campaign-fence-token))
   (format "attempts (~a waves):\n" (length (hash-ref explanation 'attempts)))
   (string-join (for/list ([row (in-list (hash-ref explanation 'attempts))])
                  (format "  W~a  status=~a  attempt-count=~a  current-attempt=~a  fence=~a\n"
                          (hash-ref row 'wave)
                          (hash-ref row 'status)
                          (hash-ref row 'attempt-count)
                          (or (hash-ref row 'current-attempt-id) "(none)")
                          (or (hash-ref row 'current-attempt-fence) "(none)")))
                "")
   (let ([lease (hash-ref explanation 'lease)])
     (if (not lease)
         "lease: (none: no campaign inspected)\n"
         (string-append
          (line "lease lock file" (hash-ref lease 'path))
          (line "lease file exists" (if (hash-ref lease 'file-exists?) "yes" "no"))
          (line "lease recorded holder"
                (if (hash-ref lease 'file-exists?)
                    (format "~a (pid ~a, acquired ~a)"
                            (or (hash-ref lease 'recorded-owner) "unrecorded")
                            (or (hash-ref lease 'recorded-pid) "unrecorded")
                            (or (hash-ref lease 'recorded-acquired) "unrecorded"))
                    "(no lock file)"))
          (line "lease assessment" (hash-ref lease 'assessment))
          (line "lease note" (hash-ref lease 'note)))))
   (format "legal operator action(s) (~a):\n" (length (hash-ref explanation 'legal-actions)))
   (string-join (for/list ([action (in-list (hash-ref explanation 'legal-actions))])
                  (format "  - ~a\n" action))
                "")
   (line "snapshot manifest digest" (snapshot-value 'snapshot-manifest-digest))
   (line "snapshot PLAN.md sha256" (snapshot-value 'snapshot-plan-sha256))
   (if (null? (hash-ref explanation 'errors))
       ""
       (string-append
        "inspection errors:\n"
        (string-join (for/list ([error (in-list (hash-ref explanation 'errors))])
                       (format "  [~a] ~a\n" (hash-ref error 'code) (hash-ref error 'message)))
                     "")))
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
                                   "")))
   ;; Additive artifacts section: informational only. Nothing here is an
   ;; error, and nothing is deleted or cleaned up.
   (format "unknown planning artifacts (~a, informational; nothing is removed):\n"
           (length (hash-ref explanation 'unknown-artifacts)))
   (if (null? (hash-ref explanation 'unknown-artifacts))
       "  (none)\n"
       (string-join (for/list ([row (in-list (hash-ref explanation 'unknown-artifacts))])
                      (format "  [unowned] ~a  (~a ~a)\n"
                              (hash-ref row 'path)
                              (hash-ref row 'location)
                              (hash-ref row 'kind)))
                    ""))
   (if (null? (hash-ref explanation 'owned-planning-artifacts))
       ""
       (string-append
        (format "known owned planning artifacts (~a, excluded from unknown):\n"
                (length (hash-ref explanation 'owned-planning-artifacts)))
        (string-join (for/list ([row (in-list (hash-ref explanation 'owned-planning-artifacts))])
                       (format "  [owned] ~a  (~a ~a)\n"
                               (hash-ref row 'path)
                               (hash-ref row 'location)
                               (hash-ref row 'kind)))
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
