#lang racket/base

;; extensions/gsd/wave-docs.rkt — Wave document I/O and PLAN.md index management
;; STABILITY: evolving
;;
;; v0.21.1 W0: Per-wave document files in .planning/waves/.
;; PLAN.md index with status markers ([Inbox], [DONE], [DEFERRED], [FAILED]).
;; Dual-write: wave doc + PLAN.md index on status transitions.
;;
;; DUAL-WRITE INVARIANT (I-24, v0.35.1):
;; Every wave status change must update BOTH:
;;   1. The per-wave document file (.planning/waves/W*.md)
;;   2. The PLAN.md index table (status column)
;; mark-wave-status! is the ONLY sanctioned writer (BUG-0034): it
;; dual-writes the doc header AND the PLAN.md index row, and leaves a
;; debug-level audit trace. check-status-consistency is the read-path
;; guard that makes any divergence between the two sources loud.
;; External edits that update only one of the two are a bug.
;;
;; SINGLE STATUS MARKER (BUG-0050, W2): the machine-managed header is
;; the ONLY sanctioned `Status:` line of a wave doc. The legacy authored
;; doc template carried a second `Status: PENDING` line in the body that
;; no writer ever updated — it went stale forever while the header moved
;; on (DONE-on-top / PENDING-below drift). The authoring path now strips
;; body `Status:` lines at write time, and the consistency checker + doc
;; lint flag any survivor by name (advisory, BUG-0034 convention).

(require racket/contract
         racket/format
         racket/string
         racket/file
         racket/list
         racket/path
         racket/port
         (only-in "shared.rkt" slugify)
         (only-in "wave-status.rkt"
                  STATUS-INBOX
                  STATUS-DONE
                  STATUS-DEFERRED
                  STATUS-FAILED
                  done-or-deferred?
                  active-status?
                  normalize-status!))

;; Struct exports (plain)
(provide wave-index-entry
         wave-index-entry?
         wave-index-entry-idx
         wave-index-entry-title
         wave-index-entry-slug
         wave-index-entry-status
         ;; BUG-0034 (W2): one dual-source status divergence
          status-divergence
          status-divergence?
          status-divergence-wave-idx
          status-divergence-plan-status
          status-divergence-doc-status
          status-divergence-plan-path
          status-divergence-doc-path
          ;; BUG-0050 (W2): 'index-vs-doc-header | 'body-vs-header
          status-divergence-kind
         ;; BUG-0041 (W4): one wave-doc lint violation
         wave-doc-violation
         wave-doc-violation?
         wave-doc-violation-wave-idx
         wave-doc-violation-doc-path
         wave-doc-violation-section
         ;; BUG-0041 (W4): index-arrow ↔ doc-slug mismatch
         slug-mismatch
         slug-mismatch?
         slug-mismatch-wave-idx
         slug-mismatch-arrow-slug
         slug-mismatch-disk-slug
         slug-mismatch-doc-path
         ;; Functions (contracted)
         (contract-out
          [wave-doc-path (-> path-string? exact-nonnegative-integer? string? path?)]
          [write-wave-doc! (-> path-string? exact-nonnegative-integer? string? string? string? path?)]
          [read-wave-doc (-> path-string? exact-nonnegative-integer? string? (or/c hash? #f))]
          [parse-wave-doc-from-string (->* (string? any/c string?) ((or/c path? #f)) hash?)]
          [slugify (-> string? string?)]
          [parse-plan-index (-> string? (listof wave-index-entry?))]
          [update-wave-in-index! (-> path-string? exact-nonnegative-integer? string? boolean?)]
          [update-plan-index-text (-> string? exact-nonnegative-integer? string? string?)]
          [next-inbox-wave (-> path-string? (or/c wave-index-entry? #f))]
          [find-next-inbox-entry (-> (listof wave-index-entry?) (or/c wave-index-entry? #f))]
          [mark-wave-status! (-> path-string? exact-nonnegative-integer? string? boolean?)]
          [wave-slug (-> path-string? exact-nonnegative-integer? (or/c string? #f))]
          [plan-slug-map (-> path-string? hash?)]
          [plan-overall-status (-> path-string? symbol?)]
          [compute-plan-overall-status (-> (listof wave-index-entry?) symbol?)]
          [wave-exists? (-> path-string? exact-nonnegative-integer? string? boolean?)]
          [wave-status-markers (-> (listof pair?))]
          [count-inline-wave-sections (-> string? exact-nonnegative-integer?)]
          [index-entry-doc-display-path (-> wave-index-entry? string?)]
          [missing-index-doc-paths (-> path-string? (listof wave-index-entry?) (listof string?))]
          [check-status-consistency (-> path-string? (listof status-divergence?))]
          [format-status-divergence-warning (-> status-divergence? string?)]
          [resolve-status-precedence (-> string? string? string?)]
          [plan-format-deprecation-warnings (-> string? (listof string?))]
          [plan-format-deprecation-warning-lines (-> path-string? (listof string?))]
          ;; BUG-0041 (W4): wave-doc lint at /go entry with recorded verdicts
          [lint-wave-doc (-> hash? (listof wave-doc-violation?))]
          [lint-campaign-wave-docs (-> path-string? (listof wave-doc-violation?))]
          [wave-doc-lint-warning-lines (-> path-string? (listof string?))]
          [format-wave-doc-lint-warning
           (-> exact-nonnegative-integer? string? (listof wave-doc-violation?) string?)]
          [check-slug-consistency (-> path-string? (listof slug-mismatch?))]
          [format-slug-mismatch-warning (-> slug-mismatch? string?)]
          [slug-mismatch-warning-lines (-> path-string? (listof string?))]
          [store-wave-doc-lint-verdict! (-> path-string? string? boolean?)]))

;; ============================================================
;; Constants
;; ============================================================

(define WAVE-STATUS-MARKERS
  (list (cons STATUS-INBOX "[Inbox]")
        (cons "In-Progress" "[In-Progress]")
        (cons STATUS-DONE "[DONE]")
        (cons STATUS-DEFERRED "[DEFERRED]")
        (cons STATUS-FAILED "[FAILED]")))

(define (wave-status-markers)
  WAVE-STATUS-MARKERS)

;; Pre-compiled regex patterns (using #rx for proper \n handling)
(define wave-header-rx #rx"^# Wave [0-9]+\nStatus: ([^\n]+)\n")
(define wave-header-full-rx #rx"^# Wave [0-9]+\nStatus: [^\n]+\n\n(.*)$")
(define index-line-rx #rx"^[-*] +\\[([A-Za-z-]+)\\] +W([0-9]+): +(.+?)(?: +\u2192 +(.+))?$")
(define relaxed-index-line-rx #rx"^[-*] +W([0-9]+): +(.+?)(?: +→ +(.+))?$")
(define slug-from-target-rx #rx"waves/W[0-9]+-(.+?)\\.md")

;; BUG-0050 (W2): a line-anchored `Status:` at column 0 ANYWHERE in the
;; doc body is a second status marker (stale authored-template residue).
;; Prose mentions of `Status:` sit mid-line (typically backticked) and
;; never match, so only real marker lines are affected.
(define body-status-line-rx #px"^Status:[ \t]*([^\n]*)$")

;; ============================================================
;; Slug generation
;; ============================================================

;; slugify + collapse-hyphens: imported from shared.rkt (v0.32.1 Wave 1 DRY)

;; ============================================================
;; Wave document path
;; ============================================================

(define (wave-doc-path base-dir idx slug)
  (build-path base-dir ".planning" "waves" (format "W~a-~a.md" idx slug)))

;; ============================================================
;; Wave document I/O
;; ============================================================

(define (wave-exists? base-dir idx slug)
  (file-exists? (wave-doc-path base-dir idx slug)))

;; strip-body-status-lines : string? -> string?
;; PURE (BUG-0050, W2). Removes every line-anchored `Status:` line from
;; wave-doc BODY text, so the machine-managed header stays the single
;; authoritative status marker. The authoring path (write-wave-doc!)
;; always routes bodies through this — including mark-wave-status!
;; rewrites, which therefore SELF-HEAL any legacy doc that still carries
;; a body `Status:` line. Prose lines that merely mention `Status:`
;; (mid-line, backticked) are untouched.
(define (strip-body-status-lines content)
  (string-join
   (for/list ([line (in-list (string-split content "\n"))]
              #:unless (regexp-match? body-status-line-rx line))
     line)
   "\n"))

;; body-status-line : string? -> (or/c string? #f)
;; PURE (BUG-0050, W2). The raw value of the FIRST line-anchored
;; `Status:` line in the doc body, #f when the body is clean. This is
;; the stale marker that never updates; detection surfaces (consistency
;; checker + doc lint) flag it by name.
(define (body-status-line content)
  (for/first ([line (in-list (string-split content "\n"))]
              #:when (regexp-match? body-status-line-rx line))
    (string-trim (cadr (regexp-match body-status-line-rx line)))))

(define (write-wave-doc! base-dir idx slug content status)
  (define path (wave-doc-path base-dir idx slug))
  (define dir (path-only path))
  (unless (directory-exists? dir)
    (make-directory* dir))
  ;; BUG-0050 (W2): the authoring path emits exactly ONE `Status:` line
  ;; (the machine header); any body `Status:` line is stripped.
  (define header (format "# Wave ~a\nStatus: ~a\n\n" idx status))
  (define sanitized-body (strip-body-status-lines content))
  (call-with-output-file path
                         (lambda (out)
                           (display header out)
                           (display sanitized-body out))
                         #:exists 'truncate)
  path)

(define (parse-wave-doc-from-string text idx slug [path #f])
  (define status (extract-status text))
  (define content (strip-status-header text))
  (hasheq 'index
          idx
          'slug
          slug
          'status
          status
          ;; BUG-0041 (W4): raw header presence (vs the "Inbox" default that
          ;; extract-status substitutes when the header is absent) so
          ;; lint-wave-doc can distinguish "no header" from "header: Inbox".
          'status-header?
          (and (regexp-match? wave-header-rx text) #t)
          'content
          content
          'path
          (if path
              (path->string path)
              #f)))

(define (read-wave-doc base-dir idx slug)
  (define path (wave-doc-path base-dir idx slug))
  (cond
    [(not (file-exists? path)) #f]
    [else
     (define text (call-with-input-file path port->string))
     (parse-wave-doc-from-string text idx slug path)]))

(define (extract-status text)
  (define m (regexp-match wave-header-rx text))
  (if m
      (string-trim (cadr m))
      "Inbox"))

(define (strip-status-header text)
  (define m (regexp-match wave-header-full-rx text))
  (if m
      (cadr m)
      text))

;; ============================================================
;; PLAN.md index parsing
;; ============================================================

(struct wave-index-entry (idx title slug status) #:transparent)

(define (parse-plan-index md-text)
  (define lines (string-split md-text "\n"))
  (for/fold ([entries '()]) ([line lines])
    (define m (regexp-match index-line-rx line))
    (if m
        ;; Standard format: - [Inbox] W0: Title
        (let* ([raw-status (cadr m)]
               [status (or (normalize-status! raw-status) raw-status)]
               [idx (string->number (caddr m))]
               [title (string-trim (cadddr m))]
               [target (and (list? m) (> (length m) 4) (list-ref m 4))]
               [slug (or (and target (extract-slug-from-target target)) (slugify title))])
          (append entries (list (wave-index-entry idx title slug status))))
        ;; Try relaxed format: - W0: Title (without status bracket)
        (let ([rm (regexp-match relaxed-index-line-rx line)])
          (if rm
              (let* ([idx (string->number (cadr rm))]
                     [title (string-trim (caddr rm))]
                     [target (and (list? rm) (> (length rm) 3) (list-ref rm 3))]
                     [slug (or (and target (extract-slug-from-target target)) (slugify title))])
                (append entries (list (wave-index-entry idx title slug STATUS-INBOX))))
              entries)))))
;; BUG-0023 (W2): a target that does not follow the W<n>-<slug>.md
;; convention yields NO slug, so the caller falls back to slugify(title).
;; Previously this returned (slugify target), producing nonsense expected
;; paths like W0-wavesnotesmd.md in strict-validation errors.
(define (extract-slug-from-target target)
  (define m (regexp-match slug-from-target-rx target))
  (and m (cadr m)))

;; ============================================================
;; Plan-format provenance & strict index validation (BUG-0023, W2)
;; ============================================================

;; Inline wave-section header — mirrors the wave-start regex in
;; plan-types-parser.rkt (`^## +[Ww]ave +[0-9]+`) so diagnostics report
;; exactly what the inline parser family would find.
(define inline-wave-section-rx #rx"^## +[Ww]ave +[0-9]+")

;; count-inline-wave-sections : string? -> exact-nonnegative-integer?
;; Count inline `## Wave N:` sections in PLAN.md text. Used by the /go
;; no-waves diagnostic (BUG-0023) so a rejection reports parser-provenance
;; counts for BOTH accepted formats instead of a bare "Plan has no waves".
(define (count-inline-wave-sections md-text)
  (for/sum ([line (in-list (string-split md-text "\n"))])
           (if (regexp-match? inline-wave-section-rx line) 1 0)))

;; index-entry-doc-display-path : wave-index-entry? -> string?
;; Path (relative to the project root) where an index entry's wave doc is
;; expected, following the W<n>-<slug>.md convention. Named verbatim in
;; strict-validation errors so authors can comply immediately.
(define (index-entry-doc-display-path e)
  (format ".planning/waves/W~a-~a.md" (wave-index-entry-idx e) (wave-index-entry-slug e)))

;; missing-index-doc-paths : path-string? (listof wave-index-entry?) -> (listof string?)
;; Display paths of index entries whose target wave doc does not exist on
;; disk. A non-empty result is a validation error (BUG-0023): previously the
;; wave loaded with silent empty content instead of erroring.
(define (missing-index-doc-paths base-dir entries)
  (for/list ([e (in-list entries)]
             #:unless (wave-exists? base-dir (wave-index-entry-idx e) (wave-index-entry-slug e)))
    (index-entry-doc-display-path e)))

;; ============================================================
;; Dual-source status consistency & precedence (BUG-0034, W2)
;; ============================================================

;; Wave status lives TWICE (see DUAL-WRITE INVARIANT above): the bracket
;; status in the PLAN.md index row and the `Status:` header of the wave
;; doc. mark-wave-status! keeps both in lockstep, but external edits
;; (editor, another concurrent TUI, manual fix-up) bypass it — and before
;; BUG-0034 no read path compared the two sources, so a reverted row
;; silently disagreed with the wave doc.

;; One divergence involving wave status, for one wave.
;;   plan-status : PLAN.md index row status (raw, e.g. "DONE"); for the
;;                 BUG-0050 'body-vs-header kind this carries the
;;                 AUTHORITATIVE machine-header status instead (both
;;                 claims live in the same doc file)
;;   doc-status  : wave-doc `Status:` header (raw, e.g. "PENDING"); for
;;                 'body-vs-header, the stale BODY `Status:` line value
;;   plan-path   : display path of the PLAN.md index (.planning/PLAN.md)
;;   doc-path    : display path of the wave doc (.planning/waves/W<n>-<slug>.md)
;;   kind        : 'index-vs-doc-header — BUG-0034 row↔header divergence
;;                 'body-vs-header     — BUG-0050 second `Status:` line
;;                                     in the doc body (any value: the
;;                                     header is the single marker)
(struct status-divergence (wave-idx plan-status doc-status plan-path doc-path kind)
  #:transparent)

;; status->symbol : (or/c string? symbol?) -> symbol?
;; Canonical comparison key. Mirrors campaign-state.rkt's
;; canonical-wave-status vocabulary so semantically IDENTICAL statuses
;; spelled differently ("Inbox" row vs "PENDING" doc header) compare
;; equal: the same meaning is not a BUG-0034 divergence. Unrecognized
;; values canonicalize to 'pending (same fallback as the campaign layer).
(define STATUS-CONSIDERED-EQUAL
  (list (list 'pending "INBOX" "PENDING" "NOT STARTED")
        (list 'in-progress "IN-PROGRESS" "IN PROGRESS")
        (list 'verifying "VERIFYING")
        (list 'done "DONE" "COMPLETED")
        (list 'failed "FAILED")
        (list 'interrupted "INTERRUPTED")
        (list 'deferred "DEFERRED")))

(define (status->symbol s)
  (define up
    (string-upcase (string-trim (if (symbol? s)
                                    (symbol->string s)
                                    s))))
  (define hit
    (for/first ([group (in-list STATUS-CONSIDERED-EQUAL)]
                #:when (member up (cdr group)))
      (car group)))
  (or hit 'pending))

;; check-status-consistency : path-string? -> (listof status-divergence?)
;; Pure read path: compares every PLAN.md index-row status against its
;; wave doc's `Status:` header, AND (BUG-0050) scans the doc BODY for a
;; second line-anchored `Status:` line. A divergence is reported ONLY
;; when the sources exist; kinds:
;;   'index-vs-doc-header — canonical row status ≠ canonical header
;;                         status (BUG-0034)
;;   'body-vs-header      — the doc body carries ANY `Status:` line
;;                         (BUG-0050): the header is the single
;;                         authoritative marker, so a second one is a
;;                         drift risk whatever its value
;; Missing wave docs are BUG-0023 strict-validation territory, not a
;; consistency concern. Never writes; callers turn divergences into
;; named warnings.
(define (check-status-consistency base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (not (file-exists? plan-path))
      '()
      (for/fold ([divs '()])
                ([e (in-list (parse-plan-index (call-with-input-file plan-path port->string)))])
        (define idx (wave-index-entry-idx e))
        (define doc
          (and (wave-exists? base-dir idx (wave-index-entry-slug e))
               (read-wave-doc base-dir idx (wave-index-entry-slug e))))
        (cond
          [(not doc) divs]
          [else
           (define row-status (wave-index-entry-status e))
           (define header-status (hash-ref doc 'status))
           (define doc-display (index-entry-doc-display-path e))
           (append
            divs
            ;; BUG-0034: PLAN.md index row vs wave-doc header.
            (if (not (eq? (status->symbol row-status) (status->symbol header-status)))
                (list (status-divergence idx
                                         row-status
                                         header-status
                                         ".planning/PLAN.md"
                                         doc-display
                                         'index-vs-doc-header))
                '())
            ;; BUG-0050: a second `Status:` line in the doc body. The
            ;; machine header (authoritative) rides plan-status; the
            ;; stale body line value rides doc-status.
            (let ([body (body-status-line (hash-ref doc 'content))])
              (if body
                  (list (status-divergence idx
                                           header-status
                                           body
                                           ".planning/PLAN.md"
                                           doc-display
                                           'body-vs-header))
                  '())))]))))

;; format-status-divergence-warning : status-divergence? -> string?
;; One named, user-visible warning per divergent wave (BUG-0034): names
;; BOTH file paths and both claimed statuses so a human can resolve the
;; disagreement. BUG-0050 'body-vs-header divergences get their own
;; named message (second `Status:` line in the doc body). Advisory only
;; — it never blocks /go by itself.
(define (format-status-divergence-warning d)
  (case (status-divergence-kind d)
    [(body-vs-header)
     (format (string-append "WARNING (duplicate Status line, BUG-0050): W~a — ~a declares TWO statuses: "
                            "machine header '~a' (authoritative) and a body `Status:` line '~a' "
                            "(stale authored-template residue no writer updates). "
                            "The header is the single sanctioned marker; remove the body line "
                            "(write-wave-doc! sanitizes it on rewrite).")
             (status-divergence-wave-idx d)
             (status-divergence-doc-path d)
             (status-divergence-plan-status d)
             (status-divergence-doc-status d))]
    [else
     (format (string-append "WARNING (status divergence, BUG-0034): W~a — PLAN.md row says [~a] "
                            "but wave doc header says '~a' (~a vs ~a). "
                            "mark-wave-status! is the only sanctioned writer; align one source.")
             (status-divergence-wave-idx d)
             (status-divergence-plan-status d)
             (status-divergence-doc-status d)
             (status-divergence-plan-path d)
             (status-divergence-doc-path d))]))

;; resolve-status-precedence : string? string? -> string?
;; DOCUMENTED PRECEDENCE (BUG-0034, applied to SELECTION only):
;;   * The wave DOC header wins for progress statuses — the doc is the
;;     wave's own record of where execution stands.
;;   * The PLAN.md row wins ONLY for [DEFERRED]: a deferred row stays
;;     deferred whatever the doc header claims (a stale header must not
;;     resurrect a wave a human deferred).
;; Implemented as exactly one function so tests pin the decision.
;; Selection reads statuses through it; everything else (warnings)
;; reports both raw sides.
(define (resolve-status-precedence plan-row-status doc-header-status)
  (if (eq? (status->symbol plan-row-status) 'deferred)
      STATUS-DEFERRED
      (string-trim doc-header-status)))

;; effective-wave-status : path-string? wave-index-entry? -> string?
;; Entry status resolved through the documented BUG-0034 precedence:
;; the wave-doc header when it exists, otherwise the PLAN.md row.
(define (effective-wave-status base-dir e)
  (define idx (wave-index-entry-idx e))
  (define doc
    (and (wave-exists? base-dir idx (wave-index-entry-slug e))
         (read-wave-doc base-dir idx (wave-index-entry-slug e))))
  (if doc
      (resolve-status-precedence (wave-index-entry-status e) (hash-ref doc 'status))
      (wave-index-entry-status e)))

;; ============================================================
;; Plan-format deprecation warnings (BUG-0035, W6)
;; ============================================================

;; Post-BUG-0023 both plan grammars remain first-class: the PLAN.md
;; index (strict `- [Inbox] W0: Title → waves/W0-slug.md` rows, plus a
;; relaxed status-less `- W0: Title` form) and the legacy INLINE
;; `## Wave N:` sections. The index+status grammar is the one slated to
;; survive; inline sections are on the removal path (docs/gsd-guide.md
;; roadmap: removal targeted after v1.00.21). These warnings make the
;; deprecated authoring forms LOUD without changing any behavior —
;; loading and execution proceed exactly as today (non-fatal by
;; design; BUG-0035).

;; plan-format-deprecation-warnings : string? -> (listof string?)
;; PURE. Given PLAN.md text, return one advisory warning per deprecated
;; authoring form found:
;;   * INLINE path — zero parsed index entries but at least one
;;     `## Wave N:` section: exactly ONE warning naming the index
;;     skeleton to migrate to.
;;   * RELAXED rows — each status-less index row (`- W0: Title`, the
;;     relaxed-index-line-rx form parse-plan-index itself accepts)
;;     gets its own warning recommending the explicit `[Inbox]`
;;     bracket. Strict rows start with '[' and cannot match that rx.
;; The full index format (`- [Inbox] W0: Title → waves/W0-slug.md`)
;; produces ZERO warnings. Never raises; callers print at will.
(define (plan-format-deprecation-warnings md-text)
  (define inline-warning
    (if (and (null? (parse-plan-index md-text)) (> (count-inline-wave-sections md-text) 0))
        (list
         (string-append
          "WARNING (deprecated plan format, BUG-0035): PLAN.md uses inline `## Wave N:` sections. "
          "Inline sections are deprecated — removal targeted after v1.00.21. "
          "Migrate to the PLAN.md index grammar, one row per wave: "
          "- [Inbox] W0: Title → waves/W0-slug.md"))
        '()))
  (define relaxed-warnings
    (for/list ([line (in-list (string-split md-text "\n"))]
               #:when (regexp-match? relaxed-index-line-rx line))
      (string-append "WARNING (deprecated index row, BUG-0035): `"
                     (string-trim line)
                     "` lacks a [Status] bracket. "
                     "Add the explicit bracket, e.g. - [Inbox] W0: Title → waves/W0-slug.md")))
  (append inline-warning relaxed-warnings))

;; plan-format-deprecation-warning-lines : path-string? -> (listof string?)
;; File-backed variant for command surfaces (/go, /gsd status): reads
;; <base-dir>/.planning/PLAN.md and returns the deprecation warnings
;; for its text. No plan file → no warnings (nothing loaded, nothing
;; deprecated). Non-fatal: this never blocks a campaign.
(define (plan-format-deprecation-warning-lines base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (not (file-exists? plan-path))
      '()
      (plan-format-deprecation-warnings (call-with-input-file plan-path port->string))))

;; ============================================================
;; Wave-doc lint at /go entry (BUG-0041, W4)
;; ============================================================

;; /go validates the PLAN.md index but never the wave docs' CONTENT. The
;; executor contract relies on conventions — `## Files` with `- File:`
;; lines (the stall-steering target extractor parses exactly that shape),
;; `## Verify`, `## Done`, and the canonical managed status header — yet
;; a doc missing any of these loaded cleanly and the gap surfaced
;; mid-wave as degraded steering, guessed verify commands, and
;; unfalsifiable done criteria. The lint below makes that degradation
;; explicit AT /go ENTRY, as named warnings (never blocks) plus a
;; durable per-campaign verdict recorded at campaign-record creation.

;; Mirrors the stall-steering target extractor's file-line shape in
;; go-orchestrator.rkt (wave-file-line-rx): `- File: <path>` bullets.
(define wave-doc-file-line-rx #px"^[-*] *File:[ \t]+([^ \t\n]+)")
(define wave-doc-section-heading-rx #px"^## ")

;; One missing-or-invalid executor-contract section of one wave doc.
;;   wave-idx : wave index from the PLAN.md row
;;   doc-path : display path of the wave doc (relative to project root)
;;   section  : one of 'status-header 'duplicate-status 'files 'verify 'done
(struct wave-doc-violation (wave-idx doc-path section) #:transparent)

;; wave-doc-section-body : string? string? -> (listof string?)
;; PURE. Body lines of the `## <name>` section (heading line matched by
;; `^## <name>\b`, so `## Files` matches but `## Files-legacy` does not),
;; up to the next `## ` heading. '() when the section is absent.
(define (wave-doc-section-body content name)
  ;; NOTE: Racket `\b` is a backspace escape, not a word boundary —
  ;; anchor canonical headings at end-of-line instead.
  (define heading-rx (regexp (string-append "^## +" (regexp-quote name) " *$")))
  (let loop ([lines (string-split content "\n")]
             [mode 'seeking])
    (cond
      [(null? lines) '()]
      [(eq? mode 'seeking)
       (loop (cdr lines) (if (regexp-match? heading-rx (car lines)) 'body 'seeking))]
      [else
       (if (regexp-match? wave-doc-section-heading-rx (car lines))
           '()
           (cons (car lines) (loop (cdr lines) 'body)))])))

;; wave-doc-section-nonempty? : string? string? -> boolean?
;; Section exists and has at least one non-blank body line.
(define (wave-doc-section-nonempty? content name)
  (ormap (lambda (l) (not (string=? (string-trim l) ""))) (wave-doc-section-body content name)))

;; recognized-status? : (or/c string? symbol?) -> boolean?
;; #t iff the status is in the STATUS-CONSIDERED-EQUAL vocabulary (i.e.
;; status->symbol found a real group, not the 'pending fallback).
(define (recognized-status? s)
  (define up
    (string-upcase (string-trim (if (symbol? s)
                                    (symbol->string s)
                                    s))))
  (for/or ([group (in-list STATUS-CONSIDERED-EQUAL)])
    (and (member up (cdr group)) #t)))

;; lint-wave-doc : hash? -> (listof wave-doc-violation?)
;; PURE. Lints ONE parsed wave doc (the hash read-wave-doc /
;; parse-wave-doc-from-string return) against the executor contract.
;; Violations, in a deterministic order:
;;   status-header — managed header (`# Wave N\nStatus: ...`) absent or
;;                   its value not a canonical status word
;;   duplicate-status — BUG-0050: the doc BODY carries a second
;;                   line-anchored `Status:` line. The machine header is
;;                   the single authoritative marker; any body `Status:`
;;                   line is stale template residue and must be removed.
;;   files         — `## Files` section missing or with no `- File:`
;;                   line. Required for EVERY wave doc — no category
;;                   exemption: even pure-analysis waves create
;;                   test/report files, and the stall-steering target
;;                   extractor parses exactly this shape.
;;   verify        — `## Verify` missing or empty (no non-blank line)
;;   done          — `## Done` missing or empty
;; Hashes without the 'status-header? key (pre-BUG-0041 producers) are
;; treated as header-present: the lint never faults old records it
;; cannot inspect.
(define (lint-wave-doc doc)
  (define idx (hash-ref doc 'index 0))
  (define path-display
    (or (hash-ref doc 'path #f)
        (format ".planning/waves/W~a-~a.md" idx (hash-ref doc 'slug "unknown"))))
  (define content (hash-ref doc 'content ""))
  (define status (hash-ref doc 'status "Inbox"))
  (append (if (and (hash-ref doc 'status-header? #t) (recognized-status? status))
               '()
               (list (wave-doc-violation idx path-display 'status-header)))
          ;; BUG-0050: exactly ONE sanctioned `Status:` line (the machine
          ;; header). A body `Status:` line is flagged whatever its value.
          (if (body-status-line content)
              (list (wave-doc-violation idx path-display 'duplicate-status))
              '())
          (if (ormap (lambda (l) (regexp-match? wave-doc-file-line-rx l))
                     (wave-doc-section-body content "Files"))
              '()
              (list (wave-doc-violation idx path-display 'files)))
          (if (wave-doc-section-nonempty? content "Verify")
              '()
              (list (wave-doc-violation idx path-display 'verify)))
          (if (wave-doc-section-nonempty? content "Done")
              '()
              (list (wave-doc-violation idx path-display 'done)))))

;; lint-campaign-wave-docs : path-string? -> (listof wave-doc-violation?)
;; File-backed lint of every wave doc the PLAN.md index references. Docs
;; missing on disk are BUG-0023 strict-validation territory (/go errors
;; before lint), never lint violations.
(define (lint-campaign-wave-docs base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (not (file-exists? plan-path))
      '()
      (append* (for/list ([e (in-list (parse-plan-index (call-with-input-file plan-path
                                                                              port->string)))])
                 (define idx (wave-index-entry-idx e))
                 (define doc
                   (and (wave-exists? base-dir idx (wave-index-entry-slug e))
                        (read-wave-doc base-dir idx (wave-index-entry-slug e))))
                 (if doc
                     (lint-wave-doc doc)
                     '())))))

;; Human label for each violation section, named verbatim in warnings.
(define WAVE-DOC-LINT-SECTION-LABELS
  (list (list 'status-header "`# Wave N` + canonical `Status:` header")
        (list 'duplicate-status "exactly one `Status:` line (BUG-0050: a second `Status:` line in the doc body — remove it; the machine header is the single marker)")
        (list 'files "`## Files` with at least one `- File:` line")
        (list 'verify "non-empty `## Verify`")
        (list 'done "non-empty `## Done`")))

;; format-wave-doc-lint-warning :
;;   exact-nonnegative-integer? string? (listof wave-doc-violation?) -> string?
;; ONE named warning for ONE doc listing every missing-or-invalid section
;; (BUG-0041 action: one named warning per doc at /go entry).
(define (format-wave-doc-lint-warning wave-idx doc-path violations)
  (define section-names
    (string-join (for/list ([v (in-list violations)])
                   (cond
                     [(assoc (wave-doc-violation-section v) WAVE-DOC-LINT-SECTION-LABELS)
                      =>
                      cadr]
                     [else (format "~a" (wave-doc-violation-section v))]))
                 "; "))
  (format (string-append "WARNING (wave-doc lint, BUG-0041): W~a (~a) — missing or invalid: ~a. "
                         "The executor contract depends on these sections; fix the doc so "
                         "steering, verification and done criteria are falsifiable.")
          wave-idx
          doc-path
          section-names))

;; wave-doc-lint-warning-lines : path-string? -> (listof string?)
;; One NAMED warning PER DOC for the whole campaign; advisory only —
;; lint NEVER blocks /go (missing sections degrade, they do not reject).
(define (wave-doc-lint-warning-lines base-dir)
  (for/list ([group (in-list (group-by (lambda (v)
                                         (list (wave-doc-violation-wave-idx v)
                                               (wave-doc-violation-doc-path v)))
                                       (lint-campaign-wave-docs base-dir)))])
    (format-wave-doc-lint-warning (wave-doc-violation-wave-idx (car group))
                                  (wave-doc-violation-doc-path (car group))
                                  group)))

;; store-wave-doc-lint-verdict! : path-string? string? -> boolean?
;; Durable campaign evidence (BUG-0041): the lint verdict recorded at
;; campaign-record creation lands in the campaign's evidence directory
;; as .planning/campaigns/<plan-id>/lint-verdict.rktd — the verdict of
;; EVERY referenced wave doc (clean docs included: a recorded PASS is
;; evidence too), write-once so later /go runs never rewrite the
;; verdict the campaign started under. Best-effort: any failure logs at
;; debug level and returns #f; storing evidence must never block /go.
(define (store-wave-doc-lint-verdict! base-dir plan-id)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-debug "gsd: store-wave-doc-lint-verdict! failed for ~a: ~a"
                                          plan-id
                                          (exn-message e))
                               #f)])
    (define dest (build-path base-dir ".planning" "campaigns" plan-id "lint-verdict.rktd"))
    (cond
      [(file-exists? dest) #f]
      [else
       (define plan-path (build-path base-dir ".planning" "PLAN.md"))
       (define entries
         (if (file-exists? plan-path)
             (parse-plan-index (call-with-input-file plan-path port->string))
             '()))
       (define verdict
         (list 'wave-doc-lint-verdict
               (list 'recorded-at (current-seconds))
               (for/list ([e (in-list entries)])
                 (define idx (wave-index-entry-idx e))
                 (define doc
                   (and (wave-exists? base-dir idx (wave-index-entry-slug e))
                        (read-wave-doc base-dir idx (wave-index-entry-slug e))))
                 (list 'wave
                       idx
                       'doc-path
                       (index-entry-doc-display-path e)
                       'violations
                       (if doc
                           (map wave-doc-violation-section (lint-wave-doc doc))
                           '(missing-doc))))))
       (make-directory* (path-only dest))
       (call-with-output-file dest
                              (lambda (out)
                                (write verdict out)
                                (newline out))
                              #:exists 'error)
       #t])))

;; One index-arrow ↔ doc-slug disagreement for one wave (BUG-0041).
;; The wave doc's identity is stated TWICE: the PLAN.md arrow target
;; (`waves/W<n>-<slug>.md`) and the doc's on-disk filename. A W<n>-*.md
;; file whose slug differs from the arrow slug means the index points at
;; a stale path — reported through the BUG-0034 consistency surface so
;; there is a SINGLE divergence surface, not two.
;;   arrow-slug : slug derived from the PLAN.md row (arrow or title)
;;   disk-slug  : slug of the W<n>-*.md file actually on disk
;;   doc-path   : display path of that on-disk file
(struct slug-mismatch (wave-idx arrow-slug disk-slug doc-path) #:transparent)

;; wave-doc-filename-slug : exact-nonnegative-integer? path? -> (or/c string? #f)
;; Slug encoded in a `W<n>-<slug>.md` filename; #f when the shape differs.
(define (wave-doc-filename-slug idx filename)
  (define s (path->string filename))
  (define prefix (format "W~a-" idx))
  (and (string-prefix? s prefix)
       (string-suffix? s ".md")
       (> (string-length s) (+ (string-length prefix) 3))
       (substring s (string-length prefix) (- (string-length s) 3))))

;; check-slug-consistency : path-string? -> (listof slug-mismatch?)
;; PURE read path: for every PLAN.md index row, flags any on-disk
;; W<n>-*.md wave doc whose filename slug differs from the row's slug.
;; (The arrow-target file itself missing is BUG-0023 strict-validation
;; territory.) Never writes; callers turn mismatches into warnings.
(define (check-slug-consistency base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (not (file-exists? plan-path))
      '()
      (append*
       (for/list ([e (in-list (parse-plan-index (call-with-input-file plan-path port->string)))])
         (define idx (wave-index-entry-idx e))
         (define idx-prefix-rx (regexp (format "^W~a-" idx)))
         (define waves-dir (build-path base-dir ".planning" "waves"))
         (for/list ([f (in-list (if (directory-exists? waves-dir)
                                    (sort (directory-list waves-dir) path<?)
                                    '()))]
                    #:when (regexp-match? idx-prefix-rx (path->string f))
                    #:do [(define disk-slug (wave-doc-filename-slug idx f))]
                    #:when (and disk-slug (not (string=? disk-slug (wave-index-entry-slug e)))))
           (slug-mismatch idx
                          (wave-index-entry-slug e)
                          disk-slug
                          (format ".planning/waves/~a" (path->string f))))))))

;; format-slug-mismatch-warning : slug-mismatch? -> string?
;; One named, user-visible warning per mismatch (BUG-0041): names BOTH
;; spellings of the wave's identity so a human can align them. Advisory
;; only — never blocks /go by itself.
(define (format-slug-mismatch-warning m)
  (format (string-append "WARNING (slug mismatch, BUG-0041): W~a — PLAN.md targets "
                         "waves/W~a-~a.md but a wave doc exists on disk as ~a. "
                         "Align the arrow target with the doc filename (reported via the "
                         "status-consistency surface; single divergence surface).")
          (slug-mismatch-wave-idx m)
          (slug-mismatch-wave-idx m)
          (slug-mismatch-arrow-slug m)
          (slug-mismatch-doc-path m)))

;; slug-mismatch-warning-lines : path-string? -> (listof string?)
;; File-backed variant for the /go and /gsd advisory surfaces.
(define (slug-mismatch-warning-lines base-dir)
  (for/list ([m (in-list (check-slug-consistency base-dir))])
    (format-slug-mismatch-warning m)))

;; ============================================================
;; PLAN.md index status update
;; ============================================================

(define (update-plan-index-text text wave-idx new-status)
  (define marker (status->marker new-status))
  (update-index-line text wave-idx marker))

(define (update-wave-in-index! base-dir wave-idx new-status)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define new-text (update-plan-index-text text wave-idx new-status))
     (call-with-output-file plan-path (lambda (out) (display new-text out)) #:exists 'truncate)
     #t]))

(define (status->marker status)
  (cond
    [(string? status)
     (define entry (assoc status WAVE-STATUS-MARKERS))
     (if entry
         (cdr entry)
         (format "[~a]" status))]
    [(symbol? status) (status->marker (symbol->string status))]
    [else "[Inbox]"]))

(define (update-index-line text wave-idx new-marker)
  (define lines (string-split text "\n"))
  (define update-rx
    (regexp (string-append "^([-*] +)\\[([A-Za-z-]+)\\]( +W" (number->string wave-idx) ":.*)$")))
  (define new-lines
    (for/list ([line lines])
      (define m (regexp-match update-rx line))
      (if m
          (string-append (cadr m) new-marker (list-ref m 3))
          line)))
  (string-join new-lines "\n"))

;; ============================================================
;; Wave queries
;; ============================================================

(define (find-next-inbox-entry entries)
  (for/first ([e entries]
              #:when (or (string=? (wave-index-entry-status e) STATUS-INBOX)
                         (string=? (wave-index-entry-status e) STATUS-FAILED)))
    e))

(define (next-inbox-wave base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     ;; BUG-0034 (W2): selection honors the documented precedence — the
     ;; EFFECTIVE status (doc header wins for progress statuses, row wins
     ;; only for [DEFERRED]), not the raw PLAN.md row — so an externally
     ;; doctored row can no longer silently steer next-wave selection.
     (for/first ([e (in-list (parse-plan-index text))]
                 #:when (memq (status->symbol (effective-wave-status base-dir e)) '(pending failed)))
       e)]))

;; ============================================================
;; Dual-write status transition
;; ============================================================

(define (mark-wave-status! base-dir wave-idx new-status)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (define entry
       (for/first ([e entries]
                   #:when (= (wave-index-entry-idx e) wave-idx))
         e))
     (cond
       [(not entry) #f]
       [else
        (define slug (wave-index-entry-slug entry))
        (define wave-path (wave-doc-path base-dir wave-idx slug))
        (when (file-exists? wave-path)
          (define wave-text (call-with-input-file wave-path port->string))
          (define content (strip-status-header wave-text))
          (write-wave-doc! base-dir wave-idx slug content new-status))
        (update-wave-in-index! base-dir wave-idx new-status)
        ;; BUG-0034 (W2): audibility — the sanctioned dual-write leaves a
        ;; debug-level trace so post-hoc divergence forensics can tell a
        ;; sanctioned status transition from an external edit.
        (log-debug "gsd: mark-wave-status! dual-write W~a → ~a (wave doc + PLAN.md index row)"
                   wave-idx
                   new-status)
        #t])]))

;; ============================================================
;; Plan overall status
;; ============================================================

(define (compute-plan-overall-status entries)
  (cond
    [(null? entries) 'not-started]
    [else
     (define statuses (map wave-index-entry-status entries))
     (define all-done?
       (for/and ([s statuses])
         (done-or-deferred? s)))
     (define any-progress?
       (for/or ([s statuses])
         (not (string=? s STATUS-INBOX))))
     (cond
       [all-done? 'all-done]
       [any-progress? 'partly-done]
       [else 'in-progress])]))

(define (plan-overall-status base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) 'not-started]
    [else
     (define text (call-with-input-file plan-path port->string))
     (compute-plan-overall-status (parse-plan-index text))]))

;; ============================================================
;; Slug resolution helpers (v0.99.89 W2 projection shell)
;; ============================================================

;; Slug for one wave index, read from the PLAN.md index.
(define (wave-slug base-dir wave-idx)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entry
       (for/first ([e (parse-plan-index text)]
                   #:when (= (wave-index-entry-idx e) wave-idx))
         e))
     (and entry (wave-index-entry-slug entry))]))

;; Whole-index slug map (idx → slug) from the PLAN.md index.
(define (plan-slug-map base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (file-exists? plan-path)
      (for/hash ([e (parse-plan-index (call-with-input-file plan-path port->string))])
        (values (wave-index-entry-idx e) (wave-index-entry-slug e)))
      (hash)))
