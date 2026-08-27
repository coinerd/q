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

(require racket/contract
         racket/format
         racket/string
         racket/file
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
          [plan-format-deprecation-warning-lines (-> path-string? (listof string?))]))

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

(define (write-wave-doc! base-dir idx slug content status)
  (define path (wave-doc-path base-dir idx slug))
  (define dir (path-only path))
  (unless (directory-exists? dir)
    (make-directory* dir))
  (define header (format "# Wave ~a\nStatus: ~a\n\n" idx status))
  (call-with-output-file path
                         (lambda (out)
                           (display header out)
                           (display content out))
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

;; One divergence between the two sources for one wave.
;;   plan-status : PLAN.md index row status (raw, e.g. "DONE")
;;   doc-status  : wave-doc `Status:` header (raw, e.g. "PENDING")
;;   plan-path   : display path of the PLAN.md index (.planning/PLAN.md)
;;   doc-path    : display path of the wave doc (.planning/waves/W<n>-<slug>.md)
(struct status-divergence (wave-idx plan-status doc-status plan-path doc-path) #:transparent)

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
;; wave doc's `Status:` header. A divergence is reported ONLY when both
;; sources exist and their canonical statuses differ. Missing wave docs
;; are BUG-0023 strict-validation territory, not a consistency concern.
;; Never writes; callers turn divergences into named warnings.
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
        (if (and doc
                 (not (eq? (status->symbol (wave-index-entry-status e))
                           (status->symbol (hash-ref doc 'status)))))
            (append divs
                    (list (status-divergence idx
                                             (wave-index-entry-status e)
                                             (hash-ref doc 'status)
                                             ".planning/PLAN.md"
                                             (index-entry-doc-display-path e))))
            divs))))

;; format-status-divergence-warning : status-divergence? -> string?
;; One named, user-visible warning per divergent wave (BUG-0034): names
;; BOTH file paths and both claimed statuses so a human can resolve the
;; disagreement. Advisory only — it never blocks /go by itself.
(define (format-status-divergence-warning d)
  (format (string-append "WARNING (status divergence, BUG-0034): W~a — PLAN.md row says [~a] "
                         "but wave doc header says '~a' (~a vs ~a). "
                         "mark-wave-status! is the only sanctioned writer; align one source.")
          (status-divergence-wave-idx d)
          (status-divergence-plan-status d)
          (status-divergence-doc-status d)
          (status-divergence-plan-path d)
          (status-divergence-doc-path d)))

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
