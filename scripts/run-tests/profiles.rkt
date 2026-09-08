#lang racket/base

;; q/scripts/run-tests/profiles.rkt — Environment profile skip policy
;;
;; Profiles make environment-dependent skips explicit and reportable. Skips are
;; represented as SKIPPED_BY_PROFILE results and must never be counted as PASS.

(require racket/list
         racket/path
         racket/string
         json
         (only-in "parse.rkt"
                  make-test-file-result
                  test-file-result-path
                  test-file-result-grouped-fallback-reason)
         (only-in "classify.rkt" base-dir normalize-test-path))

(provide known-profiles
         profile-unavailable-requirements
         profile-skips-test?
         skipped-requirements
         make-skipped-result
         skipped-result-exit-code
         ;; v1.00.27 W3 (#9591): per-area grouped-expansion policy
         test-file-area
         grouped-area-config
         grouped-area-config->jsexpr
         grouped-expansion-artifact-path
         grouped-config-violation
         grouped-rollback-env-var
         grouped-rollback-areas
         area-grouped-decision
         grouped-fallback-rows)

(define known-profiles '(local vps ci headless full))

(define skipped-result-exit-code 5)

(define (normalize-requirement req)
  (cond
    [(symbol? req) (symbol->string req)]
    [(string? req) (string-downcase (string-trim req))]
    [else (format "~a" req)]))

(define (profile-unavailable-requirements profile)
  (case profile
    ;; Developer workstation: terminal/subprocess/filesystem/git are assumed;
    ;; live provider credentials are intentionally not assumed.
    [(local) '("provider-key")]
    ;; VPS is headless: browser/terminal are unavailable; provider credentials
    ;; may exist and are therefore not skipped by policy.
    [(vps) '("browser" "terminal")]
    ;; CI must be deterministic and non-interactive by default.
    [(ci) '("provider-key" "browser" "terminal" "network")]
    ;; Headless means no interactive/UI/provider dependencies.
    [(headless) '("provider-key" "browser" "terminal")]
    ;; Full is opt-in to run everything possible.
    [(full) '()]
    [else '()]))

(define (skipped-requirements profile requirements)
  (define unavailable (profile-unavailable-requirements profile))
  (filter (lambda (req)
            (define normalized (normalize-requirement req))
            (and (not (string=? normalized "none")) (member normalized unavailable)))
          requirements))

(define (profile-skips-test? profile requirements)
  (pair? (skipped-requirements profile requirements)))

(define (make-skipped-result path profile requirements)
  (define skipped (skipped-requirements profile requirements))
  (define reason
    (format "SKIPPED_BY_PROFILE profile=~a requires=~a" profile (string-join skipped ",")))
  (make-test-file-result path
                         skipped-result-exit-code
                         (string->bytes/utf-8 (string-append reason "\n"))
                         #""
                         0
                         0
                         0
                         0))

;; ── v1.00.27 W3 (#9591): per-area grouped-expansion policy ──────────────
;;
;; The grouped execution mode is rolled out per tests/ area, backed by an
;; exact subprocess-vs-grouped comparison artifact checksummed in
;; artifacts/tier-ownership/v1.00.27-w3/SHA256SUMS. An area executes
;; grouped only with a matching comparison record; every area-level
;; fallback is named (grouped-fallback-rows) and surfaced by the runner
;; summary and JSON report — never silent.

(define grouped-expansion-artifact-path
  (build-path base-dir "artifacts" "tier-ownership" "v1.00.27-w3" "grouped-expansion.json"))

(define grouped-rollback-env-var "Q_GROUPED_ROLLBACK_AREAS")

(define (test-file-area f)
  ;; Map a test file (path or string, absolute or repo-relative) to its
  ;; area: the first directory component under tests/ (e.g. "ci",
  ;; "classify"), "(root)" for files directly in tests/, and "(other)"
  ;; for anything outside tests/.
  (define norm
    (normalize-test-path (if (path? f)
                             (path->string f)
                             f)))
  (define parts (string-split norm "/"))
  (cond
    [(and (>= (length parts) 3) (string=? (first parts) "tests")) (second parts)]
    [(and (>= (length parts) 2) (string=? (first parts) "tests")) "(root)"]
    [else "(other)"]))

(define (grouped-rollback-areas [v #f])
  ;; Parse the per-area rollback switch: comma-separated area names,
  ;; whitespace-tolerant; unset/empty means no rollback.
  (define raw
    (or v
        (let ([env (getenv grouped-rollback-env-var)])
          (and env (non-empty-string? (string-trim env)) env))))
  (if raw
      (map string-trim (string-split (string-trim raw) ","))
      '()))

(define grouped-area-config-cache #f)

(define (load-grouped-area-config)
  (if (file-exists? grouped-expansion-artifact-path)
      (let ([artifact (call-with-input-file grouped-expansion-artifact-path read-json)])
        (for/hash ([row (in-list (hash-ref artifact 'areas '()))]
                   #:when (equal? (hash-ref row 'decision #f) "grouped"))
          (values (hash-ref row 'area) 'grouped)))
      (hash)))

(define (grouped-area-config)
  ;; area (string) → policy symbol 'grouped (only areas backed by an exact
  ;; comparison record appear); everything else defaults to subprocess.
  (or grouped-area-config-cache
      (begin
        (set! grouped-area-config-cache (load-grouped-area-config))
        grouped-area-config-cache)))

(define (grouped-area-config->jsexpr [config (grouped-area-config)])
  ;; write-json rejects string hash keys (jsexpr objects carry symbol keys),
  ;; so the runner JSON summary reports the area policy through this
  ;; projection: area names become symbols and are rendered as JSON strings
  ;; on the wire. The internal config stays string-keyed (test-file-area
  ;; convention); only the JSON boundary converts.
  (for/hash ([(area policy) (in-hash config)])
    (values (string->symbol (if (symbol? area)
                                (symbol->string area)
                                area))
            (if (symbol? policy)
                (symbol->string policy)
                policy))))

(define (area-grouped-decision path [rollback-areas (grouped-rollback-areas)])
  ;; → (values mode fallback-reason): mode is 'grouped or 'subprocess;
  ;; fallback-reason is #f for grouped and a named reason symbol otherwise.
  (define area (test-file-area path))
  (define policy (hash-ref (grouped-area-config) area 'subprocess))
  (cond
    [(and (eq? policy 'grouped) (member area rollback-areas))
     (values 'subprocess 'area-grouped-rollback)]
    [(eq? policy 'grouped) (values 'grouped #f)]
    [else (values 'subprocess 'area-not-expanded)]))

(define (grouped-config-violation [config (grouped-area-config)]
                                  [artifact
                                   (and (file-exists? grouped-expansion-artifact-path)
                                        (call-with-input-file grouped-expansion-artifact-path
                                                              read-json))])
  ;; → #f when the delivered configuration is backed by the comparison
  ;; artifact, or a string naming the violation. An area may only be
  ;; 'grouped with an exact-match comparison record, and the config must
  ;; mirror every grouped decision in the artifact.
  (cond
    [(not (hash? config)) "grouped-area-config must be a hash of area → policy"]
    [(not (jsexpr? artifact)) "W3 comparison artifact missing or unreadable"]
    [else
     (define rows (hash-ref artifact 'areas #f))
     (cond
       [(not (list? rows)) "comparison artifact carries no areas rows"]
       [else
        (define exact
          ;; Areas are keyed by string everywhere: test-file-area returns
          ;; strings (e.g. "ci") and grouped-area-config copies them
          ;; verbatim from the artifact, so exact matches that convention.
          (for/hash ([row (in-list rows)])
            (values (hash-ref row 'area)
                    (and (hash-ref row 'match #f) (equal? (hash-ref row 'decision #f) "grouped")))))
        (or (for/or ([(area policy) (in-hash config)]
                     #:when (eq? policy 'grouped))
              (and (not (hash-ref exact area #f))
                   (format "area ~a marked grouped without exact comparison evidence" area)))
            (for/or ([row (in-list rows)]
                     #:when (equal? (hash-ref row 'decision #f) "grouped"))
              (define area (hash-ref row 'area))
              (and (not (eq? (hash-ref config area 'subprocess) 'grouped))
                   (format "artifact grouped decision for ~a is not mirrored in grouped-area-config"
                           area))))])]))

(define (grouped-fallback-rows results)
  ;; Aggregate area-level grouped fallbacks across results into sorted rows:
  ;; each row names the area, the fallback kind, the reason token, and the
  ;; affected file count.
  (define per-area (make-hash))
  (for ([r (in-list results)])
    (define reason (test-file-result-grouped-fallback-reason r))
    (define kind
      (cond
        [(eq? reason 'area-grouped-rollback) "grouped-rollback"]
        [(eq? reason 'area-not-expanded) "grouped-not-expanded"]
        [else #f]))
    (when kind
      (define area (test-file-area (test-file-result-path r)))
      (define cur (hash-ref per-area area (cons kind 0)))
      (hash-set! per-area area (cons (car cur) (add1 (cdr cur))))))
  (for/list ([area (in-list (sort (hash-keys per-area) string<?))])
    (define pair (hash-ref per-area area))
    (hasheq 'area
            area
            'kind
            (car pair)
            'reason
            (if (string=? (car pair) "grouped-rollback")
                grouped-rollback-env-var
                "no-exact-comparison-evidence")
            'files
            (cdr pair))))
