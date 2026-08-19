#lang racket/base

;; q/scripts/run-tests/mutation-pilot.rkt — bounded mutation-analysis pilot (W9)
;;
;; TOOL DECISION (recorded per the W9 wave contract): before writing this
;; script, candidate external/OSS Racket-compatible mutation tools were
;; evaluated against three criteria — (a) maintained and compatible with the
;; supported Racket version, (b) deterministic, budget-bounded execution with
;; retained per-mutant artifacts, (c) no external service / database. No
;; candidate satisfied (a) AND (b); the Racket ecosystem's mutation tooling is
;; experimental/unmaintained. Fallback per the plan constraint ("repository-
;; owned Racket scripts; reviewed manual micro-mutations otherwise"): this
;; module implements a REVIEWED SET OF MANUAL MICRO-MUTATIONS (comparison
;; swaps, arithmetic operator swaps, boolean connective swaps, numeric
;; boundary off-by-one) as repo-owned, free/open-source tooling.
;; RE-EVALUATION TRIGGER: revisit if a maintained Racket mutation tool with
;; CI-compatible budget hooks appears on the official package index.
;;
;; SCOPE + SAFETY INVARIANTS (all mandatory):
;;   - Scope is ALWAYS explicit: `--modules a.rkt,b.rkt` or `--from-diff BASE`.
;;     There is NO repo-wide default path.
;;   - Hard wall-clock budget (`--budget`, default 300 s). When exhausted the
;;     pilot stops scheduling new mutants, restores every mutated file, and
;;     writes partial artifacts with aborted_early=true — a clean abort,
;;     never a crash, never a masked timeout.
;;   - Mutant cap (`--max-mutants`, default 25) across the whole run.
;;   - Original file bytes are snapshotted into <out>/.originals/ before the
;;     first mutation; every mutant is restored immediately after its tests,
;;     and the wind-exit restores any leftovers. A crash-recovery pass at
;;     startup restores leftovers from a previous killed run.
;;   - A mutated file that does not parse is skipped (site status `invalid`),
;;     never executed.
;;   - Every mutant's outcome (killed | survived | timeout | not-run |
;;     invalid | not-covered), its module, operator, line/col, and the killing
;;     test are retained as JSON artifacts.
;;   - Timeouts are never reported as passes: a test that hits the per-test
;;     timeout is recorded `timeout` and counted separately from kills.
;;   - Findings are INFORMATIONAL. Exit code 0 for completed AND
;;     budget-aborted runs; only usage errors exit non-zero (exit 2).
;;
;; DETERMINISM: site enumeration order (operator table order, then ascending
;; source offset), module order (sorted), and killing-test order (as supplied
;; / manifest order) are fixed — identical inputs yield identical plans. Only
;; measured `duration_seconds` values vary between runs.
;; STABILITY: internal — executed via CLI (`racket scripts/run-tests/
;; mutation-pilot.rkt …`), contract-tested by tests/test-mutation-pilot.rkt.

(require racket/cmdline
         racket/dict
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         json)

(define pilot-version "1.00.03")

;; Planning (pure)
(provide mutation-operators
         enumerate-sites
         site?
         site-module
         site-offset
         site-line
         site-col
         site-operator
         site-description
         parse-plan-mutants
         apply-single-mutation
         module-text-parses?
         offset->line/col
         ;; Killing-test resolution (pure)
         resolve-killing-tests
         manifest->killing-map
         ;; Execution
         run-one-test
         run-killing-tests
         ;; Orchestration
         run-pilot
         pilot->jsexpr
         write-pilot-json!
         write-findings!
         ensure-clean-state!
         restore-all!
         budget-exhausted?
         ;; CLI
         main)

;; ---------------------------------------------------------------------------
;; Mutation operators (reviewed manual micro-mutation set)
;; ---------------------------------------------------------------------------

;; Each operator: name, pattern, replacement, human description. Operator
;; swaps are `(`-anchored and require trailing whitespace so `(or ` never
;; matches `(org `. Boundary literals are guarded against identifiers and
;; floats (no `x1`, `-1`, `1.0` sites).
(define (mutation-operators)
  (list (list 'comparison-lt->le #px"\\(<[ ]" "(<= " "weak comparison (< becomes <=)")
        (list 'comparison-le->lt #px"\\(<=[ ]" "(< " "strict comparison (<= becomes <)")
        (list 'comparison-gt->ge #px"\\(>[ ]" "(>= " "weak comparison (> becomes >=)")
        (list 'comparison-ge->gt #px"\\(>=[ ]" "(> " "strict comparison (>= becomes >)")
        (list 'arithmetic-add->sub #px"\\(\\+[ ]" "(- " "arithmetic (+ becomes -)")
        (list 'arithmetic-sub->add #px"\\(\\-[ ]" "(+ " "arithmetic (- becomes +)")
        (list 'boolean-and->or #px"\\(and[ ]" "(or " "boolean connective (and becomes or)")
        (list 'boolean-or->and #px"\\(or[ ]" "(and " "boolean connective (or becomes and)")
        (list 'numeric-boundary+1 #px"(?<![\\w#])0(?![\\w.])" "1" "boundary literal (0 becomes 1)")
        (list 'numeric-boundary-1 #px"(?<![\\w#])1(?![\\w.])" "0" "boundary literal (1 becomes 0)")))

;; ---------------------------------------------------------------------------
;; Sites
;; ---------------------------------------------------------------------------

(struct site
        (module offset line
          col
          operator
          description)
  #:transparent)

;; offset->line/col : string? exact-nonnegative-integer?
;;   -> (values exact-positive-integer? exact-nonnegative-integer?)
;; 1-based line, 0-based column (consistent with read-syntax columns).
(define (offset->line/col text offset)
  (define prefix (substring text 0 (min offset (string-length text))))
  (define line (add1 (length (regexp-match* #rx"\n" prefix))))
  (define m (regexp-match-positions #rx"\n[^\n]*$" prefix))
  (define col
    (if m
        (- (string-length prefix) (cdar m))
        (string-length prefix)))
  (values line col))

;; enumerate-sites : string? string? -> (listof site?)
;; Deterministic: operators in table order, then ascending source offset.
(define (enumerate-sites module text)
  (sort (append* (for/list ([op (in-list (mutation-operators))])
                   (match-define (list name pattern _replacement description) op)
                   (for/list ([m (in-list (regexp-match-positions* pattern text))])
                     (define-values (line col) (offset->line/col text (car m)))
                     (site module (car m) line col name description))))
        <
        #:key site-offset))

;; module-text-parses? : string? -> boolean?
;; Strip leading #lang / #reader / shebang LINES (same rationale as
;; run-tests/impact.rkt: language readers are nondeterministic code
;; execution), then read every datum — any read error fails validation.
(define (module-text-parses? text)
  (with-handlers ([exn:fail:read? (lambda (_) #f)]
                  [exn:fail? (lambda (_) #f)])
    (define body
      (let loop ([t text])
        (define m
          (regexp-match-positions #rx"^[ \t]*(?:#lang[^\n]*\n|#reader[^\n]*\n|#![^\n]*)\n?" t))
        (if m
            (loop (substring t (cdar m)))
            t)))
    (define port (open-input-string body))
    (let loop ()
      (define d (read port))
      (if (eof-object? d)
          #t
          (loop)))))

;; apply-single-mutation : string? site? -> string?
;; Re-applies the site's operator at its recorded offset; if the pattern no
;; longer matches exactly at that offset the text is returned unchanged.
(define (apply-single-mutation text s)
  (define op (findf (lambda (o) (eq? (car o) (site-operator s))) (mutation-operators)))
  (cond
    [(not op) text]
    [else
     (match-define (list _name pattern replacement _description) op)
     (define m (regexp-match-positions pattern text (site-offset s)))
     (if (and m (= (caar m) (site-offset s)))
         (string-append (substring text 0 (caar m)) replacement (substring text (cdar m)))
         text)]))

;; parse-plan-mutants : (listof (cons/c string? string?))
;;   [#:max-mutants exact-positive-integer?]
;;   -> (listof (list site? string?))
;; Plan for sorted (module . text) pairs: enumerate sites, keep sites whose
;; mutated text still parses, cap the EXECUTABLE plan at max-mutants
;; (deterministically — first sites in order; unparseable sites are recorded
;; as `invalid` entries carrying the ORIGINAL text and never executed).
(define (parse-plan-mutants module-texts #:max-mutants [max-mutants 25])
  (define all-sites
    (append* (for/list ([mt (in-list module-texts)])
               (define module (car mt))
               (define text (cdr mt))
               (for/list ([s (in-list (enumerate-sites module text))])
                 (cons s text)))))
  (define planned '())
  (define count 0)
  (for ([entry (in-list all-sites)])
    #:break (>= count max-mutants)
    (define s (car entry))
    (define text (cdr entry))
    (define mutated (apply-single-mutation text s))
    (cond
      [(not (module-text-parses? mutated))
       (set!
        planned
        (cons (list (struct-copy site
                                 s
                                 [operator
                                  (string->symbol (string-append (symbol->string (site-operator s))
                                                                 ":invalid"))])
                    text)
              planned))]
      [else
       (set! planned (cons (list s mutated) planned))
       (set! count (add1 count))]))
  (reverse planned))

;; ---------------------------------------------------------------------------
;; Killing-test resolution
;; ---------------------------------------------------------------------------

;; resolve-killing-tests : string? (dict/c string? (listof string?))
;;   (listof (cons/c string? (listof string?))) (listof string?)
;;   -> (listof string?)
;; Order: explicit --tests-for override > coverage manifest (@covers) >
;; global --tests fallback > none (caller records the gap).
(define (resolve-killing-tests module manifest tests-for-overrides global-tests)
  (cond
    [(dict-ref tests-for-overrides module #f)]
    [(pair? (dict-ref manifest module '())) (dict-ref manifest module)]
    [(pair? global-tests) global-tests]
    [else '()]))

;; manifest->killing-map : path-string? (listof string?) -> (hash/c string? (listof string?))
;; Invert tests/.coverage-manifest.json for the wanted modules: module ->
;; sorted list of tests whose @covers names the module.
(define (manifest->killing-map root modules)
  (define manifest-path (build-path root "tests" ".coverage-manifest.json"))
  (cond
    [(not (file-exists? manifest-path)) (hash)]
    [else
     (define manifest
       (with-handlers ([exn:fail? (lambda (_) (hasheq))])
         (string->jsexpr (file->string manifest-path))))
     (define wanted
       (for/hash ([m (in-list modules)])
         (values m #t)))
     (define acc (make-hash))
     (for* ([entry (in-list (hash-ref manifest 'entries '()))]
            [covered (in-list (hash-ref entry 'covers '()))]
            #:when (hash-ref wanted covered #f))
       (hash-update! acc
                     covered
                     (lambda (old) (sort (cons (hash-ref entry 'test) old) string<?))
                     '()))
     acc]))

;; ---------------------------------------------------------------------------
;; Test execution (subprocess `raco test`, per-test timeout, no masked exits)
;; ---------------------------------------------------------------------------

;; run-one-test : path-string? path-string? number? -> (cons symbol? number?)
;; → (cons status duration-seconds); status ∈ {pass, fail, timeout}.
;; `raco test` exit 0 => pass; non-zero => fail; over timeout-sec => kill +
;; timeout. The child runs with cwd = root (tests rely on repo-relative
;; requires). stdout/stderr are pipes drained by threads so the pipe can never
;; fill and deadlock the child; content is discarded (exit code is the verdict).
(define (run-one-test test-path root timeout-sec)
  (define start (current-inexact-milliseconds))
  (define-values (sp stdout _stdin stderr)
    (parameterize ([current-directory root])
      (subprocess #f #f #f (find-executable-path "raco") "test" "--quiet" test-path)))
  (define out-drain (thread (lambda () (copy-port stdout (open-output-nowhere)))))
  (define err-drain (thread (lambda () (copy-port stderr (open-output-nowhere)))))
  (define done (make-semaphore))
  (define waiter
    (thread (lambda ()
              (subprocess-wait sp)
              (semaphore-post done))))
  (define finished? (sync/timeout (max 0.001 timeout-sec) done))
  (kill-thread waiter)
  (define status
    (cond
      [finished? (if (eq? (subprocess-status sp) 0) 'pass 'fail)]
      [else
       (subprocess-kill sp #t)
       (subprocess-wait sp)
       'timeout]))
  ;; Child is gone: wait for EOF-driven drain threads, then close pipes.
  (sync/timeout 5.0 out-drain err-drain)
  (close-input-port stdout)
  (close-input-port stderr)
  (cons status (/ (- (current-inexact-milliseconds) start) 1000.0)))

;; run-killing-tests : (listof string?) path-string? number?
;;   -> (list symbol? (or/c string? #f) (listof (list/c string? symbol? number?)))
;; → (list status killing-test per-test outcomes). Tests run in order; the
;; FIRST failing (or timing-out) test is the killing test — a sufficient
;; classical criterion for detection.
(define (run-killing-tests tests root timeout-sec)
  (define outcomes '())
  (define killing #f)
  (define status 'survived)
  (for ([t (in-list tests)])
    #:break (eq? status 'killed)
    (define result (run-one-test t root timeout-sec))
    (set! outcomes (cons (list t (car result) (cdr result)) outcomes))
    (when (memq (car result) '(fail timeout))
      (set! killing t)
      (set! status 'killed)))
  (list status killing (reverse outcomes)))

;; ---------------------------------------------------------------------------
;; Orchestration
;; ---------------------------------------------------------------------------

;; Originals live under <out>/.originals/<module-path-encoded>; any file there
;; at startup means a previous run died mid-mutant — restore before anything
;; else (crash recovery), then re-snapshot pristine content.
(define originals-dir-name ".originals")

(define (originals-path out-dir)
  (build-path (string->path out-dir) originals-dir-name))

(define (module->original-path out-dir module)
  (build-path (originals-path out-dir) (string-replace module "/" "_")))

;; ensure-clean-state! : path-string? path-string? (listof (cons/c string? string?)) -> void?
(define (ensure-clean-state! out-dir root module-texts)
  (make-directory* (originals-path out-dir))
  (for ([mt (in-list module-texts)])
    (define module (car mt))
    (define target (build-path (string->path root) module))
    (define backup (module->original-path out-dir module))
    (when (file-exists? backup)
      (copy-file backup target #:exists-ok? #t)
      (delete-file backup))
    (with-output-to-file backup
                         (lambda () (write-bytes (string->bytes/utf-8 (cdr mt))))
                         #:exists 'replace)))

;; restore-all! : path-string? path-string? (listof string?) -> void?
;; Idempotent: restores every module that still has a backup, then removes
;; the backups.
(define (restore-all! out-dir root modules)
  (for ([module (in-list modules)])
    (define backup (module->original-path out-dir module))
    (when (file-exists? backup)
      (copy-file backup (build-path (string->path root) module) #:exists-ok? #t)
      (delete-file backup))))

;; budget-exhausted? : number? -> boolean?
(define (budget-exhausted? deadline-ms)
  (> (current-inexact-milliseconds) deadline-ms))

(define (write-mutated! root module text)
  (with-output-to-file (build-path (string->path root) module)
                       (lambda () (write-bytes (string->bytes/utf-8 text)))
                       #:exists 'replace))

;; restore-module! : path-string? (listof (cons/c string? string?)) string? -> void?
;; Restore one module from the in-memory original snapshot. Used after every
;; mutant: unlike restore-all! it must NOT delete the .originals backup, which
;; stays on disk for crash recovery until the final wind-exit.
(define (restore-module! root module-texts module)
  (define mt (assoc module module-texts))
  (when mt
    (write-mutated! root (car mt) (cdr mt))))

;; run-pilot : hash? -> hash?
;; Required keys: root, out-dir, module-texts (sorted alist), modules,
;; killing-map (alist module -> tests), budget-seconds, max-mutants,
;; test-timeout. Optional: dry-run.
;; Returns the artifact jsexpr (see pilot->jsexpr) and writes artifacts
;; incrementally after every mutant (partial progress survives a hard kill).
;; Never leaves a mutated file behind: each mutant is restored immediately
;; after its tests; the wind-exit restores anything the error paths missed.
(define (run-pilot opts)
  (define root (hash-ref opts 'root))
  (define out-dir (hash-ref opts 'out-dir))
  (define dry-run (hash-ref opts 'dry-run #f))
  (define module-texts (hash-ref opts 'module-texts))
  (define modules (map car module-texts))
  (define budget-sec (hash-ref opts 'budget-seconds 300))
  (define max-mutants (hash-ref opts 'max-mutants 25))
  (define test-timeout (hash-ref opts 'test-timeout 120))
  (define killing-map (hash-ref opts 'killing-map '()))
  (define deadline-ms (+ (current-inexact-milliseconds) (* 1000.0 budget-sec)))
  (make-directory* out-dir)
  (ensure-clean-state! out-dir root module-texts)
  (define start-ms (current-inexact-milliseconds))
  (define results '())
  (define aborted-early #f)
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (define planned (parse-plan-mutants module-texts #:max-mutants max-mutants))
     (for ([entry (in-list planned)])
       (match-define (list s mutated-text) entry)
       (define module (site-module s))
       (define tests (dict-ref killing-map module '()))
       (define-values (status killing-test outcome-details duration)
         (cond
           [(string-suffix? (symbol->string (site-operator s)) ":invalid")
            (values 'invalid #f '() 0.0)]
           [(null? tests) (values 'not-covered #f '() 0.0)]
           [dry-run (values 'not-run #f '() 0.0)]
           [(budget-exhausted? deadline-ms)
            (set! aborted-early #t)
            (values 'not-run #f '() 0.0)]
           [else
            (define t0 (current-inexact-milliseconds))
            ;; Write the mutant, run its killing tests, ALWAYS restore the
            ;; original immediately afterwards (also on infra failure).
            (write-mutated! root module mutated-text)
            (define outcome
              (with-handlers ([exn:fail? (lambda (_)
                                           (set! aborted-early #t)
                                           (list 'not-run #f '()))])
                (run-killing-tests tests root test-timeout)))
            (restore-module! root module-texts module)
            (values (list-ref outcome 0)
                    (list-ref outcome 1)
                    (list-ref outcome 2)
                    (/ (- (current-inexact-milliseconds) t0) 1000.0))]))
       (set! results
             (cons (hasheq 'module
                           module
                           'operator
                           (symbol->string (site-operator s))
                           'line
                           (site-line s)
                           'col
                           (site-col s)
                           'description
                           (site-description s)
                           'status
                           (symbol->string status)
                           'killing_test
                           (or killing-test (json-null))
                           'tests
                           tests
                           'test_outcomes
                           (for/list ([o (in-list outcome-details)])
                             (list (list-ref o 0) (symbol->string (list-ref o 1)) (list-ref o 2)))
                           'duration_seconds
                           duration)
                   results))
       (write-pilot-json!
        out-dir
        (pilot->jsexpr (reverse results) opts #:aborted aborted-early #:start-ms start-ms))))
   (lambda () (restore-all! out-dir root modules)))
  (pilot->jsexpr (reverse results) opts #:aborted aborted-early #:start-ms start-ms))

;; ---------------------------------------------------------------------------
;; Artifacts
;; ---------------------------------------------------------------------------

;; pilot->jsexpr : (listof hash?) hash? [#:aborted boolean?] [#:start-ms number?]
;;   -> hash?
(define (pilot->jsexpr results opts #:aborted [aborted #f] #:start-ms [start-ms #f])
  (define (count-status st)
    (length (filter (lambda (r) (string=? (hash-ref r 'status) st)) results)))
  (define killed (count-status "killed"))
  (define survived (count-status "survived"))
  (hasheq 'schema_version
          1
          'generated_by
          "racket scripts/run-tests/mutation-pilot.rkt"
          'tool_decision
          "repo-owned reviewed micro-mutations (no external tool met criteria)"
          'pilot_version
          pilot-version
          'racket_version
          (version)
          'modules
          (hash-ref opts 'modules)
          'budget_seconds
          (hash-ref opts 'budget-seconds 300)
          'max_mutants
          (hash-ref opts 'max-mutants 25)
          'test_timeout_seconds
          (hash-ref opts 'test-timeout 120)
          'dry_run
          (hash-ref opts 'dry-run #f)
          'mutants
          results
          'totals
          (hasheq 'planned
                  (length results)
                  'killed
                  killed
                  'survived
                  survived
                  'timeout
                  (count-status "timeout")
                  'invalid
                  (count-status "invalid")
                  'not_run
                  (count-status "not-run")
                  'not_covered
                  (count-status "not-covered"))
          'detection_score
          (if (> (+ killed survived) 0)
              (exact->inexact (/ killed (+ killed survived)))
              (json-null))
          'aborted_early
          aborted
          'wall_clock_seconds
          (if start-ms
              (/ (- (current-inexact-milliseconds) start-ms) 1000.0)
              (json-null))))

;; write-pilot-json! : path-string? hash? -> void?
(define (write-pilot-json! out-dir jsexpr)
  (with-output-to-file (build-path (string->path out-dir) "mutation-pilot.json")
                       (lambda () (write-json jsexpr))
                       #:exists 'replace))

;; write-findings! : path-string? hash? -> void?
;; Human-readable findings summary; the JSON artifact stays authoritative.
(define (write-findings! out-dir jsexpr)
  (define totals (hash-ref jsexpr 'totals))
  (define (lines-for status heading)
    (define ms
      (for/list ([m (in-list (hash-ref jsexpr 'mutants))]
                 #:when (eq? (hash-ref m 'status) status))
        (format "- ~a:~a:~a ~a"
                (hash-ref m 'module)
                (hash-ref m 'line)
                (hash-ref m 'col)
                (hash-ref m 'description))))
    (if (pair? ms)
        (cons "" (cons heading ms))
        '()))
  (define detection (hash-ref jsexpr 'detection_score))
  (define lines
    (list "# Mutation pilot findings (informational — never a gate)"
          ""
          (format "Modules: ~a" (string-join (map ~a (hash-ref jsexpr 'modules)) ", "))
          (format "Planned mutants: ~a  (budget ~as, cap ~a)"
                  (hash-ref totals 'planned)
                  (hash-ref jsexpr 'budget_seconds)
                  (hash-ref jsexpr 'max_mutants))
          (format "killed=~a survived=~a timeout=~a invalid=~a not-run=~a not-covered=~a"
                  (hash-ref totals 'killed)
                  (hash-ref totals 'survived)
                  (hash-ref totals 'timeout)
                  (hash-ref totals 'invalid)
                  (hash-ref totals 'not_run)
                  (hash-ref totals 'not_covered))
          (format "Detection score (killed/(killed+survived)): ~a"
                  (if (equal? detection (json-null)) "n/a (no executed verdicts)" detection))
          (if (hash-ref jsexpr 'aborted_early)
              "ABORTED EARLY (budget/infrastructure) — partial artifacts retained"
              "Run completed within budget")))
  (define all-lines
    (append lines
            (lines-for 'survived "## Surviving mutants (actionable gaps)")
            (lines-for 'timeout "## Timeout mutants (never counted as pass)")
            (lines-for 'not-covered "## Uncovered scope (no killing tests — @covers gap)")))
  (with-output-to-file (build-path (string->path out-dir) "findings.md")
                       (lambda () (display (string-append (string-join all-lines "\n") "\n")))
                       #:exists 'replace))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (parse-tests-for specs)
  ;; "module=test1,test2" -> (cons module (list test1 test2))
  (for/list ([spec (in-list specs)])
    (match (string-split spec "=")
      [(list module tests) (cons module (filter non-empty-string? (string-split tests ",")))]
      [else (raise-user-error 'mutation-pilot "bad --tests-for spec: ~a" spec)])))

;; changed-modules-from-diff : path-string? string? -> (listof string?)
;; Production .rkt modules changed relative to BASE (tests/ and docs/
;; excluded — only modules the coverage manifest can reason about).
(define (changed-modules-from-diff root base)
  (define git (find-executable-path "git"))
  (unless git
    (raise-user-error 'mutation-pilot "git not found for --from-diff"))
  (define-values (sp out _in _err) (subprocess #f #f #f git "-C" root "diff" "--name-only" base))
  (define text (port->string out))
  (subprocess-wait sp)
  (unless (eq? (subprocess-status sp) 0)
    (raise-user-error 'mutation-pilot "git diff ~a failed: ~a" base (string-trim text)))
  (filter (lambda (p)
            (and (string-suffix? p ".rkt")
                 (not (string-prefix? p "tests/"))
                 (not (string-prefix? p "docs/"))))
          (filter non-empty-string? (map string-trim (string-split text "\n")))))

;; main : (vectorof string?) -> hash?
(define (main [argv (current-command-line-arguments)])
  (define modules-spec #f)
  (define from-diff #f)
  (define budget 300)
  (define max-mutants 25)
  (define test-timeout 120)
  (define out-dir "reports/mutation-pilot")
  (define global-tests '())
  (define tests-for-specs '())
  (define dry-run #f)
  (define root (path->string (current-directory)))
  (command-line
   #:program "mutation-pilot"
   #:argv (vector->list argv)
   #:once-each
   [("--budget")
    sec
    "wall-clock budget in seconds (default 300)"
    (set! budget (or (string->number sec) (raise-user-error 'mutation-pilot "bad --budget: ~a" sec)))]
   [("--max-mutants")
    n
    "total mutant cap (default 25)"
    (set! max-mutants
          (or (string->number n) (raise-user-error 'mutation-pilot "bad --max-mutants: ~a" n)))]
   [("--test-timeout")
    sec
    "per-test timeout in seconds (default 120)"
    (set! test-timeout
          (or (string->number sec) (raise-user-error 'mutation-pilot "bad --test-timeout: ~a" sec)))]
   [("--out") dir "artifact directory (default reports/mutation-pilot)" (set! out-dir dir)]
   [("--root") dir "repo root (default cwd)" (set! root dir)]
   [("--dry-run") "plan only: enumerate sites, write plan, execute nothing" (set! dry-run #t)]
   [("--from-diff")
    base
    "mutate changed .rkt modules from git diff BASE (no repo-wide path)"
    (set! from-diff base)]
   [("--modules")
    list
    "comma-separated repo-relative modules (required unless --from-diff)"
    (set! modules-spec list)]
   [("--tests")
    list
    "global fallback killing tests (comma-separated)"
    (set! global-tests (filter non-empty-string? (string-split list ",")))]
   #:multi [("--tests-for")
            spec
            "module=test1,test2 — explicit killing tests (repeatable)"
            (set! tests-for-specs (cons spec tests-for-specs))])
  (define modules
    (cond
      [modules-spec (filter non-empty-string? (string-split modules-spec ","))]
      [from-diff (changed-modules-from-diff root from-diff)]
      [else
       (raise-user-error 'mutation-pilot
                         (string-append "no scope: pass --modules a.rkt,b.rkt or --from-diff BASE "
                                        "(there is no repo-wide default)"))]))
  (when (null? modules)
    (raise-user-error 'mutation-pilot "scope resolved to zero modules"))
  (for ([m (in-list modules)])
    (unless (file-exists? (build-path (string->path root) m))
      (raise-user-error 'mutation-pilot "module not found: ~a" m)))
  (define sorted-modules (sort modules string<?))
  (define module-texts
    (for/list ([m (in-list sorted-modules)])
      (cons m (file->string (build-path (string->path root) m)))))
  (define overrides (parse-tests-for (reverse tests-for-specs)))
  (define manifest-map (manifest->killing-map root sorted-modules))
  (define killing-alist
    (for/list ([m (in-list sorted-modules)])
      (cons m (resolve-killing-tests m manifest-map overrides global-tests))))
  (define opts
    (hasheq 'root
            root
            'out-dir
            out-dir
            'dry-run
            dry-run
            'module-texts
            module-texts
            'modules
            sorted-modules
            'budget-seconds
            budget
            'max-mutants
            max-mutants
            'test-timeout
            test-timeout
            'killing-map
            killing-alist))
  (unless dry-run
    (displayln (format "mutation-pilot: ~a module(s), budget ~as, cap ~a mutant(s)"
                       (length modules)
                       budget
                       max-mutants)))
  (define jsexpr (run-pilot opts))
  (write-findings! out-dir jsexpr)
  (unless dry-run
    (displayln (format "mutation-pilot: artifacts in ~a (findings.md + mutation-pilot.json)"
                       out-dir)))
  jsexpr)

(module+ main
  (with-handlers ([exn:fail:user? (lambda (e)
                                    (fprintf (current-error-port) "~a\n" (exn-message e))
                                    (exit 2))])
    (main)))
