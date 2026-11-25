#lang racket/base

;; w3-dry-run.rkt — CL5 W3.1: single-case dry run (machinery validation only).
;;
;; Purpose. W3.1 is NOT a measurement result and cannot support a GO decision.
;; It validates that the W3 machinery can, for one frozen case:
;;   1. bind every number to an exact revision, tree SHA, contract revision and
;;      selection input (no floating refs, no re-extraction);
;;   2. enumerate the selected set and the comprehensive set EXPLICITLY from the
;;      shipped tools' own plans (`--explain`), never from filename inference;
;;   3. classify through the frozen W3 taxonomy and cross-check the W2 result;
;;   4. record timing that is measured on this host, and refuse CI-derived
;;      timing instead of substituting it;
;;   5. be re-derived from the record alone (`verify-record`), so an independent
;;      agent can reproduce the classification from frozen inputs.
;;
;; The selected set is executed EXACTLY as enumerated (`./qt full --file ...`
;; per enumerated file): enumeration and execution cannot drift. The clean
;; control runs that same enumerated set against the unmodified revision.
;;
;; No production selector, scheduler, admission record or CI policy is changed.

(require file/sha1
         json
         racket/file
         racket/cmdline
         racket/format
         racket/list
         racket/match
         racket/path
         racket/place
         racket/string
         "ci-evidence.rkt"
         "ci-producers.rkt"
         "contract-layer.rkt"
         "selection-cases.rkt"
         "selection-experiment.rkt")

(provide parse-explain-files
         parse-selected-explain
         enumeration-digest
         comprehensive-producers
         combine-enumerations
         decision-instruction
         w3-outcome
         timing-record
         host-fingerprint
         verify-record
         verify-decision-record
         verify-replay-record
         run-dry-case
         run-decision-case
         run-replay-case)

;; ---- explicit plan parsing ---------------------------------------------------------

(define (truncate-text text)
  (if (> (string-length text) 400)
      (substring text 0 400)
      text))

;; The shipped `--explain` plans declare `selection: N files` and then list
;; exactly N repo-relative paths, one per line, at the end of the output. Both
;; sides must agree: a header/payload mismatch is refused, never repaired.
(define (parse-explain-files text)
  (define declared
    (match (regexp-match #px"selection: ([0-9]+) files" text)
      [(list _ n) (string->number n)]
      [#f
       (raise-arguments-error 'parse-explain-files
                              "no 'selection: N files' header in the plan"
                              "output"
                              (truncate-text text))]))
  (define lines
    (for/list ([line (in-list (string-split text "\n"))]
               #:unless (string=? (string-trim line) ""))
      (string-trim line)))
  ;; The plan block is the contiguous run of repo-relative paths. The output
  ;; continues with an explicit "NOT PROVIDED" footer after it, so the block is
  ;; located by structure and its length must equal the declared count exactly:
  ;; a shorter block and an extra path are both refused.
  (define path-like?
    (lambda (line)
      (and (string-suffix? line ".rkt")
           (string-contains? line "/")
           (not (regexp-match? #px"\\s" line)))))
  (define indexed
    (for/list ([line (in-list lines)]
               [index (in-naturals)]
               #:when (path-like? line))
      (cons index line)))
  (define start (and (pair? indexed) (caar indexed)))
  ;; the block is the maximal run of consecutive indices starting at `start`
  (define consecutive
    (if start
        (let loop ([entries indexed]
                   [next start]
                   [acc '()])
          (cond
            [(null? entries) (reverse acc)]
            [(= (caar entries) next) (loop (cdr entries) (add1 next) (cons (cdar entries) acc))]
            [else (reverse acc)]))
        '()))
  (when (null? consecutive)
    (raise-arguments-error 'parse-explain-files
                           "the plan lists no repo-relative Racket paths"
                           "declared"
                           declared))
  (unless (= declared (length consecutive))
    (raise-arguments-error 'parse-explain-files
                           "the plan's payload does not match its declared count"
                           "declared"
                           declared
                           "payload"
                           (length consecutive)))
  (hash 'declared declared 'files (sort consecutive string<?)))

;; The selected plan carries two views of the same set: `included:` lines and a
;; trailing bare-path list. They must agree; a disagreement is a plan-parsing
;; defect, not something to pick a winner from.
(define (parse-selected-explain text)
  (define included
    (for/list ([line (in-list (string-split text "\n"))]
               #:when (string-prefix? (string-trim line) "included:"))
      (car (string-split (string-trim (substring (string-trim line) (string-length "included:")))
                         " "))))
  (define trailing
    (for/list ([line (in-list (string-split text "\n"))]
               #:when (let ([trimmed (string-trim line)])
                        (and (string-suffix? trimmed ".rkt")
                             (string-contains? trimmed "/")
                             (not (regexp-match? #px"\\s" trimmed)))))
      (string-trim line)))
  (unless (equal? (sort (remove-duplicates included) string<?)
                  (sort (remove-duplicates trailing) string<?))
    (raise-arguments-error 'parse-selected-explain
                           "the selected plan's two views disagree"
                           "included"
                           (sort (remove-duplicates included) string<?)
                           "trailing"
                           (sort (remove-duplicates trailing) string<?)))
  (hash 'files
        (sort (remove-duplicates included) string<?)
        'escalations
        (for/list ([line (in-list (string-split text "\n"))]
                   #:when (string-prefix? (string-trim line) "escalation:"))
          (string-trim line))))

;; ---- digests -----------------------------------------------------------------------

;; SHA-1 is used as an identity digest for provenance (list equality), never as
;; a security hash. The algorithm is recorded with every digest so no reader can
;; mistake it for a signature.
(define (enumeration-digest files)
  (bytes->hex-string (sha1-bytes (string->bytes/utf-8
                                  (string-append (string-join (sort files string<?) "\n") "\n")))))

(define (bytes->hex-string bytes)
  (apply string-append
         (for/list ([byte (in-bytes bytes)])
           (~r byte #:min-width 2 #:pad-string "0" #:base 16))))

;; ---- comprehensive enumeration -----------------------------------------------------

;; The authoritative comprehensive surface is the required CI producer set, from
;; the ONE registry the F2 baseline verifier uses (ci-producers.rkt): fast shards
;; 0..2, platform, security, workflows shards 0..1, and smoke. Each lane producer
;; is enumerated by the shipped `--explain` under the same lane/shard spec CI
;; passes; smoke has no artifact and its executed set is the workflow-declared
;; command file -- never an inferred `--lane smoke` list (W3.1 overcounted smoke
;; by 18 files, disclosed in the F2 report).
(define (comprehensive-producers)
  baseline-producers)

;; producer records come in as (name, declared, files). Shards of one lane must
;; partition that lane (duplicates would double-count and hide a plan change).
(define (combine-enumerations records)
  (for ([record (in-list records)])
    (define declared (hash-ref record 'declared))
    (define files (hash-ref record 'files))
    (unless (= declared (length files))
      (raise-arguments-error 'combine-enumerations
                             "a producer's declared count does not match its file list"
                             "producer"
                             (hash-ref record 'name)
                             "declared"
                             declared
                             "listed"
                             (length files))))
  (check-shards-partition records)
  (define union
    (sort (remove-duplicates (append* (map (lambda (r) (hash-ref r 'files)) records))) string<?))
  (define producers
    (for/list ([record (in-list records)])
      (hash 'name
            (hash-ref record 'name)
            'declared
            (hash-ref record 'declared)
            'files-count
            (length (hash-ref record 'files))
            'digest
            (enumeration-digest (hash-ref record 'files)))))
  ;; The per-producer file lists are not stored (they are mostly the union), so
  ;; their digests are inputs, not re-derivations. An aggregate digest makes an
  ;; edited producer row detectable the same way the list digests are: integrity
  ;; against corruption/editing, not a signature.
  (hash 'producers
        producers
        'producers-digest
        (enumeration-digest
         (for/list ([p (in-list producers)])
           (format "~a:~a:~a" (hash-ref p 'name) (hash-ref p 'declared) (hash-ref p 'digest))))
        'files
        union
        'union-count
        (length union)
        'digest
        (enumeration-digest union)
        'digest-algorithm
        "sha1"))

(define (check-shards-partition records)
  (for ([lane (in-list (remove-duplicates (map (lambda (r) (hash-ref r 'lane)) records)))])
    (define group (filter (lambda (r) (equal? (hash-ref r 'lane) lane)) records))
    (define total (hash-ref (car group) 'shard-total #f))
    (when total
      (define listed (append* (map (lambda (r) (hash-ref r 'files)) group)))
      (unless (= (length listed) (length (remove-duplicates listed)))
        (raise-arguments-error 'combine-enumerations
                               "shard plans overlap; the lane partition is not explicit"
                               "lane"
                               lane)))))

;; ---- classification ----------------------------------------------------------------

;; A JSON round-trip turns symbols into strings; a verifier must accept the
;; recorded form or it could never re-check a committed record.
(define (as-symbol value)
  (if (string? value)
      (string->symbol value)
      value))

;; The deployed decision layer's instruction. F3 evaluates the tree's own
;; contract layer (see contract-layer.rkt) and records its decision; the
;; instruction below is the layer's, never invented. A revision without the
;; contract layer has no escalation path: its instruction is exactly the shipped
;; selector's, i.e. run the reduced set (`run-required`). An instruction that
;; cannot be resolved to a measurable set is `unavailable`, never `run-all`
;; quietly treated as an escalation.
(define (decision-instruction layer)
  (cond
    [(hash-has-key? layer 'status) (layer-instruction layer)]
    ;; Frozen W3.1 record shape: absence was the only representable state.
    [else
     (case (as-symbol (hash-ref layer 'state 'unavailable))
       [(absent-at-revision) 'run-required]
       [(present) (as-symbol (hash-ref layer 'instruction 'unavailable))]
       [else 'unavailable])]))

;; W3 outcome taxonomy (frozen in docs/reports/CL5-W3-DESIGN.md), applied to the
;; shadow execution + the decision layer's instruction. The reduced set is always
;; executed for measurement; the instruction only labels the case.
(define (w3-outcome instruction comprehensive reduced control)
  (define instruction* (as-symbol instruction))
  (define comprehensive* (as-symbol comprehensive))
  (define reduced* (as-symbol reduced))
  (define control* (as-symbol control))
  (cond
    [(memq instruction* '(unavailable run-all)) 'unavailable]
    [(eq? control* 'unavailable) 'unavailable]
    [(eq? control* 'fail) 'unavailable]
    [(eq? comprehensive* 'unavailable) 'unavailable]
    [(eq? reduced* 'unavailable) 'unavailable]
    [(eq? comprehensive* 'pass) 'out-of-scope]
    [(eq? comprehensive* 'fail)
     (if (eq? instruction* 'run-required)
         (if (eq? reduced* 'fail) 'correct-reduced 'unsafe-reduced)
         (if (eq? reduced* 'fail) 'safe-but-useless 'correct-escalation))]
    [else 'unavailable]))

;; ---- timing -------------------------------------------------------------------------

(define (hostname)
  (define from-file
    (and (file-exists? "/etc/hostname")
         (let ([text (string-trim (file->string "/etc/hostname"))])
           (and (not (equal? text "")) text))))
  (or from-file (getenv "HOSTNAME") "unknown"))

(define (host-fingerprint)
  (hash 'hostname
        (hostname)
        'processor-count
        (processor-count)
        'os
        (symbol->string (system-type 'os))
        'racket-version
        (version)))

;; Timing that decides a cost comparison must be measured on this host under the
;; same conditions as the other arm. CI durations are never a substitute: the
;; record refuses them rather than reporting a crossed-machine comparison.
(define (timing-record #:host host
                       #:selected-seconds selected-seconds
                       #:comprehensive-source comprehensive-source
                       #:comprehensive-seconds comprehensive-seconds)
  (when (eq? comprehensive-source 'ci)
    (raise-arguments-error 'timing-record
                           "CI-derived comprehensive timing may not be substituted"
                           "source"
                           comprehensive-source))
  ;; "same host" is only claimable when BOTH timings exist on this machine. A
  ;; missing comprehensive timing leaves the comparison unavailable, never true.
  (define same-host?
    (cond
      [(not (eq? comprehensive-source 'local)) 'unavailable]
      [(and (hash? host) (not (equal? (hash-ref host 'hostname "unknown") "unknown"))) #t]
      [else 'unverified]))
  (hash 'host
        host
        'selected-seconds
        selected-seconds
        'comprehensive-seconds
        comprehensive-seconds
        'comprehensive-source
        comprehensive-source
        'same-host
        same-host?
        'ci-timing-substituted?
        #f
        'm6
        'unavailable
        'm6-basis
        (string-append "M6 is a corpus-level cost ratio: it needs the escalation rate M3 and a "
                       "matched-host comprehensive median C_comp. One dry case supplies neither, "
                       "and no CI duration may be substituted for C_comp.")))

;; ---- record verification ------------------------------------------------------------

;; Re-derive every claim that can be re-derived from the record alone. This is
;; the mechanism an independent agent uses: it must not trust the stored label.
;;
;; What this CANNOT do: it cannot detect a *consistent* forgery of the recorded
;; verdicts, because the verdicts are the trust anchor. The external anchors are
;; the git tree SHAs (the committed tree is re-derived from the frozen mutation
;; at run time) and the CI runs keyed by tree SHA; a verifier must check those
;; against GitHub, not only this record.
(define (verify-record record)
  (define problems '())
  (define (problem! message)
    (set! problems (cons message problems)))
  (define selection-input (hash-ref record 'selection-input))
  (define comprehensive-input (hash-ref record 'comprehensive-input))
  (define provenance (hash-ref record 'provenance))
  (define contract-layer (hash-ref record 'contract-layer))
  (define defect (hash-ref record 'defect))
  (define control (hash-ref record 'control))
  ;; 1. explicit enumerations are internally consistent and digest-bound
  (for ([entry (in-list (list (cons "selected" selection-input)
                              (cons "comprehensive" comprehensive-input)))])
    (define files (hash-ref (cdr entry) 'files))
    (unless (equal? (hash-ref (cdr entry) 'digest) (enumeration-digest files))
      (problem! (format "~a digest does not match its file list" (car entry))))
    (unless (equal? (sort files string<?) (hash-ref (cdr entry) 'files))
      (problem! (format "~a file list is not sorted" (car entry)))))
  ;; 2. the executed set is exactly the enumerated set
  (define enumerated (hash-ref selection-input 'files))
  (define (same-set? files)
    (equal? (sort files string<?) enumerated))
  (unless (same-set? (hash-ref (hash-ref defect 'selected) 'selected))
    (problem! "the defect execution's file list is not the enumerated selection"))
  (unless (same-set? (hash-ref (hash-ref control 'selected) 'selected))
    (problem! "the control execution's file list is not the enumerated selection"))
  (unless (same-set? (hash-ref (hash-ref defect 'strategy) 'selected))
    (problem! "the deployed strategy's selected set is not the enumerated selection"))
  ;; 3. provenance: the committed tree changes exactly the declared files
  (unless (hash-ref provenance 'provenance-ok?)
    (problem! "provenance is not ok"))
  (unless (equal? (sort (hash-ref provenance 'committed-changes) string<?)
                  (sort (hash-ref provenance 'declared-changes) string<?))
    (problem! "committed changes differ from the declared mutation"))
  ;; 4. classification re-derives from the recorded verdicts
  (define instruction (decision-instruction contract-layer))
  (define defect-verdict (as-symbol (hash-ref (hash-ref defect 'selected) 'verdict)))
  (define control-verdict (as-symbol (hash-ref (hash-ref control 'selected) 'verdict)))
  (define defect-comprehensive (as-symbol (hash-ref defect 'comprehensive-verdict)))
  (define control-comprehensive (as-symbol (hash-ref control 'comprehensive-verdict)))
  (define expected-w3 (w3-outcome instruction defect-comprehensive defect-verdict control-verdict))
  ;; 4b. the contract block is structurally sound and its instruction follows
  ;; from its own recorded decision (F3). A counterfactual evaluation may never
  ;; be recorded as a historical one.
  (define layer-problems (validate-layer-record contract-layer (hash-ref comprehensive-input 'files)))
  (when (pair? layer-problems)
    (problem! (format "contract layer record: ~a" (string-join layer-problems "; "))))
  (define recorded-mode (as-symbol (hash-ref (hash-ref record 'classification) 'mode 'historical)))
  (unless (eq? recorded-mode (as-symbol (hash-ref contract-layer 'mode 'historical)))
    (problem! "classification mode disagrees with the contract layer mode"))
  (unless (eq? expected-w3 (as-symbol (hash-ref (hash-ref record 'classification) 'w3-outcome)))
    (problem! (format "w3 outcome re-derives to ~a" expected-w3)))
  (define expected-w2
    (classify-case-evidence defect-verdict
                            control-verdict
                            defect-comprehensive
                            control-comprehensive))
  (unless (eq? expected-w2 (as-symbol (hash-ref (hash-ref record 'classification) 'w2)))
    (problem! (format "W2 classification re-derives to ~a" expected-w2)))
  ;; 5. producer accounting and its aggregate digest
  (define producers (hash-ref comprehensive-input 'producers))
  (when (hash-ref comprehensive-input 'producers-digest #f)
    (unless (equal?
             (hash-ref comprehensive-input 'producers-digest)
             (enumeration-digest
              (for/list ([p (in-list producers)])
                (format "~a:~a:~a" (hash-ref p 'name) (hash-ref p 'declared) (hash-ref p 'digest)))))
      (problem! "the producer aggregate digest does not match the producer rows")))
  (for ([p (in-list producers)])
    (unless (= (hash-ref p 'declared) (hash-ref p 'files-count))
      (problem! (format "producer ~a's count and declared total disagree" (hash-ref p 'name)))))
  ;; 6. timing consistency: a single host, no CI substitution, and the timed
  ;; value is the deployed strategy's own measurement.
  (define timing (hash-ref record 'timing))
  (define source (as-symbol (hash-ref timing 'comprehensive-source)))
  (define comprehensive-seconds (hash-ref timing 'comprehensive-seconds))
  (when (hash-ref timing 'ci-timing-substituted?)
    (problem! "CI timing was substituted into the record"))
  (when (eq? source 'ci)
    (problem! "comprehensive timing was sourced from CI"))
  (when (and (eq? source 'unavailable) (not (eq? (as-symbol comprehensive-seconds) 'unavailable)))
    (problem! "comprehensive timing is declared unavailable but carries a value"))
  (unless (= (hash-ref timing 'selected-seconds) (hash-ref (hash-ref defect 'strategy) 'seconds))
    (problem! "selected timing is not the deployed strategy's measured seconds"))
  (define same-host (as-symbol (hash-ref timing 'same-host)))
  (when (and (eq? source 'unavailable) (not (eq? same-host 'unavailable)))
    (problem! "same-host is claimed without a comprehensive timing to compare"))
  (if (null? problems)
      #t
      (raise-arguments-error 'verify-record
                             "record verification failed"
                             "problems"
                             (reverse problems))))

;; ---- execution ---------------------------------------------------------------------

(define (refuse-inside! worktree out-file)
  (define wt (path->string (simplify-path (path->complete-path worktree))))
  (define out (path->string (simplify-path (path->complete-path out-file))))
  (when (string-prefix? out (string-append wt "/"))
    (raise-arguments-error 'run-dry-case
                           "the output path must live outside the disposable worktree"
                           "out"
                           out)))

(define (preflight! worktree revision)
  (define head (git! worktree "rev-parse" "HEAD"))
  (unless (equal? (string-trim head) revision)
    (raise-arguments-error 'run-dry-case
                           "worktree HEAD is not the pinned revision"
                           "head"
                           (string-trim head)
                           "revision"
                           revision))
  (define status (string-trim (git! worktree "status" "--porcelain")))
  (unless (equal? status "")
    (raise-arguments-error 'run-dry-case "worktree is not clean" "status" status))
  (define git-dir (string-trim (git! worktree "rev-parse" "--git-dir")))
  (when (member git-dir '(".git" "./.git"))
    (raise-arguments-error 'run-dry-case
                           "the measurement worktree must be a disposable linked worktree"
                           "git-dir"
                           git-dir)))

(define (explain! worktree command #:timeout [timeout 600])
  (define-values (exit-code _elapsed stdout stderr timed-out?)
    (run-command command #:cwd worktree #:timeout-seconds timeout))
  (when (or timed-out? (not (equal? exit-code 0)))
    (raise-arguments-error 'run-dry-case
                           "the shipped tool's plan enumeration failed"
                           "command"
                           command))
  stdout)

(define (enumerate-selected! worktree)
  (define text (explain! worktree (list "./qt" "quick" "--changed" "HEAD" "--explain")))
  (parse-selected-explain text))

(define (enumerate-comprehensive! worktree)
  (define records
    (for/list ([producer (in-list (comprehensive-producers))])
      (cond
        [(lane-producer? producer)
         (define text (explain! worktree (producer-plan-command producer)))
         (define plan (parse-explain-files text))
         (hash 'name
               (hash-ref producer 'name)
               'lane
               (hash-ref producer 'lane)
               'shard-total
               (hash-ref producer 'shard-total)
               'declared
               (hash-ref plan 'declared)
               'files
               (hash-ref plan 'files))]
        [else
         ;; A declared-command producer (smoke): the workflow names the files.
         (define files (hash-ref producer 'files))
         (hash 'name
               (hash-ref producer 'name)
               'lane
               "smoke"
               'shard-total
               #f
               'declared
               (length files)
               'files
               files)])))
  (combine-enumerations records))

(define (tree-sha! worktree)
  (git! worktree "add" "-A")
  (git! worktree "write-tree"))

(define (derived-record-at-revision worktree revision)
  (for/list ([rel (in-list (list "tests/.quick-admission.rktd" "tests/.coverage-manifest.json"))])
    (hash 'path rel 'sha (string-trim (git! worktree "rev-parse" (format "~a:~a" revision rel))))))

(define (preflight-contract! contract-worktree contract-revision)
  (define head (string-trim (git! contract-worktree "rev-parse" "HEAD")))
  (unless (equal? head contract-revision)
    (raise-arguments-error 'run-dry-case
                           "the contract worktree is not at the contract revision"
                           "head"
                           head
                           "contract-revision"
                           contract-revision))
  (define status (string-trim (git! contract-worktree "status" "--porcelain")))
  (unless (equal? status "")
    (raise-arguments-error 'run-dry-case "the contract worktree is not clean" "status" status)))

(define (ci-verdict-for-cache cache tree-sha)
  (define lookup (or (hash-ref cache tree-sha #f) (hash-ref cache (string->symbol tree-sha) #f)))
  (if lookup
      (hash 'verdict
            (string->symbol (hash-ref lookup 'verdict))
            'source
            'cache
            'run-id
            (hash-ref lookup 'run-id #f)
            'head-sha
            (hash-ref lookup 'head-sha #f)
            'detecting
            (hash-ref lookup 'detecting '())
            'behavioural-detecting
            (hash-ref lookup 'behavioural-detecting '())
            'detecting-note
            (hash-ref lookup 'detecting-note #f)
            'missing-jobs
            (hash-ref lookup 'missing-jobs '())
            'unavailable-reasons
            (hash-ref lookup 'unavailable-reasons '()))
      (hash 'verdict
            'unavailable
            'source
            'absent
            'run-id
            #f
            'head-sha
            #f
            'detecting
            '()
            'missing-jobs
            '()
            'unavailable-reasons
            (list (format "no comprehensive verdict cached for tree ~a" tree-sha)))))

(define (run-dry-case worktree
                      revision
                      case
                      #:ci-cache cache
                      #:out-file out-file
                      #:contract-worktree [contract-worktree #f]
                      #:contract-revision [contract-revision #f])
  (refuse-inside! worktree out-file)
  (preflight! worktree revision)
  (define layer-worktree (or contract-worktree worktree))
  (define layer-revision (or contract-revision revision))
  (when (and contract-worktree (not contract-revision))
    (raise-arguments-error 'run-dry-case "--contract-worktree requires --contract-revision"))
  (when (and contract-revision (not contract-worktree))
    (raise-arguments-error 'run-dry-case "--contract-revision requires --contract-worktree"))
  (when contract-worktree
    (preflight-contract! layer-worktree layer-revision))
  (define revision-tree (string-trim (git! worktree "rev-parse" (format "~a^{tree}" revision))))
  ;; ---- defect state: enumerate, then execute exactly the enumeration ----
  (git! worktree "reset" "-q" "--hard" revision)
  (git! worktree "clean" "-q" "-fd")
  (void (apply-case! worktree case))
  ((current-admission-refresh) worktree)
  (define selected-plan (enumerate-selected! worktree))
  (define selected-files (hash-ref selected-plan 'files))
  (unless (pair? selected-files)
    (raise-arguments-error 'run-dry-case "the selected enumeration is empty"))
  (define comprehensive (enumerate-comprehensive! worktree))
  ;; (a) execute EXACTLY the enumerated list, file by file: this proves the
  ;; enumerated set is the set that runs, and is the diagnostic execution.
  (define defect-selected (selected-state! worktree #:files selected-files))
  ;; (b) run the DEPLOYED strategy once (`./qt quick --changed HEAD`) and time
  ;; it: C_sel is the strategy's cost, not N separate process startups. The
  ;; strategy's own plan must equal the enumeration or the case is refused.
  (define defect-strategy (selected-state! worktree))
  (unless (equal? (sort (hash-ref defect-strategy 'selected) string<?) selected-files)
    (raise-arguments-error 'run-dry-case
                           "the deployed strategy selected a different set than the enumeration"
                           "enumerated"
                           selected-files
                           "strategy"
                           (hash-ref defect-strategy 'selected)))
  (define measured-tree (string-trim (tree-sha! worktree)))
  (define derived (derived-record-at-revision worktree revision))
  (define record-changed?
    (for/or ([entry (in-list derived)])
      (not (equal? (string-trim (git! worktree "hash-object" (hash-ref entry 'path)))
                   (hash-ref entry 'sha)))))
  ((current-derived-restore) worktree revision)
  (define committed-tree (string-trim (tree-sha! worktree)))
  (define committed-changes
    (filter (lambda (p) (not (equal? p "")))
            (for/list ([p (in-list (changed-paths worktree revision-tree committed-tree))])
              (string-trim p))))
  (define declared-changes (hash-ref case 'changed))
  (define provenance-ok?
    (and (equal? (sort committed-changes string<?) (sort declared-changes string<?))))
  (unless provenance-ok?
    (raise-arguments-error 'run-dry-case
                           "the committed state does not match the registered mutation"
                           "declared"
                           declared-changes
                           "committed"
                           committed-changes))
  ;; ---- contract-aware decision, evaluated on the recorded tree ----
  ;; The layer runs against the same tree the provenance just recorded (mutation
  ;; applied, derived files restored), so the decision and the tree SHA name the
  ;; same input. It is evaluation-only: no test set is chosen by it.
  (define layer
    (evaluate-layer #:root worktree
                    #:changed declared-changes
                    #:comprehensive (hash-ref comprehensive 'files)
                    #:evaluated-revision revision
                    #:evaluated-tree committed-tree
                    #:contract-worktree layer-worktree
                    #:contract-revision layer-revision))
  ;; ---- clean control: the SAME enumerated set on the unmodified revision ----
  (git! worktree "reset" "-q" "--hard" revision)
  (git! worktree "clean" "-q" "-fd")
  (define control-selected (selected-state! worktree #:files selected-files))
  (define control-tree (string-trim (tree-sha! worktree)))
  (unless (equal? control-tree revision-tree)
    (raise-arguments-error 'run-dry-case
                           "the clean control tree is not the revision tree"
                           "control-tree"
                           control-tree
                           "revision-tree"
                           revision-tree))
  ;; ---- comprehensive ground truth from the frozen cache, keyed by tree ----
  (define defect-ci (ci-verdict-for-cache cache committed-tree))
  (define control-ci (ci-verdict-for-cache cache revision-tree))
  (define instruction (decision-instruction layer))
  (define w3
    (w3-outcome instruction
                (hash-ref defect-ci 'verdict)
                (hash-ref defect-selected 'verdict)
                (hash-ref control-selected 'verdict)))
  (define w2
    (classify-case-evidence (hash-ref defect-selected 'verdict)
                            (hash-ref control-selected 'verdict)
                            (hash-ref defect-ci 'verdict)
                            (hash-ref control-ci 'verdict)))
  (define timing
    (timing-record #:host (host-fingerprint)
                   #:selected-seconds (hash-ref defect-strategy 'seconds)
                   #:comprehensive-source 'unavailable
                   #:comprehensive-seconds 'unavailable))
  (define record
    (hash 'experiment
          "CL5 W3 dry run (#9982, F3 contract-aware)"
          'protocol
          "docs/reports/CL5-W3-DESIGN.md"
          'generated-by
          "scripts/cl5/w3-dry-run.rkt"
          'arm
          (hash 'revision
                revision
                'revision-tree
                revision-tree
                'worktree
                (path->string (simplify-path (path->complete-path worktree))))
          'case
          (hash 'id
                (hash-ref case 'id)
                'category
                (hash-ref case 'category)
                'note
                (hash-ref case 'note)
                'declared-changes
                declared-changes)
          'selection-input
          (hash 'command
                "./qt quick --changed HEAD --explain"
                'changed
                declared-changes
                'files
                selected-files
                'digest
                (enumeration-digest selected-files)
                'digest-algorithm
                "sha1"
                'escalations
                (hash-ref selected-plan 'escalations)
                'derived-at-revision
                derived
                'record-changed-by-refresh?
                record-changed?)
          'comprehensive-input
          comprehensive
          'provenance
          (hash 'measured-tree-sha
                measured-tree
                'committed-tree-sha
                committed-tree
                'revision-tree-sha
                revision-tree
                'committed-changes
                committed-changes
                'declared-changes
                declared-changes
                'provenance-ok?
                provenance-ok?
                'derived-at-revision
                derived
                'record-changed-by-refresh?
                record-changed?)
          'contract-layer
          layer
          'defect
          (hash 'selected
                defect-selected
                'strategy
                defect-strategy
                'comprehensive-verdict
                (hash-ref defect-ci 'verdict)
                'comprehensive
                defect-ci)
          'control
          (hash 'selected
                control-selected
                'comprehensive-verdict
                (hash-ref control-ci 'verdict)
                'comprehensive
                control-ci
                'tree-sha
                control-tree)
          'timing
          timing
          'classification
          (hash 'instruction instruction 'w2 w2 'w3-outcome w3 'mode (hash-ref layer 'mode))))
  (define verified
    (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
      (verify-record record)))
  (unless (eq? verified #t)
    (raise-arguments-error 'run-dry-case "record failed self-verification" "reason" verified))
  (make-directory* (path-only out-file))
  (define tmp (string->path (string-append (path->string out-file) ".tmp")))
  (call-with-output-file tmp
                         (lambda (out) (write-string (jsexpr->string (->jsexpr record)) out))
                         #:exists 'truncate/replace)
  (rename-file-or-directory tmp out-file #t)
  record)

;; ---- decision-only record (F3) -----------------------------------------------------
;;
;; A decision record binds the contract decision to one tree and one case without
;; executing any test. It is the artifact F4 consumes and the cheapest way to
;; check the integration against a pinned revision.

(define (write-record! out-file record)
  (make-directory* (path-only out-file))
  (define tmp (string->path (string-append (path->string out-file) ".tmp")))
  (call-with-output-file tmp
                         (lambda (out) (write-string (jsexpr->string (->jsexpr record)) out))
                         #:exists 'truncate/replace)
  (rename-file-or-directory tmp out-file #t))

(define (run-decision-case worktree
                           revision
                           case
                           #:out-file out-file
                           #:contract-worktree [contract-worktree #f]
                           #:contract-revision [contract-revision #f])
  (refuse-inside! worktree out-file)
  (preflight! worktree revision)
  (when (and contract-worktree (not contract-revision))
    (raise-arguments-error 'run-decision-case "--contract-worktree requires --contract-revision"))
  (when (and contract-revision (not contract-worktree))
    (raise-arguments-error 'run-decision-case "--contract-revision requires --contract-worktree"))
  (define layer-worktree (or contract-worktree worktree))
  (define layer-revision (or contract-revision revision))
  (when contract-worktree
    (preflight-contract! layer-worktree layer-revision))
  (define revision-tree (string-trim (git! worktree "rev-parse" (format "~a^{tree}" revision))))
  (git! worktree "reset" "-q" "--hard" revision)
  (git! worktree "clean" "-q" "-fd")
  (void (apply-case! worktree case))
  ((current-admission-refresh) worktree)
  (define comprehensive (enumerate-comprehensive! worktree))
  ((current-derived-restore) worktree revision)
  (define mutated-tree (string-trim (tree-sha! worktree)))
  (define declared-changes (hash-ref case 'changed))
  (define committed-changes
    (filter (lambda (p) (not (equal? p "")))
            (for/list ([p (in-list (changed-paths worktree revision-tree mutated-tree))])
              (string-trim p))))
  (unless (equal? (sort committed-changes string<?) (sort declared-changes string<?))
    (raise-arguments-error 'run-decision-case
                           "the measured state does not match the registered mutation"
                           "declared"
                           declared-changes
                           "committed"
                           committed-changes))
  (define layer
    (evaluate-layer #:root worktree
                    #:changed declared-changes
                    #:comprehensive (hash-ref comprehensive 'files)
                    #:evaluated-revision revision
                    #:evaluated-tree mutated-tree
                    #:contract-worktree layer-worktree
                    #:contract-revision layer-revision))
  (define layer-problems (validate-layer-record layer (hash-ref comprehensive 'files)))
  (when (pair? layer-problems)
    (raise-arguments-error 'run-decision-case
                           "the contract decision record is not structurally sound"
                           "problems"
                           layer-problems))
  (define record
    (hash 'experiment
          "CL5 W3 F3 contract decision (#9982)"
          'protocol
          "docs/reports/CL5-W3-DESIGN.md"
          'generated-by
          "scripts/cl5/w3-dry-run.rkt"
          'revision
          revision
          'revision-tree
          revision-tree
          'measured-tree-sha
          mutated-tree
          'case
          (hash 'id
                (hash-ref case 'id)
                'category
                (hash-ref case 'category)
                'note
                (hash-ref case 'note)
                'declared-changes
                declared-changes)
          'comprehensive
          comprehensive
          'contract-layer
          layer
          'decision-verified?
          #t))
  (write-record! out-file record)
  ;; Leave the disposable worktree as found so a re-run is reproducible.
  (git! worktree "reset" "-q" "--hard" revision)
  (git! worktree "clean" "-q" "-fd")
  record)

(define (verify-decision-record record)
  (define layer (hash-ref record 'contract-layer))
  (define comprehensive (hash-ref record 'comprehensive))
  (define files (hash-ref comprehensive 'files))
  (define problems (validate-layer-record layer files))
  (define (problem! message)
    (set! problems (cons message problems)))
  (unless (equal? (hash-ref comprehensive 'digest) (enumeration-digest files))
    (problem! "comprehensive digest does not match its file list"))
  (unless (= (hash-ref comprehensive 'union-count) (length files))
    (problem! "comprehensive union count does not match its file list"))
  (unless (equal? (hash-ref layer 'evaluated-revision) (hash-ref record 'revision))
    (problem! "the contract decision was not evaluated at the recorded revision"))
  (unless (equal? (hash-ref layer 'evaluated-tree) (hash-ref record 'measured-tree-sha))
    (problem! "the contract decision was not evaluated on the recorded tree"))
  (unless (hash-ref record 'decision-verified? #f)
    (problem! "the record does not claim a verified decision"))
  (if (null? problems)
      #t
      (raise-arguments-error 'verify-decision-record
                             "decision record verification failed"
                             "problems"
                             (reverse problems))))

;; ---- D5 replay (F4) ----------------------------------------------------------------
;;
;; Reconstructs the historical defect and control trees, evaluates the contract
;; layer (from a separately pinned revision) as an explicit counterfactual, and
;; executes the escalated required set on both trees. The claim is derived, never
;; assumed: "prevented" needs a required file that fails on the defect tree and
;; passes on the control tree, and is "attributed" only when that file is among
;; the historical run's own failed files.

(define (common a b)
  (filter (lambda (value) (member value b)) a))

(define (execution-verdicts execution)
  (for/hash ([row (in-list (hash-ref execution 'files '()))])
    (values (hash-ref row 'file)
            (cond
              [(hash-ref row 'timed-out #f) 'unavailable]
              [(equal? (hash-ref row 'exit) 0) 'pass]
              [else 'fail]))))

(define (run-replay-case defect-worktree
                         control-worktree
                         revision
                         case
                         #:out-file out-file
                         #:contract-worktree contract-worktree
                         #:contract-revision contract-revision
                         #:ci-cache cache
                         #:repo-root repo-root
                         #:expect-defect-tree expect-defect-tree
                         #:expect-revision-tree expect-revision-tree
                         #:timeout [timeout 2400])
  (refuse-inside! defect-worktree out-file)
  (preflight! defect-worktree revision)
  (preflight! control-worktree revision)
  (unless (and contract-worktree contract-revision)
    (raise-arguments-error 'run-replay-case "the replay needs a pinned contract layer"))
  (preflight-contract! contract-worktree contract-revision)
  (define revision-tree
    (string-trim (git! defect-worktree "rev-parse" (format "~a^{tree}" revision))))
  (unless (equal? revision-tree expect-revision-tree)
    (raise-arguments-error 'run-replay-case
                           "the revision tree is not the expected historical tree"
                           "expected"
                           expect-revision-tree
                           "actual"
                           revision-tree))
  ;; control: the untouched revision tree
  (git! control-worktree "reset" "-q" "--hard" revision)
  (git! control-worktree "clean" "-q" "-fd")
  (define control-tree (string-trim (tree-sha! control-worktree)))
  (unless (equal? control-tree expect-revision-tree)
    (raise-arguments-error 'run-replay-case
                           "the control worktree is not the revision tree"
                           "expected"
                           expect-revision-tree
                           "actual"
                           control-tree))
  ;; defect: the registered mutation
  (git! defect-worktree "reset" "-q" "--hard" revision)
  (git! defect-worktree "clean" "-q" "-fd")
  (void (apply-case! defect-worktree case))
  ((current-admission-refresh) defect-worktree)
  (define selected-plan (enumerate-selected! defect-worktree))
  (define selected-files (hash-ref selected-plan 'files))
  (define comprehensive (enumerate-comprehensive! defect-worktree))
  (define deployed-strategy (selected-state! defect-worktree #:timeout-seconds timeout))
  (unless (equal? (sort (hash-ref deployed-strategy 'selected) string<?) selected-files)
    (raise-arguments-error 'run-replay-case
                           "the deployed strategy selected a different set than the enumeration"
                           "enumerated"
                           selected-files
                           "strategy"
                           (hash-ref deployed-strategy 'selected)))
  (define derived (derived-record-at-revision defect-worktree revision))
  ((current-derived-restore) defect-worktree revision)
  (define defect-tree (string-trim (tree-sha! defect-worktree)))
  (unless (equal? defect-tree expect-defect-tree)
    (raise-arguments-error 'run-replay-case
                           "the reconstructed defect tree is not the historical defect tree"
                           "expected"
                           expect-defect-tree
                           "actual"
                           defect-tree))
  ;; counterfactual contract decision on the defect tree
  (define layer
    (evaluate-layer #:root defect-worktree
                    #:changed (hash-ref case 'changed)
                    #:comprehensive (hash-ref comprehensive 'files)
                    #:evaluated-revision revision
                    #:evaluated-tree defect-tree
                    #:contract-worktree contract-worktree
                    #:contract-revision contract-revision))
  (define decision (hash-ref layer 'decision #f))
  (define required
    (let ([value (hash-ref layer 'required 'unresolved)])
      (if (list? value)
          value
          '())))
  (define selected-tests
    (let ([selection (hash-ref layer 'selection #f)])
      (if (and selection (list? (hash-ref selection 'tests 'unresolved)))
          (hash-ref selection 'tests)
          '())))
  (define required-subset?
    (and (pair? required) (= (length required) (length (common required selected-tests)))))
  (define decision-ok?
    (and (eq? (hash-ref layer 'status) 'evaluated)
         (eq? (as-symbol (hash-ref layer 'mode 'unrecognized)) 'counterfactual)
         (and decision (eq? (as-symbol (hash-ref decision 'verdict 'unrecognized)) 'escalate))
         (eq? (layer-instruction layer) 'run-comprehensive)
         (and (member "tests/test-gsd-wave-status.rkt" required) #t)
         required-subset?))
  ;; historical evidence for the defect tree, from that run's own artifacts
  (define defect-ci (ci-verdict-for-cache cache defect-tree))
  (define control-ci (ci-verdict-for-cache cache revision-tree))
  (define run-id (hash-ref defect-ci 'run-id #f))
  (define historical (and run-id (failed-tests-in-run repo-root run-id)))
  (define ci-failed
    (if historical
        (hash-ref historical 'failed-files)
        '()))
  (define overlap (common required ci-failed))
  ;; Behavioural evidence tied to the trees, bounded and pre-registered: the
  ;; required files that the historical run itself failed, executed on the
  ;; reconstructed defect tree and on the control tree. The rule is evidence-
  ;; based (historical failures), never chosen after seeing the defect result.
  (define execution-set (sort overlap string<?))
  (define defect-execution
    (selected-state! defect-worktree #:files execution-set #:timeout-seconds 300))
  (define control-execution
    (selected-state! control-worktree #:files execution-set #:timeout-seconds 300))
  (define defect-verdicts (execution-verdicts defect-execution))
  (define control-verdicts (execution-verdicts control-execution))
  (define attributed
    (sort (for/list ([file (in-list execution-set)]
                     #:when (and (eq? (hash-ref defect-verdicts file 'unavailable) 'fail)
                                 (eq? (hash-ref control-verdicts file 'unavailable) 'pass)))
            file)
          string<?))
  (define attributed-overlap (common attributed ci-failed))
  (define behavioural? (pair? attributed))
  (define claim
    (cond
      [(not decision-ok?) 'no-escalation]
      [(and behavioural? (pair? attributed-overlap)) 'prevented-attributed]
      [behavioural? 'prevented-unattributed]
      [else 'decision-level]))
  (define record
    (hash
     'experiment
     "CL5 W3 F4 D5 replay (#9982)"
     'protocol
     "docs/reports/CL5-W3-F4-PROTOCOL.md"
     'generated-by
     "scripts/cl5/w3-dry-run.rkt"
     'revision
     revision
     'revision-tree
     revision-tree
     'case
     (hash 'id
           (hash-ref case 'id)
           'category
           (hash-ref case 'category)
           'note
           (hash-ref case 'note)
           'declared-changes
           (hash-ref case 'changed))
     'expected-trees
     (hash 'defect expect-defect-tree 'control expect-revision-tree)
     'trees
     (hash 'defect defect-tree 'control control-tree)
     'selection
     (hash 'command
           "./qt quick --changed HEAD --explain"
           'files
           selected-files
           'digest
           (enumeration-digest selected-files))
     'deployed-strategy
     (hash 'command
           "./qt quick --changed HEAD"
           'verdict
           (hash-ref deployed-strategy 'verdict)
           'selected
           (hash-ref deployed-strategy 'selected)
           'seconds
           (hash-ref deployed-strategy 'seconds)
           'failures
           (hash-ref deployed-strategy 'failures))
     'comprehensive
     comprehensive
     'contract-layer
     layer
     'escalation
     (hash 'decision-ok?
           decision-ok?
           'required
           required
           'selected
           selected-tests
           'required-count
           (length required)
           'selected-count
           (length selected-tests)
           'required-subset-of-selected?
           required-subset?)
     'required-execution
     (hash 'defect
           defect-execution
           'control
           control-execution
           'rule
           "required ∩ historical failed files"
           'files
           execution-set)
     'historical
     (hash 'run-id
           run-id
           'head-sha
           (and historical (hash-ref historical 'head-sha))
           'comprehensive-verdict
           (hash-ref defect-ci 'verdict)
           'control-comprehensive-verdict
           (hash-ref control-ci 'verdict)
           'failed-files
           ci-failed
           'overlap
           overlap
           'attributed-overlap
           attributed-overlap
           'producers
           (and historical (hash-ref historical 'producers)))
     'classification
     (hash 'decision-level decision-ok? 'behavioural behavioural? 'attributed attributed 'claim claim)
     'derived-at-revision
     derived))
  (write-record! out-file record)
  (git! defect-worktree "reset" "-q" "--hard" revision)
  (git! defect-worktree "clean" "-q" "-fd")
  (git! control-worktree "reset" "-q" "--hard" revision)
  (git! control-worktree "clean" "-q" "-fd")
  record)

(define (verify-replay-record record)
  (define problems '())
  (define (problem! message)
    (set! problems (cons message problems)))
  (define layer (hash-ref record 'contract-layer))
  (define comprehensive (hash-ref record 'comprehensive))
  (define files (hash-ref comprehensive 'files))
  (set! problems (append (reverse (validate-layer-record layer files)) problems))
  (define trees (hash-ref record 'trees))
  (define expected (hash-ref record 'expected-trees))
  (unless (equal? (hash-ref trees 'defect) (hash-ref expected 'defect))
    (problem! "the recorded defect tree is not the expected historical tree"))
  (unless (equal? (hash-ref trees 'control) (hash-ref expected 'control))
    (problem! "the recorded control tree is not the expected revision tree"))
  (define revision-tree (hash-ref record 'revision-tree))
  (unless (equal? (hash-ref trees 'control) revision-tree)
    (problem! "the control tree is not the recorded revision tree"))
  (define selection (hash-ref record 'selection))
  (unless (equal? (hash-ref selection 'digest) (enumeration-digest (hash-ref selection 'files)))
    (problem! "the selected enumeration digest does not match its list"))
  (unless (equal? (hash-ref comprehensive 'digest) (enumeration-digest files))
    (problem! "the comprehensive digest does not match its file list"))
  (unless (= (hash-ref comprehensive 'union-count) (length files))
    (problem! "the comprehensive union count does not match its file list"))
  ;; the decision follows from the recorded layer block
  (define decision (hash-ref layer 'decision #f))
  (define escalation (hash-ref record 'escalation))
  (define required (hash-ref escalation 'required))
  (define selected (hash-ref escalation 'selected))
  (define decision-ok?
    (and (eq? (as-symbol (hash-ref layer 'status 'unrecognized)) 'evaluated)
         (eq? (as-symbol (hash-ref layer 'mode 'unrecognized)) 'counterfactual)
         (and decision (eq? (as-symbol (hash-ref decision 'verdict 'unrecognized)) 'escalate))
         (eq? (layer-instruction layer) 'run-comprehensive)
         (and (member "tests/test-gsd-wave-status.rkt" required) #t)
         (pair? required)
         (= (length required) (length (common required selected)))))
  (unless (eq? decision-ok? (hash-ref escalation 'decision-ok? #f))
    (problem! "decision-ok? does not follow from the recorded layer block"))
  ;; behavioural attribution is sound for every attributed file, and the executed
  ;; set is the pre-registered rule: required ∩ the historical failed files
  (define execution (hash-ref record 'required-execution))
  (define execution-set (hash-ref execution 'files '()))
  (unless (equal? (sort execution-set string<?)
                  (sort (common required (hash-ref (hash-ref record 'historical) 'failed-files))
                        string<?))
    (problem! "the executed set is not required ∩ historical failed files"))
  (define defect-verdicts (execution-verdicts (hash-ref execution 'defect)))
  (define control-verdicts
    (execution-verdicts (hash-ref (hash-ref record 'required-execution) 'control)))
  (for ([file (in-list (hash-ref (hash-ref record 'classification) 'attributed '()))])
    (unless (member file required)
      (problem! (format "attributed file ~a is not in the required set" file)))
    (unless (eq? (hash-ref defect-verdicts file 'unavailable) 'fail)
      (problem! (format "attributed file ~a did not fail on the defect tree" file)))
    (unless (eq? (hash-ref control-verdicts file 'unavailable) 'pass)
      (problem! (format "attributed file ~a did not pass on the control tree" file))))
  ;; the historical overlap follows from the recorded failed files
  (define historical (hash-ref record 'historical))
  (define recorded-overlap (hash-ref historical 'overlap))
  (unless (equal? (sort (common required (hash-ref historical 'failed-files)) string<?)
                  (sort recorded-overlap string<?))
    (problem! "the recorded historical overlap does not follow from the failed files"))
  (define attributed (hash-ref (hash-ref record 'classification) 'attributed '()))
  (define attributed-overlap (common attributed (hash-ref historical 'failed-files)))
  (define expected-claim
    (cond
      [(not decision-ok?) 'no-escalation]
      [(and (pair? attributed) (pair? attributed-overlap)) 'prevented-attributed]
      [(pair? attributed) 'prevented-unattributed]
      [else 'decision-level]))
  (unless (eq? expected-claim (as-symbol (hash-ref (hash-ref record 'classification) 'claim 'x)))
    (problem! (format "the recorded claim does not follow from the evidence (~a)" expected-claim)))
  (if (null? problems)
      #t
      (raise-arguments-error 'verify-replay-record
                             "replay record verification failed"
                             "problems"
                             (reverse problems))))

(module+ main
  (define worktree #f)
  (define revision #f)
  (define out-file #f)
  (define cache-file #f)
  (define case-id 'D5)
  (define verify-file #f)
  (define layer-worktree #f)
  (define layer-revision #f)
  (define decide-only? #f)
  (define verify-decision-file #f)
  (define control-worktree #f)
  (define replay? #f)
  (define expect-defect-tree #f)
  (define expect-revision-tree #f)
  (define repo-root #f)
  (define verify-replay-file #f)
  (command-line
   #:program "w3-dry-run.rkt"
   #:once-each ["--worktree" path "clean disposable linked worktree" (set! worktree path)]
   ["--revision" sha "full SHA the worktree is pinned at" (set! revision sha)]
   ["--out" path "JSON record to write" (set! out-file path)]
   ["--ci-cache" path "frozen JSON map tree-sha -> CI verdict" (set! cache-file path)]
   ["--case" id "frozen case id (default D5)" (set! case-id (string->symbol id))]
   ["--contract-worktree"
    path
    "clean worktree holding the contract layer (default: --worktree); a different revision is a counterfactual"
    (set! layer-worktree path)]
   ["--contract-revision" sha "revision of the contract layer worktree" (set! layer-revision sha)]
   ["--decide-only"
    "record the contract decision for the case; run no test and need no CI cache"
    (set! decide-only? #t)]
   ["--verify" path "re-derive and verify an existing record; launch nothing" (set! verify-file path)]
   ["--verify-decision"
    path
    "re-derive and verify an existing decision record; launch nothing"
    (set! verify-decision-file path)]
   ["--replay"
    "D5 replay: evaluate the pinned layer as a counterfactual and execute the required set on both trees"
    (set! replay? #t)]
   ["--control-worktree"
    path
    "clean worktree at the evaluated revision (control)"
    (set! control-worktree path)]
   ["--expect-defect-tree"
    sha
    "expected historical defect tree SHA (refuse otherwise)"
    (set! expect-defect-tree sha)]
   ["--expect-revision-tree"
    sha
    "expected revision tree SHA (refuse otherwise)"
    (set! expect-revision-tree sha)]
   ["--repo-root" path "workspace root containing scripts/gh_helpers.py" (set! repo-root path)]
   ["--verify-replay"
    path
    "re-derive and verify an existing replay record; launch nothing"
    (set! verify-replay-file path)]
   #:args ()
   (cond
     [verify-file
      (define record (with-input-from-file verify-file (lambda () (read-json))))
      (verify-record record)
      (printf "verified ~a\n" verify-file)]
     [verify-decision-file
      (define record (with-input-from-file verify-decision-file (lambda () (read-json))))
      (verify-decision-record record)
      (printf "verified decision ~a\n" verify-decision-file)]
     [verify-replay-file
      (define record (with-input-from-file verify-replay-file (lambda () (read-json))))
      (verify-replay-record record)
      (printf "verified replay ~a\n" verify-replay-file)]
     [replay?
      (unless (and worktree
                   control-worktree
                   revision
                   out-file
                   cache-file
                   layer-worktree
                   layer-revision
                   repo-root
                   expect-defect-tree
                   expect-revision-tree)
        (raise-user-error
         'w3-dry-run
         (string-append "--replay needs --worktree, --control-worktree, --revision, "
                        "--contract-worktree, --contract-revision, --ci-cache, --repo-root, "
                        "--expect-defect-tree, --expect-revision-tree and --out")))
      (define cache (with-input-from-file cache-file (lambda () (read-json))))
      (define trees (hash-ref cache 'trees cache))
      (define record
        (run-replay-case (string->path worktree)
                         (string->path control-worktree)
                         revision
                         (selection-case case-id)
                         #:out-file (string->path out-file)
                         #:contract-worktree (string->path layer-worktree)
                         #:contract-revision layer-revision
                         #:ci-cache trees
                         #:repo-root (string->path repo-root)
                         #:expect-defect-tree expect-defect-tree
                         #:expect-revision-tree expect-revision-tree))
      (printf "wrote ~a: claim=~a required=~a attributed=~a\n"
              out-file
              (hash-ref (hash-ref record 'classification) 'claim)
              (hash-ref (hash-ref record 'escalation) 'required-count)
              (length (hash-ref (hash-ref record 'classification) 'attributed)))]
     [decide-only?
      (unless (and worktree revision out-file)
        (raise-user-error 'w3-dry-run "--decide-only needs --worktree, --revision and --out"))
      (define record
        (run-decision-case (string->path worktree)
                           revision
                           (selection-case case-id)
                           #:out-file (string->path out-file)
                           #:contract-worktree (and layer-worktree (string->path layer-worktree))
                           #:contract-revision layer-revision))
      (printf "wrote ~a: verdict=~a instruction=~a required=~a mode=~a\n"
              out-file
              (let ([d (hash-ref (hash-ref record 'contract-layer) 'decision #f)])
                (if d
                    (hash-ref d 'verdict 'unknown)
                    'no-decision))
              (layer-instruction (hash-ref record 'contract-layer))
              (length (hash-ref (hash-ref record 'contract-layer) 'required '()))
              (hash-ref (hash-ref record 'contract-layer) 'mode))]
     [else
      (unless (and worktree revision out-file cache-file)
        (raise-user-error 'w3-dry-run "--worktree, --revision, --out and --ci-cache are required"))
      (define cache (with-input-from-file cache-file (lambda () (read-json))))
      (define trees (hash-ref cache 'trees cache))
      (define record
        (run-dry-case (string->path worktree)
                      revision
                      (selection-case case-id)
                      #:ci-cache trees
                      #:out-file (string->path out-file)
                      #:contract-worktree (and layer-worktree (string->path layer-worktree))
                      #:contract-revision layer-revision))
      (printf "wrote ~a: w2=~a w3=~a mode=~a\n"
              out-file
              (hash-ref (hash-ref record 'classification) 'w2)
              (hash-ref (hash-ref record 'classification) 'w3-outcome)
              (hash-ref (hash-ref record 'classification) 'mode))])))
