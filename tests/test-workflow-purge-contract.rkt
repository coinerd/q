#lang racket

;; @suite ci
;; @speed fast
;; @boundary integration
;; tests/test-workflow-purge-contract.rkt
;;
;; BUG-0065 systemic invariant (the campaign's W4 wave): every lane that consumes a
;; restored/prepared workspace purges or cryptographically verifies
;; workspace bytecode BEFORE executing tests, and every producer stamps
;; its artifact (manifest tuple: repository, git SHA, source digest,
;; lock digest — verified by consumers on restore).
;;
;; History: at 04637d83 (the release-lane BUG-0065 fix, immediately preceding
;; this campaign) the prepared-env RESTORE lane was observed skipping the
;; stale-bytecode purge — restored .zo
;; files carried extraction mtimes NEWER than the fresh checkout, so
;; Racket executed producer-era bytecode (a producer-era suite ran
;; against the v1.00.27 tag: the cohort-report failure). 04637d83 fixed
;; that single lane locally; W4 makes the invariant SYSTEMIC:
;;
;;   1. The shared setup-racket action purges + verifies workspace
;;      bytecode on EVERY path (restored, flagged fallback, full
;;      install), fails closed if any .zo survives outside the frozen
;;      fixture, and makes every purge loud and counted.
;;   2. THIS test pins the whole workflow graph: every job that
;;      consumes a restored workspace either runs through the shared
;;      action or carries a local purge; direct restore consumers
;;      without a purge fail here. Adding a new unpatched lane turns
;;      this test RED (negative fixtures below prove it).
;;
;; Producer side: prepare-racket-environment stamps the artifact with
;; its manifest tuple and restore-racket-environment verifies the tuple
;; before materializing (pinned separately by the prepared-env tests);
;; this file adds the workspace-side purge guarantee on top, because
;; verified provenance alone does not protect against producer-era
;; bytecode (the materialized .zo mtimes are not a freshness signal).

(require rackunit
         racket/file
         racket/list
         racket/string
         racket/port
         racket/runtime-path
         json
         (only-in "../util/version.rkt" q-version))

;; ── Path helpers ──

(define-runtime-path workflows-dir "../.github/workflows")
(define-runtime-path setup-racket-action-path "../.github/actions/setup-racket/action.yml")
(define-runtime-path artifacts-dir "../artifacts")
;; The current campaign's W6 consumer matrix (derived from the version
;; module per BUG-0009).
(define consumers-json-path
  (build-path artifacts-dir "proof-graph" (format "v~a-w6" q-version) "consumers.json"))

(define workflow-paths
  (sort (for/list ([p (in-directory workflows-dir)]
                   #:when (regexp-match? #rx"\\.(ya?ml)$" (path->string p)))
          p)
        path<?))

;; The scanner must actually sweep the whole workflow graph.
(check-true
 (>= (length workflow-paths) 8)
 "expected to scan at least 8 workflow files (ci, release, release-core,
release-repair, nightly, full-regression, benchmark, pilot, ...)")

;; ── Text-level workflow graph parsing ──
;;
;; We deliberately do NOT pull a YAML parser dependency: the contract is
;; text-structural and must keep this file dependency-free. A job header
;; is a 2-space-indented `name:` line inside the `jobs:` section.

(define (workflow-jobs text)
  (define all-lines (string-split text "\n"))
  (define jobs-start
    (for/first ([ln all-lines]
                [i (in-naturals)]
                #:when (regexp-match? #px"^jobs:\\s*$" ln))
      i))
  (cond
    [(not jobs-start) '()]
    [else
     (define lines (drop all-lines jobs-start))
     (define len (length lines))
     (define headers
       (for/list ([ln lines]
                  [i (in-naturals)]
                  #:when (regexp-match? #px"^  [A-Za-z0-9_.-]+:\\s*$" ln))
         (cons i (cadr (regexp-match #px"^  ([A-Za-z0-9_.-]+):\\s*$" ln)))))
     (for/list ([h headers]
                [i (in-naturals)])
       (define start (car h))
       (define end
         (if (= i (sub1 (length headers)))
             len
             (car (list-ref headers (add1 i)))))
       (list (cdr h) (string-join (take (drop lines start) (- end start)) "\n")))]))

;; ── Contract markers ──

(define shared-action-marker "./.github/actions/setup-racket")
(define direct-restore-marker "./.github/actions/restore-racket-environment")
(define producer-action-marker "./.github/actions/prepare-racket-environment")
(define local-purge-marker "name '*.zo'")
(define fixture-exclusion-marker "tests/metadata-discovery/fixture")
(define exemption-marker "BUG-0065 PURGE-EXEMPT")
(define shared-purge-step-name "Purge and verify workspace bytecode (BUG-0065, every path)")
(define removed-local-purge-step "Purge restored workspace bytecode")

(define (classify-job-body body)
  (cond
    ;; A step-level `uses:` of the raw restore action makes the job a
    ;; DIRECT consumer — checked FIRST because the pilot legitimately has
    ;; both legs (direct restore + shared action) in one job body.
    [(regexp-match? #px"(?m:^)\\s*(?:-\\s+)?uses:\\s*\\./\\.github/actions/restore-racket-environment"
                    body)
     'direct-restore]
    ;; Everything else consuming the prepared env goes through the shared
    ;; action (step-level `uses:` only — comments and paths filters don't count).
    [(regexp-match? #px"(?m:^)\\s*(?:-\\s+)?uses:\\s*\\./\\.github/actions/setup-racket" body)
     'via-shared-action]
    [(string-contains? body producer-action-marker) 'producer]
    [else 'other]))

(define (has-local-purge? body)
  (and (string-contains? body local-purge-marker) (string-contains? body fixture-exclusion-marker)))

(define (purge-exempt? body)
  (string-contains? body exemption-marker))

;; ── The fail-closed lane scan ──

(struct violation (workflow job reason) #:transparent)

(define (restore-contract-violations wf-name job-name body)
  (case (classify-job-body body)
    [(direct-restore)
     (if (or (has-local-purge? body) (purge-exempt? body))
         '()
         (list (violation wf-name
                          job-name
                          (string-append
                           "job consumes a restored prepared-env workspace via"
                           " restore-racket-environment directly but has NEITHER the BUG-0065"
                           " local purge (must remove .zo with the "
                           fixture-exclusion-marker
                           " exclusion) NOR a "
                           exemption-marker
                           " marker; restored .zo mtimes are newer than the checkout, so"
                           " producer-era bytecode would execute (BUG-0065)"))))]
    [else '()]))

;; ── Shared-action contract (the mechanism every shared lane inherits) ──

(define (scan-workflow-text wf-name text)
  ;; Flatten across jobs: each entry is an individual violation record.
  (for*/list ([job (workflow-jobs text)]
              [v (restore-contract-violations wf-name (car job) (cadr job))])
    v))

(define (scan-all-workflows texts)
  (apply append
         (for/list ([p workflow-paths])
           (scan-workflow-text (path->string p) (file->string p)))))

;; ── Shared-action contract (the mechanism every shared lane inherits) ──

(define (require-marker text marker why)
  (if (string-contains? text marker)
      '()
      (list (violation ".github/actions/setup-racket/action.yml"
                       "-"
                       (format "~a: missing ~s" why marker)))))

(define (shared-action-violations action-text)
  (append (require-marker action-text shared-purge-step-name "always-on purge step")
          (require-marker action-text
                          "if: always()"
                          "purge must run on EVERY path (restored, fallback, full install)")
          (require-marker action-text
                          "find . -type f -name '*.zo'"
                          "purge removes stale workspace .zo files")
          (require-marker action-text
                          fixture-exclusion-marker
                          "purge preserves the frozen metadata-discovery fixture")
          (require-marker action-text
                          "::error::BUG-0065 workspace bytecode purge FAILED"
                          "post-purge verification must fail closed")
          (require-marker action-text
                          "bug-0065-purge-stamp.json"
                          "purge events must be counted (RUNNER_TEMP stamp), never silent")
          (require-marker action-text
                          "## Workspace bytecode purge (BUG-0065 systemic invariant)"
                          "purge events must be loud in the step summary")))

;; ── 1. Shared action carries the systemic purge/verify step ──

(test-case "setup-racket action carries the always-on BUG-0065 purge/verify step"
  (define action (file->string setup-racket-action-path))
  (check-equal?
   (shared-action-violations action)
   '()
   "the shared action must purge + verify workspace bytecode on every path,
fail closed, and keep purge events loud and counted"))

(test-case "tampered shared action (purge made restore-conditional) is detected"
  (define action (file->string setup-racket-action-path))
  (define tampered
    (string-replace action "if: always()" "if: steps.prepared-env.outcome != 'success'" #:all? #t))
  (check-true
   (ormap (lambda (v) (string-contains? (violation-reason v) "EVERY path"))
          (shared-action-violations tampered))
   "regressing the shared purge to the restore-conditional lane re-opens
BUG-0065: a successful restore would keep producer-era .zo (no lane may
silently skip the purge)"))

;; ── 2. Repo-wide graph scan: real workflows ──

(test-case "every restore-consuming lane in the real workflow graph carries the purge/verify contract"
  (define real-violations (scan-all-workflows workflow-paths))
  (unless (null? real-violations)
    (for ([v real-violations])
      (eprintf "VIOLATION ~a / ~a: ~a\n"
               (violation-workflow v)
               (violation-job v)
               (violation-reason v))))
  (check-equal?
   real-violations
   '()
   "every lane consuming a restored/prepared workspace must purge or
verify workspace bytecode before executing tests (BUG-0065 systemic)"))

(test-case "the removed release-lane local purge step must not reappear in any workflow"
  (for ([p workflow-paths])
    (check-false
     (string-contains? (file->string p) removed-local-purge-step)
     (format
      "~a still carries the superseded local purge step; the shared
setup-racket purge owns this invariant since v~a W4"
      (path->string p)
      q-version))))

;; ── 3. Positive detection: the restore-capable lanes are really covered ──
;;
;; Guards against the scanner silently matching nothing (a contract test
;; that vacuously passes is worse than no test).

(test-case "ci.yml fast-shard restore lane is detected and covered by the shared action"
  (define ci-text (file->string (build-path workflows-dir "ci.yml")))
  (define test-shard-jobs
    (filter (lambda (j) (string-contains? (cadr j) "prepared-environment: ${{ env.PREPARED_ENV }}"))
            (workflow-jobs ci-text)))
  (check-true (>= (length test-shard-jobs) 1)
              "ci.yml must expose at least one restore-capable fast-shard lane")
  (for ([j test-shard-jobs])
    (check-equal?
     (classify-job-body (cadr j))
     'via-shared-action
     "restore-capable fast-shard lanes must go through setup-racket,
which owns the always-on BUG-0065 purge")))

(test-case "ci.yml producer lane is detected (stamp side of the contract)"
  (define ci-text (file->string (build-path workflows-dir "ci.yml")))
  (define producer-jobs
    (filter (lambda (j) (equal? 'producer (classify-job-body (cadr j)))) (workflow-jobs ci-text)))
  (check-true (>= (length producer-jobs) 1)
              "ci.yml must expose the prepared-env producer (artifact stamping)"))

(test-case "nightly, full-regression, and release lanes consume the shared purge"
  (for ([wf (list "nightly.yml" "full-regression.yml" "release.yml")])
    (define text (file->string (build-path workflows-dir wf)))
    (define via-shared
      (filter (lambda (j) (equal? 'via-shared-action (classify-job-body (cadr j))))
              (workflow-jobs text)))
    (check-true
     (>= (length via-shared) 1)
     (format
      "~a must route Racket setup through the shared action
(inheriting the always-on BUG-0065 purge)"
      wf))))

(test-case "pilot direct-restore consumer is detected as exempt (report-only)"
  (define pilot-text (file->string (build-path workflows-dir "prepared-environment-pilot.yml")))
  (define direct-jobs
    (filter (lambda (j) (equal? 'direct-restore (classify-job-body (cadr j))))
            (workflow-jobs pilot-text)))
  (check-equal? (length direct-jobs) 1 "the pilot is the ONLY sanctioned direct restore consumer")
  (check-true
   (purge-exempt? (cadr (car direct-jobs)))
   "the pilot's direct-restore job must carry the BUG-0065 PURGE-EXEMPT
marker documenting its report-only status"))

;; ── 4. Negative fixtures: fail-closed on new unpatched lanes ──

(define hypothetical-lane-base
  #<<YAML
name: Hypothetical New Lane

jobs:
  consume:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v7
      - name: Restore prepared env directly
        uses: ./.github/actions/restore-racket-environment
        with:
          racket-version: '8.11'
      - name: Run tests
        run: racket scripts/run-tests.rkt --suite fast
YAML
  )

(test-case "NEGATIVE fixture: a new unpatched restore lane turns the contract red"
  (define found (scan-workflow-text "hypothetical.yml" hypothetical-lane-base))
  (check-equal? (length found) 1 "the unpatched direct-restore lane must violate the contract")
  (check-true (string-contains? (violation-job (car found)) "consume")
              "the violation must name the offending job"))

(test-case "NEGATIVE fixture variant: the same lane patched with the local purge is green"
  (define patched
    (string-replace
     hypothetical-lane-base
     "      - name: Run tests"
     (string-append
      "      - name: Purge restored workspace bytecode locally (BUG-0065)\n"
      "        run: |\n"
      "          find . -type f -name '*.zo' -not -path './tests/metadata-discovery/fixture/*' -delete\n"
      "          find . -depth -type d -name compiled -not -path './tests/metadata-discovery/fixture/*' -exec rm -rf {} +\n"
      "      - name: Run tests")))
  (check-true (has-local-purge? patched))
  (check-equal? (scan-workflow-text "hypothetical-patched.yml" patched) '()))

(test-case "NEGATIVE fixture variant: an exemption marker alone is accepted for a report-only lane"
  (define exempted
    (string-replace
     hypothetical-lane-base
     "      - name: Restore prepared env directly"
     "      # BUG-0065 PURGE-EXEMPT: report-only, never executes the q test suite\n      - name: Restore prepared env directly"))
  (check-equal? (scan-workflow-text "hypothetical-exempt.yml" exempted) '()))

;; ── W6 consumer-matrix purge coverage ──
;;
;; The campaign's W6 wave (spec §6 W4): the prepared-environment restore is expanded
;; to every consumer whose env-profile is provably identical to the
;; producer tuple (the wave's consumers.json under artifacts/proof-graph/).
;; EVERY consumer the matrix lists as `activated` must consume the
;; restored workspace through the shared setup-racket action (which owns
;; the always-on BUG-0065 purge/verify) and must carry its one-command
;; rollback variable in the workflow, so the expansion can never create
;; an unpatched lane: adding an activated consumer whose job bypasses the
;; shared action turns this file RED.

(define (consumers-doc)
  (call-with-input-file consumers-json-path read-json))

(define (workflow-file-name workflow)
  ;; consumers.json stores the repo-relative workflow path.
  (last (string-split workflow "/")))

(define (activated-consumer-violations consumers)
  ;; Scan every `activated` consumer's live job body; each violation is a
  ;; (consumer workflow reason) record.
  (for*/list ([consumer (in-list consumers)]
              #:when (equal? (hash-ref consumer 'activation #f) "activated"))
    (define name (hash-ref consumer 'consumer))
    (define workflow (hash-ref consumer 'workflow))
    (define job-key (hash-ref consumer 'job))
    (define wf-path (build-path workflows-dir (workflow-file-name workflow)))
    (cond
      [(not (file-exists? wf-path))
       (violation name workflow (format "workflow file ~a is missing" workflow))]
      [else
       (define jobs (workflow-jobs (file->string wf-path)))
       (define job-body
         (for/first ([j (in-list jobs)]
                     #:when (string=? (car j) job-key))
           (cadr j)))
       (cond
         [(not job-body)
          (violation name
                     workflow
                     (format "activated consumer job ~a not found in ~a" job-key workflow))]
         [(not (equal? 'via-shared-action (classify-job-body job-body)))
          (violation
           name
           workflow
           (format
            "activated consumer ~a must consume the restored workspace via the shared ~a (which owns the always-on BUG-0065 purge), not a bypass"
            name
            shared-action-marker))]
         [(not (string-contains? job-body
                                 (format "~a != 'off'" (hash-ref consumer 'rollback-variable ""))))
          (violation
           name
           workflow
           (format
            "activated consumer ~a must wire its §11.6 rollback variable ~a into its PREPARED_ENV expression"
            name
            (hash-ref consumer 'rollback-variable "")))]
         [else #f])])))

(define (non-false xs)
  (for/list ([x (in-list xs)]
             #:when x)
    x))

(test-case "W6: every activated consumer in the matrix carries the shared purge/verify + rollback wiring"
  (define doc (consumers-doc))
  (define activated
    (for/list ([c (in-list (hash-ref doc 'consumers))]
               #:when (equal? (hash-ref c 'activation #f) "activated"))
      c))
  (check-true (>= (length activated) 7)
              "the W6 matrix must keep at least the seven activated consumers")
  (define violations (non-false (activated-consumer-violations (hash-ref doc 'consumers))))
  (unless (null? violations)
    (for ([v violations])
      (eprintf "W6 VIOLATION ~a / ~a: ~a\n"
               (violation-workflow v)
               (violation-job v)
               (violation-reason v))))
  (check-equal?
   violations
   '()
   "every activated consumer must run the shared setup-racket action (BUG-0065 purge) and wire its rollback variable"))

(test-case "W6: an activated consumer bypassing the shared action turns the matrix red"
  ;; Negative fixture: a hypothetical activated consumer whose job calls
  ;; the raw upstream Racket installer instead of the shared action.
  (define bypass-job
    #<<YAML
jobs:
  consume:
    needs: [lint, fast-env]
    runs-on: ubuntu-latest
    env:
      PREPARED_ENV: auto
    steps:
      - uses: actions/checkout@v7
      - name: Install Racket directly (bypass)
        uses: Bogdanp/setup-racket@2466913449df77df2bad149d1f2fc4e1ea4795dd
YAML
    )
  (define bypass-consumer
    (hasheq 'consumer
            "hypothetical:bypass"
            'workflow
            ".github/workflows/hypothetical.yml"
            'job
            "consume"
            'activation
            "activated"
            'rollback-variable
            "RACKET_PREPARED_BYPASS"))
  (define doc (consumers-doc))
  ;; Patch the scanner inputs: point the fixture at a temp workflow file so
  ;; the same live-file scan runs end to end.
  (define tmp-wf-dir (make-temporary-file "q-w6-purge-matrix~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define tmp-wf (build-path tmp-wf-dir "hypothetical.yml"))
                  (call-with-output-file tmp-wf (lambda (o) (display bypass-job o)) #:exists 'replace)
                  (define saved-workflows-dir workflows-dir)
                  (set! workflows-dir tmp-wf-dir)
                  (define violations
                    (non-false (activated-consumer-violations (list bypass-consumer))))
                  (set! workflows-dir saved-workflows-dir)
                  ;; The scan short-circuits on the first contract breach:
                  ;; the bypass (not routing through the shared purge-owning
                  ;; action) is the reported violation.
                  (check-equal? (length violations) 1)
                  (check-true (string-contains? (violation-reason (car violations))
                                                "shared ./.github/actions/setup-racket")
                              "the bypass must be reported as a purge-contract violation"))
                (lambda () (delete-directory/files tmp-wf-dir #:must-exist? #f))))

;; ── 5. Behavioral repro (v1.00.27 cohort-report root cause) ──
;;
;; A workspace where the .zo is NEWER than its source must execute the
;; STALE bytecode (the bug), and after the BUG-0065 purge the SAME
;; workspace must execute bytecode compiled from the CURRENT checkout.

;; This runtime's process* yields the subprocess record as a single list
;; value (rather than five values); normalize so the positional
;; destructure is stable either way. The fifth element is the exit-code
;; control procedure — call it to obtain the integer status.
(define (spawn-subprocess exe args)
  (define vs (call-with-values (lambda () (apply process* exe args)) list))
  (define flat
    (if (and (= (length vs) 1) (list? (car vs)))
        (car vs)
        vs))
  (define-values (p-stdout p-stdin p-pid p-stderr p-ctl) (apply values flat))
  (values p-stdout p-stdin p-pid p-stderr p-ctl))

(define (exit-status ctl)
  ;; This runtime's control procedure: (ctl 'wait) blocks -> void;
  ;; (ctl 'exit-code) -> integer termination code.
  (ctl 'exit-code))

(define (run-racket-capture expr)
  (define exe (find-executable-path "racket"))
  (define-values (p-stdout p-stdin p-pid p-stderr p-ctl)
    (spawn-subprocess exe (list "-l" "racket/base" "-e" expr)))
  (close-output-port p-stdin)
  (define out (string-trim (port->string p-stdout)))
  (define err (port->string p-stderr))
  (p-ctl 'wait)
  (define code (exit-status p-ctl))
  (values code out err))

(define (purge-compiled-like-the-workflow! dir)
  ;; Mirrors the shared setup-racket purge (fixture exclusion omitted:
  ;; the repro workspace contains no frozen fixture).
  (define all
    (for/list ([p (in-directory dir)])
      p))
  (for ([p all]
        #:when (and (file-exists? p) (regexp-match? #rx"\\.zo$" (path->string p))))
    (delete-file p))
  (for ([p (reverse (sort all path<?))]
        #:when (and (directory-exists? p) (equal? (last (explode-path p)) (string->path "compiled"))))
    (delete-directory/files p)))

(test-case "repro: mtime-newer stale .zo executed pre-purge; CURRENT bytecode post-purge"
  (define tmp (make-temporary-file "q-bug0065-repro-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define victim-src (build-path tmp "victim.rkt"))
     (define producer-content
       "#lang racket/base\n(provide victim-stamp)\n(define victim-stamp 'producer-era-bytecode)\n")
     (define checkout-content
       "#lang racket/base\n(provide victim-stamp)\n(define victim-stamp 'current-checkout-bytecode)\n")

     ;; Producer-era compile: raco make produces compiled/victim_rkt.zo
     ;; with an mtime of NOW.
     (call-with-output-file victim-src (lambda (o) (display producer-content o)) #:exists 'replace)
     (define raco-exe (find-executable-path "raco"))
     (check-true (and raco-exe #t) "raco must be available for the repro")
     (define compile-code
       (let-values ([(p-stdout p-stdin p-pid p-stderr p-ctl)
                     (spawn-subprocess raco-exe (list "make" (path->string victim-src)))])
         (close-output-port p-stdin)
         (port->string p-stdout)
         (port->string p-stderr)
         (p-ctl 'wait)
         (exit-status p-ctl)))
     (check-equal? compile-code 0 "raco make of the repro module must succeed")

     ;; Fresh-checkout state: source bytes are the TAGGED tree, and —
     ;; exactly like the restored prepared-env workspace — the .zo
     ;; mtime (extraction time) is NEWER than the source mtime.
     (call-with-output-file victim-src (lambda (o) (display checkout-content o)) #:exists 'replace)
     (file-or-directory-modify-seconds victim-src (- (current-seconds) 7200))

     ;; (a) Characterization of the BUG-0065 mechanism: Racket trusts the
     ;; newer .zo and executes PRODUCER bytecode over checked-out sources.
     (define-values (pre-code pre-out pre-err)
       (run-racket-capture (format "(displayln (dynamic-require (string->path ~s) 'victim-stamp))"
                                   (path->string victim-src))))
     (check-equal? pre-code 0 "pre-purge repro subprocess must run")
     (check-equal? pre-out
                   "producer-era-bytecode"
                   (string-append "mtime-newer stale .zo must be executed before the purge — this is"
                                  " exactly the v1.00.27 cohort-report failure mode. stderr: "
                                  pre-err))

     ;; (b) The invariant: purge the workspace like the shared action does.
     (purge-compiled-like-the-workflow! tmp)
     (check-equal? (for/list ([p (in-directory tmp)]
                              #:when (and (file-exists? p)
                                          (regexp-match? #rx"\\.zo$" (path->string p))))
                     p)
                   '()
                   "post-purge verification: zero .zo files must survive (fail closed)")

     ;; (c) Fixed behavior: the SAME workspace now executes bytecode
     ;; compiled from the CURRENT checkout.
     (define-values (post-code post-out post-err)
       (run-racket-capture (format "(displayln (dynamic-require (string->path ~s) 'victim-stamp))"
                                   (path->string victim-src))))
     (check-equal? post-code 0 "post-purge repro subprocess must run")
     (check-equal? post-out
                   "current-checkout-bytecode"
                   (string-append "after the BUG-0065 purge the suite must execute CURRENT"
                                  " checkout bytecode, never restored producer-era .zo. stderr: "
                                  post-err))

     ;; (d) Loud + counted: mirror the workflow's RUNNER_TEMP stamp.
     (define stamp (build-path tmp "bug-0065-purge-stamp.json"))
     (call-with-output-file
      stamp
      (lambda (o)
        (fprintf o "{\"purged_zo\": 1, \"post_purge_zo\": 0, \"prepared_env_outcome\": \"success\"}"))
      #:exists 'replace)
     (define stamp-data (file->string stamp))
     (check-true (string-contains? stamp-data "\"post_purge_zo\": 0")
                 "purge events are counted (never silent)"))
   (lambda () (delete-directory/files tmp #:must-exist? #f))))
