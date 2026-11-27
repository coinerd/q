#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
;; @timeout 240 ;; CL5 P2-D: concurrency-calibrated (see docs/reports/CL5-P2D-TIMEOUT-MARGIN.json)

;; Contract for the q lane surface (`./qt default`, `./qt release`,
;; `full|default --changed [BASE]`):
;;   - `default` is the SAME selection machinery as `full`, filtered by the
;;     repository's own metadata classifiers: slow files, mutating files (a
;;     declared mutation other than none), any declared isolation other than
;;     none, and files whose declared requirements are in the classifier's
;;     excluded token set are excluded; never a hand-maintained list;
;;   - the lane is a performance/feedback gate for the fast tier, not a
;;     correctness claim about the complete suite;
;;   - `release` is `full` plus the obligation list, printed whether or not the
;;     suite passed, and a non-zero exit while any obligation is unverified: it
;;     never prints a release pass line it cannot support;
;;   - `--changed` maps the working-tree diff through the @covers manifest and
;;     the existing impact machinery, INCLUDES unattributable test files (fail
;;     open) and fails closed on an uncomputable or empty diff;
;;   - `--explain` states the lane filter and the changed-file mapping;
;;   - plain `./qt full` stays byte-identical (plan, preamble and pass line).
;; Fixture roots keep every case fast; the real suite is never run here.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../scripts/testing/full-plan.rkt"
         "../scripts/testing/quick-runner.rkt"
         ;; The lane filter's predicates live next to the other metadata
         ;; classifiers; the tests assert them directly too (not only through
         ;; the lane's reason list).
         "../scripts/run-tests/classify.rkt")

(define-runtime-path qt "../qt")
(define-runtime-path root "..")

;; Read both pipes while the child runs; neither pipe may block the other.
(define (invoke-command timeout cwd executable . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f executable args))
                    (set! proc child)
                    (close-output-port in)
                    (define stdout #f)
                    (define stderr #f)
                    (define out-thread (thread (lambda () (set! stdout (port->string out)))))
                    (define err-thread (thread (lambda () (set! stderr (port->string err)))))
                    (define done
                      (thread (lambda ()
                                (subprocess-wait child)
                                (thread-wait out-thread)
                                (thread-wait err-thread))))
                    (unless (sync/timeout timeout done)
                      (error 'invoke-command "subprocess timed out after ~a seconds" timeout))
                    (values (subprocess-status child) stdout stderr)))
                (lambda ()
                  (when proc
                    (when (eq? (subprocess-status proc) 'running)
                      (subprocess-kill proc #t))
                    (subprocess-wait proc))
                  (custodian-shutdown-all cust))))

(define (invoke . args)
  (apply invoke-command 300 (simplify-path root) (find-executable-path "racket") qt args))

;; Exactly `./qt <args> | head -N`: consume N lines, then CLOSE the read end of
;; qt's stdout so every later write hits EPIPE. qt stays our own child, so we
;; assert ITS status and ITS stderr (a pipeline would report `head`'s status).
(define (invoke-with-short-stdout lines timeout cwd . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind
   void
   (lambda ()
     (parameterize ([current-custodian cust]
                    [current-directory cwd]
                    [subprocess-group-enabled #t])
       (define-values (child out in err)
         (apply subprocess #f #f #f (find-executable-path "racket") qt args))
       (set! proc child)
       (close-output-port in)
       (define head-lines
         (for/list ([_ (in-range lines)])
           (read-line out 'any)))
       ;; The consumer is done: no reader remains on qt's stdout.
       (close-input-port out)
       (define stderr #f)
       (define err-thread (thread (lambda () (set! stderr (port->string err)))))
       (define done
         (thread (lambda ()
                   (subprocess-wait child)
                   (thread-wait err-thread))))
       (unless (sync/timeout timeout done)
         (error 'invoke-with-short-stdout "subprocess timed out after ~a seconds" timeout))
       (values (subprocess-status child) head-lines stderr)))
   (lambda ()
     (when proc
       (when (eq? (subprocess-status proc) 'running)
         (subprocess-kill proc #t))
       (subprocess-wait proc))
     (custodian-shutdown-all cust))))

(define (write-file path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

(define (join-lines lines)
  (string-join lines "\n"))

;; A root with one fast file per excluded `@requires` token, generated from the
;; classifier's own vocabulary (`requires-excluded-tokens`) plus one plain file
;; so the lane is never empty. The fixture metadata is data, never prose.
(define (with-requires-token-root thunk)
  (define dir (make-temporary-file "qt-lanes-requires-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory (build-path dir "tests"))
                  (write-file (build-path dir "tests" "plain.rkt")
                              (join-lines '("#lang racket/base" ";; @speed fast"
                                                                "(require rackunit)"
                                                                "(module+ test (check-true #t))")))
                  (for ([tok (in-list requires-excluded-tokens)])
                    (write-file (build-path dir "tests" (format "req-~a.rkt" tok))
                                (join-lines (list "#lang racket/base"
                                                  ";; @speed fast"
                                                  (format ";; @requires ~a" tok)
                                                  "(require rackunit)"
                                                  "(module+ test (check-true #t))"))))
                  (thunk dir))
                (lambda () (delete-directory/files dir))))

;; A root with one selected test that FAILS after emitting a large stderr block,
;; so the CLI has a long failure write between its preamble and the release
;; obligation lines. The closed-stdout regression reads only the preamble and
;; closes, which makes the obligation lines the writes that hit the dead pipe.
(define (with-failing-lane-root thunk)
  (define dir (make-temporary-file "qt-lanes-red-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory (build-path dir "tests"))
     (write-file (build-path dir "tests" "fail.rkt")
                 (join-lines '("#lang racket/base"
                               ";; @speed fast"
                               "(require rackunit)"
                               "(module+ test"
                               "  (display (make-string 200000 #\\e) (current-error-port))"
                               "  (check-true #f))")))
     (thunk dir))
   (lambda () (delete-directory/files dir))))

;; One fixture repository covering every attribution outcome:
;;   tests/a.rkt    @covers runtime/foo.rkt  -> affected by a foo change
;;   tests/b.rkt    @covers runtime/bar.rkt  -> unrelated to a foo change
;;   tests/c.rkt    no @covers               -> unattributable (included)
;;   tests/slow.rkt @speed slow              -> excluded by the default lane
;;   tests/net.rkt  @requires network        -> excluded by the default lane
;;   tests/mut.rkt  @mutates home            -> excluded by the default lane
;;   tests/iso.rkt  @isolation sandboxed-home -> excluded by the default lane
;;   runtime/baz.rkt                        -> mapped by nothing (escalation)
;; slow.rkt creates tests/slow-ran when it is executed, so an execution test can
;; prove the lane filter actually skipped it instead of merely claiming to.
(define (with-lane-root thunk)
  (define dir (make-temporary-file "qt-lanes-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory (build-path dir "tests"))
     (make-directory (build-path dir "runtime"))
     (define (put rel text)
       (write-file (build-path dir rel) text))
     (put "runtime/foo.rkt" "#lang racket/base\n(provide foo)\n(define (foo) 1)\n")
     (put "runtime/bar.rkt" "#lang racket/base\n(provide bar)\n(define (bar) 2)\n")
     (put "runtime/baz.rkt" "#lang racket/base\n(provide baz)\n(define (baz) 3)\n")
     (put "tests/a.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            ";; @covers runtime/foo.rkt"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     (put "tests/b.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            ";; @covers runtime/bar.rkt"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     (put "tests/c.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     (put "tests/slow.rkt"
          (join-lines '("#lang racket/base" ";; @speed slow"
                                            "(call-with-output-file \"slow-ran\" void)"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     (put "tests/net.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            ";; @requires network"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     ;; A declared @requires the local profile cannot satisfy (provider-key):
     ;; the union SELECTS it, the plan marks it unavailable-here, and the
     ;; execution tests prove it never runs (the marker file never appears).
     (put "tests/keys.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            ";; @requires provider-key"
                                            "(call-with-output-file \"keys-ran\" void)"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     ;; A declared mutation that no name pattern would catch, and a declared
     ;; isolation value (`sandboxed-home`) outside the schema's canonical trio.
     (put "tests/mut.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            ";; @mutates home"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     (put "tests/iso.rkt"
          (join-lines '("#lang racket/base" ";; @speed fast"
                                            ";; @isolation sandboxed-home"
                                            "(require rackunit)"
                                            "(module+ test (check-true #t))")))
     ;; Keep the literal JSON on one line regardless of how the join falls.
     (write-file (build-path dir "tests" ".coverage-manifest.json")
                 (string-append
                  "{\"entries\":[{\"covers\":[\"runtime/foo.rkt\"],\"source\":\"metadata\","
                  "\"test\":\"tests/a.rkt\"},{\"covers\":[\"runtime/bar.rkt\"],"
                  "\"source\":\"metadata\",\"test\":\"tests/b.rkt\"}]}"))
     (thunk dir))
   (lambda () (delete-directory/files dir))))

;; A fixture root whose `release --explain` plan is deliberately larger than a
;; pipe buffer: 280 fast files with long names. The closed-stdout test consumes
;; a few lines then closes, so the child still has plan bytes to write (EPIPE
;; deterministically) instead of walking the repository's ~1,500 test files.
(define long-plan-fast-count 280)

(define (with-long-plan-root thunk)
  (define dir (make-temporary-file "qt-lanes-long-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory (build-path dir "tests"))
     (define (put i)
       (write-file (build-path dir "tests" (format "test-fast-~a-~a.rkt" i (make-string 230 #\x)))
                   (join-lines '("#lang racket/base" ";; @speed fast"
                                                     "(require rackunit)"
                                                     "(module+ test (check-true #t))"))))
     (for ([i (in-range long-plan-fast-count)])
       (put i))
     (thunk dir))
   (lambda () (delete-directory/files dir))))

;; A small root with one file per excluded `@requires` token is generated from
;; the classifier's own vocabulary (see `with-requires-token-root` above); the
;; old 21-slow-file cap fixture died with the fast/PR exclusion filter.

(define (git-available?)
  (and (find-executable-path "git") #t))

(define (git-in dir . args)
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port (open-output-string)])
    (apply system* (find-executable-path "git") "-C" (path->string dir) args)))

(module+ test
  ;; --- lane verdict (pure) -----------------------------------------------
  (test-case "lane-verdict maps evidence to ok/invalid/surface/unverified per lane"
    (for ([lane (in-list '(full default release))])
      (check-eq? (lane-verdict lane #t '() '()) 'ok (format "~a: clean run is ok" lane))
      (check-eq? (lane-verdict lane #f '() '()) 'invalid (format "~a: invalid evidence" lane))
      (check-eq? (lane-verdict lane #f '("info.rkt") '())
                 'invalid
                 (format "~a: invalid evidence outranks surface" lane))
      (check-eq? (lane-verdict lane #t '("info.rkt") '())
                 'surface
                 (format "~a: a changed managed surface is red" lane))
      ;; A caller that passes obligations to a non-release lane fails closed.
      (unless (eq? lane 'release)
        (check-eq? (lane-verdict lane #t '() (list (release-obligation 'x "x" 'local 'unavailable)))
                   'invalid
                   (format "~a: obligations on a non-release lane" lane)))
      (check-eq?
       (lane-verdict lane #t '("README.md") (list (release-obligation 'x "x" 'local 'unavailable)))
       'surface
       (format "~a: a changed surface outranks an obligation" lane)))
    ;; The release lane is the only lane with obligations: an unverified
    ;; obligation is red, an empty blocking list is the only path to a pass.
    (check-eq? (lane-verdict 'release #t '() (list (release-obligation 'x "x" 'local 'unavailable)))
               'unverified)
    (check-eq? (lane-verdict 'release #t '() '()) 'ok "release with empty blocking is ok")
    ;; `blocking` is the already-filtered unverified list, so a verified
    ;; obligation filters away and cannot block the pass line either.
    (check-eq? (lane-verdict 'release
                             #t
                             '()
                             (release-blocking (list (release-obligation 'x "x" 'local 'verified))))
               'ok)
    (check-equal? (map release-obligation-slug (release-obligations))
                  '(governance-parity ci-verification branch-protection release-provenance)
                  "obligation ids are STABLE SLUGS, not positional not-verified-N/not-provided-N")
    (check-eq?
     (for/sum ([o (in-list (release-obligations))]
               #:when (string-contains? (release-obligation-text o) "macOS"))
              1)
     0
     "the macOS/platform claim is not a candidate obligation (CI test-platform is unchanged)")
    (check-eq? (lane-verdict 'release #t '() (release-blocking (release-obligations)))
               'unverified
               "the real obligation list blocks every release pass"))
  ;; --- lane table shape (final plan §6 E1) --------------------------------
  (test-case "lane table shape: suite sets with a one-element compatibility accessor"
    ;; A qt-lane carries a SUITE SET. Every pre-E1 lane keeps exactly one suite
    ;; and `qt-lane-suite` (the compatibility accessor) returns that set — a
    ;; ONE-ELEMENT list for each legacy lane — so existing callers keep working.
    (for ([lane (in-list qt-lanes)])
      (define name (qt-lane-name lane))
      (define suites (qt-lane-suites lane))
      (check-true (and (pair? suites) (andmap symbol? suites))
                  (format "~a: suites is a non-empty list of symbols" name))
      (check-equal? suites (remove-duplicates suites) (format "~a: suites are distinct" name))
      (check-equal? (qt-lane-suite lane)
                    suites
                    (format "~a: the compat accessor returns the suite set" name))
      (unless (eq? name 'required-offline)
        (check-equal? (length suites) 1 (format "~a: legacy lanes stay single-suite" name))))
    (check-equal? (qt-lane-suite (qt-lane-ref 'fast))
                  '(fast)
                  "the single-suite accessor stays a one-element list")
    (check-equal? (qt-lane-suite (qt-lane-ref 'workflows))
                  '(workflows)
                  "the workflows lane keeps its single suite"))
  (test-case "required-offline drift pin: the lane's suites are exactly the required CI test jobs' lanes"
    ;; Final plan §6 E1: the mapping is derived from the lane table (never from
    ;; parsing YAML), and a PINNING TEST asserts that the `required-offline`
    ;; suite list equals the set of lanes named by the required CI test jobs —
    ;; so claim-coverage drift fails in CI instead of shipping silently.
    (define policy-path (build-path root "scripts" "required-pr-checks.policy"))
    (define policy-contexts (call-with-input-file policy-path read))
    ;; Required contexts that do not run tests: aggregation and static gates.
    (define non-producer-contexts
      '("lint" "lint-quality" "release-dry-run" "workflows-aggregate" "test-aggregate"))
    ;; The test producers this table maps to lanes (required-ci-producers).
    (define producer-contexts (append-map required-producer-contexts required-ci-producers))
    (check-equal? (length policy-contexts) 13 "all 13 required contexts stay protected")
    (check-equal? (length producer-contexts) 8)
    ;; Every producer context the table names must be a REQUIRED context: a
    ;; rename or removal of a required CI test job fails HERE.
    (for ([ctx (in-list producer-contexts)])
      (check-true (and (member ctx policy-contexts) #t)
                  (format "mapped context ~a must exist in scripts/required-pr-checks.policy" ctx)))
    ;; Every required context must be accounted for: a NEW or RENAMED required
    ;; context must be classified here in the same commit (drift fails closed).
    (for ([ctx (in-list policy-contexts)])
      (check-true
       (and (or (member ctx non-producer-contexts) (member ctx producer-contexts)) #t)
       (format "required context ~a is neither a known non-producer nor a mapped test producer" ctx)))
    ;; The lane's suite list is EXACTLY the union of the producers' lanes'
    ;; suites — in the final plan §6's own order.
    (define producer-suites
      (remove-duplicates
       (append-map (lambda (producer)
                     (qt-lane-suites (qt-lane-ref (required-producer-lane producer))))
                   required-ci-producers)))
    (check-equal? (qt-lane-suites (qt-lane-ref 'required-offline)) producer-suites)
    (check-equal? (qt-lane-suites (qt-lane-ref 'required-offline)) required-offline-suites)
    ;; Each member lane keeps its OWN timeout/shard policy (plan §6): CI's
    ;; sharding and timeout policy for those lanes is untouched by E1.
    (check-false (qt-lane-timeout (qt-lane-ref 'fast)))
    (check-equal? (qt-lane-timeout (qt-lane-ref 'workflows)) 90)
    (check-equal? (qt-lane-shard-total (qt-lane-ref 'workflows)) 2)
    (check-equal? (qt-lane-timeout (qt-lane-ref 'tui)) 60))
  ;; --- default lane (required-offline, final plan §6 E1) ------------------
  (test-case "default lane is the required-offline union: mandatory slow/requires owners INCLUDED"
    (with-lane-root
     (lambda (dir)
       (define default-rels (lane-relative 'default dir))
       (define full-rels (lane-relative 'full dir))
       ;; The union of the required CI suites: every fast fixture joins, including
       ;; the network/mutation/isolation ones a filter used to drop. Mandatory
       ;; slow owners reach the lane THROUGH THEIR SUITE (the security lane runs
       ;; slow security files); a file in NO required suite (tests/slow.rkt) is
       ;; not the offline PR lane.
       (check-equal? (sort default-rels string<?)
                     '("tests/a.rkt" "tests/b.rkt"
                                     "tests/c.rkt"
                                     "tests/iso.rkt"
                                     "tests/keys.rkt"
                                     "tests/mut.rkt"
                                     "tests/net.rkt"))
       (check-equal? (sort full-rels string<?)
                     '("tests/a.rkt" "tests/b.rkt"
                                     "tests/c.rkt"
                                     "tests/iso.rkt"
                                     "tests/keys.rkt"
                                     "tests/mut.rkt"
                                     "tests/net.rkt"
                                     "tests/slow.rkt"))
       (check-false (member "tests/slow.rkt" default-rels)
                    "a file in no required CI suite is not the offline PR lane")
       ;; The classifier predicates still fire on the fixtures (the vocabulary
       ;; is the metadata's) — the LANE just no longer excludes for them.
       (check-true (requires-excluded? (path->string (build-path dir "tests" "net.rkt"))))
       (check-true (declared-mutation? (path->string (build-path dir "tests" "mut.rkt"))))
       (check-true (isolation-excluded? (path->string (build-path dir "tests" "iso.rkt"))))
       ;; The plan partitions the union into executable evidence and the
       ;; unavailable-here list: keys.rkt declares @requires provider-key, which
       ;; the local profile policy declares unavailable. Listed, not executed.
       (define plan (resolve-lane-plan 'default dir))
       (check-equal? (length (lane-plan-files plan)) 6 "runnable members only")
       (check-equal? (length (lane-plan-unavailable plan)) 1)
       (define u (car (lane-plan-unavailable plan)))
       (check-equal? (unavailable-here-file u) "tests/keys.rkt")
       (check-equal? (unavailable-here-requirements u) '("provider-key"))
       (for ([f (in-list (lane-plan-files plan))])
         (check-false (string-suffix? "keys.rkt" f)
                      "the unavailable-here member is not part of the executable evidence")))))
  (test-case "requires vocabulary is pinned independently of the filter"
    ;; A literal list: deleting a token from classify.rkt must fail HERE, not
    ;; merely shrink the generated fixture set below.
    (check-equal? requires-excluded-tokens '("network" "terminal" "browser" "provider-key")))
  (test-case "every classifier requires token joins the union; only the unsatisfiable one is unavailable"
    ;; Generated from `requires-excluded-tokens`, so each token in the
    ;; repository vocabulary gets a fixture and the list cannot drift from the
    ;; classifier. The union SELECTS every one of them; the plan marks a member
    ;; unavailable-here exactly when this host cannot satisfy it.
    (with-requires-token-root
     (lambda (dir)
       (define rels (lane-relative 'default dir))
       (check-true (and (member "tests/plain.rkt" rels) #t))
       (define plan (resolve-lane-plan 'default dir))
       (define unavailable-rels (map unavailable-here-file (lane-plan-unavailable plan)))
       (for ([tok (in-list requires-excluded-tokens)])
         (define rel (format "tests/req-~a.rkt" tok))
         (define f (path->string (build-path dir "tests" (format "req-~a.rkt" tok))))
         (check-true (requires-excluded? f) (format "the classifier still flags ~a" tok))
         (check-true (and (member rel rels) #t) (format "~a joins the required-offline union" tok))
         (if (host-capability-available? tok)
             (check-false (member rel unavailable-rels)
                          (format "~a runs here (never silently listed)" tok))
             (check-true (and (member rel unavailable-rels) #t)
                         (format "~a is unavailable-here (listed, never verified)" tok))))
       (check-false (member "tests/plain.rkt" unavailable-rels)
                    "a file with no declared @requires never blocks"))))
  (test-case "default explain prints the claim -> lane -> suite mapping"
    ;; Every required CI test job is named with the lane (and suite) it selects;
    ;; the required-offline lane names its union.
    (with-lane-root
     (lambda (dir)
       (define lines (plan-explain-lines (resolve-lane-plan 'default dir)))
       (define text (join-lines lines))
       (check-regexp-match
        #rx"claim mapping for \\./qt default \\(required CI test job -> lane -> suite\\):"
        text)
       (for ([producer (in-list required-ci-producers)])
         (define needle
           (format "~a -> lane ~a -> suite"
                   (string-join (required-producer-contexts producer) ", ")
                   (required-producer-lane producer)))
         (check-true (string-contains? text needle) (format "the mapping must name ~a" needle)))
       (check-true
        (string-contains?
         text
         "required-offline (this command) -> union of those suites: fast, security, workflows, smoke, platform")
        "the lane's own union line is part of the mapping"))))
  (test-case "default explain states the selection, capabilities and unavailable-here"
    (with-lane-root
     (lambda (dir)
       (define lines (plan-explain-lines (resolve-lane-plan 'default dir)))
       (define text (join-lines lines))
       (check-regexp-match
        #rx"default: offline PR verification \\(required-offline lane\\) over the complete available Racket suite"
        text)
       (check-regexp-match
        #rx"label: offline PR verification only; platform and GitHub-hosted claims remain with CI"
        text)
       (check-regexp-match #rx"lane selection: required-offline" text)
       ;; selection = the executable evidence; the union count is stated separately
       (check-regexp-match #rx"selection: 6 files" text)
       (check-regexp-match #rx"union: 7 file\\(s\\)" text)
       (for ([needle (in-list (list "terminal: available"
                                    "browser: available"
                                    "network: available"
                                    "provider-key: NOT available locally"
                                    "git: available"))])
         (check-true (string-contains? text needle) (format "capabilities must state ~a" needle)))
       (check-regexp-match #rx"unavailable-here: 1 required-offline member" text)
       (check-true (for/or ([line (in-list lines)])
                     (string-prefix? line
                                     "  unavailable-here: tests/keys.rkt (@requires provider-key)"))
                   "the unavailable-here member is NAMED with its unsatisfied requirement")
       (check-regexp-match #rx"CI stays authoritative" text)
       (check-regexp-match #rx"NOT VERIFIED by \\./qt default:" text)
       (check-regexp-match #rx"NOT PROVIDED by \\./qt default" text)
       (check-equal? (plan-explain-lines (resolve-lane-plan 'default dir))
                     lines
                     "the default plan must be deterministic")
       (check-false (regexp-match? #rx"default passed:" text) "explain never executes")
       (check-false (file-exists? (build-path dir "tests" "keys-ran"))
                    "explain must not execute selected tests")
       (check-false (file-exists? (build-path dir "tests" "slow-ran"))
                    "explain must not execute selected tests"))))
  (test-case "unavailable-here members are listed for CI, never silently dropped"
    ;; The pure partition and the capability model, pinned directly: the local
    ;; profile policy cannot satisfy provider-key, an unknown token is never
    ;; claimed, and the partition names the file with its requirements.
    (with-lane-root (lambda (dir)
                      (define members (unavailable-here-members dir (lane-relative 'default dir)))
                      (check-equal? (map unavailable-here-file members) '("tests/keys.rkt"))
                      (check-equal? (unavailable-here-requirements (car members)) '("provider-key"))
                      (check-false (host-capability-available? "provider-key"))
                      (check-true (host-capability-available? "network"))
                      (check-true (host-capability-available? "terminal"))
                      (check-false (host-capability-available? "no-such-token")
                                   "an unknown token is never claimed (conservative, listed)"))))
  (test-case "default runs exactly the runnable selection; the recorded run drives the verdict"
    (if (full-execution-supported?)
        (with-lane-root
         (lambda (dir)
           (define plan (resolve-lane-plan 'default dir))
           (define run (run-plan plan 2))
           ;; ONE recorded run drives both the evidence contract and the lane
           ;; verdict (W4b absorbed the separate lane-verdict recorded-run
           ;; execution of the same fixture selection into this case).
           (check-eq?
            (lane-verdict 'default (full-run-valid? run) (full-run-surface-violations run) '())
            'ok)
           (check-true (full-run-valid? run))
           (check-equal? (full-run-files run) (lane-plan-files plan))
           (check-equal? (length (full-run-results run)) 6)
           (check-false (file-exists? (build-path dir "tests" "slow-ran"))
                        "a file in no required suite must not have been executed")
           (check-false (file-exists? (build-path dir "tests" "keys-ran"))
                        "the unavailable-here member must not have been executed")
           (check-regexp-match default-pass-pattern
                               (default-pass-line run (length (lane-plan-unavailable plan))))
           ;; Per-file timeouts survive the union selection.
           (check-equal? (hash-count (full-timeouts (lane-plan-files plan)))
                         (length (lane-plan-files plan)))))
        (displayln "SKIP default lane execution: Linux /proc or setsid unavailable")))
  (test-case "default CLI runs the runnable selection, labels the result, lists unavailable-here"
    (if (full-execution-supported?)
        (with-lane-root
         (lambda (dir)
           ;; Explain first: the plan's own selection count is the contract the
           ;; execution must satisfy, so a preamble-only run cannot pass.
           (define-values (explain-status explain-out explain-err)
             (invoke-command 300 dir (find-executable-path "racket") qt "default" "--explain"))
           (check-equal? explain-status 0 (string-append explain-out explain-err))
           (define selection-line
             (for/first ([line (in-list (string-split explain-out "\n"))]
                         #:when (string-prefix? line "selection: "))
               line))
           (define explained
             (and selection-line
                  (string->number (cadr (regexp-match #px"selection: ([0-9]+) files"
                                                      selection-line)))))
           (check-true (exact-positive-integer? explained) "explain states a selection count")
           (define-values (status out err)
             (invoke-command 300 dir (find-executable-path "racket") qt "default" "--jobs" "2"))
           (check-equal? status 0 (string-append out err))
           (define pass-line
             (for/first ([line (in-list (string-split out "\n"))]
                         #:when (string-prefix? line "default passed:"))
               line))
           (check-regexp-match default-pass-pattern (or pass-line ""))
           (define executed
             (and pass-line
                  (string->number (cadr (regexp-match #px"default passed: ([0-9]+) files"
                                                      pass-line)))))
           (check-equal? executed
                         explained
                         "the executed lane must be exactly the selection explain printed")
           (check-false (regexp-match? #rx"full passed:" out)
                        "a lane never borrows another lane's pass line")
           (check-regexp-match
            #rx"unavailable-here: tests/keys\\.rkt \\(@requires provider-key; verified by CI, never by this command\\)"
            out)
           (check-false (file-exists? (build-path dir "tests" "keys-ran"))
                        "the unavailable-here member never executes")
           (check-false (file-exists? (build-path dir "tests" "slow-ran")))))
        (displayln "SKIP default lane CLI: Linux /proc or setsid unavailable")))
  ;; --- closed stdout (verdict ordering) ----------------------------------
  (test-case "closed stdout: release explain exits 0 with an empty stderr"
    ;; `./qt release --explain | head -5`: the consumer stops reading after the
    ;; first lines. qt must finish the plan, exit 0, and print NO write-error
    ;; traceback. The fixture plan is larger than a pipe buffer, so the child is
    ;; still writing when the read side closes: EPIPE is deterministic and the
    ;; repository's ~1,500-file tree is never walked.
    (with-long-plan-root
     (lambda (dir)
       (define expected-lines (plan-explain-lines (resolve-lane-plan 'release dir)))
       (define expected-bytes (string-length (join-lines expected-lines)))
       (check-true (> expected-bytes 65536)
                   "the fixture plan must exceed one pipe buffer so the writer blocks")
       (define-values (status head-lines err)
         (invoke-with-short-stdout 5 300 dir "release" "--explain"))
       (check-equal? status 0 (format "qt must exit 0 on a closed stdout: ~a" err))
       (check-equal? err "" "a closed stdout must not print an error traceback")
       (check-true (and (= (length head-lines) 5) (andmap string? head-lines))
                   "the plan must have started rendering before the pipe closed")
       ;; Deterministic EPIPE: the consumer stopped long before the plan ended.
       (check-true (< (length head-lines) (length expected-lines))
                   "the read side stopped before the plan ended")
       (check-regexp-match #rx"^release: complete available Racket suite under " (car head-lines))
       (check-regexp-match #rx"^label: release review only" (cadr head-lines))
       (check-regexp-match #rx"^selection: [0-9]+ files" (caddr head-lines)))))
  (test-case "closed stdout: a red release run still exits non-zero"
    (if (full-execution-supported?)
        (with-failing-lane-root
         (lambda (dir)
           ;; The release lane writes its obligation lines to STDOUT after the
           ;; failure output. The fixture emits a large stderr block, so after we
           ;; read the preamble line and close, the child is still busy writing
           ;; the failure block when the read end dies: the obligation writes are
           ;; the ones that hit EPIPE, deterministically. The verdict is computed
           ;; before printing, so a closed pipe can never skip the failure checks
           ;; and report success.
           (define-values (status head-lines err)
             (invoke-with-short-stdout 1 300 dir "release" "--jobs" "1"))
           (check-not-equal? status 0 "a red run must stay red on a closed stdout")
           (check-true (and (= (length head-lines) 1) (string? (car head-lines)))
                       "the preamble must have been read before the close")
           (check-regexp-match #rx"^release: starting the lane selection" (car head-lines))
           (check-regexp-match #rx"FAILED: " err)
           (check-false (regexp-match? #rx"error writing to stream port" err)
                        "a closed stdout must not produce a Racket write traceback")
           (check-false (regexp-match? #rx"release passed:" err))))
        (displayln "SKIP closed-stdout red lane: Linux /proc or setsid unavailable")))
  ;; --- release lane ------------------------------------------------------
  (test-case "release obligations are explicit, unverified, and block the pass"
    (define obligations (release-obligations))
    (check-true (> (length obligations) 0))
    (check-equal? (release-blocking obligations)
                  obligations
                  "no release obligation is verified by the suite run alone")
    (define text (join-lines (release-obligation-lines obligations)))
    ;; STABLE SLUGS + state vocabulary (stated contract change, final plan §6
    ;; E2): the ids name the claim, and only "verified" counts as satisfied.
    (check-regexp-match #rx"RELEASE OBLIGATION governance-parity \\[UNAVAILABLE\\]" text)
    (check-regexp-match #rx"RELEASE OBLIGATION ci-verification \\[UNAVAILABLE\\]" text)
    (check-regexp-match #rx"RELEASE OBLIGATION branch-protection \\[UNAVAILABLE\\]" text)
    (check-regexp-match #rx"RELEASE OBLIGATION release-provenance \\[UNAVAILABLE\\]" text)
    (check-false (regexp-match? #rx"macOS" text)
                 "the macOS/platform claim is not a candidate obligation")
    (check-regexp-match #rx"Python governance-controller suite" text)
    ;; A fully verified fixture list blocks nothing; FAILED and UNAVAILABLE
    ;; both block — only VERIFIED does not (fail closed).
    (check-equal?
     (release-blocking (list (release-obligation 'fixture-a "fixture check A" 'local 'verified)))
     '())
    (define mixed
      (list (release-obligation 'fixture-a "A" 'local 'verified)
            (release-obligation 'fixture-b "B" 'evidence 'unavailable)
            (release-obligation 'fixture-c "C" 'ci-results 'failed)))
    (check-equal? (map release-obligation-slug (release-blocking mixed)) '(fixture-b fixture-c))
    (check-regexp-match #rx"^release unverified: 2 release obligation"
                        (release-unverified-message (release-blocking mixed))))
  (test-case "release explain lists the obligations next to the plan"
    (with-lane-root (lambda (dir)
                      (define text (join-lines (plan-explain-lines (resolve-lane-plan 'release dir))))
                      (check-regexp-match #rx"RELEASE OBLIGATIONS " text)
                      (check-regexp-match #rx"\\[UNAVAILABLE\\]" text)
                      (check-regexp-match #rx"NOT VERIFIED by \\./qt release:" text)
                      (check-regexp-match #rx"lane filter: none" text)
                      (check-regexp-match #rx"complete available Racket suite" text))))
  (test-case "release CLI prints the obligations and never prints a pass"
    (if (full-execution-supported?)
        (with-lane-root
         (lambda (dir)
           (define-values (status out err)
             (invoke-command 300 dir (find-executable-path "racket") qt "release" "--jobs" "2"))
           (define text (string-append out err))
           (check-not-equal? status 0)
           (check-regexp-match #rx"RELEASE OBLIGATION governance-parity \\[verified\\]" text)
           ;; The FULL obligation list must be present, not just its first entry:
           ;; a preamble-only (or truncated) run cannot satisfy these.
           (check-regexp-match #rx"RELEASE OBLIGATION release-provenance \\[UNAVAILABLE\\]" text)
           ;; The evidence verdict line precedes the obligations; without
           ;; --evidence the CI results are unavailable, never inferred.
           (check-regexp-match #rx"release evidence: unavailable \\(no --evidence input given\\)"
                               text)
           (check-equal? (length (regexp-match* #rx"RELEASE OBLIGATION " text))
                         (length (release-obligations))
                         "every obligation is printed on its own line")
           (check-regexp-match #rx"qt: release unverified: [0-9]+ release obligation" err)
           (check-false (regexp-match? #rx"release passed:" text)
                        "an unverified obligation must never yield a release pass line")
           ;; The suite really ran its complete selection (8 fixture files), so
           ;; the failure is the obligation, not a skipped suite.
           (check-regexp-match
            #rx"release: starting the lane selection of the complete available Racket suite \\(release review\\) \\(8 files"
            out)))
        (displayln "SKIP release CLI: Linux /proc or setsid unavailable")))
  ;; --- release evidence (--evidence, final plan §6 E2) --------------------
  (define (write-evidence-doc dir rel json-text)
    (make-parent-directory* (build-path dir rel))
    (write-file (build-path dir rel) json-text))
  (define (results-doc sha pass fail)
    (format
     "{\"run_summary\":{\"run_sha\":\"~a\",\"file_count\":2,\"pass\":~a,\"fail\":~a,\"timeout\":0,\"skip\":0}}"
     sha
     pass
     fail))
  (define candidate-sha "0123456789abcdef0123456789abcdef01234567")
  (test-case "release evidence: artifact directory binding and cleanliness"
    (define dir (make-temporary-file "qt-lanes-evidence-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       ;; Absent and empty inputs are unavailable, never inferred.
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha
                                                                 (build-path dir "missing")))
                  'unavailable)
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha dir)) 'unavailable)
       ;; A clean, SHA-bound document verifies.
       (write-evidence-doc dir "shard-0/test-results.json" (results-doc candidate-sha 2 0))
       (define clean (load-release-evidence candidate-sha dir))
       (check-eq? (release-evidence-state clean) 'clean)
       (check-equal? (release-evidence-documents clean) 1)
       ;; A second clean document keeps the verdict clean.
       (write-evidence-doc dir "shard-1/test-results.json" (results-doc candidate-sha 1 0))
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha dir)) 'clean)
       ;; Bound but red: honest FAILED evidence.
       (write-evidence-doc dir "shard-2/test-results.json" (results-doc candidate-sha 0 1))
       (define failed (load-release-evidence candidate-sha dir))
       (check-eq? (release-evidence-state failed) 'failed)
       ;; Wrong SHA, missing attestation, malformed JSON: unavailable.
       (write-evidence-doc dir
                           "shard-3/test-results.json"
                           (results-doc "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef" 1 0))
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha dir)) 'unavailable)
       (delete-file (build-path dir "shard-3" "test-results.json"))
       (write-evidence-doc dir "shard-3/test-results.json" "{\"broken\":true}")
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha dir)) 'unavailable)
       (delete-file (build-path dir "shard-3" "test-results.json"))
       (write-file (build-path dir "shard-3" "test-results.json") "not json at all")
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha dir)) 'unavailable))
     (lambda () (delete-directory/files dir))))
  (test-case "release evidence: proof bundle binding is read fail-closed"
    (define dir (make-temporary-file "qt-lanes-bundle-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define bundle (build-path dir "bundle.json"))
       ;; A clean bundle bound to the candidate verifies.
       (write-file
        bundle
        (format
         (string-append
          "{\"schema\":\"q.proof-bundle/1\",\"subject\":{\"commit_sha\":\"~a\"},"
          "\"result\":{\"status\":\"pass\",\"tests_passed\":5,\"tests_failed\":0,\"timeouts\":0}}")
         candidate-sha))
       (define clean (load-release-evidence candidate-sha bundle))
       (check-eq? (release-evidence-state clean) 'clean)
       (check-equal? (release-evidence-documents clean) 1)
       ;; Bound but red.
       (write-file
        bundle
        (format
         (string-append
          "{\"schema\":\"q.proof-bundle/1\",\"subject\":{\"commit_sha\":\"~a\"},"
          "\"result\":{\"status\":\"pass\",\"tests_passed\":4,\"tests_failed\":1,\"timeouts\":0}}")
         candidate-sha))
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha bundle)) 'failed)
       ;; Wrong SHA: unavailable, never inferred.
       (write-file
        bundle
        (string-append
         "{\"schema\":\"q.proof-bundle/1\",\"subject\":{\"commit_sha\":"
         "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"},"
         "\"result\":{\"status\":\"pass\",\"tests_passed\":5,\"tests_failed\":0,\"timeouts\":0}}"))
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha bundle)) 'unavailable)
       ;; Unknown schema and non-JSON content: malformed, unavailable.
       (write-file bundle "{\"schema\":\"something-else\"}")
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha bundle)) 'unavailable)
       (write-file bundle "[[[")
       (check-eq? (release-evidence-state (load-release-evidence candidate-sha bundle)) 'unavailable))
     (lambda () (delete-directory/files dir))))
  (test-case "release evidence: obligation states follow the evidence, never inference"
    ;; No evidence: everything unavailable; only the local verifier can move.
    (define none (release-obligations-with-evidence (release-evidence 'unavailable "nothing" 0)))
    (check-true (and (for/and ([o (in-list none)])
                       (eq? (release-obligation-state o) 'unavailable))
                     #t))
    ;; Clean evidence verifies ONLY the ci-results obligation; the attestation
    ;; obligations stay unavailable (no input format yet) and keep blocking.
    (define verified (release-obligations-with-evidence (release-evidence 'clean "clean" 3)))
    (check-equal? (for/list ([o (in-list verified)])
                    (list (release-obligation-slug o) (release-obligation-state o)))
                  '((governance-parity unavailable) (ci-verification verified)
                                                    (branch-protection unavailable)
                                                    (release-provenance unavailable)))
    (check-equal? (map release-obligation-slug (release-blocking verified))
                  '(governance-parity branch-protection release-provenance))
    ;; Failed evidence is honest failure, not absence.
    (define failed (release-obligations-with-evidence (release-evidence 'failed "red" 2)))
    (define ci
      (for/first ([o (in-list failed)]
                  #:when (eq? (release-obligation-slug o) 'ci-verification))
        o))
    (check-eq? (release-obligation-state ci) 'failed)
    ;; The local verifier is applied by the run: valid + clean surfaces only.
    (define applied (release-apply-run verified #t '()))
    (check-eq? (release-obligation-state (car applied)) 'verified)
    (define not-applied (release-apply-run verified #f '()))
    (check-eq? (release-obligation-state (car not-applied)) 'unavailable)
    (define touched (release-apply-run verified #t '("info.rkt")))
    (check-eq? (release-obligation-state (car touched)) 'unavailable)
    ;; The evidence line states the verdict in the fail-closed vocabulary.
    (check-regexp-match #rx"^release evidence: unavailable"
                        (release-evidence-line (release-evidence 'unavailable "nothing" 0)))
    (check-regexp-match #rx"^release evidence: "
                        (release-evidence-line (release-evidence 'clean "clean" 1))))
  ;; --- changed-file selection --------------------------------------------
  (test-case "changed selection includes affected and unattributable files, excludes unrelated"
    (with-lane-root (lambda (dir)
                      (define rels '("tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))
                      (define report
                        (changed-selection dir rels '("runtime/foo.rkt") #:base "fixture-base"))
                      (check-true (and (member "tests/a.rkt" (change-report-included report)) #t))
                      (check-true (and (member "tests/c.rkt" (change-report-included report)) #t)
                                  "a file with no @covers attribution must be INCLUDED (fail open)")
                      (check-false (member "tests/b.rkt" (change-report-included report)))
                      (check-equal? (change-report-excluded report) '("tests/b.rkt"))
                      (check-equal? (change-report-unattributable report) '("tests/c.rkt"))
                      (check-equal? (change-report-escalations report) '())
                      (check-equal? (change-report-changed report) '("runtime/foo.rkt"))
                      (check-equal? (change-report-base report) "fixture-base")
                      (define text (join-lines (change-report-lines report)))
                      (check-regexp-match #rx"changed mapping: @covers manifest" text)
                      (check-regexp-match #rx"included: tests/a.rkt" text)
                      (check-regexp-match #rx"excluded: tests/b.rkt" text)
                      (check-regexp-match #rx"no @covers attribution; fail open" text)
                      (check-regexp-match #rx"changed escalations: none" text))))
  (test-case "an unmapped changed file escalates and includes the whole lane"
    (with-lane-root
     (lambda (dir)
       (define rels '("tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))
       (define report (changed-selection dir rels '("runtime/baz.rkt") #:base "fixture-base"))
       (check-true (pair? (change-report-escalations report)))
       (check-equal? (sort (change-report-included report) string<?) (sort rels string<?))
       (check-equal? (change-report-excluded report) '())
       (check-regexp-match #rx"the whole lane is included, fail open"
                           (join-lines (change-report-lines report))))))
  (test-case "changed explain states the mapping and the filter together"
    (with-lane-root (lambda (dir)
                      ;; Build the plan through the pure mapping path (no git needed) by
                      ;; resolving the same lane files the command resolves.
                      (define report
                        (changed-selection dir
                                           (lane-relative 'default dir)
                                           '("runtime/foo.rkt")
                                           #:base "fixture-base"))
                      (check-equal? (sort (change-report-included report) string<?)
                                    '("tests/a.rkt" "tests/c.rkt"
                                                    "tests/iso.rkt"
                                                    "tests/keys.rkt"
                                                    "tests/mut.rkt"
                                                    "tests/net.rkt"))
                      (define text (join-lines (change-report-lines report)))
                      (check-regexp-match #rx"changed: working-tree diff against fixture-base" text)
                      (check-regexp-match #rx"changed files: 1" text))))
  (test-case "--changed with an unusable base fails closed"
    (with-lane-root (lambda (dir)
                      (define-values (status out err)
                        (invoke-command 120
                                        dir
                                        (find-executable-path "racket")
                                        qt
                                        "default"
                                        "--changed"
                                        "no-such-ref"
                                        "--explain"))
                      (check-not-equal? status 0)
                      (check-regexp-match #rx"^qt: " err)
                      (check-regexp-match #rx"cannot compute the working-tree diff" err)
                      (check-false (regexp-match? #rx"selection:" out)
                                   "no plan is printed when the diff fails"))))
  (test-case "--changed selects against a real git base (explain only)"
    (if (git-available?)
        (with-lane-root
         (lambda (dir)
           (check-true (git-in dir "init" "-q"))
           (check-true (git-in dir "add" "-A"))
           (check-true (git-in dir
                               "-c"
                               "user.email=fixture@example.invalid"
                               "-c"
                               "user.name=fixture"
                               "-c"
                               "commit.gpgsign=false"
                               "commit"
                               "-q"
                               "-m"
                               "fixture baseline"))
           ;; A clean tree is a computed-but-empty diff: fail closed, never a pass.
           (define-values (clean-status clean-out clean-err)
             (invoke-command 300
                             dir
                             (find-executable-path "racket")
                             qt
                             "default"
                             "--changed"
                             "--explain"))
           (check-not-equal? clean-status 0)
           (check-regexp-match #rx"working-tree diff against HEAD is empty" clean-err)
           (check-false (regexp-match? #rx"selection:" clean-out))
           (write-file (build-path dir "runtime" "foo.rkt")
                       "#lang racket/base\n(provide foo)\n(define (foo) 2)\n")
           ;; An untracked new test file counts as working-tree change too.
           (write-file (build-path dir "tests" "new.rkt")
                       (join-lines '("#lang racket/base" ";; @speed fast"
                                                         "(require rackunit)"
                                                         "(module+ test (check-true #t))")))
           (define-values (status out err)
             (invoke-command 300
                             dir
                             (find-executable-path "racket")
                             qt
                             "default"
                             "--changed"
                             "--explain"))
           (check-equal? status 0 (string-append out err))
           (check-regexp-match #rx"changed: working-tree diff against HEAD" out)
           (check-regexp-match #rx"diff against HEAD" out)
           (check-regexp-match #rx"lane selection: required-offline" out)
           (check-regexp-match #rx"included: tests/a.rkt" out)
           (check-regexp-match #rx"included: tests/new.rkt \\(no @covers attribution; fail open\\)"
                               out)
           (check-regexp-match #rx"excluded: tests/b.rkt" out)
           (check-false (regexp-match? #rx"default passed:" out) "explain never executes")
           (check-false (file-exists? (build-path dir "tests" "slow-ran")))
           ;; `full --changed` uses the same mapping without the lane filter.
           (define-values (full-status full-out full-err)
             (invoke-command 300
                             dir
                             (find-executable-path "racket")
                             qt
                             "full"
                             "--changed"
                             "--explain"))
           (check-equal? full-status 0 (string-append full-out full-err))
           (check-regexp-match
            #rx"full: changed-file selection of the complete available Racket suite"
            full-out)
           (check-regexp-match #rx"lane filter: none" full-out)
           (check-regexp-match #rx"included: tests/a.rkt" full-out)
           (check-regexp-match #rx"excluded: tests/b.rkt" full-out)
           ;; The mapped selection EXECUTES: 3 files (tests/a.rkt,
           ;; tests/c.rkt unattributable, tests/new.rkt unattributable), and the
           ;; filtered slow file still never runs.
           (if (full-execution-supported?)
               (let-values ([(run-status run-out run-err)
                             (invoke-command 300
                                             dir
                                             (find-executable-path "racket")
                                             qt
                                             "default"
                                             "--changed"
                                             "--jobs"
                                             "2")])
                 (check-equal? run-status 0 (string-append run-out run-err))
                 (define run-pass-line
                   (for/first ([line (in-list (string-split run-out "\n"))]
                               #:when (string-prefix? line "default passed:"))
                     line))
                 (check-regexp-match default-pass-pattern (or run-pass-line ""))
                 (check-regexp-match #rx"^default passed: 6 files, 6 tests" (or run-pass-line ""))
                 (check-regexp-match #rx"default: starting the changed-file selection" run-out)
                 (check-false (file-exists? (build-path dir "tests" "slow-ran"))
                              "a lane-filtered file must not run under --changed either"))
               (displayln "SKIP --changed execution: Linux /proc or setsid unavailable"))))
        (displayln "SKIP --changed git base: git unavailable")))
  ;; --- frozen full surface and CLI failures ------------------------------
  (test-case "plain full explain and preamble stay byte-identical"
    (with-lane-root
     (lambda (dir)
       (define plan (resolve-lane-plan 'full dir))
       (check-equal? (plan-explain-lines plan) (full-explain-lines dir))
       (check-equal?
        (plan-preamble plan 4)
        (format
         (string-append
          "full: starting the complete available Racket suite (work-conserving queue, jobs ~a; "
          "mutation owners dispatch first); ~a")
         4
         full-label))
       (check-equal? (plan-label plan) full-label))))
  (test-case "unsupported command and options fail before any execution"
    ;; One launch per refusal; the message pins ride on the same invocation
    ;; (W4b removed the three duplicate launches that re-invoked an already
    ;; invoked spelling only to read its message).
    (for ([case (in-list (list (cons '("bogus")
                                     #rx"only quick, full, default and release are available")
                               (cons '("quick" "--changed")
                                     #rx"quick --changed requires BASE..HEAD or a single base ref")
                               (cons '("release" "--changed") #rx"release does not accept --changed")
                               (cons '("default" "--bogus") #rx"unsupported option: --bogus")
                               (cons '("default" "extra") #f)
                               (cons '("full" "tests/test-qt-lanes.rkt") #f)
                               (cons '("default" "--changed" "a" "b") #f)))])
      (define args (car case))
      (define-values (status out err) (apply invoke args))
      (check-not-equal? status 0 (format "~a should fail" args))
      (check-regexp-match #rx"^qt: " err)
      (when (cdr case)
        (check-regexp-match (cdr case) err))
      (check-false (regexp-match? #rx"passed:" out) (format "~a must not execute" args)))))
