#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration

;; Contract for `./qt full`:
;;   - selection is the repository's own `collect-test-files 'all`, with
;;     duplicate entries rejected;
;;   - `--explain` prints that selection, the per-file timeout policy, the
;;     mutation-isolation split, host execution support and BOTH honesty lists
;;     (not verified / not provided); it executes and cleans nothing;
;;   - execution goes through the structured-evidence quick runner only:
;;     per-file `@timeout` bounds, mutation-sensitive files serialized before
;;     the parallel batch, managed surfaces detected (never restored), failure
;;     reasons reported, and one honest pass line;
;;   - every failure mode (zero selection, duplicates, surface mutation,
;;     unsupported host, invalid arguments) fails closed.
;; Fixture roots keep execution tests fast; the real 1570-file suite is never
;; run here.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         racket/runtime-path
         "../scripts/testing/full-plan.rkt"
         "../scripts/testing/quick-runner.rkt"
         (submod "../scripts/testing/full-plan.rkt" test-support)
         (submod "../scripts/testing/quick-runner.rkt" test-support))

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

;; Build a fixture repository root containing only <root>/tests/<file>.
(define (with-fixture-root files thunk)
  (define dir (make-temporary-file "qt-full-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory (build-path dir "tests"))
                  (for ([file (in-list files)])
                    (call-with-output-file (build-path dir "tests" (car file))
                                           #:exists 'truncate
                                           (lambda (out) (display (cdr file) out))))
                  (thunk dir))
                (lambda () (delete-directory/files dir))))

(define (write-file path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; --- fixtures -------------------------------------------------------------

(define failing-fixture (cons "fail.rkt" "#lang racket/base\n(require rackunit)\n(check-true #f)\n"))

(define passing-fixture
  (cons "pass.rkt" "#lang racket/base\n(require rackunit)\n(check-true #t)\n(check-equal? 1 1)\n"))

;; Loading this fixture creates tests/marker; it must never happen under
;; --explain.
(define marker-fixture
  (cons "marker.rkt"
        "#lang racket/base\n(call-with-output-file \"marker\" void)\n(raise \"executed\")\n"))

;; A declared @timeout of 1 second must bound this file; sleeping far past it
;; turns the result into an honest timeout instead of a hang.
(define slow-fixture
  (cons "slow.rkt"
        (string-join '("#lang racket/base" ";; @timeout 1"
                                           "(require rackunit)"
                                           "(module+ test"
                                           "  (check-true #t)"
                                           "  (sleep 30))")
                     "\n")))

;; Passing assertions followed by a non-zero exit: the evidence never lands, so
;; the reason must be the child exit, not an assertion failure.
(define exit-fixture
  (cons "exit.rkt"
        (string-join
         '("#lang racket/base" "(require rackunit)" "(module+ main" "  (check-true #t)" "  (exit 3))")
         "\n")))

;; Mutation-sensitive (@isolation process): must run alone, before the parallel
;; batch. The long pause makes marker ordering (not timing) decisive.
(define mutating-fixture
  (cons "mutating.rkt"
        (string-join
         '("#lang racket/base" ";; @isolation process"
                               "(require rackunit)"
                               "(define (mark! line)"
                               "  (call-with-output-file \"mutation.log\" #:exists 'append"
                               "    (lambda (out) (displayln line out))))"
                               "(mark! \"mutating-start\")"
                               "(module+ test"
                               "  (check-true #t)"
                               "  (sleep 1.5)"
                               "  (mark! \"mutating-end\"))")
         "\n")))

;; Non-mutating sibling: marks as soon as it starts, so a shared parallel batch
;; would interleave its marker before mutating-end.
(define parallel-fixture
  (cons "parallel.rkt"
        (string-join '("#lang racket/base" "(require rackunit)"
                                           "(call-with-output-file \"mutation.log\" #:exists 'append"
                                           "  (lambda (out) (displayln \"parallel-start\" out)))"
                                           "(module+ test (check-true #t))")
                     "\n")))

;; Mutates managed repository surfaces: README.md bytes and CHANGELOG.md mode.
(define surface-fixture
  (cons "surface.rkt"
        (string-join '("#lang racket/base"
                       "(require rackunit)"
                       "(define root (build-path (current-directory) \"..\"))"
                       "(call-with-output-file (build-path root \"README.md\") #:exists 'append"
                       "  (lambda (out) (displayln \"mutated-by-test\" out)))"
                       "(file-or-directory-permissions (build-path root \"CHANGELOG.md\") #o600)"
                       "(module+ test (check-true #t))")
                     "\n")))

(module+ test
  (test-case "full explain lists selection, policies and both honesty lists without executing"
    (with-fixture-root
     (list marker-fixture)
     (lambda (dir)
       ;; A stale bytecode directory is exactly what a cleaning step would
       ;; delete; --explain must leave it in place.
       (define stale-dir (build-path dir "tests" "compiled"))
       (make-directory stale-dir)
       (define stale-zo (build-path stale-dir "stale_rkt.zo"))
       (define stale-log (build-path dir "tests" "mutation.log"))
       (write-file stale-zo "stale bytecode")
       (define lines (full-explain-lines dir))
       (define text (string-join lines "\n"))
       (check-regexp-match #rx"tests/marker\\.rkt" text)
       (check-equal? (full-explain-lines dir) lines "selection must be deterministic")
       (check-regexp-match #rx"selection only" text)
       (check-regexp-match #rx"not PR/CI verification" text)
       ;; Timeout policy: the default, the declared count and the max bound.
       (check-regexp-match #rx"timeouts: @timeout metadata when declared, else default 120s" text)
       (check-regexp-match #rx"declared 0 of 1 files" text)
       (check-regexp-match #rx"max 120s" text)
       ;; Mutation isolation split is visible in the plan.
       (check-regexp-match #rx"mutation isolation: 0 mutating file\\(s\\) run serially" text)
       (check-regexp-match #rx"execution support: " text)
       (check-regexp-match #rx"NOT VERIFIED by \\./qt full:" text)
       (for ([obligation
              (in-list '("Python" "macOS/platform lane" "branch protection" "release provenance"))])
         (check-regexp-match (pregexp obligation) text))
       (check-regexp-match #rx"NOT PROVIDED by \\./qt full \\(counted by other gates\\):" text)
       (for ([gap (in-list '("PR/CI verification of this commit" "packaging"
                                                                 "benchmark suite"
                                                                 "@requires"
                                                                 "not restored"
                                                                 "not sandboxed"))])
         (check-regexp-match (pregexp gap) text))
       (check-false (file-exists? (build-path dir "tests" "marker"))
                    "explain must not execute selected tests")
       (check-false (file-exists? stale-log) "explain must not execute selected tests")
       (check-true (file-exists? stale-zo) "explain must not clean stale bytecode")
       (check-equal? (map path->string (directory-list dir))
                     '("tests")
                     "explain must not write into the repository root")
       (check-equal? (sort (map path->string (directory-list (build-path dir "tests"))) string<?)
                     '("compiled" "marker.rkt")
                     "explain must not write into the fixture tree"))))
  (test-case "full explain reports host execution support honestly"
    (with-fixture-root (list passing-fixture)
                       (lambda (dir)
                         (define text (string-join (full-explain-lines dir) "\n"))
                         (check-equal? (and (regexp-match? #rx"execution support: available" text) #t)
                                       (full-execution-supported?))
                         (check-equal? (and (regexp-match? #rx"execution support: UNAVAILABLE" text)
                                            #t)
                                       (not (full-execution-supported?))))))
  (test-case "full explain fails through qt: on an empty or tests-less root"
    (define racket-exe (find-executable-path "racket"))
    (with-fixture-root '()
                       (lambda (dir)
                         (define-values (status out err)
                           (invoke-command 300 dir racket-exe qt "full" "--explain"))
                         (check-not-equal? status 0)
                         (check-regexp-match #rx"^qt: " err)
                         (check-regexp-match #rx"full selection is empty" err)))
    (with-fixture-root '()
                       (lambda (dir)
                         (delete-directory/files (build-path dir "tests"))
                         (define-values (status out err)
                           (invoke-command 300 dir racket-exe qt "full" "--explain"))
                         (check-not-equal? status 0)
                         (check-regexp-match #rx"^qt: " err))))
  (test-case "duplicate selection entries are rejected (#f-returning guard is live)"
    (check-equal? (selection-duplicate-guard '("tests/a.rkt" "tests/b.rkt"))
                  '("tests/a.rkt" "tests/b.rkt"))
    (check-exn #rx"duplicate entries: tests/a.rkt"
               (lambda () (selection-duplicate-guard '("tests/a.rkt" "tests/a.rkt")))))
  (test-case "failing fixture under full reports failure with reason and reproduction"
    (if (full-execution-supported?)
        (with-fixture-root (list failing-fixture)
                           (lambda (dir)
                             (define run (run-full dir 1))
                             (check-false (full-run-valid? run))
                             (check-equal? (length (full-run-files run)) 1)
                             (check-equal? (length (full-run-results run)) 1)
                             (define result (car (full-run-results run)))
                             (check-false (quick-result-valid? result))
                             ;; A failed RackUnit assertion leaves the child exit at 0;
                             ;; the failure count is what invalidates the result.
                             (check-equal? (quick-result-failed result) 1)
                             (check-equal? (quick-result-total result) 1)
                             (check-equal? (full-result-reason result) 'assertion)
                             (define text (string-join (full-failure-lines run) "\n"))
                             (check-regexp-match #rx"FAILED: " text)
                             (check-regexp-match #rx"reason: assertion" text)
                             (check-regexp-match #rx"fail\\.rkt" text)
                             (check-regexp-match #rx"Reproduce \\(whole-suite rerun\\): \\./qt full"
                                                 text)
                             (check-equal? (full-run-surface-violations run) '())))
        (displayln "SKIP full failing fixture: Linux /proc or setsid unavailable")))
  (test-case "child exit after passing assertions is reported with reason exit"
    (if (full-execution-supported?)
        (with-fixture-root (list exit-fixture)
                           (lambda (dir)
                             (define run (run-full dir 1))
                             (check-false (full-run-valid? run))
                             (check-equal? (full-result-reason (car (full-run-results run))) 'exit)
                             (define text (string-join (full-failure-lines run) "\n"))
                             (check-regexp-match #rx"FAILED: " text)
                             (check-regexp-match #rx"reason: exit" text)))
        (displayln "SKIP full exit reason: Linux /proc or setsid unavailable")))
  (test-case "per-file @timeout metadata bounds the child and reports reason timeout"
    (if (full-execution-supported?)
        (with-fixture-root (list slow-fixture)
                           (lambda (dir)
                             (define file (car (full-selection dir)))
                             (check-equal? (full-file-timeout file) 1)
                             (check-equal? (hash-ref (full-timeouts (list file)) file) 1)
                             (check-equal? (full-file-timeout file 7) 1 "declared wins over default")
                             (define run (run-full dir 1))
                             (check-false (full-run-valid? run))
                             (check-equal? (full-result-reason (car (full-run-results run))) 'timeout)
                             (define text (string-join (full-failure-lines run) "\n"))
                             (check-regexp-match #rx"reason: timeout" text)))
        (displayln "SKIP full per-file timeout: Linux /proc or setsid unavailable")))
  (test-case "mutating fixture is serialized before the parallel batch (marker order)"
    (if (full-execution-supported?)
        (with-fixture-root
         (list mutating-fixture parallel-fixture)
         (lambda (dir)
           (define run (run-full dir 2))
           (check-true (full-run-valid? run))
           (check-equal? (length (full-run-results run)) 2)
           (define log (build-path dir "tests" "mutation.log"))
           (check-true (file-exists? log) "both fixture batches must have run")
           (define marks
             (filter (lambda (l) (not (string=? l ""))) (string-split (file->string log) "\n")))
           (check-equal? marks
                         '("mutating-start" "mutating-end" "parallel-start")
                         "mutating file must finish before any parallel worker starts")))
        (displayln "SKIP full mutation isolation: Linux /proc or setsid unavailable")))
  (test-case "managed-surface mutation is detected and reported without being reverted"
    (if (full-execution-supported?)
        (with-fixture-root (list passing-fixture surface-fixture)
                           (lambda (dir)
                             (define readme (build-path dir "README.md"))
                             (define changelog (build-path dir "CHANGELOG.md"))
                             (write-file readme "original\n")
                             (write-file changelog "original\n")
                             (define run (run-full dir 2))
                             (check-true (full-run-valid? run) "per-file evidence stays honest")
                             (check-equal? (full-run-surface-violations run)
                                           '("README.md" "CHANGELOG.md"))
                             (define text (string-join (full-surface-violation-lines run) "\n"))
                             (for ([name (in-list '("README.md" "CHANGELOG.md"))])
                               (check-regexp-match (pregexp name) text))
                             (check-regexp-match #rx"detected, not restored" text)
                             ;; Never restored: bytes and mode stay exactly as the suite left them.
                             (check-regexp-match #rx"mutated-by-test" (file->string readme))
                             (check-equal? (file-or-directory-permissions changelog 'bits) #o600)))
        (displayln "SKIP full surface guard: Linux /proc or setsid unavailable")))
  (test-case "non-canonical root spellings normalize to the same selection"
    (with-fixture-root
     (list slow-fixture passing-fixture)
     (lambda (dir)
       (define canonical (full-selection dir))
       (check-equal? (full-selection (string-append (path->string dir) "/")) canonical)
       (define dotted (build-path dir "tests" ".."))
       (check-equal? (full-selection dotted) canonical)
       (check-equal? (full-selection (path->string dotted)) canonical)
       ;; Metadata lookup must survive the same dotted spelling.
       (define slow (car (filter (lambda (f) (string-suffix? f "slow.rkt")) canonical)))
       (define dotted-slow
         (path->string (build-path dotted "tests" (file-name-from-path (string->path slow)))))
       (check-equal? (full-file-timeout dotted-slow) 1)))
    ;; Execution identity uses the same normalization: a dotted root still
    ;; yields exactly one valid result per selected file.
    (if (full-execution-supported?)
        (with-fixture-root (list passing-fixture)
                           (lambda (dir)
                             (define canonical (full-selection dir))
                             (define run (run-full (build-path dir "tests" "..") 1))
                             (check-true (full-run-valid? run))
                             (check-equal? (full-run-files run) canonical)))
        (displayln "SKIP full dotted-root execution: Linux /proc or setsid unavailable")))
  (test-case "passing fixture under full is valid and counts assertions"
    (if (full-execution-supported?)
        (with-fixture-root (list passing-fixture)
                           (lambda (dir)
                             (define run (run-full dir 1))
                             (check-true (full-run-valid? run))
                             (check-equal? (length (full-run-files run)) 1)
                             (check-equal? (apply + (map quick-result-total (full-run-results run)))
                                           2)
                             (check-equal? (full-failure-lines run) '())
                             (check-equal? (full-run-surface-violations run) '())
                             (check-regexp-match full-pass-pattern (full-pass-line run))))
        (displayln "SKIP full passing fixture: Linux /proc or setsid unavailable")))
  (test-case "runner-side failure never becomes a full pass"
    (if (full-execution-supported?)
        (with-fixture-root (list passing-fixture)
                           (lambda (dir)
                             (define run
                               (parameterize ([current-quick-result-transform
                                               (lambda (_thunk)
                                                 (error 'simulated-runner-failure "worker died"))])
                                 (run-full dir 1)))
                             (check-false (full-run-valid? run))
                             (define result (car (full-run-results run)))
                             (check-false (quick-result-valid? result))
                             (check-equal? (quick-result-failed result) #f)
                             (check-regexp-match #rx"simulated-runner-failure"
                                                 (string-join (full-failure-lines run) "\n"))))
        (displayln "SKIP full runner failure: Linux /proc or setsid unavailable")))
  (test-case "zero selection fails closed instead of passing empty"
    (with-fixture-root '()
                       (lambda (dir)
                         (check-exn #rx"full selection is empty" (lambda () (full-selection dir)))
                         (check-exn #rx"full selection is empty" (lambda () (full-explain-lines dir)))
                         (check-exn #rx"full selection is empty" (lambda () (run-full dir 1))))))
  (test-case "full CLI executes the fixture selection and labels the result"
    (if (full-execution-supported?)
        (let ([racket-exe (find-executable-path "racket")])
          (with-fixture-root
           (list passing-fixture)
           (lambda (dir)
             (define-values (status out err)
               (invoke-command 300 dir racket-exe qt "full" "--jobs" "1"))
             (check-equal? status 0 (string-append out err))
             (define pass-line
               (for/first ([line (in-list (string-split out "\n"))]
                           #:when (string-prefix? line "full passed:"))
                 line))
             (check-regexp-match full-pass-pattern (or pass-line ""))
             (check-equal? pass-line
                           (string-append "full passed: 1 files, 2 tests ("
                                          "full Racket suite only, not PR/CI verification)"))))
          (with-fixture-root
           (list failing-fixture)
           (lambda (dir)
             (define-values (status out err)
               (invoke-command 300 dir racket-exe qt "full" "--jobs" "1"))
             (check-not-equal? status 0)
             (check-regexp-match #rx"FAILED: " err)
             (check-regexp-match #rx"reason: assertion" err)
             (check-regexp-match #rx"full failed or produced missing/invalid results" err)))
          (with-fixture-root '()
                             (lambda (dir)
                               (define-values (status out err)
                                 (invoke-command 300 dir racket-exe qt "full"))
                               (check-not-equal? status 0)
                               (check-regexp-match #rx"full selection is empty" err))))
        (displayln "SKIP full CLI execution: Linux /proc or setsid unavailable")))
  (test-case "full CLI reports managed-surface mutation after per-file results"
    (if (full-execution-supported?)
        (let ([racket-exe (find-executable-path "racket")])
          (with-fixture-root (list failing-fixture surface-fixture)
                             (lambda (dir)
                               (define readme (build-path dir "README.md"))
                               (write-file readme "original\n")
                               (write-file (build-path dir "CHANGELOG.md") "original\n")
                               (define-values (status out err)
                                 (invoke-command 300 dir racket-exe qt "full" "--jobs" "2"))
                               (check-not-equal? status 0)
                               (define failed-pos (regexp-match-positions #rx"FAILED: " err))
                               (define surface-pos
                                 (regexp-match-positions #rx"SURFACE MODIFIED: README\\.md" err))
                               (check-true (and failed-pos surface-pos #t))
                               (when (and failed-pos surface-pos)
                                 (check-true (< (car (car failed-pos)) (car (car surface-pos)))
                                             "surface report must come after per-file results"))
                               (check-regexp-match #rx"detected, not restored" err)
                               (check-false (regexp-match? #rx"full passed:" out))
                               ;; The command failed, and it still never restored the surface.
                               (check-regexp-match #rx"mutated-by-test" (file->string readme)))))
        (displayln "SKIP full CLI surface guard: Linux /proc or setsid unavailable")))
  (test-case "invalid full arguments fail closed"
    ;; `default` is a real lane now (tests/test-qt-lanes.rkt covers its surface),
    ;; so it no longer belongs in this list of rejected arguments.
    (for ([args (in-list '(("full" "--bogus") ("full" "--jobs")
                                              ("full" "--jobs" "0")
                                              ("full" "--jobs" "2.5")
                                              ("full" "--explain" "--explain")
                                              ("full" "--explain" "extra")
                                              ("full" "tests/test-qt.rkt")))])
      (define-values (status out err) (apply invoke args))
      (check-not-equal? status 0 (format "~a should fail" args))
      (check-regexp-match #rx"^qt: " err)))
  ;; Repository-wide selection is deliberately not asserted here: a cold
  ;; metadata parse of the ~1570-file tree costs ~25s per fresh process (an
  ;; inherited legacy discovery cost). The CLI plan path is covered against a
  ;; fixture root instead, which also exercises the default launch-tree root.
  (test-case "full CLI explain prints the plan without executing"
    (let ([racket-exe (find-executable-path "racket")])
      (with-fixture-root
       (list marker-fixture)
       (lambda (dir)
         (define-values (status out err) (invoke-command 300 dir racket-exe qt "full" "--explain"))
         (check-equal? status 0 (string-append out err))
         (check-regexp-match #rx"tests/marker\\.rkt" out)
         (check-regexp-match #rx"selection only" out)
         (check-regexp-match #rx"full Racket suite only, not PR/CI verification" out)
         (check-regexp-match #rx"timeouts: " out)
         (check-regexp-match #rx"mutation isolation: " out)
         (check-regexp-match #rx"NOT VERIFIED by \\./qt full:" out)
         (check-regexp-match #rx"NOT PROVIDED by \\./qt full" out)
         (check-false (regexp-match? #rx"full passed:" out) "explain must not execute")
         (check-false (regexp-match? #rx"FAILED: " out) "explain must not execute")
         (check-false (file-exists? (build-path dir "tests" "marker"))
                      "explain must not execute selected tests"))))))
