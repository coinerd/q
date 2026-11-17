#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration

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

;; A small root with 21 declared-slow files plus one kept file: enough to pin
;; the default lane's 20-name cap and its `(+ N more)` remainder.
(define cap-root-excluded-count 21)

(define (with-excluded-many-root thunk)
  (define dir (make-temporary-file "qt-lanes-cap-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory (build-path dir "tests"))
                  (define (put name marker)
                    (write-file (build-path dir "tests" name)
                                (join-lines (list "#lang racket/base"
                                                  marker
                                                  "(require rackunit)"
                                                  "(module+ test (check-true #t))"))))
                  (put "plain.rkt" ";; @speed fast")
                  (for ([i (in-range cap-root-excluded-count)])
                    (put (format "slow-~a.rkt" i) ";; @speed slow"))
                  (thunk dir))
                (lambda () (delete-directory/files dir))))

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
        (check-eq? (lane-verdict lane #t '() (list (release-obligation 'x "x" #f)))
                   'invalid
                   (format "~a: obligations on a non-release lane" lane)))
      (check-eq? (lane-verdict lane #t '("README.md") (list (release-obligation 'x "x" #f)))
                 'surface
                 (format "~a: a changed surface outranks an obligation" lane)))
    ;; The release lane is the only lane with obligations: an unverified
    ;; obligation is red, an empty blocking list is the only path to a pass.
    (check-eq? (lane-verdict 'release #t '() (list (release-obligation 'x "x" #f))) 'unverified)
    (check-eq? (lane-verdict 'release #t '() '()) 'ok "release with empty blocking is ok")
    ;; `blocking` is the already-filtered unverified list, so a verified
    ;; obligation filters away and cannot block the pass line either.
    (check-eq? (lane-verdict 'release #t '() (release-blocking (list (release-obligation 'x "x" #t))))
               'ok)
    (check-eq? (lane-verdict 'release #t '() (release-blocking (release-obligations)))
               'unverified
               "the real obligation list blocks every release pass"))
  (test-case "lane-verdict fields drive the verdict for a recorded run"
    (with-lane-root
     (lambda (dir)
       (define plan (resolve-lane-plan 'default dir))
       (if (full-execution-supported?)
           (let* ([run (run-plan plan 2)]
                  [verdict (lane-verdict 'default
                                         (full-run-valid? run)
                                         (full-run-surface-violations run)
                                         '())])
             (check-true (full-run-valid? run))
             (check-eq? verdict 'ok)
             (check-regexp-match default-pass-pattern (default-pass-line run)))
           (displayln "SKIP lane-verdict recorded run: Linux /proc or setsid unavailable")))))
  ;; --- default lane ------------------------------------------------------
  (test-case "default lane excludes slow/network fixtures and keeps fast ones"
    (with-lane-root
     (lambda (dir)
       (define default-rels (lane-relative 'default dir))
       (define full-rels (lane-relative 'full dir))
       (check-true (and (member "tests/a.rkt" default-rels) #t))
       (check-true (and (member "tests/c.rkt" default-rels) #t))
       (check-true (and (member "tests/slow.rkt" full-rels) #t) "full keeps every discovered file")
       (check-false (member "tests/slow.rkt" default-rels) "@speed slow is not the PR lane")
       (check-false (member "tests/net.rkt" default-rels) "@requires network is not the PR lane")
       (check-false (member "tests/mut.rkt" default-rels)
                    "a declared @mutates value is not the PR lane")
       (check-false (member "tests/iso.rkt" default-rels)
                    "a declared @isolation value (sandboxed-home) is not the PR lane")
       ;; The exclusion is reasoned, not merely membership: the filter reads the
       ;; repository's own metadata helpers.
       (check-true
        (and (memq 'slow
                   (default-exclusion-reasons (path->string (build-path dir "tests" "slow.rkt"))))
             #t))
       (check-true
        (and (memq 'requires
                   (default-exclusion-reasons (path->string (build-path dir "tests" "net.rkt"))))
             #t))
       (check-true
        (and (memq 'mutation
                   (default-exclusion-reasons (path->string (build-path dir "tests" "mut.rkt"))))
             #t)
        "a declared mutation alone excludes a file")
       (check-true
        (and (memq 'sandbox
                   (default-exclusion-reasons (path->string (build-path dir "tests" "iso.rkt"))))
             #t)
        "a declared isolation alone excludes a file")
       ;; The predicates live in classify.rkt next to the other classifiers, so
       ;; the excluded vocabulary is the metadata's, not a lane-local list.
       (check-true (isolation-excluded? (path->string (build-path dir "tests" "iso.rkt"))))
       (check-false (isolation-excluded? (path->string (build-path dir "tests" "a.rkt"))))
       (check-true (declared-mutation? (path->string (build-path dir "tests" "mut.rkt"))))
       (check-false (declared-mutation? (path->string (build-path dir "tests" "a.rkt"))))
       (check-true (requires-excluded? (path->string (build-path dir "tests" "net.rkt"))))
       (check-false (requires-excluded? (path->string (build-path dir "tests" "a.rkt"))))
       (check-equal? (default-exclusion-reasons (path->string (build-path dir "tests" "a.rkt"))) '())
       (check-equal? (sort default-rels string<?) '("tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))
       (check-equal? (sort full-rels string<?)
                     '("tests/a.rkt" "tests/b.rkt"
                                     "tests/c.rkt"
                                     "tests/iso.rkt"
                                     "tests/mut.rkt"
                                     "tests/net.rkt"
                                     "tests/slow.rkt")))))
  (test-case "requires vocabulary is pinned independently of the filter"
    ;; A literal list: deleting a token from classify.rkt must fail HERE, not
    ;; merely shrink the generated fixture set below.
    (check-equal? requires-excluded-tokens '("network" "terminal" "browser" "provider-key")))
  (test-case "default lane excludes every requirement token the classifier names"
    ;; Generated from `requires-excluded-tokens`, so each token in the
    ;; repository vocabulary (including the live-tty one) gets a fixture
    ;; and the list cannot drift from the classifier.
    (with-requires-token-root
     (lambda (dir)
       (for ([tok (in-list requires-excluded-tokens)])
         (define f (path->string (build-path dir "tests" (format "req-~a.rkt" tok))))
         (check-true (requires-excluded? f) (format "~a must be excluded" tok))
         (check-true (and (memq 'requires (default-exclusion-reasons f)) #t)
                     (format "~a must yield the requires reason" tok))
         (check-false (member (format "tests/req-~a.rkt" tok) (lane-relative 'default dir))
                      (format "~a must not reach the PR lane" tok)))
       (check-equal? (lane-relative 'default dir) '("tests/plain.rkt")))))
  (test-case "default filter description names exactly the predicates it applies"
    ;; The printed claim must not be broader or narrower than the filter: the
    ;; requirement tokens are the classifier's own vocabulary, not a copy.
    (for ([needle (in-list (list "slow-file?"
                                 "mutating-file?"
                                 "other than none"
                                 "sandboxed-home"
                                 "network/terminal/browser/provider-key"))])
      (check-true (string-contains? default-filter-description needle)
                  (format "filter description must state ~a" needle))))
  (test-case "default explain states the lane filter and the exclusion counts"
    (with-lane-root
     (lambda (dir)
       (define lines (plan-explain-lines (resolve-lane-plan 'default dir)))
       (define text (join-lines lines))
       (check-regexp-match #rx"default: fast/PR lane over the complete available Racket suite" text)
       (check-true (string-contains? text default-filter-description))
       (check-regexp-match #rx"lane excluded: 4 of 7 files" text)
       (check-regexp-match
        #rx"\\(reasons overlap: slow 1, mutation 1, sandbox/isolation 1, requires 1\\)"
        text)
       ;; The excluded files are NAMED, not only counted, so a silently
       ;; excluded file cannot hide behind a total.
       (check-true (for/or ([line (in-list lines)])
                     (string-prefix? line "  excluded file: tests/slow.rkt ("))
                   "a slow file must be named")
       (check-true (for/or ([line (in-list lines)])
                     (string-prefix? line "  excluded file: tests/net.rkt (requires)"))
                   "a requires file must be named with its reason")
       (check-true (for/or ([line (in-list lines)])
                     (string-prefix? line "  excluded file: tests/mut.rkt (mutation)"))
                   "a declared-mutation file must be named with its reason")
       (check-true (for/or ([line (in-list lines)])
                     (string-prefix? line "  excluded file: tests/iso.rkt (sandbox)"))
                   "a declared-isolation file must be named with its reason")
       (check-regexp-match #rx"lane filter: " text)
       (check-regexp-match #rx"NOT VERIFIED by \\./qt default:" text)
       (check-regexp-match #rx"NOT PROVIDED by \\./qt default" text)
       (check-equal? (plan-explain-lines (resolve-lane-plan 'default dir))
                     lines
                     "the default plan must be deterministic")
       (check-false (regexp-match? #rx"default passed:" text) "explain never executes")
       (check-false (file-exists? (build-path dir "tests" "slow-ran"))
                    "explain must not execute selected tests"))))
  (test-case "default explain names a capped list of excluded files"
    (with-excluded-many-root
     (lambda (dir)
       (define lines (plan-explain-lines (resolve-lane-plan 'default dir)))
       (define named
         (for/list ([line (in-list lines)]
                    #:when (string-prefix? line "  excluded file: "))
           line))
       (define file-lines
         (filter (lambda (line) (string-prefix? line "  excluded file: tests/")) named))
       (check-equal? (length file-lines) 20 "the named list is capped at 20 files")
       (check-true (for/or ([line (in-list named)])
                     (string-prefix? line "  excluded file: ... (+ 1 more)"))
                   "the remainder is reported as a count"))))
  (test-case "default lane runs exactly its selection, minus the filtered files"
    (if (full-execution-supported?)
        (with-lane-root (lambda (dir)
                          (define plan (resolve-lane-plan 'default dir))
                          (define run (run-plan plan 2))
                          (check-true (full-run-valid? run))
                          (check-equal? (full-run-files run) (lane-plan-files plan))
                          (check-equal? (length (full-run-results run)) 3)
                          (check-false (file-exists? (build-path dir "tests" "slow-ran"))
                                       "the filtered slow file must not have been executed")
                          (check-regexp-match default-pass-pattern (default-pass-line run))
                          ;; Per-file timeouts survive the lane filter.
                          (check-equal? (hash-count (full-timeouts (lane-plan-files plan)))
                                        (length (lane-plan-files plan)))))
        (displayln "SKIP default lane execution: Linux /proc or setsid unavailable")))
  (test-case "default lane CLI runs the filtered selection and labels the result"
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
    (check-regexp-match #rx"RELEASE OBLIGATION not-verified-1 \\[NOT VERIFIED\\]" text)
    (check-regexp-match #rx"RELEASE OBLIGATION not-provided-1 \\[NOT VERIFIED\\]" text)
    (check-regexp-match #rx"Python governance-controller suite" text)
    ;; A fully verified fixture list blocks nothing; one unverified entry blocks.
    (check-equal? (release-blocking (list (release-obligation 'fixture-a "fixture check A" #t))) '())
    (define mixed
      (list (release-obligation 'fixture-a "A" #t) (release-obligation 'fixture-b "B" #f)))
    (check-equal? (map release-obligation-id (release-blocking mixed)) '(fixture-b))
    (check-regexp-match #rx"^release unverified: 1 release obligation"
                        (release-unverified-message (release-blocking mixed))))
  (test-case "release explain lists the obligations next to the plan"
    (with-lane-root (lambda (dir)
                      (define text (join-lines (plan-explain-lines (resolve-lane-plan 'release dir))))
                      (check-regexp-match #rx"RELEASE OBLIGATIONS " text)
                      (check-regexp-match #rx"\\[NOT VERIFIED\\]" text)
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
           (check-regexp-match #rx"RELEASE OBLIGATION not-verified-1 \\[NOT VERIFIED\\]" text)
           ;; The FULL obligation list must be present, not just its first entry:
           ;; a preamble-only (or truncated) run cannot satisfy these.
           (check-regexp-match #rx"RELEASE OBLIGATION not-provided-6 \\[NOT VERIFIED\\]" text)
           (check-equal? (length (regexp-match* #rx"RELEASE OBLIGATION " text))
                         (length (release-obligations))
                         "every obligation is printed on its own line")
           (check-regexp-match #rx"qt: release unverified: [0-9]+ release obligation" err)
           (check-false (regexp-match? #rx"release passed:" text)
                        "an unverified obligation must never yield a release pass line")
           ;; The suite really ran its complete selection (7 fixture files), so
           ;; the failure is the obligation, not a skipped suite.
           (check-regexp-match
            #rx"release: starting the lane selection of the complete available Racket suite \\(release review\\) \\(7 files"
            out)))
        (displayln "SKIP release CLI: Linux /proc or setsid unavailable")))
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
                                    '("tests/a.rkt" "tests/c.rkt"))
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
           (check-regexp-match #rx"lane filter: " out)
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
                 (check-regexp-match #rx"^default passed: 3 files, 3 tests" (or run-pass-line ""))
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
    (for ([args (in-list '(("bogus") ("quick" "--changed")
                                     ("release" "--changed")
                                     ("default" "--bogus")
                                     ("default" "extra")
                                     ("full" "tests/test-qt-lanes.rkt")
                                     ("default" "--changed" "a" "b")))])
      (define-values (status out err) (apply invoke args))
      (check-not-equal? status 0 (format "~a should fail" args))
      (check-regexp-match #rx"^qt: " err)
      (check-false (regexp-match? #rx"passed:" out) (format "~a must not execute" args)))
    (let-values ([(status out err) (invoke "bogus")])
      (check-regexp-match #rx"only quick, full, default and release are available" err))
    (let-values ([(status out err) (invoke "release" "--changed")])
      (check-regexp-match #rx"release does not accept --changed" err))
    (let-values ([(status out err) (invoke "default" "--bogus")])
      (check-regexp-match #rx"unsupported option: --bogus" err))))
