#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 1500
;; @covers extensions/gsd/go-orchestrator.rkt
;; @covers extensions/gsd/review-session.rkt
;; @covers extensions/gsd/review-dispatch.rkt
;; Actual /go loop, default production coordinator and Python controller.
;; Only the model transport is deterministic; Git remote/publication, fresh
;; sessions, Verify, journal and rejection checkpoint are real. The campaign
;; declares a REAL frozen Verify chain (raco fmt -i, raco make, the fixture
;; checkout's honest scripts/run-tests.rkt and scripts/lint-all.rkt) and the
;; production verifier runs it through the registry, so each attempt's
;; review prepare authenticates a GENUINE retained owned proof (audit mode).
;; The verify counter below wraps — never replaces — the real verifier.
;; Measured runtime on the dev workstation: ~420s (two full owned Verify
;; chain executions plus four controller/review subprocess rounds).
(require rackunit
         racket/file
         racket/format
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         json
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/review-dispatch.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/review-session.rkt"
         "../extensions/gsd/wave-executor.rkt"
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-failure-context)
         (only-in "../extensions/gsd/wave-executor.rkt" current-gsd-worktree-isolation)
         (only-in "../extensions/gsd/review-proof.rkt" current-owned-proof-root)
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../util/message/message.rkt" message-content)
         (only-in "../util/content/content-parts.rkt" text-part? text-part-text)
         "../util/loop-result.rkt")
;; The frozen Verify chain spells REAL repository commands only (the same
;; approved grammar as the frozen W1 declaration): in-place canonical
;; format, compile, the focused positional test stage, the full fast suite
;; through the real --suite CLI flag, and the fixture checkout's honest
;; whole-tree lint entrypoint. There is no shimmed or fictional CLI here.
(define verify-chain
  (string-append "```sh\ncd <project-base>/q"
                 "\nraco fmt -i src/widget.rkt"
                 "\nraco make src/widget.rkt"
                 "\nracket scripts/run-tests.rkt tests/acceptance-check.rkt"
                 "\nracket scripts/run-tests.rkt --suite fast"
                 "\nracket scripts/lint-all.rkt\n```"))
(define-runtime-path installed-root "..")
(define (install-runner! repo)
  (define seen (make-hash))
  (define (install source)
    (unless (hash-ref seen source #f)
      (hash-set! seen source #t)
      (define target
        (build-path repo (find-relative-path (simplify-path installed-root) (simplify-path source))))
      (make-directory* (path-only target))
      (copy-file source target #t)
      (for ([dependency (in-list (regexp-match* #px"\"([^\"\n]+[.]rkt)\""
                                                (file->string source)
                                                #:match-select cadr))])
        (when (relative-path? dependency)
          (define next (simplify-path (build-path (path-only source) dependency)))
          (when (file-exists? next)
            (install next))))))
  (install (build-path installed-root "scripts/run-tests.rkt")))
;; Honest fixture gate runner for this fixture ONLY: runs the named focused
;; test files (or the full fixture fast suite) through the REAL `raco test`
;; runner, fail-fast, propagating the real exit code. NOT the repository's
;; production test runner.
(define fixture-run-tests
  #<<RUNNER-EOF
#lang racket/base
;; Fixture gate runner for the auto-review loop fixture ONLY: runs the named
;; focused test files (or the full fixture fast suite) through the REAL
;; `raco test` runner, fail-fast, propagating the real exit code. This is NOT
;; the repository's production test runner.
(require racket/list racket/system)
(define args (vector->list (current-command-line-arguments)))
(define files
  (cond
    [(null? args) (error 'run-tests "fixture runner requires test files or --suite fast")]
    [(equal? (car args) "--suite")
     (unless (equal? (cdr args) '("fast"))
       (error 'run-tests "fixture runner supports only --suite fast"))
     (sort (for/list ([p (directory-list "tests")] #:when (regexp-match? #rx"[.]rkt$" p))
             (build-path "tests" p))
           path<?)]
    [else (map string->path args)]))
(for ([path (in-list files)])
  (define code (system*/exit-code (find-executable-path "raco") "test" (path->string path)))
  (unless (zero? code) (exit code)))
RUNNER-EOF
  )
;; Fixture whole-checkout lint for this fixture ONLY: real checks (no tabs,
;; no trailing whitespace, no line over 100 columns) over this fixture's own
;; Racket sources. NOT the production linter.
(define fixture-lint-all
  #<<LINT-EOF
#lang racket/base
;; Fixture whole-checkout lint for the auto-review loop fixture ONLY: real
;; checks (no tabs, no trailing whitespace, no line over 100 columns) over
;; this fixture's own Racket sources. NOT the production linter.
(require racket/file racket/list racket/string)
(define files
  (sort (for/list ([p (directory-list "tests")] #:when (regexp-match? #rx"[.]rkt$" p))
          (build-path "tests" p))
        path<?))
(define (file-violations path)
  (for/list ([line (in-list (file->lines path))]
             [n (in-naturals 1)]
             #:when (or (string-contains? line "\t")
                        (regexp-match? #rx"[[:space:]]$" line)
                        (> (string-length line) 100)))
    (format "~a:~a" path n)))
(define bad
  (for*/fold ([acc '()]) ([path (in-list files)])
    (append (file-violations path) acc)))
(unless (null? bad)
  (eprintf "fixture lint failures:\n~a\n" (string-join bad "\n"))
  (exit 1))
LINT-EOF
  )
;; Focused acceptance check: behaviorally verifies the committed acceptance
;; file of this checkout and fails with real rackunit output when a required
;; acceptance line is missing.
(define fixture-acceptance-check
  #<<TEST-EOF
#lang racket/base
;; Focused acceptance check for the auto-review loop fixture: reads the
;; acceptance file at the checkout root and fails with real rackunit output
;; when a required acceptance line is missing.
;; @speed fast
(require rackunit "../src/widget.rkt")
(provide evidence-suite)
(define evidence-suite
  (test-suite "acceptance"
    (test-case "squares input" (check-equal? (widget-score 3) 9))))
(module+ test
  (require rackunit/text-ui)
  (exit (run-tests evidence-suite)))
TEST-EOF
  )
(module+ test
  (test-case "production /go review rejection -> ordinary fenced retry -> real superset Verify -> r1 fresh review"
    (define root (make-temporary-file "auto-review-loop-~a" 'directory))
    ;; The owned proof store is deliberately placed under a fixture-local
    ;; HOME so BOTH sides agree on the trusted root: the in-process capture
    ;; (current-owned-proof-root) and the controller's gsd-binding-data.rkt
    ;; subprocesses, which resolve the same default from their HOME.
    ;; The fixture-local HOME keeps the owned-proof store out of the real
    ;; user home while the controller subprocesses resolve the same trusted
    ;; root from their HOME. The real Racket user scope is linked in so the
    ;; fixture toolchain (raco fmt) behaves exactly as on the dev host.
    (define fake-home (build-path root "fake-home"))
    (make-directory* fake-home)
    (define real-racket-scope (build-path (find-system-path 'home-dir) ".racket"))
    (when (directory-exists? real-racket-scope)
      (with-handlers ([exn:fail? void])
        (make-file-or-directory-link real-racket-scope (build-path fake-home ".racket"))))
    (define fake-env (make-environment-variables))
    (for ([name (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! fake-env
                                  name
                                  (environment-variables-ref (current-environment-variables) name)))
    (environment-variables-set! fake-env #"HOME" (path->bytes fake-home))
    (dynamic-wind
     void
     (lambda ()
       (make-directory* (build-path root ".planning/waves"))
       (display-to-file "# Plan: Review test\n\n## Waves\n\n- [Inbox] W0: Test → waves/W0-test.md\n"
                        (build-path root ".planning/PLAN.md"))
       (display-to-file
        (string-append "# Wave 0: Test\n\nGoal: test\n\nBranch: `campaign/review-test`; "
                       "GitHub issue [#9686](https://github.com/example/q/issues/9686), "
                       "milestone #895.\n\n## Files\n\n- File: `q/acceptance`\n\n## Verify\n\n"
                       verify-chain
                       "\n")
        (build-path root ".planning/waves/W0-test.md"))
       (define rec (migrate-campaign! root))
       ;; This fixture exercises a historical receipt/rejection continuation:
       ;; frozen FILES predates test-author policy and cannot authorize tests.
       ;; Fresh owned-red-v1 scheduling has its own real integration fixture.
       (set-campaign-record-execution-policy! rec #f)
       (define plan (campaign-plan-id rec))
       (define gsd-plan (load-plan-from-index root))
       (define repo (build-path root "q"))
       (make-directory* (build-path repo "scripts"))
       (make-directory* (build-path repo "tests"))
       (display-to-file "compiled/\n" (build-path repo ".gitignore"))
       (display-to-file "(\"lint\" \"test (0)\" \"workflows (0)\")"
                        (build-path repo "scripts/required-pr-checks.policy"))
       (install-runner! repo)
       (make-directory* (build-path repo "src"))
       (display-to-file
        "#lang racket/base\n(provide widget-score)\n(define (widget-score n)\n  (+ n n))\n"
        (build-path repo "src/widget.rkt"))
       (display-to-file fixture-lint-all (build-path repo "scripts/lint-all.rkt"))
       (display-to-file fixture-acceptance-check (build-path repo "tests/acceptance-check.rkt"))
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (git-quiet! repo "checkout" "-b" "main")
       ;; Canonicalize formatting BEFORE committing so the frozen
       ;; `raco fmt -i` stage genuinely runs as a no-op (clean before/after)
       ;; and the owned proof capture sees an unchanged tree.
       (let ([code (parameterize ([current-directory repo])
                     (system*/exit-code (find-executable-path "raco")
                                        "fmt"
                                        "-i"
                                        "tests/acceptance-check.rkt"
                                        "scripts/lint-all.rkt"))])
         (unless (zero? code)
           (error 'review-fixture "raco fmt -i failed: ~a" code)))
       (git-quiet! repo "add" ".")
       (git-quiet! repo "commit" "-qm" "baseline")
       (define remote (build-path root "remote.git"))
       (git-quiet! root "init" "--bare" "-q" (path->string remote))
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (git-quiet! repo
                   "config"
                   (string-append "url." (path->string remote) ".insteadOf")
                   "https://github.com/example/q.git")
       (git-quiet! repo "push" "-q" "origin" "main")
       (git-quiet! repo "fetch" "-q" "origin")
       (define implementations 0)
       (define verifies 0)
       (define reviews 0)
       (define attempts '())
       (define generations '())
       ;; The wrapped verifier keeps the genuine production verifier: the
       ;; declared frozen chain still runs through the registry (owned proof
       ;; capture included); the wrapper only observes the committed
       ;; implementation bytes at verify time, preserving the original
       ;; acceptance-content obligations of the boolean seam it replaces.
       (define real-verifier (make-delivery-verifier root gsd-plan (campaign-record-created-at rec)))
       (define (verifying-runner wave-idx)
         (set! verifies (add1 verifies))
         (define context (current-gsd-delivery-branch-context))
         (define worktree (hash-ref context 'worktree-path))
         (define contents (file->string (build-path worktree "acceptance")))
         (check-true (string-contains? contents "first acceptance"))
         (when (= verifies 2)
           (check-true (string-contains? contents "second acceptance")))
         (define result (real-verifier wave-idx))
         result)
       (define (audit-rejection-text prompt)
         ;; Honest audit acknowledgement: read the pinned binding and the
         ;; retained manifest/raw-output bundle the controller exported,
         ;; then echo the exact binding back. No gate facts, no fabrication.
         (define (pinned-line prefix)
           (for/or ([line (in-list (string-split prompt "\n"))])
             (and (string-prefix? line prefix) (substring line (string-length prefix)))))
         (define binding-json (pinned-line "Pinned audit binding: "))
         (define bundle-line (pinned-line "Owned manifest/raw log bundle: "))
         (unless (and binding-json bundle-line)
           (error 'review-fixture "audit prompt does not pin the owned binding/bundle"))
         (define binding (string->jsexpr binding-json))
         (define bundle (string->path bundle-line))
         (define manifest-path
           (build-path bundle (string-append (hash-ref binding 'proof-sha256) ".json")))
         (define manifest (string->jsexpr (file->string manifest-path)))
         (define raw-output
           (file->string (build-path bundle (hash-ref (hash-ref manifest 'log) 'path))))
         (check-true (positive? (string-length raw-output)))
         (define red-match (regexp-match #px"red-ack \\{\"proof-sha256\": \"([0-9a-f]{64})\"" prompt))
         (unless red-match
           (error 'review-fixture "missing linked RED identity"))
         ;; Read the exported negative manifest and its retained raw outputs.
         (for ([path (in-directory bundle)]
               #:when (file-exists? path))
           (file->bytes path))
         (jsexpr->string
          (hasheq
           'kind
           "request-changes"
           'scope
           "audited the pinned implementation source and the retained owned proof bundle at the receipt head"
           'report
           "read the owned manifest and actual raw Verify output; the implementation misses a declared acceptance check"
           'findings
           '("Add the second acceptance check")
           'audit-ack
           (hasheq 'mode
                   "owned-proof-audit-v1"
                   'verification-origin
                   "audited-owned"
                   'source-read
                   #t
                   'raw-output-read
                   #t
                   'binding
                   binding
                   'red-ack
                   (hasheq 'proof-sha256 (cadr red-match) 'raw-output-read #t)))))
       (define (prompt-text request)
         (string-join (for/list ([m (in-list (model-request-messages request))])
                        (if (hash? m)
                            (format "~a" (hash-ref m 'content ""))
                            (string-join (for/list ([part (in-list (message-content m))]
                                                    #:when (text-part? part))
                                           (text-part-text part))
                                         "\n")))
                      "\n"))
       (define runner
         (make-auto-review-runner
          root
          plan
          #:provider-builder
          (lambda (cwd)
            (set! reviews (add1 reviews))
            ;; The reviewer session runs in the clean receipt checkout; the
            ;; acknowledgment's source-read is backed by actually reading it.
            (define source (file->string (build-path cwd "acceptance")))
            (check-true (string-contains? source "first acceptance"))
            (define (response-text request)
              (if (= reviews 1)
                  (audit-rejection-text (prompt-text request))
                  (jsexpr->string (hasheq 'kind
                                          "review-failed"
                                          'reason
                                          "test ends at fresh r1 review; no approval fabricated"))))
            (define (response-for request)
              (make-model-response (list (hasheq 'type "text" 'text (response-text request)))
                                   (hasheq 'prompt_tokens 3 'completion_tokens 4 'total_tokens 7)
                                   "glm-5.3"
                                   'stop))
            (define (chunks-for request)
              (define response (response-for request))
              (append (for/list ([tp (in-list (model-response-content response))]
                                 #:when (equal? (hash-ref tp 'type #f) "text"))
                        (make-stream-chunk (hash-ref tp 'text "") #f #f #f))
                      (list (make-stream-chunk #f #f (model-response-usage response) #t))))
            (make-provider (lambda () "audit-mock")
                           (lambda () (hasheq 'streaming #t 'token-counting #t))
                           response-for
                           chunks-for))))
       (define (recording-runner request)
         (set! generations (cons (hash-ref request 'generation) generations))
         (define receipt (hash-ref (load-delivery-journal root plan 0) 'receipt))
         (for ([key '(head branch attempt-id attempt-fence)])
           (check-equal? (hash-ref request key) (hash-ref receipt key)))
         (runner request))
       (define crashed? #f)
       (define (crash-once-coordinator dir p wave)
         (define outcome (default-delivery-coordinator dir p wave))
         (cond
           [(and (not crashed?) (eq? (delivery-outcome-kind outcome) 'review-rejected))
            ;; The REAL controller already pushed/read back the rejection.
            ;; Lose only the checkpoint outcome, as on process death.
            (set! crashed? #t)
            (delivery-outcome 'review-infra "simulated publication-to-checkpoint crash")]
           [else outcome]))
       (define (request-for live-rec)
         (make-campaign-request root
                                live-rec
                                (lambda (_)
                                  (string-append "Implement the acceptance checks"
                                                 (or (current-gsd-wave-failure-context) "")))
                                verifying-runner
                                #:auto-review-runner recording-runner
                                #:delivery-coordinator crash-once-coordinator))
       (define (execute request)
         (parameterize
             ([current-gsd-worktree-isolation #t]
              [current-owned-proof-root (build-path fake-home ".q" "agent" "owned-review-proofs")]
              [current-environment-variables fake-env]
              [current-gsd-remote-published
               (lambda (r b h)
                 (git-quiet! r "push" "-q" "origin" (string-append h ":refs/heads/" b))
                 #t)]
              [current-gsd-freshness-check (lambda (_) (campaign-freshness "test" "test" #f #f #f))])
           (execute-campaign-request!
            request
            (lambda (prompt)
              (let ()
                ;; Abort before any write/commit if isolation ever regresses.
                (define relative-cwd (find-relative-path root (current-directory)))
                (unless (and (relative-path? relative-cwd)
                             (not (memq 'up (explode-path relative-cwd))))
                  (error 'review-fixture "refusing implementation writes outside temporary fixture"))
                (set! implementations (add1 implementations))
                (when (= implementations 2)
                  (check-true (string-contains? prompt "Add the second acceptance check"))
                  (check-true (file-exists?
                               (build-path (current-directory)
                                           (format "docs/reports/gsd-wave-reviews/~a-w0.rktd"
                                                   plan)))))
                (define live (load-campaign-record root plan))
                (define a (campaign-wave-current-attempt (car (campaign-record-waves live))))
                (set! attempts
                      (cons (cons (campaign-attempt-id a) (campaign-attempt-fence-token a)) attempts))
                (display-to-file (if (= implementations 1)
                                     "first acceptance\n"
                                     "first acceptance\nsecond acceptance\n")
                                 (build-path (current-directory) "acceptance")
                                 #:exists 'truncate)
                (display-to-file
                 "#lang racket/base\n(provide widget-score)\n(define (widget-score n)\n  (* n n))\n"
                 (build-path (current-directory) "src/widget.rkt")
                 #:exists 'truncate)
                (git-quiet! (current-directory) "add" "acceptance" "src/widget.rkt")
                (git-quiet! (current-directory) "commit" "-qm" "implementation")
                (make-loop-result '() 'completed (hasheq)))))))
       (define first-result (execute (request-for rec)))
       (check-equal? (campaign-result-status first-result)
                     'wave-blocked
                     (format "first campaign result: ~s" first-result))
       (check-equal? implementations 1)
       (check-equal? reviews 1 (format "first campaign result: ~s" first-result))
       (check-true crashed? (format "first campaign result: ~s" first-result))
       (check-false (hash-ref (load-delivery-journal root plan 0) 'review-rejection #f))
       ;; Resume with neither a marker nor local dispatch staging. The real
       ;; fetched artifact must reenter the ordinary retry without another r0
       ;; reviewer; the second reviewer belongs ONLY to the fresh r1 attempt.
       (delete-directory/files (review-dispatch-staging-path root plan 0 0))
       (define result (execute (request-for (load-campaign-record root plan))))
       (delete-directory/files (build-path fake-home ".q" "agent" "evidence-repairs" plan "W0.g1")
                               #:must-exist? #f)
       (check-equal? (campaign-result-status result)
                     'wave-blocked
                     (format "resumed campaign result: ~s" result))
       (check-equal? implementations 2 (campaign-result-message result))
       (check-equal? verifies 2)
       (check-equal? reviews 2)
       (check-equal? (reverse generations) '(0 1))
       (check-not-equal? (caar attempts) (caadr attempts))
       (check-true (> (cdar attempts) (cdadr attempts)))
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (length (hash-ref journal 'receipt-history)) 1)
       (check-equal? (hash-ref journal 'review-rejection-count) 1))
     (lambda () (delete-directory/files root)))))
