#lang racket/base

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; tests/test-gsd-command-handlers-unit.rkt — Unit tests for GSD command parsing & dispatch (T-6)
;; Tests the pure parse-gsd-command and dispatch-gsd-command functions.

(require racket/base
         racket/file
         racket/path
         racket/string
         rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/plan-types.rkt"
                  gsd-plan
                  gsd-plan-waves
                  gsd-wave
                  gsd-wave-status)
         "../extensions/gsd/command-parser.rkt"
         "../extensions/gsd/command-handlers.rkt"
         (only-in "../extensions/gsd/planning-artifact-validation.rkt"
                  validate-plan-artifacts
                  plan-validation-finding-code
                  plan-validation-finding-message
                  plan-validation-finding-wave-idx)
         (only-in "../tools/scratch.rkt"
                  current-scratch-base-dir
                  reset-session-scratch-registry!
                  allocate-session-scratch-root!))

(define (make-scratch-prompt-fixture)
  (define dir (make-temporary-file "scratch-prompt-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display
      "# Plan: Scratch Guidance Test\n\n## Wave 0: W0 test\n- [Inbox] W0: W0 test → waves/W0-wave.md\n"
      out)))
  (call-with-output-file
   (build-path dir ".planning" "waves" "W0-wave.md")
   (lambda (out) (display "# Wave 0\nStatus: PENDING\n\n# W0 test\n\n- File: q/some-file.rkt\n" out)))
  dir)

(define (make-scratch-test-plan)
  (gsd-plan (list (gsd-wave 0
                            "W0: test wave"
                            'pending
                            "root cause"
                            (list "q/some-file.rkt")
                            '()
                            "raco test"
                            '("done")))
            #f
            '()
            '()))

;; A complete, otherwise-preflight-clean plan whose only variable is the
;; PLAN.md row's bracket status spelling.
(define (make-status-plan-fixture status)
  (define dir (make-temporary-file "status-preflight-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display (format "# Plan\n\n- [~a] W0: status probe → waves/W0-status-probe.md\n" status)
              out))
   #:exists 'truncate)
  (call-with-output-file
   (build-path dir ".planning" "waves" "W0-status-probe.md")
   (lambda (out)
     (display (string-append "# Wave 0\nStatus: Inbox\n\n"
                             "## Files\n- File: q/status-probe.rkt\n\n"
                             "## Verify\n- `printf 'status-probe'` green.\n\n"
                             "## Done\n- status probe done\n")
              out))
   #:exists 'truncate)
  dir)

(define suite
  (test-suite "GSD command parsing + dispatch unit tests"

    ;; === parse-gsd-command ===

    (test-case "parse /go → gsd-cmd-go"
      (define cmd (parse-gsd-command "/go" "/go"))
      (check-not-false cmd)
      (check-true (gsd-cmd-go? cmd))
      (check-equal? (parsed-gsd-command-canonical-name cmd) "/go"))

    (test-case "parse /go alias /i → gsd-cmd-go"
      (define cmd (parse-gsd-command "/i" "/i"))
      (check-not-false cmd)
      (check-true (gsd-cmd-go? cmd)))

    (test-case "parse /go alias /implement → gsd-cmd-go"
      (define cmd (parse-gsd-command "/implement" "/implement"))
      (check-not-false cmd)
      (check-true (gsd-cmd-go? cmd)))

    (test-case "gsd-command-handlers-unit: parse /go with wave arg"
      (define cmd (parse-gsd-command "/go" "/go 3"))
      (check-not-false cmd)
      (check-true (gsd-cmd-go? cmd))
      (check-equal? (gsd-cmd-go-wave-arg cmd) "3"))

    (test-case "parse /done → gsd-cmd-done"
      (define cmd (parse-gsd-command "/done" "/done"))
      (check-not-false cmd)
      (check-true (gsd-cmd-done? cmd))
      (check-false (gsd-cmd-done-force? cmd)))

    (test-case "parse /done --force → gsd-cmd-done with force"
      (define cmd (parse-gsd-command "/done" "/done --force"))
      (check-not-false cmd)
      (check-true (gsd-cmd-done? cmd))
      (check-true (gsd-cmd-done-force? cmd)))

    (test-case "parse /gsd → gsd-cmd-status"
      (define cmd (parse-gsd-command "/gsd" "/gsd"))
      (check-not-false cmd)
      (check-true (gsd-cmd-status? cmd)))

    (test-case "parse /replan → gsd-cmd-replan"
      (define cmd (parse-gsd-command "/replan" "/replan"))
      (check-not-false cmd)
      (check-true (gsd-cmd-replan? cmd)))

    (test-case "parse /reset → gsd-cmd-reset"
      (define cmd (parse-gsd-command "/reset" "/reset"))
      (check-not-false cmd)
      (check-true (gsd-cmd-reset? cmd)))

    (test-case "parse /skip → gsd-cmd-skip"
      (define cmd (parse-gsd-command "/skip" "/skip 2"))
      (check-not-false cmd)
      (check-true (gsd-cmd-skip? cmd))
      (check-equal? (gsd-cmd-skip-skip-arg cmd) "2"))

    (test-case "parse /wave-done → gsd-cmd-wave-done"
      (define cmd (parse-gsd-command "/wave-done" "/wave-done 1"))
      (check-not-false cmd)
      (check-true (gsd-cmd-wave-done? cmd)))

    (test-case "parse /wave-done alias /wd"
      (define cmd (parse-gsd-command "/wd" "/wd 1"))
      (check-not-false cmd)
      (check-true (gsd-cmd-wave-done? cmd)))

    (test-case "parse /plan without text → artifact display"
      (define cmd (parse-gsd-command "/plan" "/plan"))
      (check-not-false cmd)
      (check-true (gsd-cmd-plan? cmd))
      (check-false (gsd-cmd-plan-plan-text cmd)))

    (test-case "parse /plan with text → plan submit"
      (define cmd (parse-gsd-command "/plan" "/plan implement feature X"))
      (check-not-false cmd)
      (check-true (gsd-cmd-plan? cmd))
      (check-equal? (gsd-cmd-plan-plan-text cmd) "implement feature X"))

    (test-case "parse /state → artifact"
      (define cmd (parse-gsd-command "/state" "/state"))
      (check-not-false cmd)
      (check-true (gsd-cmd-artifact? cmd))
      (check-equal? (gsd-cmd-artifact-artifact-name cmd) "STATE"))

    (test-case "parse /handoff → artifact"
      (define cmd (parse-gsd-command "/handoff" "/handoff"))
      (check-not-false cmd)
      (check-true (gsd-cmd-artifact? cmd))
      (check-equal? (gsd-cmd-artifact-artifact-name cmd) "HANDOFF"))

    (test-case "parse /p alias → plan"
      (define cmd (parse-gsd-command "/p" "/p"))
      (check-not-false cmd)
      (check-true (gsd-cmd-plan? cmd)))

    (test-case "parse unknown command → #f"
      (define cmd (parse-gsd-command "/unknown" "/unknown"))
      (check-false cmd))

    ;; === dispatch-gsd-command ===

    (test-case "dispatch /go → 'go action"
      (define parsed (parse-gsd-command "/go" "/go"))
      (define-values (action data) (dispatch-gsd-command parsed "/go" "/tmp"))
      (check-equal? action 'go))

    (test-case "dispatch /gsd → 'status action"
      (define parsed (parse-gsd-command "/gsd" "/gsd"))
      (define-values (action data) (dispatch-gsd-command parsed "/gsd" "/tmp"))
      (check-equal? action 'status))

    (test-case "dispatch /replan → 'replan action"
      (define parsed (parse-gsd-command "/replan" "/replan"))
      (define-values (action data) (dispatch-gsd-command parsed "/replan" "/tmp"))
      (check-equal? action 'replan)
      (check-true (gsd-cmd-replan? data)))

    (test-case "dispatch /skip → 'skip action"
      (define parsed (parse-gsd-command "/skip" "/skip 2"))
      (define-values (action data) (dispatch-gsd-command parsed "/skip 2" "/tmp"))
      (check-equal? action 'skip))

    (test-case "dispatch /done → 'done action"
      (define parsed (parse-gsd-command "/done" "/done"))
      (define-values (action data) (dispatch-gsd-command parsed "/done" "/tmp"))
      (check-equal? action 'done))

    (test-case "dispatch /plan with text → 'plan-submit action"
      (define parsed (parse-gsd-command "/plan" "/plan do thing"))
      (define-values (action data) (dispatch-gsd-command parsed "/plan do thing" "/tmp"))
      (check-equal? action 'plan-submit)
      (check-equal? data "do thing"))

    (test-case "dispatch /plan without text → 'artifact action"
      (define parsed (parse-gsd-command "/plan" "/plan"))
      (define-values (action data) (dispatch-gsd-command parsed "/plan" "/tmp"))
      (check-equal? action 'artifact))

    ;; === gsd-command-specs ===

    (test-case "gsd-command-specs has expected entries"
      (define canonicals (map gsd-command-spec-canonical gsd-command-specs))
      (for ([expected '("/plan" "/state"
                                "/handoff"
                                "/go"
                                "/wave-done"
                                "/done"
                                "/replan"
                                "/skip"
                                "/reset"
                                "/gsd")])
        (check-not-false (member expected canonicals) (format "Expected ~a in specs" expected))))

    (test-case "aliases-for returns list with canonical"
      (define aliases (aliases-for "/go"))
      (check-not-false (member "/go" aliases))
      (check-not-false (member "/i" aliases))
      (check-not-false (member "/implement" aliases)))

    ;; === retry-with-adaptation: extract-last-failure ===

    (test-case "extract-last-failure returns reason from wave doc"
      (define doc
        "# Wave 3\nStatus: FAILED\n\n## Last Failure\ndelivery verification failed: no wave target files changed\n## Action\nstep")
      (check-equal? (extract-last-failure doc)
                    "delivery verification failed: no wave target files changed"))

    (test-case "extract-last-failure handles failure section at end of doc"
      (define doc "# Wave 3\nStatus: FAILED\n\n## Last Failure\nreason at end")
      (check-equal? (extract-last-failure doc) "reason at end"))

    (test-case "extract-last-failure returns empty when no failure section"
      (define doc "# Wave 1\nStatus: Inbox\n## Action\nstep")
      (check-equal? (extract-last-failure doc) ""))

    (test-case "extract-last-failure multi-line reason"
      (define doc "## Last Failure\nline one\nline two\n## Next")
      (check-equal? (extract-last-failure doc) "line one\nline two"))

    ;; ── W2: one trusted session scratch root in the executor prompt ──

    (test-case "single-wave prompt embeds the trusted scratch root only when a token or root is supplied"
      (define dir (make-scratch-prompt-fixture))
      (define base (make-temporary-file "q-gsd-scratch-handlers-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (parameterize ([current-scratch-base-dir base])
                        (reset-session-scratch-registry!)
                        (define plan (make-scratch-test-plan))
                        (define plain (build-single-wave-prompt dir plan 0))
                        (check-false (string-contains? plain "Session scratch directory")
                                     "no token → no scratch section (backward-compatible bytes)")
                        (define expected
                          (path->string
                           (allocate-session-scratch-root! "0123456789abcdef" "0123456789abcdef")))
                        (define guided
                          (build-single-wave-prompt dir
                                                    plan
                                                    0
                                                    #:scratch-token "0123456789abcdef"
                                                    #:scratch-root-abs
                                                    "/tmp/abs/.planning/scratch/legacy"))
                        (check-true (string-contains? guided (format "~a" expected))
                                    "one concrete trusted absolute path")
                        (check-false (string-contains? guided ".planning/scratch")
                                     "the obsolete .planning scratch path is retired")
                        (check-false (string-contains? guided "/tmp/abs/.planning/scratch/legacy")
                                     "a legacy absolute root is not advertised")
                        ;; W2: when the campaign interpreter hands the prompt
                        ;; builder the exact root allocated for THIS executor
                        ;; session, that exact root is what the prompt
                        ;; advertises (no re-derivation, no shared process root).
                        (define session-root
                          (allocate-session-scratch-root! "wave-attempt-token" "fedcba9876543210"))
                        (define exact
                          (build-single-wave-prompt dir
                                                    plan
                                                    0
                                                    #:scratch-token "wave-attempt-token"
                                                    #:scratch-root-abs session-root))
                        (check-true (string-contains? exact (path->string session-root))
                                    "the session's own allocated root is advertised")
                        (check-false (string-contains? exact expected)
                                     "another session's root is never advertised")
                        (reset-session-scratch-registry!)))
                    (lambda ()
                      (delete-directory/files dir #:must-exist? #f)
                      (delete-directory/files base #:must-exist? #f))))

    (test-case "fresh executor scratch tokens are opaque 16-hex and distinct"
      (define a (fresh-executor-scratch-token))
      (define b (fresh-executor-scratch-token))
      (check-equal? (string-length a) 16)
      (check-true (regexp-match? #px"^[0-9a-f]{16}$" a))
      (check-false (string=? a b) "fresh sessions must not share a scratch token"))

    ;; === W5: typed unknown status at the /go preflight seam ===

    (test-case "W5: unknown row status is a stable preflight finding and /go rejects it"
      (define dir (make-status-plan-fixture "REWORK"))
      (dynamic-wind
       void
       (lambda ()
         (define kernel (validate-plan-artifacts dir))
         (check-false (hash-ref kernel 'ok?) "unknown status is blocking, never a pending wave")
         (check-not-false (member 'wave-status-unknown (hash-ref kernel 'codes))
                          (format "stable machine code expected; got ~s" (hash-ref kernel 'codes)))
         (define finding
           (for/first ([f (in-list (hash-ref kernel 'findings))]
                       #:when (eq? (plan-validation-finding-code f) 'wave-status-unknown))
             f))
         (check-not-false finding "the unknown status is a named finding")
         (when finding
           (check-equal? (plan-validation-finding-wave-idx finding) 0)
           (check-true (string-contains? (plan-validation-finding-message finding) "W0"))
           (check-true (string-contains? (plan-validation-finding-message finding) "REWORK")))
         ;; /go's own seam turns the typed diagnostic into an ordinary error
         ;; result (no uncaught exception, no dispatch).
         (define go-result (validate-plan-for-go dir))
         (check-equal? (car go-result) 'error)
         (check-true (string-contains? (cadr go-result) "unknown wave status"))
         (check-true (string-contains? (cadr go-result) "REWORK")))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "W5: legacy status aliases keep their canonical meaning in preflight"
      (define dir (make-status-plan-fixture "COMPLETED"))
      (dynamic-wind
       void
       (lambda ()
         (define kernel (validate-plan-artifacts dir))
         (check-true (hash-ref kernel 'ok?) (hash-ref kernel 'error-message))
         (define wave (car (gsd-plan-waves (hash-ref kernel 'plan))))
         (check-eq? (gsd-wave-status wave) 'completed))
       (lambda () (delete-directory/files dir #:must-exist? #f))))))

(run-tests suite)
