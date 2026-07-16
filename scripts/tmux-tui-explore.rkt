#!/usr/bin/env racket
#lang racket/base

;; scripts/tmux-tui-explore.rkt — Official exploratory tmux TUI runner
;;
;; This command is intentionally conservative. Mock mode is local/no-network and
;; suitable for fast developer checks. Real-provider mode refuses to run unless
;; explicit cost/credential gates are present.

(require json
         racket/cmdline
         racket/date
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string
         "../util/credential-redaction.rkt"
         "tmux-explore/executor.rkt"
         "tmux-explore/verifiers.rkt")

(provide (struct-out explore-scenario)
         (struct-out explore-result)
         scenario-registry
         find-scenarios
         valid-mode?
         real-provider-authorized?
         require-real-provider-authorization!
         redact-explore-text
         make-explore-root
         render-scenario-markdown
         write-explore-report!
         write-summary-files!
         run-exploration
         real-results-gating-pass?
         exploration-exit-code
         print-scenario-list)

(struct explore-scenario (tag title description prompt expected-status real-tools?) #:transparent)
(struct explore-result
        (tag title mode status classification report-path evidence started-at completed-at)
  #:transparent)

(define scenario-registry
  (list
   (explore-scenario "memory"
                     "Memory/context"
                     "Session-context recall and context-source truthfulness."
                     "Remember that the codename is blue-harbor, then identify its source."
                     'pass-mock-tui
                     #f)
   (explore-scenario "gsd"
                     "GSD planning"
                     "Wave planning with explicit gate evidence and no fabricated PASS."
                     "Create W0/W1 plan and revise it for red CI without claiming unverified PASS."
                     'pass-mock-tui
                     #f)
   (explore-scenario "mas"
                     "MAS/subagents"
                     "Subagent-oriented review scenario; W6 adds lifecycle-event proof."
                     "Use subagents or explain their unavailability, then aggregate results."
                     'pass-mock-tui-with-caveat
                     #f)
   (explore-scenario "tools"
                     "Tools/approval"
                     "Read-only tool scenario over README_FIXTURE_ALPHA."
                     "Inspect README.md only and report whether README_FIXTURE_ALPHA is present."
                     'pass-mock-tui
                     #t)
   (explore-scenario "release-audit"
                     "Release/audit truth"
                     "Release authorization refusal without live CI/release evidence."
                     "Decide whether release is authorized when no live CI evidence is available."
                     'pass-mock-tui-partial
                     #f)
   (explore-scenario
    "durable-memory"
    "Durable memory restart"
    "Store, resume, and retrieve one correlated durable-memory item."
    "Persist the codename amber-quay, restart the session, and retrieve it without restating it."
    'pass-mock-tui-with-caveat
    #f)
   (explore-scenario "resume"
                     "Session resume"
                     "Resume the exact persisted session with structured continuity evidence."
                     "Resume this session and report the prior session identifier."
                     'pass-mock-tui-with-caveat
                     #f)
   (explore-scenario "compact"
                     "Durable compaction"
                     "Run compaction and require a terminal persisted lifecycle event."
                     "/compact"
                     'pass-mock-tui-with-caveat
                     #f)
   (explore-scenario "interrupt"
                     "Functional interruption"
                     "Cancel one active turn, observe correlated acknowledgement, then recover."
                     "Produce a detailed multi-section explanation of cooperative cancellation."
                     'pass-mock-tui-with-caveat
                     #f)))

(define (valid-mode? mode)
  (member mode '(mock real)))

(define (mode->string mode)
  (case mode
    [(mock) "mock"]
    [(real) "real"]
    [else (format "~a" mode)]))

(define (parse-mode str)
  (cond
    [(equal? str "mock") 'mock]
    [(equal? str "real") 'real]
    [else (error 'tmux-tui-explore "invalid mode: ~a" str)]))

(define (find-scenarios #:filter [filter-tag #f])
  (cond
    [(not filter-tag) scenario-registry]
    [else (filter (lambda (s) (equal? (explore-scenario-tag s) filter-tag)) scenario-registry)]))

(define (real-provider-authorized?)
  (and (equal? (getenv "Q_TMUX_TUI_TESTS") "1")
       (equal? (getenv "Q_TMUX_TUI_REAL_PROVIDER") "1")
       (equal? (getenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM") "I_UNDERSTAND_COSTS")))

(define (require-real-provider-authorization!)
  (unless (real-provider-authorized?)
    (error 'tmux-tui-explore
           (string-append "real-provider mode requires Q_TMUX_TUI_TESTS=1, "
                          "Q_TMUX_TUI_REAL_PROVIDER=1, and "
                          "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM=I_UNDERSTAND_COSTS"))))

(define redact-explore-text redact-secrets)

(define (make-explore-root [base-dir (find-system-path 'temp-dir)])
  (define root (make-temporary-file "q-tmux-explore-~a" 'directory base-dir))
  (path->string root))

(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))

(define (scenario->mock-evidence scenario)
  (case (string->symbol (explore-scenario-tag scenario))
    [(memory)
     '(("completion" . "structured trace completion: stream.turn.completed")
       ("context" . "blue-harbor retained as session context only")
       ("durability" . "not claimed")
       ("classification" . "W9: classify-result-status maps status to pass/partial/flake/etc."))]
    [(gsd)
     '(("completion" . "mock turn completed")
       ("waves" . "W0/W1 with gates")
       ("truth" . "no executed PASS claimed")
       ("transition-trace" . "W8: gsd.transition.attempted/succeeded phases in trace.jsonl")
       ("wave-trace" . "W8: gsd.wave.started/completed phases in trace.jsonl")
       ("plan-trace" . "W8: gsd.plan.parsed/validated phases in trace.jsonl")
       ("lifecycle" . "W8: parse-gsd-lifecycle + verify-gsd-transition-succeeded"))]
    [(mas)
     '(("completion" . "mock turn completed")
       ("caveat" . "lifecycle events deferred to W6")
       ("truth" . "prose-only evidence is not final PASS")
       ("spawn-trace" . "W6: mas.spawn-approval-requested phase in trace.jsonl")
       ("lifecycle" . "W6: verify-subagent-spawn-lifecycle cross-references spawn+tool-exec"))]
    [(tools)
     '(("completion" . "mock turn completed")
       ("fixture" . "README_FIXTURE_ALPHA")
       ("mutation" . "no file modifications")
       ("tool-trace" . "W5: tool.execution.started/completed phases in trace.jsonl")
       ("artifact-truth" . "W5: fingerprint verify-file-unchanged! for read-only scenarios"))]
    [(release-audit)
     '(("completion" . "mock turn completed")
       ("authorization" . "refused without live CI/release evidence")
       ("required" . "release-manifest.json")
       ("refusal-detection" . "W8: detect-release-authorization-refusal scans for refusal patterns")
       ("manifest-verify" . "W8: verify-release-manifest-present! checks file exists")
       ("truth" . "W8: verify-release-authorization-refused! asserts refusal in captured text"))]
    [(durable-memory)
     '(("completion" . "mock turn completed")
       ("status" . "W7: durable-memory-roundtrip helpers available")
       ("store-trace" . "W7: memory.item.stored phase in trace.jsonl")
       ("retrieval-trace" . "W7: memory.retrieval.performed phase in trace.jsonl")
       ("restart-trace" . "W7: session.started with reason resume in trace.jsonl")
       ("roundtrip" . "W7: verify-durable-memory-roundtrip checks store+restart+retrieval")
       ("truth" . "trace evidence required, not prose")
       ("reporting" . "W9: render-bundle-index-markdown generates structured summary table"))]
    [else '(("completion" . "mock turn completed"))]))

(define (scenario-status mode scenario)
  (cond
    [(eq? mode 'mock) (explore-scenario-expected-status scenario)]
    [(equal? (explore-scenario-tag scenario) "durable-memory") 'pass-mock-tui-with-caveat]
    [else 'requires-real-provider-run]))

(define (scenario-classification status)
  (case status
    [(pass-mock-tui) 'pass]
    [(pass-mock-tui-with-caveat pass-mock-tui-partial) 'partial]
    [(unsupported-until-w7 unsupported) 'unsupported]
    [(requires-real-provider-run) 'pending-real-run]
    [else 'unknown]))

(define (alist-ref key alist [default #f])
  (define found (assoc key alist))
  (if found
      (cdr found)
      default))

(define (render-scenario-markdown result)
  (define evidence (explore-result-evidence result))
  (string-append
   (format "# tmux TUI exploration — ~a~n~n" (explore-result-title result))
   (format "**Scenario:** `~a`  ~n" (explore-result-tag result))
   (format "**Mode:** `~a`  ~n" (mode->string (explore-result-mode result)))
   (format "**Status:** `~a`  ~n" (explore-result-status result))
   (format "**Classification:** `~a`  ~n" (explore-result-classification result))
   (format "**Started:** ~a  ~n" (explore-result-started-at result))
   (format "**Completed:** ~a  ~n~n" (explore-result-completed-at result))
   "## Evidence\n\n"
   (apply string-append
          (for/list ([item (in-list evidence)])
            (format "- **~a:** ~a~n"
                    (car item)
                    (redact-explore-text (format "~a" (redact-credential-data (cdr item)))))))
   "\n## Safety\n\n"
   "```text\n"
   "real-provider mode is explicit opt-in only\n"
   "temp HOME/project isolation required for real mode\n"
   "credential-like values are redacted before report writing\n"
   "automation should wait for trace.jsonl turn.completed or stream.turn.completed, not visible q>\n"
   "```\n"))

(define (safe-file-tag tag)
  (regexp-replace* #px"[^A-Za-z0-9_.-]+" tag "-"))

(define (write-explore-report! root result)
  (make-directory* root)
  (define report-path
    (build-path root
                (format "~a-~a.md"
                        (mode->string (explore-result-mode result))
                        (safe-file-tag (explore-result-tag result)))))
  (call-with-output-file report-path
                         (lambda (out)
                           (display (redact-explore-text (render-scenario-markdown result)) out))
                         #:exists 'replace)
  (struct-copy explore-result result [report-path (path->string report-path)]))

(define (safe-output-field value)
  (redact-explore-text (format "~a" value)))

(define (result->hash result)
  (redact-credential-data (hasheq 'tag
                                  (explore-result-tag result)
                                  'title
                                  (explore-result-title result)
                                  'mode
                                  (mode->string (explore-result-mode result))
                                  'status
                                  (format "~a" (explore-result-status result))
                                  'classification
                                  (format "~a" (explore-result-classification result))
                                  'report_path
                                  (or (explore-result-report-path result) "")
                                  'started_at
                                  (explore-result-started-at result)
                                  'completed_at
                                  (explore-result-completed-at result))))

(define (write-summary-files! root results)
  (make-directory* root)
  (define tsv-path (build-path root "summary.tsv"))
  (define json-path (build-path root "summary.json"))
  (call-with-output-file tsv-path
                         (lambda (out)
                           (fprintf out "tag\ttitle\tmode\tstatus\tclassification\treport_path~n")
                           (for ([r (in-list results)])
                             (fprintf out
                                      "~a\t~a\t~a\t~a\t~a\t~a~n"
                                      (safe-output-field (explore-result-tag r))
                                      (safe-output-field (explore-result-title r))
                                      (safe-output-field (mode->string (explore-result-mode r)))
                                      (safe-output-field (explore-result-status r))
                                      (safe-output-field (explore-result-classification r))
                                      (safe-output-field (or (explore-result-report-path r) "")))))
                         #:exists 'replace)
  (call-with-output-file json-path
                         (lambda (out) (write-json (map result->hash results) out))
                         #:exists 'replace)
  (values (path->string tsv-path) (path->string json-path)))

(define (append-central-log! path results)
  (make-directory* (or (let-values ([(base name dir?) (split-path path)])
                         (and (path? base) base))
                       (current-directory)))
  (call-with-output-file path
                         (lambda (out)
                           (fprintf out "~n## Explorer run — ~a~n~n" (timestamp))
                           (for ([r (in-list results)])
                             (fprintf out
                                      "- `~a` mode=`~a` status=`~a` classification=`~a` report=`~a`~n"
                                      (safe-output-field (explore-result-tag r))
                                      (safe-output-field (mode->string (explore-result-mode r)))
                                      (safe-output-field (explore-result-status r))
                                      (safe-output-field (explore-result-classification r))
                                      (safe-output-field (or (explore-result-report-path r) "")))))
                         #:exists 'append))

(define (default-real-executor scenario root)
  (run-real-scenario (explore-scenario-tag scenario)
                     (explore-scenario-prompt scenario)
                     root
                     #:tools? (explore-scenario-real-tools? scenario)))

(define (failed-executor-observation error)
  (hash 'status
        'failed
        'trace-events
        '()
        'capture
        ""
        'mock-provider?
        #f
        'timed-out?
        #f
        'crashed?
        #t
        'error
        (exn-message error)))

(define (real-result-evidence observation verification)
  (list (cons "evidence-source" "observed isolated tmux/q execution")
        (cons "execution-status" (hash-ref observation 'status 'failed))
        (cons "trace-event-count" (length (hash-ref observation 'trace-events '())))
        (cons "verifier"
              (if (verification-result-passed? verification)
                  "correlated semantic lifecycle evidence matched"
                  (string-join (verification-result-reasons verification) "; ")))
        (cons "artifact" (hash-ref observation 'artifact-path "no retained artifact"))))

(define (run-one-scenario scenario mode root executor)
  (define started (timestamp))
  (define result
    (cond
      [(eq? mode 'mock)
       (define status (scenario-status mode scenario))
       (explore-result (explore-scenario-tag scenario)
                       (explore-scenario-title scenario)
                       mode
                       status
                       (scenario-classification status)
                       #f
                       (cons '("evidence-source" . "deterministic mock; never valid for real PASS")
                             (scenario->mock-evidence scenario))
                       started
                       (timestamp))]
      [else
       (define observation
         (with-handlers ([exn:fail? failed-executor-observation])
           (executor scenario root)))
       (define verification (verify-scenario-evidence (explore-scenario-tag scenario) observation))
       (define passed? (verification-result-passed? verification))
       (define execution-status (hash-ref observation 'status 'failed))
       (explore-result (explore-scenario-tag scenario)
                       (explore-scenario-title scenario)
                       mode
                       (if passed?
                           'pass
                           (if (eq? execution-status 'completed) 'failed-verifier execution-status))
                       (if passed? 'pass 'fail)
                       #f
                       (real-result-evidence observation verification)
                       started
                       (timestamp))]))
  (write-explore-report! root result))

(define (run-exploration #:mode [mode 'mock]
                         #:filter [filter-tag #f]
                         #:out [out-dir #f]
                         #:append-log [append-log #f]
                         #:executor [executor default-real-executor])
  (unless (valid-mode? mode)
    (error 'tmux-tui-explore "invalid mode: ~a" mode))
  (when (eq? mode 'real)
    (require-real-provider-authorization!))
  (define scenarios (find-scenarios #:filter filter-tag))
  (when (null? scenarios)
    (error 'tmux-tui-explore "no scenarios match filter: ~a" filter-tag))
  (define root (or out-dir (make-explore-root)))
  (make-directory* root)
  (define results
    (for/list ([scenario (in-list scenarios)])
      (run-one-scenario scenario mode root executor)))
  (define-values (_tsv _json) (write-summary-files! root results))
  (when append-log
    (append-central-log! append-log results))
  results)

(define (real-results-gating-pass? results)
  (and (pair? results)
       (andmap (lambda (result)
                 (and (eq? (explore-result-mode result) 'real)
                      (eq? (explore-result-status result) 'pass)
                      (eq? (explore-result-classification result) 'pass)))
               results)))

(define (exploration-exit-code mode results non-gating?)
  (cond
    [(not (eq? mode 'real)) 0]
    [non-gating? 0]
    [(real-results-gating-pass? results) 0]
    [else 1]))

(define (print-scenario-list [out (current-output-port)])
  (fprintf out "Available tmux TUI exploration scenarios:~n")
  (for ([s (in-list scenario-registry)])
    (fprintf out
             "  ~a~a~a~n"
             (explore-scenario-tag s)
             (make-string (max 1 (- 18 (string-length (explore-scenario-tag s)))) #\space)
             (explore-scenario-description s))))

(define (print-summary results out-dir)
  (printf "=== q tmux TUI explorer ===~n")
  (printf "Output: ~a~n" out-dir)
  (for ([r (in-list results)])
    (printf "  ~a mode=~a status=~a classification=~a~n"
            (explore-result-tag r)
            (mode->string (explore-result-mode r))
            (explore-result-status r)
            (explore-result-classification r)))
  (printf "Summary: ~a scenario(s)~n" (length results)))

(module+ main
  (define list-only? #f)
  (define mode-str "mock")
  (define filter-tag #f)
  (define out-dir #f)
  (define append-log #f)
  (define non-gating? #f)
  (command-line
   #:program "tmux-tui-explore"
   #:once-each [("-l" "--list") "List scenarios without running" (set! list-only? #t)]
   [("-m" "--mode") mode "Mode: mock or real (default: mock)" (set! mode-str mode)]
   [("-f" "--filter") tag "Run only one scenario tag" (set! filter-tag tag)]
   [("-o" "--out") dir "Output directory for reports" (set! out-dir dir)]
   [("--append-log") path "Append compact ledger entry to a markdown log" (set! append-log path)]
   [("--non-gating") "Report real non-PASS outcomes without a nonzero exit" (set! non-gating? #t)]
   #:args ()
   (void))
  (cond
    [list-only? (print-scenario-list)]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (eprintf "tmux-tui-explore: ~a~n" (exn-message e))
                                  (exit 2))])
       (define mode (parse-mode mode-str))
       (define root (or out-dir (make-explore-root)))
       (define results
         (run-exploration #:mode mode #:filter filter-tag #:out root #:append-log append-log))
       (print-summary results root)
       (define code (exploration-exit-code mode results non-gating?))
       (unless (zero? code)
         (eprintf "tmux-tui-explore: one or more real scenarios did not PASS~n")
         (exit code)))]))
