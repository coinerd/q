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
         racket/string)

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
    "Restart/round-trip memory scenario; W7 upgrades this from unsupported to evidence-backed."
    "Persist a durable fact, restart, and recall it without restating it."
    'unsupported-until-w7
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

(define secret-regexps
  (list #px"sk-[A-Za-z0-9_-]+"
        #px"(?i:bearer) +[A-Za-z0-9._-]+"
        #px"(?i:(api[_-]?key|token|secret|password))=([^ \t\n\r]+)"
        #px"(?i:(api[_-]?key|token|secret|password))\":\"([^\"]+)"))

(define (redact-explore-text text)
  (define redacted
    (for/fold ([acc text]) ([rx (in-list secret-regexps)])
      (regexp-replace*
       rx
       acc
       (lambda matches
         (define matched (car matches))
         (cond
           [(regexp-match? #px"=" matched) (regexp-replace #px"=.*$" matched "=<REDACTED>")]
           [(regexp-match? #px"\":\"" matched)
            (regexp-replace #px"\":\".*$" matched "\":\"<REDACTED>")]
           [(regexp-match? #px"(?i:bearer)" matched) "Bearer <REDACTED>"]
           [else "<REDACTED>"])))))
  redacted)

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
       ("durability" . "not claimed"))]
    [(gsd)
     '(("completion" . "mock turn completed") ("waves" . "W0/W1 with gates")
                                              ("truth" . "no executed PASS claimed"))]
    [(mas)
     '(("completion" . "mock turn completed") ("caveat" . "lifecycle events deferred to W6")
                                              ("truth" . "prose-only evidence is not final PASS"))]
    [(tools)
     '(("completion" . "mock turn completed") ("fixture" . "README_FIXTURE_ALPHA")
                                              ("mutation" . "no file modifications"))]
    [(release-audit)
     '(("completion" . "mock turn completed")
       ("authorization" . "refused without live CI/release evidence")
       ("required" . "release-manifest.json"))]
    [(durable-memory)
     '(("completion" . "mock turn completed")
       ("status" . "unsupported until restart round-trip exists")
       ("truth" . "one-session context is not durable memory"))]
    [else '(("completion" . "mock turn completed"))]))

(define (scenario-status mode scenario)
  (cond
    [(eq? mode 'mock) (explore-scenario-expected-status scenario)]
    [(equal? (explore-scenario-tag scenario) "durable-memory") 'unsupported-until-w7]
    [else 'requires-real-provider-run]))

(define (scenario-classification status)
  (case status
    [(pass-mock-tui) 'pass]
    [(pass-mock-tui-with-caveat pass-mock-tui-partial) 'partial]
    [(unsupported-until-w7) 'unsupported]
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
            (format "- **~a:** ~a~n" (car item) (redact-explore-text (format "~a" (cdr item))))))
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

(define (result->hash result)
  (hasheq 'tag
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
          (explore-result-completed-at result)))

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
                                      (explore-result-tag r)
                                      (explore-result-title r)
                                      (mode->string (explore-result-mode r))
                                      (explore-result-status r)
                                      (explore-result-classification r)
                                      (or (explore-result-report-path r) ""))))
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
                                      (explore-result-tag r)
                                      (mode->string (explore-result-mode r))
                                      (explore-result-status r)
                                      (explore-result-classification r)
                                      (or (explore-result-report-path r) ""))))
                         #:exists 'append))

(define (run-one-scenario scenario mode root)
  (define started (timestamp))
  ;; W1 intentionally establishes the official runner/report contract. Later
  ;; waves replace pane/sentinel mock evidence with structured TUI event waits.
  (define status (scenario-status mode scenario))
  (define result
    (explore-result (explore-scenario-tag scenario)
                    (explore-scenario-title scenario)
                    mode
                    status
                    (scenario-classification status)
                    #f
                    (scenario->mock-evidence scenario)
                    started
                    (timestamp)))
  (write-explore-report! root result))

(define (run-exploration #:mode [mode 'mock]
                         #:filter [filter-tag #f]
                         #:out [out-dir #f]
                         #:append-log [append-log #f])
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
    (for/list ([s (in-list scenarios)])
      (run-one-scenario s mode root)))
  (define-values (_tsv _json) (write-summary-files! root results))
  (when append-log
    (append-central-log! append-log results))
  results)

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
  (command-line
   #:program "tmux-tui-explore"
   #:once-each [("-l" "--list") "List scenarios without running" (set! list-only? #t)]
   [("-m" "--mode") mode "Mode: mock or real (default: mock)" (set! mode-str mode)]
   [("-f" "--filter") tag "Run only one scenario tag" (set! filter-tag tag)]
   [("-o" "--out") dir "Output directory for reports" (set! out-dir dir)]
   [("--append-log") path "Append compact ledger entry to a markdown log" (set! append-log path)]
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
       (print-summary results root))]))
