#lang racket/base

;; scripts/cl5/trial-runner.rkt — orchestration for the CL5 agent trials
;; (W1 #9982). This module builds and runs the trial commands; it executes
;; NO trials itself. W3 drives it.
;;
;; One trial = one fresh non-interactive pi session in an existing pinned
;; worktree (CL0 §1: fresh context, fixed model configuration, verbatim
;; prompt). Model and thinking are REQUIRED keyword arguments everywhere —
;; a series can never silently drift to an environment default.
;;
;; CLI mode is --dry-run only: it prints the exact command and the task spec
;; and executes nothing. Live execution goes through run-trial / run-series
;; from W3 code.

(require json
         racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/string
         "experiment-config.rkt"
         "oracle.rkt"
         "session-metrics.rkt"
         "task-spec.rkt")

(provide trial-command
         run-trial
         run-series
         series-path-for
         find-newest-session-file
         last-assistant-text
         reset-worktree!
         available-models
         pi-version
         ->jsexpr)

;; ---- command construction --------------------------------------------------------

;; tool allowlist: the shared experiment-config value (identical in both arms).
;; A per-call override exists only for the dry-run CLI / tests; the config is
;; the authority and run-trial never passes a different value.

;; trial-command : task-id arm-config session-dir out-dir [#:pi path] -> (listof string)
;;
;; The exact fresh-session command line, derived from ONE arm config (revision
;; + provider/model/thinking/tool allowlist). There is no default model here
;; on purpose: a trial cannot run without an explicit experiment config.
(define (trial-command task-id arm-cfg session-dir out-dir #:pi [pi-executable "pi"])
  (define spec (task-spec task-id))
  (define revision (hash-ref arm-cfg 'revision))
  (define short-rev (substring revision 0 (min 8 (string-length revision))))
  (list pi-executable
        "-p"
        "--provider"
        (hash-ref arm-cfg 'provider)
        "--model"
        (hash-ref arm-cfg 'model)
        "--thinking"
        (hash-ref arm-cfg 'thinking)
        "--no-extensions"
        "--no-mcp"
        "--no-skills"
        "--no-prompt-templates"
        "--session-dir"
        session-dir
        "--tools"
        (hash-ref arm-cfg 'tool-allowlist)
        "--name"
        (format "cl5-~a-~a-~a"
                (string-downcase (symbol->string (hash-ref arm-cfg 'arm)))
                (string-downcase (symbol->string task-id))
                short-rev)
        (hash-ref spec 'prompt)))

;; ---- effective CLI identity --------------------------------------------------------

;; `pi --version` (used in every series metadata hash).
(define (pi-version #:pi [pi-executable "pi"] #:timeout-ms [timeout-ms 60000])
  (define-values (exit-code stdout stderr _timed-out?)
    (run-captured (list pi-executable "--version") #:cwd (current-directory) #:timeout-ms timeout-ms))
  (cond
    [(not (equal? exit-code 0))
     (raise-arguments-error 'pi-version "cannot read the pi CLI version" "stderr" stderr)]
    [else (string-trim stdout)]))

;; Provider's advertised models, parsed by experiment-config's pure parser.
(define (available-models provider #:pi [pi-executable "pi"] #:timeout-ms [timeout-ms 120000])
  (define-values (exit-code stdout stderr _timed-out?)
    (run-captured (list pi-executable "--list-models" provider)
                  #:cwd (current-directory)
                  #:timeout-ms timeout-ms))
  (unless (equal? exit-code 0)
    (raise-arguments-error 'available-models
                           "cannot enumerate the provider's models"
                           "provider"
                           provider
                           "stderr"
                           stderr))
  (parse-model-list stdout))

;; ---- session discovery -------------------------------------------------------------

(define (session-jsonl-files dir)
  (if (directory-exists? dir)
      (for/list ([path (in-directory dir)]
                 #:when (and (file-exists? path) (string-suffix? (path->string path) ".jsonl")))
        path)
      '()))

;; The session file produced by a trial: the newest .jsonl under session-dir
;; that was not present before the run (pi may nest by working directory).
;; Falls back to the newest .jsonl overall when the pre-run set is unknown.
(define (find-newest-session-file session-dir [before '()])
  (define fresh (filter (lambda (path) (not (member path before))) (session-jsonl-files session-dir)))
  (define candidates
    (if (null? fresh)
        (session-jsonl-files session-dir)
        fresh))
  (and (pair? candidates) (argmax file-or-directory-modify-seconds candidates)))

;; Concatenated text blocks of the LAST assistant message — the trial's
;; submitted answer for navigation tasks.
(define (last-assistant-text events)
  (define last-assistant
    (for/last ([event (in-list events)]
               #:when (equal? (hash-ref event 'type #f) "message")
               #:when (equal? (hash-ref (hash-ref event 'message (hash)) 'role #f) "assistant"))
      (hash-ref event 'message (hash))))
  (if (not last-assistant)
      ""
      (string-join (filter-map (lambda (item)
                                 (and (hash? item)
                                      (equal? (hash-ref item 'type #f) "text")
                                      (hash-ref item 'text #f)))
                               (let ([content (hash-ref last-assistant 'content '())])
                                 (if (list? content)
                                     content
                                     '())))
                   "\n")))

;; ---- one trial ---------------------------------------------------------------------

;; Run one trial in an existing pinned worktree. Returns the trial record
;; hash. The worktree must already be at the arm revision; the harness resets
;; it AFTER grading so the next trial starts clean.
;;
;; The configuration comes from ONE arm config (scripts/cl5/experiment-config.rkt).
;; After the session, the EFFECTIVE provider/model/thinking are verified against
;; that config and a mismatch raises: an invalid trial is never recorded.
(define (run-trial task-id
                   worktree
                   #:config config
                   #:arm arm
                   #:session-dir session-dir
                   #:reset? [reset? #t]
                   #:timeout-ms [timeout-ms 3600000])
  (define spec (task-spec task-id))
  (define arm-cfg (arm-config config arm))
  (define arm-revision (hash-ref arm-cfg 'revision))
  ;; The session directory must be OUTSIDE the worktree (a `git clean` between
  ;; trials would delete it) and must be absolute (pi resolves it against the
  ;; process cwd, which is the worktree).
  (define absolute-session-dir (path->complete-path (simplify-path session-dir)))
  (make-directory* absolute-session-dir)
  (define before (session-jsonl-files absolute-session-dir))
  (define command
    (trial-command task-id
                   arm-cfg
                   (path->string absolute-session-dir)
                   (path->string absolute-session-dir)))
  (define-values (exit-code stdout stderr timed-out?)
    (run-captured command #:cwd worktree #:timeout-ms timeout-ms))
  (define session-file (find-newest-session-file absolute-session-dir before))
  (unless session-file
    (raise-arguments-error 'run-trial
                           "trial produced no session file; refusing to record an unverified trial"
                           "task"
                           (symbol->string task-id)
                           "exit-code"
                           exit-code
                           "stderr"
                           stderr))
  (define-values (events _skipped) (read-session-events session-file))
  (define effective (verify-effective-config events arm-cfg))
  (define repo-root (path->string (path->complete-path worktree)))
  (define metrics
    (session-metrics-file session-file
                          (hash-ref spec 'ownership-set)
                          #:repo-root repo-root
                          #:owner-paths (hash-ref spec 'owner-location-paths)))
  (define changed (changed-paths worktree arm-revision))
  (define constraint
    (and (eq? (hash-ref spec 'oracle) 'execution)
         (diff-constraint changed (hash-ref spec 'ownership-set) (hash-ref spec 'ownership-set))))
  (define oracle-record
    (cond
      [(eq? (hash-ref spec 'oracle) 'text) (grade-task task-id (last-assistant-text events))]
      [else
       (define execution (grade-task-by-execution task-id worktree #:timeout-ms timeout-ms))
       (define constraint-pass? (eq? (hash-ref constraint 'verdict) 'pass))
       (hash-set* execution
                  'verdict
                  (if (and (eq? (hash-ref execution 'verdict) 'pass) constraint-pass?) 'pass 'fail)
                  'diff-constraint
                  constraint
                  'reasons
                  (if constraint-pass?
                      (hash-ref execution 'reasons '())
                      (list (format "diff constraint failed: stray=~a missing=~a"
                                    (hash-ref constraint 'stray)
                                    (hash-ref constraint 'missing)))))]))
  (define record
    (hash 'task
          (symbol->string task-id)
          'arm
          arm
          'arm-revision
          arm-revision
          'configured
          (hash 'provider
                (hash-ref arm-cfg 'provider)
                'model
                (hash-ref arm-cfg 'model)
                'full-id
                (hash-ref arm-cfg 'full-id)
                'thinking
                (hash-ref arm-cfg 'thinking)
                'tool-allowlist
                (hash-ref arm-cfg 'tool-allowlist))
          'effective
          effective
          'session-file
          (and session-file (path->string session-file))
          'exit-code
          exit-code
          'timed-out
          timed-out?
          'changed-paths
          changed
          'metrics
          metrics
          'oracle
          oracle-record))
  ;; Reset AFTER grading: the next trial starts from the arm revision and the
  ;; agent's work is already captured in `changed-paths`.
  (when reset?
    (reset-worktree! worktree arm-revision))
  record)

;; Restore the worktree to the arm revision: tracked edits, deletions and
;; untracked files (including compiled bytecode). This is a harness action on
;; a disposable trial worktree, never on a developer checkout.
(define (reset-worktree! worktree arm-revision #:timeout-ms [timeout-ms 300000])
  (define-values (reset-exit _o reset-err _t)
    (run-captured (list "git" "reset" "--hard" arm-revision) #:cwd worktree #:timeout-ms timeout-ms))
  (define-values (clean-exit _o2 clean-err _t2)
    (run-captured (list "git" "clean" "-fdx") #:cwd worktree #:timeout-ms timeout-ms))
  (unless (and (equal? reset-exit 0) (equal? clean-exit 0))
    (raise-arguments-error 'reset-worktree!
                           "cannot reset the trial worktree"
                           "reset-exit"
                           reset-exit
                           "clean-exit"
                           clean-exit
                           "stderr"
                           (string-append reset-err clean-err))))

;; ---- series (append-only JSON) -------------------------------------------------------

(define (series-path-for arm arm-revision out-dir)
  (define short-rev (substring arm-revision 0 (min 8 (string-length arm-revision))))
  (build-path out-dir (format "CL5-TRIALS-~a-~a.json" (symbol->string arm) short-rev)))

(define (current-host)
  (or (getenv "CL5_HOST") (getenv "HOSTNAME") "unknown"))

;; JSON conversion: symbols become strings, paths become strings; hashes and
;; lists recurse. #f stays #f (JSON false) — a recorded absence, not a hole.
(define (->jsexpr value)
  (cond
    [(hash? value)
     (for/hash ([(key val) (in-hash value)])
       (values key (->jsexpr val)))]
    [(symbol? value) (symbol->string value)]
    [(list? value) (map ->jsexpr value)]
    [(path? value) (path->string value)]
    [else value]))

;; Run n trials per task (rotation order) for ONE arm and append the records
;; to the series file docs/reports/CL5-TRIALS-<arm>-<short-sha>.json.
;;
;; The arm's provider/model/thinking come from the experiment config; an
;; unavailable model raises BEFORE the first session (no fallback), and an
;; existing series may only be extended when its stamped identity matches
;; exactly — a drifted configuration is an error, not a new file.
(define (run-series tasks
                    worktree
                    #:config config
                    #:arm arm
                    #:n [n 3]
                    #:rotation [rotation tasks]
                    #:session-dir session-dir
                    #:out-dir [out-dir "docs/reports"]
                    #:timeout-ms [timeout-ms 3600000]
                    #:models [models #f])
  (define arm-cfg (arm-config config arm))
  (define arm-revision (hash-ref arm-cfg 'revision))
  (define short-rev (substring arm-revision 0 (min 8 (string-length arm-revision))))
  ;; Fail closed before the first session: the provider must advertise the
  ;; configured model. `models` is injectable only so tests can exercise the
  ;; refusal without a network call; production passes #f and asks the CLI.
  (assert-model-available (or models (available-models (hash-ref arm-cfg 'provider))) arm-cfg)
  (define path (series-path-for arm arm-revision out-dir))
  (define metadata
    (series-metadata config arm #:host (current-host) #:n n #:rotation rotation))
  (define existing
    (and (file-exists? path)
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (string->jsexpr (file->string path)))))
  (when (and (file-exists? path) (not existing))
    (raise-arguments-error 'run-series
                           "series file exists but is not readable JSON; refusing to overwrite"
                           "path"
                           (path->string path)))
  (when existing (check-series-metadata existing metadata))
  (define new-trials
    (for*/list ([round (in-range n)]
                [task-id (in-list rotation)])
      (run-trial task-id
                 worktree
                 #:config config
                 #:arm arm
                 #:session-dir session-dir
                 #:timeout-ms timeout-ms)))
  (define all-trials
    (append (if existing
                (hash-ref existing 'trials '())
                '())
            (map ->jsexpr new-trials)))
  (make-directory* out-dir)
  (define series (hash-set (->jsexpr metadata) 'trials all-trials))
  (call-with-output-file path
                         (lambda (out) (write-string (jsexpr->string series) out))
                         #:exists 'truncate/replace)
  (hash 'series-file (path->string path) 'trial-count (length all-trials)))

;; ---- CLI: dry-run only ----------------------------------------------------------

(module+ main
  (define task-id #f)
  (define arm #f)
  (define session-dir "cl5-sessions")
  (define dry-run? #f)
  (command-line
   #:program "trial-runner.rkt"
   #:once-each ["--task" id "task id (T1, T3, T4, T2, T5)" (set! task-id (string->symbol id))]
   ["--arm"
    name
    "arm: baseline or intervention (selects the pinned revision from the experiment config)"
    (set! arm (string->symbol name))]
   ["--session-dir" dir "where pi writes the session JSONL" (set! session-dir dir)]
   ["--dry-run" "print the exact command and task spec; execute nothing" (set! dry-run? #t)])
  (unless (and task-id arm)
    (raise-user-error 'trial-runner "--task and --arm are required"))
  (unless dry-run?
    (raise-user-error 'trial-runner
                      "this harness executes no trials; rerun with --dry-run (execution is W3)"))
  (define config (experiment-config #:pi-version (pi-version)))
  (define arm-cfg (arm-config config arm))
  (define spec (task-spec task-id))
  (define command
    (trial-command task-id arm-cfg (path->string (path->complete-path session-dir)) "docs/reports"))
  (printf "experiment:  ~a\n" (hash-ref config 'id))
  (printf "arm:         ~a @ ~a\n" arm (hash-ref arm-cfg 'revision))
  (printf "model:       ~a (thinking ~a)\n"
          (hash-ref arm-cfg 'full-id)
          (hash-ref arm-cfg 'thinking))
  (printf "pi version:  ~a\n" (hash-ref config 'pi-version))
  (printf "task:        ~a (~a, oracle: ~a)\n"
          (hash-ref spec 'id)
          (hash-ref spec 'kind)
          (hash-ref spec 'oracle))
  (printf "provenance:  ~a\n" (hash-ref spec 'provenance))
  (printf "corpus:      ~a\n" (hash-ref spec 'corpus-revision))
  (printf "ownership:   ~a\n" (hash-ref spec 'ownership-set))
  (printf "owner-paths: ~a\n" (hash-ref spec 'owner-location-paths))
  (printf "prompt:\n~a\n" (hash-ref spec 'prompt))
  (printf "command:\n")
  (for ([arg (in-list command)])
    (if (string-contains? arg " ")
        (printf "  '~a'\n" arg)
        (printf "  ~a\n" arg))))
