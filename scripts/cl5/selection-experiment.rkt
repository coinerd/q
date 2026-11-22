#lang racket/base

;; scripts/cl5/selection-experiment.rkt — deterministic shadow-selection
;; experiment for CL5 W2 (#9982).
;;
;; For each frozen CL0 corpus case it (1) applies the documented injection in a
;; DISPOSABLE worktree, (2) computes the selector's chosen set with the
;; existing, unchanged tooling (`./qt quick --changed HEAD --explain`),
;; (3) runs the selected set, (4) runs comprehensive verification (`./qt full`)
;; as ground truth on the same tree, and (5) classifies the case with
;; classify-selection. The tree is restored and re-verified clean afterwards.
;;
;; This changes NO selector, scheduler, admission record or CI policy: it is a
;; measurement of the selector that already exists, in shadow mode.

(require json
         racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "ci-evidence.rkt"
         "selection-cases.rkt")

(provide run-command
         run-case
         run-experiment
         selected-plan
         parse-explain-output
         resolve-command-path
         current-admission-refresh
         current-derived-restore
         current-case-runner
         run-case-ci
         run-experiment-ci
         classify-pair
         expected-scope-check
         measure-selected!
         selected-state!
         selected-failures
         append-checkpoint!
         git!
         classify-run
         ->jsexpr)

;; ---- running commands in the worktree -------------------------------------------

;; First argument of a command, resolved for subprocess:
;;   * "./x" or "../x" -> cwd-relative path
;;   * bare name       -> PATH lookup, else unchanged
;;   * absolute path   -> unchanged
(define (resolve-command-path program cwd)
  (define text (if (path? program) (path->string program) program))
  (cond
    [(regexp-match? #px"^\\.{1,2}/" text)
     (path->string (simplify-path (build-path (simplify-path (path->complete-path cwd)) text)))]
    [(absolute-path? (string->path text)) text]
    [else (path->string (or (find-executable-path text) (string->path text)))]))

;; -> (values exit-code elapsed-seconds stdout stderr timed-out?)
;;
;; Completion means the CHILD PROCESS has exited AND both output readers have
;; drained. Waiting only for reader EOF could report a verdict while the child
;; was still running (early EOF when the child closes one stream), so this
;; polls the process status and both reader threads against one deadline.
(define (run-command command #:cwd cwd #:timeout-seconds [timeout-seconds 5400])
  ;; `subprocess` does not search PATH, and a relative executable such as
  ;; "./qt" must resolve inside the WORKTREE — otherwise the harness would run
  ;; the caller's checkout while measuring the trial one.
  (define executable (resolve-command-path (first command) cwd))
  (define start (current-inexact-milliseconds))
  (define-values (proc out in err)
    (parameterize ([current-directory cwd])
      (apply subprocess #f #f #f executable (rest command))))
  (close-output-port in)
  (define out-box (box ""))
  (define err-box (box ""))
  (define out-thread
    (thread (lambda ()
              (set-box! out-box (with-handlers ([exn:fail? (lambda (_) "")]) (port->string out))))))
  (define err-thread
    (thread (lambda ()
              (set-box! err-box (with-handlers ([exn:fail? (lambda (_) "")]) (port->string err))))))
  (define deadline (+ start (* 1000.0 timeout-seconds)))
  (define (readers-done?) (and (thread-dead? out-thread) (thread-dead? err-thread)))
  (define (process-running?) (eq? (subprocess-status proc) 'running))
  (define (wait-for-completion)
    (cond
      [(and (readers-done?) (not (process-running?))) #f]
      [(> (current-inexact-milliseconds) deadline)
       (subprocess-kill proc #t)
       (subprocess-wait proc)
       ;; Give the readers a bounded moment to drain after the kill.
       (let drain ([waited 0])
         (cond [(readers-done?) (void)]
               [(> waited 100) (void)]
               [else (sleep 0.05) (drain (add1 waited))]))
       #t]
      [else (sleep 0.02) (wait-for-completion)]))
  (define timed-out? (wait-for-completion))
  (define status (subprocess-status proc))
  (values (if (exact-integer? status) status -1)
          (/ (- (current-inexact-milliseconds) start) 1000.0)
          (unbox out-box)
          (unbox err-box)
          timed-out?))

;; ---- admission refresh (documented workflow step) ---------------------------------

;; Changing an admitted owner's closure makes `./qt quick` fail closed until the
;; admission record is refreshed — the documented workflow ("refresh the record
;; once when an admitted owner's closure changed, then rerun"). The experiment
;; performs that refresh and RESTORES both derived files afterwards, recording
;; that it happened so the cost is visible rather than hidden.
(define admission-files
  (list "tests/.quick-admission.rktd" "tests/.coverage-manifest.json"))

(define (refresh-admission! worktree)
  (define-values (exit-code _elapsed stdout stderr _timed-out?)
    (run-command (list "racket"
                       "-e"
                       (string-append "(begin (require (file \"scripts/testing/quick-audit.rkt\"))"
                                      " (refresh-record!))"))
                 #:cwd worktree
                 #:timeout-seconds 1800))
  (unless (equal? exit-code 0)
    (raise-arguments-error 'refresh-admission!
                           "admission refresh failed"
                           "stderr"
                           (if (equal? stderr "") stdout stderr)))
  #t)

(define (restore-derived-files! worktree)
  (for ([rel (in-list admission-files)])
    (define-values (_exit _elapsed _out _err _timed-out?)
      (run-command (list "git" "checkout" "--" rel) #:cwd worktree #:timeout-seconds 300))
    (void)))

;; ---- the selector's chosen set ----------------------------------------------------

;; Selection is computed by the SHIPPED tool, never re-implemented here:
;; `./qt quick --changed HEAD --explain` prints the plan without executing it.
(define (explain-command)
  (list "./qt" "quick" "--changed" "HEAD" "--explain"))

(define (parse-explain-output text)
  (define included
    (for/list ([line (in-list (string-split text "\n"))]
               #:when (string-prefix? (string-trim line) "included:"))
      (let ([rest (string-trim (substring (string-trim line) (string-length "included:")))])
        (car (string-split rest " ")))))
  (hash 'selected
        (remove-duplicates included)
        'escalations
        (for/list ([line (in-list (string-split text "\n"))]
                   #:when (string-prefix? (string-trim line) "escalation:"))
          (string-trim line))))

(define (selected-plan worktree)
  (define-values (exit-code _elapsed stdout stderr _timed-out?)
    (run-command (explain-command) #:cwd worktree #:timeout-seconds 600))
  (unless (equal? exit-code 0)
    (raise-arguments-error 'selected-plan
                           "the selection tool failed"
                           "exit"
                           exit-code
                           "stderr"
                           (if (equal? stderr "") stdout stderr)))
  (parse-explain-output stdout))

;; ---- one case ----------------------------------------------------------------------

;; Runs the defect state and the clean control in the same worktree.
;; -> hash record with selected/comprehensive exits, timings and classification.
(define (run-case worktree case #:label [label 'defect])
  (define plan (selected-plan worktree))
  (define-values (selected-exit selected-seconds _selected-out _selected-err selected-timeout?)
    (run-command (list "./qt" "quick" "--changed" "HEAD") #:cwd worktree))
  (define-values (full-exit full-seconds _full-out _full-err full-timeout?)
    (run-command (list "./qt" "full") #:cwd worktree))
  (hash 'case
        (hash-ref case 'id)
        'label
        label
        'changed
        (hash-ref case 'changed)
        'selected-count
        (length (hash-ref plan 'selected))
        'selected
        (hash-ref plan 'selected)
        'escalations
        (hash-ref plan 'escalations)
        'selected-exit
        selected-exit
        'selected-seconds
        selected-seconds
        'selected-timed-out
        selected-timeout?
        'comprehensive-exit
        full-exit
        'comprehensive-seconds
        full-seconds
        'comprehensive-timed-out
        full-timeout?
        'classification
        (classify-run selected-exit selected-timeout? full-exit full-timeout?)))

;; ---- experiment driver ---------------------------------------------------------------

(define (git-status worktree)
  (define-values (exit-code _s stdout _e _t)
    (run-command (list "git" "status" "--porcelain") #:cwd worktree #:timeout-seconds 120))
  (values exit-code (string-trim stdout)))

(define (git-head worktree)
  (define-values (_exit _s stdout _e _t)
    (run-command (list "git" "rev-parse" "HEAD") #:cwd worktree #:timeout-seconds 120))
  (string-trim stdout))

;; Refuses a dirty or wrong-revision worktree, mirroring the W1 trial preflight.
(define (preflight! worktree revision)
  (define-values (status-exit status) (git-status worktree))
  (unless (equal? status-exit 0)
    (raise-arguments-error 'preflight! "cannot read git status" "worktree" (path->string worktree)))
  (unless (equal? status "")
    (raise-arguments-error 'preflight!
                           "worktree is dirty; refusing to measure"
                           "worktree"
                           (path->string worktree)
                           "changes"
                           status))
  (define head (git-head worktree))
  (unless (equal? head revision)
    (raise-arguments-error 'preflight!
                           "worktree HEAD is not the experiment revision"
                           "configured"
                           revision
                           "head"
                           head)))

;; A timed-out or killed run is NOT evidence about detection: the ground truth
;; was never obtained. Such a case is recorded as 'unavailable so it can never
;; be counted as detected or missed.
(define (classify-run selected-exit selected-timed-out? comprehensive-exit comprehensive-timed-out?)
  (cond
    [(or selected-timed-out? comprehensive-timed-out?) 'unavailable]
    [else (classify-selection selected-exit comprehensive-exit)]))

(define (->jsexpr value)
  (cond
    [(hash? value)
     (for/hash ([(k v) (in-hash value)])
       (values k (->jsexpr v)))]
    [(symbol? value) (symbol->string value)]
    [(list? value) (map ->jsexpr value)]
    [(path? value) (path->string value)]
    [else value]))

;; Run every case (defect state + clean control), writing one JSON series.
;; Injectable for tests: the orchestration must call the refresh exactly once
;; per defect state and restore the derived files afterwards.
(define current-admission-refresh (make-parameter refresh-admission!))
(define current-derived-restore (make-parameter restore-derived-files!))
(define current-case-runner (make-parameter run-case))

;; ---- CI-backed state preparation (Option A) ---------------------------------------

(define (git! worktree . args)
  (define-values (exit-code _elapsed stdout stderr _timed-out?)
    (run-command (append (list "git"
                               "-c" "user.email=cl5-harness@example.invalid"
                               "-c" "user.name=CL5 harness")
                         args)
                 #:cwd worktree
                 #:timeout-seconds 600))
  (unless (equal? exit-code 0)
    (raise-arguments-error 'git!
                           "git command failed"
                           "args"
                           args
                           "stderr"
                           (if (equal? stderr "") stdout stderr)))
  (string-trim stdout))

;; Lines that identify what a failing selected run actually reported.
(define (selected-failures text)
  (define lines
    (for/list ([line (in-list (string-split text "\n"))]
               #:when (regexp-match? #rx"(?i:(?:fail|failed|raco test:.*test failure))" line))
      (string-trim line)))
  (take lines (min (length lines) 20)))

;; Run the SHIPPED selector on the current working tree. The plan is read from
;; `--explain`, the execution from `--changed HEAD`; both see the same diff.
;; A clean control has no diff, and the selector treats a zero selection as a
;; failure, so the control runs the SAME selected set that the defect shipped
;; against the clean source (same set, clean tree).
(define (selected-state! worktree
                        #:timeout-seconds [timeout-seconds 1800]
                        #:files [files #f])
  (define (verdict-of exit-code timed-out? refusal?)
    (cond [timed-out? 'unavailable]
          [refusal? 'unavailable]
          [(equal? exit-code 0) 'pass]
          [else 'fail]))
  (define (run-file file)
    (run-command (list "./qt" "full" "--file" file)
                 #:cwd worktree
                 #:timeout-seconds timeout-seconds))
  (define (selected-files-result)
    (define results
      (for/list ([file (in-list files)])
        (define-values (exit-code elapsed stdout stderr timed-out?) (run-file file))
        (hash 'file
              file
              'exit
              exit-code
              'timed-out
              timed-out?
              'seconds
              elapsed
              'failures
              (selected-failures (string-append stdout "\n" stderr)))))
    (hash 'verdict
          (cond
            [(for/or ([r (in-list results)]) (hash-ref r 'timed-out)) 'unavailable]
            [(for/or ([r (in-list results)]) (not (equal? (hash-ref r 'exit) 0))) 'fail]
            [else 'pass])
          'exit
          (if (for/or ([r (in-list results)]) (not (equal? (hash-ref r 'exit) 0))) 1 0)
          'timed-out
          (for/or ([r (in-list results)]) (hash-ref r 'timed-out))
          'refused
          #f
          'seconds
          (for/sum ([r (in-list results)]) (hash-ref r 'seconds))
          'selected
          files
          'escalations
          '()
          'plan-error
          #f
          'failures
          (append* (map (lambda (r) (hash-ref r 'failures)) results))
          'files
          results))
  (if files
      (selected-files-result)
      (let* ([plan
              (with-handlers ([exn:fail? (lambda (e)
                                           (hash 'selected '() 'escalations '() 'error (exn-message e)))])
                (selected-plan worktree))]
             [_ (void plan)])
        (define-values (exit-code elapsed stdout stderr timed-out?)
          (run-command (list "./qt" "quick" "--changed" "HEAD")
                       #:cwd worktree
                       #:timeout-seconds timeout-seconds))
        (define refusal? (regexp-match? #rx"quick refuses" (string-append stdout stderr)))
        (hash 'verdict
              (verdict-of exit-code timed-out? refusal?)
              'exit
              exit-code
              'timed-out
              timed-out?
              'refused
              refusal?
              'seconds
              elapsed
              'selected
              (hash-ref plan 'selected '())
              'escalations
              (hash-ref plan 'escalations '())
              'plan-error
              (hash-ref plan 'error #f)
              'failures
              (selected-failures (string-append stdout "\n" stderr))))))

;; Apply one state (defect injection for a defect state, untouched tree for the
;; control), refresh the admission record, then measure the shadow-selected
;; surface. The derived record is restored afterwards so the committed tree is
;; exactly `revision` plus the case mutation.
(define (measure-selected! worktree revision case label #:files [files #f])
  (git! worktree "reset" "-q" "--hard" revision)
  (git! worktree "clean" "-q" "-fd")
  (when (eq? label 'defect) (void (apply-case! worktree case)))
  ;; The shipped selector refuses to run against a stale audited record, so the
  ;; record is refreshed first — and then restored before committing so the CI
  ;; tree carries only the behavioural mutation.
  ((current-admission-refresh) worktree)
  (define measured (selected-state! worktree #:files files))
  (define tree-sha
    (begin
      (git! worktree "add" "-A")
      (git! worktree "write-tree")))
  (define record-identity
    (for/list ([rel (in-list admission-files)])
      (hash 'path rel 'sha (git! worktree "rev-parse" (format "HEAD:~a" rel)))))
  ((current-derived-restore) worktree)
  (define restored-sha
    (begin
      (git! worktree "add" "-A")
      (git! worktree "write-tree")))
  (hash 'selected measured
        'measured-tree-sha tree-sha
        'committed-tree-sha restored-sha
        'record-at-revision record-identity))

;; Commit whatever is staged in the worktree to a uniquely named scratch branch
;; and push it; returns the exact commit SHA CI must match.
(define (push-state! worktree revision case label branch)
  (git! worktree "checkout" "-q" "--detach" revision)
  (git! worktree "add" "-A")
  (git! worktree "commit" "-q" "--allow-empty" "-m"
        (format "cl5 w2 state: ~a ~a" (hash-ref case 'id) label))
  (define sha (git! worktree "rev-parse" "HEAD"))
  (git! worktree "push" "-q" "-f" "origin" (format "HEAD:refs/heads/~a" branch))
  sha)

;; Restore the disposable worktree and remove the scratch branch, whatever
;; happened during dispatch or job retrieval.
(define (cleanup-state! worktree revision branch)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (git! worktree "reset" "-q" "--hard" revision)
    (git! worktree "clean" "-q" "-fd"))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (git! worktree "push" "-q" "origin" "--delete" branch))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (git! worktree "branch" "-q" "-D" branch)
    (void)))

;; One CI-backed case state: measure the shadow-selected surface on the exact
;; injected tree, then commit that same tree, dispatch CI, and record the
;; exact-SHA verdict. Cleanup is guaranteed by dynamic-wind.
(define (run-case-ci repo-root
                     worktree
                     revision
                     case
                     label
                     nonce
                     #:timeout-seconds [timeout-seconds 2400]
                     #:selected-timeout [selected-timeout 1800]
                     #:selected-files [selected-files #f]
                     #:ci-cache [ci-cache #f])
  (define branch (scratch-branch-name (hash-ref case 'id) label nonce))
  (dynamic-wind
   void
   (lambda ()
     (define state
       (measure-selected! worktree revision case label #:files selected-files))
     ;; A cached verdict is keyed by the COMMITTED tree sha, so the same tree
     ;; always carries the same CI evidence and no new run is dispatched.
     (define tree-key (hash-ref state 'committed-tree-sha))
     (define cached
       (and ci-cache
            (or (hash-ref ci-cache tree-key #f)
                ;; read-json turns object keys into symbols
                (hash-ref ci-cache (string->symbol tree-key) #f))))
     (define-values (sha verdict source)
       (if cached
           (values #f cached 'cache)
           (let* ([sha (push-state! worktree revision case label branch)]
                  [verdict
                   (with-handlers ([exn:fail?
                                    (lambda (e)
                                      (hash 'verdict 'unavailable
                                            'unavailable-reasons (list (exn-message e))))])
                     (ci-verdict-for-sha repo-root
                                         sha
                                         #:branch branch
                                         #:timeout-seconds timeout-seconds))])
             (values sha verdict 'live))))
     (hash 'case
           (hash-ref case 'id)
           'label
           label
           'branch
           (and sha branch)
           'head-sha
           sha
           'ci-source
           source
           'selected
           (hash-ref state 'selected)
           'measured-tree-sha
           (hash-ref state 'measured-tree-sha)
           'committed-tree-sha
           (hash-ref state 'committed-tree-sha)
           'record-at-revision
           (hash-ref state 'record-at-revision)
           'ci-verdict
           (hash-ref verdict 'verdict)
           'ci-run-id
           (hash-ref verdict 'run-id #f)
           'detecting
           (hash-ref verdict 'detecting '())
           'missing-jobs
           (hash-ref verdict 'missing-jobs '())
           'unavailable-reasons
           (hash-ref verdict 'unavailable-reasons '())))
   (lambda () (cleanup-state! worktree revision branch))))

;; Expected-scope assertions for a case: the documented detecting owner (from
;; the frozen corpus) must be part of the selected set, and the documented
;; escalation must be reported by the selector.
(define (expected-scope-check case selected escalations)
  (define expected (hash-ref case 'expect-tests '()))
  (define missing
    (for/list ([rel (in-list expected)] #:when (not (member rel selected))) rel))
  (hash 'expected-tests
        expected
        'not-selected
        missing
        'covered?
        (and (pair? expected) (null? missing))
        'escalations
        escalations
        'escalation-reported?
        (pair? escalations)))

;; Pair classification: both surfaces must agree that the defect is real, the
;; shadow-selected surface must actually have RUN (a refusal is unavailable,
;; never a pass), and both clean controls must pass.
(define (classify-pair defect-selected control-selected ci-defect ci-control)
  (define (tag v)
    (define raw (if (hash? v) (hash-ref v 'verdict) v))
    ;; cached verdicts come back from JSON as strings
    (if (string? raw) (string->symbol raw) raw))
  (classify-case-evidence (tag defect-selected) (tag control-selected) (tag ci-defect) (tag ci-control)))

;; Durable, append-only per-case checkpoints OUTSIDE the disposable worktree.
(define (append-checkpoint! out-file record)
  (make-directory* (path-only out-file))
  (call-with-output-file out-file
                         (lambda (out) (write-string (jsexpr->string (->jsexpr record)) out) (newline out))
                         #:exists 'append))

(define (run-experiment-ci repo-root
                           worktree
                           #:revision revision
                           #:out-file out-file
                           #:cases [cases selection-cases]
                           #:nonce [nonce (number->string (current-seconds))]
                           #:timeout-seconds [timeout-seconds 2400]
                           #:checkpoint-file [checkpoint-file #f]
                           #:ci-cache [ci-cache #f])
  (preflight! worktree revision)
  (define records
    (for/list ([case (in-list cases)])
      (define defect
        (run-case-ci repo-root
                     worktree
                     revision
                     case
                     'defect
                     nonce
                     #:timeout-seconds timeout-seconds
                     #:ci-cache ci-cache))
      (when checkpoint-file
        (append-checkpoint! checkpoint-file (hash 'case (hash-ref case 'id) 'state defect)))
      ;; The clean control runs the SAME selected set against the clean source;
      ;; an empty selected set has nothing to control.
      (define control-files (hash-ref (hash-ref defect 'selected) 'selected '()))
      (define control
        (if (null? control-files)
            (hash 'case (hash-ref case 'id)
                  'label
                  'control
                  'selected
                  (hash 'verdict 'unavailable 'selected '() 'escalations '())
                  'ci-verdict
                  'unavailable
                  'unavailable-reasons
                  (list "the defect selected set is empty, so the control cannot be matched"))
            (run-case-ci repo-root
                         worktree
                         revision
                         case
                         'control
                         nonce
                         #:timeout-seconds timeout-seconds
                         #:selected-files control-files
                         #:ci-cache ci-cache)))
      (when checkpoint-file
        (append-checkpoint! checkpoint-file (hash 'case (hash-ref case 'id) 'state control)))
      (define scope
        (expected-scope-check case
                             (hash-ref (hash-ref defect 'selected) 'selected '())
                             (hash-ref (hash-ref defect 'selected) 'escalations '())))
      (hash 'case
            (hash-ref case 'id)
            'category
            (hash-ref case 'category)
            'note
            (hash-ref case 'note)
            'expect
            (hash-ref case 'expect)
            'scope
            scope
            'defect
            defect
            'control
            control
            'classification
            (classify-pair (hash-ref defect 'selected)
                           (hash-ref control 'selected)
                           (hash-ref defect 'ci-verdict)
                           (hash-ref control 'ci-verdict)))))
  (define report
    (hash 'experiment "CL5 W2 shadow selection, CI ground truth (#9982)"
          'revision revision
          'worktree (path->string worktree)
          'nonce nonce
          'cases records))
  (make-directory* (path-only out-file))
  (call-with-output-file out-file
                         (lambda (out) (write-string (jsexpr->string (->jsexpr report)) out))
                         #:exists 'truncate/replace)
  (hash 'out-file (path->string out-file) 'cases (length records) 'records records))

(define (run-experiment worktree
                        #:revision revision
                        #:out-file out-file
                        #:cases [cases selection-cases])
  (preflight! worktree revision)
  (define records
    (for/list ([case (in-list cases)])
      (define original (apply-case! worktree case))
      (define defect
        (dynamic-wind
         void
         (lambda ()
           ;; Refresh the admission record INSIDE the defect state, so an
           ;; admission refusal can never be mistaken for the injected defect,
           ;; and restore the derived files afterwards.
           ((current-admission-refresh) worktree)
           ((current-case-runner) worktree case #:label 'defect))
         (lambda ()
           (restore-case! worktree case original)
           ((current-derived-restore) worktree))))
      (define clean ((current-case-runner) worktree case #:label 'control))
      (hash 'case
            (hash-ref case 'id)
            'category
            (hash-ref case 'category)
            'note
            (hash-ref case 'note)
            'expect
            (hash-ref case 'expect)
            'defect
            defect
            'control
            clean)))
  (define report
    (hash 'experiment
          "CL5 W2 shadow selection (#9982)"
          'revision
          revision
          'worktree
          (path->string worktree)
          'cases
          records))
  (make-directory* (path-only out-file))
  (call-with-output-file out-file
                         (lambda (out) (write-string (jsexpr->string (->jsexpr report)) out))
                         #:exists 'truncate/replace)
  (hash 'out-file (path->string out-file) 'cases (length records) 'records records))

(module+ main
  (define worktree #f)
  (define revision #f)
  (define out-file #f)
  (define case-ids '())
  (define ci? #f)
  (define repo-root (path->string (build-path (current-directory) "..")))
  (define checkpoint #f)
  (define ci-cache-file #f)
  (command-line #:program "selection-experiment.rkt"
                #:once-each ["--worktree" path "clean disposable checkout" (set! worktree path)]
                ["--revision" sha "full SHA the worktree is pinned at" (set! revision sha)]
                ["--out" path "JSON output file" (set! out-file path)]
   ["--ci" "use exact-SHA CI ground truth instead of local ./qt full" (set! ci? #t)]
   ["--repo-root" path "workspace root holding scripts/gh_helpers.py" (set! repo-root path)]
   ["--checkpoint" path "append-only per-state checkpoint file (outside the worktree)" (set! checkpoint path)]
   ["--ci-cache" path "JSON map tree-sha -> CI verdict; avoids re-dispatching identical trees" (set! ci-cache-file path)]
                ["--case"
                 id
                 "case id (repeatable); default: every registered case"
                 (set! case-ids (cons (string->symbol id) case-ids))])
  (unless (and worktree revision out-file)
    (raise-user-error 'selection-experiment "--worktree, --revision and --out are required"))
  (define cases
    (if (null? case-ids)
        selection-cases
        (map selection-case (reverse case-ids))))
  (define result
    (if ci?
        (run-experiment-ci (string->path repo-root)
                           (string->path worktree)
                           #:revision revision
                           #:out-file (string->path out-file)
                           #:checkpoint-file (and checkpoint (string->path checkpoint))
                           #:ci-cache (and ci-cache-file
                                          (with-handlers ([exn:fail? (lambda (_) #f)])
                                            (hash-ref (with-input-from-file ci-cache-file
                                                        (lambda () (read-json)))
                                                      'trees
                                                      #f)))
                           #:cases cases)
        (run-experiment (string->path worktree)
                    #:revision revision
                        #:out-file (string->path out-file)
                        #:cases cases)))
  (printf "wrote ~a (~a case(s))\\n" (hash-ref result 'out-file) (hash-ref result 'cases)))
