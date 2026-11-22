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
         "selection-cases.rkt")

(provide run-case
         run-experiment
         selected-plan
         parse-explain-output
         ->jsexpr)

;; ---- running commands in the worktree -------------------------------------------

;; -> (values exit-code elapsed-seconds stdout stderr timed-out?)
(define (run-command command #:cwd cwd #:timeout-seconds [timeout-seconds 5400])
  (define start (current-inexact-milliseconds))
  (define-values (proc out in err)
    (parameterize ([current-directory cwd])
      (subprocess #f #f #f (car command) (cdr command))))
  (close-output-port in)
  (define done (make-semaphore 0))
  (define out-box (box ""))
  (define err-box (box ""))
  (thread (lambda ()
            (set-box! out-box
                      (with-handlers ([exn:fail? (lambda (_) "")])
                        (port->string out)))
            (semaphore-post done)))
  (thread (lambda ()
            (set-box! err-box
                      (with-handlers ([exn:fail? (lambda (_) "")])
                        (port->string err)))
            (semaphore-post done)))
  (define finished? (sync/timeout timeout-seconds done))
  (unless finished?
    (subprocess-kill proc #t)
    (subprocess-wait proc))
  (sync/timeout 0 done)
  (sync/timeout 0 done)
  (define status (subprocess-status proc))
  (values (if (exact-integer? status) status -1)
          (/ (- (current-inexact-milliseconds) start) 1000.0)
          (unbox out-box)
          (unbox err-box)
          (not finished?)))

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
        (classify-selection selected-exit full-exit)))

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
(define (run-experiment worktree
                        #:revision revision
                        #:out-file out-file
                        #:cases [cases selection-cases])
  (preflight! worktree revision)
  (define records
    (for/list ([case (in-list cases)])
      (define original (apply-case! worktree case))
      (define defect
        (dynamic-wind void
                      (lambda () (run-case worktree case #:label 'defect))
                      (lambda () (restore-case! worktree case original))))
      (define clean (run-case worktree case #:label 'control))
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
  (command-line #:program "selection-experiment.rkt"
                #:once-each ["--worktree" path "clean disposable checkout" (set! worktree path)]
                ["--revision" sha "full SHA the worktree is pinned at" (set! revision sha)]
                ["--out" path "JSON output file" (set! out-file path)]
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
    (run-experiment (string->path worktree)
                    #:revision revision
                    #:out-file (string->path out-file)
                    #:cases cases))
  (printf "wrote ~a (~a case(s))\\n" (hash-ref result 'out-file) (hash-ref result 'cases)))
