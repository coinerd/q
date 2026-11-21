#lang racket/base

;; scripts/cl3-wave3-benchmark.rkt — CL3 Wave 3 final bounded comparison.
;; Optimized one-shot execution (warm compilation cache) versus a clean persistent
;; worker, on edits to REAL component logic. Evaluation-only: no agent-facing
;; tool, permission policy, test-runner change or CI-policy change.

(require json
         racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "cl3-benchmark-probe.rkt"
         "cl3-eval-probe.rkt"
         "cl3-wave3-tasks.rkt")

(provide run-benchmark
         samples->wave3-report)

(define-runtime-path repo-root "..")

(define git-exe (find-executable-path "git"))
(define racket-exe (find-executable-path "racket"))
(define raco-exe (find-executable-path "raco"))

(define (elapsed thunk)
  (define start (current-inexact-milliseconds))
  (thunk)
  (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))

(define (run-command! directory exe arguments label)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory directory]
                   [current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code exe arguments)))
  (unless (zero? code)
    (error 'cl3-wave3 (format "~a failed (~a): ~a" label code (get-output-string err))))
  (get-output-string out))

(define (create-worktree!)
  (define directory (make-temporary-directory "cl3-wave3-~a"))
  (run-command! repo-root
                git-exe
                (list "worktree" "add" "--detach" directory "HEAD")
                "git worktree add")
  directory)

(define (remove-worktree! directory)
  (with-handlers ([exn:fail? void])
    (run-command! repo-root
                  git-exe
                  (list "worktree" "remove" "--force" directory)
                  "git worktree remove"))
  (with-handlers ([exn:fail? void])
    (when (directory-exists? directory)
      (delete-directory/files directory))))

(define (compile-target! root task)
  (elapsed (lambda ()
             (run-command! root
                           raco-exe
                           (list "make" (cl3-task-module task))
                           (format "raco make ~a" (cl3-task-module task))))))

(define (one-shot-eval! root task label)
  (define code (expression-code root task))
  (define start (current-inexact-milliseconds))
  (define stdout (run-command! root racket-exe (list "-e" code) label))
  (define ms (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))
  (hash 'ms ms 'text (first-output-line stdout)))

(define (expect-text! actual expected label)
  (unless (equal? actual expected)
    (error 'cl3-wave3 (format "~a: expected ~s, got ~s" label expected actual)))
  #t)

(define active-workers (make-parameter '()))

(define (start-tracked-worker! root)
  (define session (start-worker! #:directory root))
  (active-workers (cons session (active-workers)))
  session)

(define (kill-tracked-workers!)
  (for ([session (in-list (active-workers))])
    (with-handlers ([exn:fail? void])
      (kill-worker! session)))
  (active-workers '()))

(define (worker-eval! root task label)
  (parameterize ([active-workers '()])
    (define start (current-inexact-milliseconds))
    ;; `ms` covers spawn + load + eval + worker cleanup: the dynamic-wind
    ;; completes before the elapsed time is read, so worker teardown is not
    ;; silently excluded the way it is for one-shot (exit is inside the call).
    (define result
      (dynamic-wind
       void
       (lambda ()
         (define session (start-tracked-worker! root))
         (define spawn-ms (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))
         (define load
           (request! session
                     (hash 'op "load" 'path (path->string (build-path root (cl3-task-module task))))))
         (unless (equal? (hash-ref load 'status #f) "ok")
           (error 'cl3-wave3 (format "~a: load failed: ~s" label load)))
         (define eval-response (request! session (hash 'op "eval" 'code (expression-code root task))))
         (unless (equal? (hash-ref eval-response 'status #f) "ok")
           (error 'cl3-wave3 (format "~a: eval failed: ~s" label eval-response)))
         (hash 'spawn-ms
               spawn-ms
               'load-ms
               (hash-ref load 'wall-ms)
               'text
               (first-output-line (hash-ref eval-response 'stdout ""))
               'memory
               (and (pair? (hash-ref eval-response 'values '()))
                    (hash-ref (car (hash-ref eval-response 'values '())) 'text #f))))
       kill-tracked-workers!))
    (hash-set result 'ms (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))))

(define (target-zo-path root task)
  (define module (cl3-task-module task))
  (define name (path->string (file-name-from-path module)))
  (build-path root (path-only module) "compiled" (regexp-replace #px"\\.rkt$" name "_rkt.zo")))

;; Timestamp-based compiled-file checks can keep a stale .zo when source is
;; rewritten quickly; deleting the target .zo makes on-demand compilation
;; deterministic without clearing dependency .zos.
(define (invalidate-target-zo! root task)
  (define zo (target-zo-path root task))
  (when (file-exists? zo)
    (delete-file zo))
  (void))

(define (run-trial! root task approach)
  (parameterize ([active-workers '()])
    (dynamic-wind void
                  (lambda ()
                    (reset-source! root task)
                    (invalidate-target-zo! root task)
                    ;; Baseline: fixture sanity check of pre-edit behaviour. It is
                    ;; excluded from total-ms so the measured figure is exactly
                    ;; edit -> correct post-edit result.
                    (define baseline
                      (case approach
                        [(clean-worker clean-worker-warm) (worker-eval! root task "wave3 baseline")]
                        [else (one-shot-eval! root task "wave3 baseline")]))
                    (expect-text! (hash-ref baseline 'text)
                                  (cl3-task-expected-before task)
                                  (format "~a baseline" approach))
                    (define edit-ms
                      (elapsed (lambda () (apply-edit! root task (format "~a edit" approach)))))
                    (invalidate-target-zo! root task)
                    (define compile-ms
                      (if (memq approach '(one-shot-warm clean-worker-warm))
                          (compile-target! root task)
                          0.0))
                    (define fresh
                      (case approach
                        [(one-shot one-shot-warm) (one-shot-eval! root task "wave3 fresh")]
                        [(clean-worker clean-worker-warm) (worker-eval! root task "wave3 fresh")]
                        [else (error 'cl3-wave3 "unknown approach: ~a" approach)]))
                    (expect-text! (hash-ref fresh 'text)
                                  (cl3-task-expected-after task)
                                  (format "~a fresh" approach))
                    (hash 'approach
                          (symbol->string approach)
                          'baseline-ms
                          (hash-ref baseline 'ms)
                          'edit-ms
                          edit-ms
                          'compile-ms
                          compile-ms
                          'fresh-ms
                          (hash-ref fresh 'ms)
                          'spawn-ms
                          (hash-ref fresh 'spawn-ms 0.0)
                          'load-ms
                          (hash-ref fresh 'load-ms 0.0)
                          'total-ms
                          (+ edit-ms compile-ms (hash-ref fresh 'ms))
                          ;; Measured path = post-edit evaluation only (1 process for
                          ;; every approach). The pre-edit baseline evaluation is a
                          ;; separate process whose duration is excluded, so the whole
                          ;; workflow always starts 2 direct Racket processes.
                          'measured-path-process-count
                          1
                          'workflow-process-count
                          2
                          'correct?
                          #t))
                  kill-tracked-workers!)))

(define approaches '(one-shot one-shot-warm clean-worker clean-worker-warm))

(define (task-report root task n)
  (reset-source! root task)
  (define compilation (hash 'cold-target-ms (compile-target! root task)))
  (define collected
    (make-hash (for/list ([approach (in-list approaches)])
                 (cons approach '()))))
  (for ([i (in-range n)])
    (define rotation (remainder i (length approaches)))
    (define order (append (drop approaches rotation) (take approaches rotation)))
    (for ([approach (in-list order)])
      (define trial (run-trial! root task approach))
      (hash-set! collected
                 approach
                 (cons (hash-set (hash-set trial 'iteration i) 'order (map symbol->string order))
                       (hash-ref collected approach)))))
  (hash 'name
        (cl3-task-name task)
        'module
        (cl3-task-module task)
        'expression
        (cl3-task-expression task)
        'expected-before
        (cl3-task-expected-before task)
        'expected-after
        (cl3-task-expected-after task)
        'compilation
        compilation
        'approaches
        (for/hash ([approach (in-list approaches)])
          (define trials (reverse (hash-ref collected approach)))
          (hash-set! collected approach trials)
          (values approach
                  (hash 'n
                        n
                        'total-ms
                        (stats (format "~a / ~a" (cl3-task-name task) approach)
                               (map (lambda (trial) (hash-ref trial 'total-ms)) trials))
                        'compile-ms
                        (stats "incremental raco make"
                               (map (lambda (trial) (hash-ref trial 'compile-ms)) trials))
                        'fresh-ms
                        (stats "post-edit correct evaluation"
                               (map (lambda (trial) (hash-ref trial 'fresh-ms)) trials))
                        'measured-path-process-count
                        (stats "direct Racket processes on the measured post-edit path"
                               (map (lambda (trial) (hash-ref trial 'measured-path-process-count))
                                    trials))
                        'workflow-process-count
                        (stats "direct Racket processes in the whole workflow"
                               (map (lambda (trial) (hash-ref trial 'workflow-process-count)) trials))
                        'all-correct?
                        (andmap (lambda (trial) (hash-ref trial 'correct?)) trials)
                        'trials
                        trials)))))

(define (run-benchmark #:n [n 10])
  (define worktree (create-worktree!))
  (define (measure)
    (hash
     'revision
     (string-trim (with-output-to-string (lambda ()
                                           (parameterize ([current-directory repo-root])
                                             (system* git-exe "rev-parse" "HEAD")))))
     'host
     (format "~a/~a-bit" (system-type 'os) (system-type 'word))
     'n
     n
     'quantile-definition
     "linear interpolation at rank q*(n-1), rounded to 0.001 ms (CL0-compatible)"
     'scope
     (string-append
      "real component-logic edits (public export behaviour changes), not injected probes; "
      "expected values are declared in the task fixture and checked on baseline and "
      "post-edit evaluations")
     'approaches
     (hash 'one-shot
           "two plain racket -e processes; Racket loads/compiles on demand"
           'one-shot-warm
           "incremental raco make of the target module, then one racket -e"
           'clean-worker
           "fresh worker process: load then eval, killed afterwards"
           'clean-worker-warm
           "incremental raco make, then fresh worker load + eval")
     'cache-policy
     (string-append "target compiled/ is warm from the compilation block and reused across trials; "
                    "trials are interleaved with a per-iteration rotation recorded in trial.order")
     'cleanup-definition
     (string-append
      "workers are tracked and killed in a dynamic-wind, so cleanup runs even if an assertion fails; "
      "Worker evaluation timing includes spawn, load, eval AND cleanup, matching one-shot where "
      "process exit is inside the measured call")
     'process-definition
     (string-append
      "direct Racket processes started by the workflow; the measured post-edit path always starts "
      "one process, and the excluded pre-edit baseline evaluation starts another; "
      "process-tree ownership is not verified")
     'tasks
     (for/list ([task (in-list tasks)])
       (task-report worktree task n))))
  (dynamic-wind void measure (lambda () (remove-worktree! worktree))))

(define (samples->wave3-report result out)
  (write-json result out)
  (newline out))

(module+ main
  (define n 10)
  (define out-path #f)
  (define args (vector->list (current-command-line-arguments)))
  (let loop ([args args])
    (cond
      [(null? args) (void)]
      [(equal? (car args) "--n")
       (set! n (string->number (cadr args)))
       (loop (cddr args))]
      [(equal? (car args) "--out")
       (set! out-path (cadr args))
       (loop (cddr args))]
      [else (error 'cl3-wave3 "unknown option: ~a" (car args))]))
  (define result (run-benchmark #:n n))
  (cond
    [out-path
     (call-with-output-file out-path
                            (lambda (out) (samples->wave3-report result out))
                            #:exists 'truncate/replace)]
    [else
     (write-json result)
     (newline)]))
