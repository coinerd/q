#lang racket/base

;; scripts/cl3-wave2-benchmark.rkt — CL3 Wave 2 evaluation-only workflow benchmark.
;; Compares one-shot Racket, persistent worker + reload, and worker replacement
;; on real q modules copied into a disposable git worktree. Every evaluated
;; value is checked against an independent oracle; no production code, test
;; runner, admission record or CI policy is changed.

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "cl3-benchmark-probe.rkt"
         "cl3-eval-probe.rkt"
         "cl3-wave2-tasks.rkt")

(provide run-benchmark
         samples->wave2-report)

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
    (error 'cl3-wave2 (format "~a failed (~a): ~a" label code (get-output-string err))))
  (get-output-string out))

(define (create-worktree!)
  (define directory (make-temporary-directory "cl3-wave2-~a"))
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

(define (clear-compiled! root task)
  (define dir (build-path root (path-only (cl3-task-module task)) "compiled"))
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define (measure-compilation! root task)
  (clear-compiled! root task)
  (define start (current-inexact-milliseconds))
  (run-command! root raco-exe (list "make" (cl3-task-module task)) "raco make cold")
  (define cold (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))
  (define warm
    (elapsed (lambda ()
               (run-command! root raco-exe (list "make" (cl3-task-module task)) "raco make warm"))))
  (hash 'cold-ms cold 'warm-ms warm))

(define (first-value-text response)
  (define values (hash-ref response 'values '()))
  (and (pair? values) (hash-ref (car values) 'text #f)))

(define (expect-status! response expected label)
  (unless (equal? (hash-ref response 'status #f) expected)
    (error
     'cl3-wave2
     (format "~a: expected ~s, got ~s (~s)" label expected (hash-ref response 'status #f) response)))
  response)

(define (read-probe-result text label)
  (define parsed (parse-probe text))
  (unless parsed
    (error 'cl3-wave2 (format "~a: unparseable probe result: ~s" label text)))
  parsed)

(define (one-shot-probe! root expression label)
  (define start (current-inexact-milliseconds))
  (define stdout
    (run-command! root racket-exe (list "-e" (format "(displayln ~a)" expression)) label))
  (define ms (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))
  (define line
    (or (findf (lambda (s) (non-empty-string? (string-trim s))) (string-split stdout "\n")) ""))
  (define parsed (read-probe-result line label))
  (hash 'ms ms 'value (car parsed) 'memory (cdr parsed)))

(define (worker-probe! session expression label)
  (define response (request! session (hash 'op "eval" 'code expression)))
  (expect-status! response "ok" label)
  (define parsed (read-probe-result (first-value-text response) label))
  (hash 'ms (hash-ref response 'wall-ms) 'value (car parsed) 'memory (cdr parsed)))

(define (expect-value! actual expected label)
  (unless (equal? actual expected)
    (error 'cl3-wave2 (format "~a: expected ~s, got ~s" label expected actual))))

(define (finish! total-ms process-count max-memory phases observations)
  (hash 'total-ms
        total-ms
        'process-count
        process-count
        'max-memory-bytes
        max-memory
        'correct?
        #t
        'phases
        phases
        'observations
        observations))

(define (run-one-shot-workflow! root task)
  (write-task-source! root task (cl3-task-before task))
  (clear-compiled! root task)
  (define expression (probe-expression root task))
  (define baseline (one-shot-probe! root expression "one-shot baseline"))
  (expect-value! (hash-ref baseline 'value) (cl3-task-before task) "one-shot baseline value")
  (define edit-ms (elapsed (lambda () (write-task-source! root task (cl3-task-after task)))))
  (define fresh (one-shot-probe! root expression "one-shot fresh"))
  (expect-value! (hash-ref fresh 'value) (cl3-task-after task) "one-shot fresh value")
  (finish! (+ (hash-ref baseline 'ms) edit-ms (hash-ref fresh 'ms))
           2
           (max (hash-ref baseline 'memory) (hash-ref fresh 'memory))
           (hash 'baseline-eval-ms
                 (hash-ref baseline 'ms)
                 'edit-ms
                 edit-ms
                 'fresh-eval-ms
                 (hash-ref fresh 'ms))
           '()))

;; Every worker started for a trial is tracked so cleanup runs even when an
;; assertion raises mid-workflow (the success path also kills its worker).
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

(define (timed-start-worker! root)
  (define start (current-inexact-milliseconds))
  (define session (start-tracked-worker! root))
  (values (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0) session))

(define (timed-kill-worker! session)
  (define start (current-inexact-milliseconds))
  (define result (kill-worker! session))
  (values (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0) result))

(define (run-namespace-reset-workflow! root task)
  (write-task-source! root task (cl3-task-before task))
  (clear-compiled! root task)
  (define expression (probe-expression root task))
  (define module-path (path->string (build-path root (cl3-task-module task))))
  (define-values (spawn-ms session) (timed-start-worker! root))
  (define handshake (expect-status! (request! session (hash 'op "reset")) "ok" "reload handshake"))
  (define load
    (expect-status! (request! session (hash 'op "load" 'path module-path))
                    "ok"
                    "reload initial load"))
  (define baseline (worker-probe! session expression "reload baseline"))
  (expect-value! (hash-ref baseline 'value) (cl3-task-before task) "reload baseline value")
  (define edit-ms (elapsed (lambda () (write-task-source! root task (cl3-task-after task)))))
  (define reload
    (expect-status! (request! session (hash 'op "reload" 'path module-path))
                    "ok"
                    "reload after edit"))
  (define stale (worker-probe! session expression "reload stale eval"))
  (expect-value! (hash-ref stale 'value) (cl3-task-before task) "reload stale value")
  (define reset (expect-status! (request! session (hash 'op "reset")) "ok" "reload namespace reset"))
  (define reload-after-reset
    (expect-status! (request! session (hash 'op "load" 'path module-path))
                    "ok"
                    "reload load after reset"))
  (define fresh (worker-probe! session expression "reload fresh eval"))
  (expect-value! (hash-ref fresh 'value) (cl3-task-after task) "reload fresh value")
  (define-values (cleanup-ms cleanup) (timed-kill-worker! session))
  (unless (number? (hash-ref cleanup 'exit-status #f))
    (error 'cl3-wave2 "reload cleanup did not report a numeric exit status"))
  (finish!
   (+ spawn-ms
      (hash-ref handshake 'wall-ms)
      (hash-ref load 'wall-ms)
      (hash-ref baseline 'ms)
      edit-ms
      (hash-ref reload 'wall-ms)
      (hash-ref stale 'ms)
      (hash-ref reset 'wall-ms)
      (hash-ref reload-after-reset 'wall-ms)
      (hash-ref fresh 'ms)
      cleanup-ms)
   1
   (max (hash-ref baseline 'memory) (hash-ref stale 'memory) (hash-ref fresh 'memory))
   (hash 'spawn-ms
         spawn-ms
         'handshake-ms
         (hash-ref handshake 'wall-ms)
         'load-ms
         (hash-ref load 'wall-ms)
         'baseline-eval-ms
         (hash-ref baseline 'ms)
         'edit-ms
         edit-ms
         'reload-ms
         (hash-ref reload 'wall-ms)
         'stale-eval-ms
         (hash-ref stale 'ms)
         'reset-ms
         (hash-ref reset 'wall-ms)
         'reload-after-reset-ms
         (hash-ref reload-after-reset 'wall-ms)
         'fresh-eval-ms
         (hash-ref fresh 'ms)
         'cleanup-ms
         cleanup-ms)
   (list (format "reload returned stale value ~a; correct value ~a required namespace reset + reload"
                 (hash-ref stale 'value)
                 (hash-ref fresh 'value)))))

(define (worker-phase! root task label expected)
  (define-values (spawn-ms session) (timed-start-worker! root))
  (define handshake
    (expect-status! (request! session (hash 'op "reset")) "ok" (format "~a handshake" label)))
  (define load
    (expect-status!
     (request! session
               (hash 'op "load" 'path (path->string (build-path root (cl3-task-module task)))))
     "ok"
     (format "~a load" label)))
  (define result (worker-probe! session (probe-expression root task) label))
  (expect-value! (hash-ref result 'value) expected (format "~a value" label))
  (define-values (cleanup-ms cleanup) (timed-kill-worker! session))
  (unless (number? (hash-ref cleanup 'exit-status #f))
    (error 'cl3-wave2 (format "~a cleanup failed" label)))
  (hash 'spawn-ms
        spawn-ms
        'handshake-ms
        (hash-ref handshake 'wall-ms)
        'load-ms
        (hash-ref load 'wall-ms)
        'eval-ms
        (hash-ref result 'ms)
        'memory
        (hash-ref result 'memory)
        'cleanup-ms
        cleanup-ms))

(define (run-replacement-workflow! root task)
  (write-task-source! root task (cl3-task-before task))
  (clear-compiled! root task)
  (define baseline (worker-phase! root task "replacement baseline" (cl3-task-before task)))
  (define edit-ms (elapsed (lambda () (write-task-source! root task (cl3-task-after task)))))
  (define fresh (worker-phase! root task "replacement fresh" (cl3-task-after task)))
  (define total-ms
    (+ (hash-ref baseline 'spawn-ms)
       (hash-ref baseline 'handshake-ms)
       (hash-ref baseline 'load-ms)
       (hash-ref baseline 'eval-ms)
       edit-ms
       (hash-ref fresh 'spawn-ms)
       (hash-ref fresh 'handshake-ms)
       (hash-ref fresh 'load-ms)
       (hash-ref fresh 'eval-ms)
       (hash-ref baseline 'cleanup-ms)
       (hash-ref fresh 'cleanup-ms)))
  (finish! total-ms
           2
           (max (hash-ref baseline 'memory) (hash-ref fresh 'memory))
           (hash 'baseline-phase baseline 'edit-ms edit-ms 'fresh-phase fresh)
           '("process replacement used a fresh OS process for the post-edit evaluation")))

;; `persistent-namespace-reset` is named for what it actually does: the post-edit
;; correct value only appears after reload AND a namespace reset plus reload.
(define approaches '(one-shot persistent-namespace-reset process-replacement))

(define (run-approach! root task approach)
  (case approach
    [(one-shot) (run-one-shot-workflow! root task)]
    [(persistent-namespace-reset) (run-namespace-reset-workflow! root task)]
    [(process-replacement) (run-replacement-workflow! root task)]
    [else (error 'cl3-wave2 "unknown approach: ~a" approach)]))

(define (run-trial! root task approach)
  (parameterize ([active-workers '()])
    (dynamic-wind void (lambda () (run-approach! root task approach)) kill-tracked-workers!)))

(define (task-report root task n)
  (write-task-source! root task (cl3-task-before task))
  (define compilation (measure-compilation! root task))
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
  (define _ordered collected)
  (hash 'name
        (cl3-task-name task)
        'module
        (cl3-task-module task)
        'typed?
        (cl3-task-typed? task)
        'expected-before
        (cl3-task-before task)
        'expected-after
        (cl3-task-after task)
        'compilation
        compilation
        'approaches
        ;; Trials are interleaved: each iteration rotates the approach order so
        ;; OS/Racket cache warmth from one approach does not systematically
        ;; favour a later one. The realised order is recorded per trial.
        (for/hash ([approach (in-list approaches)])
          (define trials (reverse (hash-ref collected approach)))
          (define total-values (map (lambda (trial) (hash-ref trial 'total-ms)) trials))
          (values approach
                  (hash 'n
                        n
                        'total-ms
                        (stats (format "~a / ~a" (cl3-task-name task) approach) total-values)
                        'process-count
                        (stats "direct Racket process count"
                               (map (lambda (trial) (hash-ref trial 'process-count)) trials))
                        'max-memory-bytes
                        (stats "cumulative allocation proxy"
                               (map (lambda (trial) (hash-ref trial 'max-memory-bytes)) trials))
                        'all-correct?
                        (andmap (lambda (trial) (hash-ref trial 'correct?)) trials)
                        'observations
                        (remove-duplicates (append-map (lambda (trial) (hash-ref trial 'observations))
                                                       trials))
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
     'correctness-oracle
     (string-append
      "an injected cl3-probe-value binding is written into a copy of each real module "
      "and its returned integer is checked against the expected value; this measures "
      "module-load and source-change overhead, NOT end-to-end debugging of real component logic")
     'compilation-definition
     (string-append
      "target-module `raco make` after deleting that module's compiled/ directory; "
      "dependency .zo files shared with the rest of the tree are NOT cleared, so this is "
      "not a fully cold dependency-chain build")
     'cache-policy
     (string-append
      "compiled/ is cleared once per task before the compilation block; trials are then "
      "interleaved with a per-iteration rotation recorded in trial.order, so OS/Racket "
      "cache warmth is distributed across approaches rather than fixed by approach order")
     'memory-definition
     "(current-memory-use 'cumulative) maximum across workflow evaluations; allocation proxy, not OS peak RSS or retained heap"
     'process-definition
     "direct Racket processes started by the workflow; process-tree ownership and descendants are not verified"
     'cleanup-definition
     "workers started for a trial are tracked and killed in a dynamic-wind, so cleanup runs even when an assertion fails"
     'tasks
     (for/list ([task (in-list tasks)])
       (task-report worktree task n))))
  (dynamic-wind void measure (lambda () (remove-worktree! worktree))))

(define (samples->wave2-report result out)
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
      [else (error 'cl3-wave2 "unknown option: ~a" (car args))]))
  (define result (run-benchmark #:n n))
  (cond
    [out-path
     (call-with-output-file out-path
                            (lambda (out) (samples->wave2-report result out))
                            #:exists 'truncate/replace)]
    [else
     (write-json result)
     (newline)]))
