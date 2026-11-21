#lang racket/base

;; scripts/cl3-benchmark-probe.rkt — CL3 Wave 1 evaluation harness.
;; Measures first/subsequent one-shot execution, persistent-worker requests and
;; a combined navigate→evaluate→edit→reload→reset→re-evaluate workflow. Every
;; protocol response is checked; durations are reported with outcomes, never as
;; bare timings that might conceal an error. Developer-only: no q tool, test
;; runner, admission record or CI policy is involved.

(require racket/cmdline
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         json
         "cl3-eval-probe.rkt"
         "semantic-navigator.rkt")

(provide percentile
         stats
         samples->report
         run-benchmark)

(define-runtime-path repo-root "..")

;; CL0-compatible linear interpolation at rank k = q*(n-1), rounded to 0.001 ms.
(define (percentile sorted fraction)
  (cond
    [(null? sorted) #f]
    [else
     (define s (sort (map (lambda (x) (exact->inexact x)) sorted) <))
     (define k (* fraction (sub1 (length s))))
     (define lo (inexact->exact (floor k)))
     (define hi (min (add1 lo) (sub1 (length s))))
     (define frac (- k lo))
     (/ (round (* 1000.0 (+ (* (- 1.0 frac) (list-ref s lo)) (* frac (list-ref s hi))))) 1000.0)]))

(define (stats label values)
  (define sorted (sort values <))
  (hash 'label
        label
        'n
        (length values)
        'p50
        (percentile sorted 0.5)
        'p90
        (percentile sorted 0.9)
        'max
        (if (null? sorted)
            #f
            (last sorted))
        'samples
        sorted))

(define (timed thunk)
  (define start (current-inexact-milliseconds))
  (define result (thunk))
  (values (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0) result))

(define (elapsed thunk)
  (define-values (ms result) (timed thunk))
  (void result)
  ms)

(define (one-shot code)
  (define exe (or (find-executable-path "racket") (error 'cl3 "racket not found")))
  (define-values (ms result)
    (timed (lambda ()
             (parameterize ([current-output-port (open-output-string)]
                            [current-error-port (open-output-string)])
               (system* exe "-e" code)))))
  (void result)
  ms)

(define (first-value-text response)
  (define values (hash-ref response 'values '()))
  (and (pair? values) (hash-ref (car values) 'text #f)))

(define (expect-status! response expected label)
  (define actual (hash-ref response 'status #f))
  (unless (equal? actual expected)
    (error 'cl3-benchmark
           (format "~a: expected status ~s, got ~s (~s)" label expected actual response)))
  response)

(define (expect-value! response expected label)
  (expect-status! response "ok" label)
  (define actual (first-value-text response))
  (unless (equal? actual expected)
    (error 'cl3-benchmark
           (format "~a: expected value ~s, got ~s (~s)" label expected actual response)))
  response)

(define (request-outcome response)
  (hash 'status
        (hash-ref response 'status #f)
        'value
        (first-value-text response)
        'error
        (hash-ref response 'error #f)))

(define (write-module! path value)
  (call-with-output-file
   path
   (lambda (out) (display (format "#lang racket/base\n(provide f)\n(define (f) ~a)\n" value) out))
   #:exists 'truncate/replace))

(define (run-benchmark #:n [n 10] #:workflow-n [workflow-n 5])
  ;; First sample is retained but not labelled a controlled cold-cache trial.
  (define one-shot-samples
    (for/list ([i (in-range n)])
      (one-shot "(displayln (+ 20 22))")))

  ;; Full startup includes start-worker! plus a confirming response.
  (define-values (worker-startup-full-ms _)
    (timed (lambda ()
             (define probe-session (start-worker! #:directory repo-root))
             (define probe (request! probe-session (hash 'op "reset")))
             (expect-status! probe "ok" "startup probe")
             (kill-worker! probe-session)
             probe)))

  (define session (start-worker! #:directory repo-root))
  (define first-request (request! session (hash 'op "eval" 'code "(+ 20 22)")))
  (expect-value! first-request "42" "first persistent request")
  (define worker-samples
    (cons (hash-ref first-request 'wall-ms)
          (for/list ([i (in-range (sub1 n))])
            (define response (request! session (hash 'op "eval" 'code "(+ 20 22)")))
            (expect-value! response "42" (format "persistent request ~a" (+ i 2)))
            (hash-ref response 'wall-ms))))

  ;; Single-session operation costs with verified outcomes: load → eval →
  ;; edit → reload → eval (stale) → reset → load → eval (fresh).
  (define module-file (build-path (find-system-path 'temp-dir) "cl3-benchmark-module.rkt"))
  (write-module! module-file 1)
  (define load-response (request! session (hash 'op "load" 'path (path->string module-file))))
  (expect-status! load-response "ok" "single-session load")
  (define initial-eval (request! session (hash 'op "eval" 'code "(f)")))
  (expect-value! initial-eval "1" "single-session initial eval")
  (write-module! module-file 2)
  (define reload-response (request! session (hash 'op "reload" 'path (path->string module-file))))
  (expect-status! reload-response "ok" "single-session reload")
  (define stale-eval (request! session (hash 'op "eval" 'code "(f)")))
  (expect-value! stale-eval "1" "single-session stale eval")
  (define reset-response (request! session (hash 'op "reset")))
  (expect-status! reset-response "ok" "single-session reset")
  (define reload-after-reset (request! session (hash 'op "load" 'path (path->string module-file))))
  (expect-status! reload-after-reset "ok" "single-session load after reset")
  (define fresh-eval (request! session (hash 'op "eval" 'code "(f)")))
  (expect-value! fresh-eval "2" "single-session fresh eval")
  (define cleanup (kill-worker! session))
  (unless (number? (hash-ref cleanup 'exit-status #f))
    (error 'cl3-benchmark "worker cleanup did not report a numeric exit status"))

  ;; Navigation is measured separately from worker operations, so the worker
  ;; duration can no longer be mislabelled as navigation time.
  (define workflow-navigator
    (for/list ([i (in-range workflow-n)])
      (elapsed (lambda () (module-query "agent/state.rkt" #:root repo-root)))))

  (define workflow-steps
    (for/list ([i (in-range workflow-n)])
      (write-module! module-file 1)
      (define-values (startup-ms worker) (timed (lambda () (start-worker! #:directory repo-root))))
      (define handshake (request! worker (hash 'op "reset")))
      (expect-status! handshake "ok" (format "workflow ~a handshake" i))
      (define load (request! worker (hash 'op "load" 'path (path->string module-file))))
      (expect-status! load "ok" (format "workflow ~a load" i))
      (define baseline (request! worker (hash 'op "eval" 'code "(f)")))
      (expect-value! baseline "1" (format "workflow ~a baseline" i))
      (define edited-value (+ 10 i))
      (define-values (edit-ms _edited) (timed (lambda () (write-module! module-file edited-value))))
      (define reload (request! worker (hash 'op "reload" 'path (path->string module-file))))
      (expect-status! reload "ok" (format "workflow ~a reload" i))
      (define after-reload (request! worker (hash 'op "eval" 'code "(f)")))
      (expect-value! after-reload "1" (format "workflow ~a stale eval" i))
      (define reset (request! worker (hash 'op "reset")))
      (expect-status! reset "ok" (format "workflow ~a reset" i))
      (define reload-after-clean (request! worker (hash 'op "load" 'path (path->string module-file))))
      (expect-status! reload-after-clean "ok" (format "workflow ~a reload after reset" i))
      (define final-eval (request! worker (hash 'op "eval" 'code "(f)")))
      (expect-value! final-eval (number->string edited-value) (format "workflow ~a final eval" i))
      (define step-cleanup (kill-worker! worker))
      (unless (number? (hash-ref step-cleanup 'exit-status #f))
        (error 'cl3-benchmark (format "workflow ~a cleanup failed" i)))
      (hash 'iteration
            i
            'worker-spawn-ms
            startup-ms
            'handshake-ms
            (hash-ref handshake 'wall-ms)
            'load-ms
            (hash-ref load 'wall-ms)
            'baseline-eval-ms
            (hash-ref baseline 'wall-ms)
            'edit-ms
            edit-ms
            'reload-ms
            (hash-ref reload 'wall-ms)
            'stale-eval-ms
            (hash-ref after-reload 'wall-ms)
            'reset-ms
            (hash-ref reset 'wall-ms)
            'reload-after-reset-ms
            (hash-ref reload-after-clean 'wall-ms)
            'fresh-eval-ms
            (hash-ref final-eval 'wall-ms)
            'outcomes
            (hash 'handshake
                  (request-outcome handshake)
                  'load
                  (request-outcome load)
                  'baseline
                  (request-outcome baseline)
                  'reload
                  (request-outcome reload)
                  'stale-eval
                  (request-outcome after-reload)
                  'reset
                  (request-outcome reset)
                  'reload-after-reset
                  (request-outcome reload-after-clean)
                  'fresh-eval
                  (request-outcome final-eval))
            'cleanup
            step-cleanup)))
  (delete-file module-file)
  (hash 'revision
        (string-trim (with-output-to-string
                      (lambda ()
                        (parameterize ([current-directory repo-root])
                          (system* (find-executable-path "git") "rev-parse" "HEAD")))))
        'host
        (format "~a/~a-bit" (system-type 'os) (system-type 'word))
        'n
        n
        'quantile-definition
        "linear interpolation at rank q*(n-1), rounded to 0.001 ms (CL0-compatible)"
        'first-one-shot
        (stats "first one-shot sample (not a controlled cold-cache trial)" (take one-shot-samples 1))
        'subsequent-one-shot
        (stats "subsequent one-shot samples" (cdr one-shot-samples))
        'first-worker-request
        (stats "first persistent request (excludes full start-worker! cost)" (take worker-samples 1))
        'subsequent-worker-request
        (stats "subsequent persistent requests" (cdr worker-samples))
        'worker-startup-full-ms
        worker-startup-full-ms
        'worker-operations
        (hash 'load-ms
              (hash-ref load-response 'wall-ms)
              'initial-eval-ms
              (hash-ref initial-eval 'wall-ms)
              'reload-ms
              (hash-ref reload-response 'wall-ms)
              'stale-eval-ms
              (hash-ref stale-eval 'wall-ms)
              'reset-ms
              (hash-ref reset-response 'wall-ms)
              'reload-after-reset-ms
              (hash-ref reload-after-reset 'wall-ms)
              'fresh-eval-ms
              (hash-ref fresh-eval 'wall-ms)
              'stale-after-reload?
              #t
              'fresh-after-reset?
              #t
              'outcomes
              (hash 'load
                    (request-outcome load-response)
                    'initial-eval
                    (request-outcome initial-eval)
                    'reload
                    (request-outcome reload-response)
                    'stale-eval
                    (request-outcome stale-eval)
                    'reset
                    (request-outcome reset-response)
                    'reload-after-reset
                    (request-outcome reload-after-reset)
                    'fresh-eval
                    (request-outcome fresh-eval)))
        'workflow
        (hash 'n
              workflow-n
              'navigator-query
              (stats "navigator query incl. source identity" workflow-navigator)
              'worker-steps
              workflow-steps)
        'limitations
        (string-append
         "Developer-only timings on one host/revision; no fresh-context agent trials, "
         "no correctness or cognitive-load claim. First one-shot sample is not a controlled "
         "cold-cache trial; startup and handshake are reported separately. Sub-millisecond "
         "durations are recorded at 0.001 ms resolution. Reload leaves captured closures and "
         "top-level bindings stale; a namespace reset clears interactive bindings but NOT "
         "process-global state, threads or OS resources. Worker replacement is the stronger "
         "clean-process boundary. Direct child cleanup is verified; descendant process-tree "
         "cleanup is not established in Wave 1.")))

(define (samples->report result out)
  (write-json result out)
  (newline out))

(module+ main
  (define out-path #f)
  (define n 10)
  (define workflow-n 5)
  (define args (vector->list (current-command-line-arguments)))
  (let loop ([args args])
    (cond
      [(null? args) (void)]
      [(equal? (car args) "--out")
       (set! out-path (cadr args))
       (loop (cddr args))]
      [(equal? (car args) "--n")
       (set! n (string->number (cadr args)))
       (loop (cddr args))]
      [(equal? (car args) "--workflow-n")
       (set! workflow-n (string->number (cadr args)))
       (loop (cddr args))]
      [else (error 'cl3-benchmark "unknown option: ~a" (car args))]))
  (define report (run-benchmark #:n n #:workflow-n workflow-n))
  (if out-path
      (call-with-output-file out-path
                             (lambda (out) (samples->report report out))
                             #:exists 'truncate/replace)
      (write-json report))
  (newline))
