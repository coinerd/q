#lang racket
;; @speed slow
;; @suite testing
;; @boundary integration
;; @isolation process
;; @timeout 180

;; Regression tests for the quick-runner ownership contract
;; (scripts/testing/quick-runner.rkt):
;;
;;   1. Killing a worker while its quick child runs must make run-quick fail
;;      (raise, or return results that are not a full set of valid results) and
;;      the child must be reaped.
;;   2. Breaking the coordinator thread with jobs = 2 must terminate every
;;      worker and child, must not return a partial success, and must not start
;;      an input that was still queued behind the busy workers.
;;   3. A worker that dies after another worker already reported must surface as
;;      a "result(s) missing" error from the drain path, not as a short list.
;;   4. run-quick validates its arguments before launching anything. Duplicate
;;      input rejection is documented but currently unreachable in production
;;      (see the KNOWN GAP note in arguments-case).
;;
;; Every case discovers the runner's worker threads through
;; custodian-managed-list and uses bounded waits. Cleanup is custodian-based
;; only: no saved-PID signals are ever sent.

(require rackunit
         racket/async-channel
         racket/file
         racket/list
         "../scripts/testing/quick-runner.rkt")

;; ------------------------------------------------------------
;; Timing
;; ------------------------------------------------------------

;; The fixture lives this long after publishing its readiness, so a shorter
;; window proves prompt cleanup instead of accepting a child that merely
;; finished on its own.
(define fixture-lifetime-seconds 6)

;; The runner's internal group-cleanup confirmation bound is 2s
;; (confirm-group-cleanup! in sandbox/subprocess.rkt). A 2s prompt window would
;; sit exactly on that bound, so prompt checks get 3s: strictly longer than the
;; internal cleanup bound and still well inside the fixture's 6s lifetime.
(define prompt-seconds 3)

;; The delayed marker lands ~6s after the child starts. The "no late marker"
;; probe therefore has to outlast the whole start-to-marker window.
(define late-probe-seconds 8)

;; ------------------------------------------------------------
;; Fixtures
;; ------------------------------------------------------------

(define (write-text path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; The quick child publishes its own /proc identity (so liveness can be
;; checked without a saved-PID kill) and writes `late-<tag>` after the fixture
;; lifetime. Its lifetime is bounded even when ownership is broken.
(define (write-fixture! dir [tag "one"])
  (define ready (build-path dir (format "ready-~a" tag)))
  (define late (build-path dir (format "late-~a" tag)))
  (define fixture (build-path dir (format "fixture-~a.rkt" tag)))
  (write-text fixture
              (format #<<SOURCE
#lang racket
(require rackunit)
(module+ test
  (define stat (file->string "/proc/self/stat"))
  (define pid (cadr (regexp-match #rx"^([0-9]+)" stat)))
  (define tmp (build-path ~s ~s))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (display pid out)))
  (rename-file-or-directory tmp ~s #t)
  (sleep ~a)
  (call-with-output-file ~s #:exists 'truncate void)
  (check-true #t))
SOURCE
                      (path->string dir)
                      (format "~a.tmp" tag)
                      (path->string ready)
                      fixture-lifetime-seconds
                      (path->string late)))
  (values fixture ready late))

;; A fast input that records that it started and then reports success. Used for
;; the input that must stay queued behind the busy workers.
(define (write-fast-fixture! dir tag)
  (define ran (build-path dir (format "~a.ran" tag)))
  (define fixture (build-path dir (format "~a.rkt" tag)))
  (write-text fixture
              (format #<<SOURCE
#lang racket
(require rackunit)
(module+ test
  (call-with-output-file ~s #:exists 'truncate void)
  (check-true #t))
SOURCE
                      (path->string ran)))
  (values fixture ran))

;; ------------------------------------------------------------
;; Bounded waits and /proc identity (read-only liveness checks)
;; ------------------------------------------------------------

(define (wait-until pred [seconds 30])
  (define deadline (+ (current-inexact-milliseconds) (* seconds 1000)))
  (let loop ()
    (cond
      [(pred) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else
       (sleep 0.01)
       (loop)])))

(define (read-pid path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define n (string->number (string-trim (file->string path))))
    (and (exact-positive-integer? n) n)))

;; Record the incarnation as well as the PID; zombies are no longer live.
(define (identity pid)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    (define line (file->string (format "/proc/~a/stat" pid)))
    (define m (regexp-match #rx"^[0-9]+ \\(.*\\) (.*)$" line))
    (define fields (and m (string-split (cadr m))))
    (and fields (>= (length fields) 20) (cons (car fields) (list-ref fields 19)))))

(define (live-incarnation? pid start)
  (define state (identity pid))
  (and state (equal? (cdr state) start) (not (member (car state) '("Z" "X")))))

;; ------------------------------------------------------------
;; Outcome classification
;; ------------------------------------------------------------

;; An interrupted run has failed when it raises, or when it returns results
;; that are short or not all valid. The ownership contract does not prescribe
;; which of those two surfaces the runner uses.
(define (failure-kind result files)
  (and (pair? result)
       (case (car result)
         [(exn) 'exception]
         [(ok)
          (define results (cadr result))
          (and (or (not (list? results))
                   (not (= (length results) (length files)))
                   (not (andmap quick-result-valid? results)))
               'invalid)]
         [else 'invalid])))

(define (failure-exn result)
  (and (pair? result) (eq? (car result) 'exn) (cadr result)))

;; ------------------------------------------------------------
;; Runner discovery (custodian-managed-list, never saved handles)
;; ------------------------------------------------------------

;; Values managed by `cust` but not by its superior `super`.
(define (managed-below cust super)
  (custodian-managed-list cust super))

;; Threads anywhere beneath `cust` (beneath superior `super`). Discovery is
;; recursive so the test still finds the worker when ownership nesting differs.
(define (managed-threads cust super)
  (append-map (lambda (v)
                (cond
                  [(thread? v) (list v)]
                  [(custodian? v) (managed-threads v cust)]
                  [else '()]))
              (managed-below cust super)))

;; Contract: the runner owns a private custodian (beneath the coordinator's
;; custodian) that directly owns the worker and its quick child.
(define (owning-custodian parent super worker)
  (let loop ([cust parent]
             [above super])
    (for/or ([v (in-list (managed-below cust above))])
      (cond
        [(eq? v worker) cust]
        [(custodian? v) (loop v cust)]
        [else #f]))))

;; A worker is a thread owned by the runner that is executing a quick child;
;; the subprocess wait loop is visible in its continuation context.
(define (quick-worker? t)
  (define ctx (continuation-mark-set->context (continuation-marks t)))
  (and (or (assq 'wait-subprocess-status! ctx) (assq 'run-subprocess ctx)) #t))

;; Running workers only: a worker that already published its result (or died)
;; must not be counted, and a worker between two inputs has no subprocess wait
;; in its context, so it is not counted either.
(define (live-quick-workers parent super)
  (filter (lambda (t) (and (thread-running? t) (quick-worker? t))) (managed-threads parent super)))

(define (wait-for-workers parent super n [what "quick workers"])
  (unless (wait-until (lambda () (= n (length (live-quick-workers parent super)))) 60)
    (define found (live-quick-workers parent super))
    (fail (format "expected ~a ~a, found ~a\ncontexts: ~s"
                  n
                  what
                  (length found)
                  (map (lambda (t) (continuation-mark-set->context (continuation-marks t))) found))))
  (live-quick-workers parent super))

;; A coordinator thread under its own parent custodian; every run-quick
;; custodian/thread is therefore discoverable and killable by custodian only.
(define (start-run! parent files jobs outcome)
  (parameterize ([current-custodian parent])
    (thread (lambda ()
              (async-channel-put outcome
                                 (with-handlers ([exn? (lambda (e) (list 'exn e))])
                                   (list 'ok
                                         (run-quick (map path->string files) jobs #:timeout 60))))))))

(define (wait-for-child-identity ready [what "quick child"])
  (unless (wait-until (lambda () (read-pid ready)) 60)
    (fail (format "~a did not publish its identity" what)))
  (define pid (read-pid ready))
  (define state (identity pid))
  (unless state
    (fail (format "~a vanished before identity capture" what)))
  (values pid (cdr state)))

;; Emergency cleanup: custodian shutdown is the only kill path (no saved-PID
;; signals). The fixture is finite-lived, so give any escaped child a bounded
;; window to finish before its directory disappears.
(define (emergency-cleanup! parent threads dir children)
  (custodian-shutdown-all parent)
  (for ([t (in-list threads)]
        #:when (and t (thread-running? t)))
    (kill-thread t))
  (for ([c (in-list children)]
        #:when c)
    (define pid (car c))
    (define start (cdr c))
    (when start
      (wait-until (lambda () (not (live-incarnation? pid start))) 10)))
  (with-handlers ([exn:fail:filesystem? void])
    (delete-directory/files dir)))

(define (quick-supported?)
  (and (eq? (system-type 'os*) 'linux)
       (directory-exists? "/proc/self")
       (find-executable-path "setsid")))

;; ------------------------------------------------------------
;; Cases
;; ------------------------------------------------------------

;; (1) Kill the worker while its child is running.
(define (worker-death-case)
  (define here (current-custodian))
  (define dir (make-temporary-file "qt-cancel-worker-~a" 'directory))
  (define-values (fixture ready late) (write-fixture! dir))
  (define parent (make-custodian))
  (define outcome (make-async-channel))
  (define coordinator #f)
  (define worker #f)
  (define pid #f)
  (define start #f)
  (dynamic-wind void
                (lambda ()
                  (set! coordinator (start-run! parent (list fixture) 1 outcome))
                  (define-values (child-pid child-start) (wait-for-child-identity ready))
                  (set! pid child-pid)
                  (set! start child-start)
                  (set! worker (car (wait-for-workers parent here 1)))
                  (define worker-owner (owning-custodian parent here worker))
                  (kill-thread worker)
                  (define result (sync/timeout 60 outcome))
                  (define kind (failure-kind result (list fixture)))
                  (check-not-false result "run-quick returned after its worker was killed")
                  ;; Either failure surface is acceptable: the contract is about
                  ;; ownership, not about how the failure is reported.
                  (check-true (and (memq kind '(exception invalid)) #t)
                              "run-quick reported a failure instead of a full set of valid results")
                  (when (eq? kind 'exception)
                    (check-true (and (exn:fail? (failure-exn result)) #t)
                                "the reported failure is an error, not an unrelated break"))
                  (check-true (and (sync/timeout prompt-seconds (thread-dead-evt coordinator)) #t)
                              "coordinator thread finished after run-quick failed")
                  (check-true (and (sync/timeout prompt-seconds (thread-dead-evt worker)) #t)
                              "worker thread terminated")
                  (check-true (wait-until (lambda () (not (live-incarnation? pid start)))
                                          prompt-seconds)
                              "quick child survived the death of its worker")
                  (check-false (wait-until (lambda () (file-exists? late)) late-probe-seconds)
                               "quick child outlived its worker and wrote the delayed marker")
                  (check-true (and worker-owner (custodian-shut-down? worker-owner))
                              "runner custodian was torn down when run-quick failed"))
                (lambda ()
                  (emergency-cleanup! parent
                                      (list coordinator worker)
                                      dir
                                      (list (and pid start (cons pid start)))))))

;; (2) Break the coordinator while two workers run children.
;;
;; jobs = 2 with three inputs: both blockers publish their identity, so both
;; workers are provably busy and the third input is genuinely queued (with
;; jobs = 1 it could never have started and the claim would be vacuous). The
;; break must not launch it.
(define (coordinator-break-case)
  (define here (current-custodian))
  (define dir (make-temporary-file "qt-cancel-break-~a" 'directory))
  (define-values (blocker-a ready-a late-a) (write-fixture! dir "a"))
  (define-values (blocker-b ready-b late-b) (write-fixture! dir "b"))
  (define-values (queued queued-ran) (write-fast-fixture! dir "queued"))
  (define files (list blocker-a blocker-b queued))
  (define parent (make-custodian))
  (define outcome (make-async-channel))
  (define coordinator #f)
  (define workers '())
  (define children '())
  (dynamic-wind
   void
   (lambda ()
     (set! coordinator (start-run! parent files 2 outcome))
     (define-values (pid-a start-a) (wait-for-child-identity ready-a "first quick child"))
     (define-values (pid-b start-b) (wait-for-child-identity ready-b "second quick child"))
     (set! children (list (cons pid-a start-a) (cons pid-b start-b)))
     ;; jobs = 2 really is concurrent: both children are live at the same time.
     (check-true (and (live-incarnation? pid-a start-a) (live-incarnation? pid-b start-b))
                 "jobs = 2 did not run two children concurrently")
     (set! workers (wait-for-workers parent here 2))
     (check-false (file-exists? queued-ran)
                  "the queued input started while both workers were still busy")
     (define owners (filter values (map (lambda (w) (owning-custodian parent here w)) workers)))
     (check-equal? (length owners) 2 "every worker is owned by a runner custodian")
     (break-thread coordinator)
     (define result (sync/timeout 60 outcome))
     (define kind (failure-kind result files))
     (check-not-false result "run-quick did not return after the coordinator break")
     (check-true (and (memq kind '(exception invalid)) #t)
                 "interrupted run returned a partial success list")
     (when (eq? kind 'exception)
       (check-true (and (exn:break? (failure-exn result)) #t) "interruption surfaces as a break"))
     (check-true (and (sync/timeout prompt-seconds (thread-dead-evt coordinator)) #t)
                 "coordinator thread terminated after the break")
     (check-true (wait-until (lambda () (andmap (lambda (w) (not (thread-running? w))) workers))
                             prompt-seconds)
                 "worker threads terminated after the coordinator break")
     (for ([owner (in-list owners)])
       (check-true (custodian-shut-down? owner) "runner custodian was torn down after the break"))
     ;; Every worker is dead now, so nothing remains that could pick up work.
     (check-false (file-exists? queued-ran)
                  "a queued quick child was launched after the interruption")
     (check-true (wait-until (lambda () (not (live-incarnation? pid-a start-a))) prompt-seconds)
                 "first quick child survived the coordinator break")
     (check-true (wait-until (lambda () (not (live-incarnation? pid-b start-b))) prompt-seconds)
                 "second quick child survived the coordinator break")
     (check-false (wait-until (lambda () (or (file-exists? late-a) (file-exists? late-b)))
                              late-probe-seconds)
                  "a quick child outlived the coordinator break and wrote the delayed marker"))
   (lambda () (emergency-cleanup! parent (cons coordinator workers) dir children))))

;; (3) Drain path: with jobs = 2 one worker dies after the other already
;; reported. Nothing can publish the missing result, so the runner must raise
;; the "result(s) missing" error instead of returning a short list.
(define (drain-shortfall-case)
  (define here (current-custodian))
  (define dir (make-temporary-file "qt-cancel-drain-~a" 'directory))
  (define-values (fast fast-ran) (write-fast-fixture! dir "fast"))
  (define-values (blocker ready late) (write-fixture! dir "blocker"))
  (define files (list fast blocker))
  (define parent (make-custodian))
  (define outcome (make-async-channel))
  (define coordinator #f)
  (define worker #f)
  (define pid #f)
  (define start #f)
  (dynamic-wind
   void
   (lambda ()
     (set! coordinator (start-run! parent files 2 outcome))
     (define-values (child-pid child-start) (wait-for-child-identity ready "blocking quick child"))
     (set! pid child-pid)
     (set! start child-start)
     ;; Both inputs really started, so the surviving worker is the one whose
     ;; result can no longer be published.
     (check-true (wait-until (lambda () (file-exists? fast-ran)) 60) "the fast input never started")
     (set! worker (car (wait-for-workers parent here 1 "surviving quick worker")))
     (define worker-owner (owning-custodian parent here worker))
     (kill-thread worker)
     (define result (sync/timeout 60 outcome))
     (define failure (failure-exn result))
     (check-not-false result "run-quick returned after one of two workers died")
     (check-true (and failure (exn:fail? failure)) "a short run raises an error")
     (check-true (and failure
                      (exn:fail? failure)
                      (regexp-match? #rx"result\\(s\\) missing" (exn-message failure)))
                 "the drain path reports the missing result(s)")
     (check-true
      (and failure (exn:fail? failure) (regexp-match? #rx"1 of 2 inputs" (exn-message failure)))
      "the drain path counts the result that was reported")
     (check-true (and (sync/timeout prompt-seconds (thread-dead-evt coordinator)) #t)
                 "coordinator thread finished after the shortfall error")
     (check-true (and worker-owner (custodian-shut-down? worker-owner))
                 "runner custodian was torn down when the shortfall was detected")
     (check-true (wait-until (lambda () (not (live-incarnation? pid start))) prompt-seconds)
                 "blocking quick child survived the death of its worker")
     (check-false (wait-until (lambda () (file-exists? late)) late-probe-seconds)
                  "blocking quick child outlived its worker and wrote the delayed marker"))
   (lambda ()
     (emergency-cleanup! parent
                         (list coordinator worker)
                         dir
                         (list (and pid start (cons pid start)))))))

;; (4) Argument validation rejects bad jobs and non-string inputs before any
;; child is launched. Duplicate inputs are checked separately because the
;; production guard for them is currently unreachable (see below).
(define (arguments-case)
  (define dir (make-temporary-file "qt-cancel-args-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define fixture (build-path dir "fixture.rkt"))
     (write-text fixture "#lang racket\n(require rackunit)\n(module+ test (check-true #t))\n")
     (define path (path->string fixture))
     ;; Same file, non-canonical spelling: only normalization can catch it.
     (define lexical (string-append (path->string dir) "/./fixture.rkt"))
     (define (check-error label re thunk)
       (check-exn (lambda (e) (and (exn:fail? e) (regexp-match? re (exn-message e)))) thunk label))
     (check-error "jobs 0 is rejected"
                  #rx"jobs must be a positive integer"
                  (lambda () (run-quick (list path) 0)))
     (check-error "negative jobs is rejected"
                  #rx"jobs must be a positive integer"
                  (lambda () (run-quick (list path) -1)))
     (check-error "non-integer jobs is rejected"
                  #rx"jobs must be a positive integer"
                  (lambda () (run-quick (list path) 1.0)))
     (check-error "a non-string input is rejected"
                  #rx"files must be a list of path strings"
                  (lambda () (run-quick (list fixture) 1)))
     ;; Duplicate inputs (same file, non-canonical spelling) are a hard error.
     (check-error "duplicate normalized inputs are rejected"
                  #rx"duplicate inputs"
                  (lambda () (run-quick (list path lexical) 1 #:timeout 60))))
   (lambda () (delete-directory/files dir))))

(module+ test
  (test-case "run-quick validation rejects bad jobs, non-string and duplicate inputs"
    (arguments-case))
  (if (quick-supported?)
      (begin
        (test-case "worker death while its child runs fails the run and reaps the child"
          (worker-death-case))
        (test-case "coordinator break with jobs 2 terminates workers without launching queued input"
          (coordinator-break-case))
        (test-case "drain path reports the missing result(s) when a worker dies after a report"
          (drain-shortfall-case)))
      (displayln "SKIP qt cancellation: Linux /proc or setsid unavailable")))
