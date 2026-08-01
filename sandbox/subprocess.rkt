#lang racket/base

;; sandbox/subprocess.rkt — subprocess management under custodians
;;
;; Provides a reusable subprocess runner with resource limits and
;; custodian-based cleanup. Every subprocess runs under its own custodian
;; so that timeout or kill cleans up all associated resources.

(require racket/contract
         racket/port
         racket/string
         racket/system
         "limits.rkt"
         ;; SEC-16 (v0.22.0): consolidated shell-quote
         (only-in "../util/shell-quote.rkt" shell-quote)
         ;; W3 v0.99.35: Pure result-boundary helpers
         "subprocess-helpers.rkt")

(define-logger subprocess)

;; Struct accessors (re-exported from subprocess-helpers.rkt)
(provide subprocess-result
         subprocess-result?
         subprocess-result-exit-code
         subprocess-result-stdout
         subprocess-result-stderr
         subprocess-result-timed-out?
         subprocess-result-elapsed-ms
         subprocess-result-truncated?
         ;; W3 v0.99.35: Pure result-boundary helpers (re-exported)
         ;; Note: make-error-result/make-success-result NOT re-exported here
         ;; to avoid conflict with tool.rkt's exports of the same names.
         exit-success?
         exit-timeout?
         exit-error?
         EXIT-CODE-TIMEOUT
         EXIT-CODE-ERROR
         ;; Parameters (no contract needed)
         default-timeout-seconds
         default-max-output-bytes
         current-secret-scrub-denylist
         current-secret-scrub-allowlist
         current-secret-scrub-patterns
         ;; W0 v0.99.77: per-execution trace id (instrumentation)
         current-subprocess-trace-id
         ;; Contracted functions
         (contract-out [run-subprocess
                        (->* (string?)
                             (#:args (listof string?)
                                     #:limits exec-limits?
                                     #:timeout (or/c number? #f)
                                     #:directory (or/c path-string? #f)
                                     #:environment any/c
                                     #:encoding symbol?
                                     ;; W1 v0.99.77: process-group kill support
                                     #:process-group? boolean?)
                             subprocess-result?)]
                       [kill-subprocess! (-> (or/c custodian? #f) void?)]
                       [sanitize-env (->* () (any/c) any/c)]
                       [secret-env-var? (-> string? boolean?)]))

;; W3 v0.99.35: subprocess-result struct moved to subprocess-helpers.rkt.
;; All struct exports above are re-exported from there.

;; --------------------------------------------------
;; Bounded port reader — reads incrementally with a byte budget
;; --------------------------------------------------

;; Non-blocking read — reads only bytes already available.
;; BUGFIX: Uses read-bytes-avail! which returns immediately with
;; available data, BUT it blocks waiting for NEW data when the pipe
;; has no data AND is still open (e.g., backgrounded child inherited it).
;; So we use sync/timeout with a short deadline to check availability
;; before each read, and stop when no more data is immediately ready.
(define (read-available-bounded p max-bytes)
  (if (or (not p) (port-closed? p))
      ""
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-subprocess-warning "read-available-bounded: ~a"
                                                           (exn-message e))
                                   "")])
        (define acc (open-output-bytes))
        (define buf (make-bytes (min 4096 max-bytes)))
        (let loop ([remaining max-bytes])
          (when (> remaining 0)
            ;; sync/timeout 0 returns #f only when NO bytes are available.
            ;; When bytes ARE available, it returns the port.
            ;; This prevents blocking when a backgrounded child holds the pipe.
            (define ready? (sync/timeout 0 p))
            (when ready?
              (define n (read-bytes-avail! buf p))
              (cond
                [(eof-object? n) (void)]
                [(number? n)
                 (define to-write (min n remaining))
                 (write-bytes buf acc 0 to-write)
                 (loop (- remaining to-write))]
                [else (void)]))))
        (get-output-string acc))))

(define (read-port-bounded p max-bytes)
  (define-values (reader get-result) (start-bounded-reader p max-bytes "read-port-bounded"))
  (sync reader)
  (define-values (text truncated?) (get-result))
  (if truncated?
      (string-append text (format "\n[output truncated at ~a bytes]" max-bytes))
      text))

;; Start a reader thread immediately after subprocess launch.  This prevents a
;; child that writes more than the OS pipe buffer from blocking forever while
;; the parent is still waiting for process exit.  The reader stores at most
;; max-bytes and drains the remainder so the child can finish; callers can ask
;; for the latest captured value even if timeout cleanup interrupts the thread.
(define (start-bounded-reader p max-bytes label)
  (define acc (open-output-bytes))
  (define lock (make-semaphore 1))
  (define truncated? #f)
  (define (record-bytes! bs [start 0] [end (bytes-length bs)])
    (call-with-semaphore lock (lambda () (write-bytes bs acc start end))))
  (define (mark-truncated!)
    (call-with-semaphore lock (lambda () (set! truncated? #t))))
  (define (snapshot)
    (call-with-semaphore lock
                         (lambda ()
                           (values (bytes->string/utf-8 (get-output-bytes acc) #\uFFFD) truncated?))))
  (define reader
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-subprocess-warning "~a: ~a" label (exn-message e)))])
                (unless (or (not p) (port-closed? p))
                  (define buf (make-bytes 4096))
                  (let loop ([remaining max-bytes])
                    (sync p)
                    (cond
                      [(> remaining 0)
                       (define n (read-bytes-avail! buf p))
                       (cond
                         [(eof-object? n) (void)]
                         [(number? n)
                          (define to-record (min n remaining))
                          (record-bytes! buf 0 to-record)
                          (when (> n remaining)
                            (mark-truncated!))
                          (loop (- remaining to-record))]
                         [else (void)])]
                      [else
                       ;; Budget exhausted.  Keep draining only if there is actually
                       ;; another byte beyond the budget.  If the next read is EOF,
                       ;; output was exactly max-bytes and is not truncated.
                       (define n (read-bytes-avail! buf p))
                       (cond
                         [(eof-object? n) (void)]
                         [(number? n)
                          (mark-truncated!)
                          (loop 0)]
                         [else (void)])])))))))
  (values reader snapshot))

;; W1 v0.99.77: Send a signal to a whole process group. The child must have
;; been launched under `setsid` so its PID == its PGID (process-group
;; leader). bash's `kill` builtin accepts `-- -PGID` (dash, `/bin/sh`, does
;; not), so we invoke bash explicitly. This is what lets a timed-out tool
;; call kill SIGSTOP'd grandchildren that hold inherited pipes open.
(define (signal-process-group! pid signal)
  (with-handlers
      ([exn:fail? (lambda (e) (log-subprocess-warning "signal group ~a: ~a" signal (exn-message e)))])
    (system (format "/bin/bash -c 'kill -~a -- -~a 2>/dev/null || true'" signal pid))))

;; --------------------------------------------------
;; Kill helper
;; --------------------------------------------------

(define (kill-subprocess! custodian)
  (custodian-shutdown-all custodian))

;; --------------------------------------------------
;; Shell quoting
;; --------------------------------------------------

;; shell-quote — POSIX single-quote escaping for command arguments.
;; SEC-16 (v0.22.0): Consolidated into util/shell-quote.rkt.
;; This file imports from there; no local definition needed.

;; --------------------------------------------------
;; Resolve command to executable path
;; --------------------------------------------------

(define (resolve-command cmd)
  (if (absolute-path? cmd)
      cmd
      (find-executable-path cmd)))

;; --------------------------------------------------
;; Environment sanitization — strip sensitive env vars
;; --------------------------------------------------

;; W3 v0.99.35: secret-patterns and SECRET-IMPLICIT-ALLOWLIST moved to
;; subprocess-helpers.rkt. secret-env-var? delegates to check-secret-var?.

;; ── RA-2 (v0.24.7): Configurable secret scrubbing ──
;; Extra denylist: additional regex patterns to scrub (extends secret-patterns).
(define current-secret-scrub-denylist (make-parameter '()))

;; Allowlist: env vars matching these patterns are kept even if they match deny.
(define current-secret-scrub-allowlist (make-parameter '()))

;; Replaceable denylist: if non-empty, REPLACES the default secret-patterns entirely.
;; When empty (default), the built-in secret-patterns from subprocess-helpers.rkt are used.
(define current-secret-scrub-patterns (make-parameter '()))

;; W0 v0.99.77: Per-execution trace id. Set by tool callers to correlate log
;; lines with a single tool execution. Each run-subprocess call consumes the
;; counter to produce a fresh id, optionally prefixed by the caller via
;; current-subprocess-trace-id (a string prefix, default "sp").
(define subprocess-trace-counter (make-parameter 0))
(define current-subprocess-trace-id (make-parameter "sp"))

(define (next-subprocess-trace-id)
  (define n (subprocess-trace-counter))
  (subprocess-trace-counter (add1 n))
  (format "~a-~a" (current-subprocess-trace-id) n))

(define (secret-env-var? name)
  (define name-str
    (if (bytes? name)
        (bytes->string/utf-8 name)
        name))
  (define effective-patterns (current-secret-scrub-patterns))
  (if (pair? effective-patterns)
      ;; User-provided patterns REPLACE the defaults entirely
      (for/or ([pat (in-list effective-patterns)])
        (regexp-match? pat name-str))
      ;; Use default patterns + extra denylist (backward compatible)
      (check-secret-var? name-str (current-secret-scrub-denylist) (current-secret-scrub-allowlist))))

;; DESIGN FACT (W6, v0.99.38 adapter audit): sanitize-env is the sole mutation
;; boundary for environment variables crossing into subprocesses. The pure
;; predicate (secret-env-var? / check-secret-var?) lives in subprocess-helpers.rkt.
;; This split follows the same pure-helpers + thin-shell pattern as metrics.rkt.
(define (sanitize-env [env (current-environment-variables)])
  (define clean (make-environment-variables))
  (define scrubbed-names '())
  (for ([name (in-list (environment-variables-names env))])
    (cond
      [(secret-env-var? name)
       (define name-str
         (if (bytes? name)
             (bytes->string/utf-8 name)
             name))
       (set! scrubbed-names (cons name-str scrubbed-names))]
      [else
       (define val (environment-variables-ref env name))
       (environment-variables-set! clean name val)]))
  ;; RA-2: Emit audit log when vars are scrubbed
  (unless (null? scrubbed-names)
    (log-subprocess-info "sanitize-env: scrubbed ~a env vars: ~a"
                         (length scrubbed-names)
                         (string-join (sort scrubbed-names string<?) ", ")))
  clean)

;; --------------------------------------------------
;; Main subprocess runner
;; --------------------------------------------------

;; W1-fix v0.99.77 (F-18b follow-up): Racket's subprocess event does not fire
;; for setsid-launched children. When #:process-group? #t wraps the child in
;; `setsid`, the child becomes a session/process-group leader and Racket's
;; runtime event-delivery never signals the subprocess object — even after the
;; child exits (the child lingers as a zombie and sync/timeout on the event
;; always times out). subprocess-status, however, transitions reliably
;; ('running → exit-code) for both launch modes. So the main wait and the
;; post-SIGTERM grace wait poll subprocess-status instead of syncing on the
;; event whenever we launched under setsid.
;;
;; Returns #t if the process exited within timeout-secs, #f on timeout.
(define (wait-subprocess-status! sp timeout-secs)
  (define deadline-ms (+ (current-inexact-milliseconds) (* timeout-secs 1000.0)))
  (let loop ()
    (cond
      [(not (eq? (subprocess-status sp) 'running)) #t]
      [(>= (current-inexact-milliseconds) deadline-ms) #f]
      [else
       (sleep 0.005)
       (loop)])))

(define (run-subprocess command
                        #:args [args '()]
                        #:limits [limits (default-exec-limits)]
                        #:timeout [timeout-secs #f]
                        #:directory [dir (current-directory)]
                        #:environment [env (sanitize-env)]
                        #:encoding [encoding 'utf-8]
                        ;; W1 v0.99.77: when #t (and `setsid` is available),
                        ;; launch the child as a session/process-group leader so
                        ;; the timeout path can SIGKILL the whole group.
                        #:process-group? [process-group? #f])
  (define effective-timeout (or timeout-secs (exec-limits-timeout-seconds limits)))
  (define max-output (exec-limits-max-output-bytes limits))

  (define cust (make-custodian (current-custodian)))
  (define start-ms (current-inexact-milliseconds))

  (with-handlers ([exn:fail? (lambda (e)
                               (custodian-shutdown-all cust)
                               (make-error-result
                                (format "Failed to execute: ~a" (exn-message e))
                                (inexact->exact (round (- (current-inexact-milliseconds)
                                                          start-ms)))))])

    (define cmd-path (resolve-command command))
    (unless cmd-path
      (error 'run-subprocess "command not found: ~a" command))

    ;; Build shell command line when args are provided
    (define cmd-string
      (if (path? cmd-path)
          (path->string cmd-path)
          cmd-path))

    ;; W1 v0.99.77: optional setsid wrapper. When process-group? is #t and
    ;; setsid exists (Linux), the child becomes a session/process-group leader
    ;; (PGID == child PID) so the timeout path can signal the whole group.
    ;; On platforms without setsid (e.g. macOS), fall back to direct launch;
    ;; the timeout path still sends SIGTERM then SIGKILL to the direct child.
    (define setsid-path (and process-group? (find-executable-path "setsid")))

    (define-values (sp stdout-in stdin-out stderr-in)
      (parameterize ([current-custodian cust]
                     [current-directory dir]
                     [current-environment-variables env])
        (cond
          [setsid-path
           ;; Wrap with setsid: setsid execs the command without forking when
           ;; the caller is not a process-group leader (which is the case for
           ;; a subprocess spawned by Racket), so the PID stays the same.
           (apply subprocess
                  #f
                  #f
                  #f
                  setsid-path
                  (if (null? args)
                      (list cmd-string)
                      (cons cmd-string args)))]
          ;; No args: run command directly
          [(null? args) (subprocess #f #f #f cmd-path)]
          ;; With args: pass as direct arg vector — avoids shell injection
          ;; (previously used /bin/sh -c which was vulnerable to injection)
          [else (apply subprocess #f #f #f cmd-path args)])))

    ;; Close stdin immediately and start output drainers before waiting for the
    ;; child.  Waiting first can deadlock when child output fills the OS pipe.
    (close-output-port stdin-out)
    (define-values (stdout-reader get-stdout) (start-bounded-reader stdout-in max-output "stdout"))
    (define-values (stderr-reader get-stderr) (start-bounded-reader stderr-in max-output "stderr"))

    (define (reader-results)
      (define-values (out-str out-truncated?) (get-stdout))
      (define-values (err-str err-truncated?) (get-stderr))
      (values out-str err-str (or out-truncated? err-truncated?)))

    (define (finish-reader! reader port label)
      (unless (sync/timeout 1 reader)
        ;; A backgrounded grandchild can inherit stdout/stderr after the direct
        ;; subprocess exits.  Close our read end instead of waiting forever for
        ;; an EOF that may not arrive until the grandchild exits.
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (log-subprocess-warning "close inherited ~a: ~a" label (exn-message e)))])
          (close-input-port port))
        (sync/timeout 0.25 reader)))

    ;; Wait with timeout
    (define trace-id (next-subprocess-trace-id))
    ;; W1-fix: the subprocess event never fires for setsid children, so poll
    ;; subprocess-status when we launched under setsid; sync on the event
    ;; otherwise (direct launch, or macOS where setsid is absent).
    (define evt-result
      (if setsid-path
          (wait-subprocess-status! sp effective-timeout)
          (sync/timeout effective-timeout sp)))

    (cond
      ;; Timeout
      [(not evt-result)
       (define child-pid (subprocess-pid sp))
       (log-subprocess-info "~a: timeout after ~as (pid ~a, status ~s) — attempting SIGTERM"
                            trace-id
                            effective-timeout
                            child-pid
                            (subprocess-status sp))
       ;; W1 v0.99.77: Phase 1 — SIGTERM.
       ;; FIX (F-18b): previously called (subprocess-kill sp) with one argument
       ;; (arity error, silently swallowed) so NO signal was delivered.
       ;; Correct arity is (subprocess-kill subprocess kill?) where #f = SIGTERM
       ;; and #t = SIGKILL. Send SIGTERM to the direct child and, when we
       ;; launched under setsid, to the whole process group.
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "~a: subprocess-kill TERM: ~a"
                                                            trace-id
                                                            (exn-message e)))])
         (subprocess-kill sp #f))
       (when setsid-path
         (signal-process-group! child-pid "TERM"))
       (log-subprocess-info "~a: after SIGTERM: pid ~a status ~s"
                            trace-id
                            child-pid
                            (subprocess-status sp))
       ;; Grace period: allow a clean exit from SIGTERM before escalating to
       ;; SIGKILL (which cannot be caught/ignored). Same poll-vs-event split as
       ;; the main wait: the event never fires for setsid children.
       (define grace-exit?
         (if setsid-path
             (wait-subprocess-status! sp 2)
             (sync/timeout 2 sp)))
       (unless grace-exit?
         (log-subprocess-info "~a: SIGKILL after grace (pid ~a)" trace-id child-pid)
         ;; Phase 2 — SIGKILL. SIGTERM is NOT delivered to SIGSTOP'd
         ;; processes, but SIGKILL is; this kills stopped children and, via
         ;; the process group, any grandchildren holding inherited pipes.
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-subprocess-warning "~a: subprocess-kill KILL: ~a"
                                                              trace-id
                                                              (exn-message e)))])
           (subprocess-kill sp #t))
         (when setsid-path
           (signal-process-group! child-pid "KILL"))
         (log-subprocess-info "~a: after SIGKILL: pid ~a status ~s"
                              trace-id
                              child-pid
                              (subprocess-status sp)))
       ;; Give reader threads a bounded chance to observe EOF and flush their
       ;; latest snapshots.  If they do not finish, snapshot still returns the
       ;; latest captured bytes.
       (sync/timeout 1 stdout-reader)
       (sync/timeout 1 stderr-reader)
       (define-values (partial-out partial-err partial-truncated?) (reader-results))
       (define end-ms (current-inexact-milliseconds))
       (custodian-shutdown-all cust)
       (log-subprocess-info "~a: after custodian shutdown: pid ~a status ~s"
                            trace-id
                            child-pid
                            (subprocess-status sp))
       (make-timeout-result partial-out
                            partial-err
                            effective-timeout
                            (inexact->exact (round (- end-ms start-ms)))
                            partial-truncated?)]

      ;; Completed
      [else
       ;; Process exited; readers should finish after draining stdout/stderr.
       ;; Bound the join to preserve the background-child inherited-pipe fix.
       (finish-reader! stdout-reader stdout-in "stdout")
       (finish-reader! stderr-reader stderr-in "stderr")
       (define-values (out-str err-str output-truncated?) (reader-results))
       (define exit-code (subprocess-status sp))

       ;; W0 v0.99.77 instrumentation: log nonzero exit (signal / wait-status).
       (when (and (number? exit-code) (not (zero? exit-code)))
         (log-subprocess-info "~a: exit ~a (pid ~a)" trace-id exit-code (subprocess-pid sp)))

       ;; Close ports; may already be closed by EOF/custodian shutdown.
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "close stdout: ~a" (exn-message e)))])
         (close-input-port stdout-in))
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "close stderr: ~a" (exn-message e)))])
         (close-input-port stderr-in))
       (custodian-shutdown-all cust)

       (define end-ms (current-inexact-milliseconds))
       (make-success-result exit-code
                            out-str
                            err-str
                            (inexact->exact (round (- end-ms start-ms)))
                            output-truncated?)])))
