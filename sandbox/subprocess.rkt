#lang racket/base

;; sandbox/subprocess.rkt — subprocess management under custodians
;;
;; Provides a reusable subprocess runner with resource limits and
;; custodian-based cleanup. Every subprocess runs under its own custodian
;; so that timeout or kill cleans up all associated resources.

(require racket/contract
         racket/file
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
(define (start-bounded-reader p max-bytes label [encoding 'utf-8])
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
                           ;; Authorization facts must not conflate invalid
                           ;; pathname bytes with a literal U+FFFD pathname.
                           ;; Display-oriented callers retain replacement mode.
                           (values (bytes->string/utf-8 (get-output-bytes acc)
                                                       (if (eq? encoding 'utf-8/strict) #f #\uFFFD))
                                   truncated?))))
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

;; The command must not own the group's lifetime: it can exit on TERM while
;; descendants ignore TERM. A supervisor retains ownership until it kills
;; group 0 FROM INSIDE the group. No caller ever signals a saved numeric PGID
;; (even checking subprocess-status before such a signal would race reap/reuse).
;; Keep the real command status out of stdout/stderr, which belong to the tool.
(define group-owner-script
  (string-append
   "result=$1; watcher=$2; shift 2\n"
   "cleanup() { trap '' TERM HUP INT; kill -TERM 0; sleep 0.25; kill -KILL 0; }\n"
   "trap cleanup TERM HUP INT\n"
   ;; A second in-group owner watches a private control pipe. Closing it also
   ;; cleans a STOP'd supervisor: no external -PGID fallback is needed. The
   ;; command sees EOF on stdin and cannot consume the control pipe.
   "exec 3<&0\n"
   "( IFS= read -r ignored <&3; cleanup ) <&3 &\n"
   ;; Publish the subshell's actual incarnation ($$ is the supervisor in a
   ;; POSIX subshell). Never start the command without this handshake.
   "IFS= read -r watcher_stat < /proc/$!/stat || exit 125\n"
   "printf '%s\\n' \"$watcher_stat\" > \"$watcher\" || exit 125\n"
   "\"$@\" </dev/null 3<&- &\n"
   "wait $! 2>/dev/null\n"
   "code=$?\n"
   "printf '%s\\n' \"$code\" > \"$result\"\n"
   "kill -KILL 0\n"))

(define (group-command-status result-path fallback)
  (with-handlers ([exn:fail? (lambda (_) fallback)])
    (define raw (call-with-input-file result-path (lambda (in) (read-bytes 5 in))))
    (if (and (bytes? raw) (regexp-match? #rx#"^[0-9][0-9]?[0-9]?\n$" raw))
        (let ([code (string->number (string-trim (bytes->string/utf-8 raw)))])
          (if (<= 0 code 255) code fallback))
        fallback)))

;; Linux /proc is the confirmation boundary, as in verification-job. A dead
;; supervisor alone is NOT evidence that its control-pipe watcher has killed
;; the group. All uses of a numeric group/session identity below are READ ONLY.
(define (proc-stat-fields raw)
  ;; comm can contain spaces and parentheses: resume after the LAST ") ".
  (define match (and (string? raw) (regexp-match #rx"^[0-9]+ \\(.*\\) (.*)$" raw)))
  (define fields (and match (string-split (cadr match))))
  (and fields (>= (length fields) 20) fields))

(define (read-watcher-identity path owner-pid)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define raw (file->string path))
    (define pid-match (regexp-match #rx"^([0-9]+) " raw))
    (define fields (proc-stat-fields raw))
    (define ticks (and fields (string->number (list-ref fields 19))))
    (and pid-match (exact-nonnegative-integer? ticks)
         (= owner-pid (string->number (list-ref fields 2)))
         (= owner-pid (string->number (list-ref fields 3)))
         (cons (string->number (cadr pid-match)) ticks))))

(define (proc-entry-gone? e)
  (and (memv (car (exn:fail:filesystem:errno-errno e)) '(2 3)) #t))

(define (watcher-confirmed-dead? identity)
  ;; Permission errors, malformed data, etc. must refuse confirmation. Only
  ;; disappearance, a new incarnation, or an explicitly dead state proves it.
  (with-handlers ([exn:fail:filesystem:errno? proc-entry-gone?]
                  [exn:fail? (lambda (_) #f)])
    (define fields
      (proc-stat-fields
       (call-with-input-file (format "/proc/~a/stat" (car identity)) read-line)))
    (define ticks (and fields (string->number (list-ref fields 19))))
    (and (exact-nonnegative-integer? ticks)
         (or (not (= ticks (cdr identity)))
             (and (member (car fields) '("Z" "X")) #t)))))

(define (group-quiescent? owner-pid)
  ;; Watcher death can precede SIGKILL delivery to another member. Do not
  ;; release on watcher death alone, or mistake redirected pipe EOF for death.
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (for/and ([entry (in-list (directory-list "/proc"))]
              #:when (regexp-match? #rx"^[0-9]+$" (path->string entry)))
      (with-handlers ([exn:fail:filesystem:errno? proc-entry-gone?])
        (define fields
          (proc-stat-fields (call-with-input-file (build-path "/proc" entry "stat") read-line)))
        (and fields
             (or (and (not (= owner-pid (string->number (list-ref fields 2))))
                      (not (= owner-pid (string->number (list-ref fields 3)))))
                 (and (member (car fields) '("Z" "X")) #t)))))))

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
  (define child #f)
  (define grouped? #f)
  (define control-out #f)
  (define result-path #f)
  (define watcher-path #f)
  (define watcher-identity #f)
  (define cleanup-attempted? #f)
  (define cleanup-confirmed? #f)
  (define (remove-result!)
    (for ([path (in-list (list result-path watcher-path))] #:when path)
      (with-handlers ([exn:fail? void]) (delete-file path))))
  (define (request-group-cleanup!)
    (when (and control-out (not (port-closed? control-out)))
      (close-output-port control-out)))
  ;; One bounded confirmation shared by completion, timeout, and cancellation.
  ;; Disable breaks across the attempt so an escaped break cannot interrupt
  ;; cleanup and cache a half-finished verdict. A startup break may arrive
  ;; before the handshake is read; continue acquiring it during this bound.
  (define (confirm-group-cleanup!)
    (parameterize-break #f
      (unless cleanup-attempted?
        (set! cleanup-attempted? #t)
        (request-group-cleanup!)
        (set! cleanup-confirmed?
              (let loop ([deadline (+ (current-inexact-monotonic-milliseconds) 2000)])
                (unless watcher-identity
                  (set! watcher-identity (read-watcher-identity watcher-path (subprocess-pid child))))
                (cond
                  [(and (not (eq? (subprocess-status child) 'running))
                        watcher-identity
                        (watcher-confirmed-dead? watcher-identity)
                        (group-quiescent? (subprocess-pid child))) #t]
                  [(>= (current-inexact-monotonic-milliseconds) deadline) #f]
                  [else (sleep 0.005) (loop deadline)]))))
      cleanup-confirmed?))
  (define (require-group-cleanup!)
    (unless (confirm-group-cleanup!)
      (error 'run-subprocess "group cleanup unconfirmed (watcher death/group quiescence)")))
  ;; Decode failures can occur after reap; never signal an old numeric group
  ;; identity. Even an escaped break is only re-raised after confirmed cleanup.
  (define (abort-child!)
    (define confirmed?
      (cond
        [(not child) #t]
        [grouped? (confirm-group-cleanup!)]
        [else
         (when (eq? (subprocess-status child) 'running)
           (with-handlers ([exn:fail? void]) (subprocess-kill child #t)))
         (wait-subprocess-status! child 1)]))
    (custodian-shutdown-all cust)
    (remove-result!)
    confirmed?)

  (define (handle-error e)
    (define confirmed? (parameterize-break #f (abort-child!)))
    (cond
      [(and confirmed? (not (exn:fail? e))) (raise e)]
      [(not (exn:fail? e))
       (error 'run-subprocess "cleanup unconfirmed during cancellation: ~a" (exn-message e))]
      [else
       (make-error-result
        (format "Failed to execute: ~a~a" (exn-message e)
                (if confirmed? "" "; cleanup unconfirmed"))
        (inexact->exact (round (- (current-inexact-milliseconds) start-ms))))]))

  (with-handlers ([exn? handle-error])

    (define cmd-path (resolve-command command))
    (unless cmd-path
      (error 'run-subprocess "command not found: ~a" command))

    ;; Build shell command line when args are provided
    (define cmd-string
      (if (path? cmd-path)
          (path->string cmd-path)
          cmd-path))

    ;; Isolate an owner, not the command, in a new session. Without setsid
    ;; retain the existing direct-child fallback (no group cleanup available).
    (define setsid-path (and process-group? (find-executable-path "setsid")))
    (when setsid-path
      (unless (directory-exists? "/proc/self")
        (error 'run-subprocess "group cleanup confirmation requires Linux /proc"))
      (set! result-path (make-temporary-file "q-subprocess-status-~a"))
      (set! watcher-path (make-temporary-file "q-subprocess-watcher-~a")))

    (define-values (sp stdout-in stdin-out stderr-in)
      ;; Do not allow cancellation between spawn and recording its handle.
      (parameterize-break #f
        (define-values (p out in err)
          (parameterize ([current-custodian cust]
                         [current-directory dir]
                         [current-environment-variables env])
            (cond
              [setsid-path
               ;; Do not pre-create a group: setsid must exec the owner rather
               ;; than fork and leave us holding a different process's handle.
               (parameterize ([subprocess-group-enabled #f])
                 (apply subprocess #f #f #f setsid-path "--wait" "--"
                        "/bin/sh" "-c" group-owner-script "q-subprocess-owner"
                        result-path watcher-path cmd-string args))]
              ;; No args: run command directly
              [(null? args) (subprocess #f #f #f cmd-path)]
              ;; With args: pass as direct arg vector — avoids shell injection
              ;; (previously used /bin/sh -c which was vulnerable to injection)
              [else (apply subprocess #f #f #f cmd-path args)])))
        (set! child p)
        (set! grouped? (and setsid-path #t))
        (when grouped? (set! control-out in))
        (values p out in err)))

    ;; Grouped commands get /dev/null as stdin; keep the owner's private control
    ;; pipe until cleanup. Direct commands retain immediate stdin EOF. Start
    ;; drainers before waiting so a full output pipe cannot deadlock the child.
    (unless setsid-path (close-output-port stdin-out))
    (define-values (stdout-reader get-stdout) (start-bounded-reader stdout-in max-output "stdout" encoding))
    (define-values (stderr-reader get-stderr) (start-bounded-reader stderr-in max-output "stderr" encoding))

    (when grouped?
      (set! watcher-identity
            (let ready ([deadline (+ (current-inexact-monotonic-milliseconds) 2000)])
              (or (read-watcher-identity watcher-path (subprocess-pid sp))
                  (and (eq? (subprocess-status sp) 'running)
                       (< (current-inexact-monotonic-milliseconds) deadline)
                       (begin (sleep 0.005) (ready deadline))))))
      (unless watcher-identity
        (error 'run-subprocess "group watcher identity unconfirmed")))

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
       ;; EOF asks the in-group watcher to TERM then KILL its own group, even
       ;; if the supervisor is STOP'd. Direct launches retain TERM -> KILL.
       (if setsid-path
           (request-group-cleanup!)
           (with-handlers ([exn:fail? (lambda (e)
                                      (log-subprocess-warning "~a: subprocess-kill TERM: ~a"
                                                              trace-id
                                                              (exn-message e)))])
             (subprocess-kill sp #f)))
       (log-subprocess-info "~a: after SIGTERM: pid ~a status ~s"
                            trace-id
                            child-pid
                            (subprocess-status sp))
       ;; Grouped launches require watcher death AND group quiescence, not
       ;; merely an exited supervisor. Direct launches retain TERM grace
       ;; before escalating to KILL.
       (define grace-exit?
         (if setsid-path
             (begin (require-group-cleanup!) #t)
             (sync/timeout 2 sp)))
       (unless grace-exit?
         (log-subprocess-info "~a: SIGKILL after grace (pid ~a)" trace-id child-pid)
         ;; Bounded last resort for an unresponsive direct child. Group
         ;; cleanup belongs to the watcher, never to a saved numeric PGID.
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-subprocess-warning "~a: subprocess-kill KILL: ~a"
                                                              trace-id
                                                              (exn-message e)))])
           (subprocess-kill sp #t))
         (log-subprocess-info "~a: after SIGKILL: pid ~a status ~s"
                              trace-id
                              child-pid
                              (subprocess-status sp)))
       ;; Grouped cleanup was explicitly confirmed, not inferred from reap.
       ;; Never signal child-pid as a group here: it may already be reused.
       (unless (wait-subprocess-status! sp 0.5)
         (error 'run-subprocess "direct child cleanup unconfirmed"))
       ;; Give reader threads a bounded chance to observe EOF and flush their
       ;; latest snapshots.  If they do not finish, snapshot still returns the
       ;; latest captured bytes.
       (sync/timeout 1 stdout-reader)
       (sync/timeout 1 stderr-reader)
       (define-values (partial-out partial-err partial-truncated?) (reader-results))
       (define end-ms (current-inexact-milliseconds))
       (custodian-shutdown-all cust)
       (remove-result!)
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
       ;; An out-of-band supervisor death leaves the watcher and descendants
       ;; alive. Close its control pipe and confirm cleanup BEFORE returning,
       ;; even if the command redirected every stream and readers saw EOF.
       (when grouped? (require-group-cleanup!))
       ;; Process exited; readers should finish after draining stdout/stderr.
       ;; Bound the join to preserve the background-child inherited-pipe fix.
       (finish-reader! stdout-reader stdout-in "stdout")
       (finish-reader! stderr-reader stderr-in "stderr")
       (define-values (out-str err-str output-truncated?) (reader-results))
       (define exit-code
         (if setsid-path
             (group-command-status result-path (subprocess-status sp))
             (subprocess-status sp)))

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
       (remove-result!)

       (define end-ms (current-inexact-milliseconds))
       (make-success-result exit-code
                            out-str
                            err-str
                            (inexact->exact (round (- end-ms start-ms)))
                            output-truncated?)])))
