#lang racket/base

;; extensions/gsd/verification-job.rkt — BUG-0053 / WP3.4
;;
;; Owned singleton verification jobs. One durable owner per verification
;; identity (campaign, wave, suite, checkout, profile); duplicate starts
;; reuse the active job instead of launching a second suite. Every job is
;; tracked with structured status (pid, process start identity, command
;; digest, log path, timestamps) and reaches an attributable terminal state:
;; completed / failed / timed-out / cancelled / orphan-recovered.
;;
;; Safety invariants honored here (wave W3):
;;   - no launch without an atomically-acquired ownership record first;
;;   - replacement requires confirmed cleanup of the prior job; terminal
;;     attribution alone (reaped? #f) never releases ownership;
;;   - campaign/wave cancellation cancels and reaps all its owned jobs;
;;   - TERM->KILL escalation runs inside the owned process group, via a
;;     private control-pipe watcher, never an external saved-PGID signal;
;;   - a stale PID record cannot claim ownership without process
;;     identity/start-time verification (reconcile -> orphan-recovered);
;;   - status reads are structured records, never pgrep heuristics;
;;   - output is file-backed: the job's log-path always names a real file
;;     receiving the full output, while the record's stdout/stderr carry
;;     only a bounded in-memory tail (multi-hour gates cannot grow
;;     unbounded strings).
;;
;; Concurrency discipline (W3 owned-lane review):
;;   - START: one coarse registry semaphore wraps the ownership check, the
;;     spawn, AND the insertion — deliberately NOT a placeholder record. A
;;     job is never observable before spawn initialization is complete, so
;;     concurrent starters of the same identity deterministically observe
;;     exactly one process and one job. Job-id allocation is serialized by
;;     a dedicated lock and can never hand one id to two jobs.
;;   - TERMINAL: each job owns a per-handle lock serializing the terminal
;;     transitions of wait / cancel / reconcile. The FIRST terminal
;;     transition wins and is frozen: a racing cancel can never overwrite a
;;     completed / timed-out / orphan-recovered attribution, and cancelling
;;     an already-terminal job returns the recorded record UNCHANGED.
;;   - KILL/REAP ONCE: TERM->KILL escalation + reap + pipe closes run at
;;     most once per job (cached in the handle); later racing callers
;;     observe the recorded status instead of re-signalling an
;;     already-reaped process or double-closing ports.
;;   - /proc parsing is unified around the LAST ") " (the comm field may
;;     itself contain spaces/parens) and FAILS CLOSED: when the process
;;     identity is unreadable, unparsable, was never recordable at spawn,
;;     or no longer matches the recorded incarnation, the pid is treated
;;     as DEAD — ownership is never claimed or kept on an unverifiable
;;     process.

(require racket/contract
         racket/file
         racket/format
         racket/list
         racket/match
         racket/string
         (only-in "../../util/json/checksum.rkt" sha256-string))

;; ------------------------------------------------------------ identity

(struct verification-identity (campaign wave suite checkout profile) #:transparent)

(define (verification-identity-key ident)
  (match-define (verification-identity camp wave suite checkout profile) ident)
  (sha256-string (string-join (list camp wave suite checkout profile) "\u0")))

;; ------------------------------------------------------------ records

;; start-result: what verification-start! returns.
(struct start-result (job-id started? existing-job?) #:transparent)

;; verification-job: durable ownership + terminal result record.
(struct verification-job
        (id identity-key
            command-digest
            log-path
            pid
            proc-start-ticks ; OS-side start identity (PID-reuse guard)
            start-ms ; our clock at spawn
            state ; 'running | 'completed | 'failed | 'timed-out | 'cancelled | 'orphan-recovered
            exit-code
            reaped?
            stdout
            stderr
            end-ms)
  #:transparent)

;; Internal mutable handle paired with each record while running.
(struct job-handle
        (rec-box campaign
                 proc
                 out
                 in
                 err
                 drain-threads
                 timeout-ms
                 result-path ; bounded command status, separate from the group owner's exit
                 watcher-path ; private startup identity published before the command starts
                 watcher-identity ; (pid . start-ticks), or #f: cleanup cannot be confirmed
                 group-leader?
                 out-box
                 err-box
                 log-out
                 log-lock ; semaphore 1: serializes stdout/stderr writes + terminal close
                 lock ; semaphore 1: serializes terminal transitions (wait/cancel/reconcile)
                 kill-box) ; #f | exit status (confirmed) | 'cleanup-unconfirmed
  )

;; jobs: hash identity-key -> job-handle; lock: registry semaphore making
;; check+spawn+insert (start!) atomic across threads.
;;
;; R102 identity coordination (w0/security-contracts.md §3.2):
;;   - `incarnation` is an opaque token minted at make-verification-registry;
;;     it distinguishes A->(recreate)->A in emitted identity fields. It never
;;     persists: registry recreation losing the index is by design.
;;   - `tombstones` is a bounded, newest-first list of captured immutable
;;     terminal records for REPLACED handles (see
;;     verification-terminal-tombstone). It is a reconciliation window, not a
;;     general event store: bounded capacity, explicit FIFO eviction, lost on
;;     registry recreation.
(struct verification-registry
        (jobs
         lock
         [closed? #:mutable]
         incarnation
         [tombstones #:mutable]))

;; Captured immutable terminal record retained for ONE replaced job, so late
;; or duplicate events for the OLD job stay attributable (dedupe =>
;; exactly-once UI transition) instead of failing as "unknown job". Bounded
;; per incarnation (registry-tombstone-capacity, explicit FIFO eviction);
;; registry recreation loses the index by design — identity fields carry the
;; truth, never this window.
(struct verification-terminal-tombstone
        (job-id
         identity-key
         state ; terminal state of the REPLACED job
         exit-code
         reaped?
         end-ms
         log-path
         replacement-id) ; job-id of the handle that took the slot
  #:transparent)

;; Fixed reconciliation-window capacity per registry. Eviction is explicit:
;; inserting beyond the capacity drops the OLDEST captured record.
(define registry-tombstone-capacity 32)

;; ------------------------------------------------------------ helpers

(define (job->public-record h state exit-code reaped? out err)
  (define rec (unbox (job-handle-rec-box h)))
  (verification-job (verification-job-id rec)
                    (verification-job-identity-key rec)
                    (verification-job-command-digest rec)
                    (verification-job-log-path rec)
                    (verification-job-pid rec)
                    (verification-job-proc-start-ticks rec)
                    (verification-job-start-ms rec)
                    state
                    exit-code
                    reaped?
                    out
                    err
                    (current-inexact-milliseconds)))

(define (handle-state h)
  (verification-job-state (unbox (job-handle-rec-box h))))

(define (set-state! h state exit-code reaped? out err)
  ;; FIRST TERMINAL WINS (W3 review): the transition applies only while the
  ;; record is still 'running; once terminal, the attribution (state, exit
  ;; code, reaped?, output, end-ms) is frozen. Racing waiters/cancellers
  ;; must observe the recorded terminal record, never overwrite it.
  ;; Callers hold the job lock, so check-then-set is atomic. Terminal
  ;; transition flushes and releases the shared log port; a second close
  ;; raises and is ignored.
  (when (eq? (verification-job-state (unbox (job-handle-rec-box h))) 'running)
    (call-with-semaphore (job-handle-log-lock h)
                         (lambda ()
                           (with-handlers ([exn:fail? void])
                             (flush-output (job-handle-log-out h))
                             (close-output-port (job-handle-log-out h)))))
    (with-handlers ([exn:fail? void]) (delete-file (job-handle-result-path h)))
    (with-handlers ([exn:fail? void]) (delete-file (job-handle-watcher-path h)))
    (set-box! (job-handle-rec-box h) (job->public-record h state exit-code reaped? out err))))

;; Bounded in-memory tail per stream (chars). The complete durable record
;; of a job's output lives in its log file; the stdout/stderr fields on the
;; job record keep only the most recent tail so a multi-hour gate can never
;; grow unbounded in-memory strings in the registry.
(define verify-output-tail-chars 65536)

(define (tail-append old s)
  (define joined (string-append old s))
  (define n (string-length joined))
  (if (> n verify-output-tail-chars)
      (substring joined (- n verify-output-tail-chars))
      joined))

;; One drain thread per output stream: each chunk is appended to the job's
;; log file FIRST (the log is the durable record), then to the bounded
;; in-memory tail box.
(define (drain-port! port tail-box log-out log-lock)
  (thread (lambda ()
            (let loop ()
              (define s
                (with-handlers ([exn:fail? (lambda (_) eof)])
                  (read-string 4096 port)))
              (cond
                [(eof-object? s) (void)]
                [else
                 (call-with-semaphore log-lock
                                      (lambda ()
                                        (with-handlers ([exn:fail? void])
                                          (display s log-out)
                                          (flush-output log-out))))
                 (set-box! tail-box (tail-append (unbox tail-box) s))
                 (loop)])))))

;; Real log backing for one job. The caller-declared path is opened for
;; append and recorded verbatim; without one (or when the given path cannot
;; be opened) a per-job path under the system temp directory is synthesized.
;; Either way the returned path names a real, existing file the job's output
;; drains into — log-path on the job record is never a dangling string.
(define (open-verification-log job-id given-path)
  (define (open-synth!)
    (define p
      (build-path (find-system-path 'temp-dir)
                  (format "gsd-verification-~a-~a.log" job-id (current-inexact-milliseconds))))
    (values (path->string p) (open-output-file p #:exists 'append)))
  (with-handlers ([exn:fail? (lambda (_) (open-synth!))])
    (if (not given-path)
        (open-synth!)
        (let ([p (simplify-path (path->complete-path given-path))])
          (values (path->string p) (open-output-file p #:exists 'append))))))

;; ------------------------------------------------------------ /proc identity

;; Unified /proc/<pid>/stat parsing (W3 review): the comm field (field 2,
;; wrapped in parentheses) may itself contain spaces and parentheses, so
;; the remaining fields resume after the LAST ") " in the line — parsing
;; after the FIRST ')' mis-splits any comm containing a paren. Returns two
;; values: the state field and the starttime field (clock ticks since
;; boot), either of which is #f when it cannot be determined.
(define (parse-proc-stat raw)
  (define rparen
    (let scan ([i (sub1 (string-length raw))])
      (cond
        [(< i 1) #f]
        [(and (char=? (string-ref raw i) #\))
              (< (add1 i) (string-length raw))
              (char=? (string-ref raw (add1 i)) #\space))
         i]
        [else (scan (sub1 i))])))
  (if (not rparen)
      (values #f #f)
      (let* ([rest (string-trim (substring raw (+ rparen 2)))]
             [flds (regexp-split #rx" +" rest)])
        ;; flds[0] = state (original field 3); flds[19] = starttime
        ;; (original field 22): 22 - 3 = 19.
        (values (and (pair? flds) (car flds))
                (and (>= (length flds) 20) (string->number (list-ref flds 19)))))))

;; One raw /proc/<pid>/stat line, or #f when the entry is unreadable
;; (missing pid, permission race, EOF, non-/proc platform).
(define (read-proc-stat-line pid)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define line
      (call-with-input-file (format "/proc/~a/stat" pid) (lambda (in) (read-line in 'any))))
    (and (string? line) line)))

;; OS-side process start identity from /proc (Linux); #f when unavailable.
;; A #f here makes every later liveness check FAIL CLOSED (see pid-alive?).
(define (proc-start-ticks pid)
  (define raw (read-proc-stat-line pid))
  (define-values (_state ticks)
    (if raw
        (parse-proc-stat raw)
        (values #f #f)))
  ticks)

(define (pid-alive? pid recorded-ticks)
  ;; A SIGKILLed-but-unreaped child remains in the process table as a
  ;; zombie (state 'Z' in /proc/<pid>/stat): ticks still match, so we must
  ;; treat zombies as dead or a lost process is never reconciled.
  ;; PID-reuse guard (WP3.4): identity is (pid, start-ticks). ONE atomic
  ;; /proc/<pid>/stat snapshot feeds BOTH the zombie check and the identity
  ;; check — reading state and ticks in two separate passes raced
  ;; kill/exec/reap and produced mixed verdicts, so both derive from a
  ;; single raw line through the shared last-") " parser above.
  ;; Verdict policy (W3 review) — FAIL CLOSED: unreadable entry, unparsable
  ;; line, identity that could not be recorded at spawn, or identity
  ;; mismatch (PID reused by a new incarnation) all mean DEAD. Ownership is
  ;; never claimed or kept on an unverifiable process; a stale record
  ;; reconciles to orphan-recovered instead of trusting an unrelated pid.
  (define raw (read-proc-stat-line pid))
  (define-values (state-field now-ticks)
    (if raw
        (parse-proc-stat raw)
        (values #f #f)))
  (define verdict
    (cond
      [(not now-ticks) #f] ; entry unreadable / malformed -> stale
      [(not recorded-ticks) #f] ; identity unavailable at spawn -> unverifiable
      [(not (= recorded-ticks now-ticks)) #f] ; PID reused -> mismatch
      [(equal? state-field "Z") #f] ; zombie: reaped-pending -> dead
      [else #t]))
  (when (getenv "VJ_DEBUG")
    (eprintf "VJ pid=~a rec=~a now=~a state=~s -> ~a\n"
             pid
             recorded-ticks
             now-ticks
             state-field
             verdict))
  verdict)

;; ---------------------------------------------------- group leadership

;; Mirror sandbox/subprocess.rkt (W1 v0.99.77): the child gets its own
;; process group so TERM/KILL can reach the whole group. Without group
;; leadership, fall back to direct-child signals only (a `-PGID` kill
;; would hit our own group).
(define setsid-path (find-executable-path "setsid"))

;; The command is NOT the session/group owner. A tiny supervisor retains that
;; identity until it kills its own group, including redirected background
;; descendants, on both command completion and TERM. Signalling group 0 from
;; INSIDE the owned group cannot target a reused PGID. Never defer successful
;; cleanup until registry close: terminal handles can be replaced in the hash.
;; The supervisor dies with its group, so preserve the command's real status
;; separately (never in stdout, which is untrusted gate output).
(define owner-script
  (string-append "result=$1; watcher=$2; shift 2\n"
                 "cleanup() { trap '' TERM HUP INT; kill -TERM 0; sleep 0.25; kill -KILL 0; }\n"
                 "trap cleanup TERM HUP INT\n"
                 ;; A second in-group owner watches a private control pipe.
                 ;; EOF cleans even a STOP'd supervisor, without signalling a
                 ;; saved numeric PGID from outside the group. The command
                 ;; cannot consume or retain that pipe.
                 "exec 3<&0\n"
                 "( IFS= read -r ignored <&3; cleanup ) <&3 &\n"
                 ;; Publish the actual subshell incarnation, not $$ (which is
                 ;; the supervisor's PID in POSIX subshells). No command may
                 ;; start before this private ownership handshake succeeds.
                 "IFS= read -r watcher_stat < /proc/$!/stat || exit 125\n"
                 "printf '%s\\n' \"$watcher_stat\" > \"$watcher\" || exit 125\n"
                 "\"$@\" </dev/null 3<&- &\n"
                 "wait $! 2>/dev/null\n"
                 "code=$?\n"
                 "printf '%s\\n' \"$code\" > \"$result\"\n"
                 "kill -KILL 0\n"))

(define (command-exit-code h fallback)
  (with-handlers ([exn:fail? (lambda (_) fallback)])
    (define raw
      (call-with-input-file (job-handle-result-path h) (lambda (in) (read-bytes 5 in))))
    (if (and (bytes? raw) (regexp-match? #rx#"^[0-9][0-9]?[0-9]?\n$" raw))
        (let ([code (string->number (string-trim (bytes->string/utf-8 raw)))])
          (if (<= 0 code 255) code fallback))
        fallback)))

;; Unlike pid-alive? (a reconciliation hint), confirmation must fail CLOSED
;; on unreadable /proc entries. Only ENOENT/ESRCH, a different incarnation,
;; or an explicitly dead state proves that the recorded watcher is gone.
(define (confirmed-dead? pid ticks)
  (with-handlers ([exn:fail:filesystem:errno?
                   (lambda (e) (and (memv (car (exn:fail:filesystem:errno-errno e)) '(2 3)) #t))]
                  [exn:fail? (lambda (_) #f)])
    (define raw (call-with-input-file (format "/proc/~a/stat" pid) read-line))
    (define-values (state now-ticks) (parse-proc-stat raw))
    (and now-ticks
         (or (not (= ticks now-ticks)) (and (member state '("Z" "X")) #t)))))

;; The watcher dies on return from its in-group KILL syscall. Also require
;; the owned session/group to be quiescent: SIGKILL delivery to a descendant
;; may lag the watcher's death. This is observation only, NEVER numeric PGID
;; signalling. An unreadable snapshot refuses release rather than guessing.
(define (group-quiescent? owner-pid)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (for/and ([entry (in-list (directory-list "/proc"))]
              #:when (regexp-match? #rx"^[0-9]+$" (path->string entry)))
      (with-handlers ([exn:fail:filesystem:errno?
                       (lambda (e) (and (memv (car (exn:fail:filesystem:errno-errno e)) '(2 3)) #t))])
        (define raw (call-with-input-file (build-path "/proc" entry "stat") read-line))
        ;; Greedy comm matching ends at the LAST closing paren, just as in
        ;; parse-proc-stat. Fields here are state, ppid, pgrp, session.
        (define fields (regexp-match #rx"^[0-9]+ \\(.*\\) ([^ ]+) [^ ]+ ([0-9]+) ([0-9]+) " raw))
        (and fields
             (or (and (not (= owner-pid (string->number (list-ref fields 2))))
                      (not (= owner-pid (string->number (list-ref fields 3)))))
                 (and (member (list-ref fields 1) '("Z" "X")) #t)))))))

(define (read-watcher-identity path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define raw (file->string path))
    (define pid-match (regexp-match #rx"^([0-9]+) " raw))
    (define-values (_state ticks) (parse-proc-stat raw))
    (and pid-match ticks (cons (string->number (cadr pid-match)) ticks))))

(define (cleanup-confirmed? h)
  (exact-integer? (unbox (job-handle-kill-box h))))

;; Request TERM -> KILL from INSIDE the group by closing its private control
;; pipe. No status/identity check can make an external numeric PGID signal
;; safe: a concurrent waiter can reap the owner between check and signal.
;; Confirm watcher death AND group quiescence before releasing ownership;
;; reaping the supervisor alone is insufficient when it died out of band.
;; Then close inherited pipes. Returns the direct child's final exit status
;; only on confirmation, otherwise 'cleanup-unconfirmed at the hard bound.
(define (do-escalated-kill! h)
  (define p (job-handle-proc h))
  (if (job-handle-group-leader? h)
      (with-handlers ([exn:fail? void])
        (close-output-port (job-handle-in h)))
      (begin
        (subprocess-kill p #f)
        (let loop ([deadline (+ (current-inexact-milliseconds) 500)])
          (when (and (eq? (subprocess-status p) 'running) (< (current-inexact-milliseconds) deadline))
            (sleep 0.05)
            (loop deadline)))
        (subprocess-kill p #t)))
  ;; Bounded reap (W1 v0.99.77 lesson, applied to the kill path too):
  ;; Racket's process-completion signaling is unreliable for
  ;; process-group-leader children, so NEVER block indefinitely on the
  ;; process object — poll subprocess-status (the status read performs the
  ;; OS reap) under a hard 2s bound. This runs while the caller holds the
  ;; job lock; an unbounded wait here would wedge every racing
  ;; waiter/canceller on that lock (observed as a gate-wide deadlock).
  (define watcher (job-handle-watcher-identity h))
  (define (finished?)
    (and (not (eq? (subprocess-status p) 'running))
         (or (not (job-handle-group-leader? h))
             (and watcher
                  (confirmed-dead? (car watcher) (cdr watcher))
                  (group-quiescent? (subprocess-pid p))))))
  (define confirmed?
    (let reap ([deadline (+ (current-inexact-monotonic-milliseconds) 2000)])
      (cond [(finished?) #t]
            [(>= (current-inexact-monotonic-milliseconds) deadline) #f]
            [else (sleep 0.01) (reap deadline)])))
  (define status (if confirmed? (subprocess-status p) 'cleanup-unconfirmed))
  ;; close inherited descriptors
  (with-handlers ([exn:fail? void])
    (close-input-port (job-handle-out h)))
  (with-handlers ([exn:fail? void])
    (close-output-port (job-handle-in h)))
  (with-handlers ([exn:fail? void])
    (close-input-port (job-handle-err h)))
  status)

;; KILL/REAP EXACTLY ONCE (W3 review): every caller holds the job lock and
;; the performed status is cached in the handle, so the escalation —
;; signals, reap, pipe closes — runs at most once per job. A second racing
;; waiter/canceller reads the cached status instead of signalling an
;; already-reaped process or double-closing pipes.
(define (escalated-kill! h)
  (or (unbox (job-handle-kill-box h))
      (let ([status (do-escalated-kill! h)])
        (set-box! (job-handle-kill-box h) status)
        status)))

;; ------------------------------------------------------------ registry

;; The incarnation token is minted once per registry lifetime and re-minted
;; on every recreation (make-verification-registry); emitted identity fields
;; derive from it so A->(recreate)->A is distinguishable from one A.
(define (make-verification-registry)
  (verification-registry (make-hash) (make-semaphore 1) #f (gensym 'vjreg) '()))

;; Record the terminal record of a REPLACED handle into the bounded
;; reconciliation window (caller holds the registry lock). Explicit FIFO
;; eviction at the fixed capacity; never a general event store.
(define (record-terminal-tombstone! reg prior-rec replacement-id)
  (unless (eq? (verification-job-state prior-rec) 'running)
    (set-verification-registry-tombstones!
     reg
     (let ([next
            (cons (verification-terminal-tombstone
                   (verification-job-id prior-rec)
                   (verification-job-identity-key prior-rec)
                   (verification-job-state prior-rec)
                   (verification-job-exit-code prior-rec)
                   (verification-job-reaped? prior-rec)
                   (verification-job-end-ms prior-rec)
                   (verification-job-log-path prior-rec)
                   replacement-id)
                  (verification-registry-tombstones reg))])
       (if (> (length next) registry-tombstone-capacity)
           (take next registry-tombstone-capacity)
           next)))))

;; Attribution lookup for identity coordination (R102): the CURRENT record
;; for `job-id`, or the retained tombstone when the handle was replaced, or
;; #f when this incarnation does not know the job. Never raises — a late or
;; duplicate event for a replaced job must be attributable, not fatal.
(define (verification-job-facts reg job-id)
  (call-with-semaphore
   (verification-registry-lock reg)
   (lambda ()
     (or (for/first ([h (in-hash-values (verification-registry-jobs reg))]
                     #:when (eq? (verification-job-id (unbox (job-handle-rec-box h))) job-id))
           (unbox (job-handle-rec-box h)))
         (for/first ([t (in-list (verification-registry-tombstones reg))]
                     #:when (eq? (verification-terminal-tombstone-job-id t) job-id))
           t)))))

;; Never iterate the mutable registry hash while another thread may insert a
;; freshly spawned owner. Snapshot under the registry lock, then release it
;; before any per-job operation (which may block on that job's lock).
(define (registry-handles reg)
  (call-with-semaphore (verification-registry-lock reg)
                       (lambda () (hash-values (verification-registry-jobs reg)))))

(define (registry-active-count reg)
  ;; Failed confirmation retains ownership even with a terminal attribution.
  (for/sum ([h (in-list (registry-handles reg))]
            #:unless (verification-job-reaped? (unbox (job-handle-rec-box h)))) 1))

;; A dedicated owner closes admission under the SAME lock as start/spawn.
;; A concurrent launch is either fully registered and reaped below, or refused;
;; it cannot escape between cancellation's snapshot and worker termination.
;; Never close a shared ambient registry to cancel an individual attempt.
(define (verification-close-registry! reg)
  (define handles
    (call-with-semaphore (verification-registry-lock reg)
                         (lambda ()
                           (set-verification-registry-closed?! reg #t)
                           (hash-values (verification-registry-jobs reg)))))
  (for/list ([h (in-list handles)])
    (verification-cancel! reg (verification-job-id (unbox (job-handle-rec-box h))))))

(define job-counter 0)

(define job-id-lock (make-semaphore 1)) ; job-id allocation is atomic (W3 review)

(define (allocate-job-id!)
  (call-with-semaphore job-id-lock
                       (lambda ()
                         (set! job-counter (add1 job-counter))
                         (string->symbol (format "vj-~a" job-counter)))))

;; Start (or reuse) the job for `ident`. Ownership is acquired BEFORE any
;; process is launched, and the whole check + spawn + insert sequence runs
;; under ONE coarse registry semaphore (W3 review). Deliberately NOT a
;; placeholder record: a handle is inserted only after spawn initialization
;; is complete, so no caller can ever observe a half-initialized job. The
;; registry lock makes the sequence atomic: concurrent starters of the same
;; identity deterministically produce exactly one process and one registry
;; entry (every other starter reuses the active job), the spawn is short so
;; a coarse lock is the right price for correctness, and the job-id
;; allocation inside the section can never collide.
(define (verification-start! reg
                             ident
                             executable
                             args
                             #:timeout-ms [timeout-ms 3600000]
                             #:log-path [log-path #f])
  (define key (verification-identity-key ident))
  (call-with-semaphore
   (verification-registry-lock reg)
   (lambda ()
     ;; Once spawn can create a process, defer asynchronous breaks until its
     ;; handle is registered. In particular, the bounded watcher handshake
     ;; sleeps below must not abandon an unregistered owner. Lock acquisition
     ;; stays breakable; registry close can find the job when breaks resume.
     (parameterize-break
      #f
     (when (verification-registry-closed? reg)
       (error 'verification-start! "owned verification registry is closed"))
     (define existing (hash-ref (verification-registry-jobs reg) key #f))
     (cond
       ;; 1. running OR unconfirmed terminal owner: never launch a duplicate
       [(and existing
             (not (verification-job-reaped? (unbox (job-handle-rec-box existing)))))
        (start-result (verification-job-id (unbox (job-handle-rec-box existing))) #f #t)]
       ;; 2. ownership slot free (no record, or confirmed reap): fresh launch.
       ;;    R102: a REPLACED terminal handle's captured record enters the
       ;;    bounded tombstone window before the slot is reused, so late or
       ;;    duplicate events for the old job stay attributable.
       [else
        (define digest (sha256-string (string-join (cons executable args) "\u0")))
        (define job-id (allocate-job-id!))
        (when existing
          (record-terminal-tombstone! reg (unbox (job-handle-rec-box existing)) job-id))
        ;; Real log backing BEFORE spawn so every byte of output — including
        ;; output produced before any status read — has a durable home.
        (define-values (eff-log-path log-out) (open-verification-log job-id log-path))
        (define result-path (make-temporary-file "gsd-verification-status-~a"))
        (define watcher-path (make-temporary-file "gsd-verification-watcher-~a"))
        ;; Verification runs in a new session, not merely a new process group.
        ;; A background process group in the TUI's session still owns
        ;; /dev/tty; terminal tests that call tcsetattr/stty then receive
        ;; SIGTTOU and stop the entire verification tree. Spawn without
        ;; subprocess-group-enabled so the setsid child is not already a
        ;; process-group leader; util-linux setsid can then exec directly.
        ;; The resulting owner identity is PID == PGID == SID. Its command
        ;; status is recorded separately before self-cleanup; terminal records
        ;; still attribute the declared command, never the supervisor's KILL.
        (define-values (p out in err)
          (with-handlers ([exn? (lambda (e)
                                  (delete-file result-path)
                                  (delete-file watcher-path)
                                  (close-output-port log-out)
                                  (raise e))])
            (if setsid-path
                (parameterize ([subprocess-group-enabled #f])
                  (apply subprocess #f #f #f setsid-path "--wait" "--"
                         "/bin/sh" "-c" owner-script "gsd-verification-owner"
                         result-path watcher-path executable args))
                (error 'verification-start!
                       "terminal-isolated verification requires the setsid executable"))))
        ;; Keep stdin open as the watcher's private cancellation pipe. The
        ;; actual command gets /dev/null, not this control descriptor.
        (define out-box (box ""))
        (define err-box (box ""))
        (define log-lock (make-semaphore 1))
        (define drains
          (list (drain-port! out out-box log-out log-lock)
                (drain-port! err err-box log-out log-lock)))
        (define start-ms (current-inexact-milliseconds))
        ;; Own the watcher's incarnation before exposing the job (including
        ;; to simulate-lost-process!). A missing handshake cannot claim reap.
        (define watcher-identity
          (let ready ([deadline (+ (current-inexact-monotonic-milliseconds) 2000)])
            (or (read-watcher-identity watcher-path)
                (cond
                  ;; The owner may publish and exit after our empty read.
                  ;; Still require a validated identity, never exit alone.
                  [(not (eq? (subprocess-status p) 'running))
                   (read-watcher-identity watcher-path)]
                  [(< (current-inexact-monotonic-milliseconds) deadline)
                   (sleep 0.005)
                   (ready deadline)]
                  [else #f]))))
        (define h
          (job-handle (box (verification-job job-id
                                             key
                                             digest
                                             eff-log-path
                                             (subprocess-pid p)
                                             (proc-start-ticks (subprocess-pid p))
                                             start-ms
                                             'running
                                             #f
                                             #f
                                             ""
                                             ""
                                             #f))
                      (verification-identity-campaign ident)
                      p
                      out
                      in
                      err
                      drains
                      timeout-ms
                      result-path
                      watcher-path
                      watcher-identity
                      #t ; session and process-group leader created by setsid
                      out-box
                      err-box
                      log-out
                      log-lock ; serializes both drain writers + terminal close
                      (make-semaphore 1) ; serializes terminal transitions
                      (box #f))) ; kill/reap-once status cache
        (hash-set! (verification-registry-jobs reg) key h)
        (start-result job-id #t #f)])))))

(define (require-handle reg job-id)
  (or (for/first ([h (in-list (registry-handles reg))]
                  #:when (eq? (verification-job-id (unbox (job-handle-rec-box h))) job-id))
        h)
      (error 'verification-job "unknown job id: ~a" job-id)))

;; Bounded structured status read.
(define (verification-status reg job-id)
  (unbox (job-handle-rec-box (require-handle reg job-id))))

;; Wait up to timeout-ms for terminal state; reaps on completion/timeout.
;; W1 v0.99.77 lesson: Racket's subprocess event does not fire reliably for
;; process-group-leader children (even after exit), so poll subprocess-status
;; instead of syncing on the process object. The job's own timeout (start!)
;; is a hard upper bound on the effective deadline: a 700ms job must report
;; timed-out even if the caller passes a 10s wait window.
;; W3 review: every terminal transition happens under the job lock and only
;; while the record is still 'running, so racing waiters/cancellers can
;; neither double-kill nor overwrite the first terminal attribution — each
;; observes and returns the ONE recorded terminal record.
(define (verification-wait reg job-id timeout-ms)
  (define h (require-handle reg job-id))
  (define p (job-handle-proc h))
  (define rec0 (unbox (job-handle-rec-box h)))
  (cond
    ;; already terminal: idempotent read
    [(not (eq? (verification-job-state rec0) 'running)) rec0]
    [else
     (define caller-deadline (+ (current-inexact-milliseconds) timeout-ms))
     (define own-deadline (+ (verification-job-start-ms rec0) (job-handle-timeout-ms h)))
     (define deadline (min caller-deadline own-deadline))
     (let loop ()
       (cond
         ;; terminalized by a concurrent waiter/cancel/reconcile: return
         ;; the recorded attribution unchanged
         [(not (eq? (handle-state h) 'running)) (unbox (job-handle-rec-box h))]
         [(not (eq? (subprocess-status p) 'running))
          ;; process exited: reap with truthful exit code (serialized)
          (call-with-semaphore
           (job-handle-lock h)
           (lambda ()
             (cond
               ;; a racing waiter/cancel recorded the terminal state first
               [(not (eq? (handle-state h) 'running)) (unbox (job-handle-rec-box h))]
               [else
                (define code (command-exit-code h (subprocess-status p)))
                ;; Drain buffered command output before cleanup closes the
                ;; read ports. Both joins are bounded even if a lost owner's
                ;; watcher still holds the descriptors open.
                (for ([t (job-handle-drain-threads h)])
                  (sync/timeout 1 t))
                ;; Even on exit, the supervisor may have been lost rather
                ;; than having performed its normal in-group cleanup.
                (escalated-kill! h)
                (set-state! h
                            (if (eq? code 0) 'completed 'failed)
                            code
                            (cleanup-confirmed? h)
                            (unbox (job-handle-out-box h))
                            (unbox (job-handle-err-box h)))
                (unbox (job-handle-rec-box h))])))]
         [(>= (current-inexact-milliseconds) deadline)
          ;; deadline hit: TERM->KILL, exit 124 is authoritative
          (call-with-semaphore
           (job-handle-lock h)
           (lambda ()
             (cond
               ;; a racing waiter/cancel recorded the terminal state first
               [(not (eq? (handle-state h) 'running)) (unbox (job-handle-rec-box h))]
               [else
                ;; Escalation is best-effort (see verification-cancel!):
                ;; the timed-out attribution always applies.
                (with-handlers ([exn:fail? void])
                  (escalated-kill! h) ; exactly-once: cached in the handle
                  (for ([t (job-handle-drain-threads h)])
                    (sync/timeout 1 t)))
                (set-state! h
                            'timed-out
                            124
                            (cleanup-confirmed? h)
                            (unbox (job-handle-out-box h))
                            (unbox (job-handle-err-box h)))
                (unbox (job-handle-rec-box h))])))]
         [else
          (sleep 0.005)
          (loop)]))]))

;; Explicit cancel + reap (replacement prerequisite / campaign teardown).
;; Serialized on the job lock; kill/reap runs at most once. CANCEL ON
;; TERMINAL RETURNS UNCHANGED (W3 review): a job already carrying a
;; terminal attribution (completed / failed / timed-out / ...) is returned
;; exactly as recorded — cancel never rewrites history.
(define (verification-cancel! reg job-id)
  (define h (require-handle reg job-id))
  (call-with-semaphore (job-handle-lock h)
                       (lambda ()
                         (define rec (unbox (job-handle-rec-box h)))
                         (cond
                           [(not (eq? (verification-job-state rec) 'running)) rec]
                           [else
                            ;; ESCALATION IS BEST-EFFORT (W3 repair): a failure
                            ;; inside kill/drain must NEVER leave the job stuck
                            ;; 'running (waiters would poll to their deadline
                            ;; and block the job lock) — the terminal
                            ;; attribution below always applies.
                            (with-handlers ([exn:fail? void])
                              (escalated-kill! h) ; exactly-once: cached in the handle
                              (for ([t (job-handle-drain-threads h)])
                                (sync/timeout 1 t)))
                            (set-state! h
                                        'cancelled
                                        (verification-job-exit-code rec)
                                        (cleanup-confirmed? h)
                                        (unbox (job-handle-out-box h))
                                        (unbox (job-handle-err-box h)))
                            (unbox (job-handle-rec-box h))]))))

;; Campaign/wave cancellation: cancel+reap every job owned by `campaign`.
(define (verification-cancel-campaign! reg campaign)
  (define reaped '())
  (for ([h (in-list (registry-handles reg))]
        #:when (and (string=? (job-handle-campaign h) campaign) (eq? (handle-state h) 'running)))
    (set! reaped
          (cons (verification-cancel! reg (verification-job-id (unbox (job-handle-rec-box h))))
                reaped)))
  (reverse reaped))

;; Reconcile a record against OS reality (resume/restart): a record whose
;; process is gone (or whose start identity no longer matches — or was
;; never recordable, fail-closed) becomes orphan-recovered. Ownership is
;; released ONLY with confirmed watcher/group cleanup (reaped? #t); a failed
;; confirmation retains the slot, as does a live identity-matching process.
(define (verification-reconcile! reg job-id)
  (define h (require-handle reg job-id))
  (define rec (unbox (job-handle-rec-box h)))
  (cond
    [(not (eq? (verification-job-state rec) 'running)) rec]
    [(pid-alive? (verification-job-pid rec) (verification-job-proc-start-ticks rec)) rec]
    [else
     (call-with-semaphore (job-handle-lock h)
                          (lambda ()
                            (define cur (unbox (job-handle-rec-box h)))
                            (cond
                              ;; a racing waiter/cancel recorded the terminal state first
                              [(not (eq? (verification-job-state cur) 'running)) cur]
                              [else
                               ;; A lost supervisor may still have descendants.
                               ;; Closing the control pipe lets its in-group
                               ;; watcher clean them before ownership is released.
                               (escalated-kill! h)
                               (for ([t (job-handle-drain-threads h)])
                                 (sync/timeout 1 t))
                               (set-state! h
                                           'orphan-recovered
                                           #f
                                           (cleanup-confirmed? h)
                                           (verification-job-stdout cur)
                                           (verification-job-stderr cur))
                               (unbox (job-handle-rec-box h))])))]))

;; Test/ops hook: kill the real child OUT OF BAND (registry loses track),
;; simulating a lost turn / coordinator crash between launch and reap.
(define (simulate-lost-process! reg job-id)
  (define h (require-handle reg job-id))
  (define p (job-handle-proc h))
  (with-handlers ([exn:fail? void])
    (subprocess-kill p #t))
  ;; close our ends so the record no longer holds a live handle
  (with-handlers ([exn:fail? void])
    (close-input-port (job-handle-out h)))
  (with-handlers ([exn:fail? void])
    (close-output-port (job-handle-in h)))
  (with-handlers ([exn:fail? void])
    (close-input-port (job-handle-err h))))

;; ------------------------------------------------------------ contracts

(provide (contract-out
          (struct verification-identity
                  ((campaign string?) (wave string?)
                                      (suite string?)
                                      (checkout string?)
                                      (profile string?)))
          [verification-identity-key (-> verification-identity? string?)]
          (struct start-result ((job-id symbol?) (started? boolean?) (existing-job? boolean?)))
          (struct verification-job
                  ((id symbol?) (identity-key string?)
                                (command-digest string?)
                                (log-path (or/c string? #f))
                                (pid real?)
                                (proc-start-ticks (or/c real? #f))
                                (start-ms real?)
                                (state symbol?)
                                (exit-code (or/c exact-integer? #f))
                                (reaped? boolean?)
                                (stdout string?)
                                (stderr string?)
                                (end-ms (or/c real? #f))))
          [make-verification-registry (-> verification-registry?)]
          [verification-registry? (-> any/c boolean?)]
          [verification-registry-incarnation (-> verification-registry? symbol?)]
          [registry-active-count (-> verification-registry? exact-nonnegative-integer?)]
          [verification-close-registry! (-> verification-registry? (listof verification-job?))]
          ;; R102 identity coordination: bounded tombstone window + facts lookup
          [registry-tombstone-capacity exact-positive-integer?]
          (struct verification-terminal-tombstone
                  ((job-id symbol?)
                   (identity-key string?)
                   (state symbol?)
                   (exit-code (or/c exact-integer? #f))
                   (reaped? boolean?)
                   (end-ms (or/c real? #f))
                   (log-path (or/c string? #f))
                   (replacement-id symbol?)))
          [verification-job-facts
           (-> verification-registry? symbol?
               (or/c verification-job? verification-terminal-tombstone? #f))]
          [verification-start!
           (->*
            (verification-registry? verification-identity? path-string? (listof (or/c string? path?)))
            (#:timeout-ms real? #:log-path (or/c path-string? #f))
            start-result?)]
          [verification-status (-> verification-registry? symbol? verification-job?)]
          [verification-wait (-> verification-registry? symbol? real? verification-job?)]
          [verification-cancel! (-> verification-registry? symbol? verification-job?)]
          [verification-cancel-campaign!
           (-> verification-registry? string? (listof verification-job?))]
          [verification-reconcile! (-> verification-registry? symbol? verification-job?)]
          [simulate-lost-process! (-> verification-registry? symbol? void?)]
          [verify-output-tail-chars exact-positive-integer?]))
