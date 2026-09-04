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
;;   - replacement requires explicit cancel-and-reap of the prior job;
;;   - campaign/wave cancellation cancels and reaps all its owned jobs;
;;   - TERM->KILL escalation covers the full process group;
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
         racket/match
         racket/string
         racket/system
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
                 group-leader?
                 out-box
                 err-box
                 log-out
                 log-lock ; semaphore 1: serializes stdout/stderr writes + terminal close
                 lock ; semaphore 1: serializes terminal transitions (wait/cancel/reconcile)
                 kill-box) ; box #f | integer status: kill+reap+close already ran
  )

;; jobs: hash identity-key -> job-handle; lock: registry semaphore making
;; check+spawn+insert (start!) atomic across threads.
(struct verification-registry (jobs lock))

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

(define (signal-group! pid signal)
  (with-handlers ([exn:fail? void])
    (system (format "/bin/bash -c 'kill -~a -- -~a 2>/dev/null || true'" signal pid))))

;; TERM -> bounded 500ms grace -> KILL over the whole process group, then
;; close inherited pipes so no descendant can hold them open. Returns the
;; final exit status (integer) of the direct child.
(define (do-escalated-kill! h)
  (define p (job-handle-proc h))
  (define pid (subprocess-pid p))
  (if (job-handle-group-leader? h)
      (begin
        (signal-group! pid "TERM")
        (let loop ([deadline (+ (current-inexact-milliseconds) 500)])
          (when (and (eq? (subprocess-status p) 'running) (< (current-inexact-milliseconds) deadline))
            (sleep 0.05)
            (loop deadline)))
        (signal-group! pid "KILL"))
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
  (let reap ([deadline (+ (current-inexact-milliseconds) 2000)])
    (when (and (eq? (subprocess-status p) 'running) (< (current-inexact-milliseconds) deadline))
      (sleep 0.01)
      (reap deadline)))
  (define status (subprocess-status p))
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

(define (make-verification-registry)
  (verification-registry (make-hash) (make-semaphore 1)))

;; Never iterate the mutable registry hash while another thread may insert a
;; freshly spawned owner. Snapshot under the registry lock, then release it
;; before any per-job operation (which may block on that job's lock).
(define (registry-handles reg)
  (call-with-semaphore (verification-registry-lock reg)
                       (lambda () (hash-values (verification-registry-jobs reg)))))

(define (registry-active-count reg)
  (for/sum ([h (in-list (registry-handles reg))] #:when (eq? (handle-state h) 'running)) 1))

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
     (define existing (hash-ref (verification-registry-jobs reg) key #f))
     (cond
       ;; 1. active owner: reuse, never launch a duplicate
       [(and existing (eq? (handle-state existing) 'running))
        (start-result (verification-job-id (unbox (job-handle-rec-box existing))) #f #t)]
       ;; 2. ownership slot free (no record, or terminal record): fresh launch
       [else
        (define digest (sha256-string (string-join (cons executable args) "\u0")))
        (define job-id (allocate-job-id!))
        ;; Real log backing BEFORE spawn so every byte of output — including
        ;; output produced before any status read — has a durable home.
        (define-values (eff-log-path log-out) (open-verification-log job-id log-path))
        ;; W1 v0.99.77 parity: subprocess-group-enabled makes the child its own
        ;; process-group leader (PID == PGID), so `kill -SIGNAL -- -PID` reaches
        ;; every descendant. Scoped via parameterize (W3 review): leaving the
        ;; parameter enabled would leak group leadership into every later
        ;; subprocess on this thread. Do NOT wrap in a bare `setsid`: setsid
        ;; forks, the wrapper exits 0 immediately, orphaning the real child
        ;; and hiding its exit code (observed as fabricated 'completed/0
        ;; results).
        (define-values (p out in err)
          (parameterize ([subprocess-group-enabled #t])
            (apply subprocess #f #f #f executable args)))
        (close-output-port in) ; child never reads our stdin
        (define out-box (box ""))
        (define err-box (box ""))
        (define log-lock (make-semaphore 1))
        (define drains
          (list (drain-port! out out-box log-out log-lock)
                (drain-port! err err-box log-out log-lock)))
        (define start-ms (current-inexact-milliseconds))
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
                      #t ; group leader: spawned under subprocess-group-enabled
                      out-box
                      err-box
                      log-out
                      log-lock ; serializes both drain writers + terminal close
                      (make-semaphore 1) ; serializes terminal transitions
                      (box #f))) ; kill/reap-once status cache
        (hash-set! (verification-registry-jobs reg) key h)
        (start-result job-id #t #f)]))))

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
                (define code (subprocess-status p))
                (for ([t (job-handle-drain-threads h)])
                  (sync/timeout 1 t))
                (set-state! h
                            (if (eq? code 0) 'completed 'failed)
                            code
                            #t
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
                            #t
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
                                        #t
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
;; never recordable, fail-closed) becomes orphan-recovered and releases
;; ownership; a live, identity-matching process keeps ownership.
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
                               (set-state! h
                                           'orphan-recovered
                                           #f
                                           #t
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
          [registry-active-count (-> verification-registry? exact-nonnegative-integer?)]
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
