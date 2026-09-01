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
;;   - status reads are structured records, never pgrep heuristics.

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
  (id
   identity-key
   command-digest
   log-path
   pid
   proc-start-ticks          ; OS-side start identity (PID-reuse guard)
   start-ms                  ; our clock at spawn
   state                     ; 'running | 'completed | 'failed | 'timed-out | 'cancelled | 'orphan-recovered
   exit-code
   reaped?
   stdout
   stderr
   end-ms)
  #:transparent)

;; Internal mutable handle paired with each record while running.
(struct job-handle (rec-box campaign proc out in err drain-threads timeout-ms
                             group-leader? out-box err-box))

(struct verification-registry (jobs) #:mutable) ; hash: identity-key -> job-handle

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
  (set-box! (job-handle-rec-box h)
            (job->public-record h state exit-code reaped? out err)))

(define (drain-port! port box)
  (thread (lambda ()
            (let loop ()
              (define s (with-handlers ([exn:fail? (lambda (_) eof)])
                          (read-string 4096 port)))
              (cond [(eof-object? s) (void)]
                    [else (set-box! box (string-append (unbox box) s))
                          (loop)])))))

;; OS-side process start identity from /proc (Linux); fallback #f elsewhere.
;; /proc/<pid>/stat field 22 (1-indexed) is starttime in clock ticks. The
;; comm field may contain spaces/parens, so parse after the LAST ')'.
(define (proc-start-ticks pid)
  (define path (format "/proc/~a/stat" pid))
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define raw (file->string path))
    (define after-comm (substring raw (add1 (string-index-of raw #\)))))
    (define fields (string-split (string-trim after-comm)))
    ;; fields(0) is state => original field 3; starttime is original field 22
    ;; => index 22 - 3 = 19 in `fields`.
    (define start-field (list-ref fields 19))
    (string->number start-field)))

(define (string-index-of s ch)
  (for/first ([i (in-range (string-length s))]
              #:when (char=? (string-ref s i) ch))
    i))

(define (pid-alive? pid recorded-ticks)
  ;; A SIGKILLed-but-unreaped child remains in the process table as a
  ;; zombie (state 'Z' in /proc/<pid>/stat): ticks still match, so we must
  ;; treat zombies as dead or a lost process is never reconciled.
  ;; PID-reuse guard (WP3.4): identity is (pid, start-ticks). If the /proc
  ;; entry's start ticks differ from the recorded incarnation — e.g. the
  ;; child was reaped out-of-band and the PID was reused by a new process —
  ;; or the entry cannot be read, the recorded pid is treated as DEAD so
  ;; stale records reconcile instead of waiting on an unrelated process.
  ;; Single atomic /proc/<pid>/stat snapshot for BOTH the zombie check and
  ;; the PID-reuse identity check. Reading state and start ticks in two
  ;; separate passes raced kill/exec/reap and produced mixed verdicts
  ;; (ticks matching the pre-kill incarnation, state read post-exec), so
  ;; the verdict is derived from one raw line.
  (define-values (state-field now-ticks)
    (with-handlers ([exn:fail? (lambda (_) (values #f #f))])
      (define raw
        (call-with-input-file (format "/proc/~a/stat" pid)
          (lambda (in) (read-line in 'any))))
      ;; fields resume after the comm field's closing ") "; comm may
      ;; itself contain parens/spaces, so scan for ") " from the end
      (define rparen
        (let go ([i (sub1 (string-length raw))])
          (cond [(< i 1) #f]
                [(and (char=? (string-ref raw i) #\))
                      (< (add1 i) (string-length raw))
                      (char=? (string-ref raw (add1 i)) #\space))
                 i]
                [else (go (sub1 i))])))
      (if rparen
          (let* ([rest (string-trim (substring raw (+ rparen 2)))]
                 [flds (regexp-split #rx" +" rest)])
            ;; fld 0 = state; fld 19 = starttime (clock ticks since boot)
            (values (and (pair? flds) (car flds))
                    (and (>= (length flds) 20)
                         (string->number (list-ref flds 19)))))
          (values #f #f))))
  ;; Verdict policy (WP3.4): unreadable -> stale; ticks mismatch ->
  ;; PID reused (identity is (pid, start-ticks)); zombie -> reaped-pending
  ;; (dead); identity match and live state -> alive.
  (define verdict
    (cond
      [(not now-ticks) #f]
      [(and recorded-ticks (not (= recorded-ticks now-ticks))) #f]
      [(equal? state-field "Z") #f]
      [else #t]))
  (when (getenv "VJ_DEBUG")
    (eprintf "VJ pid=~a rec=~a now=~a state=~s -> ~a\n"
             pid recorded-ticks now-ticks state-field verdict))
  verdict)

;; ---------------------------------------------------- group leadership

;; Mirror sandbox/subprocess.rkt (W1 v0.99.77): wrap the child in setsid so
;; PID == PGID and TERM/KILL can reach the whole process group. Without
;; setsid, fall back to direct-child signals only (a `-PGID` kill would hit
;; our own group).
(define setsid-path (find-executable-path "setsid"))

(define (signal-group! pid signal)
  (with-handlers ([exn:fail? void])
    (system (format "/bin/bash -c 'kill -~a -- -~a 2>/dev/null || true'"
                    signal pid))))

;; TERM -> bounded 500ms grace -> KILL over the whole process group, then
;; close inherited pipes so no descendant can hold them open. Returns the
;; final exit status (integer) of the direct child.
(define (escalated-kill! h)
  (define p (job-handle-proc h))
  (define pid (subprocess-pid p))
  (if (job-handle-group-leader? h)
      (begin
        (signal-group! pid "TERM")
        (let loop ([deadline (+ (current-inexact-milliseconds) 500)])
          (when (and (eq? (subprocess-status p) 'running)
                     (< (current-inexact-milliseconds) deadline))
            (sleep 0.05)
            (loop deadline)))
        (signal-group! pid "KILL"))
      (begin
        (subprocess-kill p #f)
        (let loop ([deadline (+ (current-inexact-milliseconds) 500)])
          (when (and (eq? (subprocess-status p) 'running)
                     (< (current-inexact-milliseconds) deadline))
            (sleep 0.05)
            (loop deadline)))
        (subprocess-kill p #t)))
  (subprocess-wait p)
  (define status (subprocess-status p))
  ;; close inherited descriptors
  (with-handlers ([exn:fail? void]) (close-input-port (job-handle-out h)))
  (with-handlers ([exn:fail? void]) (close-output-port (job-handle-in h)))
  (with-handlers ([exn:fail? void]) (close-input-port (job-handle-err h)))
  status)

;; ------------------------------------------------------------ registry

(define (make-verification-registry) (verification-registry (make-hash)))

(define (registry-active-count reg)
  (for/sum ([(_ h) (in-hash (verification-registry-jobs reg))]
            #:when (eq? (handle-state h) 'running))
    1))

(define job-counter 0)

;; Start (or reuse) the job for `ident`. Acquires ownership atomically
;; (single-threaded registry mutation) BEFORE any process is launched.
(define (verification-start! reg ident executable args
                             #:timeout-ms [timeout-ms 3600000]
                             #:log-path [log-path #f])
  (define key (verification-identity-key ident))
  (define existing (hash-ref (verification-registry-jobs reg) key #f))
  (cond
    ;; 1. active owner: reuse, never launch a duplicate
    [(and existing (eq? (handle-state existing) 'running))
     (start-result (verification-job-id (unbox (job-handle-rec-box existing)))
                   #f #t)]
    ;; 2. ownership slot free (no record, or terminal record): fresh launch
    [else
     (define digest (sha256-string (string-join (cons executable args) "\u0")))
     (set! job-counter (add1 job-counter))
     (define job-id (string->symbol (format "vj-~a" job-counter)))
     ;; process-group leadership BEFORE spawn so TERM/KILL cover descendants
     (subprocess-group-enabled #t)
     ;; W1 v0.99.77 parity: subprocess-group-enabled makes the child its own
     ;; process-group leader (PID == PGID), so `kill -SIGNAL -- -PID` reaches
     ;; every descendant. Do NOT wrap in a bare `setsid`: setsid forks, the
     ;; wrapper exits 0 immediately, orphaning the real child and hiding its
     ;; exit code (observed as fabricated 'completed/0 results).
     (define-values (p out in err)
       (apply subprocess #f #f #f executable args))
     (close-output-port in) ; child never reads our stdin
     (define out-box (box ""))
     (define err-box (box ""))
     (define drains
       (list (thread (lambda () (drain-port! out out-box)))
             (thread (lambda () (drain-port! err err-box)))))
     (define start-ms (current-inexact-milliseconds))
     (define h
      (job-handle (box (verification-job job-id key digest log-path
                                         (subprocess-pid p)
                                         (proc-start-ticks (subprocess-pid p))
                                         start-ms
                                         'running #f #f "" ""
                                         #f))
                  (verification-identity-campaign ident)
                   p
                    out in err drains timeout-ms
                    #t ; group leader: subprocess-group-enabled #t above
                    out-box err-box))
     (hash-set! (verification-registry-jobs reg) key h)
     (start-result job-id #t #f)]))

(define (require-handle reg job-id)
  (or (for/first ([(_ h) (in-hash (verification-registry-jobs reg))]
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
(define (verification-wait reg job-id timeout-ms)
  (define h (require-handle reg job-id))
  (define p (job-handle-proc h))
  (define rec0 (unbox (job-handle-rec-box h)))
  (cond
    ;; already terminal: idempotent read
    [(not (eq? (verification-job-state rec0) 'running)) rec0]
    [else
     (define caller-deadline (+ (current-inexact-milliseconds) timeout-ms))
     (define own-deadline
       (+ (verification-job-start-ms rec0) (job-handle-timeout-ms h)))
     (define deadline (min caller-deadline own-deadline))
     (let loop ()
       (cond
         [(not (eq? (subprocess-status p) 'running))
          ;; process exited: reap with truthful exit code
          (define code (subprocess-status p))
          (for ([t (job-handle-drain-threads h)]) (sync/timeout 1 t))
          (define out (unbox (job-handle-out-box h)))
          (define err (unbox (job-handle-err-box h)))
          (set-state! h (if (eq? code 0) 'completed 'failed) code #t out err)
          (unbox (job-handle-rec-box h))]
         [(>= (current-inexact-milliseconds) deadline)
          ;; deadline hit: TERM->KILL, exit 124 is authoritative
          (define status (escalated-kill! h))
          (for ([t (job-handle-drain-threads h)]) (sync/timeout 1 t))
          (define out (unbox (job-handle-out-box h)))
          (define err (unbox (job-handle-err-box h)))
          (set-state! h 'timed-out 124 #t out err)
          (unbox (job-handle-rec-box h))]
         [else
          (sleep 0.005)
          (loop)]))]))

;; Explicit cancel + reap (replacement prerequisite / campaign teardown).
(define (verification-cancel! reg job-id)
  (define h (require-handle reg job-id))
  (when (eq? (handle-state h) 'running)
    (escalated-kill! h))
  (define out (unbox (job-handle-out-box h)))
  (define err (unbox (job-handle-err-box h)))
  (set-state! h 'cancelled (or (verification-job-exit-code (unbox (job-handle-rec-box h))) #f)
              #t out err)
  (unbox (job-handle-rec-box h)))

;; Campaign/wave cancellation: cancel+reap every job owned by `campaign`.
(define (verification-cancel-campaign! reg campaign)
  (define reaped '())
  (for ([(_ h) (in-hash (verification-registry-jobs reg))]
        #:when (and (string=? (job-handle-campaign h) campaign)
                    (eq? (handle-state h) 'running)))
    (set! reaped (cons (verification-cancel! reg
                                             (verification-job-id
                                              (unbox (job-handle-rec-box h))))
                       reaped)))
  (reverse reaped))

;; Reconcile a record against OS reality (resume/restart): a record whose
;; process is gone (or whose start identity no longer matches) becomes
;; orphan-recovered and releases ownership; a live, identity-matching
;; process keeps ownership.
(define (verification-reconcile! reg job-id)
  (define h (require-handle reg job-id))
  (define rec (unbox (job-handle-rec-box h)))
  (cond
    [(not (eq? (verification-job-state rec) 'running)) rec]
    [(pid-alive? (verification-job-pid rec) (verification-job-proc-start-ticks rec))
     rec]
    [else
     (set-state! h 'orphan-recovered #f #t
                 (verification-job-stdout rec) (verification-job-stderr rec))
     (unbox (job-handle-rec-box h))]))

;; Test/ops hook: kill the real child OUT OF BAND (registry loses track),
;; simulating a lost turn / coordinator crash between launch and reap.
(define (simulate-lost-process! reg job-id)
  (define h (require-handle reg job-id))
  (define p (job-handle-proc h))
  (with-handlers ([exn:fail? void])
    (subprocess-kill p #t))
  ;; close our ends so the record no longer holds a live handle
  (with-handlers ([exn:fail? void]) (close-input-port (job-handle-out h)))
  (with-handlers ([exn:fail? void]) (close-output-port (job-handle-in h)))
  (with-handlers ([exn:fail? void]) (close-input-port (job-handle-err h))))

;; ------------------------------------------------------------ contracts

(provide
 (contract-out
  [struct verification-identity ((campaign string?) (wave string?) (suite string?)
                                 (checkout string?) (profile string?))]
  [verification-identity-key (-> verification-identity? string?)]
  [struct start-result ((job-id symbol?) (started? boolean?) (existing-job? boolean?))]
  [struct verification-job
    ((id symbol?) (identity-key string?) (command-digest string?) (log-path (or/c string? #f))
     (pid real?) (proc-start-ticks (or/c real? #f)) (start-ms real?) (state symbol?)
     (exit-code (or/c exact-integer? #f)) (reaped? boolean?) (stdout string?) (stderr string?)
     (end-ms (or/c real? #f)))]
  [make-verification-registry (-> verification-registry?)]
  [registry-active-count (-> verification-registry? exact-nonnegative-integer?)]
  [verification-start! (->* (verification-registry? verification-identity? path-string?
                                            (listof (or/c string? path?)))
                            (#:timeout-ms real? #:log-path (or/c string? #f))
                            start-result?)]
  [verification-status (-> verification-registry? symbol? verification-job?)]
  [verification-wait (-> verification-registry? symbol? real? verification-job?)]
  [verification-cancel! (-> verification-registry? symbol? verification-job?)]
  [verification-cancel-campaign! (-> verification-registry? string? (listof verification-job?))]
  [verification-reconcile! (-> verification-registry? symbol? verification-job?)]
  [simulate-lost-process! (-> verification-registry? symbol? void?)]))
