#lang racket

;; Structured-evidence quick runner: each audited test runs in a fresh Racket
;; child with its own owned process group, writes RackUnit counts to a
;; dedicated evidence file, and the parent validates one result per input.
;;
;; Each result carries a `reason` (#f when the run was clean, otherwise why it
;; is not trustworthy): 'timeout, 'truncated, 'identity, 'no-evidence or 'exit.
;; A reason is always accompanied by an invalid result; callers print it, they
;; never need to re-derive it from exit codes and counts.
;;
;; Trust boundary: audited code is TRUSTED, not sandboxed. Process groups do
;; not contain descendants that deliberately escape them, the advertised
;; memory/process limits are not enforced by the OS, and concurrent source
;; modification is out of scope. See q/docs/TEST_CONVENTIONS.md.

(require racket/async-channel
         racket/runtime-path
         "../../sandbox/subprocess.rkt"
         (only-in "../../sandbox/limits.rkt" exec-limits)
         "scheduler.rkt")

(provide run-quick
         run-quick-file
         quick-result-valid?
         classify-quick-child-run
         (struct-out quick-result))

(define current-quick-result-transform (make-parameter values))
(module+ test-support
  (provide current-quick-result-transform))

(define-runtime-path child "quick-child.rkt")

(struct quick-result (path exit failed total stdout stderr reason) #:transparent)

;; `reason` is documentation for the caller, never a second source of truth:
;; a non-#f reason forces the result to be invalid.
(define (quick-result-valid? r)
  (and (quick-result? r)
       (string? (quick-result-path r))
       (equal? (quick-result-exit r) 0)
       (not (quick-result-reason r))
       (exact-nonnegative-integer? (quick-result-failed r))
       (zero? (quick-result-failed r))
       (exact-positive-integer? (quick-result-total r))))

;; The ONE trust classification for a finished audited child run (pure):
;; observed facts in, (values reported-exit reason) out — the same decision for
;; both evidence shapes, so the precedence lives exactly once:
;;   identity mismatch ('identity — the trust root) > timeout > truncated >
;;   nonzero exit > the evidence tail ('no-evidence for an absent/malformed
;;   record, #f for a clean well-formed run).
;; Only 'exit reports the child's own exit code; every other reason reports 1
;; because the evidence may come from a child that exited 0 anyway (the
;; contract quick-result-valid? enforces: a reason always invalidates).
;; evidence ∈ 'matched (well-formed record naming exactly the requested path)
;;          | 'mismatched (well-formed record naming a different path)
;;          | 'absent (missing or malformed record — identity unknowable)
(define (classify-quick-child-run evidence timed-out? truncated? child-exit)
  (cond
    [(eq? evidence 'mismatched) (values 1 'identity)]
    [timed-out? (values 1 'timeout)]
    [truncated? (values 1 'truncated)]
    [(not (equal? child-exit 0)) (values child-exit 'exit)]
    [(eq? evidence 'absent) (values 1 'no-evidence)]
    [else (values child-exit #f)]))

;; Fail once, up front, instead of producing N identical per-file failures on a
;; host that cannot own process groups.
;; Per-file execution adapter for the shared scheduler (final plan §2): ONE
;; audited child with its own evidence file, exactly what run-quick uses per
;; task. full-plan.rkt's run-files builds scheduler tasks around this adapter;
;; the repository-exclusive decision belongs to the CALLER (classify.rkt's
;; resource classes), never to this trusted-evidence executor.
(define (run-quick-file path timeout)
  (with-handlers ([exn:break? raise]
                  [exn? (lambda (e) (quick-result path 1 #f #f "" (exn-message e) 'no-evidence))])
    (run-one path timeout)))

(define (require-owned-process-groups!)
  (unless (and (eq? (system-type 'os*) 'linux)
               (directory-exists? "/proc/self")
               (find-executable-path "setsid"))
    (error 'run-quick "owned process group cleanup unavailable (need Linux /proc and setsid)")))

(define (read-evidence evidence)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file evidence
                          (lambda (in)
                            (define v (read in))
                            (and (eof-object? (read in)) v)))))

(define (run-one path timeout)
  (define dir (make-temporary-file "qt-run-~a" 'directory))
  (define evidence (build-path dir "evidence"))
  (dynamic-wind
   void
   (lambda ()
     (define raw
       (with-handlers ([exn:break? raise]
                       [exn:fail? (lambda (e)
                                    (quick-result path 1 #f #f "" (exn-message e) 'no-evidence))])
         ((current-quick-result-transform)
          (run-subprocess (path->string (find-executable-path "racket"))
                          #:args (list (path->string child) path (path->string evidence))
                          ;; Real tests can be chatty; the cap only exists to
                          ;; stop runaway output, not to fail verbose passes.
                          #:limits (exec-limits timeout 4194304 536870912 10)
                          ;; Inherit the caller's environment: audited tests may
                          ;; read HOME/PATH and the child prints no secrets.
                          #:environment (current-environment-variables)
                          #:process-group? #t))))
     (cond
       [(quick-result? raw) raw]
       [else
        ;; Read the evidence even for a failed child: a non-zero exit after a
        ;; complete evidence record must still report the counts and the real
        ;; exit code, not a synthesized failure.
        (define record (read-evidence evidence))
        (define stdout (subprocess-result-stdout raw))
        (define stderr (subprocess-result-stderr raw))
        (define timed-out? (subprocess-result-timed-out? raw))
        (define truncated? (subprocess-result-truncated? raw))
        (define exit (subprocess-result-exit-code raw))
        (match record
          [(list (? string? identity)
                 (? exact-nonnegative-integer? failed)
                 (? exact-positive-integer? total))
           ;; Well-formed evidence: the recorded identity is checkable.
           (define-values (code reason)
             (classify-quick-child-run (if (equal? identity path) 'matched 'mismatched)
                                       timed-out?
                                       truncated?
                                       exit))
           (quick-result path code failed total stdout stderr reason)]
          [_
           ;; Missing/malformed record: there is no identity to mismatch, so
           ;; the classification runs on the child plane with 'absent evidence.
           (define-values (code reason) (classify-quick-child-run 'absent timed-out? truncated? exit))
           (quick-result path code #f #f stdout stderr reason)])]))
   (lambda () (delete-directory/files dir))))

(define (check-arguments! files jobs timeout)
  (unless (and (exact-integer? jobs) (positive? jobs))
    (error 'run-quick "jobs must be a positive integer, got ~s" jobs))
  (unless (and (exact-positive-integer? timeout) (positive? timeout))
    (error 'run-quick "timeout must be a positive integer, got ~s" timeout))
  (unless (and (list? files) (andmap string? files))
    (error 'run-quick "files must be a list of path strings"))
  (define dups (check-duplicates files))
  (when dups
    (error 'run-quick "duplicate inputs are not supported: ~a" dups)))

;; Per-file timeouts are provided as a hash from input path to seconds. Values
;; are provisionally required to be positive exact integers, matching #:timeout.
(define (check-timeouts! timeouts)
  (unless (hash? timeouts)
    (error 'run-quick "#:timeouts must be a hash of path string to seconds, got ~s" timeouts))
  (for ([(k v) (in-hash timeouts)])
    (unless (string? k)
      (error 'run-quick "#:timeouts keys must be path strings, got ~s" k))
    (unless (and (exact-positive-integer? v) (positive? v))
      (error 'run-quick "#:timeouts values must be positive integers, got ~s" v))))

;; Normalize once so the evidence identity comparison cannot misfire on a
;; relative or non-canonical spelling of the same file.
(define (normalize-path-string f)
  (path->string (simplify-path (path->complete-path f))))

(define (normalize-files files)
  (map normalize-path-string files))

;; Timeout keys use the same normalization as the inputs, so a per-file entry
;; still applies when the caller spelled the path differently.
(define (normalize-timeouts timeouts)
  (define normalized
    (for/hash ([(k v) (in-hash timeouts)])
      (values (normalize-path-string k) v)))
  (define dups (check-duplicates (hash-keys normalized)))
  (when dups
    (error 'run-quick "duplicate #:timeouts inputs are not supported: ~a" dups))
  normalized)

(define (run-quick files jobs #:timeout [timeout 120] #:timeouts [timeouts #hash()])
  (check-arguments! files jobs timeout)
  (check-timeouts! timeouts)
  (define normalized (normalize-files files))
  (define dups (check-duplicates normalized))
  (when dups
    (error 'run-quick "duplicate inputs are not supported: ~a" dups))
  (define per-file-timeouts (normalize-timeouts timeouts))
  (when (pair? normalized)
    (require-owned-process-groups!))
  ;; Own workers and their subprocess groups: an interrupted coordinator must
  ;; not leave children running. The watcher belongs to the parent custodian,
  ;; so it survives (and cleans up after) an outright kill of this thread.
  (define cust (make-custodian))
  (define owner (current-thread))
  (define watcher
    (thread (lambda ()
              (sync owner)
              (custodian-shutdown-all cust))))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-subprocess-custodian-mode 'kill]
                                 [subprocess-group-enabled #t])
                    (run-quick/owned normalized jobs per-file-timeouts #:timeout timeout)))
                (lambda ()
                  (custodian-shutdown-all cust)
                  (kill-thread watcher))))

;; Scheduling is the ONE shared dynamic queue (scripts/testing/scheduler.rkt,
;; final plan §2): this adapter owns only per-file execution — a fresh Racket
;; child per audited file with its own evidence file — and the audited owners
;; are pure, so no task takes the repository-exclusive lock. `run-quick/owned`
;; runs inside the caller's custodian (owned process groups, cancellation,
;; worker cleanup), which the scheduler's worker threads inherit.
(define (run-quick/owned files jobs per-file-timeouts #:timeout [timeout 120])
  (define tasks
    (for/list ([f (in-list files)]
               [i (in-naturals)])
      (sched-task
       f
       (lambda (id file-timeout)
         ;; A raise other than exn:break dies without publishing and the
         ;; scheduler fails the run (unexpected worker termination);
         ;; run-one already synthesizes no-evidence results for ordinary
         ;; child failures, so this handler is belt-and-braces.
         (with-handlers ([exn:break? raise]
                         [exn? (lambda (e)
                                 (quick-result id 1 #f #f "" (exn-message e) 'no-evidence))])
           (run-one id file-timeout)))
       (hash-ref per-file-timeouts f timeout)
       #f
       #f
       0
       i)))
  (run-scheduled tasks jobs))
