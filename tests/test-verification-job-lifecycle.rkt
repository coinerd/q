#lang racket/base

;; tests/test-verification-job-lifecycle.rkt — BUG-0053 / WP3.4
;;
;; Owned singleton verification jobs: one durable owner per verification
;; identity (campaign, wave, suite, checkout, profile), no duplicate
;; launches, attributable terminal states, stale-PID-safe reconciliation.
;;
;; TDD contract (wave W3 WP3.4):
;;   1. starting the same identity twice reuses the active job (no 2nd launch);
;;   2. replacement requires explicit cancel-and-reap first;
;;   3. ownership survives model turns (registry persists) but campaign
;;      cancellation cancels+reaps all jobs for that campaign;
;;   4. completed / failed / timed-out / cancelled / orphan-recovered are
;;      distinct terminal states (exit 124 => timed-out);
;;   5. status reads are structured records (no pgrep heuristics);
;;   6. a stale PID record cannot reclaim ownership without process
;;      identity/start-time verification (reconcile marks orphan-recovered).
;;
;; W3 owned-lane review regressions (R/WP3.4-C*): the registry's
;; check+spawn+insert and job-id allocation are ATOMIC, terminal
;; transitions of wait/cancel are SERIALIZED with kill/reap running EXACTLY
;; ONCE, cancel of a terminal job returns the record UNCHANGED, and
;; subprocess-group-enablement is scoped to the spawn via parameterize.

(require rackunit
         racket/file
         racket/list
         racket/port
         racket/system
         racket/string
         ;; subprocess-group-enabled is a racket/base primitive (the spawn
         ;; parameter checked in R/WP3.4-C5); no extra require needed.
         (prefix-in vj: "../extensions/gsd/verification-job.rkt"))

;; ---------------------------------------------------------------- identity

(test-case "identity key covers campaign, wave, suite, checkout, profile"
  (define a (vj:verification-identity "camp-1" "W3" "fast" "/repo" "local"))
  (define b (vj:verification-identity "camp-1" "W3" "fast" "/repo" "local"))
  (define c (vj:verification-identity "camp-1" "W3" "broad" "/repo" "local"))
  (check-equal? (vj:verification-identity-key a) (vj:verification-identity-key b))
  (check-not-equal? (vj:verification-identity-key a) (vj:verification-identity-key c)))

;; ------------------------------------------------------ R102 identity coordination

(test-case "R102: each registry mints its own incarnation token"
  (define reg1 (vj:make-verification-registry))
  (define reg2 (vj:make-verification-registry))
  (check-true (symbol? (vj:verification-registry-incarnation reg1)))
  (check-true (symbol? (vj:verification-registry-incarnation reg2)))
  (check-not-equal? (vj:verification-registry-incarnation reg1)
                    (vj:verification-registry-incarnation reg2)
                    "registry recreation is distinguishable from one registry")
  (check-false (vj:verification-job-facts reg1 'vj-999) "unknown job: #f, never an error"))

(test-case "R102: a replaced job's captured terminal record stays attributable (tombstone)"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-tomb" "W1" "fast" "/repo" "local"))
  (define r1 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 30")))
  (define id1 (vj:start-result-job-id r1))
  (check-pred vj:verification-job? (vj:verification-job-facts reg id1))
  (define cancelled (vj:verification-cancel! reg id1))
  (check-equal? (vj:verification-job-state cancelled) 'cancelled)
  (define r2 (vj:verification-start! reg ident "/bin/sh" '("-c" "exit 0")))
  (define id2 (vj:start-result-job-id r2))
  (check-not-equal? id2 id1 "replacement allocates a fresh job id")
  (define tomb (vj:verification-job-facts reg id1))
  (check-true (vj:verification-terminal-tombstone? tomb)
              "the replaced job is retained as a bounded tombstone")
  (when (vj:verification-terminal-tombstone? tomb)
    (check-equal? (vj:verification-terminal-tombstone-job-id tomb) id1)
    (check-equal? (vj:verification-terminal-tombstone-state tomb) 'cancelled)
    (check-true (vj:verification-terminal-tombstone-reaped? tomb))
    (check-equal? (vj:verification-terminal-tombstone-replacement-id tomb) id2)
    (check-true (string? (vj:verification-terminal-tombstone-log-path tomb))))
  (check-pred vj:verification-job? (vj:verification-job-facts reg id2))
  (vj:verification-close-registry! reg))

(test-case "R102: a terminal job completed normally is tombstoned with its real facts"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-tomb2" "W1" "fast" "/repo" "local"))
  (define r1 (vj:verification-start! reg ident "/bin/sh" '("-c" "exit 0")))
  (define id1 (vj:start-result-job-id r1))
  (define done (vj:verification-wait reg id1 5000))
  (check-equal? (vj:verification-job-state done) 'completed)
  (define r2 (vj:verification-start! reg ident "/bin/sh" '("-c" "exit 0")))
  (define tomb (vj:verification-job-facts reg id1))
  (check-true (vj:verification-terminal-tombstone? tomb))
  (when (vj:verification-terminal-tombstone? tomb)
    (check-equal? (vj:verification-terminal-tombstone-state tomb) 'completed)
    (check-equal? (vj:verification-terminal-tombstone-replacement-id tomb)
                  (vj:start-result-job-id r2)))
  (check-pred vj:verification-job? (vj:verification-job-facts reg (vj:start-result-job-id r2)))
  (vj:verification-close-registry! reg))

(test-case "R102: the tombstone window is bounded with explicit FIFO eviction"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-tombcap" "W1" "fast" "/repo" "local"))
  (define first-id #f)
  (define second-id #f)
  (for ([i (in-range (+ vj:registry-tombstone-capacity 2))])
    (define r
      (vj:verification-start! reg ident "/bin/sh" (list "-c" (if (zero? i) "sleep 30" "exit 0"))))
    (when (zero? i)
      (set! first-id (vj:start-result-job-id r))
      (vj:verification-cancel! reg first-id))
    (when (= i 1) (set! second-id (vj:start-result-job-id r)))
    (unless (zero? i)
      (vj:verification-wait reg (vj:start-result-job-id r) 10000)))
  (check-false (vj:verification-job-facts reg first-id)
               "the oldest captured record is evicted at the fixed capacity")
  (check-true (vj:verification-terminal-tombstone? (vj:verification-job-facts reg second-id))
              "recent records stay attributable")
  (vj:verification-close-registry! reg))

;; ------------------------------------------------------------ singleton

(test-case "F/WP3.4-1: same identity twice reuses the active job (no duplicate launch)"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-A" "W1" "fast" "/repo" "local"))
  (define r1 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-true (vj:start-result-started? r1) "first start must launch")
  (define r2 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-false (vj:start-result-started? r2) "second start must NOT launch a new process")
  (check-equal? (vj:start-result-job-id r2)
                (vj:start-result-job-id r1)
                "second start reuses the active job id")
  (check-equal? (vj:registry-active-count reg) 1)
  ;; reap for cleanup
  (vj:verification-cancel! reg (vj:start-result-job-id r1)))

(test-case "F/WP3.4-2: replacement requires explicit cancel-and-reap of prior job"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-B" "W1" "fast" "/repo" "local"))
  (define r1 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-true (vj:start-result-started? r1))
  (define r2 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-equal? (vj:start-result-job-id r2) (vj:start-result-job-id r1))
  ;; cancel + reap the prior job, then a genuinely new start is permitted
  (define cancelled (vj:verification-cancel! reg (vj:start-result-job-id r1)))
  (check-equal? (vj:verification-job-state cancelled) 'cancelled)
  (check-true (vj:verification-job-reaped? cancelled) "cancel must reap the process")
  (define r3 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 0.1; exit 0")))
  (check-true (vj:start-result-started? r3) "new start after cancel/reap launches fresh")
  (check-not-equal? (vj:start-result-job-id r3) (vj:start-result-job-id r1))
  (vj:verification-cancel! reg (vj:start-result-job-id r3)))

;; ------------------------------------------------- campaign cancellation

(test-case "F/WP3.4-3: campaign cancel reaps every owned job for that campaign"
  (define reg (vj:make-verification-registry))
  (define r1
    (vj:verification-start! reg
                            (vj:verification-identity "camp-C" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 5; exit 0")))
  (define r2
    (vj:verification-start! reg
                            (vj:verification-identity "camp-C" "W2" "broad" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 5; exit 0")))
  (define r3
    (vj:verification-start! reg
                            (vj:verification-identity "camp-D" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 5; exit 0")))
  (check-equal? (vj:registry-active-count reg) 3)
  (define reaped (vj:verification-cancel-campaign! reg "camp-C"))
  (check-equal? (length reaped) 2 "both camp-C jobs cancelled")
  (check-equal? (vj:registry-active-count reg) 1 "camp-D untouched")
  (for ([job reaped])
    (check-equal? (vj:verification-job-state job) 'cancelled))
  (vj:verification-cancel! reg (vj:start-result-job-id r3)))

;; ------------------------------------------------------- terminal states

(test-case "F/WP3.4-4a: success terminates as completed"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-E" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "exit 0")))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 5000))
  (check-equal? (vj:verification-job-state job) 'completed)
  (check-equal? (vj:verification-job-exit-code job) 0))

(test-case "F/WP3.4-4b: nonzero exit terminates as failed with truthful code"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-F" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "echo fake-status; exit 3")))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 5000))
  (check-equal? (vj:verification-job-state job) 'failed)
  (check-equal? (vj:verification-job-exit-code job) 3)
  (check-true (string-contains? (vj:verification-job-stdout job) "fake-status")
              "wrapper stdout is captured but printed status is not authoritative"))

(test-case "F/WP3.4-4c: exit 124 propagates as timed-out failure"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-G" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 30; exit 0")
                            #:timeout-ms 700))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 10000))
  (check-equal? (vj:verification-job-state job) 'timed-out)
  (check-equal? (vj:verification-job-exit-code job) 124)
  (check-true (vj:verification-job-reaped? job) "timeout must reap the process group"))

;; A completed owner cannot leave redirected work behind, even when its
;; terminal handle is replaced (registry hash retains only the newest owner).
(for ([exit-code '(0 3)])
  (test-case (format "completed command ~a kills redirected descendants before handle replacement"
                     exit-code)
    (define root (make-temporary-file "vj-descendant-~a" 'directory))
    (define reg (vj:make-verification-registry))
    (define ident (vj:verification-identity "descendant" "W1" "fast" "/repo" "local"))
    (define pid-file (build-path root "child.pid"))
    (dynamic-wind
     void
     (lambda ()
       (define r
         (parameterize ([current-directory root])
           (vj:verification-start!
            reg
            ident
            "/bin/sh"
            (list "-c"
                  (format "(trap '' TERM; sleep 60) >/dev/null 2>&1 & echo $! > child.pid; exit ~a"
                          exit-code)))))
       (define id (vj:start-result-job-id r))
       (define done (vj:verification-wait reg id 5000))
       (check-equal? (vj:verification-job-state done) (if (zero? exit-code) 'completed 'failed))
       (check-equal? (vj:verification-job-exit-code done) exit-code)
       (check-eq? (vj:verification-cancel! reg id)
                  done
                  "cleanup never rewrites completed attribution")
       (check-true (file-exists? pid-file))
       (define pid (string-trim (file->string pid-file)))
       (define (gone?)
         (define stat (build-path "/proc" pid "stat"))
         (or (not (file-exists? stat)) (regexp-match? #rx"\\) [ZX] " (file->string stat))))
       (define deadline (+ (current-inexact-milliseconds) 2000))
       (let wait ()
         (when (and (not (gone?)) (< (current-inexact-milliseconds) deadline))
           (sleep 0.01)
           (wait)))
       (check-true (gone?) "successful leader must not strand a child")
       (define replacement (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 30")))
       (check-true (vj:start-result-started? replacement))
       (check-not-equal? id (vj:start-result-job-id replacement))
       (check-equal?
        (vj:verification-job-state (vj:verification-status reg (vj:start-result-job-id replacement)))
        'running)
       (vj:verification-close-registry! reg))
     (lambda ()
       (vj:verification-close-registry! reg)
       (when (file-exists? pid-file)
         ;; Keep an intentionally failing RED probe from leaking its child.
         (parameterize ([current-output-port (open-output-nowhere)]
                        [current-error-port (open-output-nowhere)])
           (system* (find-executable-path "kill") "-KILL" (string-trim (file->string pid-file)))))
       (delete-directory/files root)))))

(test-case "control-pipe watcher kills a stopped group owner and its redirected command"
  (define root (make-temporary-file "vj-stopped-owner-~a" 'directory))
  (define reg (vj:make-verification-registry))
  (dynamic-wind void
                (lambda ()
                  (define r
                    (parameterize ([current-directory root])
                      (vj:verification-start!
                       reg
                       (vj:verification-identity "stopped-owner" "W1" "fast" "/repo" "local")
                       "/bin/sh"
                       '("-c" "echo $$ > child.pid; kill -STOP $PPID; trap '' TERM; sleep 60")
                       #:timeout-ms 5000)))
                  (define deadline (+ (current-inexact-milliseconds) 4000))
                  (let ready ()
                    (unless (or (file-exists? (build-path root "child.pid"))
                                (>= (current-inexact-milliseconds) deadline))
                      (sleep 0.01)
                      (ready)))
                  (check-true (file-exists? (build-path root "child.pid")))
                  (define job (vj:verification-wait reg (vj:start-result-job-id r) 200))
                  (check-equal? (vj:verification-job-state job) 'timed-out)
                  (check-equal? (vj:verification-job-exit-code job) 124)
                  (define pid (string-trim (file->string (build-path root "child.pid"))))
                  (define stat (build-path "/proc" pid "stat"))
                  (check-true (or (not (file-exists? stat))
                                  (regexp-match? #rx"\\) [ZX] " (file->string stat)))))
                (lambda ()
                  (vj:verification-close-registry! reg)
                  (delete-directory/files root))))

;; A delayed security guard reproduced the old check -> external helper race:
;; while cancellation held the terminal lock, a waiter could still reap the
;; owner in its polling loop. Never actually deliver that stale group signal.
(test-case "concurrent waiter cannot reap an owner ahead of a delayed external group signal"
  (define root (make-temporary-file "vj-no-external-signal-~a" 'directory))
  (define reg (vj:make-verification-registry))
  (define (wait-until pred seconds)
    (define deadline (+ (current-inexact-monotonic-milliseconds) (* seconds 1000)))
    (let loop ()
      (cond
        [(pred) #t]
        [(>= (current-inexact-monotonic-milliseconds) deadline) #f]
        [else
         (sleep 0.01)
         (loop)])))
  (define (dead? pid)
    (define stat (format "/proc/~a/stat" pid))
    (or (not (file-exists? stat)) (regexp-match? #rx"\\) [ZX] " (file->string stat))))
  (dynamic-wind
   void
   (lambda ()
     (define r
       (parameterize ([current-directory root])
         (vj:verification-start!
          reg
          (vj:verification-identity "delayed-helper" "W1" "fast" "/repo" "local")
          "/bin/sh"
          (list
           "-c"
           (string-append
            "trap '' TERM; echo $$ > command.pid\n"
            "/bin/sh -c 'trap \"\" TERM; echo $$ > descendant.pid; while :; do sleep 1; done' >/dev/null 2>&1 &\n"
            "while [ ! -s descendant.pid ]; do sleep 0.01; done\n"
            "echo $PPID > owner.pid; kill -STOP $PPID; echo ready > ready\n"
            ;; Only the obsolete external-helper guard releases this.
            "while [ ! -f release ]; do sleep 0.01; done\n"
            "kill -CONT $PPID; while :; do sleep 1; done\n")))))
     (define id (vj:start-result-job-id r))
     (define pid (vj:verification-job-pid (vj:verification-status reg id)))
     (check-true (wait-until (lambda () (file-exists? (build-path root "ready"))) 4))
     (define executions '())
     (define reaped-during-delay? #f)
     (define guard
       (make-security-guard
        (current-security-guard)
        (lambda (_who path modes)
          (when (memq 'execute modes)
            (set! executions (cons path executions))
            (call-with-output-file (build-path root "release") void #:exists 'truncate)
            ;; The command resumes its owner; the pending TERM kills the
            ;; group. A concurrent waiter reaps it outside the terminal lock.
            (set! reaped-during-delay?
                  (wait-until (lambda () (not (directory-exists? (format "/proc/~a" pid)))) 2))
            (error 'delayed-helper "blocked stale group signal after delay")))
        (lambda args (void))))
     (define results (make-vector 4 #f))
     (define errors (box '()))
     (define waiters
       (for/list ([i (in-range 4)])
         (thread (lambda ()
                   (with-handlers ([exn? (lambda (e) (set-box! errors (cons e (unbox errors))))])
                     (vector-set! results i (vj:verification-wait reg id 10000)))))))
     (define cancelled #f)
     (define canceller
       (parameterize ([current-security-guard guard])
         (thread (lambda ()
                   (with-handlers ([exn? (lambda (e) (set-box! errors (cons e (unbox errors))))])
                     (set! cancelled (vj:verification-cancel! reg id)))))))
     (for ([worker (in-list (cons canceller waiters))])
       (check-not-false (sync/timeout 5 worker) "wait/cancel must remain bounded"))
     (check-equal? (unbox errors) '() "no waiter/canceller errors")
     (check-equal? executions
                   '()
                   (format "no external signal helper, including after concurrent reap (~a)"
                           reaped-during-delay?))
     (check-pred vj:verification-job? cancelled)
     (when (vj:verification-job? cancelled)
       (check-equal? (vj:verification-job-state cancelled) 'cancelled)
       (check-true (vj:verification-job-reaped? cancelled))
       (for ([result (in-vector results)])
         (check-eq? result cancelled "one frozen terminal attribution"))
       (check-eq? (vj:verification-cancel! reg id) cancelled "no second cleanup"))
     (for ([name '("owner.pid" "command.pid" "descendant.pid")])
       (define path (build-path root name))
       (check-true (file-exists? path))
       (when (file-exists? path)
         (define child (string->number (string-trim (file->string path))))
         (check-true (wait-until (lambda () (dead? child)) 2)
                     (format "~a must not survive cleanup" name)))))
   (lambda ()
     (vj:verification-close-registry! reg)
     (for ([name '("owner.pid" "command.pid" "descendant.pid")])
       (define path (build-path root name))
       (when (file-exists? path)
         (define pid (string->number (string-trim (file->string path))))
         (unless (dead? pid)
           (parameterize ([current-output-port (open-output-nowhere)]
                          [current-error-port (open-output-nowhere)])
             (system* (find-executable-path "kill") "-KILL" (number->string pid))))))
     (delete-directory/files root))))

;; ---------------------------------------------------- structured status

(test-case "F/WP3.4-5: status is a structured record (id/pid/start/digest/log), no pgrep"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-H" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 2; exit 0")
                            #:log-path "/tmp/vj-test-H.log"))
  (define job (vj:verification-status reg (vj:start-result-job-id r)))
  (check-true (vj:verification-job? job))
  (check-equal? (vj:verification-job-identity-key job)
                (vj:verification-identity-key
                 (vj:verification-identity "camp-H" "W1" "fast" "/repo" "local")))
  (check-true (real? (vj:verification-job-pid job)) "records live pid")
  (check-true (real? (vj:verification-job-start-ms job)) "records start time for reuse guard")
  (check-true (string? (vj:verification-job-command-digest job))
              "records command digest (sha256) for identity")
  (check-equal? (vj:verification-job-log-path job) "/tmp/vj-test-H.log")
  (check-equal? (vj:verification-job-state job) 'running)
  (vj:verification-cancel! reg (vj:start-result-job-id r)))

;; ------------------------------------------------- stale-PID reconciliation

(test-case "F/WP3.4-6: stale record (dead pid) reconciles to orphan-recovered, releases ownership"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-I" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 2; exit 0")))
  (define id (vj:start-result-job-id r))
  ;; Simulate process death WITHOUT going through the registry (lost turn/crash):
  ;; forcibly kill the real child behind the registry's back.
  (define job (vj:verification-status reg id))
  (vj:simulate-lost-process! reg id) ; kills child out-of-band for the test
  ;; Reconciliation models a resume/restart after the loss, which is always
  ;; (far) later than the kill: a SIGKILL against a child still inside its
  ;; fork->exec window is observable as a live incarnation for a few ms.
  (sleep 0.3)
  (define rec (vj:verification-reconcile! reg id))
  (check-equal? (vj:verification-job-state rec)
                'orphan-recovered
                "dead pid with live record must be reconciled, not left running")
  (check-true (vj:verification-job-reaped? rec))
  ;; ownership released: a fresh start for the same identity is now allowed
  (define r2
    (vj:verification-start! reg
                            (vj:verification-identity "camp-I" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 0.1; exit 0")))
  (check-true (vj:start-result-started? r2) "identity is free after orphan-recovery")
  (vj:verification-cancel! reg (vj:start-result-job-id r2)))

;; No grace-period sleep after losing the supervisor: reconciliation itself
;; must wait for the independent watcher, not merely reap the dead owner.
(test-case "lost supervisor reconciliation confirms watcher cleanup before return/replacement"
  (define root (make-temporary-file "vj-lost-watcher-~a" 'directory))
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "lost-watcher" "W1" "fast" "/repo" "local"))
  (define (dead? pid)
    (define stat (format "/proc/~a/stat" pid))
    (or (not (file-exists? stat)) (regexp-match? #rx"\\) [ZX] " (file->string stat))))
  (dynamic-wind
   void
   (lambda ()
     (define r
       (parameterize ([current-directory root])
         (vj:verification-start!
          reg
          ident
          "/bin/sh"
          (list
           "-c"
           (string-append
            "trap '' TERM; echo $$ > command.pid\n"
            "/bin/sh -c 'trap \"\" TERM; echo $$ > descendant.pid; while :; do sleep 1; done' >/dev/null 2>&1 &\n"
            "while [ ! -s descendant.pid ]; do sleep 0.01; done\n"
            "echo ready > ready; while :; do sleep 1; done\n")))))
     (define deadline (+ (current-inexact-monotonic-milliseconds) 4000))
     (let ready ()
       (unless (or (file-exists? (build-path root "ready"))
                   (>= (current-inexact-monotonic-milliseconds) deadline))
         (sleep 0.005)
         (ready)))
     (check-true (file-exists? (build-path root "ready")))
     (define id (vj:start-result-job-id r))
     (define owner (vj:verification-job-pid (vj:verification-status reg id)))
     (vj:simulate-lost-process! reg id)
     ;; Wait only for the supervisor to die, not the watcher's 250ms grace.
     (let lost ()
       (unless (or (dead? owner) (>= (current-inexact-monotonic-milliseconds) deadline))
         (sleep 0.001)
         (lost)))
     (check-true (dead? owner))
     (define start (current-inexact-monotonic-milliseconds))
     (define recovered (vj:verification-reconcile! reg id))
     (check-true (< (- (current-inexact-monotonic-milliseconds) start) 3500)
                 "cleanup confirmation must remain bounded")
     (check-eq? (vj:verification-job-state recovered) 'orphan-recovered)
     (check-true (vj:verification-job-reaped? recovered))
     (for ([name '("command.pid" "descendant.pid")])
       (define pid (string->number (string-trim (file->string (build-path root name)))))
       ;; Deliberately NO retry/sleep here: death is a return-time guarantee.
       (check-true (dead? pid) (format "~a must be dead when reconcile returns" name)))
     (define replacement (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 30")))
     (check-true (vj:start-result-started? replacement))
     (check-not-equal? (vj:start-result-job-id replacement) id))
   (lambda ()
     (vj:verification-close-registry! reg)
     (for ([name '("command.pid" "descendant.pid")])
       (define path (build-path root name))
       (when (file-exists? path)
         (define pid (string-trim (file->string path)))
         (unless (dead? pid)
           (parameterize ([current-output-port (open-output-nowhere)]
                          [current-error-port (open-output-nowhere)])
             (system* (find-executable-path "kill") "-KILL" pid)))))
     (delete-directory/files root))))

;; A failed observation is NOT evidence of successful cleanup, even if the
;; supervisor is already dead. Every terminal path must preserve that fact.
(for ([operation '(cancel timeout completion reconcile)])
  (test-case (format "~a: failed cleanup confirmation retains ownership and refuses replacement"
                     operation)
    (define reg (vj:make-verification-registry))
    (define ident (vj:verification-identity "unconfirmed" "W1" "fast" "/repo" "local"))
    (define r
      (vj:verification-start! reg
                              ident
                              "/bin/sh"
                              (if (eq? operation 'completion)
                                  '("-c" "exit 0")
                                  '("-c" "sleep 30"))))
    (define id (vj:start-result-job-id r))
    (when (eq? operation 'reconcile)
      (vj:simulate-lost-process! reg id)
      (define deadline (+ (current-inexact-monotonic-milliseconds) 1000))
      (let lost ()
        (define pid (vj:verification-job-pid (vj:verification-status reg id)))
        (define stat (format "/proc/~a/stat" pid))
        (when (and (file-exists? stat)
                   (not (regexp-match? #rx"\\) [ZX] " (file->string stat)))
                   (< (current-inexact-monotonic-milliseconds) deadline))
          (sleep 0.001)
          (lost))))
    (define guard
      (make-security-guard (current-security-guard)
                           (lambda (_who path modes)
                             (when (and path (equal? path (string->path "/proc")) (memq 'read modes))
                               (error 'cleanup-confirmation "cannot observe the owned group")))
                           (lambda args (void))))
    (define start (current-inexact-monotonic-milliseconds))
    (define rec
      (parameterize ([current-security-guard guard])
        (case operation
          [(cancel) (vj:verification-cancel! reg id)]
          [(timeout) (vj:verification-wait reg id 0)]
          [(completion) (vj:verification-wait reg id 5000)]
          [(reconcile) (vj:verification-reconcile! reg id)])))
    (check-true (< (- (current-inexact-monotonic-milliseconds) start) 4000))
    (check-eq? (vj:verification-job-state rec)
               (case operation
                 [(cancel) 'cancelled]
                 [(timeout) 'timed-out]
                 [(completion) 'completed]
                 [(reconcile) 'orphan-recovered]))
    (check-false (vj:verification-job-reaped? rec) "unconfirmed is never reaped")
    (check-equal? (vj:registry-active-count reg) 1 "ownership is retained")
    (define retry (vj:verification-start! reg ident "/bin/sh" '("-c" "exit 0")))
    (check-false (vj:start-result-started? retry) "must not replace an unconfirmed owner")
    (check-equal? (vj:start-result-job-id retry) id)
    (check-eq? (vj:verification-cancel! reg id) rec "failed confirmation stays honest")
    (vj:verification-close-registry! reg)))

(test-case "F/WP3.4-6b: reconcile of a LIVE record keeps ownership (no false orphan)"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-J" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 2; exit 0")))
  (define rec (vj:verification-reconcile! reg (vj:start-result-job-id r)))
  (check-equal? (vj:verification-job-state rec)
                'running
                "live process must keep its ownership across reconcile")
  (vj:verification-cancel! reg (vj:start-result-job-id r)))

;; ---------------------------------------------------- file-backed output

;; W3 verification-truth: log-path must be REAL (an existing file
;; the job's output drains into) and the in-memory stdout/stderr records
;; must be BOUNDED tails — a multi-hour gate can never grow unbounded
;; strings in the registry, and dropped output stays durable in the log.

(test-case "log-path is real: the declared path receives the job's output"
  (define reg (vj:make-verification-registry))
  (define log-path
    (build-path (find-system-path 'temp-dir)
                (format "vj-real-log-~a.log" (current-inexact-milliseconds))))
  (with-handlers ([exn:fail? void])
    (delete-file log-path))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-K" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "echo real-log-line; exit 0")
                            #:log-path (path->string log-path)))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 5000))
  (check-equal? (vj:verification-job-state job) 'completed)
  (check-equal? (vj:verification-job-log-path job)
                (path->string log-path)
                "the declared path is recorded verbatim")
  (check-true (file-exists? log-path) "the declared log path is a real file")
  (check-true (string-contains? (file->string log-path) "real-log-line")
              "output is file-backed, not merely recorded")
  (delete-file log-path))

(test-case "no declared log-path: a real per-job log file is synthesized"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-K2" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "echo synth-log-line; exit 0")))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 5000))
  (check-equal? (vj:verification-job-state job) 'completed)
  (define p (vj:verification-job-log-path job))
  (check-true (and (string? p) (non-empty-string? p))
              "a real log path is synthesized when none is declared")
  (check-true (file-exists? p) "the synthesized log exists on disk")
  (check-true (string-contains? (file->string p) "synth-log-line")
              "the synthesized log receives the job's output")
  (delete-file p))

(test-case "in-memory stdout/stderr are BOUNDED tails; the log keeps the rest"
  (define reg (vj:make-verification-registry))
  ;; ~123KB of output — well past the documented tail bound
  (define r
    (vj:verification-start!
     reg
     (vj:verification-identity "camp-K3" "W1" "fast" "/repo" "local")
     "/bin/sh"
     (list "-c"
           (string-append "echo HEADMARK; "
                          "for i in $(seq 1 3000); do "
                          "echo 0123456789012345678901234567890123456789; done; "
                          "echo TAILMARK; exit 0"))))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 20000))
  (check-equal? (vj:verification-job-state job) 'completed)
  (define tail (vj:verification-job-stdout job))
  (check-true (<= (string-length tail) vj:verify-output-tail-chars)
              (format "in-memory record is bounded, got ~a chars" (string-length tail)))
  (check-true (positive? (string-length tail)))
  ;; the tail keeps the END: last marker survives, first marker is dropped
  (check-true (string-contains? tail "TAILMARK"))
  (check-false (string-contains? tail "HEADMARK")
               "prefix output beyond the bound is dropped from memory")
  ;; ...but the dropped prefix stays durable in the file-backed log
  (define log (vj:verification-job-log-path job))
  (check-true (and (string? log) (file-exists? log)))
  (check-true (string-contains? (file->string log) "HEADMARK")
              "the log file retains what the bounded tail dropped")
  (delete-file log))

;; ------------------------------------------- W3 review: concurrency regressions

;; Helpers: deterministic start-line barrier. Every thread blocks on `gate`
;; until the main thread releases the whole cohort at once, so concurrent
;; entries into the registry are forced to interleave.
(define (join-threads! ts)
  (for ([t ts])
    (sync t)))

(define (error-recorder! errors)
  (lambda (e) (set-box! errors (cons (exn-message e) (unbox errors)))))

(test-case "R/WP3.4-C1: concurrent same-identity starts yield exactly ONE process and job"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-R1" "W1" "fast" "/repo" "local"))
  ;; pid ledger: each spawned child appends its own pid; a racing second
  ;; spawn would append a second line
  (define pid-log
    (path->string (build-path (find-system-path 'temp-dir)
                              (format "vj-conc1-pids-~a.log" (current-inexact-milliseconds)))))
  (define n 8)
  (define gate (make-semaphore 0))
  (define errors (box '()))
  (define results (make-vector n #f))
  (define threads
    (for/list ([i (in-range n)])
      (thread (lambda ()
                (semaphore-wait gate)
                (with-handlers ([exn:fail? (error-recorder! errors)])
                  (vector-set!
                   results
                   i
                   (vj:verification-start!
                    reg
                    ident
                    "/bin/sh"
                    (list "-c" (string-append "echo $$ >> " pid-log "; sleep 2; exit 0")))))))))
  (for ([_ (in-range n)])
    (semaphore-post gate))
  (join-threads! threads)
  (check-equal? (unbox errors) '() "no concurrent starter may fail")
  ;; exactly one starter launched; every other starter reused the active job
  (define started-count
    (for/sum ([r (in-vector results)] #:when (and r (vj:start-result-started? r))) 1))
  (check-equal? started-count 1 "exactly one concurrent start may launch a process")
  (for ([r (in-vector results)])
    (check-true (vj:start-result? r) "every starter returns a start-result")
    (check-equal? (vj:start-result-job-id r)
                  (vj:start-result-job-id (vector-ref results 0))
                  "all starters converge on the ONE job id"))
  (for ([r (in-vector results)])
    (unless (vj:start-result-started? r)
      (check-true (vj:start-result-existing-job? r)
                  "a non-launching starter must report reuse of the existing job")))
  (check-equal? (vj:registry-active-count reg) 1 "exactly one active job in the registry")
  ;; process-level proof: the child pid ledger holds exactly ONE pid.
  ;; (bounded wait: fork returns before the child's echo runs)
  (let wait-for-ledger ([deadline (+ (current-inexact-milliseconds) 5000)])
    (unless (or (file-exists? pid-log) (>= (current-inexact-milliseconds) deadline))
      (sleep 0.01)
      (wait-for-ledger deadline)))
  (check-true (file-exists? pid-log) "the single child wrote its pid ledger")
  (check-equal? (length (file->lines pid-log)) 1 "exactly one child process was spawned")
  (vj:verification-cancel! reg (vj:start-result-job-id (vector-ref results 0)))
  (with-handlers ([exn:fail? void])
    (delete-file pid-log)))

(test-case "break during watcher handshake preserves one registered owner for registry close"
  (define root (make-temporary-file "vj-start-break-~a" 'directory))
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "start-break" "W1" "fast" "/repo" "local"))
  (define entered (make-semaphore 0))
  (define release (make-semaphore 0))
  (define delayed? #f)
  (define broken? #f)
  (define errors (box '()))
  (define retry #f)
  (define workers '())
  (define (wait-until pred)
    (define deadline (+ (current-inexact-monotonic-milliseconds) 4000))
    (let loop ()
      (cond
        [(pred) #t]
        [(>= (current-inexact-monotonic-milliseconds) deadline) #f]
        [else
         (sleep 0.005)
         (loop)])))
  (define (dead? pid)
    (define stat (format "/proc/~a/stat" pid))
    (or (not (file-exists? stat)) (regexp-match? #rx"\\) [ZX] " (file->string stat))))
  (define guard
    (make-security-guard (current-security-guard)
                         (lambda (_who path modes)
                           (when (and (not delayed?)
                                      path
                                      (memq 'read modes)
                                      (regexp-match? #rx"gsd-verification-watcher-"
                                                     (path->string path)))
                             (set! delayed? #t)
                             (semaphore-post entered)
                             (semaphore-wait release)))
                         (lambda args (void))))
  (dynamic-wind
   void
   (lambda ()
     (define starter
       (parameterize ([current-directory root]
                      [current-security-guard guard])
         (thread
          (lambda ()
            (with-handlers ([exn:break? (lambda (_) (set! broken? #t))]
                            [exn:fail? (error-recorder! errors)])
              (vj:verification-start!
               reg
               ident
               "/bin/sh"
               (list
                "-c"
                (string-append
                 "trap '' TERM; echo $PPID > owner.pid; echo $$ > command.pid; "
                 "(trap '' TERM; exec sleep 60) >/dev/null 2>&1 & echo $! > descendant.pid; wait"))))))))
     (set! workers (list starter))
     (check-not-false (sync/timeout 4 entered) "pause after spawn, before registration")
     (check-true (wait-until (lambda () (file-exists? (build-path root "descendant.pid"))))
                 "the unregistered owner already has a redirected descendant")
     (define duplicate
       (thread (lambda ()
                 (with-handlers ([exn:fail? (error-recorder! errors)])
                   (set! retry (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 60")))))))
     (set! workers (cons duplicate workers))
     (break-thread starter)
     (semaphore-post release)
     (for ([worker (in-list workers)])
       (check-not-false (sync/timeout 4 worker) "startup and pending break remain bounded"))
     (check-true broken? "the deferred break is delivered after registration")
     (check-equal? (unbox errors) '())
     (check-pred vj:start-result? retry)
     (when (vj:start-result? retry)
       (check-false (vj:start-result-started? retry) "no duplicate ownership after interrupted start")
       (check-true (vj:start-result-existing-job? retry)))
     (check-equal? (vj:registry-active-count reg) 1)
     (define closed (vj:verification-close-registry! reg))
     (check-equal? (length closed) 1 "close finds the interrupted starter's owner")
     (for ([job (in-list closed)])
       (check-equal? (vj:verification-job-state job) 'cancelled)
       (check-true (vj:verification-job-reaped? job))
       (check-equal? (vj:verification-job-pid job)
                     (string->number (string-trim (file->string (build-path root "owner.pid"))))))
     (for ([name '("command.pid" "descendant.pid")])
       (define pid (string-trim (file->string (build-path root name))))
       (check-true (dead? pid) (format "~a is dead when close returns" name)))
     (check-equal? (vj:registry-active-count reg) 0))
   (lambda ()
     (semaphore-post release)
     (for ([worker (in-list workers)])
       (sync/timeout 4 worker))
     (vj:verification-close-registry! reg)
     ;; A failing regression must not leak the unregistered group either.
     (define owner-file (build-path root "owner.pid"))
     (when (file-exists? owner-file)
       (define owner (string-trim (file->string owner-file)))
       (unless (dead? owner)
         (parameterize ([current-output-port (open-output-nowhere)]
                        [current-error-port (open-output-nowhere)])
           (system* (find-executable-path "kill") "-KILL" "--" (string-append "-" owner)))))
     (delete-directory/files root))))

(test-case "R/WP3.4-C2: concurrent waiters and cancellers share ONE terminal attribution"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-R2" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 3; exit 0")
                            #:timeout-ms 10000))
  (define id (vj:start-result-job-id r))
  (define n 6)
  (define gate (make-semaphore 0))
  (define errors (box '()))
  (define waits (make-vector n #f))
  (define cancels (make-vector 2 #f))
  (define threads
    (append (for/list ([i (in-range n)])
              (thread (lambda ()
                        (semaphore-wait gate)
                        (with-handlers ([exn:fail? (error-recorder! errors)])
                          (vector-set! waits i (vj:verification-wait reg id 10000))))))
            (for/list ([c (in-range 2)])
              (thread (lambda ()
                        (semaphore-wait gate)
                        (with-handlers ([exn:fail? (error-recorder! errors)])
                          (vector-set! cancels c (vj:verification-cancel! reg id))))))))
  (for ([_ (in-range (+ n 2))])
    (semaphore-post gate))
  (join-threads! threads)
  (check-equal? (unbox errors) '() "no waiter/canceller may fail")
  (define first-wait (vector-ref waits 0))
  (for ([w (in-vector waits)])
    (check-true (vj:verification-job? w) "each waiter returns a job record")
    (check-equal? w first-wait "all waiters observe the SAME frozen terminal record"))
  (check-equal? (vj:verification-job-state first-wait) 'cancelled)
  (check-true (vj:verification-job-reaped? first-wait) "cancel reaps the process exactly once")
  (for ([c (in-vector cancels)])
    (check-equal? c first-wait "every canceller returns the single recorded attribution"))
  (check-equal? (vj:verification-status reg id) first-wait "status read agrees too")
  (check-equal? (vj:registry-active-count reg) 0))

(test-case "R/WP3.4-C3: racing waiters on a self-exiting job keep the truthful attribution"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-R3" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 0.4; exit 3")))
  (define id (vj:start-result-job-id r))
  (define n 6)
  (define gate (make-semaphore 0))
  (define errors (box '()))
  (define waits (make-vector n #f))
  (define threads
    (for/list ([i (in-range n)])
      (thread (lambda ()
                (semaphore-wait gate)
                (with-handlers ([exn:fail? (error-recorder! errors)])
                  (vector-set! waits i (vj:verification-wait reg id 10000)))))))
  (for ([_ (in-range n)])
    (semaphore-post gate))
  (join-threads! threads)
  (check-equal? (unbox errors) '() "no waiter may fail")
  (define first-wait (vector-ref waits 0))
  (for ([w (in-vector waits)])
    (check-equal? w first-wait "one frozen terminal record for every racing waiter"))
  (check-equal? (vj:verification-job-state first-wait) 'failed)
  (check-equal? (vj:verification-job-exit-code first-wait) 3)
  (check-true (vj:verification-job-reaped? first-wait))
  (check-equal? (vj:registry-active-count reg) 0))

(test-case "R/WP3.4-C4: cancel of an already-terminal job returns the record UNCHANGED"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-R4" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "exit 0")))
  (define id (vj:start-result-job-id r))
  (define done (vj:verification-wait reg id 5000))
  (check-equal? (vj:verification-job-state done) 'completed)
  (define after-cancel (vj:verification-cancel! reg id))
  (check-equal? after-cancel done "cancel after completion must not rewrite the terminal record")
  (check-equal? (vj:verification-job-state after-cancel) 'completed)
  (check-equal? (vj:verification-job-exit-code after-cancel) 0)
  (check-true (vj:verification-job-reaped? after-cancel))
  ;; idempotent: a second cancel still returns the identical record
  (check-equal? (vj:verification-cancel! reg id) done))

(test-case "R/WP3.4-C4b: cancel of a timed-out job returns the timed-out record unchanged"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-R4b" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 30; exit 0")
                            #:timeout-ms 500))
  (define id (vj:start-result-job-id r))
  (define timed-out (vj:verification-wait reg id 10000))
  (check-equal? (vj:verification-job-state timed-out) 'timed-out)
  (check-equal? (vj:verification-job-exit-code timed-out) 124)
  (check-equal? (vj:verification-cancel! reg id)
                timed-out
                "cancel after timeout must not rewrite the terminal record"))

(test-case "R/WP3.4-C5: start! scopes subprocess-group-enabled via parameterize"
  (define reg (vj:make-verification-registry))
  (define r #f)
  (parameterize ([subprocess-group-enabled #f])
    (set! r
          (vj:verification-start! reg
                                  (vj:verification-identity "camp-R5" "W1" "fast" "/repo" "local")
                                  "/bin/sh"
                                  '("-c" "sleep 2; exit 0")))
    (check-false (subprocess-group-enabled)
                 "spawn must not leak group-enable into the caller's parameterization"))
  ;; the child still owns its process group: cancel reaches and reaps it
  (define rec (vj:verification-cancel! reg (vj:start-result-job-id r)))
  (check-equal? (vj:verification-job-state rec) 'cancelled)
  (check-true (vj:verification-job-reaped? rec)))

(test-case "R/WP3.4-C6: racing waiters on a timeout kill/reap ONCE, share the 124 attribution"
  (define reg (vj:make-verification-registry))
  (define r
    (vj:verification-start! reg
                            (vj:verification-identity "camp-R6" "W1" "fast" "/repo" "local")
                            "/bin/sh"
                            '("-c" "sleep 20; exit 0")
                            #:timeout-ms 600))
  (define id (vj:start-result-job-id r))
  (define n 4)
  (define gate (make-semaphore 0))
  (define errors (box '()))
  (define waits (make-vector n #f))
  (define threads
    (for/list ([i (in-range n)])
      (thread (lambda ()
                (semaphore-wait gate)
                (with-handlers ([exn:fail? (error-recorder! errors)])
                  (vector-set! waits i (vj:verification-wait reg id 20000)))))))
  (for ([_ (in-range n)])
    (semaphore-post gate))
  (join-threads! threads)
  (check-equal? (unbox errors) '() "escalation must run exactly once: no waiter may fail")
  (define first-wait (vector-ref waits 0))
  (for ([w (in-vector waits)])
    (check-equal? w first-wait "one frozen timed-out record for every racing waiter"))
  (check-equal? (vj:verification-job-state first-wait) 'timed-out)
  (check-equal? (vj:verification-job-exit-code first-wait) 124)
  (check-true (vj:verification-job-reaped? first-wait))
  (check-equal? (vj:registry-active-count reg) 0))

(test-case "R/WP3.4-C7: concurrent distinct-identity starts allocate distinct job ids"
  (define reg (vj:make-verification-registry))
  (define n 8)
  (define gate (make-semaphore 0))
  (define errors (box '()))
  (define results (make-vector n #f))
  (define threads
    (for/list ([i (in-range n)])
      (thread (lambda ()
                (semaphore-wait gate)
                (with-handlers ([exn:fail? (error-recorder! errors)])
                  (vector-set!
                   results
                   i
                   (vj:verification-start!
                    reg
                    (vj:verification-identity "camp-R7" (format "W~a" i) "fast" "/repo" "local")
                    "/bin/sh"
                    '("-c" "sleep 2; exit 0"))))))))
  (for ([_ (in-range n)])
    (semaphore-post gate))
  (join-threads! threads)
  (check-equal? (unbox errors) '())
  (define ids
    (for/list ([r (in-vector results)])
      (vj:start-result-job-id r)))
  (check-equal? (length (remove-duplicates ids)) n "job-id allocation never collides")
  (check-equal? (vj:registry-active-count reg) n)
  (for ([id ids])
    (vj:verification-cancel! reg id))
  (check-equal? (vj:registry-active-count reg) 0))
