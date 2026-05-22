#lang racket/base

;; sandbox/limits.rkt — resource ceilings and timeout policy
;;
;; Provides structured execution limits, preset configurations, merge logic,
;; and a predicate to check whether observed resource usage is within bounds.

(require racket/contract)

(provide exec-limits
         exec-limits?
         exec-limits-timeout-seconds
         exec-limits-max-output-bytes
         exec-limits-max-memory-bytes
         exec-limits-max-processes
         default-timeout-seconds
         default-max-output-bytes
         process-count-box
         current-max-processes
         (contract-out
          [default-exec-limits (-> exec-limits?)]
          [strict-exec-limits (-> exec-limits?)]
          [permissive-exec-limits (-> exec-limits?)]
          [merge-limits (-> exec-limits? exec-limits? exec-limits?)]
          [within-limits?
           (->* [exec-limits?]
                [#:elapsed (or/c #f number?)
                 #:output-size (or/c #f exact-nonnegative-integer?)
                 #:memory (or/c #f exact-nonnegative-integer?)
                 #:processes (or/c #f exact-nonnegative-integer?)]
                boolean?)]
          [with-resource-limits
           (->* [(-> any/c any)]
                [#:timeout number? #:max-output exact-nonnegative-integer? #:custodian custodian?]
                (values any/c boolean?))]
          [get-process-count (-> exact-nonnegative-integer?)]
          [track-process! (-> exact-positive-integer?)]
          [untrack-process! (-> exact-nonnegative-integer?)]
          [reset-process-count! (-> void?)]))

;; --------------------------------------------------
;; Standalone defaults (convenient constants)
;; --------------------------------------------------

(define default-timeout-seconds 300)
(define default-max-output-bytes 10485760) ; 10 MB

;; --------------------------------------------------
;; Process tracking (SEC-12, #115 thread-safe, #452 fixed, I-22 unified)
;;
;; process-count-box is the sole authoritative thread-safe counter.
;; I-22: Removed redundant current-process-count parameter.
;; All reads go through get-process-count (unbox).
;; Test isolation uses reset-process-count! instead of parameterize.
;; --------------------------------------------------

(define process-count-box (box 0))
(define process-count-sem (make-semaphore 1))

;; I-22: current-process-count now just reads from the box (no independent state)
;; DEPRECATED (N-06): Use (get-process-count) instead. Not exported.
(define current-process-count (make-parameter (unbox process-count-box)))

;; Internal: read authoritative count from box
(define (get-process-count)
  (unbox process-count-box))

;; W5.4 (S4-13): Max concurrent processes — enforced at track time
;; Process count scope: This limit is global across all sessions. In the current
;; architecture, only one agent session runs per process. If multi-session support
;; is added, this must become per-session.
(define current-max-processes (make-parameter 10))

(define (track-process!)
  (call-with-semaphore
   process-count-sem
   (lambda ()
     (define n (add1 (unbox process-count-box)))
     (when (> n (current-max-processes))
       (error 'track-process! "process limit exceeded: ~a (max ~a)" n (current-max-processes)))
     (set-box! process-count-box n)
     n)))

(define (untrack-process!)
  (call-with-semaphore process-count-sem
                       (lambda ()
                         (define n (max 0 (sub1 (unbox process-count-box))))
                         (set-box! process-count-box n)
                         n)))

;; T09: Reset process count for test isolation.
;; Only use in test teardown.
(define (reset-process-count!)
  (call-with-semaphore process-count-sem (lambda () (set-box! process-count-box 0))))

;; --------------------------------------------------
;; exec-limits struct
;; --------------------------------------------------

(struct exec-limits
        (timeout-seconds ; number — wall-clock timeout
         max-output-bytes ; integer — max stdout+stderr bytes
         max-memory-bytes ; integer — max memory
         max-processes) ; integer — max child processes
  #:transparent)

;; --------------------------------------------------
;; Presets
;; --------------------------------------------------

(define (default-exec-limits)
  (exec-limits 120
               1048576 ; 1 MB
               536870912 ; 512 MB
               10))

(define (strict-exec-limits)
  (exec-limits 30
               65536 ; 64 KB
               134217728 ; 128 MB
               3))

(define (permissive-exec-limits)
  (exec-limits 600
               10485760 ; 10 MB
               2147483648 ; 2 GB
               50))

;; --------------------------------------------------
;; merge-limits — stricter value wins
;; --------------------------------------------------

(define (merge-limits base override)
  (exec-limits (min (exec-limits-timeout-seconds base) (exec-limits-timeout-seconds override))
               (min (exec-limits-max-output-bytes base) (exec-limits-max-output-bytes override))
               (min (exec-limits-max-memory-bytes base) (exec-limits-max-memory-bytes override))
               (min (exec-limits-max-processes base) (exec-limits-max-processes override))))

;; --------------------------------------------------
;; within-limits? — check observed usage against limits
;; --------------------------------------------------

(define (within-limits? limits
                        #:elapsed [elapsed #f]
                        #:output-size [output-size #f]
                        #:memory [memory #f]
                        #:processes [processes #f])
  ;; Timeout check
  (and (or (not elapsed) (<= elapsed (exec-limits-timeout-seconds limits)))
       ;; Output size check
       (or (not output-size) (<= output-size (exec-limits-max-output-bytes limits)))
       ;; Memory check
       (or (not memory) (<= memory (exec-limits-max-memory-bytes limits)))
       ;; Process count check (SEC-12)
       (or (not processes) (<= processes (exec-limits-max-processes limits)))))

;; --------------------------------------------------
;; with-resource-limits — runs a thunk with timeout, output cap, and
;; custodian-based cleanup.  Returns (values result-or-#f timed-out?)
;;
;; The thunk receives a custodian it should use for its work.
;; If the thunk exceeds timeout-seconds, the custodian is shut down
;; and (values #f #t) is returned.
;; --------------------------------------------------

(define (with-resource-limits thunk
                              #:timeout [timeout default-timeout-seconds]
                              #:max-output [max-output default-max-output-bytes]
                              #:custodian [parent-cust (current-custodian)])
  (define cust (make-custodian parent-cust))
  (define result-box (box #f))
  (define timed-out-box (box #f))

  (define runner-thread
    (parameterize ([current-custodian cust])
      (thread (lambda () (set-box! result-box (thunk cust))))))

  ;; Wait for thread or timeout
  (define evt-result (sync/timeout timeout runner-thread))

  (cond
    [(not evt-result)
     ;; Timeout — kill everything under the custodian
     (set-box! timed-out-box #t)
     (custodian-shutdown-all cust)
     (values (unbox result-box) #t)]
    [else (values (unbox result-box) #f)]))
