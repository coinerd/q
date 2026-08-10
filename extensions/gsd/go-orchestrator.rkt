#lang racket/base

;; extensions/gsd/go-orchestrator.rkt — Single-Wave Campaign Coordinator
;;
;; v0.99.80 W2: GC-1, GC-6, GC-7, GC-9, GC-13
;;
;; D1 — Coordinator loop is OUTSIDE run-prompt!:
;;   acquire campaign lease
;;   while earliest actionable wave exists:
;;       persist IN-PROGRESS + fresh attempt/fence
;;       result := run-wave!(single-wave-prompt)
;;       wait for run-wave! return
;;       if cancelled/error/timeout: persist INTERRUPTED/FAILED; stop
;;       persist VERIFYING
;;       verification := verify current attempt evidence
;;       if rejected: persist FAILED; stop
;;       commit DONE + completion outbox record
;;       deliver completion event idempotently
;;   release campaign lease

(require racket/format
         racket/file
         racket/match
         "campaign-state.rkt"
         "wave-completion.rkt"
         (only-in "wave-docs.rkt" wave-slug plan-slug-map)
         (only-in "wave-status.rkt" STATUS-DONE STATUS-FAILED)
         "projection-effects.rkt"
         "../../util/loop-result.rkt"
         (only-in "../../sandbox/gateway-bridge.rkt" shutdown-worker!)
         (only-in "plan-context-builder.rkt" current-git-root))

;; ============================================================
;; Lease (D5: process-safe OS advisory lock)
;; ============================================================

(struct campaign-lease (path port owner-pid owner-session) #:mutable)

(define (lease-path base-dir plan-id)
  (build-path base-dir ".planning" "campaigns" (string-append plan-id ".lock")))

(define (acquire-lease base-dir plan-id #:session-id [session-id "unknown"])
  (define p (lease-path base-dir plan-id))
  (define-values (dir _ __) (split-path p))
  (make-directory* dir)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    ;; The lock file may survive a crash, but the OS advisory lock cannot.
    ;; Reopening the stable path therefore recovers safely after process exit.
    (define port (open-output-file p #:exists 'can-update))
    (if (port-try-file-lock? port 'exclusive)
        (begin
          (file-position port 0)
          (write (hasheq 'owner session-id 'acquired (current-seconds)) port)
          (flush-output port)
          (campaign-lease p port (current-seconds) session-id))
        (begin
          (close-output-port port)
          #f))))

(define (release-lease! lease)
  (when (and lease (campaign-lease? lease))
    (with-handlers ([exn:fail? void])
      (port-file-unlock (campaign-lease-port lease))
      (close-output-port (campaign-lease-port lease)))))

;; ============================================================
;; Coordinator result
;; ============================================================

(struct campaign-result (status completed-waves message) #:transparent)

;; ============================================================
;; Wave runner abstraction (injectable for testing)
;; ============================================================

(define default-runner (lambda (wave-idx) 'ok))
(define default-verifier (lambda (wave-idx) #t))

;; A campaign request is the interface-safe execution boundary for /go.  It
;; carries durable campaign identity plus callbacks that build one wave prompt
;; and verify one completed attempt; interfaces supply only the prompt runner.
(struct campaign-request (base-dir record prompt-for-wave verifier)
  #:transparent
  #:constructor-name make-campaign-request)

(define (prompt-run-result->outcome result)
  (cond
    [(loop-result? result)
     (define termination (loop-result-termination-reason result))
     (define metadata (loop-result-metadata result))
     (define tool-loop-limit? (hash-ref metadata 'toolLoopLimit #f))
     (cond
       [tool-loop-limit? 'error]
       [(member termination '(completed tool-calls-pending empty-response)) 'ok]
       [(member termination '(cancelled force-shutdown shutdown)) 'cancelled]
       [else 'error])]
    [(eq? result 'completed) 'ok]
    [(eq? result 'ok) 'ok]
    [(eq? result 'cancelled) 'cancelled]
    [else 'error]))

(define (execute-campaign-request! request run-prompt)
  (run-campaign!
   (campaign-request-base-dir request)
   (campaign-request-record request)
   #:runner (lambda (wave-idx)
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-error "campaign runner failed: ~a" (exn-message e))
                                           'error)])
                (define returned-values
                  (call-with-values
                   (lambda () (run-prompt ((campaign-request-prompt-for-wave request) wave-idx)))
                   list))
                ;; Runtime/session runners return either a single
                ;; loop-result or (values updated-session result).
                (define run-result
                  (if (= (length returned-values) 2)
                      (cadr returned-values)
                      (and (pair? returned-values) (car returned-values))))
                (prompt-run-result->outcome run-result)))
   #:verifier (campaign-request-verifier request)))

;; Hook payloads cross a Typed Racket Any boundary that intentionally rejects
;; higher-order values. Keep callbacks process-local and send only an opaque
;; token through TUI/GUI/SDK hook payloads.
(define campaign-request-registry (make-hash))
(define campaign-request-registry-lock (make-semaphore 1))

(define (register-campaign-request! request)
  (define token
    (format "~a-~a-~a"
            (campaign-plan-id (campaign-request-record request))
            (current-inexact-milliseconds)
            (random 1000000000)))
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-set! campaign-request-registry token request)))
  token)

(define (lookup-campaign-request token)
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-ref campaign-request-registry token #f))))

(define (execute-campaign-token! token run-prompt)
  (define request (lookup-campaign-request token))
  (if request
      (dynamic-wind void
                    (lambda () (execute-campaign-request! request run-prompt))
                    (lambda ()
                      (call-with-semaphore campaign-request-registry-lock
                                           (lambda ()
                                             (hash-remove! campaign-request-registry token)))))
      (campaign-result 'error '() "campaign request token is missing or expired")))

;; ============================================================
;; Find wave helper
;; ============================================================

(define (find-wave rec wave-idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) wave-idx))
    w))

(define (mirror-durable-statuses! target durable)
  (when durable
    (for ([durable-wave (campaign-record-waves durable)])
      (define target-wave (find-wave target (campaign-wave-index durable-wave)))
      (when target-wave
        (set-campaign-wave-status! target-wave (campaign-wave-status durable-wave))))))

;; ============================================================
;; Single-wave campaign coordinator (D1)
;; ============================================================

(define (current-wave-for-attempt rec wave-idx fence attempt-id)
  (define wave (and rec (find-wave rec wave-idx)))
  (define attempt (and wave (campaign-wave-current-attempt wave)))
  (and rec
       wave
       attempt
       (= (campaign-fence-token rec) fence)
       (= (campaign-attempt-fence-token attempt) fence)
       (equal? (campaign-attempt-id attempt) attempt-id)
       wave))

(define (run-campaign-wave base-dir
                           rec
                           wave-idx
                           #:runner [runner default-runner]
                           #:verifier [verifier default-verifier]
                           #:meta-fix-predicate [meta-fix-predicate (lambda (_) #f)]
                           #:fence-token [requested-fence #f])
  ;; Reload before beginning so an old request token cannot overwrite a newer
  ;; completion, cancellation, or fence after waiting for the process lock.
  (define active (or (load-campaign-record base-dir (campaign-plan-id rec)) rec))
  (define fence (or requested-fence (add1 (campaign-fence-token active))))
  (define initial-wave (find-wave active wave-idx))
  (cond
    [(campaign-record-cancellation active)
     (campaign-result 'wave-cancelled '() "campaign cancellation requested")]
    [(or (not initial-wave)
         (memq (campaign-wave-status initial-wave) '(done deferred))
         (<= fence (campaign-fence-token active)))
     (campaign-result 'wave-cancelled '() "stale campaign request ignored")]
    [else
     (set-campaign-fence-token! active fence)
     (begin-attempt! active wave-idx fence)
     (persist-campaign! base-dir active)
     (define started-attempt (campaign-wave-current-attempt (find-wave active wave-idx)))
     (define expected-id (campaign-attempt-id started-attempt))
     (define (observe)
       (load-campaign-record base-dir (campaign-plan-id active)))
     (define (mirror-status! status)
       (define caller-wave (find-wave rec wave-idx))
       (when caller-wave
         (set-campaign-wave-status! caller-wave status)))
     (define (persist-current-status! status)
       (define observed (observe))
       (define observed-wave (current-wave-for-attempt observed wave-idx fence expected-id))
       (and observed-wave
            (begin
              (set-campaign-wave-status! observed-wave status)
              (persist-campaign! base-dir observed)
              (mirror-status! status)
              observed)))
     (define (interrupt-current! message)
       (persist-current-status! 'interrupted)
       (campaign-result 'wave-cancelled '() message))
     (define run-result (runner wave-idx))
     (define after-run (observe))
     (cond
       [(and after-run (campaign-record-cancellation after-run))
        (interrupt-current! "campaign cancellation requested")]
       [(not (current-wave-for-attempt after-run wave-idx fence expected-id))
        (campaign-result 'wave-cancelled '() "stale runner result ignored")]
       [else
        (case run-result
          [(ok)
           (cond
             [(meta-fix-predicate run-result)
              ;; Meta-fix: reset wave status to pending, don't consume attempt
              (log-info "meta-fix detected for wave ~a -- resetting to pending" wave-idx)
              (define meta-wave (current-wave-for-attempt after-run wave-idx fence expected-id))
              (when meta-wave
                (set-campaign-wave-status! meta-wave 'pending)
                (persist-campaign! base-dir after-run)
                (mirror-status! 'pending))
              (campaign-result 'meta-fix (list wave-idx) "meta-fix wave reset")]
             [else
              (define verifying (persist-current-status! 'verifying))
              (if (not verifying)
                  (campaign-result 'wave-cancelled '() "stale runner result ignored")
                  (let* ([approved? (with-handlers ([exn:fail? (lambda (_) #f)])
                                      (and (verifier wave-idx) #t))]
                         [after-verifier (observe)])
                    (cond
                      [(and after-verifier (campaign-record-cancellation after-verifier))
                       (interrupt-current! "campaign cancelled during verification")]
                      [(not (current-wave-for-attempt after-verifier wave-idx fence expected-id))
                       (campaign-result 'wave-cancelled '() "stale verifier result ignored")]
                      [else
                       (define result
                         (try-complete-wave! base-dir
                                             after-verifier
                                             wave-idx
                                             #:verifier-approve? approved?
                                             #:expected-attempt-id expected-id
                                             #:expected-fence-token fence))
                       (define completion-status (completion-result-status result))
                       (when (memq completion-status '(done failed))
                         (mirror-status! completion-status))
                       (case completion-status
                         [(done) (campaign-result 'wave-done (list wave-idx) "wave completed")]
                         [(failed) (campaign-result 'wave-failed '() "verifier rejected")]
                         [(stale-attempt invalid-state)
                          (campaign-result 'wave-cancelled '() "stale completion ignored")]
                         [else
                          (campaign-result 'wave-failed '() "unexpected completion state")])])))])]
          [(error)
           (if (persist-current-status! 'failed)
               (begin
                 (apply-wave-status-projections! base-dir
                                                 wave-idx
                                                 STATUS-FAILED
                                                 (lambda (idx) (wave-slug base-dir idx)))
                 (campaign-result 'wave-failed '() "runner error"))
               (campaign-result 'wave-cancelled '() "stale runner result ignored"))]
          [(cancelled) (interrupt-current! "runner cancelled")]
          [else
           (if (persist-current-status! 'failed)
               (begin
                 (apply-wave-status-projections! base-dir
                                                 wave-idx
                                                 STATUS-FAILED
                                                 (lambda (idx) (wave-slug base-dir idx)))
                 (campaign-result 'wave-failed '() "unknown runner result"))
               (campaign-result 'wave-cancelled '() "stale runner result ignored"))])])]))

;; ============================================================
;; Full campaign execution (loop one wave at a time)
;; ============================================================

(define (run-campaign! base-dir
                       rec
                       #:runner [runner default-runner]
                       #:verifier [verifier default-verifier]
                       #:meta-fix-predicate [meta-fix-predicate (lambda (_) #f)])
  (define plan-id (campaign-plan-id rec))
  (define lease (acquire-lease base-dir plan-id))
  (if (not lease)
      (campaign-result 'busy '() "campaign lease held by another process")
      (dynamic-wind
       void
       (lambda ()
         ;; A request may have waited behind another process. Reload only after
         ;; owning the lease, then carry durable state between waves.
         (define authoritative (or (load-campaign-record base-dir plan-id) rec))
         ;; v0.99.89 W2: repair stale projections left by a crash between the
         ;; durable commit and the projection apply (golden-trace oracle
         ;; finding #2). The durable record is the source of truth; reconcile
         ;; re-derives PLAN.md / wave docs / STATE.md from it. Never blocks
         ;; the campaign — a reconcile failure only logs.
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning "projection reconcile failed: ~a"
                                                   (exn-message e)))])
           (reconcile-projections-from-waves! base-dir
                                              (for/list ([w (campaign-record-waves authoritative)])
                                                (cons (campaign-wave-index w)
                                                      (campaign-wave-status w)))
                                              (plan-slug-map base-dir)))
         (let loop ([current authoritative]
                    [completed '()])
           (define next-idx (select-next-actionable-wave current))
           (cond
             [(campaign-record-cancellation current)
              (campaign-result 'wave-cancelled (reverse completed) "campaign cancellation requested")]
             [(not next-idx)
              (campaign-result 'campaign-complete (reverse completed) "all waves done or deferred")]
             [else
              (define result
                (run-campaign-wave base-dir
                                   current
                                   next-idx
                                   #:runner runner
                                   #:verifier verifier
                                   #:meta-fix-predicate meta-fix-predicate
                                   #:fence-token (add1 (campaign-fence-token current))))
              (define observed (load-campaign-record base-dir plan-id))
              (mirror-durable-statuses! rec observed)
              (case (campaign-result-status result)
                [(wave-done)
                 (define refreshed (load-campaign-record base-dir plan-id))
                 (if refreshed
                     (loop refreshed (cons next-idx completed))
                     (campaign-result 'error (reverse completed) "campaign record disappeared"))]
                [(meta-fix)
                 ;; Meta-fix: retry the same wave, attempt not consumed
                 (define refreshed (load-campaign-record base-dir plan-id))
                 (if refreshed
                     (loop refreshed completed)
                     (campaign-result 'error (reverse completed) "campaign record disappeared"))]
                [(wave-failed wave-cancelled)
                 ;; B4: Kill any stuck tool execution worker so pending tools
                 ;; don't keep running after the campaign stops.
                 (with-handlers ([exn:fail? void])
                   (shutdown-worker!))
                 (campaign-result (campaign-result-status result)
                                  (reverse completed)
                                  (campaign-result-message result))]
                [else
                 (campaign-result 'error (reverse completed) "unexpected coordinator state")])])))
       (lambda () (release-lease! lease)))))

;; ============================================================
;; /go N assertion (D8)
;; ============================================================

(define (assert-go-n rec n)
  (define next (select-next-actionable-wave rec))
  (and next (= n next)))

;; ============================================================
;; Git Root Resolution (F-7)
;; Uses `current-git-root` parameter from plan-context-builder for W1 cwd migration.
(define (find-git-root start-dir)
  (define start-path
    (path->complete-path (if (path? start-dir)
                             start-dir
                             (string->path start-dir))))
  (define (has-git? dir)
    (define git-marker (build-path dir ".git"))
    (or (directory-exists? git-marker) (file-exists? git-marker)))
  (define q-sub (build-path start-path "q"))
  (cond
    [(has-git? start-path) start-path]
    [(and (directory-exists? q-sub) (has-git? q-sub)) q-sub]
    [else
     ;; Walk up from start-path first (handles nested dirs in temp tests)
     (define walked (find-git-root-walking-up start-path has-git?))
     (if walked
         walked
         ;; Last resort: use current-git-root parameter if set and valid
         (let ([param-root (current-git-root)])
           (if (and param-root (has-git? param-root)) param-root #f)))]))

(define (find-git-root-walking-up start-path has-git?)
  (let loop ([dir start-path])
    (cond
      [(has-git? dir) dir]
      [else
       (define-values (parent _sub _dir?) (split-path dir))
       (if (and parent (path? parent) (not (equal? parent dir)))
           (loop parent)
           #f)])))

(define (git-available? base-dir)
  (and (find-git-root base-dir) #t))
;; ============================================================
;; Provide
;; ============================================================

(provide campaign-lease
         find-git-root
         git-available?
         campaign-lease?
         acquire-lease
         release-lease!
         campaign-result
         campaign-result-status
         campaign-result-completed-waves
         campaign-result-message
         run-campaign-wave
         run-campaign!
         assert-go-n
         campaign-request
         campaign-request?
         make-campaign-request
         campaign-request-base-dir
         campaign-request-record
         campaign-request-prompt-for-wave
         campaign-request-verifier
         execute-campaign-request!
         register-campaign-request!
         lookup-campaign-request
         execute-campaign-token!)
