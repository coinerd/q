#lang racket/base
;; STABILITY: internal

;; Concrete production adapters for extensions/gsd/effect-ports.rkt.
;; Construction is centralized by composition-root.rkt; GSD domain modules
;; consume port values rather than importing this effect shell directly.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "effect-ports.rkt"
         "wave-runner-port.rkt")

(provide make-system-filesystem-port
         make-system-git-port
         make-system-clock-port
         make-system-process-port
         run-wave-with-timeout
         current-gsd-timeout-now-ms
         current-gsd-timeout-wait)

(define (system-kind path)
  (cond
    [(file-exists? path) 'file]
    [(directory-exists? path) 'directory]
    [(link-exists? path) 'link]
    [else #f]))

(define (system-write-bytes! path content)
  (call-with-output-file path
                         (lambda (out)
                           (write-bytes content out)
                           (void))
                         #:exists 'truncate/replace))

(define (system-delete! path)
  (case (system-kind path)
    [(directory) (delete-directory/files path)]
    [(file link) (delete-file path)]
    [else (void)]))

(define (system-acquire-lock path)
  (define out (open-output-file path #:exists 'update))
  (if (port-try-file-lock? out 'exclusive)
      out
      (begin
        (close-output-port out)
        #f)))

(define (system-release-lock! _path token)
  (when (output-port? token)
    (port-file-unlock token)
    (close-output-port token)))

(define (make-system-filesystem-port)
  (gsd-filesystem-port system-kind
                       file->bytes
                       system-write-bytes!
                       (lambda (from to) (rename-file-or-directory from to #t))
                       system-delete!
                       (lambda (path) (make-directory* path))
                       (lambda (path) (sort (directory-list path #:build? #t) path<?))
                       system-acquire-lock
                       system-release-lock!))

(define (find-git-root start-dir)
  (define start-path (path->complete-path start-dir))
  (define (has-git? dir)
    (define marker (build-path dir ".git"))
    (or (directory-exists? marker) (file-exists? marker)))
  (define q-sub (build-path start-path "q"))
  (cond
    [(has-git? start-path) start-path]
    [(and (directory-exists? q-sub) (has-git? q-sub)) q-sub]
    [else
     (let loop ([dir start-path])
       (cond
         [(has-git? dir) dir]
         [else
          (define-values (parent _name _dir?) (split-path dir))
          (and (path? parent) (not (equal? parent dir)) (loop parent))]))]))

(define (run-system-process program args cwd)
  (parameterize ([current-directory cwd])
    (define executable (or (find-executable-path program) program))
    ;; v0.99.90 W0 (#9231) MAJOR-1 review fix: subprocess stdio slots accept
    ;; only #f or file-stream ports, never symbols. #f creates pipes the
    ;; parent reads via the returned port values (same pattern as
    ;; extensions/remote-collab/ssh-helpers.rkt). The pre-W0 code passed
    ;; 'out/'in/'err which raised on every call, so get-diff-excerpt always
    ;; degraded to "" in production.
    (define-values (sp out in err) (apply subprocess #f #f #f executable args))
    (close-output-port in)
    (define stdout (port->bytes out))
    (define stderr (port->bytes err))
    (close-input-port out)
    (close-input-port err)
    (subprocess-wait sp)
    (gsd-process-result (subprocess-status sp) stdout stderr)))

(define (make-system-process-port)
  ;; This adapter does not retain an owned subprocess handle, so cancellation
  ;; is intentionally a no-op. It must never stop the process-global gateway
  ;; worker, which can be shared by unrelated sessions.
  (gsd-process-port run-system-process void))

(define (make-system-git-port process-port)
  (gsd-git-port
   find-git-root
   (lambda (root files)
     (if (null? files)
         ""
         (with-handlers ([exn:fail? (lambda (_) "")])
           (define result
             ((gsd-process-port-run process-port) "git"
                                                  (append '("log" "-1" "--stat" "--oneline" "--")
                                                          files)
                                                  root))
           (define trimmed (string-trim (bytes->string/utf-8 (gsd-process-result-stdout result))))
           (if (> (string-length trimmed) 2000)
               (string-append (substring trimmed 0 2000) "...")
               trimmed))))))

(define (make-system-clock-port)
  (gsd-clock-port current-seconds current-inexact-milliseconds))

;; ============================================================
;; Wave runner timeout adapter (v0.99.90 W3 #9234)
;; ============================================================

;; Run one wave with an explicit deadline. Deterministic under fake ports:
;;   - runner finishes in time        -> its outcome
;;   - deadline passes                -> cancel! is requested, then the thread
;;                                       is given a bounded grace period to
;;                                       honor the cancellation; if it still
;;                                       will not stop it is force-killed.
;;                                       Returns 'timed-out exactly once.
;; The cancel grace is intentionally small and separate from the deadline so
;; a pending tool is asked to abort rather than being silently killed the
;; instant the deadline passes.
(define cancel-grace-sec 2)

;; W4 deterministic deadline/event seam (v1.00.24). These thread-inherited
;; parameters are the ONLY clock/wait entry points of the timeout adapter.
;; Production defaults keep the previous wall-clock behavior byte-for-byte:
;;   - now-ms reads (current-inexact-milliseconds);
;;   - wait performs (sync/timeout secs evt) with the existing two-second
;;     grace and the 0.1s poll quantum supplied by the caller.
;; Tests parameterize them with a deterministic fake clock so GSD component
;; tests never pay the one-second deadline + two-second cancellation-grace
;; wall-clock waits; no timeout, cancel, cleanup, or outcome semantics change.
;; Values are inherited by the worker thread (Racket threads inherit the
;; parameterization of their creating thread), so fakes installed around a
;; run-campaign-wave call also cover the runner worker.
(define current-gsd-timeout-now-ms
  (make-parameter current-inexact-milliseconds
                  (lambda (f)
                    (unless (and (procedure? f) (procedure-arity-includes? f 0))
                      (raise-argument-error 'current-gsd-timeout-now-ms "(-> real?)" f))
                    f)))

(define current-gsd-timeout-wait
  (make-parameter
   (lambda (evt secs) (sync/timeout secs evt))
   (lambda (f)
     (unless (and (procedure? f) (procedure-arity-includes? f 2))
       (raise-argument-error 'current-gsd-timeout-wait "(-> evt? (or/c real? #f) any/c)" f))
     f)))

(define (run-wave-with-timeout port timeout-sec wave-idx)
  (define now-ms (current-gsd-timeout-now-ms))
  (define wait! (current-gsd-timeout-wait))
  (define result-box (box #f))
  ;; Mandatory-deadline follow-up (v1.00.24): the worker must post `done`
  ;; even when the runner raises. Without this a raising runner left the
  ;; waiter below spinning until the deadline; the exception is captured
  ;; here and re-raised in the waiting thread so propagation stays
  ;; transparent to direct callers.
  (define exn-box (box #f))
  (define done (make-semaphore 0))
  (define worker
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (set-box! exn-box e))])
                (set-box! result-box ((gsd-wave-runner-port-run port) wave-idx)))
              (semaphore-post done))))
  (define deadline (+ (now-ms) (* timeout-sec 1000.0)))
  (define (stop-runner! status message)
    ((gsd-wave-runner-port-cancel! port))
    (wait! done cancel-grace-sec)
    (unless (thread-dead? worker)
      (kill-thread worker))
    (wave-execution-outcome status message))
  (let wait-loop ()
    (cond
      [((gsd-wave-runner-port-cancel-requested? port))
       (stop-runner! 'cancelled "campaign cancellation requested")]
      [else
       (define remaining (/ (- deadline (now-ms)) 1000.0))
       (cond
         [(<= remaining 0)
          (stop-runner! 'timed-out (format "runner exceeded ~a second(s)" timeout-sec))]
         [(wait! done (min 0.1 remaining))
          (define raised (unbox exn-box))
          (if raised
              (raise raised)
              (unbox result-box))]
         [else (wait-loop)])])))
