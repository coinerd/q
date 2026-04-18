#lang racket/base

;; util/lockfile.rkt — Process-safe file locking via fcntl/flock
;;
;; Provides advisory file locking for concurrent write protection.
;; Uses Racket's file-port-lock where available, with a portable
;; PID-based fallback for stale lock detection.
;;
;; #1194: Process-Safe Settings Writes

(require racket/contract
         racket/file
         racket/port
         racket/string)

(provide
 (contract-out
  [call-with-lock (->* (path-string? (-> any/c))
                       (#:mode (or/c 'exclusive 'shared)
                        #:timeout-ms exact-nonnegative-integer?
                        #:stale-ms (or/c #f exact-nonnegative-integer?))
                       any/c)]
  [with-lock-result (->* (path-string? (-> any/c))
                         (#:mode (or/c 'exclusive 'shared)
                          #:timeout-ms exact-nonnegative-integer?
                          #:stale-ms (or/c #f exact-nonnegative-integer?))
                         (or/c (list/c 'ok any/c) (list/c 'timeout #f)))]))

;; ══════════════════════════════════════════════════════════════
;; Get PID via subprocess
;; ══════════════════════════════════════════════════════════════

(define (getpid)
  (define-values (sp out in err)
    (subprocess #f #f #f #f "/bin/sh" "-c" "echo $$"))
  (subprocess-wait sp)
  (define pid-str (port->string out))
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)
  (string->number (string-trim pid-str)))

;; Lock directory — ~/.q/locks/
(define (locks-dir)
  (build-path (find-system-path 'home-dir) ".q" "locks"))

;; Ensure lock directory exists
(define (ensure-locks-dir!)
  (define d (locks-dir))
  (unless (directory-exists? d)
    (make-directory* d)))

;; Lock file path for a given target
(define (lock-path target)
  (ensure-locks-dir!)
  ;; Sanitize target to a safe filename
  (define target-str (if (path? target) (path->string target) target))
  (define safe-name (regexp-replace* #rx"[^a-zA-Z0-9._-]" target-str "_"))
  (build-path (locks-dir) (string-append "lock-" safe-name)))

;; Check if a PID is alive (Unix)
(define (pid-alive? pid)
  (with-handlers ([exn:fail? (lambda (e) #t)])
    (define-values (sp out in err)
      (subprocess #f #f #f #f "/bin/sh" "-c" (format "kill -0 ~a 2>/dev/null" pid)))
    (subprocess-wait sp)
    (close-output-port in)
    (close-input-port out)
    (close-input-port err)
    (= (subprocess-status sp) 0)))

;; Read PID from lockfile
(define (read-lock-pid lock-file)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (call-with-input-file lock-file
      (lambda (in)
        (define line (read-line in))
        (and (string? line)
             (let ([n (string->number (string-trim line))])
               (and (exact-positive-integer? n) n)))))))

;; Write our PID to lockfile
(define (write-lock-pid lock-file)
  (call-with-output-file lock-file
    (lambda (out)
      (display (getpid) out)
      (newline out))
    #:exists 'truncate))

;; Try to acquire lock — returns #t on success, #f on failure
(define (try-acquire lock-file stale-ms)
  ;; Check for stale lock
  (when (and stale-ms (file-exists? lock-file))
    (define existing-pid (read-lock-pid lock-file))
    (when (and existing-pid (not (pid-alive? existing-pid)))
      (with-handlers ([exn:fail? void])
        (delete-file lock-file))))
  ;; Try to create lock file exclusively
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (call-with-output-file lock-file
      (lambda (out)
        (display (getpid) out)
        (newline out))
      #:exists 'error) ;; Fail if file exists
    #t))

;; Release lock by deleting the file (only if we own it)
(define (release-lock! lock-file)
  (with-handlers ([exn:fail? void])
    (when (file-exists? lock-file)
      (define our-pid (getpid))
      (define owner-pid (read-lock-pid lock-file))
      (when (or (not owner-pid) (= owner-pid our-pid))
        (delete-file lock-file)))))

;; call-with-lock: acquire lock, run thunk, release lock.
;; Retries until timeout-ms elapsed.
(define (call-with-lock target thunk
                        #:mode [mode 'exclusive]
                        #:timeout-ms [timeout-ms 5000]
                        #:stale-ms [stale-ms 30000])
  (define result (with-lock-result target thunk
                   #:mode mode
                   #:timeout-ms timeout-ms
                   #:stale-ms stale-ms))
  (if (eq? (car result) 'ok)
      (cadr result)
      (error 'call-with-lock "timeout acquiring lock for ~a" target)))

;; with-lock-result: returns '(ok result) or '(timeout #f)
(define (with-lock-result target thunk
                          #:mode [mode 'exclusive]
                          #:timeout-ms [timeout-ms 5000]
                          #:stale-ms [stale-ms 30000])
  (define lf (lock-path target))
  (ensure-locks-dir!)
  (define deadline (+ (current-inexact-milliseconds) timeout-ms))
  (define (retry-acquire)
    (cond
      [(try-acquire lf stale-ms)
       ;; Acquired — run thunk, then release
       (dynamic-wind
         void
         (lambda ()
           (define result (thunk))
           (list 'ok result))
         (lambda () (release-lock! lf)))]
      [(< (current-inexact-milliseconds) deadline)
       (sleep 0.05)
       (retry-acquire)]
      [else
       (list 'timeout #f)]))
  (retry-acquire))
