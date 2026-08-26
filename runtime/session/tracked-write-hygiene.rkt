#lang racket/base

;; runtime/session/tracked-write-hygiene.rkt — BUG-0038 (v1.00.20 W3)
;;
;; Write-time staleness guard + concurrent-session hygiene for repo-tracked
;; file writes. The v1.00.19 /go freshness guard (BUG-0031) protects the /go
;; ENTRY only: a long-running TUI that predates a landed release can still
;; rewrite working-tree files from stale in-memory modules (two release
;; nights lost verified work that way — info.rkt / CHANGELOG reverted
;; between sync and commit).
;;
;; This module is the shared seam consulted before every tracked-file write
;; (extensions/racket-tooling-helpers.rkt:write-file-string!). Detection is
;; exact and network-free, mirroring the /go guard: compare the RUNNING
;; process's (q-version) against a FRESH read of util/version.rkt on disk.
;; Divergence => the write is REFUSED with an error naming the PID and both
;; versions ("stale process must not modify tracked files; restart q").
;;
;; Scope: paths inside the q checkout ONLY. Session/state/temp directories
;; outside the repo stay writable (fail-open there).
;;
;; Escape hatch (mirrors BUG-0031 allow-stale): parameterize
;; current-allow-stale-tracked-writes to #t for legitimate tooling; every
;; override is announced loudly (warning log + observer hook).
;;
;; Also provides:
;;   * concurrent-q-processes / concurrent-writer-warning-once! — PID-file
;;     registry under the system temp dir (one dir per checkout) with a
;;     /proc liveness check; the TUI announces other live q processes once
;;     at startup.
;;   * idle demotion — after session.idle-demote-hours (default 12) without
;;     user input the TUI demotes itself to read-only (assert-fresh-tracked-
;;     write! refuses tracked writes) until note-user-activity! fires.

(require racket/contract
         racket/dict
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         (only-in "../../util/version.rkt" q-version)
         ;; PID handling (FFI getpid + cross-platform liveness) reuses the
         ;; audited lockfile primitives rather than hand-rolling them.
         (only-in "../../util/lockfile.rkt"
                  [getpid lockfile-getpid]
                  [pid-alive? lockfile-pid-alive?]))

(define-logger q-session-hygiene)

(define-runtime-path hygiene-module-path ".")

;; Fresh read of util/version.rkt is the authoritative checkout identity:
;; `(define q-version "1.00.19")`.
(define TRACKED-WRITE-VERSION-RX #rx"q-version[ \t]*\"([^\"]+)\"")

;; ============================================================
;; Parameters (injection points for tests + the escape hatch)
;; ============================================================

;; Escape hatch: #t => stale/idle tracked writes proceed but are announced.
(define current-allow-stale-tracked-writes (make-parameter #f (lambda (v) (and v #t))))

;; Overrides the checkout root used by the guard. #f => auto-resolve to this
;; module's repository (runtime/session/../..).
(define current-tracked-write-repo-root (make-parameter #f))

;; Overrides the "running" version compared against disk. #f => (q-version).
(define current-tracked-write-running-version (make-parameter #f))

;; Overrides the PID-registry directory (tests). #f => derived from root.
(define current-q-pid-dir (make-parameter #f))

;; Observer invoked on every accepted override. Receives one string
;; describing the override. Default: loud warning log (the "logged loudly"
;; requirement); tests/telemetry can substitute a recorder.
(define current-tracked-write-override-observer
  (make-parameter (lambda (msg) (log-q-session-hygiene-warning "~a" msg))))

(define (tracked-write-running-version)
  (or (current-tracked-write-running-version) q-version))

;; The q checkout root this module ships in: runtime/session/../..
(define q-checkout-root-cache (box #f))
(define (default-q-checkout-root)
  (or (unbox q-checkout-root-cache)
      (let ([root (simplify-path (build-path hygiene-module-path 'up 'up 'up))])
        (set-box! q-checkout-root-cache root)
        root)))

(define (tracked-write-repo-root)
  (or (current-tracked-write-repo-root) (default-q-checkout-root)))

;; Containment without git: a path is "repo-tracked" for guard purposes iff
;; it resolves inside the checkout root (fail-open on any resolution error).
(define (path-under-root? root path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define rel
      (find-relative-path (simplify-path (path->complete-path root))
                          (simplify-path (path->complete-path path))))
    (and (relative-path? rel) (not (memq 'up (explode-path rel))) #t)))

;; ============================================================
;; Write-time staleness guard
;; ============================================================

;; Reads util/version.rkt FRESH from disk. #f when unresolvable (no repo /
;; unparsable) — the guard fails open there, exactly like the /go guard.
(define (read-checkout-version root)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and root
         (let ([version-file (build-path root "util" "version.rkt")])
           (and (file-exists? version-file)
                (let ([m (regexp-match TRACKED-WRITE-VERSION-RX
                                       (file->string version-file #:mode 'text))])
                  (and m (cadr m))))))))

;; #f when the write is clean (outside repo / unknown checkout / versions
;; match); (list running-version checkout-version) on divergence.
(define (tracked-write-staleness path
                                 #:root [root (tracked-write-repo-root)]
                                 #:running-version [running (tracked-write-running-version)])
  (and root
       (path-under-root? root path)
       (let ([on-disk (read-checkout-version root)])
         (and on-disk (not (string=? running on-disk)) (list running on-disk)))))

(define (announce-override! reason path running on-disk)
  (define msg
    (format
     "TRACKED-WRITE OVERRIDE accepted (pid ~a): ~a write to ~a from a ~a process — running version ~a, checkout version ~a. Legitimate only for release tooling; a stale process must not modify tracked files; restart q."
     (lockfile-getpid)
     reason
     path
     (if (string=? reason "idle") "read-only-demoted" "stale")
     running
     (or on-disk "?")))
  ((current-tracked-write-override-observer) msg))

;; THE seam. Call before writing any repo-tracked file. Outside the repo this
;; is a no-op; inside, refuses stale-version writes and idle-session writes
;; unless the escape hatch is armed (then logs loudly and proceeds).
(define (assert-fresh-tracked-write! path)
  (define root (tracked-write-repo-root))
  (define running (tracked-write-running-version))
  (cond
    [(not (path-under-root? root path)) (void)]
    [else
     (define on-disk (read-checkout-version root))
     (cond
       [(and on-disk (not (string=? running on-disk)))
        (cond
          [(current-allow-stale-tracked-writes)
           (announce-override! "stale" path running on-disk)
           (void)]
          [else
           (raise
            (exn:fail
             (format
              "stale process must not modify tracked files; restart q (pid ~a, running version ~a, checkout version ~a). To override for legitimate tooling: parameterize current-allow-stale-tracked-writes to #t."
              (lockfile-getpid)
              running
              on-disk)
             (current-continuation-marks)))])]
       [(unbox session-idle-readonly-box)
        (cond
          [(current-allow-stale-tracked-writes)
           (announce-override! "idle" path running on-disk)
           (void)]
          [else
           (raise
            (exn:fail
             (format
              "idle session is read-only until the user interacts (pid ~a, running version ~a, checkout version ~a): session demoted after session.idle-demote-hours without input; auto-write paths disabled."
              (lockfile-getpid)
              running
              (or on-disk "?"))
             (current-continuation-marks)))])]
       [else (void)])]))

;; ============================================================
;; Idle demotion (read-only until touched)
;; ============================================================

(define last-activity-ms-box (box #f))
(define session-idle-readonly-box (box #f))
(define idle-demote-hours-box (box 12.0))
(define DEFAULT-IDLE-DEMOTE-HOURS 12.0)

(define (note-user-activity!)
  (set-box! last-activity-ms-box (current-inexact-milliseconds))
  (when (unbox session-idle-readonly-box)
    (set-box! session-idle-readonly-box #f)
    (log-q-session-hygiene-info
     "session reactivated by user input — auto-write paths re-enabled (pid ~a)"
     (lockfile-getpid))))

(define (session-idle-readonly?)
  (and (unbox session-idle-readonly-box) #t))

(define (idle-demote-hours)
  (unbox idle-demote-hours-box))

(define (set-idle-demote-hours! hours)
  (set-box! idle-demote-hours-box
            (if (and (real? hours) (positive? hours))
                (exact->inexact hours)
                DEFAULT-IDLE-DEMOTE-HOURS)))

;; settings key: session.idle-demote-hours. Tolerant to symbol/string keys
;; and nested (hash 'session ... 'idle-demote-hours) or flat spelling.
(define (settings-idle-demote-hours settings)
  (define raw
    (cond
      [(not (dict? settings)) #f]
      [else
       (define session-dict (dict-ref settings 'session (dict-ref settings "session" #f)))
       (or (and session-dict
                (dict? session-dict)
                (dict-ref session-dict
                          'idle-demote-hours
                          (and (dict? session-dict) (dict-ref session-dict "idle-demote-hours" #f))))
           (dict-ref settings 'session.idle-demote-hours #f)
           (dict-ref settings "session.idle-demote-hours" #f))]))
  (if (and (real? raw) (positive? raw))
      (exact->inexact raw)
      DEFAULT-IDLE-DEMOTE-HOURS))

;; Called from the TUI render loop. 'demoted on the transition (logs a
;; staleness notice and flips the session to read-only), 'already-demoted /
;; 'not-idle otherwise. Never raises.
(define (maybe-idle-demote! #:now-ms [now-ms (current-inexact-milliseconds)])
  (with-handlers ([exn:fail? (lambda (_) 'not-idle)])
    (cond
      [(unbox session-idle-readonly-box) 'already-demoted]
      [(not (unbox last-activity-ms-box)) 'not-idle]
      [(>= (- now-ms (unbox last-activity-ms-box)) (* (unbox idle-demote-hours-box) 3600.0 1000.0))
       (set-box! session-idle-readonly-box #t)
       (log-q-session-hygiene-warning
        "idle session demoted to read-only: no user input for ~a h (session.idle-demote-hours) — auto-write paths disabled until the user interacts (pid ~a, running version ~a)"
        (unbox idle-demote-hours-box)
        (lockfile-getpid)
        (tracked-write-running-version))
       'demoted]
      [else 'not-idle])))

;; ============================================================
;; Concurrent-session detection (PID registry in the system temp dir)
;; ============================================================

;; via /proc + kill(pid,0) fallback (util/lockfile.rkt); wrapped to be
;; total (non-positive pids => #f, never raises) for fail-open semantics.
(define (pid-alive? pid)
  (and (exact-positive-integer? pid) (lockfile-pid-alive? pid)))

(define (q-checkout-pid-key root)
  (define raw (path->string (simplify-path (path->complete-path root))))
  (define digest
    (with-handlers ([exn:fail? (lambda (_) "default")])
      (let loop ([h 5381]
                 [bs (bytes->list (string->bytes/utf-8 raw))]
                 [n 0])
        (if (or (null? bs) (> n 4096))
            (number->string h 16)
            (loop (bitwise-and (+ (* h 33) (car bs)) #xFFFFFFFFFFFF) (cdr bs) (add1 n))))))
  digest)

(define (q-checkout-pid-dir [root (tracked-write-repo-root)])
  (or (current-q-pid-dir)
      (build-path (find-system-path 'temp-dir) "q-checkouts" (q-checkout-pid-key root))))

(define (pid-file-path dir pid)
  (build-path dir (format "~a.pid" pid)))

(define (read-pid-file-name f)
  (define m (regexp-match #rx"^([0-9]+)\\.pid$" (path->string f)))
  (and m (string->number (cadr m))))

;; Live q processes registered for this checkout, excluding ourselves.
;; Best-effort: unreadable/absent registry => '().
(define (concurrent-q-processes [root (tracked-write-repo-root)])
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define dir (q-checkout-pid-dir root))
    (define self (lockfile-getpid))
    (sort (filter (lambda (pid) (and pid (not (= pid self)) (pid-alive? pid)))
                  (map read-pid-file-name (directory-list dir #:build? #f)))
          <)))

(define (register-q-process! [root (tracked-write-repo-root)])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-q-session-hygiene-warning
                                "pid registration failed (best-effort, continuing): ~a"
                                (exn-message e)))])
    (define dir (q-checkout-pid-dir root))
    (make-directory* dir)
    (call-with-output-file (pid-file-path dir (lockfile-getpid))
                           (lambda (out)
                             (fprintf out
                                      "~a ~a ~a\n"
                                      (current-inexact-milliseconds)
                                      (tracked-write-running-version)
                                      (path->string (simplify-path (path->complete-path root)))))
                           #:exists 'replace)))

(define (unregister-q-process! [root (tracked-write-repo-root)])
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (define f (pid-file-path (q-checkout-pid-dir root) (lockfile-getpid)))
    (when (file-exists? f)
      (delete-file f))))

(define concurrent-warning-emitted-box (box #f))

;; One concurrent-writer warning per process. Prints the one-line warning at
;; TUI startup, logs it, and returns the message (or #f when nothing to
;; report / already announced). Subsequent calls are no-ops.
(define (concurrent-writer-warning-once! [root (tracked-write-repo-root)])
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and
     (not (unbox concurrent-warning-emitted-box))
     (let ()
       (define others (concurrent-q-processes root))
       (cond
         [(null? others) #f]
         [else
          (set-box! concurrent-warning-emitted-box #t)
          (define msg
            (format
             "concurrent-writer warning: ~a other live q process~a on this checkout (pid~a ~a) — two writers can silently revert tracked files; exit one of them."
             (length others)
             (if (= 1 (length others)) "" "es")
             (if (= 1 (length others)) "" "s")
             (string-join (map ~a others) ", ")))
          (displayln msg)
          (log-q-session-hygiene-warning "~a" msg)
          msg])))))

(define (reset-concurrent-writer-warning!)
  (set-box! concurrent-warning-emitted-box #f))

;; Test/isolation hook: restore every hygiene box to its pristine state.
(define (reset-session-hygiene-state!)
  (set-box! last-activity-ms-box #f)
  (set-box! session-idle-readonly-box #f)
  (set-box! idle-demote-hours-box DEFAULT-IDLE-DEMOTE-HOURS)
  (set-box! concurrent-warning-emitted-box #f))

;; ============================================================
;; Exports (re-exported through runtime/session/session-mutation.rkt)
;; ============================================================

(provide (contract-out
          [current-allow-stale-tracked-writes (parameter/c boolean?)]
          [current-tracked-write-repo-root (parameter/c (or/c #f path-string?))]
          [current-tracked-write-running-version (parameter/c (or/c #f string?))]
          [current-q-pid-dir (parameter/c (or/c #f path-string?))]
          [current-tracked-write-override-observer (parameter/c (-> string? void?))]
          [default-q-checkout-root (-> path?)]
          [tracked-write-repo-root (-> path?)]
          [path-under-root? (-> path-string? path-string? boolean?)]
          [tracked-write-staleness
           (->* (path-string?)
                (#:root (or/c #f path-string?) #:running-version (or/c #f string?))
                (or/c #f (list/c string? string?)))]
          [assert-fresh-tracked-write! (-> path-string? void?)]
          [note-user-activity! (-> void?)]
          [session-idle-readonly? (-> boolean?)]
          [idle-demote-hours (-> real?)]
          [set-idle-demote-hours! (-> real? void?)]
          [settings-idle-demote-hours (-> any/c real?)]
          [maybe-idle-demote! (->* () (#:now-ms real?) (symbols 'demoted 'already-demoted 'not-idle))]
          [pid-alive? (-> any/c boolean?)]
          [concurrent-q-processes (->* () ((or/c #f path-string?)) (listof exact-positive-integer?))]
          [register-q-process! (->* () ((or/c #f path-string?)) void?)]
          [unregister-q-process! (->* () ((or/c #f path-string?)) void?)]
          [concurrent-writer-warning-once! (->* () ((or/c #f path-string?)) (or/c #f string?))]
          [reset-concurrent-writer-warning! (-> void?)]
          [reset-session-hygiene-state! (-> void?)]))
