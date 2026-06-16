#lang racket/base

;; agent/registry-watcher.rkt — File-system watcher for agent hot-swap
;; STABILITY: experimental
;;
;; v0.99.13 W2 (G-4): Registry Watcher
;;
;; ⚠️ INTENTIONALLY UNWIRED (v0.99.18 W4, F-HS-06):
;;   This module is fully implemented and tested but is NOT connected
;;   to the runtime startup. The watcher is gated behind
;;   mas.hot-swap.auto-reload.enabled (default #f). Even after Phase 4
;;   (hot-swap default-on), auto-reload remains opt-in.
;;   See: test-registry-hot-swap.rkt for watcher tests.
;;
;; Feature-gated: only active when:
;;   mas.hot-swap.enabled = #t AND
;;   mas.hot-swap.auto-reload.enabled = #t (default #f)
;;
;; Uses polling-based file change detection (checks modification times
;; at configurable intervals). This avoids platform-specific
;; filesystem event dependencies.
;;
;; Design:
;;   - start-registry-watcher!: start background thread watching agent/roles/
;;   - stop-registry-watcher!: terminate thread cleanly
;;   - path->role-name: convert file path to role symbol
;;   - next-version: increment patch version string ("1.0.0" → "1.0.1")

(require racket/match
         racket/string
         racket/list
         racket/contract
         "registry.rkt")

;; ============================================================
;; Path Utilities
;; ============================================================

;; Convert a role file path to a role name symbol.
;; "agent/roles/planner.rkt" → 'planner
(define (path->role-name path-str)
  (define filename
    (let* ([parts (string-split path-str "/" #:trim? #f)]
           [fname (if (null? parts)
                      path-str
                      (last parts))])
      (if (string-suffix? fname ".rkt")
          (substring fname 0 (- (string-length fname) 4))
          fname)))
  (string->symbol filename))

;; Increment the patch version of a semver string.
;; "1.0.0" → "1.0.1", "2.3.4" → "2.3.5"
(define (next-version version-str)
  (define parts (string-split version-str "."))
  (cond
    [(< (length parts) 3) (string-append version-str ".1")]
    [else
     (define major (car parts))
     (define minor (cadr parts))
     (define patch (caddr parts))
     (define new-patch
       (cond
         [(string->number patch)
          =>
          (lambda (n) (number->string (+ n 1)))]
         [else "1"]))
     (string-append major "." minor "." new-patch)]))

;; ============================================================
;; Watcher State
;; ============================================================

;; Box holding the watcher thread (or #f when not running).
(define watcher-thread-box (box #f))
;; Box holding the watcher custodian (for clean shutdown).
(define watcher-custodian-box (box #f))

;; ============================================================
;; Watcher Lifecycle
;; ============================================================

;; Start watching the roles directory for changes.
;; When a .rkt file changes, registers a new version of the role.
;; Parameters:
;;   roles-dir: path to the agent/roles/ directory
;;   register-callback: function called with (role-name new-version module-path factory-name)
;;   poll-interval: seconds between polls (default 2)
(define (start-registry-watcher! roles-dir register-callback #:poll-interval [poll-interval 2])
  (when (unbox watcher-thread-box)
    (stop-registry-watcher!))
  (define cust (make-custodian))
  (set-box! watcher-custodian-box cust)
  (define prev-mod-times (box (hasheq)))
  ;; Initialize modification times
  (init-mod-times! roles-dir prev-mod-times)
  (define thd
    (parameterize ([current-custodian cust])
      (thread (lambda ()
                (let loop ()
                  (sleep poll-interval)
                  (check-for-changes! roles-dir prev-mod-times register-callback)
                  (loop))))))
  (set-box! watcher-thread-box thd)
  thd)

;; Stop the watcher thread and clean up.
(define (stop-registry-watcher!)
  (define cust (unbox watcher-custodian-box))
  (when cust
    (custodian-shutdown-all cust))
  (set-box! watcher-thread-box #f)
  (set-box! watcher-custodian-box #f))

;; Check if the watcher is currently running.
(define (watcher-running?)
  (and (unbox watcher-thread-box) (thread-running? (unbox watcher-thread-box))))

;; ============================================================
;; Internal: File Change Detection
;; ============================================================

;; Initialize modification times for all .rkt files in the directory.
(define (init-mod-times! dir times-box)
  (when (directory-exists? dir)
    (for ([f (in-list (directory-list dir))])
      (define full-path (build-path dir f))
      (when (and (file-exists? full-path) (regexp-match? #rx"\\.rkt$" (path->string f)))
        (define mtime (file-or-directory-modify-seconds full-path))
        (set-box! times-box (hash-set (unbox times-box) (path->string full-path) mtime))))))

;; Check for changed/added files and invoke callback.
(define (check-for-changes! dir times-box callback)
  (when (directory-exists? dir)
    (for ([f (in-list (directory-list dir))])
      (define full-path (build-path dir f))
      (when (and (file-exists? full-path) (regexp-match? #rx"\\.rkt$" (path->string f)))
        (define path-str (path->string full-path))
        (define mtime (file-or-directory-modify-seconds full-path))
        (define prev (hash-ref (unbox times-box) path-str #f))
        (unless (and prev (= prev mtime))
          ;; File changed or is new
          (set-box! times-box (hash-set (unbox times-box) path-str mtime))
          (define role-name (path->role-name path-str))
          (define current-versions (agent-versions role-name))
          (define new-version
            (if (null? current-versions)
                "1.0.0"
                (next-version (car (reverse current-versions)))))
          (callback role-name new-version path-str))))))

;; ============================================================
;; Provides
;; ============================================================

(provide (contract-out [path->role-name (-> string? symbol?)]
                       [next-version (-> string? string?)]
                       [start-registry-watcher!
                        (->* (path-string? procedure?) (#:poll-interval real?) thread?)]
                       [stop-registry-watcher! (-> void?)]
                       [watcher-running? (-> boolean?)]))
