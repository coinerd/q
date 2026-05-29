#lang racket/base

;; runtime/safe-mode.rkt — safe-mode runtime: actions, locking, introspection
;;
;; ARCH-02 (v0.22.0): Parameters, struct, and predicates extracted to
;; util/safe-mode-state.rkt. This module imports from there and adds
;; mutation actions, locking, and introspection.

(require racket/contract
         racket/string
         (only-in "../util/safe-mode-state.rkt"
                  current-safe-mode
                  current-safe-mode-config
                  project-root
                  safe-mode-config
                  safe-mode-config?
                  safe-mode-config-active
                  safe-mode-config-allowed-tools
                  safe-mode-config-allowed-paths
                  safe-mode-config-locked
                  safe-mode-config-project-root-path
                  make-safe-mode-config
                  blocked-tools
                  safe-mode?
                  allowed-tool?
                  allowed-path?
                  safe-mode-project-root
                  trust-level))

;; ============================================================
;; Re-export everything from safe-mode-state
;; ============================================================

(provide (contract-out
          [current-safe-mode (parameter/c boolean?)]
          [current-safe-mode-config (parameter/c (or/c safe-mode-config? #f))]
          [project-root (parameter/c path?)]
          [make-safe-mode-config
           (->* ()
                (#:active boolean?
                 #:allowed-tools (listof string?)
                 #:allowed-paths (listof path?)
                 #:locked boolean?
                 #:project-root (or/c path? #f))
                safe-mode-config?)]
          [safe-mode? (-> boolean?)]
          [allowed-tool? (-> string? boolean?)]
          [allowed-path? (-> (or/c path? string?) boolean?)]
          [safe-mode-project-root (-> path?)]
          [trust-level (-> (or/c 'full 'restricted))]
          [safe-mode-active! (-> void?)]
          [safe-mode-deactivate! (-> void?)]
          [lock-safe-mode! (-> void?)]
          [safe-mode-locked? (parameter/c boolean?)]
          [safe-mode-config-info (-> hash?)])
         ;; Struct type + accessors (plain provide for struct)
         safe-mode-config
         safe-mode-config?
         safe-mode-config-active
         safe-mode-config-allowed-tools
         safe-mode-config-allowed-paths
         safe-mode-config-locked
         safe-mode-config-project-root-path
         blocked-tools)

;; ============================================================
;; Lock state
;; ============================================================

(define safe-mode-lock-one-shot (box #f))
;; Parameter justified: tests use `parameterize` for isolation;
;; production sets #t once on lock activation.
(define safe-mode-locked? (make-parameter #f))

(define (locked?)
  (define cfg (current-safe-mode-config))
  (cond
    [(safe-mode-config? cfg) (safe-mode-config-locked cfg)]
    [else (or (unbox safe-mode-lock-one-shot) (safe-mode-locked?))]))

;; ============================================================
;; Actions
;; ============================================================

(define (safe-mode-active!)
  (current-safe-mode #t))

(define (safe-mode-deactivate!)
  (when (locked?)
    (define source (detect-safe-mode-source))
    (error
     'safe-mode-deactivate!
     (format
      "Safe mode is locked and cannot be changed during this session. Safe mode was enabled by: ~a."
      source)))
  (current-safe-mode #f)
  (when (safe-mode-config? (current-safe-mode-config))
    (define old-cfg (current-safe-mode-config))
    (current-safe-mode-config (safe-mode-config #f
                                                (safe-mode-config-allowed-tools old-cfg)
                                                (safe-mode-config-allowed-paths old-cfg)
                                                (safe-mode-config-locked old-cfg)
                                                (safe-mode-config-project-root-path old-cfg)))))

(define (lock-safe-mode!)
  (set-box! safe-mode-lock-one-shot #t)
  (safe-mode-locked? #t))

;; ============================================================
;; Introspection
;; ============================================================

(define (detect-safe-mode-source)
  (cond
    [(current-safe-mode) "CLI flag (--safe)"]
    [(let ([v (getenv "Q_SAFE_MODE")]) (and v (string=? v "1")))
     "environment variable (Q_SAFE_MODE=1)"]
    [(safe-mode-config? (current-safe-mode-config)) "session config"]
    [else "unknown source"]))

(define (safe-mode-config-info)
  (define active (safe-mode?))
  (define source (detect-safe-mode-source))
  (define root
    (let ([cfg (current-safe-mode-config)])
      (cond
        [(and (safe-mode-config? cfg) (safe-mode-config-project-root-path cfg))
         (safe-mode-config-project-root-path cfg)]
        [else (project-root)])))
  (hasheq 'active?
          active
          'trust-level
          (trust-level)
          'blocked-tools
          (if active
              blocked-tools
              '())
          'project-root
          (path->string root)
          'source
          source
          'reason
          (if active
              (format "Safe mode active (enabled by: ~a). File access restricted to: ~a"
                      source
                      (path->string root))
              #f)))
