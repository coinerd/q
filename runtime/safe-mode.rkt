#lang racket/base

;; runtime/safe-mode.rkt — safe mode restrictions and trust model
;;
;; Provides query functions for safe mode state, tool restrictions,
;; and path restrictions.  Does NOT enforce anything itself — callers
;; (tool dispatch, extension loader) check these predicates.

(require racket/string
         racket/list
         racket/path)

(provide
 ;; Parameters
 current-safe-mode
 current-safe-mode-config
 project-root

 ;; Per-session config struct
 safe-mode-config
 safe-mode-config?
 safe-mode-config-active
 safe-mode-config-allowed-tools
 safe-mode-config-allowed-paths
 safe-mode-config-locked
 make-safe-mode-config

 ;; Predicates
 safe-mode?
 allowed-tool?
 allowed-path?

 ;; Actions
 safe-mode-active!
 safe-mode-deactivate!
 lock-safe-mode!
 safe-mode-locked?

 ;; Introspection
 trust-level
 safe-mode-config-info)

;; ============================================================
;; Parameters
;; ============================================================

;; Current safe mode state: #t = active, #f = inactive.
;; Set by CLI flag parsing or explicit activation.
(define current-safe-mode (make-parameter #f))

;; Per-session safe-mode config (#117).
;; When set, takes precedence over global parameters.
;; This allows RPC multi-client isolation.
(define current-safe-mode-config (make-parameter #f))

;; Project root directory for path restriction.
;; Defaults to current working directory.
(define project-root (make-parameter (current-directory)))

;; ============================================================
;; Per-session config struct (#117)
;; ============================================================

;; safe-mode-config encapsulates all safe-mode state for a session.
;; This avoids reliance on global parameters for multi-session isolation.
(struct safe-mode-config (active allowed-tools allowed-paths locked project-root-path)
  #:transparent)

(define (make-safe-mode-config #:active [active #t]
                               #:allowed-tools [allowed-tools #f]
                               #:allowed-paths [allowed-paths #f]
                               #:locked [locked #f]
                               #:project-root [proj-root (current-directory)])
  (safe-mode-config active
                    (or allowed-tools 'default)
                    (or allowed-paths 'default)
                    locked
                    proj-root))

;; ============================================================
;; Blocked tool set (constant)
;; ============================================================

(define blocked-tools '("bash" "edit" "write" "firecrawl"))

;; ============================================================
;; Predicates
;; ============================================================

;; safe-mode? : -> boolean?
;; Returns #t if safe mode is currently active.
;; Checks: (1) per-session config, (2) parameter, (3) env var Q_SAFE_MODE=1
(define (safe-mode?)
  (define cfg (current-safe-mode-config))
  (cond
    [(safe-mode-config? cfg)
     (safe-mode-config-active cfg)]
    [else
     (or (current-safe-mode)
         (let ([v (getenv "Q_SAFE_MODE")])
           (and v (string=? v "1"))))]))

;; allowed-tool? : string? -> boolean?
;; Returns #t if the tool is allowed under current mode.
(define (allowed-tool? tool-name)
  (define cfg (current-safe-mode-config))
  (cond
    [(and (safe-mode-config? cfg)
          (not (eq? (safe-mode-config-allowed-tools cfg) 'default)))
     ;; Per-session tool list
     (define allowed (safe-mode-config-allowed-tools cfg))
     (and (not (member tool-name blocked-tools string=?))
          (not (string-prefix? tool-name "extension:"))
          (or (not (list? allowed))
              (member tool-name allowed string=?)))]
    [(safe-mode?)
     (and (not (member tool-name blocked-tools string=?))
          (not (string-prefix? tool-name "extension:")))]
    [else #t]))

;; allowed-path? : path-string? -> boolean?
;; In safe mode, path must be under project root.
;; Uses resolve-path to prevent symlink bypass (SEC-04).
;; Outside safe mode, always #t.
(define (allowed-path? path)
  (cond
    [(not (safe-mode?)) #t]
    [else
     (define cfg (current-safe-mode-config))
     (define root-path
       (cond
         [(and (safe-mode-config? cfg) (safe-mode-config-project-root-path cfg))
          (safe-mode-config-project-root-path cfg)]
         [else (project-root)]))
     (define allowed-paths
       (and (safe-mode-config? cfg)
            (safe-mode-config-allowed-paths cfg)))
     ;; If specific allowed-paths list given, check against that
     (cond
       [(and (list? allowed-paths) (not (eq? allowed-paths 'default)))
        (for/or ([ap (in-list allowed-paths)])
          (string-prefix? (if (path? path) (path->string path) path)
                          (if (path? ap) (path->string ap) ap)))]
       [else
        ;; Default: check against project root
        (let* ([resolved-root (with-handlers ([exn:fail? (λ (_) (simplify-path root-path))])
                                (resolve-path root-path))]
               [resolved-path (with-handlers ([exn:fail? (λ (_) (simplify-path path))])
                                (resolve-path path))]
               [root-str (path->string (if (complete-path? resolved-root) resolved-root (path->complete-path resolved-root)))]
               [path-str (path->string (if (complete-path? resolved-path) resolved-path (path->complete-path resolved-path)))]
               ;; Ensure root ends with separator for prefix matching
               [root-prefix (if (string-suffix? root-str "/")
                                root-str
                                (string-append root-str "/"))])
          (or (string=? path-str root-str)
              (string-prefix? path-str root-prefix)))])]))

;; Whether safe-mode deactivation is locked (one-way switch)
(define safe-mode-locked? (make-parameter #f))

;; Internal: check if locked via per-session config or parameter
(define (locked?)
  (define cfg (current-safe-mode-config))
  (cond
    [(safe-mode-config? cfg) (safe-mode-config-locked cfg)]
    [else (safe-mode-locked?)]))

;; ============================================================
;; Actions
;; ============================================================

;; safe-mode-active! : -> void?
;; Explicitly activate safe mode.
(define (safe-mode-active!)
  (current-safe-mode #t))

;; safe-mode-deactivate! : -> void?
;; Explicitly deactivate safe mode.
;; Raises an error if safe-mode is locked.
(define (safe-mode-deactivate!)
  (when (locked?)
    (error 'safe-mode-deactivate! "safe-mode is locked and cannot be deactivated"))
  (current-safe-mode #f)
  ;; Also clear per-session config active state
  (when (safe-mode-config? (current-safe-mode-config))
    (define old-cfg (current-safe-mode-config))
    (current-safe-mode-config
     (safe-mode-config #f
                       (safe-mode-config-allowed-tools old-cfg)
                       (safe-mode-config-allowed-paths old-cfg)
                       (safe-mode-config-locked old-cfg)
                       (safe-mode-config-project-root-path old-cfg)))))

;; lock-safe-mode! : -> void?
;; Lock safe-mode so it cannot be deactivated.
;; Once locked, only process restart can change the state.
(define (lock-safe-mode!)
  (safe-mode-locked? #t))

;; ============================================================
;; Introspection
;; ============================================================

;; trust-level : -> (or/c 'full 'restricted 'sandbox)
(define (trust-level)
  (if (safe-mode?)
      'restricted
      'full))

;; safe-mode-config-info : -> hash?
;; Returns a hash summarizing current safe mode state.
(define (safe-mode-config-info)
  (define active (safe-mode?))
  (hash 'active? active
        'trust-level (trust-level)
        'blocked-tools (if active blocked-tools '())
        'reason (if active
                    "Safe mode active (CLI flag, config, or Q_SAFE_MODE=1)"
                    #f)))
