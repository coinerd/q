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
 project-root

 ;; Predicates
 safe-mode?
 allowed-tool?
 allowed-path?

 ;; Actions
 safe-mode-active!
 safe-mode-deactivate!

 ;; Introspection
 trust-level
 safe-mode-config)

;; ============================================================
;; Parameters
;; ============================================================

;; Current safe mode state: #t = active, #f = inactive.
;; Set by CLI flag parsing or explicit activation.
(define current-safe-mode (make-parameter #f))

;; Project root directory for path restriction.
;; Defaults to current working directory.
(define project-root (make-parameter (current-directory)))

;; ============================================================
;; Blocked tool set (constant)
;; ============================================================

(define blocked-tools '("bash" "edit" "write" "firecrawl"))

;; ============================================================
;; Predicates
;; ============================================================

;; safe-mode? : -> boolean?
;; Returns #t if safe mode is currently active.
;; Checks: (1) parameter, (2) env var Q_SAFE_MODE=1
(define (safe-mode?)
  (or (current-safe-mode)
      (let ([v (getenv "Q_SAFE_MODE")])
        (and v (string=? v "1")))))

;; allowed-tool? : string? -> boolean?
;; Returns #t if the tool is allowed under current mode.
(define (allowed-tool? tool-name)
  (if (safe-mode?)
      (and (not (member tool-name blocked-tools string=?))
           (not (string-prefix? tool-name "extension:")))
      #t))

;; allowed-path? : path-string? -> boolean?
;; In safe mode, path must be under project root.
;; Uses resolve-path to prevent symlink bypass (SEC-04).
;; Outside safe mode, always #t.
(define (allowed-path? path)
  (if (safe-mode?)
      (let* ([resolved-root (with-handlers ([exn:fail? (λ (_) (simplify-path (project-root)))])
                              (resolve-path (project-root)))]
             [resolved-path (with-handlers ([exn:fail? (λ (_) (simplify-path path))])
                              (resolve-path path))]
             [root-str (path->string (if (complete-path? resolved-root) resolved-root (path->complete-path resolved-root)))]
             [path-str (path->string (if (complete-path? resolved-path) resolved-path (path->complete-path resolved-path)))]
             ;; Ensure root ends with separator for prefix matching
             [root-prefix (if (string-suffix? root-str "/")
                              root-str
                              (string-append root-str "/"))])
        (or (string=? path-str root-str)
            (string-prefix? path-str root-prefix)))
      #t))

;; ============================================================
;; Actions
;; ============================================================

;; safe-mode-active! : -> void?
;; Explicitly activate safe mode.
(define (safe-mode-active!)
  (current-safe-mode #t))

;; safe-mode-deactivate! : -> void?
;; Explicitly deactivate safe mode.
(define (safe-mode-deactivate!)
  (current-safe-mode #f))

;; ============================================================
;; Introspection
;; ============================================================

;; trust-level : -> (or/c 'full 'restricted 'sandbox)
(define (trust-level)
  (if (safe-mode?)
      'restricted
      'full))

;; safe-mode-config : -> hash?
;; Returns a hash summarizing current safe mode state.
(define (safe-mode-config)
  (define active (safe-mode?))
  (hash 'active? active
        'trust-level (trust-level)
        'blocked-tools (if active blocked-tools '())
        'reason (if active
                    "Safe mode active (CLI flag, config, or Q_SAFE_MODE=1)"
                    #f)))
