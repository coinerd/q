#lang racket/base

;; util/config-paths.rkt — shared .q/ / .pi/ fallback path resolution
;;
;; Responsibility: centralize the directory naming convention for q
;; configuration directories.  Both runtime/resource-loader.rkt and
;; runtime/settings.rkt (and any future consumer) should use these
;; helpers instead of hard-coding ".q" / ".pi" path segments.
;;
;; Convention:
;;   - Project-local: .q/ first, .pi/ as fallback
;;   - Global:        ~/.q/

(require racket/contract)

(provide (contract-out [project-config-dirs (-> (or/c path-string? #f) (listof path?))]
                       [global-config-dir (-> path?)]
                       [resolve-project-dir-from-args (-> hash? path?)]))

;; Returns list of possible project config directory paths in priority order.
;; ".q/" first, then ".pi/" as fallback.
(define (project-config-dirs project-dir)
  (list (build-path project-dir ".q") (build-path project-dir ".pi")))

;; DESIGN FACT (W6, v0.99.38 adapter audit): This helper centralizes the
;; (build-path (find-system-path 'home-dir) ".q") pattern. ~19 modules still
;; inline this construction; they should be migrated to use this function.
;; The provider-factory.rkt was the first consolidation (W6).
;; Returns global config directory path (~/.q).
(define (global-config-dir)
  (build-path (find-system-path 'home-dir) ".q"))

;; Resolve project directory from a tool-call args hash.
;; Tries 'project_dir (symbol, from JSON) then "project_dir" (string).
;; Falls back to (current-directory) when absent or #f.
;; Consolidates the repeated
;;   (hash-ref args 'project_dir (path->string (current-directory)))
;; pattern found across extensions and tool handlers.
(define (resolve-project-dir-from-args args)
  (define raw (or (hash-ref args 'project_dir #f) (hash-ref args "project_dir" #f)))
  (cond
    [(not raw) (current-directory)]
    [(path? raw) raw]
    [else
     (if (string? raw)
         (string->path raw)
         (current-directory))]))
