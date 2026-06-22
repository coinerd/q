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
                       [resolve-project-dir-from-args (-> hash? path?)]
                       [find-project-root (->* () ((or/c path-string? #f)) (or/c path? #f))]
                       [project-root-or-cwd (->* () ((or/c path-string? #f)) path?)]))

;; ---------------------------------------------------------------------------
;; W7 (#8569): Project root detection via sentinel file
;; ---------------------------------------------------------------------------
;; DESIGN FACT (W7, v0.99.42): 26 script call-sites use
;; (build-path (current-directory) "util" "version.rkt") assuming cwd
;; is the project root. find-project-root eliminates this assumption
;; by using util/version.rkt as a sentinel to locate the project root
;; from any starting directory.

;; Sentinel file used to identify the project root.
;; Must exist only at the q/ project root.
(define project-root-sentinel (build-path "util" "version.rkt"))

;; find-project-root: walks up from a starting directory looking for
;; the sentinel file. Returns the directory containing it, or #f.
(define (find-project-root [start-dir #f])
  (define start
    (if start-dir
        (path->complete-path (if (string? start-dir)
                                 (string->path start-dir)
                                 start-dir))
        (current-directory)))
  (let loop ([dir (simplify-path start)])
    (if (file-exists? (build-path dir project-root-sentinel))
        dir
        (let-values ([(parent _name _dir?) (split-path dir)])
          (and (path? parent) (loop parent))))))

;; project-root-or-cwd: find-project-root with cwd fallback.
;; Backward-compatible: if sentinel not found, returns (current-directory).
(define (project-root-or-cwd [start-dir #f])
  (or (find-project-root start-dir) (current-directory)))

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
