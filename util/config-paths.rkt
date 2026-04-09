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

(provide project-config-dirs
         global-config-dir)

;; Returns list of possible project config directory paths in priority order.
;; ".q/" first, then ".pi/" as fallback.
(define (project-config-dirs project-dir)
  (list (build-path project-dir ".q")
        (build-path project-dir ".pi")))

;; Returns global config directory path (~/.q).
(define (global-config-dir)
  (build-path (find-system-path 'home-dir) ".q"))
