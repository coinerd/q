#lang racket/base

;; extensions/resource-discovery.rkt — extension resource discovery
;;
;; Moved from runtime/resource-loader.rkt (v0.14.1 Wave 7).
;; Provides:
;;   discover-extension-resources — query extensions for additional resource paths
;;
;; Uses the 'resources-discover hook point to let extensions contribute
;; dynamic skill/prompt/config paths at runtime.
;;
;; Sits in extensions/ because it dispatches extension hooks — this is the
;; natural layer for code that interacts with the extension API.

(require racket/list
         "api.rkt"
         "hooks.rkt")

(provide discover-extension-resources)

;; ============================================================
;; discover-extension-resources : extension-registry? -> (listof path?)
;; ============================================================

;; Discover additional resource paths from extensions via the
;; 'resources-discover hook. Returns a list of validated directory
;; paths that extensions want scanned for skills, prompts, etc.
(define (discover-extension-resources registry)
  (unless registry
    (error 'discover-extension-resources "registry is required, got #f"))
  ;; Dispatch 'resources-discover hook on all extensions
  (define hook-result (dispatch-hooks 'resources-discover (hasheq) registry))
  ;; Extract paths from hook result payload
  (define payload (hook-result-payload hook-result))
  (define raw-paths
    (cond
      [(list? payload) payload]
      [(path? payload) (list payload)]
      [else '()]))
  ;; Validate paths exist as directories
  (filter (lambda (p) (and (path? p) (directory-exists? p))) raw-paths))
