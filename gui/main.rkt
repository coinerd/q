#lang racket/base

;; q/gui/main.rkt — GUI entry point
;;
;; Entry point for the native GUI mode. Uses gui-easy for rendering.
;; All GUI modules are guarded by (gui-available?) checks for headless CI.

(require racket/contract
         racket/format)

(provide (contract-out [run-gui-with-runtime (-> any/c any/c void?)] [run-gui (-> void?)]))

;; Check if GUI is available (display server present)
(define (gui-available?)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (dynamic-require 'racket/gui/base 'gui-available?)))

;; ──────────────────────────────
;; run-gui — standalone GUI (no runtime)
;; ──────────────────────────────
(define (run-gui)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (exit 1))
  ;; Will be implemented in later waves
  (eprintf "GUI mode started (placeholder)\n"))

;; ──────────────────────────────
;; run-gui-with-runtime — full GUI with agent runtime
;; ──────────────────────────────
(define (run-gui-with-runtime rt-config cfg)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (exit 1))
  (eprintf "GUI mode started with runtime (placeholder)\n")
  ;; Will be wired up in W5 with observable bridge
  (run-gui))
