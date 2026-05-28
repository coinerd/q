#lang racket/base

;; q/gui/main.rkt — GUI entry point
;;
;; Entry point for the native GUI mode. Wires the observable bridge
;; to the agent runtime event-bus. Uses gui-easy for rendering when
;; available; falls back gracefully in headless environments.

(require racket/contract
         racket/format
         "../ui-core/observable-bridge.rkt"
         "../ui-core/dispatch.rkt"
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt"
         "../util/event.rkt")

(provide (contract-out [run-gui-with-runtime (-> any/c any/c void?)]
                       [run-gui (-> void?)]
                       [gui-available? (-> boolean?)]))

;; Check if GUI is available (display server present)
(define (gui-available?)
  (and (getenv "DISPLAY")
       (with-handlers ([exn:fail? (λ (e) #f)])
         #t)))

;; ──────────────────────────────
;; Internal: create bridge and wire to runtime
;; ──────────────────────────────
(define (wire-bridge! rt-config)
  (define bus (hash-ref rt-config 'event-bus #f))
  (define state-box (box (hash 'messages '() 'status 'idle 'model #f)))
  (define bridge
    (make-gui-state-bridge state-box
                           (unbox state-box)
                           #:filter (lambda (evt)
                                      (define t (event-ev evt))
                                      (and t (symbol? t)))
                           #:transform (lambda (evt) evt)))
  (when bus
    (bridge-subscribe! bridge bus))
  bridge)

;; ──────────────────────────────
;; run-gui — standalone GUI (no runtime)
;; ──────────────────────────────
(define (run-gui)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (exit 1))
  (eprintf "q GUI v0.63.0 — no standalone mode yet, use --gui with a prompt\n")
  (exit 0))

;; ──────────────────────────────
;; run-gui-with-runtime — full GUI with agent runtime
;; ──────────────────────────────
(define (run-gui-with-runtime rt-config cfg)
  (unless (gui-available?)
    (eprintf "No display server available. Cannot start GUI.\n")
    (exit 1))
  ;; Wire the observable bridge to the runtime event bus
  (define bridge (wire-bridge! rt-config))
  (define theme (default-theme))
  (define layout (default-gui-layout))
  ;; The actual window rendering will be added in v0.64.0 (gui/app.rkt)
  ;; For now, log and exit cleanly
  (eprintf "q GUI v0.63.0 — bridge wired, rendering placeholder\n")
  (bridge-dispose! bridge))
