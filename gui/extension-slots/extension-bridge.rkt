#lang racket/base

;; q/gui/extension-slots/extension-bridge.rkt — Bridge between GUI and extension system
;;
;; Connects GUI widget zones and custom renderers to the extension
;; lifecycle. Extensions register widgets and renderers during load,
;; and they are cleaned up during unload.

(require racket/contract
         "widget-zone.rkt"
         "custom-renderer.rkt")

(provide gui-extension-bridge?
         (contract-out
          [make-gui-extension-bridge (-> gui-extension-bridge?)]
          [bridge-register-zone! (-> gui-extension-bridge? symbol? gui-extension-bridge?)]
          [bridge-get-zone (-> gui-extension-bridge? symbol? (or/c widget-zone? #f))]
          [bridge-get-renderer-registry (-> gui-extension-bridge? renderer-registry?)]
          [bridge-extension-loaded (-> gui-extension-bridge? string? hash? gui-extension-bridge?)]
          [bridge-extension-unloaded (-> gui-extension-bridge? string? gui-extension-bridge?)]
          [bridge-list-extensions (-> gui-extension-bridge? (listof string?))]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct gui-extension-bridge (zones-box renderers-box extensions-box) #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-gui-extension-bridge)
  (gui-extension-bridge (box (hasheq)) (box (make-renderer-registry)) (box (hasheq))))

;; ──────────────────────────────
;; Zone management
;; ──────────────────────────────
(define (bridge-register-zone! bridge zone-name)
  (define current (unbox (gui-extension-bridge-zones-box bridge)))
  (set-box! (gui-extension-bridge-zones-box bridge)
            (hash-set current zone-name (make-widget-zone zone-name)))
  bridge)

(define (bridge-get-zone bridge zone-name)
  (hash-ref (unbox (gui-extension-bridge-zones-box bridge)) zone-name #f))

;; ──────────────────────────────
;; Renderer registry
;; ──────────────────────────────
(define (bridge-get-renderer-registry bridge)
  (unbox (gui-extension-bridge-renderers-box bridge)))

;; ──────────────────────────────
;; Extension lifecycle
;; ──────────────────────────────
(define (bridge-extension-loaded bridge ext-id ext-meta)
  (define current (unbox (gui-extension-bridge-extensions-box bridge)))
  (set-box! (gui-extension-bridge-extensions-box bridge) (hash-set current ext-id ext-meta))
  bridge)

(define (bridge-extension-unloaded bridge ext-id)
  (define current (unbox (gui-extension-bridge-extensions-box bridge)))
  (set-box! (gui-extension-bridge-extensions-box bridge) (hash-remove current ext-id))
  bridge)

(define (bridge-list-extensions bridge)
  (hash-keys (unbox (gui-extension-bridge-extensions-box bridge))))
