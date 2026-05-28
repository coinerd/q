#lang racket/base

;; q/gui/extension-slots/widget-zone.rkt — Extension widget zones for GUI
;;
;; Defines widget zones where extensions can register custom views.

(require racket/contract)

(provide widget-zone?
         widget-zone-name
         (contract-out [make-widget-zone (->* [symbol?] [(listof hash?)] widget-zone?)]
                       [zone-register-widget! (-> widget-zone? hash? widget-zone?)]
                       [zone-unregister-widget! (-> widget-zone? symbol? widget-zone?)]
                       [zone-render (-> widget-zone? (listof hash?))]
                       [zone-find-widget (-> widget-zone? symbol? (or/c hash? #f))]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct widget-zone (name widgets-box) #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-widget-zone name [initial-widgets '()])
  (widget-zone name (box initial-widgets)))

;; ──────────────────────────────
;; Register / unregister
;; ──────────────────────────────
(define (zone-register-widget! zone widget)
  (define wid (hash-ref widget 'id #f))
  (define current (unbox (widget-zone-widgets-box zone)))
  (when wid
    (set-box! (widget-zone-widgets-box zone)
              (filter (lambda (w) (not (eq? (hash-ref w 'id #f) wid))) current)))
  (set-box! (widget-zone-widgets-box zone)
            (append (unbox (widget-zone-widgets-box zone)) (list widget)))
  zone)

(define (zone-unregister-widget! zone widget-id)
  (set-box! (widget-zone-widgets-box zone)
            (filter (lambda (w) (not (eq? (hash-ref w 'id #f) widget-id)))
                    (unbox (widget-zone-widgets-box zone))))
  zone)

;; ──────────────────────────────
;; Render
;; ──────────────────────────────
(define (zone-render zone)
  (unbox (widget-zone-widgets-box zone)))

;; ──────────────────────────────
;; Find
;; ──────────────────────────────
(define (zone-find-widget zone widget-id)
  (findf (lambda (w) (eq? (hash-ref w 'id #f) widget-id)) (unbox (widget-zone-widgets-box zone))))
