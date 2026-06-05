#lang racket

;; q/ui-core/widget-descriptor.rkt — Backend-neutral widget descriptor
;;
;; Defines the schema for extension-contributed UI widgets.
;; Both TUI and GUI consume this descriptor — rendering is backend-specific.
;;
;; W6.1 (v0.94.6): Widget descriptor schema with trust levels and lifecycle.

(require racket/contract)

(provide widget-descriptor
         widget-descriptor?
         widget-descriptor-id
         widget-descriptor-extension-id
         widget-descriptor-zone
         widget-descriptor-kind
         widget-descriptor-content
         widget-descriptor-priority
         widget-descriptor-capabilities
         widget-descriptor-trust-level
         widget-descriptor-lifecycle-token

         ;; Trust levels
         trust-level?
         valid-trust-levels

         ;; Widget zones and kinds
         valid-widget-zones
         valid-widget-kinds

         ;; Validation
         (contract-out
          [make-widget-descriptor
           (->* (symbol? symbol?)
                (#:zone (or/c symbol? #f)
                        #:kind symbol?
                        #:content any/c
                        #:priority exact-integer?
                        #:capabilities (listof symbol?)
                        #:trust-level trust-level?)
                widget-descriptor?)]
          [widget-descriptor-valid? (-> any/c boolean?)]
          [widget-descriptors->sorted (-> (listof widget-descriptor?) (listof widget-descriptor?))]
          [filter-widgets-by-zone
           (-> (listof widget-descriptor?) symbol? (listof widget-descriptor?))]
          [filter-widgets-by-trust
           (-> (listof widget-descriptor?) symbol? (listof widget-descriptor?))]
          [widget-descriptor-matches-lifecycle? (-> widget-descriptor? symbol? boolean?)]))

;; ── Trust levels ───────────────────────────────────────────

(define valid-trust-levels '(core extension untrusted))

(define (trust-level? v)
  (and (symbol? v) (member v valid-trust-levels) #t))

;; ── Widget descriptor struct ───────────────────────────────

(struct widget-descriptor
        (id ; symbol — unique widget identifier
         extension-id ; symbol — owning extension
         zone ; symbol or #f — 'sidebar, 'toolbar, 'status, 'transcript, 'overlay
         kind ; symbol — 'text, 'html, 'component, 'custom
         content ; any — backend-specific content payload
         priority ; exact-integer — render order (lower = first)
         capabilities ; (listof symbol) — requested capabilities
         trust-level ; symbol — 'core, 'extension, 'untrusted
         lifecycle-token) ; symbol — unique token for lifecycle tracking
  #:transparent)

;; ── Constructor ─────────────────────────────────────────────

(define (make-widget-descriptor id
                                extension-id
                                #:zone [zone #f]
                                #:kind [kind 'text]
                                #:content [content ""]
                                #:priority [priority 100]
                                #:capabilities [capabilities '()]
                                #:trust-level [trust-level 'extension])
  (widget-descriptor id
                     extension-id
                     zone
                     kind
                     content
                     priority
                     capabilities
                     trust-level
                     (gensym 'widget-lifecycle)))

;; ── Validation ──────────────────────────────────────────────

(define valid-widget-zones '(sidebar toolbar status transcript overlay))
(define valid-widget-kinds '(text html component custom))

(define (widget-descriptor-valid? v)
  (and (widget-descriptor? v)
       (symbol? (widget-descriptor-id v))
       (symbol? (widget-descriptor-extension-id v))
       (or (not (widget-descriptor-zone v)) (member (widget-descriptor-zone v) valid-widget-zones))
       (member (widget-descriptor-kind v) valid-widget-kinds)
       (trust-level? (widget-descriptor-trust-level v))
       (exact-integer? (widget-descriptor-priority v))
       (list? (widget-descriptor-capabilities v))))

;; ── Sorting and filtering ──────────────────────────────────

(define (widget-descriptors->sorted descs)
  (sort descs < #:key widget-descriptor-priority))

(define (filter-widgets-by-zone descs zone)
  (filter (lambda (d) (eq? (widget-descriptor-zone d) zone)) descs))

(define (filter-widgets-by-trust descs level)
  (filter (lambda (d) (eq? (widget-descriptor-trust-level d) level)) descs))

;; ── Lifecycle token consumer ──────────────────────────────
;; m-8: Consume lifecycle token for widget identity tracking.

(define (widget-descriptor-matches-lifecycle? desc token)
  (eq? (widget-descriptor-lifecycle-token desc) token))
