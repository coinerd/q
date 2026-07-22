#lang racket/base
;; agent/event-structs/memory-events.rkt — modular memory audit events
;;
;; Canonical event taxonomy from SPEC-v0.95.xx-MODULAR-MEMORY-SYSTEM.md.
;; Payloads are intentionally metadata/redacted-snippet only; raw memory content
;; must not be carried in events.
;;
;; M13-F12: Schema versioning policy:
;;   - MEMORY-EVENT-SCHEMA-VERSION tracks the current schema version.
;;   - Each typed event has #:schema-version N. When fields are added/renamed,
;;     bump the event's #:schema-version AND this constant.
;;   - Field removals are breaking; field additions are backward-compatible.
;;   - Consumers MUST tolerate unknown fields and MUST NOT assume field
;;     ordering. Consumers SHOULD check #:schema-version >= their minimum.

(require "base.rkt"
         "../../util/event/event-macro.rkt")

;; M13-F12: Schema version constant — bump when event fields change.
(define MEMORY-EVENT-SCHEMA-VERSION 2)

;; Before explicit/auto store.
(define-typed-event mem-store-requested-event
                    "memory.item.store.requested"
                    (candidate-id mem-type scope source)
                    #:defaults (source 'tool)
                    #:schema-version 1)

;; After search/retrieve/list.
(define-typed-event mem-retrieval-performed-event
                    "memory.retrieval.performed"
                    (query-snippet result-count query-limit scope latency-ms)
                    #:optional ([result-id #f] [matched-item-ids '()] [presence #f])
                    #:defaults (query-snippet "" result-count 0 query-limit 5 scope #f latency-ms 0)
                    #:schema-version 2)

;; After successful store.
(define-typed-event mem-item-stored-event
                    "memory.item.stored"
                    (memory-id mem-type scope source redacted-snippet)
                    #:defaults (source 'tool redacted-snippet "")
                    #:schema-version 1)

;; After delete/tombstone.
(define-typed-event mem-item-deleted-event
                    "memory.item.deleted"
                    (memory-id scope backend)
                    #:defaults (backend "unknown")
                    #:schema-version 1)

;; After successful update (M13-F5/W3).
(define-typed-event mem-item-updated-event
                    "memory.item.updated"
                    (memory-id scope source redacted-snippet)
                    #:defaults (source 'tool redacted-snippet "")
                    #:schema-version 1)

;; Policy block without raw content.
(define-typed-event mem-policy-blocked-event
                    "memory.policy.blocked"
                    (action reason source redacted-snippet)
                    #:defaults (source 'tool redacted-snippet "")
                    #:schema-version 1)

;; Backend absent/down.
(define-typed-event mem-backend-unavailable-event
                    "memory.backend.unavailable"
                    (backend action)
                    #:defaults (backend "none" action #f)
                    #:schema-version 1)

(provide MEMORY-EVENT-SCHEMA-VERSION
         (struct-out mem-store-requested-event)
         make-mem-store-requested-event
         mem-store-requested-event-fields
         (struct-out mem-item-stored-event)
         make-mem-item-stored-event
         mem-item-stored-event-fields
         (struct-out mem-item-deleted-event)
         make-mem-item-deleted-event
         mem-item-deleted-event-fields
         (struct-out mem-item-updated-event)
         make-mem-item-updated-event
         mem-item-updated-event-fields
         (struct-out mem-retrieval-performed-event)
         make-mem-retrieval-performed-event
         mem-retrieval-performed-event-fields
         (struct-out mem-policy-blocked-event)
         make-mem-policy-blocked-event
         mem-policy-blocked-event-fields
         (struct-out mem-backend-unavailable-event)
         make-mem-backend-unavailable-event
         mem-backend-unavailable-event-fields)
