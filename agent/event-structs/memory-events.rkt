#lang racket/base
;; agent/event-structs/memory-events.rkt — modular memory audit events
;;
;; Canonical event taxonomy from SPEC-v0.95.xx-MODULAR-MEMORY-SYSTEM.md.
;; Payloads are intentionally metadata/redacted-snippet only; raw memory content
;; must not be carried in events.

(require "base.rkt"
         "../../util/event/event-macro.rkt")

;; Before explicit/auto store.
(define-typed-event mem-store-requested-event
                    "memory.item.store.requested"
                    (candidate-id mem-type scope source)
                    #:defaults (source 'tool)
                    #:schema-version 1)

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

;; After search/retrieve/list.
(define-typed-event mem-retrieval-performed-event
                    "memory.retrieval.performed"
                    (query-snippet result-count query-limit scope latency-ms)
                    #:defaults (query-snippet "" result-count 0 query-limit 5 scope #f latency-ms 0)
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

;; Backward-compatible aliases for old code/tests during transition. These keep
;; constructor names available but canonical type strings are the SPEC names.
(define mem-stored-event-type mem-item-stored-event-type)
(define mem-retrieved-event-type mem-retrieval-performed-event-type)
(define mem-deleted-event-type mem-item-deleted-event-type)
(define mem-stored-event-fields mem-item-stored-event-fields)
(define mem-retrieved-event-fields mem-retrieval-performed-event-fields)
(define mem-deleted-event-fields mem-item-deleted-event-fields)
(define make-mem-stored-event make-mem-item-stored-event)
(define make-mem-retrieved-event make-mem-retrieval-performed-event)
(define make-mem-deleted-event make-mem-item-deleted-event)

(provide (struct-out mem-store-requested-event)
         make-mem-store-requested-event
         mem-store-requested-event-fields
         (struct-out mem-item-stored-event)
         make-mem-item-stored-event
         mem-item-stored-event-fields
         (struct-out mem-item-deleted-event)
         make-mem-item-deleted-event
         mem-item-deleted-event-fields
         (struct-out mem-retrieval-performed-event)
         make-mem-retrieval-performed-event
         mem-retrieval-performed-event-fields
         (struct-out mem-policy-blocked-event)
         make-mem-policy-blocked-event
         mem-policy-blocked-event-fields
         (struct-out mem-backend-unavailable-event)
         make-mem-backend-unavailable-event
         mem-backend-unavailable-event-fields
         ;; compatibility aliases
         mem-stored-event-type
         mem-retrieved-event-type
         mem-deleted-event-type
         mem-stored-event-fields
         mem-retrieved-event-fields
         mem-deleted-event-fields
         make-mem-stored-event
         make-mem-retrieved-event
         make-mem-deleted-event)
