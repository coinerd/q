#lang racket/base
;; agent/event-structs/memory-events.rkt — memory system audit events
;;
;; Events emitted when memory items are stored, retrieved, or deleted.
;; Used for auditing, observability, and session replay.
;;
;; NOTE: Constructor names use "make-" prefix to avoid collision with
;; the typed-event "type" field accessor (e.g., memory-stored-event-type).

(require "base.rkt"
         "../../util/event/event-macro.rkt"
         (only-in "../../util/event/event-macro.rkt" register-event-fields!))

;; ============================================================
;; Memory stored event
;; ============================================================

(define-typed-event mem-stored-event
                    "memory.stored"
                    (memory-id scope mem-type sensitivity content-length)
                    #:defaults (content-length 0)
                    #:schema-version 1)

;; ============================================================
;; Memory retrieved event
;; ============================================================

(define-typed-event mem-retrieved-event
                    "memory.retrieved"
                    (query-scope result-count query-limit)
                    #:defaults (result-count 0 query-limit 10)
                    #:schema-version 1)

;; ============================================================
;; Memory deleted event
;; ============================================================

(define-typed-event mem-deleted-event
                    "memory.deleted"
                    (memory-id scope)
                    #:defaults (scope #f)
                    #:schema-version 1)

;; ============================================================
;; Memory cleared event (batch delete)
;; ============================================================

(define-typed-event mem-cleared-event
                    "memory.cleared"
                    (scope deleted-count)
                    #:defaults (deleted-count 0)
                    #:schema-version 1)

(provide (struct-out mem-stored-event)
         make-mem-stored-event
         mem-stored-event-fields
         (struct-out mem-retrieved-event)
         make-mem-retrieved-event
         mem-retrieved-event-fields
         (struct-out mem-deleted-event)
         make-mem-deleted-event
         mem-deleted-event-fields
         (struct-out mem-cleared-event)
         make-mem-cleared-event
         mem-cleared-event-fields)
