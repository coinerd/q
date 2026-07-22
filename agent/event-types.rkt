#lang racket/base

;; agent/event-types.rkt — facade re-exporting event-structs + event-json
;;
;; Wave 0: Issues #1308 + #1311
;;
;; Defines typed Racket structs for each hook point event category,
;; replacing raw hash payloads with structured, contract-validated types.
;;
;; This file is a backward-compatible facade. All definitions live in:
;;   - event-structs.rkt  — struct definitions and constructors
;;   - event-json.rkt     — JSON serialization and registry

(require racket/contract
         "event-structs.rkt"
         "event-json.rkt"
         "../util/event/event-access.rkt")

;; NOTE: (all-from-out "event-structs.rkt") is intentional — this file is
;; a backward-compatible facade. event-structs.rkt provides ~200 explicit
;; identifiers across all event categories. Listing them all here would
;; duplicate event-structs.rkt's provide list and create maintenance burden.
;; ADR-0028 (no all-from-out) is relaxed for facade re-exports.
(provide (all-from-out "event-structs.rkt")
         typed-event->jsexpr
         jsexpr->typed-event
         all-known-event-types
         event-name->tool-name
         register-tool-event-serializer!
         event-type-ref
         event-timestamp-ref
         event-session-id-ref
         event-turn-id-ref
         event-payload-ref
         event?
         make-event
         (contract-out [valid-typed-event-type? (-> string? boolean?)]))

;; Thin validation helper — contract-gated facade utility
(define (valid-typed-event-type? type-str)
  (and (member type-str (all-known-event-types)) #t))
