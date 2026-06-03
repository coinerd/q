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

(provide (all-from-out "event-structs.rkt")
         (all-from-out "event-json.rkt")
         (all-from-out "../util/event/event-access.rkt")
         (contract-out [valid-typed-event-type? (-> string? boolean?)]))

;; Thin validation helper — contract-gated facade utility
(define (valid-typed-event-type? type-str)
  (and (member type-str (all-known-event-types)) #t))
