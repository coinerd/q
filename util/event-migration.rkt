#lang racket/base

;; util/event-migration.rkt — Per-type event migration registry (F6, W1)
;; STABILITY: evolving
;;
;; Provides a migration function registry keyed by (type-string . from-version).
;; When deserializing events, the codec can run migrations sequentially to
;; bring an old event hash to the current schema version.

(require (only-in "event-macro.rkt" lookup-event-schema-version current-schema-version))

(provide register-event-migration!
         run-event-migrations!
         lookup-event-migration
         current-event-migration-registry)

;; Migration registry: (cons type-string from-version) -> (hash -> hash)
(define current-event-migration-registry (make-parameter (make-hash)))

(define (register-event-migration! type-str from-version mig-fn)
  (hash-set! (current-event-migration-registry) (cons type-str from-version) mig-fn))

(define (lookup-event-migration type-str from-version)
  (hash-ref (current-event-migration-registry) (cons type-str from-version) #f))

(define (run-event-migrations! type-str h)
  ;; Run all migrations from the event's version to the current per-type version.
  ;; h must contain 'schemaVersion key.
  (define event-ver (hash-ref h 'schemaVersion 1))
  (define target-ver (lookup-event-schema-version type-str))
  (for/fold ([h h]) ([v (in-range event-ver target-ver)])
    (define mig (lookup-event-migration type-str v))
    (if mig
        (let ([migrated (mig h)]) (hash-set migrated 'schemaVersion (add1 v)))
        (hash-set h 'schemaVersion (add1 v)))))
