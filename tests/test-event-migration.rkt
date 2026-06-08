#lang racket/base

;; @speed fast
;; @suite default
;; BOUNDARY: pure

;; tests/test-event-migration.rkt — Event migration registry tests (F6, W1)

(require rackunit
         rackunit/text-ui
         (only-in "../util/event/event-migration.rkt"
                  register-event-migration!
                  run-event-migrations!
                  lookup-event-migration
                  current-event-migration-registry)
         (only-in "../util/event/event-macro.rkt"
                  register-event-schema-version!
                  lookup-event-schema-version
                  with-fresh-event-registries))

(define migration-tests
  (test-suite "event-migration"

    (test-case "register and lookup migration"
      (define called? (box #f))
      (register-event-migration! "test-type"
                                 1
                                 (lambda (h)
                                   (set-box! called? #t)
                                   (hash-set h 'newField 'migrated)))
      (check-not-false (lookup-event-migration "test-type" 1))
      (check-false (lookup-event-migration "test-type" 2)))

    (test-case "run-event-migrations applies single migration"
      ;; Register type with schema version 2, and a migration from 1->2
      (register-event-schema-version! "mig-test-1" 2)
      (register-event-migration! "mig-test-1"
                                 1
                                 (lambda (h) (hash-set h 'addedField "from-migration")))
      (define h (hasheq 'type "mig-test-1" 'schemaVersion 1 'data "original"))
      (define result (run-event-migrations! "mig-test-1" h))
      (check-equal? (hash-ref result 'data) "original")
      (check-equal? (hash-ref result 'addedField) "from-migration")
      (check-equal? (hash-ref result 'schemaVersion) 2))

    (test-case "run-event-migrations chains multiple versions"
      (register-event-schema-version! "mig-test-2" 3)
      (register-event-migration! "mig-test-2" 1 (lambda (h) (hash-set h 'v2field "added-v2")))
      (register-event-migration! "mig-test-2" 2 (lambda (h) (hash-set h 'v3field "added-v3")))
      (define h (hasheq 'type "mig-test-2" 'schemaVersion 1 'data "original"))
      (define result (run-event-migrations! "mig-test-2" h))
      (check-equal? (hash-ref result 'v2field) "added-v2")
      (check-equal? (hash-ref result 'v3field) "added-v3")
      (check-equal? (hash-ref result 'schemaVersion) 3))

    (test-case "no-op when event version matches current"
      (register-event-schema-version! "mig-test-3" 1)
      (define h (hasheq 'type "mig-test-3" 'schemaVersion 1 'data "unchanged"))
      (define result (run-event-migrations! "mig-test-3" h))
      (check-equal? (hash-ref result 'data) "unchanged")
      (check-equal? (hash-ref result 'schemaVersion) 1))

    (test-case "missing migration still increments version"
      (register-event-schema-version! "mig-test-4" 3)
      ;; No migration registered for 1->2 or 2->3
      (define h (hasheq 'type "mig-test-4" 'schemaVersion 1 'data "original"))
      (define result (run-event-migrations! "mig-test-4" h))
      (check-equal? (hash-ref result 'data) "original")
      (check-equal? (hash-ref result 'schemaVersion) 3))))

(with-fresh-event-registries (run-tests migration-tests))
