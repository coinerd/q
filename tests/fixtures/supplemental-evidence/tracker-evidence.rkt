#lang racket/base
;; Pure witness: only a missing export becomes #f. Load/setup errors propagate.
(require racket/runtime-path
         "../../runtime/settings-core.rkt")
(provide valid-tracker-binding-normalizes?)
(define-runtime-path query "../../runtime/settings-query.rkt")
(define (valid-tracker-binding-normalizes?)
  (define normalize (dynamic-require query 'gsd-tracker-live-binding (lambda () #f)))
  (define settings
    (q-settings
     (hash)
     (hash)
     (hash 'gsd
           (hash 'tracker
                 (hash 'provider "local" 'board-id "fixture-board" 'project-id "fixture-project")))))
  (and normalize
       (equal? (normalize settings)
               (hash 'provider 'local 'board-id "fixture-board" 'project-id "fixture-project"))))
