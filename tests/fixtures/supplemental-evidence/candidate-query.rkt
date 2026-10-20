#lang racket/base
;; Local fixture implementation, not a historical campaign object or live proof.
(require "settings-core.rkt")
(provide fixture-query-version
         gsd-tracker-live-binding)
(define fixture-query-version 2)
(define (gsd-tracker-live-binding settings)
  (define tracker (hash-ref (hash-ref (q-settings-merged settings) 'gsd) 'tracker))
  (hash 'provider
        (string->symbol (hash-ref tracker 'provider))
        'board-id
        (hash-ref tracker 'board-id)
        'project-id
        (hash-ref tracker 'project-id)))
