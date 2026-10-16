#lang racket/base
(require rackunit "../extensions/gsd/delivery-journal.rkt")
(module+ test
  (check-false (journal-evidence-author-repair (hasheq)))
  (check-false (evidence-author-repair-pending? (hasheq))))
