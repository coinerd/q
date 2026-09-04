#lang racket/base

;; W7 characterization fixture: NO (module+ test ...) form at all (pure
;; definitions script). A grouped request must fall back with reason
;; missing-module-plus-test-form.
(define always-two 2)
(provide always-two)
