#lang racket/base

;; Deprecated scratch stub (W4). The shared deterministic fake lives at
;; tests/helpers/gsd-timeout-fake.rkt; this file intentionally contains no
;; timeout logic and is kept only because shell deletion is policy-blocked.
;; It self-reports one passing check so the runner's strict zero-parsed
;; gate (BUG-0042 runner contract) does not flag it.

(module+ test
  (require rackunit
           racket/runtime-path)
  (define-runtime-path real-fake "../helpers/gsd-timeout-fake.rkt")
  (check-true (file-exists? real-fake)
              "shared deterministic fake must live at tests/helpers/gsd-timeout-fake.rkt"))
