#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @covers extensions/gsd/supplemental-evidence-recovery.rkt
(require rackunit
         (only-in "../extensions/gsd/supplemental-evidence-controller.rkt"
                  propose-supplemental-recovery!
                  install-supplemental-recovery!))
(module+ test
  (test-case "owner recovery API is explicit and separate from ordinary task authorization"
    (check-true (procedure-arity-includes? propose-supplemental-recovery! 5))
    (check-true (procedure-arity-includes? install-supplemental-recovery! 5))))
