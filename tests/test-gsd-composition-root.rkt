#lang racket
;; @speed fast
;; @suite extensions

(require rackunit
         racket/runtime-path
         "../extensions/gsd/effect-ports.rkt"
         "../extensions/gsd/composition-root.rkt"
         "helpers/gsd-port-fakes.rkt")

(define-runtime-path effect-ports-source "../extensions/gsd/effect-ports.rkt")

(module+ test
  (test-case "default composition produces valid cohesive ports"
    (define ports system-gsd-effect-ports)
    (check-true (gsd-effect-ports? ports))
    (check-true (gsd-filesystem-port? (gsd-effect-ports-filesystem ports)))
    (check-true (gsd-git-port? (gsd-effect-ports-git ports)))
    (check-true (gsd-clock-port? (gsd-effect-ports-clock ports)))
    (check-true (gsd-process-port? (gsd-effect-ports-process ports)))
    (check-true (procedure? (gsd-effect-ports-event-sink ports))))

  (test-case "current composition supports dynamic DI and restores afterward"
    (define saved (current-gsd-effect-ports))
    (define-values (fake _state) (make-fake-gsd-effect-ports))
    (parameterize ([current-gsd-effect-ports fake])
      (check-eq? (current-gsd-effect-ports) fake))
    (check-eq? (current-gsd-effect-ports) saved))

  (test-case "two fake compositions do not share mutable state"
    (define-values (a a-state) (make-fake-gsd-effect-ports))
    (define-values (b b-state) (make-fake-gsd-effect-ports))
    ((gsd-effect-ports-event-sink a) 'gsd.wave.started #hasheq((wave . 0)))
    (check-equal? (length (fake-gsd-state-events a-state)) 1)
    (check-equal? (fake-gsd-state-events b-state) '())
    (check-not-eq? a b))

  (test-case "stable facades do not export internal ports or composition root"
    (define facade (build-path "extensions" "gsd-planning.rkt"))
    (define core (build-path "extensions" "gsd" "core.rkt"))
    (for ([module-path (in-list (list facade core))]
          [name (in-cycle '(gsd-effect-ports current-gsd-effect-ports))])
      (check-exn exn:fail? (lambda () (dynamic-require module-path name)))))

  (test-case "neutral contracts carry no concrete adapter dependencies"
    (define source (file->string effect-ports-source))
    (check-false (regexp-match?
                  #rx"require[^\n]*(?:runtime/|sandbox/|racket/file|racket/system|net/|github)"
                  source))))
