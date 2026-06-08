#lang racket

;; @speed fast
;; @suite default

;; Tests for wave-gate blocking in wave-executor.rkt
;; BOUNDARY: unit

(require rackunit
         racket/runtime-path)

(define-runtime-path wave-executor-path "../extensions/gsd/wave-executor.rkt")

(test-case "wave-gate-interval: default is 5"
  (define param (dynamic-require wave-executor-path 'wave-gate-interval))
  (check-equal? (param) 5))

(test-case "wave-gate-blocked?: returns #f when counter < interval"
  (define blocked? (dynamic-require wave-executor-path 'wave-gate-blocked?))
  (define counter (dynamic-require wave-executor-path 'wave-gate-counter))
  (define interval (dynamic-require wave-executor-path 'wave-gate-interval))
  (parameterize ([counter 0]
                 [interval 5])
    (check-false (blocked?)))
  (parameterize ([counter 4]
                 [interval 5])
    (check-false (blocked?))))

(test-case "wave-gate-blocked?: returns #t when counter >= interval"
  (define blocked? (dynamic-require wave-executor-path 'wave-gate-blocked?))
  (define counter (dynamic-require wave-executor-path 'wave-gate-counter))
  (define interval (dynamic-require wave-executor-path 'wave-gate-interval))
  (parameterize ([counter 5]
                 [interval 5])
    (check-true (blocked?)))
  (parameterize ([counter 10]
                 [interval 5])
    (check-true (blocked?))))

(test-case "wave-gate-clear!: resets counter to 0"
  (define clear! (dynamic-require wave-executor-path 'wave-gate-clear!))
  (define counter (dynamic-require wave-executor-path 'wave-gate-counter))
  (parameterize ([counter 7])
    (clear!)
    (check-equal? (counter) 0)))

(test-case "wave-gate-interval: configurable"
  (define interval (dynamic-require wave-executor-path 'wave-gate-interval))
  (parameterize ([interval 3])
    (check-equal? (interval) 3)))
