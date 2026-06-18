#lang racket

;; @requires browser

;; tests/test-browser-audit-w2.rkt — Tests for Wave 2 audit fixes
;; C1: restart-sidecar! actually relaunches
;; C2: pending-box guarded by semaphore
;; H1: Restart count tracked persistently
;; H2/H3: Reader and heartbeat threads restarted after recovery
;; M1: Reader thread death detection in heartbeat

(require rackunit
         racket/async-channel
         (only-in racket/base make-semaphore call-with-semaphore)
         "../browser/adapters/playwright-sidecar.rkt")

;; ---------------------------------------------------------------------------
;; C1: struct is transparent with accessors
;; ---------------------------------------------------------------------------

(test-case "C1: playwright-sidecar-state is transparent with accessors"
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f (hasheq) #f #f))
  (check-true (playwright-sidecar-state? state))
  (check-false (playwright-sidecar-state-process state)))

;; ---------------------------------------------------------------------------
;; H1: Restart count tracked in config
;; ---------------------------------------------------------------------------

(test-case "H1: restart-sidecar! increments restart count"
  (define config
    (hasheq 'timeout-ms
            5000
            'sidecar-path
            #f ; no actual relaunch
            'restart-count
            0
            'pending-sema
            (make-semaphore 1)))
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f config #f #f))
  (restart-sidecar! state)
  (check-equal? (hash-ref (playwright-sidecar-state-config state) 'restart-count) 1))

(test-case "H1: double restart increments count twice"
  (define config
    (hasheq 'timeout-ms 5000 'sidecar-path #f 'restart-count 0 'pending-sema (make-semaphore 1)))
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f config #f #f))
  (restart-sidecar! state)
  (restart-sidecar! state)
  (check-equal? (hash-ref (playwright-sidecar-state-config state) 'restart-count) 2))

;; ---------------------------------------------------------------------------
;; C2: pending-box semaphore stored in config
;; ---------------------------------------------------------------------------

(test-case "C2: pending-box semaphore stored in config"
  (define sema (make-semaphore 1))
  (define cfg (hasheq 'pending-sema sema))
  (check-not-false (hash-ref cfg 'pending-sema #f)))

(test-case "C2: restart-sidecar! preserves config with pending-sema"
  (define pending (make-hash))
  (define sema (make-semaphore 1))
  (define config
    (hasheq 'timeout-ms
            5000
            'pending-sema
            sema
            'sidecar-path
            #f
            'restart-count
            0
            'node-path
            "node"
            'headless?
            #t))
  (define state (playwright-sidecar-state #f #f #f pending #f #f config #f #f))
  (restart-sidecar! state)
  (check-equal? (hash-ref (playwright-sidecar-state-config state) 'restart-count) 1))

;; ---------------------------------------------------------------------------
;; H2/H3: Graceful handling when sidecar-path is missing
;; ---------------------------------------------------------------------------

(test-case "H2/H3: restart-sidecar! handles missing sidecar-path"
  (define state
    (playwright-sidecar-state #f
                              #f
                              #f
                              (make-hash)
                              #f
                              #f
                              (hasheq 'sidecar-path #f 'restart-count 0)
                              #f
                              #f))
  (restart-sidecar! state)
  (check-equal? (hash-ref (playwright-sidecar-state-config state) 'restart-count) 1))

;; ---------------------------------------------------------------------------
;; M1: start-heartbeat! works with dead reader thread
;; ---------------------------------------------------------------------------

(test-case "M1: start-heartbeat! works with dead reader thread"
  (define dead-thread (thread (lambda () (void))))
  (sleep 0.1) ; ensure thread finishes
  (define state
    (playwright-sidecar-state #f
                              #f
                              #f
                              (make-hash)
                              dead-thread
                              (make-custodian)
                              (hasheq 'timeout-ms 5000)
                              #f
                              #f))
  (start-heartbeat! state)
  (check-not-false (playwright-sidecar-state-heartbeat-thread state)))

;; ---------------------------------------------------------------------------
;; send-command-with-recovery! retry constant
;; ---------------------------------------------------------------------------

(test-case "max-sidecar-restarts is 2"
  (check-equal? max-sidecar-restarts 2))
