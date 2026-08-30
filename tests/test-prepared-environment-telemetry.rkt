#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration

;; W1: prepared-environment state is normalized into CI evidence via an
;; explicit environment/input contract (Q_PREPARED_ENV_STATE /
;; Q_PREPARED_ENV_RESTORE_MS / Q_PREPARED_ENV_FALLBACK_MS). Local runs must
;; report `unavailable` and never invent `restored`/`rebuilt`.

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/port
         racket/system
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/reporting.rkt")

(define saved-state (getenv "Q_PREPARED_ENV_STATE"))
(define saved-legacy (getenv "Q_PREPARED_ENV_RESULT"))
(define saved-restore (getenv "Q_PREPARED_ENV_RESTORE_MS"))
(define saved-fallback (getenv "Q_PREPARED_ENV_FALLBACK_MS"))

(define (clear-env!)
  (for ([name '("Q_PREPARED_ENV_STATE" "Q_PREPARED_ENV_RESULT"
                                       "Q_PREPARED_ENV_RESTORE_MS"
                                       "Q_PREPARED_ENV_FALLBACK_MS")])
    (putenv name "")))

(define (restore-env!)
  (putenv "Q_PREPARED_ENV_STATE" (or saved-state ""))
  (putenv "Q_PREPARED_ENV_RESULT" (or saved-legacy ""))
  (putenv "Q_PREPARED_ENV_RESTORE_MS" (or saved-restore ""))
  (putenv "Q_PREPARED_ENV_FALLBACK_MS" (or saved-fallback "")))

(define suite
  (test-suite "prepared-environment telemetry"

    (test-case "local runs report unavailable, never invented restored"
      (dynamic-wind clear-env!
                    (lambda ()
                      (define pe (prepared-environment-state))
                      (check-equal? (hash-ref pe 'result) "unavailable")
                      (check-equal? (hash-ref pe 'restore_ms) 'null)
                      (check-equal? (hash-ref pe 'fallback_ms) 'null))
                    restore-env!))

    (test-case "restored carries restore elapsed time, no fallback"
      (dynamic-wind clear-env!
                    (lambda ()
                      (putenv "Q_PREPARED_ENV_STATE" "restored")
                      (putenv "Q_PREPARED_ENV_RESTORE_MS" "1234")
                      (define pe (prepared-environment-state))
                      (check-equal? (hash-ref pe 'result) "restored")
                      (check-equal? (hash-ref pe 'restore_ms) 1234)
                      (check-equal? (hash-ref pe 'fallback_ms) 'null))
                    restore-env!))

    (test-case "rebuilt carries restore and fallback elapsed time"
      (dynamic-wind clear-env!
                    (lambda ()
                      (putenv "Q_PREPARED_ENV_STATE" "rebuilt")
                      (putenv "Q_PREPARED_ENV_RESTORE_MS" "100")
                      (putenv "Q_PREPARED_ENV_FALLBACK_MS" "2500")
                      (define pe (prepared-environment-state))
                      (check-equal? (hash-ref pe 'result) "rebuilt")
                      (check-equal? (hash-ref pe 'restore_ms) 100)
                      (check-equal? (hash-ref pe 'fallback_ms) 2500))
                    restore-env!))

    (test-case "unknown/typo'd state values fall back to unavailable"
      (dynamic-wind clear-env!
                    (lambda ()
                      (putenv "Q_PREPARED_ENV_STATE" "invented")
                      (putenv "Q_PREPARED_ENV_RESTORE_MS" "500")
                      (define pe (prepared-environment-state))
                      (check-equal? (hash-ref pe 'result) "unavailable")
                      (check-equal? (hash-ref pe 'restore_ms) 500)
                      (check-equal? (hash-ref pe 'fallback_ms) 'null))
                    restore-env!))

    (test-case "legacy alias Q_PREPARED_ENV_RESULT still normalizes"
      (dynamic-wind clear-env!
                    (lambda ()
                      (putenv "Q_PREPARED_ENV_RESULT" "restored")
                      (putenv "Q_PREPARED_ENV_RESTORE_MS" "42")
                      (define pe (prepared-environment-state))
                      (check-equal? (hash-ref pe 'result) "restored")
                      (check-equal? (hash-ref pe 'restore_ms) 42))
                    restore-env!))

    (test-case "non-numeric timing values become null, not garbage"
      (dynamic-wind clear-env!
                    (lambda ()
                      (putenv "Q_PREPARED_ENV_STATE" "rebuilt")
                      (putenv "Q_PREPARED_ENV_RESTORE_MS" "abc")
                      (putenv "Q_PREPARED_ENV_FALLBACK_MS" "-3")
                      (define pe (prepared-environment-state))
                      (check-equal? (hash-ref pe 'result) "rebuilt")
                      (check-equal? (hash-ref pe 'restore_ms) 'null)
                      (check-equal? (hash-ref pe 'fallback_ms) 'null))
                    restore-env!))

    (test-case "prepared_environment is machine-readable in run-summary JSON"
      (define out (make-temporary-file "w1-preenv-json-~a.json" #f))
      (dynamic-wind clear-env!
                    (lambda ()
                      (putenv "Q_PREPARED_ENV_STATE" "rebuilt")
                      (putenv "Q_PREPARED_ENV_RESTORE_MS" "90")
                      (putenv "Q_PREPARED_ENV_FALLBACK_MS" "1800")
                      (write-json-results!
                       out
                       '()
                       #:suite 'testing
                       #:mode 'subprocess
                       #:elapsed-ms 10
                       #:ledger #f
                       #:profile 'local
                       #:shard #f
                       #:runner-version "1.0.23-test"
                       #:extra (let ([m (make-hasheq)])
                                 (hash-set! m 'prepared_environment (prepared-environment-state))
                                 m))
                      (define js (call-with-input-file out read-json))
                      (check-true (hash? js))
                      (define extra (hash-ref js 'extra))
                      (define pe (hash-ref extra 'prepared_environment))
                      (check-equal? (hash-ref pe 'result) "rebuilt")
                      (check-equal? (hash-ref pe 'restore_ms) 90)
                      (check-equal? (hash-ref pe 'fallback_ms) 1800))
                    (lambda ()
                      (restore-env!)
                      (delete-file out))))))

(run-tests suite)
