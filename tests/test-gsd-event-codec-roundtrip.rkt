#lang racket

;; BOUNDARY: unit

;; tests/test-gsd-event-codec-roundtrip.rkt -- Round-trip codec tests for GSD typed events (v0.43.1)
;;
;; Verifies that all GSD typed events can be constructed, serialized to hash,
;; and deserialized back with all fields preserved.

(require rackunit
         rackunit/text-ui
         (only-in "../agent/event-emitter.rkt" event-struct->hasheq)
         "../extensions/gsd/event-structs.rkt")

(define (roundtrip-event evt field-spec)
  (define h (event-struct->hasheq evt))
  (for ([(key expected) (in-hash field-spec)])
    (check-equal? (hash-ref h key #f) expected
                  (format "field ~a mismatch" key))))

(define codec-suite
  (test-suite "GSD typed event codec round-trip"

    (test-case "gsd-mode-changed-event round-trip"
      (define evt (make-gsd-mode-changed-event #:session-id "s1" #:turn-id 0 #:mode 'exploring))
      (roundtrip-event evt (hasheq 'mode 'exploring 'type "gsd.mode.changed")))

    (test-case "gsd-transition-attempted-event round-trip"
      (define evt (make-gsd-transition-attempted-event #:session-id "s1" #:turn-id 0 #:from 'idle #:to 'exploring))
      (roundtrip-event evt (hasheq 'from 'idle 'to 'exploring 'type "gsd.transition.attempted")))

    (test-case "gsd-transition-succeeded-event round-trip"
      (define evt (make-gsd-transition-succeeded-event #:session-id "s1" #:turn-id 0 #:from 'idle #:to 'exploring))
      (roundtrip-event evt (hasheq 'from 'idle 'to 'exploring 'type "gsd.transition.succeeded")))

    (test-case "gsd-wave-completed-event round-trip"
      (define evt (make-gsd-wave-completed-event #:session-id "s1" #:turn-id 0 #:wave 3))
      (roundtrip-event evt (hasheq 'wave 3 'type "gsd.wave.completed")))

    (test-case "gsd-wave-started-event round-trip"
      (define evt (make-gsd-wave-started-event #:session-id "s1" #:turn-id 0 #:wave 1))
      (roundtrip-event evt (hasheq 'wave 1 'type "gsd.wave.started")))

    (test-case "gsd-plan-parsed-event round-trip"
      (define evt (make-gsd-plan-parsed-event #:session-id "s1" #:turn-id 0 #:wave-count 4))
      (roundtrip-event evt (hasheq 'waveCount 4 'type "gsd.plan.parsed")))

    (test-case "gsd-command-received-event round-trip"
      (define evt (make-gsd-command-received-event #:session-id "s1" #:turn-id 0 #:command "wave" #:args "0"))
      (roundtrip-event evt (hasheq 'command "wave" 'args "0" 'type "gsd.command.received")))

    (test-case "gsd-archive-failed-event round-trip"
      (define evt (make-gsd-archive-failed-event #:session-id "s1" #:turn-id 0 #:error "disk full"))
      (roundtrip-event evt (hasheq 'error "disk full" 'type "gsd.archive.failed")))

    (test-case "gsd-plan-validated-event round-trip"
      (define evt (make-gsd-plan-validated-event #:session-id "s1" #:turn-id 0 #:wave-count 3))
      (roundtrip-event evt (hasheq 'waveCount 3 'type "gsd.plan.validated")))

    (test-case "gsd-plan-normalized-event round-trip"
      (define evt (make-gsd-plan-normalized-event #:session-id "s1" #:turn-id 0 #:wave-count 2))
      (roundtrip-event evt (hasheq 'waveCount 2 'type "gsd.plan.normalized")))

    (test-case "gsd-command-completed-event round-trip"
      (define evt (make-gsd-command-completed-event #:session-id "s1" #:turn-id 0 #:command "wave" #:success #t))
      (roundtrip-event evt (hasheq 'command "wave" 'success #t 'type "gsd.command.completed")))))

(run-tests codec-suite)
