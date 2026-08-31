#lang reader "rktd-reader.rkt"

;; Wave evidence record for v1.00.23 W3
;; Deterministic FIFO/LPT ordering and duration fallback

((wave . "W3")
 (milestone . "v1.00.23")
 (branch . "campaign/7537743a3dcedebc6ff1e40ed7b601712b6ee26329cedc4e9bae627b4f06f327/w3-delivery")
 (verify-command . "racket tests/test-runner-scheduler-order.rkt && racket tests/test-run-tests-shard-plan.rkt && racket scripts/run-tests.rkt --suite fast --scheduler batch && racket scripts/run-tests.rkt --suite fast --scheduler queue")
 (verify-results
  ((test-runner-scheduler-order (tests . 21) (pass . 21) (fail . 0) (result . PASS))
   (test-run-tests-shard-plan (result . PASS))
   (fast-batch (files . 1162) (tests . 16808) (pass . 1162) (fail . 0) (result . PASS) (elapsed . "6m37s"))
   (fast-queue (files . 1162) (tests . 16808) (pass . 1162) (fail . 0) (result . PASS) (elapsed . "3m15s"))))
 (ordering-modes (default . fifo) (supported . (fifo lpt)))
 (ci-defaults-unchanged (scheduler . batch) (ordering . fifo) (v1.00.23 . true))
 (delivery-files
  ("q/scripts/run-tests/cli.rkt"
   "q/scripts/run-tests/runner.rkt"
   "q/scripts/run-tests/shard-plan.rkt"
   "q/scripts/run-tests/scheduler-order.rkt"
   "q/tests/test-runner-scheduler-order.rkt"
   "q/tests/test-run-tests-shard-plan.rkt"
   "q/docs/TEST_CONVENTIONS.md")))

; Follow-up PR validation: wave W3 evidence isolated for canonical governance checking.
