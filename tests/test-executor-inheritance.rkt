#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-executor-inheritance.rkt — BUG-0018 W3 (R-B3)
;;
;; /go-spawned GSD executor sessions must inherit the coordinator's switched
;; provider/model when an explicit runtime /model override is present, and
;; must be untouched when it is not (v1.00.13 semantics preserved).

(require rackunit
         "../runtime/session/executor-inheritance.rkt")

(test-case "BUG-0018: executor inherits switched model + provider with override"
  (define rt-config (hasheq 'provider 'startup-provider 'model-name "glm-5.3" 'session-dir "/tmp/x"))
  (define prior-config (hasheq 'model-name "ark-code-latest" 'model-override #t))
  (define inherited
    (inherit-coordinator-runtime-config rt-config prior-config "ark-code-latest" 'switched-provider))
  (check-equal? (hash-ref inherited 'provider) 'switched-provider)
  (check-equal? (hash-ref inherited 'model-name) "ark-code-latest")
  ;; startup config object itself untouched
  (check-equal? (hash-ref rt-config 'model-name) "glm-5.3"))

(test-case "BUG-0018: no explicit override leaves executor config unchanged"
  (define rt-config (hasheq 'model-name "glm-5.3"))
  (define inherited
    (inherit-coordinator-runtime-config rt-config
                                        (hasheq 'model-name "ark-code-latest")
                                        "ark-code-latest"
                                        'switched-provider))
  (check-equal? inherited rt-config))

(test-case "BUG-0018: nil prior config leaves executor config unchanged"
  (define rt-config (hasheq 'model-name "glm-5.3"))
  (check-equal? (inherit-coordinator-runtime-config rt-config #f #f #f) rt-config))

(test-case "BUG-0018: session-config struct prior config is honored"
  ;; explicit-model-override? accepts a session-config via its data hash; the
  ;; helper normalizes both raw hashes and session-config structs.
  (define rt-config (hasheq 'model-name "glm-5.3"))
  (define cfg-hash (hasheq 'model-name "ark-code-latest" 'model-override #t))
  (check-not-equal? (inherit-coordinator-runtime-config rt-config cfg-hash "ark-code-latest" 'p)
                    rt-config))
