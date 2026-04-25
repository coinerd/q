#lang racket

;; tests/test-iteration-edge-cases.rkt — T08: Edge-case tests for iteration module
;;
;; Tests boundary conditions:
;; - ensure-hash-args edge cases
;; - check-mid-turn-budget! boundary behavior
;; - update-seen-paths steering counter reset
;; - v0.19.10: spiral breaker event structure

(require rackunit
         racket/set
         "../runtime/iteration.rkt"
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; ensure-hash-args edge cases
;; ============================================================

(test-case "ensure-hash-args passes through hash unchanged"
  (define h (hasheq 'a 1 'b 2))
  (check-equal? (ensure-hash-args h) h))

(test-case "ensure-hash-args parses JSON string"
  (define result (ensure-hash-args "{\"key\": \"value\"}"))
  (check-equal? (hash-ref result 'key) "value"))

(test-case "ensure-hash-args handles empty JSON object string"
  (define result (ensure-hash-args "{}"))
  (check-true (hash? result))
  (check-equal? (hash-count result) 0))

(test-case "ensure-hash-args handles empty string"
  (define result (ensure-hash-args ""))
  (check-true (hash? result))
  (check-equal? (hash-count result) 0))

(test-case "ensure-hash-args handles malformed JSON gracefully"
  (define result (ensure-hash-args "not json"))
  (check-true (hash-has-key? result '_parse_failed)))

;; ============================================================
;; check-mid-turn-budget! edge cases
;; ============================================================

(test-case "check-mid-turn-budget! passes when well under limit"
  (define bus (make-event-bus))
  (define config (hasheq 'max-context-tokens 1000))
  (check-not-exn (lambda () (check-mid-turn-budget! '() bus "test-session" config))))

(test-case "check-mid-turn-budget! uses default when no config key"
  (define bus (make-event-bus))
  (define config (hasheq))
  (check-not-exn (lambda () (check-mid-turn-budget! '() bus "test-session" config))))

;; ============================================================
;; update-seen-paths / steering counter reset tests
;; ============================================================

(test-case "update-seen-paths resets counter on write tool"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc1" "write" (hasheq 'path "/tmp/x"))) (set)))
  (check-equal? (set-count seen) 0 "seen-paths reset on write")
  (check-false inc? "should not increment on write"))

(test-case "update-seen-paths resets counter on planning-write (extension tool)"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc2" "planning-write" (hasheq))) (set)))
  (check-equal? (set-count seen) 0 "seen-paths reset on planning-write")
  (check-false inc? "should not increment on planning-write"))

(test-case "update-seen-paths resets counter on bash tool"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc3" "bash" (hasheq 'command "mkdir"))) (set)))
  (check-equal? (set-count seen) 0 "seen-paths reset on bash")
  (check-false inc? "should not increment on bash"))

(test-case "update-seen-paths increments on read tool with new path"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc4" "read" (hasheq 'path "/tmp/new.rkt"))) (set)))
  (check-true (set-member? seen "/tmp/new.rkt") "path added to seen set")
  (check-true inc? "should increment on new read path"))

(test-case "update-seen-paths resets counter on edit tool"
  (define-values (seen inc?) (update-seen-paths (list (make-tool-call "tc5" "edit" (hasheq))) (set)))
  (check-equal? (set-count seen) 0 "seen-paths reset on edit")
  (check-false inc? "should not increment on edit"))

(test-case "update-seen-paths resets counter on gh-issue (extension tool)"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc6" "gh-issue" (hasheq))) (set)))
  (check-equal? (set-count seen) 0 "seen-paths reset on gh-issue")
  (check-false inc? "should not increment on gh-issue"))

;; ============================================================
;; v0.19.10: Spiral breaker event structure tests
;; ============================================================

(test-case "event-bus: spiral events have correct structure"
  (define bus (make-event-bus))
  (define captured '())
  (subscribe! bus
              (lambda (evt) (set! captured (cons evt captured)))
              #:filter (lambda (e)
                         (and (event? e)
                              (member (event-ev e)
                                      '("spiral.error-warning" "spiral.bash-only-warning"
                                                               "spiral.bash-breaker")))))
  (emit-session-event! bus "s1" "spiral.error-warning" (hasheq 'consecutive-errors 7 'iteration 5))
  (emit-session-event! bus "s1" "spiral.bash-only-warning" (hasheq 'bash-count 12 'iteration 8))
  (emit-session-event! bus
                       "s1"
                       "spiral.bash-breaker"
                       (hasheq 'action "steering-injected" 'iteration 10))
  (check-equal? (length captured) 3 "should capture all 3 spiral events")
  (check-equal? (event-ev (car captured)) "spiral.bash-breaker")
  (check-equal? (event-ev (cadr captured)) "spiral.bash-only-warning")
  (check-equal? (event-ev (caddr captured)) "spiral.error-warning"))

(test-case "update-seen-paths: multiple tools in one call"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc1" "read" (hasheq 'path "/tmp/a.rkt"))
                             (make-tool-call "tc2" "read" (hasheq 'path "/tmp/b.rkt")))
                       (set)))
  (check-true (set-member? seen "/tmp/a.rkt"))
  (check-true (set-member? seen "/tmp/b.rkt"))
  (check-true inc? "should increment when any new read path"))

(test-case "update-seen-paths: read of already-seen path does not increment"
  (define-values (seen inc?)
    (update-seen-paths (list (make-tool-call "tc1" "read" (hasheq 'path "/tmp/old.rkt")))
                       (set "/tmp/old.rkt")))
  (check-true (set-member? seen "/tmp/old.rkt"))
  (check-false inc? "should not increment for already-seen path"))
