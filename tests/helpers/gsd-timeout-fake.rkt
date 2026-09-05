#lang racket/base

;; q/tests/helpers/gsd-timeout-fake.rkt — deterministic GSD timeout seam fake
;; ( W4).
;;
;; ONE fake for the `current-gsd-timeout-now-ms` / `current-gsd-timeout-wait`
;; seam in extensions/gsd/system-adapters.rkt. It owns:
;;   - a fake millisecond clock advanced ONLY by the adapter's own wait calls
;;     (each wait advances by exactly the bound the adapter requested), and
;;   - a staged step list: each wait consumes the next stage, so the test
;;     dictates deterministically when completion/cancellation happens in
;;     fake time. No wall-clock deadline waits are ever paid.
;;
;; Stages (consumed one per wait call, in order):
;;   'tick                — pure timeout tick: advance the fake clock, poll
;;                          the event nonblockingly, return #f (keep waiting).
;;   (list 'quiet thunk)  — fire the staged action (typically posts a release
;;                          or flips cancellation), advance the clock, poll
;;                          the event nonblockingly.
;;   (list 'fire thunk)   — fire the staged action, advance the clock, then
;;                          give the real worker thread a small bounded
;;                          handshake window (default 0.25 s) on the event.
;;                          This is the only real time a green path pays, and
;;                          it is a hard cap, not a sleep-based expectation.
;;
;; A stage is ALWAYS consumed per call — including the adapter's
;; cancellation-grace wait — so the test can distinguish "runner cooperated
;; during grace" from "runner had to be killed after grace".
;;
;; If the stage list runs dry, later waits behave like 'tick (and advance the
;; clock by the requested bound), so an unexpected number of polls fails the
;; surrounding assertions instead of hanging.

(require racket/contract
         (only-in "../../extensions/gsd/system-adapters.rkt"
                  current-gsd-timeout-now-ms
                  current-gsd-timeout-wait))

(provide (contract-out (make-timeout-fake (->* ()
                                               (#:stages list?)
                                               (values (-> real?)
                                                       (-> (or/c evt? #f) (or/c real? #f) any/c)
                                                       (-> real?)
                                                       (-> exact-nonnegative-integer?))))
                       ;; Convenience: parameterize a thunk with a fresh fake clock + staged waits.
                       (with-deterministic-timeout (-> list? (-> any) any))))

;; Stage handshake bound: how long a green path may wait for a real worker
;; thread to observe a posted release/semaphore. Bounded by contract — never
;; an unbounded sleep.
(define stage-handshake-cap-sec 0.25)

(define (make-timeout-fake #:stages [stages '()])
  (define clock (box 0.0))
  (define stage-idx (box 0))
  (define wait-count (box 0))
  (define (now)
    (unbox clock))
  (define (consume-stage)
    (define i (unbox stage-idx))
    (set-box! stage-idx (add1 i))
    (and (< i (length stages)) (list-ref stages i)))
  (define (wait evt secs)
    (set-box! wait-count (add1 (unbox wait-count)))
    ;; Deterministic time: the fake clock advances by exactly the bound the
    ;; adapter requested (real semantics: wait at most `secs`).
    (when (and secs (real? secs))
      (set-box! clock (+ (unbox clock) (* secs 1000.0))))
    (define stage (consume-stage))
    (cond
      ; dry: behave like a pure tick
      [(not stage) (sync/timeout 0 evt)]
      [(eq? stage 'tick) (sync/timeout 0 evt)]
      [(and (list? stage) (= (length stage) 2) (memq (car stage) '(quiet fire)))
       ;; Fire the test's staged action, then poll (quiet) or allow one
       ;; bounded real handshake (fire) before reporting.
       ((cadr stage))
       (or (sync/timeout 0 evt)
           (and (eq? (car stage) 'fire) (sync/timeout stage-handshake-cap-sec evt)))]
      [else
       (raise-argument-error 'make-timeout-fake
                             "(or/c 'tick (list (or/c 'quiet 'fire) procedure?))"
                             stage)]))
  (values now wait (lambda () (unbox clock)) (lambda () (unbox wait-count))))

(define (with-deterministic-timeout stages thunk)
  (define-values (now wait _clock _waits) (make-timeout-fake #:stages stages))
  (parameterize ([current-gsd-timeout-now-ms now]
                 [current-gsd-timeout-wait wait])
    (thunk)))

(module+ main
  (void))
