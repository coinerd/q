#lang racket

;; @speed fast
;; @suite runtime
;; @boundary unit

;; tests/test-sse-phase-timeout-bounds.rkt
;; SS-4 (v1.00.12): regression guards for the SSE phase-timeout bounds.
;;
;; Locks the openai-compatible streaming phase-cap semantics after v1.00.05 W1
;; (#9393) widened all three stall windows to the raw `sse-read` config value,
;; producing the observed 600 s mid-content stall on deepseek-v4-flash
;; (session 01M0K9W1RKX28CC0BZZH22SB3J):
;;
;;   initial  = (min request-timeout 120)                     held-request detection
;;   thinking = (min request-timeout (min (or ov 120) 300))   slow-reasoning window
;;   content  = http-stream-timeout-default (60)              per-chunk gap
;;
;; W1 (#9429) landed `sse-phase-timeout-secs` / `max-thinking-gap-secs`, so the
;; guarded dynamic-require below resolves and the full matrix runs green.

(require rackunit
         (only-in "../llm/stream.rkt" http-stream-timeout-default))

;; ————————————————————————————————————————————————————————————
;; W1 resolver resolution (guarded: red until W1 lands)
;; ————————————————————————————————————————————————————————————

(define-values (sse-phase-timeout-secs max-thinking-gap-secs)
  (with-handlers ([exn:fail? (lambda (_) (values #f #f))])
    (values (dynamic-require '"../llm/stream.rkt" 'sse-phase-timeout-secs)
            (dynamic-require '"../llm/stream.rkt" 'max-thinking-gap-secs))))

(define resolver-landed? (and (procedure? sse-phase-timeout-secs) (real? max-thinking-gap-secs)))

;; ————————————————————————————————————————————————————————————
;; Helpers (only invoked once the resolver exists)
;; ————————————————————————————————————————————————————————————

(define (phase req ov)
  (call-with-values (lambda () (sse-phase-timeout-secs #:request-timeout req #:sse-read-override ov))
                    list))

(define overrides-sweep (list #f 1 60 120 299 300 301 600 100000))
(define requests-sweep (list 60 120 300 900))

;; ————————————————————————————————————————————————————————————
;; W0 red marker: fails (exit 1) until the resolver lands in W1
;; ————————————————————————————————————————————————————————————

(test-case "W1 resolver exports exist (red until v1.00.12 W1)"
  (check-true
   resolver-landed?
   "RED(W0): sse-phase-timeout-secs / max-thinking-gap-secs not exported by llm/stream.rkt yet"))

(when resolver-landed?
  ;; ————————————————————————————————————————————————————
  ;; Ceiling constant
  ;; ————————————————————————————————————————————————————
  (test-case "max-thinking-gap-secs ceiling constant is 300"
    (check-equal? max-thinking-gap-secs 300))

  ;; ————————————————————————————————————————————————————
  ;; SS-1/SS-2/SS-3: the timeout matrix
  ;; ————————————————————————————————————————————————————
  (test-case "SS-1/SS-2 regression: deepseek-style override cannot widen stall windows"
    ;; sse-read 600 with request 900 must NOT produce 600 s initial/content caps.
    (check-equal? (phase 900 600) (list 120 300 60)))

  (test-case "no override falls back to 120/120/60 defaults"
    (check-equal? (phase 900 #f) (list 120 120 60))
    (check-equal? (phase 300 #f) (list 120 120 60)))

  (test-case "SS-3: kimi/glm 300 s reasoning window preserved at the ceiling"
    (check-equal? (phase 900 300) (list 120 300 60))
    (check-equal? (phase 900 299) (list 120 299 60)))

  (test-case "tighter override honored for thinking only"
    (check-equal? (phase 900 90) (list 120 90 60))
    (check-equal? (phase 900 1) (list 120 1 60)))

  (test-case "small request budget clamps initial and thinking"
    (check-equal? (phase 100 600) (list 100 100 60))
    (check-equal? (phase 60 #f) (list 60 60 60)))

  (test-case "SS-2 invariant: content gap never exceeds 60 s"
    (for ([ov (in-list overrides-sweep)])
      (check-equal? (third (phase 900 ov)) 60)))

  (test-case "SS-1 invariant: initial never exceeds 120 s"
    (for ([ov (in-list overrides-sweep)])
      (check-true (<= (first (phase 900 ov)) 120)
                  (format "initial exceeded 120 for override ~a" ov))))

  (test-case "SS-3 invariant: thinking never exceeds min(request, 300)"
    (for* ([req (in-list requests-sweep)]
           [ov (in-list overrides-sweep)])
      (define result (phase req ov))
      (check-true (positive? (second result))
                  (format "thinking not positive for req=~a ov=~a" req ov))
      (check-true
       (<= (second result) (min req 300))
       (format "thinking ~a exceeds min(req,300) for req=~a ov=~a" (second result) req ov))))

  (test-case "all phase results positive across the full sweep"
    (for* ([req (in-list requests-sweep)]
           [ov (in-list overrides-sweep)])
      (define result (phase req ov))
      (check-true
       (and (positive? (first result)) (positive? (second result)) (positive? (third result)))
       (format "non-positive result for req=~a ov=~a: ~a" req ov result)))))
