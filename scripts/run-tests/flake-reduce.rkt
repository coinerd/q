#lang racket/base

;; q/scripts/run-tests/flake-reduce.rkt — Bounded reduction of flake incidents
;;
;; v1.00.29 W3 (campaign v1.00.29): companion to flake-forensics.rkt. Given a
;; captured suite-context failure (a failing test plus its ordered predecessor
;; sequence), `reduce-incident` finds the smallest predecessor context that
;; still reproduces the failure. The whole protocol runs as pure steps over an
;; INJECTED run-predicate thunk — (lambda (predecessor-sequence) #t/#f) — so
;; tests drive the reducer without any real process execution.
;;
;; Protocol steps (each a pure function, exported individually):
;;   (a) step-exact-sequence   — does the full captured predecessor sequence
;;                               reproduce the failure?
;;   (b) step-standalone       — does the failing test fail with NO
;;                               predecessors (=> not suite-interference)?
;;   (c) step-delta-debug      — deterministic delta-debugging: repeatedly try
;;                               to remove halves/chunks of the predecessor
;;                               sequence while the failure still reproduces.
;;   (d) step-minimal-pair     — a one-element reduction is the minimal
;;                               A->failing pair.
;;   (e) step-expansion        — optional B->A->failing expansion: try
;;                               inserting each removed predecessor before the
;;                               minimal context and record what still fails.
;;   (f) cold-vs-warm-flag     — does the failure need cold vs warm compiled
;;                               state? (two injected thunks)
;;   (g) worker-isolation-flag — does the failure reproduce in an isolated
;;                               worker, only in the shared worker, both, or
;;                               neither? (two injected thunks)
;;
;; Determinism + budget discipline:
;;   - no sleeps, no wall-clock polling inside the search: time advances only
;;     through the injected #:now thunk (tests pin it);
;;   - hard budget: max-reruns counts run-predicate invocations and
;;     wall-clock-ms bounds elapsed time; when either is exhausted the
;;     reduction STOPS and reports 'unresolved with the evidence gathered so
;;     far. The reducer NEVER reports a verdict of "non-flaky".

(require racket/list
         racket/string)

(provide reduction-budget
         reduction-budget?
         make-reduction-budget
         reduction-budget-max-reruns
         reduction-budget-wall-clock-ms
         reduction-budget-created-at
         budget-tracker
         budget-tracker?
         make-budget-tracker
         budget-spend!
         budget-exhausted?
         budget-reruns-used
         step-exact-sequence
         step-standalone
         step-delta-debug
         step-minimal-pair
         step-expansion
         cold-vs-warm-flag
         worker-isolation-flag
         reduce-incident)

;; ============================================================
;; Budget
;; ============================================================

;; created-at is a wall-clock milliseconds snapshot taken at construction.
(struct reduction-budget (max-reruns wall-clock-ms created-at) #:transparent)

(define (make-reduction-budget [max-reruns 64] [wall-clock-ms 60000])
  (unless (and (integer? max-reruns) (positive? max-reruns))
    (raise-argument-error 'make-reduction-budget "positive integer" max-reruns))
  (unless (and (integer? wall-clock-ms) (positive? wall-clock-ms))
    (raise-argument-error 'make-reduction-budget "positive integer" wall-clock-ms))
  (reduction-budget max-reruns wall-clock-ms (current-inexact-milliseconds)))

(struct budget-tracker (budget start-ms spent) #:transparent #:mutable)

(define (make-budget-tracker budget #:now [now current-inexact-milliseconds])
  (budget-tracker budget (now) 0))

(define (budget-reruns-used tracker)
  (budget-tracker-spent tracker))

(define (budget-spend! tracker)
  (set-budget-tracker-spent! tracker (add1 (budget-tracker-spent tracker))))

(define (budget-exhausted? tracker #:now [now current-inexact-milliseconds])
  (define b (budget-tracker-budget tracker))
  (or (>= (budget-tracker-spent tracker) (reduction-budget-max-reruns b))
      (>= (- (now) (budget-tracker-start-ms tracker)) (reduction-budget-wall-clock-ms b))))

;; One counted predicate invocation: #f => never charged or run.
(define (run-counted run-pred sequence tracker #:now [now current-inexact-milliseconds])
  (and run-pred
       (not (budget-exhausted? tracker #:now now))
       (begin
         (budget-spend! tracker)
         (run-pred sequence))))

;; ============================================================
;; Protocol steps — pure functions over the run predicate
;; ============================================================

;; (a) exact predecessor sequence reproduction.
;; Returns #t / #f / 'budget-exhausted.
(define (step-exact-sequence run-pred predecessors tracker #:now [now current-inexact-milliseconds])
  (cond
    [(budget-exhausted? tracker #:now now) 'budget-exhausted]
    [else
     (budget-spend! tracker)
     (run-pred predecessors)]))

;; (b) standalone check: does the failing test fail with an EMPTY predecessor
;; context? Returns #t / #f / 'budget-exhausted.
(define (step-standalone run-pred tracker #:now [now current-inexact-milliseconds])
  (cond
    [(budget-exhausted? tracker #:now now) 'budget-exhausted]
    [else
     (budget-spend! tracker)
     (run-pred '())]))

;; Remove k consecutive elements starting at index i.
(define (remove-chunk lst i k)
  (append (take lst i) (drop lst (+ i k))))

;; Internal delta-debug: returns (values reduced-sequence interrupted?) —
;; interrupted? is #t when the search stopped early because the budget ran
;; out (the caller must then report 'unresolved instead of trusting the
;; half-reduced sequence).
(define (delta-debug/internal run-pred seq tracker #:now [now current-inexact-milliseconds])
  (define interrupted (box #f))
  (define reduced
    (let outer ([current seq]
                [chunk (max 1 (quotient (length seq) 2))])
      (cond
        [(and (<= (length current) 1) (<= chunk 1)) current]
        [(budget-exhausted? tracker #:now now)
         (set-box! interrupted #t)
         current]
        [else
         (define n (length current))
         (define k (min chunk n))
         ;; Scan chunks left to right; restart after every successful removal.
         ;; The bound tracks the CURRENT accumulator length: a successful
         ;; removal shrinks it, and remove-chunk must never over-run it.
         (define next
           (let scan ([i 0]
                      [acc current])
             (cond
               [(> i (- (length acc) k)) acc]
               [(budget-exhausted? tracker #:now now)
                (set-box! interrupted #t)
                acc]
               [else
                (define candidate (remove-chunk acc i k))
                (budget-spend! tracker)
                (if (run-pred candidate)
                    (scan 0 candidate)
                    (scan (+ i 1) acc))])))
         (cond
           [(not (equal? next current)) (outer next (max 1 (quotient (length next) 2)))]
           [(> k 1) (outer current (max 1 (quotient k 2)))]
           [else current])])))
  (values reduced (unbox interrupted)))

;; (c) delta-debug predecessors (remove halves). Returns the reduced
;; sequence; only ever returns a sequence that still reproduces.
(define (step-delta-debug run-pred seq tracker #:now [now current-inexact-milliseconds])
  (let-values ([(reduced _interrupted?) (delta-debug/internal run-pred seq tracker #:now now)])
    reduced))

;; (d) minimal single-predecessor pair: a one-element reduction is exactly
;; the A->failing pair. Pure classification over the reduced sequence.
(define (step-minimal-pair reduced-sequence)
  (and (= (length reduced-sequence) 1) (car reduced-sequence)))

;; (e) optional B->A->failing expansion: try inserting each removed
;; predecessor before the minimal context; record which still reproduce.
;; Stops at the budget; returns (hash 'tested _ 'reproduced _).
(define (step-expansion run-pred minimal removed tracker #:now [now current-inexact-milliseconds])
  (define tested '())
  (define reproduced '())
  (for ([candidate (in-list removed)]
        #:break (budget-exhausted? tracker #:now now))
    (define context (append (list candidate) minimal))
    (define result (run-counted run-pred context tracker #:now now))
    (set! tested (append tested (list candidate)))
    (when result
      (set! reproduced (append reproduced (list candidate)))))
  (hasheq 'tested tested 'reproduced reproduced))

;; (f) cold vs warm compiled-state flag over two injected thunks
;; (each answers "does the failing test fail under this compiled state?").
(define (cold-vs-warm-flag #:cold cold-run #:warm warm-run)
  (cond
    [(and cold-run warm-run (cold-run) (warm-run)) 'both]
    [(and cold-run (cold-run)) 'cold-only]
    [(and warm-run (warm-run)) 'warm-only]
    [else 'neither]))

;; (g) worker isolation flag over two injected thunks (each answers "does
;; the failing test fail under this worker placement?").
(define (worker-isolation-flag #:isolated isolated-run #:shared shared-run)
  (cond
    [(and isolated-run shared-run (isolated-run) (shared-run)) 'both]
    [(and isolated-run (isolated-run)) 'isolated-only]
    [(and shared-run (shared-run)) 'shared-only]
    [else 'neither]))

;; ============================================================
;; reduce-incident
;; ============================================================

;; Verdict vocabulary (there is deliberately NO "non-flaky" verdict):
;;   'minimal-single-predecessor — reduced to the single A->failing pair
;;   'minimal-multi-predecessor  — reduced to a smaller multi-element context
;;   'irreducible-full-sequence  — nothing could be removed
;;   'standalone-reproduces      — fails with no predecessors (not
;;                                 suite-interference); empty predecessor delta
;;   'not-reproduced             — even the exact captured sequence did not
;;                                 reproduce; empty predecessor delta; recorded
;;                                 as data, never as "non-flaky"
;;   'unresolved                 — budget exhausted before the reduction
;;                                 completed; evidence retained
(define (reduce-incident failing-test
                         predecessors
                         run-pred
                         #:budget [budget (make-reduction-budget)]
                         #:now [now current-inexact-milliseconds]
                         #:cold-run [cold-run #f]
                         #:warm-run [warm-run #f]
                         #:isolated-run [isolated-run #f]
                         #:shared-run [shared-run #f])
  (define tracker (make-budget-tracker budget #:now now))
  (define evidence '())
  (define (record! step result)
    (set! evidence (append evidence (list (hasheq 'step step 'result result)))))
  (define standalone-green 'unknown)
  (define exact-reproduced 'unknown)
  (define verdict 'unresolved)
  (define minimal '())
  (define expansion (hasheq 'tested '() 'reproduced '()))
  (define delta '())

  ;; (a) exact sequence
  (define exact (step-exact-sequence run-pred predecessors tracker #:now now))
  (cond
    [(eq? exact 'budget-exhausted)
     (set! verdict 'unresolved)
     (record! 'exact-sequence 'budget-exhausted)]
    [(not exact)
     ;; (a) did not reproduce: not even the captured context fails.
     (set! verdict 'not-reproduced)
     (set! exact-reproduced #f)
     (record! 'exact-sequence 'green)
     ;; (b) standalone check completes the picture: standalone green too.
     (define standalone (step-standalone run-pred tracker #:now now))
     ;; standalone-green = the failing test is GREEN on its own
     (set! standalone-green
           (cond
             [(eq? standalone 'budget-exhausted) 'unknown]
             [standalone #f]
             [else #t]))
     (record! 'standalone (if (eq? standalone 'budget-exhausted) 'budget-exhausted 'green))]
    [else
     (set! exact-reproduced #t)
     (record! 'exact-sequence 'reproduced)
     ;; (b) standalone check
     (define standalone (step-standalone run-pred tracker #:now now))
     (cond
       [(eq? standalone 'budget-exhausted)
        (set! verdict 'unresolved)
        (record! 'standalone 'budget-exhausted)]
       [standalone
        ;; fails standalone: not suite-interference; empty predecessor delta.
        (set! standalone-green #f)
        (set! verdict 'standalone-reproduces)
        (record! 'standalone 'reproduced)]
       [else
        (set! standalone-green #t)
        (record! 'standalone 'green)
        ;; (c) delta-debug
        (define-values (reduced dd-interrupted)
          (delta-debug/internal run-pred predecessors tracker #:now now))
        (set! minimal reduced)
        (set! delta
              (for/list ([p (in-list predecessors)]
                         #:unless (member p reduced))
                p))
        (cond
          [dd-interrupted
           (set! verdict 'unresolved)
           (record! 'delta-debug 'budget-exhausted)]
          [else
           (record! 'delta-debug reduced)
           (set! verdict
                 (cond
                   [(step-minimal-pair reduced) 'minimal-single-predecessor]
                   [(= (length reduced) (length predecessors)) 'irreducible-full-sequence]
                   [else 'minimal-multi-predecessor]))
           ;; (e) optional B->A->failing expansion (only with budget left)
           (when (and (pair? delta) (not (budget-exhausted? tracker #:now now)))
             (set! expansion (step-expansion run-pred reduced delta tracker #:now now))
             (record! 'expansion expansion))])])])

  ;; (f)/(g) optional environment flags
  (define cwf
    (if (and cold-run warm-run)
        (cold-vs-warm-flag #:cold cold-run #:warm warm-run)
        'not-probed))
  (define wif
    (if (and isolated-run shared-run)
        (worker-isolation-flag #:isolated isolated-run #:shared shared-run)
        'not-probed))

  (hasheq 'failing-test
          (if (string? failing-test)
              failing-test
              (format "~a" failing-test))
          'verdict
          (symbol->string verdict)
          'minimal-predecessors
          minimal
          'predecessor-delta
          delta
          'standalone-green
          standalone-green
          'exact-sequence-reproduced
          exact-reproduced
          'expansion
          expansion
          'cold-warm-flag
          (symbol->string cwf)
          'worker-isolation-flag
          (symbol->string wif)
          'budget-exhausted
          (budget-exhausted? tracker #:now now)
          'reruns-used
          (budget-reruns-used tracker)
          'evidence
          evidence))
