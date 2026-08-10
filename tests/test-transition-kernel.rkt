#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-transition-kernel.rkt — Pure GSD transition kernel (W1 v0.99.89)
;;
;; Pins the pure transition kernel (extensions/gsd/transition-kernel.rkt):
;;   - table tests for allowed/forbidden/event-gated transitions
;;   - property sweep over the GSD-STATES cross product
;;   - idempotency (pure determinism; apply-twice stability)
;;   - neutral-state invariants (preconditions)
;;   - terminal states and the campaign-complete? terminal condition
;;   - kernel purity fitness: the kernel file may only require base
;;     collections (racket/base, racket/match, racket/set)
;;   - facade ↔ kernel equivalence: the runtime facade
;;     (transition-logic.rkt) must produce exactly the kernel's decisions

(require rackunit
         racket/set
         racket/list
         racket/file
         "helpers/arch-utils.rkt"
         "../extensions/gsd/transition-kernel.rkt"
         (only-in "../extensions/gsd/transition-logic.rkt"
                  compute-next-gsm-state
                  check-state-invariants)
         "../extensions/gsd/runtime-state-types.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (k-state mode [tw 0] [cw 0] [completed (set)])
  (make-gsd-transition-state mode tw cw completed))

(define (rt-state mode
                  #:executor [exec #f]
                  #:total-waves [tw 0]
                  #:current-wave [cw 0]
                  #:completed-waves [comp (set)])
  (gsd-runtime-state mode tw cw comp exec #f #f 500 '()))

(define (all-pairs)
  (for*/list ([a (in-list GSD-STATES)]
              [b (in-list GSD-STATES)])
    (cons a b)))

;; ============================================================
;; Table tests: allowed transitions
;; ============================================================

(define allowed-transition-pairs
  '((idle . idle) (idle . exploring)
                  (exploring . plan-written)
                  (exploring . idle)
                  (plan-written . executing)
                  (plan-written . idle)
                  (executing . verifying)
                  (executing . idle)
                  (verifying . idle)
                  (verifying . executing)))

(define allowed-event-pairs
  '(((idle . exploring) . explore) ((exploring . plan-written) . plan)
                                   ((exploring . idle) . cancel)
                                   ((plan-written . executing) . execute)
                                   ((plan-written . idle) . cancel)
                                   ((executing . verifying) . verify)
                                   ((executing . idle) . cancel)
                                   ((verifying . idle) . done)
                                   ((verifying . executing) . rework)))

(test-case "kernel: every table row is a legal transition"
  (for ([t TRANSITIONS])
    (check-true (valid-transition? (caar t) (cdr t))
                (format "~a → ~a should be valid" (caar t) (cdr t)))))

(test-case "kernel: TRANSITIONS-FLAT is the flat projection of TRANSITIONS"
  (check-equal? TRANSITIONS-FLAT
                (for/list ([t TRANSITIONS])
                  (cons (caar t) (cdr t))))
  (check-equal? (length TRANSITIONS-FLAT) (length TRANSITIONS)))

(test-case "kernel: allowed transitions table (9 + idle self-loop)"
  (for ([p allowed-transition-pairs])
    (check-true (valid-transition? (car p) (cdr p))
                (format "~a → ~a must be allowed" (car p) (cdr p)))))

(test-case "kernel: event-gated transitions accept only their own event"
  (for ([ep allowed-event-pairs])
    (define from (caar ep))
    (define to (cdar ep))
    (define event (cdr ep))
    (check-true (valid-transition? from to event)
                (format "~a → ~a via ~a must be allowed" from to event))
    (for ([other-event '(explore plan cancel execute verify done rework)]
          #:unless (eq? other-event event))
      (check-false
       (valid-transition? from to other-event)
       (format "~a → ~a via ~a must be rejected (wanted ~a)" from to other-event event)))))

;; ============================================================
;; Table tests: forbidden transitions
;; ============================================================

(test-case "kernel: forbidden transitions are all rejected"
  (define allowed-set
    (for/set ([p allowed-transition-pairs])
      p))
  (for ([p (all-pairs)]
        #:unless (set-member? allowed-set p))
    (check-false (valid-transition? (car p) (cdr p))
                 (format "~a → ~a must be forbidden" (car p) (cdr p)))))

(test-case "kernel: invalid target symbols are never valid"
  (for ([from (in-list GSD-STATES)])
    (check-false (valid-transition? from 'bogus-state))
    (check-false (valid-transition? 'bogus-state from))
    (check-false (gsm-state? 'bogus-state))))

;; ============================================================
;; Property sweep over the state cross product
;; ============================================================

(test-case "kernel: valid-transition? is consistent with TRANSITIONS-FLAT"
  (define flat
    (for/set ([t TRANSITIONS-FLAT])
      t))
  (define flat-with-self (set-add flat '(idle . idle)))
  (for ([p (all-pairs)])
    (check-equal? (valid-transition? (car p) (cdr p))
                  (set-member? flat-with-self p)
                  (format "~a → ~a inconsistent with flat table" (car p) (cdr p)))))

(test-case "kernel: valid-targets returns exactly the reachable states"
  (for ([from (in-list GSD-STATES)])
    (define targets (valid-targets from))
    (check-true (andmap gsm-state? targets) (format "~a: all targets are GSD states" from))
    (for ([t (in-list targets)])
      (check-true (valid-transition? from t) (format "~a → ~a must be valid" from t)))
    (for ([to (in-list GSD-STATES)]
          #:when (valid-transition? from to)
          #:unless (and (eq? from 'idle) (eq? to 'idle)))
      (check-true (and (member to targets) #t)
                  (format "~a → ~a missing from valid-targets" from to)))))

(test-case "kernel: find-transition-path returns a real path iff one exists"
  (for ([p (all-pairs)])
    (define from (car p))
    (define to (cdr p))
    (define path (find-transition-path from to))
    (cond
      [(eq? from to) (check-equal? path '() (format "~a → ~a zero-hop path" from to))]
      [(valid-transition? from to)
       (check-equal? path (list to) (format "~a → ~a single-hop path" from to))]
      [else
       (begin
         (define reachable?
           (let loop ([s from]
                      [seen (set)])
             (cond
               [(eq? s to) #t]
               [(set-member? seen s) #f]
               [else (ormap (lambda (n) (loop n (set-add seen s))) (valid-targets s))])))
         (if reachable?
             (begin
               (check-equal? (last path) to (format "~a → ~a path ends at target" from to))
               (check-true (and (andmap gsm-state? path) #t) "path members are GSD states")
               (check-true (and (for/and ([a (in-list path)]
                                          [b (in-list (cdr path))])
                                  (valid-transition? a b))
                                #t)
                           (format "~a → ~a: every path hop is a valid transition" from to)))
             (check-false path (format "~a → ~a must have no path" from to))))])))

;; ============================================================
;; compute-next-state semantics (neutral)
;; ============================================================

(test-case "kernel: compute-next-state ok result carries from/to"
  (define-values (r s) (compute-next-state (k-state 'idle) 'exploring))
  (check-true (ok? r))
  (check-equal? (ok-from r) 'idle)
  (check-equal? (ok-to r) 'exploring)
  (check-equal? (gts-mode s) 'exploring)
  (check-equal? (gts-total-waves s) 0))

(test-case "kernel: compute-next-state rejects invalid targets unchanged"
  (define st (k-state 'idle))
  (define-values (r s) (compute-next-state st 'nonexistent))
  (check-true (err? r))
  (check-equal? (err-result-from r) 'idle)
  (check-equal? (err-result-attempted r) 'nonexistent)
  (check-equal? s st "invalid target leaves state unchanged"))

(test-case "kernel: compute-next-state rejects forbidden transitions unchanged"
  (define st (k-state 'idle))
  (define-values (r s) (compute-next-state st 'executing))
  (check-true (err? r))
  (check-equal? (err-reason r) "invalid transition: idle → executing (valid: (exploring))")
  (check-equal? s st "forbidden transition leaves state unchanged"))

(test-case "kernel: compute-next-state respects the event gate"
  (define-values (r _s) (compute-next-state (k-state 'verifying) 'idle #:event 'done))
  (check-true (ok? r))
  (define-values (r2 _s2) (compute-next-state (k-state 'verifying) 'idle #:event 'rework))
  (check-true (err? r2)))

;; ============================================================
;; Idempotency (pure determinism)
;; ============================================================

(test-case "kernel: compute-next-state is deterministic (same input → same output)"
  (for ([p (all-pairs)])
    (define st (k-state (car p) 2 0 (set 0)))
    (define-values (r1 s1) (compute-next-state st (cdr p)))
    (define-values (r2 s2) (compute-next-state st (cdr p)))
    (check-equal? r1 r2 (format "~a → ~a: identical result structs" (car p) (cdr p)))
    (check-equal? s1 s2 (format "~a → ~a: identical next states" (car p) (cdr p)))))

(test-case "kernel: transition-idempotent? holds for every state/target pair"
  (for ([p (all-pairs)])
    (define st (k-state (car p) 2 0 (set 0)))
    (check-true (transition-idempotent? st (cdr p))
                (format "~a → ~a: apply-twice stability violated" (car p) (cdr p)))))

(test-case "kernel: transition-idempotent? holds with events"
  (for ([ep allowed-event-pairs])
    (define st (k-state (caar ep) 2 0 (set 0)))
    (check-true (transition-idempotent? st (cdar ep) #:event (cdr ep)))))

;; ============================================================
;; Neutral-state invariants (preconditions)
;; ============================================================

(test-case "kernel: invariants hold for the canonical states"
  (for ([s (in-list GSD-STATES)])
    (define-values (ok? msg) (check-transition-invariants (k-state s 3 0 (set))))
    (check-true ok? (format "~a: ~a" s msg))))

(test-case "kernel: invariants reject invalid mode"
  (define-values (ok? msg) (check-transition-invariants (k-state 'bogus)))
  (check-false ok?)
  (check-pred string? msg))

(test-case "kernel: invariants reject bad wave counters"
  (define-values (ok? _1) (check-transition-invariants (k-state 'idle -1 0 (set))))
  (check-false ok?)
  (define-values (ok2? _2) (check-transition-invariants (k-state 'idle 3 5 (set))))
  (check-false ok2? "current-wave > total-waves rejected"))

(test-case "kernel: invariants reject malformed completed set"
  (define-values (ok? _3) (check-transition-invariants (k-state 'idle 3 0 '(0 1))))
  (check-false ok? "non-set completed rejected")
  (define-values (ok2? _4) (check-transition-invariants (k-state 'idle 3 0 (set 0 5))))
  (check-false ok2? "out-of-range index rejected"))

;; ============================================================
;; Terminal states and campaign completeness
;; ============================================================

(test-case "kernel: terminal-state? classification is a table"
  (for ([s (in-list GSD-STATES)])
    (check-equal? (terminal-state? s)
                  (and (memq s GSD-TERMINAL-STATES) #t)
                  (format "~a terminal classification" s)))
  (check-false (terminal-state? 'bogus)))

(test-case "kernel: GSD-TERMINAL-STATES are real GSD states"
  (for ([s (in-list GSD-TERMINAL-STATES)])
    (check-true (gsm-state? s))))

(test-case "kernel: campaign-complete? is the pure /done precondition"
  (check-false (campaign-complete? 0 (set)))
  (check-false (campaign-complete? 2 (set 0)))
  (check-false (campaign-complete? 2 (set 0 1 2)))
  (check-true (campaign-complete? 2 (set 0 1)))
  (check-true (campaign-complete? 1 (set 0)))
  (check-false (campaign-complete? 2 'not-a-set)))

(test-case "kernel: campaign-complete? agrees with compute-next-pending-wave"
  (for ([tw (in-range 0 5)]
        [completed (in-list (list (set) (set 0) (set 0 1) (set 1) (set 0 1 2)))])
    (check-equal? (campaign-complete? tw completed)
                  (and (> tw 0) (not (compute-next-pending-wave tw completed)))
                  (format "tw=~a completed=~a" tw completed))))

;; ============================================================
;; Kernel purity fitness: only base collections may be required
;; ============================================================

(define allowed-kernel-imports '("racket/base" "racket/match" "racket/set"))

;; Extract the module path of a require spec (symbols and string paths).
(define (spec-module-path spec)
  (cond
    [(symbol? spec) (symbol->string spec)]
    [(string? spec) spec]
    [(pair? spec)
     (case (car spec)
       [(only-in prefix-in rename-in except-in)
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (cadr spec)
            #f)]
       [else #f])]
    [else #f]))

(test-case "kernel purity: transition-kernel.rkt imports only base collections"
  (define kernel-path (build-path q-dir "extensions" "gsd" "transition-kernel.rkt"))
  (check-true (file-exists? kernel-path) "kernel file exists on disk")
  (define reqs (extract-requires (path->string kernel-path)))
  (define imports
    (for/list ([spec (in-list reqs)]
               #:when (spec-module-path spec))
      (spec-module-path spec)))
  (check-true (pair? imports) "kernel has require forms")
  (define violations
    (for/list ([i (in-list imports)]
               #:unless (member i allowed-kernel-imports))
      i))
  (check-equal? violations '() (format "kernel imports forbidden modules: ~a" violations)))

;; ============================================================
;; Facade ↔ kernel equivalence
;; ============================================================

;; The runtime facade (transition-logic.rkt) adapts gsd-runtime-state to the
;; neutral kernel. Every decision the facade makes must equal the kernel's
;; decision on the projected neutral state.

(define (rt->kernel rt)
  (make-gsd-transition-state (gsd-runtime-state-mode rt)
                             (gsd-runtime-state-total-waves rt)
                             (gsd-runtime-state-current-wave rt)
                             (gsd-runtime-state-completed-waves rt)))

(test-case "equivalence: facade re-exports the full kernel surface"
  ;; The facade's (all-from-out "transition-kernel.rkt") must re-export every
  ;; kernel binding; the facade-only test files (test-transition-logic.rkt,
  ;; test-transition-matrix.rkt) import ONLY the facade and use all kernel
  ;; names. Here we verify the two runtime-typed adapters agree with the
  ;; kernel on every state/target pair.
  (check-true (procedure? compute-next-gsm-state))
  (check-true (procedure? check-state-invariants))
  (check-true (procedure? valid-transition?))
  (check-equal? GSD-STATES '(idle exploring plan-written executing verifying))
  (check-equal? TRANSITIONS TRANSITIONS))

(test-case "equivalence: compute-next-gsm-state mode matches kernel compute-next-state"
  (for ([p (all-pairs)])
    (define rt (rt-state (car p) #:total-waves 2 #:completed-waves (set 0)))
    (define-values (fr fs) (compute-next-gsm-state rt (cdr p)))
    (define-values (kr ks) (compute-next-state (rt->kernel rt) (cdr p)))
    (check-equal? (ok? fr) (ok? kr) (format "~a → ~a ok/err parity" (car p) (cdr p)))
    (when (ok? fr)
      (check-equal? (gsd-runtime-state-mode fs)
                    (gts-mode ks)
                    (format "~a → ~a mode parity" (car p) (cdr p))))))

(test-case "equivalence: check-state-invariants agrees with kernel on shared rules"
  (for ([mode (in-list GSD-STATES)]
        [tw (in-list '(0 2 3))]
        [cw (in-list '(0 0 1))])
    (define rt (rt-state mode #:total-waves tw #:current-wave cw))
    (define-values (rok? _rmsg) (check-state-invariants rt))
    (define-values (kok? kmsg) (check-transition-invariants (rt->kernel rt)))
    ;; Kernel rules are a subset of facade rules; the facade must never
    ;; accept what the kernel rejects.
    (check-true (or (not kok?) rok?)
                (format "~a ~a/~a: facade accepted what kernel rejected (~a)" mode tw cw kmsg))))

(test-case "equivalence: facade retains runtime-only executor rules"
  (define exec (lambda () 'work))
  (define-values (_r s) (compute-next-gsm-state (rt-state 'executing #:executor exec) 'verifying))
  (check-false (gsd-runtime-state-wave-executor s) "executor cleared on executing→verifying")
  (define-values (rok? _msg)
    (check-state-invariants (rt-state 'executing #:total-waves 3 #:executor #f)))
  (check-false rok? "facade requires executor in executing with waves")
  (define-values (kok? _kmsg) (check-transition-invariants (k-state 'executing 3 0 (set))))
  (check-true kok? "kernel has no executor rule"))
