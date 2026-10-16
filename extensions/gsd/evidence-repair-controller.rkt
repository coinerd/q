#lang racket/base
;; Concrete owner orchestration; no caller-supplied executor or proof authority.
(require "evidence-readiness.rkt"
         "evidence-repair-format.rkt"
         "evidence-repair-ledger.rkt"
         "evidence-probe.rkt")
(provide resume-evidence-repair!)
(define (stop code [message ""])
  (hasheq 'status "paused" 'code code 'message message))
(define (resume-evidence-repair! base plan wave #:guard live-guard)
  (with-handlers ([exn:fail? (lambda (e) (stop "evidence-repair-blocked" (exn-message e)))])
    (unless (and (procedure? live-guard) (procedure-arity-includes? live-guard 0))
      (error 'resume-evidence-repair! "live guard required"))
    (define readiness (evidence-readiness base plan wave))
    (cond
      [(not (live-guard)) (stop "stale-live-guard")]
      [(equal? (hash-ref readiness 'status) "ready") readiness]
      [(equal? (hash-ref readiness 'status) "blocked") readiness]
      [else
       (define context (hash-ref readiness 'context))
       (define spec (hash-ref readiness 'spec 'null))
       (cond
         [(not (and (valid-probe-spec? spec) (equal? context (hash-ref spec 'context))))
          (stop "evidence-repair-required"
                (hash-ref readiness 'message "No eligible authenticated probe"))]
         [else
          (define reservation
            (reserve-evidence-repair! base context (canonical-evidence-digest spec)))
          (define op (hash-ref reservation 'operation))
          (define repo (hash-ref context 'repo))
          (cond
            [(not (and (member (hash-ref reservation 'status) '("reserved" "existing"))
                       (member (hash-ref op 'state) '("reserved" "captured"))))
             ;; Ambiguous intent/start records cannot be adopted or relaunched.
             ;; Only a durably captured proof supports guarded recovery.
             (stop (if (equal? (hash-ref reservation 'status) "existing")
                       "evidence-repair-resume-paused"
                       "evidence-repair-budget-exhausted"))]
            [else
             (define id (hash-ref op 'id))
             (define (guard)
               (and (live-guard)
                    (equal? context (current-evidence-context base plan wave))
                    (let ([state (load-evidence-repair base
                                                       plan
                                                       wave
                                                       (hash-ref context 'receipt-generation 0))])
                      (define current (hash-ref state 'operation))
                      (and (equal? id (hash-ref current 'id))
                           (equal? context (hash-ref current 'context))
                           (equal? (canonical-evidence-digest spec) (hash-ref current 'spec))
                           (member (hash-ref current 'state)
                                   '("reserved" "intent" "start" "captured"))))))
             (unless (guard)
               (error 'resume-evidence-repair! "stale reservation"))
             (define captured? (equal? (hash-ref op 'state) "captured"))
             ;; Only a known captured result can be adopted; ambiguous launches pause.
             (define probe
               (if captured?
                   (hash-ref op 'probe)
                   (let ()
                     (advance-evidence-repair! base context id "reserved" (hasheq 'state "intent"))
                     (define prepared (prepare-baseline-probe! repo spec id))
                     (unless (guard)
                       (error 'resume-evidence-repair! "stale preparation"))
                     (advance-evidence-repair! base context id "intent" (hasheq 'state "start"))
                     (run-baseline-probe! repo prepared id #:guard guard))))
             (cond
               [(hash-has-key? probe 'status)
                (advance-evidence-repair! base
                                          context
                                          id
                                          "start"
                                          (hasheq 'state "blocked" 'reason (hash-ref probe 'code)))
                (stop "evidence-repair-blocked" (hash-ref probe 'code))]
               [else
                (unless captured?
                  (advance-evidence-repair! base
                                            context
                                            id
                                            "start"
                                            (hasheq 'state "captured" 'probe probe)))
                (define authorization
                  (recover-evidence-registration-authorization repo context probe id #:guard guard))
                (register-owned-evidence! base
                                          plan
                                          wave
                                          #:context context
                                          #:probe probe
                                          #:authorization authorization
                                          #:operation-id id)
                ;; Only authority readiness, never registration alone, admits review.
                (evidence-readiness base plan wave)])])])])))
