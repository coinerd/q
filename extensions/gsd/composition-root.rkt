#lang racket/base
;; STABILITY: internal

;; Sole production composition root for GSD external-domain ports.
;; Runtime and the extension loader remain unaware of these GSD-specific
;; dependencies; extensions/gsd-planning.rkt imports this root for production
;; initialization while tests parameterize current-gsd-effect-ports with real
;; deterministic fakes.
;;
;; v1.00.24 W3 verification-truth follow-up (BUG-0053): this root also owns
;; the ONE process-wide verification registry. Delivery verification
;; launches are owned singletons (extensions/gsd/verification-job.rkt): a
;; duplicate verifier call for the same wave+command+checkout attaches to
;; the running job instead of racing a second gate. Production code reads
;; the registry through current-gsd-verification-registry at call time;
;; tests rebind the parameter to an isolated registry per case.

(require racket/contract
         "effect-ports.rkt"
         "github-port.rkt"
         "system-adapters.rkt"
         "verification-job.rkt")

(provide system-gsd-effect-ports
         system-verification-registry
         (contract-out [current-gsd-effect-ports (parameter/c gsd-effect-ports?)]
                       [current-gsd-verification-registry (parameter/c verification-registry?)]))

(define system-process-port (make-system-process-port))

(define system-gsd-effect-ports
  (gsd-effect-ports (make-system-filesystem-port)
                    (make-system-git-port system-process-port)
                    (make-system-clock-port)
                    system-process-port
                    (make-dry-run-github-port)
                    (lambda (_event-name _payload) (void))))

(define current-gsd-effect-ports (make-parameter system-gsd-effect-ports))

;; ONE process-wide verification registry, owned here: owned-singleton
;; verification jobs (delivery verify gates) resolve through this parameter,
;; so tests can substitute an isolated registry while production always
;; shares the single process-wide instance.
(define system-verification-registry (make-verification-registry))
(define current-gsd-verification-registry (make-parameter system-verification-registry))
