#lang racket/base
;; STABILITY: internal

;; Sole production composition root for GSD external-domain ports.
;; Runtime and the extension loader remain unaware of these GSD-specific
;; dependencies; extensions/gsd-planning.rkt imports this root for production
;; initialization while tests parameterize current-gsd-effect-ports with real
;; deterministic fakes.

(require racket/contract
         "effect-ports.rkt"
         "system-adapters.rkt")

(provide system-gsd-effect-ports
         (contract-out [current-gsd-effect-ports (parameter/c gsd-effect-ports?)]))

(define system-process-port (make-system-process-port))

(define system-gsd-effect-ports
  (gsd-effect-ports (make-system-filesystem-port)
                    (make-system-git-port system-process-port)
                    (make-system-clock-port)
                    system-process-port
                    (lambda (_event-name _payload) (void))))

(define current-gsd-effect-ports (make-parameter system-gsd-effect-ports))
