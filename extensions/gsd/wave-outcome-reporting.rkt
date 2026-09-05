#lang racket/base

;; extensions/gsd/wave-outcome-reporting.rkt — Terminal runner outcome
;; reporting helpers for the single-wave coordinator.
;;
;; BUG-0042 (W7) size discipline + BUG-0060: extracted from
;; go-orchestrator.rkt so coordinator growth lands in a module instead of
;; the pinned orchestration surface. `emit-wave-outcome-error!` routes a
;; terminal outcome (kind != 'done) to the typed [SYS] [ERROR] transcript
;; surface; `runner-outcome-failure-reason` turns an outcome into an honest
;; durable failure reason. Both are best-effort/pure with respect to
;; campaign control flow.

(require racket/string
         "events.rkt"
         "wave-runner-port.rkt")

(provide emit-wave-outcome-error!
         runner-outcome-failure-reason)

;; A terminal wave-execution-outcome with kind != 'done must surface as a
;; typed [SYS] [ERROR] transcript event — NOT as conversation/message-surface
;; text (BUG-0043, W2). Best-effort: a bus failure must never break the
;; campaign control flow.
(define (emit-wave-outcome-error! wave-idx kind message)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "gsd: outcome-error event emission failed: ~a"
                                            (exn-message e)))])
    (emit-gsd-event! 'gsd.wave.outcome-error
                     (hasheq 'wave wave-idx 'kind kind 'level "error" 'message (or message "")))))

(define (runner-outcome-failure-reason outcome result)
  (define msg (wave-execution-outcome-message result))
  (if (and (string? msg) (positive? (string-length (string-trim msg))))
      msg
      (format "wave execution ended '~a' without a runner message" outcome)))
