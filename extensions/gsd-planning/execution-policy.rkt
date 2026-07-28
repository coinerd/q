#lang racket/base

;; extensions/gsd-planning/execution-policy.rkt — phase transitions, tool blocking rules
;;
;; Mode-based tool access control and state machine guard logic.

(require racket/path
         racket/string
         "../gsd/state-machine.rkt"
         "../hooks.rkt"
         (only-in "../gsd/session-state.rkt" current-gsd-ctx))

(provide gsd-tool-guard)

(define protected-planning-path-rx
  #px"(?:^|/)\\.planning/(?:PLAN|STATE|VALIDATION)(?:-[^/]*)?\\.md$|(?:^|/)\\.planning/HANDOFF\\.json$")

(define (generic-mutation-targets-planning-artifact? tool-name payload)
  (and (member tool-name '("write" "edit"))
       (let* ([args (hash-ref payload 'tool-arguments (hasheq))]
              [raw-path (and (hash? args) (hash-ref args 'path #f))]
              [path-string (cond
                             [(path? raw-path) (path->string raw-path)]
                             [(string? raw-path) raw-path]
                             [else #f])])
         (and path-string
              (regexp-match? protected-planning-path-rx (string-replace path-string "\\" "/"))))))

(define (gsd-tool-guard payload)
  (define mode (gsm-ctx-current (current-gsd-ctx)))
  (define tool-name (hash-ref payload 'tool-name #f))
  (define allowed (gsm-tool-allowed? tool-name))
  (cond
    [(and (eq? mode 'executing) (equal? tool-name "planning-write"))
     (hook-block "Cannot update plan during /go. Focus on executing the existing plan.")]
    [(and (eq? mode 'executing) (generic-mutation-targets-planning-artifact? tool-name payload))
     (hook-block
      "Cannot mutate authoritative .planning artifacts through generic write/edit during /go.")]
    [(and (not allowed) (eq? mode 'plan-written))
     (hook-block "Plan written to PLAN.md. Use /go to start implementing.")]
    [(not allowed) (hook-block (format "Tool '~a' blocked in ~a mode." tool-name mode))]
    [else (hook-pass payload)]))
