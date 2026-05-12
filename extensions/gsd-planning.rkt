#lang racket/base

;; extensions/gsd-planning.rkt — GSD Planning Extension FACADE
;; STABILITY: stable
;;
;; Thin re-export facade over extensions/gsd/*. Sub-modules:
;;   gsd/tool-handlers.rkt     — planning-read / planning-write handlers
;;   gsd/command-handlers.rkt  — slash-command dispatch
;;   gsd/prompts.rkt           — phase-specific prompt templates
;;
;; Also re-exports state accessors from gsd-planning-state.rkt for
;; backward compatibility.

(require racket/contract
         racket/match
         racket/string
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "ext-commands.rkt"
         "context.rkt"
         "hooks.rkt"
         "tool-api.rkt"
         "../util/event.rkt"
         "api.rkt"
         "gsd-planning/command-normalization.rkt"
         "gsd-planning/execution-policy.rkt"
         "gsd-planning/plan-diff.rkt"
         ;; v0.21.0 new modules
         "gsd/state-machine.rkt"
         (only-in "gsd/core.rkt"
                  reset-all-gsd-state!
                  gsd-show-status
                  gsd-command-result-success
                  gsd-command-result-message
                  gsd-command-result-data
                  gsd-command-result?
                  gsd-result?
                  gsd-success?
                  gsd-failed?
                  gsd-command-result-mode
                  with-gsd-transaction)
         (only-in "gsd/policy.rkt" policy-decision policy-blocked? policy-reason)
         "gsd/plan-types.rkt"
         "gsd/plan-validator.rkt"
         (except-in "gsd/wave-executor.rkt" next-pending-wave)
         "gsd/prompts.rkt"
         "gsd/context-bundle.rkt"
         "gsd/wave-docs.rkt"
         (only-in "gsd/archive.rkt" ensure-state-md!)
         (only-in "gsd/events.rkt"
                  [gsd-event-bus-box events:event-bus-box]
                  [set-gsd-event-bus! events:set-event-bus!]
                  [emit-gsd-event! events:emit-gsd-event!])
         (only-in "gsd/session-state.rkt"
                  set-gsd-state!
                  current-pinned-dir
                  set-pinned-dir!
                  current-edit-limit
                  set-edit-limit!
                  current-gsd-event-bus
                  set-gsd-event-bus!)
         ;; Extracted sub-modules
         "gsd/tool-handlers.rkt"
         "gsd/command-handlers.rkt")

;; --- Public API (stable) ---

;; Legacy mode wrappers (DEBT-01: migrated from gsd-planning-state.rkt)
(define (gsd-mode)
  (let ([s (gsm-current)])
    (cond
      [(eq? s 'idle) #f]
      [(eq? s 'exploring) 'planning]
      [else s])))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (cond
    [(not v) (gsm-reset!)]
    [(eq? v 'planning) (gsm-transition-to! 'exploring)]
    [(eq? v 'plan-written) (gsm-transition-to! 'plan-written)]
    [(eq? v 'executing) (gsm-transition-to! 'executing)]
    [else (gsm-transition! v)]))

;; Legacy accessor wrappers (DEBT-01)
(define (pinned-planning-dir)
  (current-pinned-dir))
(define (set-pinned-planning-dir! v)
  (set-pinned-dir! v))
(define (current-max-old-text-len)
  (current-edit-limit))
(define (set-current-max-old-text-len! v)
  (set-edit-limit! v))
(define (completed-waves)
  (gsm-completed-waves))
(define (total-waves)
  (gsm-total-waves))
(define (set-total-waves! n)
  (gsm-set-total-waves! n))
(define (mark-wave-complete! idx)
  (gsm-mark-wave-complete! idx))
(define (wave-complete? idx)
  (gsm-wave-complete? idx))
(define (next-pending-wave)
  (gsm-next-pending-wave))
(define (current-wave-index)
  (gsm-current-wave))
(define (set-current-wave-index! n)
  (gsm-set-current-wave! n))
(define (gsd-snapshot)
  (gsm-snapshot))
(define (gsd-event-bus)
  (current-gsd-event-bus))

;; reset-all-gsd-state! is now in gsd/core.rkt

(provide the-extension
         gsd-planning-extension
         planning-system-prompt
         planning-artifact-path
         valid-artifact-name?
         read-planning-artifact
         write-planning-artifact!
         handle-planning-read
         handle-planning-write
         planning-implement-prompt
         gsd-mode
         set-gsd-mode!
         gsd-tool-guard
         gsd-session-cleanup
         gsd-event-bus
         set-gsd-event-bus!
         emit-gsd-event!
         ;; --- Internal (subject to change, backward compat) ---
         pinned-planning-dir
         set-pinned-planning-dir!
         current-max-old-text-len
         set-current-max-old-text-len!
         reset-all-gsd-state!
         gsd-mode?
         parse-wave-headers
         completed-waves
         total-waves
         set-total-waves!
         mark-wave-complete!
         wave-complete?
         next-pending-wave
         current-wave-index
         set-current-wave-index!)

;; planning-system-prompt is an alias for planning-prompt from prompts.rkt
(define planning-system-prompt planning-prompt)

;; ============================================================
;; Event emission helper
;; ============================================================

;; Event emission: delegates to events.rkt via session bus bridge
;; See register-gsd-tools for bus wiring.
(define (emit-gsd-event! event-sym payload)
  (events:emit-gsd-event! event-sym payload))

;; ============================================================
;; Extension hooks
;; ============================================================

;; --- register-tools ---
(define (register-gsd-tools ctx _payload)
  (unless (pinned-planning-dir)
    (define cwd (ctx-cwd ctx))
    (when cwd
      (set-pinned-planning-dir! (resolve-project-root cwd))))
  ;; Wire events.rkt bus into session event bus
  (define session-bus (ctx-event-bus ctx))
  (set-gsd-event-bus! session-bus) ; backward compat shim
  (events:set-event-bus!
   (lambda (event-name wrapped-event)
     ;; Bridge: forward events.rkt events to the session event bus
     (when session-bus
       (publish! session-bus (make-event event-name (current-seconds) #f #f wrapped-event)))))
  (ext-register-tool!
   ctx
   "planning-read"
   (string-append "Read GSD planning artifacts from the .planning/ directory. "
                  "Canonical artifacts: "
                  (string-join (map car artifact-extensions) ", ")
                  ". Returns file content as text (for .md) or JSON (for .json).")
   planning-read-schema
   handle-planning-read
   #:prompt-guidelines
   "Use planning-read with artifact='PLAN' or artifact='STATE' to check current plan/state before taking action.")
  (ext-register-tool!
   ctx
   "planning-write"
   (string-append "Write or update GSD planning artifacts in the .planning/ directory. "
                  "Validates artifact names. Creates .planning/ if needed. "
                  "For .json artifacts, pass content as JSON string.")
   planning-write-schema
   handle-planning-write
   #:prompt-guidelines
   (string-append
    "Use planning-write with artifact='PLAN' to update PLAN.md after completing a wave. "
    "Use artifact='STATE' to update STATE.md with current status. "
    "Use artifact='HANDOFF' to write HANDOFF.json before machine switches."))
  (hook-pass #f))

;; ============================================================
;; GSD mode tool guard (tool-call-pre hook)
;; ============================================================

;; v0.21.3: Only mode-based blocking. No budgets.
;; gsd-tool-guard: delegated to gsd-planning/execution-policy.rkt

;; ============================================================
;; Session cleanup
;; ============================================================

(define (gsd-session-cleanup payload)
  (log-debug "gsd-planning: session shutdown, resetting state")
  (reset-all-gsd-state!)
  (hook-pass payload))

;; ============================================================
;; Extension definition
;; ============================================================

(define-q-extension gsd-planning-extension
                    #:version "1.1.0"
                    #:api-version "1"
                    #:on register-tools
                    register-gsd-tools
                    #:on register-shortcuts
                    register-gsd-commands
                    #:on execute-command
                    handle-execute-command
                    #:on tool-call-pre
                    gsd-tool-guard
                    #:on session-shutdown
                    gsd-session-cleanup)

(define the-extension gsd-planning-extension)
