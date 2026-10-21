#lang racket/base
;; Shared production composition for fresh campaign workers. UI callbacks do
;; not replace session creation, lifecycle dispatch, or prompt execution.
(require racket/dict
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         "../runtime/session/executor-inheritance.rkt"
         "../runtime/session/session-switch.rkt"
         (only-in "../extensions/gsd/supplemental-author-contract.rkt"
                  current-supplemental-author-contract
                  supplemental-author-contract-scope)
         (only-in "../extensions/gsd/policy.rkt"
                  gsd-session-iteration-budget
                  current-gsd-wave-max-iterations)
         (only-in "../extensions/gsd/core.rkt"
                  current-gsd-campaign-owner
                  call-with-gsd-owned-session-switch))
(provide make-campaign-runner)

(define (make-campaign-runner rt-config
                              prior-sess
                              #:on-created [on-created void]
                              #:before-run [before-run void])
  ;; Preserve the coordinator's explicit model switch, not a new inheritance
  ;; policy: without an override the original runtime config is unchanged.
  (define inherited-config
    (inherit-coordinator-runtime-config rt-config
                                        (and prior-sess (agent-session-config prior-sess))
                                        (and prior-sess (agent-session-model-name prior-sess))
                                        (and prior-sess (agent-session-provider prior-sess))))
  ;; Tool coordination prefers project-dir over current-directory. Owned
  ;; workers must resolve relative in-process edits in the same physical repo
  ;; as externalized writes, not the coordinator's logical checkout. Bind this
  ;; independently of model inheritance and never derive it from prompt text.
  (define contract (current-supplemental-author-contract))
  (define owned-cwd
    (cond
      [contract (hash-ref (supplemental-author-contract-scope contract) 'repo)]
      [(current-gsd-campaign-owner) (current-directory)]
      [else #f]))
  (define exec-config
    (if owned-cwd
        (dict-set inherited-config 'project-dir (path->string owned-cwd))
        inherited-config))
  (define campaign-sess (make-agent-session exec-config))
  (define campaign-sid (session-id campaign-sess))
  (define campaign-dir
    (or (agent-session-session-dir campaign-sess)
        (dict-ref rt-config 'session-dir #f)
        (dict-ref rt-config 'store-dir #f)))
  (define bus (dict-ref rt-config 'event-bus #f))
  ;; Full shutdown/rebind/start lifecycle, with GSD cleanup suppression scoped
  ;; only to the owned switch. Other extensions still receive lifecycle hooks.
  (define (switch-to-wave-session!)
    (switch-session! #:old-session-id (and prior-sess (session-id prior-sess))
                     #:old-bus bus
                     #:old-extension-registry (dict-ref rt-config 'extension-registry #f)
                     #:new-session-id campaign-sid
                     #:new-session-dir campaign-dir
                     #:new-bus bus
                     #:new-extension-registry (dict-ref rt-config 'extension-registry #f)
                     #:reason 'fork))
  (if (current-gsd-campaign-owner)
      (call-with-gsd-owned-session-switch switch-to-wave-session!)
      (switch-to-wave-session!))
  (on-created campaign-sess)
  (lambda (prompt)
    (before-run prompt)
    (run-prompt! campaign-sess
                 prompt
                 #:max-iterations
                 (gsd-session-iteration-budget
                  (dict-ref rt-config 'max-iterations (current-gsd-wave-max-iterations))))))
