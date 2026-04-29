#lang racket/base
;; STABILITY: stable

;; interfaces/sdk-compat.rkt — enriched API aliases, tree API, and GSD convenience
;;
;; Extracted from sdk.rkt (v0.22.9 W3).
;; Depends on sdk-core.rkt for runtime struct and core session operations.
;; Provides:
;;   - #1196 enriched SDK API aliases (q:create-session, q:session-send, etc.)
;;   - Tree API (q:session-branch, q:session-navigate, q:session-tree-info)
;;   - GSD observability (gsd-status)
;;   - GSD convenience (q:plan, q:go, q:gsd-status, q:reset-gsd!)
;;   - Command dispatch (dispatch-command!)

(require racket/contract
         racket/file
         racket/math
         racket/list
         "../util/protocol-types.rkt"
         (prefix-in session: "../runtime/agent-session.rkt")
         (only-in "../runtime/compactor.rkt"
                  compact-history
                  compact-and-persist!
                  compaction-result-removed-count
                  compaction-result?)
         (only-in "../extensions/hooks.rkt" dispatch-hooks hook-result? hook-result-payload)
         (only-in "../extensions/gsd-planning-state.rkt" gsd-snapshot reset-all-gsd-state!)
         (prefix-in store: "../runtime/session-store.rkt")
         "../agent/event-bus.rkt"
         "../util/cancellation.rkt"
         ;; Core SDK types
         "sdk-core.rkt")

;; Re-export core types for convenience
(provide (struct-out runtime-config)
         (struct-out runtime)
         runtime?
         make-runtime
         open-session
         run-prompt!
         subscribe-events!
         interrupt!
         fork-session!
         compact-session!
         session-info
         steer!
         follow-up!
         navigate!
         create-agent-session

         ;; Cancellation token
         make-cancellation-token
         cancellation-token?
         cancellation-token-cancelled?
         cancel-token!

         ;; Compaction types
         compaction-result?

         ;; Navigation types
         navigate-result?

         ;; In-memory session manager
         make-in-memory-session-manager
         in-memory-session-manager?
         in-memory-append!
         in-memory-append-entries!
         in-memory-load
         in-memory-list-sessions
         in-memory-fork!

         ;; Thinking level
         session:thinking-levels
         session:thinking-level?
         session:thinking-level->budget
         session:agent-session-thinking-level
         session:set-thinking-level!

         ;; Context usage
         context-usage?
         context-usage-total-tokens
         context-usage-max-tokens
         context-usage-usage-percent
         context-usage-compaction-threshold
         get-context-usage
         context-usage-near-threshold?

         ;; #1196: Enriched SDK API aliases
         q:create-session
         q:session-send
         q:session-subscribe
         q:session-interrupt
         q:session-fork
         q:session-compact
         q:session-info
         q:session-branch
         q:session-navigate
         q:session-tree-info

         ;; v0.20.4 W3: GSD observability
         gsd-status

         ;; v0.20.5 W2: GSD convenience API
         q:plan
         q:go
         q:gsd-status
         q:reset-gsd!

         ;; Command dispatch
         dispatch-command!)

;; ============================================================
;; Enriched API aliases (#1196)
;; ============================================================

(define q:create-session create-agent-session)
(define q:session-send run-prompt!)
(define q:session-subscribe subscribe-events!)
(define q:session-interrupt interrupt!)
(define q:session-fork fork-session!)
(define q:session-compact compact-session!)
(define q:session-info session-info)

;; ============================================================
;; Tree API (#1319)
;; ============================================================

(define (q:session-branch rt [entry-id #f] [branch-name "unnamed"])
  (define sess (runtime-rt-session rt))
  (cond
    [(not sess) 'no-active-session]
    [else
     (define log-path (build-path (session:agent-session-session-dir sess) "session.jsonl"))
     (define history (session:session-history sess))
     (define target-id
       (or entry-id (and (pair? history) (message-id (last history))) (session:session-id sess)))
     (define branch
       (make-branch-entry (format "branch-~a" (current-inexact-milliseconds)) target-id branch-name))
     (store:append-tree-entry! log-path branch)
     (hasheq 'branch-id (message-id branch) 'parent-entry-id target-id 'branch-name branch-name)]))

(define (q:session-navigate rt target-entry-id)
  (define sess (runtime-rt-session rt))
  (cond
    [(not sess) 'no-active-session]
    [else
     (define log-path (build-path (session:agent-session-session-dir sess) "session.jsonl"))
     (define history (session:session-history sess))
     (define from-id
       (if (pair? history)
           (message-id (last history))
           (session:session-id sess)))
     (define nav
       (make-tree-navigation-entry (format "nav-~a" (current-inexact-milliseconds))
                                   from-id
                                   target-entry-id))
     (store:append-tree-entry! log-path nav)
     (hasheq 'navigation-id
             (message-id nav)
             'from-entry-id
             from-id
             'target-entry-id
             target-entry-id)]))

(define (q:session-tree-info rt)
  (define sess (runtime-rt-session rt))
  (cond
    [(not sess) 'no-active-session]
    [else
     (define log-path (build-path (session:agent-session-session-dir sess) "session.jsonl"))
     (cond
       [(not (file-exists? log-path))
        (hasheq 'total-entries 0 'branch-count 0 'navigation-count 0 'summary-count 0 'leaf-ids '())]
       [else (store:tree-info (store:load-tree log-path))])]))

;; ============================================================
;; GSD observability (v0.20.4 W3)
;; ============================================================

(define (gsd-status)
  (define snap (gsd-snapshot))
  (define mode (hash-ref snap 'mode #f))
  (if (or (not mode) (eq? mode 'idle)) 'no-active-session snap))

;; ============================================================
;; Command dispatch (v0.20.5 W0)
;; ============================================================

(define (dispatch-command! rt command input)
  (define cfg (runtime-rt-config rt))
  (define ext-reg (runtime-config-extension-registry cfg))
  (cond
    [(not ext-reg) (values rt 'no-extension-registry)]
    [else
     (define payload (hasheq 'command command 'input input))
     (define result (dispatch-hooks 'execute-command payload ext-reg))
     (values rt result)]))

;; ============================================================
;; GSD convenience (v0.20.5 W2)
;; ============================================================

(define (q:plan rt task)
  (define-values (rt2 cmd-result) (dispatch-command! rt "/plan" (string-append "/plan " task)))
  (cond
    [(symbol? cmd-result) (values rt2 cmd-result)]
    [(not (hook-result? cmd-result)) (values rt2 'unexpected-result)]
    [else
     (define payload (hook-result-payload cmd-result))
     (define submit-text (and (hash? payload) (hash-ref payload 'submit #f)))
     (cond
       [(not submit-text) (values rt2 'no-plan-text)]
       [(not (runtime-rt-session rt2)) (values rt2 submit-text)]
       [else (run-prompt! rt2 submit-text)])]))

(define (q:go rt [wave #f])
  (define input
    (if wave
        (format "~a" wave)
        ""))
  (define-values (rt2 cmd-result) (dispatch-command! rt "/go" input))
  (cond
    [(symbol? cmd-result) (values rt2 cmd-result)]
    [(not (hook-result? cmd-result)) (values rt2 'unexpected-result)]
    [else
     (define payload (hook-result-payload cmd-result))
     (define submit-text
       (and (hash? payload) (or (hash-ref payload 'new-session #f) (hash-ref payload 'submit #f))))
     (cond
       [(not submit-text) (values rt2 'no-plan-text)]
       [(not (runtime-rt-session rt2)) (values rt2 submit-text)]
       [else (run-prompt! rt2 submit-text)])]))

(define q:gsd-status gsd-status)

(define (q:reset-gsd!)
  (reset-all-gsd-state!))
