#lang racket/base

;; runtime/context-assembly/turn-context.rkt — Turn-level context assembly helpers
;; STABILITY: internal
;; CONSUMERS: context-assembly facade, agent-session
;;
;; Extracted from turn-orchestrator.rkt (v0.96.0 W2) for single-responsibility.
;; Contains: symbol->task-state, assemble-context/pure,
;;           prepare-turn-context-state, emit-context-assembly-events!,
;;           current-last-task-fsm-state parameter.

(require racket/format
         racket/list
         racket/path
         racket/promise
         racket/set
         (only-in racket/string string-join string-trim)
         ;; Message/content types
         (only-in "../../util/message/message.rkt"
                  message-id
                  message-kind
                  message-content
                  message-role)
         (only-in "../../util/content/content-parts.rkt"
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  tool-result-part?
                  tool-result-part-tool-call-id
                  tool-result-part-content
                  tool-result-part-is-error?)
         ;; Event emission
         "../../agent/event-emitter.rkt"
         "../../agent/event-structs/iteration-events.rkt"
         "../../agent/event-structs/session-events.rkt"
         ;; Context assembly core
         (only-in "../context/context-assembly.rkt"
                  build-tiered-context-with-hooks
                  tiered-context->message-list
                  tiered-context?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c)
         (only-in "../context-assembly/context-floor.rkt" tiered-context-with-tier-a)
         (only-in "../context-assembly/operational-checkpoint.rkt"
                  make-empty-checkpoint
                  checkpoint-set-repo-root
                  checkpoint-set-planning-root
                  checkpoint-set-planning-authority
                  supercedes-generic-planning?
                  inject-checkpoint-message)
         (only-in "../context-assembly/serialization.rkt"
                  gsd-progress-message?
                  build-tiered-context/state-aware
                  current-task-state-aware-assembly?)
         ;; Working set
         (only-in "../working-set.rkt"
                  working-set-resolve-messages
                  working-set-entry-count
                  working-set-token-count)
         ;; Session config accessors
         (only-in "../session/session-config.rkt"
                  config-tier-b-count
                  config-tier-c-count
                  config-max-tokens
                  config-working-set
                  config-task-state-aware?
                  config-context-assembly-profile
                  config-project-dir
                  config-repo-root
                  config-planning-root
                  apply-context-assembly-profile!
                  context-assembly-options?
                  context-assembly-options-task-state-aware?
                  context-assembly-options-graph-conclusion-selection?
                  context-assembly-options-conclusion-token-budget)
         ;; Session state accessors (read-only)
         (only-in "../session/session-types.rkt"
                  agent-session-task-fsm-state
                  agent-session-task-conclusions
                  agent-session-recent-tool-calls)
         ;; Session mutation (for auto-distill persistence)
         (only-in "../session/session-mutation.rkt"
                  guarded-set-working-set-evolved!
                  guarded-set-task-conclusions!)
         ;; Hook types
         (only-in "../../util/hook-types.rkt" hook-result? hook-result-action)
         ;; Token estimation
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         ;; WS evolution
         (only-in "../context-assembly/ws-evolution.rkt"
                  evolve-working-set-for-state/result
                  evolution-result?
                  reset-working-set!)
         (only-in "../context-assembly/state-aware-builder.rkt" current-ws-evolution-enabled?)
         (only-in "../context-assembly/rollback-actions.rkt" current-loop-warning-count)
         ;; Auto-distillation
         (only-in "../context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-auto-distillation-enabled?)
         ;; Task-state singletons
         (only-in "../context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging))

(provide current-last-task-fsm-state
         current-pending-force-reset
         symbol->task-state
         assemble-context/pure
         prepare-turn-context-state
         emit-context-assembly-events!
         content-part->text)

;; ============================================================
;; Task state conversion
;; ============================================================

;; v0.79.2 GAP-2: Track last task FSM state for WS evolution old-state.
;; Set each turn from agent-session-task-fsm-state before it gets updated.
(define current-last-task-fsm-state (make-parameter #f))

;; R0: Pending force-reset flag — set by session-events subscriber when
;; tool.set-task-state.completed event has force-reset: #t.
;; Checked by prepare-turn-context-state to call reset-working-set!
;; before WS evolution. Cleared after check to avoid stale flags.
(define current-pending-force-reset (make-parameter #f))

;; Convert a raw state symbol to the canonical fsm-state? struct.
;; The runtime stores task-fsm-state as raw symbols, but downstream consumers
;; (ws-evolution, state-aware-builder) expect fsm-state? structs.
(define (symbol->task-state sym)
  (case sym
    [(idle) task-idle]
    [(exploration) task-exploration]
    [(planning) task-planning]
    [(implementation) task-implementation]
    [(verification) task-verification]
    [(debugging) task-debugging]
    [else #f]))

;; ============================================================
;; Pure context assembly
;; ============================================================

(define OP-CHECKPOINT-ID "op-checkpoint")

(define (without-operational-checkpoint messages)
  (filter (lambda (m) (not (equal? (message-id m) OP-CHECKPOINT-ID))) messages))

(define (path-value->string value)
  (cond
    [(path? value) (path->string value)]
    [(string? value) value]
    [else #f]))

(define (argument-ref arguments key)
  (and (hash? arguments)
       (or (hash-ref arguments key #f) (hash-ref arguments (symbol->string key) #f))))

(define (tool-call-artifact-path part planning-root)
  (define name (tool-call-part-name part))
  (define arguments (tool-call-part-arguments part))
  (cond
    [(equal? name "read") (path-value->string (argument-ref arguments 'path))]
    [(equal? name "planning-read")
     (define artifact (path-value->string (argument-ref arguments 'artifact)))
     (define base-dir (path-value->string (argument-ref arguments 'base_dir)))
     (define effective-planning-root
       (if base-dir
           (path->string (build-path base-dir ".planning"))
           planning-root))
     (and artifact
          (if (or (not effective-planning-root) (absolute-path? (string->path artifact)))
              artifact
              (path->string (build-path effective-planning-root artifact))))]
    [else #f]))

(define (result-content->string content)
  (cond
    [(string? content) content]
    [(hash? content)
     (define text (or (hash-ref content 'text #f) (hash-ref content "text" #f)))
     (if text
         (result-content->string text)
         "")]
    [(list? content) (string-join (map result-content->string content) "\n")]
    [else ""]))

(define (absolute-result-path content)
  (define candidate (string-trim (result-content->string content)))
  (and (not (string=? candidate ""))
       (not (regexp-match? #px"[\r\n]" candidate))
       (absolute-path? (string->path candidate))
       candidate))

;; Return the most recent canonical coordinates proven by successful,
;; correlated tool results. Failed or merely requested calls confer no
;; authority.
(define (successful-coordinate-discoveries messages planning-root)
  (define pending (make-hash))
  (define latest-authority #f)
  (define latest-repo #f)
  (for ([m (in-list messages)])
    (when (eq? (message-role m) 'assistant)
      (for ([part (in-list (message-content m))]
            #:when (tool-call-part? part))
        (define id (tool-call-part-id part))
        (define path (tool-call-artifact-path part planning-root))
        (define command (argument-ref (tool-call-part-arguments part) 'command))
        (cond
          [(and id path (supercedes-generic-planning? path))
           (hash-set! pending id (cons 'planning path))]
          [(and
            id
            (equal? (tool-call-part-name part) "bash")
            (string? command)
            (regexp-match?
             #px"(?:^|[[:space:]])git(?:[[:space:]].*)?rev-parse[[:space:]]+--show-toplevel(?:[[:space:]]|$)"
             command))
           (hash-set! pending id (cons 'repo #t))])))
    (for ([part (in-list (message-content m))]
          #:when (and (tool-result-part? part) (not (tool-result-part-is-error? part))))
      (define discovery (hash-ref pending (tool-result-part-tool-call-id part) #f))
      (when discovery
        (case (car discovery)
          [(planning) (set! latest-authority (cdr discovery))]
          [(repo)
           (define discovered (absolute-result-path (tool-result-part-content part)))
           (when discovered
             (set! latest-repo discovered))]))))
  (values latest-authority latest-repo))

(define (fresh-operational-checkpoint config messages)
  (define fallback-repo (path-value->string (config-repo-root config)))
  (define fallback-planning (path-value->string (config-planning-root config)))
  (define-values (authority discovered-repo)
    (successful-coordinate-discoveries messages fallback-planning))
  (define discovered-planning
    (and authority
         (let ([parent (path-only (string->path authority))]) (and parent (path->string parent)))))
  (define repo-root (or discovered-repo fallback-repo))
  (define planning-root (or discovered-planning fallback-planning))
  (define with-repo
    (if repo-root
        (checkpoint-set-repo-root (make-empty-checkpoint) repo-root)
        (make-empty-checkpoint)))
  (define with-planning
    (if planning-root
        (checkpoint-set-planning-root with-repo planning-root)
        with-repo))
  (if authority
      (checkpoint-set-planning-authority with-planning authority)
      with-planning))

;; Pure context assembly: no side effects, no session mutation.
;; Returns (values assembled-messages hook-result tiered-context).
(define (assemble-context/pure ctx-to-use
                               config-raw
                               #:hook-dispatcher [hook-dispatcher #f]
                               #:task-state [task-state #f]
                               #:conclusions [conclusions '()]
                               #:state-aware? [state-aware? #f]
                               #:ca-options [ca-options #f]
                               #:recent-tool-calls [recent-tool-calls '()])
  (define config config-raw)
  ;; An assembled checkpoint may be passed back as history by compatibility
  ;; callers. It is ephemeral: discard it before every fresh assembly.
  (define history (without-operational-checkpoint ctx-to-use))
  (define tier-b-count (config-tier-b-count config))
  (define tier-c-count (config-tier-c-count config))
  (define max-tokens (config-max-tokens config))
  (define ws (config-working-set config))
  (define ws-messages-promise
    (delay
      (if ws
          (working-set-resolve-messages ws history message-id)
          '())))
  (define ws-messages (force ws-messages-promise))
  (define state-aware-enabled
    (or state-aware?
        (current-task-state-aware-assembly?)
        (and ca-options (context-assembly-options-task-state-aware? ca-options))))
  (define-values (tc hook-result)
    (cond
      ;; v0.76.3: State-aware assembly when enabled (global flag, per-session rollout, or explicit options)
      [(and state-aware-enabled task-state)
       (define project-dir (config-project-dir config))
       (define sa-tc
         (build-tiered-context/state-aware history
                                           #:tier-b-count tier-b-count
                                           #:tier-c-count tier-c-count
                                           #:working-set-messages ws-messages
                                           #:task-state task-state
                                           #:conclusions conclusions
                                           #:ca-options ca-options
                                           #:recent-tool-calls recent-tool-calls
                                           #:project-dir project-dir
                                           #:session-config config))
       (values sa-tc #f)]
      ;; Standard assembly path
      [else
       (build-tiered-context-with-hooks history
                                        #:hook-dispatcher hook-dispatcher
                                        #:tier-b-count tier-b-count
                                        #:tier-c-count tier-c-count
                                        #:max-tokens max-tokens
                                        #:working-set-messages ws-messages)]))
  ;; Use the tiered-context constructor helper rather than prepending only to
  ;; the provider list. This keeps Tier A telemetry and provider ordering in
  ;; agreement.
  (define checkpoint (fresh-operational-checkpoint config history))
  (define final-tc
    (tiered-context-with-tier-a tc (inject-checkpoint-message checkpoint (tiered-context-tier-a tc))))
  (values (tiered-context->message-list final-tc) hook-result final-tc))

;; v0.97.6 F3: Extracted from prepare-turn-context-state for testability.
;; Converts a content part (text-part or tool-result-part) to a plain string.
;; tool-call-part and unknown types are intentionally discarded —
;; tool calls have no useful summary text for distillation purposes.
(define (content-part->text part)
  (cond
    [(text-part? part) (text-part-text part)]
    [(and (tool-result-part? part) (not (tool-result-part-is-error? part)))
     (define c (tool-result-part-content part))
     (define raw
       (cond
         [(string? c) c]
         [(hash? c) (~a c)]
         [(list? c) (string-join (map ~a c) " ")]
         [else ""]))
     (if (> (string-length raw) 500)
         (string-append (substring raw 0 497) "...")
         raw)]
    [else ""]))

;; ============================================================
;; Turn context state preparation
;; ============================================================

;; Prepare task state, conclusions (with auto-distill), and WS evolution
;; for context assembly. Mutates session state when auto-distill adds conclusions
;; or WS evolution produces a new working set.
(define (prepare-turn-context-state ctx-to-use config-raw session)
  (define ws-early (config-working-set config-raw))
  (define task-state-raw (and session (agent-session-task-fsm-state session)))
  (define task-state (or (symbol->task-state task-state-raw) task-state-raw))
  (define conclusions (and session (agent-session-task-conclusions session)))
  ;; v0.77.9 T2.1: Auto-distill uncovered WS entries when enabled
  (define augmented-conclusions
    (if (and (current-auto-distillation-enabled?) session conclusions task-state ws-early)
        (let ([ws-msgs (working-set-resolve-messages ws-early ctx-to-use message-id)])
          ;; v0.79.2 GAP-3: Build content summaries for richer auto-distill text
          ;; GAP-C: Include tool-result-parts in content summaries
          (define summaries
            (for/hash ([m (in-list ws-msgs)])
              (define parts (map content-part->text (message-content m)))
              (define full-text (string-join (filter (lambda (s) (> (string-length s) 0)) parts) " "))
              (values (message-id m) full-text)))
          (append conclusions
                  (auto-distill (map message-id ws-msgs) conclusions task-state-raw summaries)))
        (or conclusions '())))
  ;; v0.78.2 G3: Persist auto-distilled conclusions back to session
  (when (and (current-auto-distillation-enabled?)
             session
             (pair? augmented-conclusions)
             conclusions
             (> (length augmented-conclusions) (length conclusions)))
    (guarded-set-task-conclusions! session augmented-conclusions))
  ;; R0: Check pending force-reset flag — set by session-events subscriber
  ;; when tool.set-task-state.completed event had force-reset: #t.
  ;; When set, reset the working-set before WS evolution.
  (when (and (current-pending-force-reset) ws-early)
    (reset-working-set! ws-early)
    (current-pending-force-reset #f))
  ;; v0.78.2 G2: WS evolution — evolve working set on state transition
  ;; MF1 (GAP-5): Guard at call site — skip when same state to avoid
  ;; unnecessary snapshot + evolve-working-set overhead.
  ;; GAP-B v0.97.10: Removed idle guard — idle transitions must trigger
  ;; WS reset via the any→idle rule in ws-evolution.rkt.
  (define ws-old-state (current-last-task-fsm-state))
  (when (and (current-ws-evolution-enabled?)
             ws-early
             session
             task-state
             ;; GAP-5: Skip same-state transitions (first transition: old=#f → proceed)
             (or (not ws-old-state) (not (eq? ws-old-state task-state))))
    (define result
      (evolve-working-set-for-state/result ws-early ws-old-state task-state augmented-conclusions))
    (current-last-task-fsm-state task-state)
    (when (and (evolution-result? result) session)
      (guarded-set-working-set-evolved! session result)))
  ;; v0.96.13 W4: Transition detection — trigger deterministic distillation on state change
  ;; Also resets the loop warning counter on state transition
  ;; MF1-1 fix: Use ws-old-state (captured before WS mutation) instead of
  ;; re-reading current-last-task-fsm-state, which was already mutated above.
  (when (and task-state ws-old-state (not (eq? ws-old-state task-state)))
    ;; State transition detected — reset warning counter
    (current-loop-warning-count 0))
  (values task-state-raw task-state augmented-conclusions))

;; ============================================================
;; Context assembly telemetry
;; ============================================================

;; Emit telemetry events for context assembly results.
;; Fires: working-set.injected, context.assembled, context-assembly-detail.
(define (emit-context-assembly-events! bus
                                       session-id
                                       iteration
                                       ctx-to-use
                                       ctx-assembled
                                       tc-struct
                                       ws
                                       config-raw)
  ;; v0.26.0: Emit working-set.injected event
  (when ws
    (emit-typed-event! bus
                       (make-working-set-injected-event #:session-id session-id
                                                        #:turn-id ""
                                                        #:timestamp (current-inexact-milliseconds)
                                                        #:entries (working-set-entry-count ws)
                                                        #:tokens (working-set-token-count ws))))
  ;; Emit context.assembled event
  (define ctx-token-count-promise
    (delay
      (estimate-context-tokens ctx-assembled)))
  (emit-typed-event! bus
                     (make-context-assembled-event
                      #:session-id session-id
                      #:turn-id ""
                      #:timestamp (current-inexact-milliseconds)
                      #:iteration iteration
                      #:total-messages (length ctx-to-use)
                      #:assembled-messages (length ctx-assembled)
                      #:token-count (force ctx-token-count-promise)
                      #:working-set-entries (if ws
                                                (working-set-entry-count ws)
                                                0)
                      #:working-set-tokens (if ws
                                               (working-set-token-count ws)
                                               0)))
  ;; v0.45.5: Emit detailed assembly metrics
  (define tier-a-len (length (tiered-context-tier-a tc-struct)))
  (define tier-b-len (length (tiered-context-tier-b tc-struct)))
  (define tier-c-len (length (tiered-context-tier-c tc-struct)))
  (define assembled-ids
    (for/set ([m (in-list ctx-assembled)])
      (message-id m)))
  (define excluded-id-list
    (for/list ([m (in-list ctx-to-use)]
               #:unless (set-member? assembled-ids (message-id m)))
      (message-id m)))
  ;; P5: Compact excluded-ids summary instead of flat CSV
  ;; Format: "count:by-role:N_USER,N_ASST,N_TOOL;samples:id1,id2,id3,...|idN-2,idN-1,idN"
  (define role-counts
    (let ([user-count 0]
          [assistant-count 0]
          [tool-count 0])
      (for ([m (in-list ctx-to-use)]
            #:unless (set-member? assembled-ids (message-id m)))
        (case (message-role m)
          [(user) (set! user-count (add1 user-count))]
          [(assistant tool) (set! assistant-count (add1 assistant-count))]
          [else (set! tool-count (add1 tool-count))]))
      (format "~a,~a,~a" user-count assistant-count tool-count)))
  (define excluded-ids-str
    (let ([n (length excluded-id-list)])
      (cond
        [(zero? n) ""]
        [(<= n 6) (format "~a:~a" role-counts (string-join excluded-id-list ","))]
        [else
         (define first-3 (take excluded-id-list 3))
         (define last-3 (take-right excluded-id-list 3))
         (format "~a:~a|~a" role-counts (string-join first-3 ",") (string-join last-3 ","))])))
  (define summary-len
    (for/sum ([m (in-list ctx-assembled)] #:when (eq? (message-kind m) 'compaction-summary))
             (for/sum ([p (in-list (message-content m))] #:when (text-part? p))
                      (string-length (text-part-text p)))))
  (define gsd-pinned (for/sum ([m (in-list ctx-assembled)] #:when (gsd-progress-message? m)) 1))
  (emit-typed-event! bus
                     (make-context-assembly-detail-event
                      #:session-id session-id
                      #:turn-id ""
                      #:timestamp (current-inexact-milliseconds)
                      #:total-messages (length ctx-to-use)
                      #:tier-a-count tier-a-len
                      #:tier-b-count tier-b-len
                      #:tier-c-count tier-c-len
                      ;; Tier A can intentionally overlap retention tiers (for
                      ;; example, a working-set result already in history).
                      ;; Exclusions are raw source records not present in the
                      ;; provider-ordered output, never a subtraction of tier sizes.
                      #:excluded-count (length excluded-id-list)
                      #:excluded-ids excluded-ids-str
                      #:summary-length summary-len
                      #:gsd-pinned-count gsd-pinned
                      #:ws-entry-count (if ws
                                           (working-set-entry-count ws)
                                           0)
                      #:ws-tokens (if ws
                                      (working-set-token-count ws)
                                      0)
                      #:cache-hit-p #f)))
