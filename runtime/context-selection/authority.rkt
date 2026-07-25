#lang racket/base

;; runtime/context-selection/authority.rkt — Single context-selection authority
;; W7 (#8944): One decision point determines what goes into the provider context.
;;
;; Design:
;;   This module is the sole authority for context selection. It wraps:
;;     - hybrid-recall (memory ranking)
;;     - projection (active-task checkpoint)
;;     - working-set resolution
;;     - session-walk (session tree walk)
;;     - context-floor (tiered assembly)
;;   into ONE coherent decision point.
;;
;;   Existing runtime/context-assembly/ modules remain as building blocks.
;;   This authority ORCHESTRATES them — it does NOT replace them.
;;
;; Responsibilities:
;;   - Receive session state, working set, memory backend, project identity
;;   - Compute retention tiers (A/B/C) with explicit decision trace
;;   - Select memory items via hybrid-recall (from both session and project backends)
;;   - Render active-task checkpoint from projection
;;   - Produce a structured ContextPackage describing what was selected/injected/excluded
;;   - Expose trace data for event emission and debugging
;;
;; Pure core (no I/O, no parameter references):
;;   Context selection logic lives in pure functions. The orchestration layer
;;   that calls I/O backends is separate for testability.

(require racket/list
         racket/match
         racket/string
         racket/function
         (only-in "../../util/message/message.rkt"
                  message
                  message?
                  message-id
                  message-kind
                  message-role
                  message-content
                  message-meta-safe
                  make-message)
         (only-in "../../util/content/content-parts.rkt" make-text-part)
         (only-in "../../util/ids.rkt" generate-id)
         "../memory/types.rkt"
         "../memory/hybrid-recall.rkt"
         "../memory/project-identity.rkt"
         "../context-assembly/task-checkpoint.rkt"
         "../task-memory/projection.rkt")

(provide selection-authority
         authority-selection?
         authority-selection-in-scope
         authority-selection-excluded
         authority-selection-trace
         authority-selection-result
         selection-result?
         selection-result-tier-a-count
         selection-result-tier-b-count
         selection-result-tier-c-count
         selection-result-memory-count
         selection-result-checkpoint-included?
         selection-result-total-tokens
         selection-result-reasoning
         selection-result-package
         context-package?
         context-package-tiered-context
         context-package-checkpoint-message
         context-package-memory-messages
         ;; Pure decision helpers
         decide-retention
         retention-decision?
         retention-decision-tier-a-count
         retention-decision-tier-b-count
         retention-decision-tier-c-count
         retention-decision-reasoning
         retention-decision-compacted?
         select-memory
         memory-selection?
         memory-selection-items
         memory-selection-hybrid-latency
         memory-selection-session-count
         memory-selection-project-count
         memory-selection-expired-filtered
         include-checkpoint
         package-context)

;; ============================================================
;; Structs
;; ============================================================

;; Retention decision — pure output of decide-retention
(struct retention-decision
        (tier-a-count tier-b-count
                      tier-c-count ; exact-nonnegative-integer?
                      reasoning ; (listof string?)
                      compacted? ; boolean?
                      )
  #:transparent)

;; Memory selection — pure output of select-memory
(struct memory-selection
        (items ; (listof memory-item?)
         hybrid-latency ; (or/c number? #f)
         session-count ; exact-nonnegative-integer?
         project-count ; exact-nonnegative-integer?
         expired-filtered ; exact-nonnegative-integer?
         )
  #:transparent)

;; Context package — the composed result
(struct context-package
        (tiered-context ; tiered-context? (from context-floor)
         checkpoint-message ; (or/c message? #f)
         memory-messages ; (listof message?)
         )
  #:transparent)

;; Full authority selection — result of selection-authority
;; Full authority result
(struct selection-result
        (tier-a-count tier-b-count
                      tier-c-count
                      memory-count
                      checkpoint-included?
                      total-tokens
                      reasoning
                      package ; context-package?
                      )
  #:transparent)

;; Decision trace entry
(struct authority-selection (in-scope excluded trace result) #:transparent)

;; ============================================================
;; Preset retention profiles (v0.99.67 W6 WIP — refine as needed)
;; ============================================================

;; Compact profile: for recovered/short sessions
(define COMPACT-RETENTION (retention-decision 15 0 0 '("Compact: all messages in tier-a, no b/c") #t))

;; Normal profile: standard tiered retention
(define NORMAL-RETENTION
  (retention-decision 10
                      20
                      4
                      '("Normal: tier-a=10 sys+user+ws, tier-b=20 recent, tier-c=4 oldest")
                      #f))

;; Full profile: large session budget
(define FULL-RETENTION (retention-decision 20 50 12 '("Full: tier-a=20, tier-b=50, tier-c=12") #f))

;; ============================================================
;; Pure: Decide retention profile
;; ============================================================

;; Decide retention parameters based on session state and compaction status.
;; Returns a retention-decision with tier counts and reasoning.
;;
;; session-message-count: total messages in session
;; compacted?: whether the session has been compacted
;; recent-messages: (listof message?) — the most recent messages for sampling
(define (decide-retention session-message-count
                          compacted?
                          recent-messages
                          #:preferred-profile [preferred-profile 'auto])
  (define profile
    (case preferred-profile
      [(compact) COMPACT-RETENTION]
      [(full) FULL-RETENTION]
      [(normal) NORMAL-RETENTION]
      [else
       ;; Auto: decide based on session size and compaction state
       (cond
         [compacted?
          ;; After compaction, the surviving window IS the history.
          ;; Keep it all in tier-a with no b/c (the compactor already decided).
          (define compact-msgs (length recent-messages))
          (retention-decision compact-msgs
                              0
                              0
                              (list (format "Compacted session: ~a messages in tier-a only"
                                            compact-msgs))
                              #t)]
         ;; Large session: use full profile
         [(> session-message-count 200) FULL-RETENTION]
         [(> session-message-count 80) NORMAL-RETENTION]
         [else
          ;; Small session: keep everything visible
          (define a-count (min session-message-count 20))
          (retention-decision a-count
                              0
                              0
                              (list (format "Small session (~a msgs): all in tier-a"
                                            session-message-count))
                              #f)])]))
  ;; Ensure tier-a >= min-required (system + user + ws)
  (define min-tier-a 5)
  (if (< (retention-decision-tier-a-count profile) min-tier-a)
      (retention-decision min-tier-a
                          (retention-decision-tier-b-count profile)
                          (retention-decision-tier-c-count profile)
                          (cons (format "Adjusted tier-a to minimum ~a" min-tier-a)
                                (retention-decision-reasoning profile))
                          (retention-decision-compacted? profile))
      profile))

;; ============================================================
;; Pure: Select memory items via hybrid-recall
;; ============================================================

;; Select memory items from session and project backends via hybrid recall.
;; session-items: (listof memory-item?) — from session-scoped backend
;; project-items: (listof memory-item?) — from project-scoped backend
;; query-text: string — relevance query
;; limit: exact-positive-integer? — max results
;;
;; Returns memory-selection with items and counts.
(define (select-memory session-items project-items #:query-text [query-text ""] #:limit [limit 10])
  (define scored (hybrid-recall query-text session-items project-items #:limit limit))
  ;; Separate expired from non-expired
  (define-values (non-expired expired)
    (partition (lambda (si)
                 (not (expired-at?
                       (scored-item-item si)
                       (or (parse-iso-8601 (current-seconds-epoch->iso-8601 (current-seconds))) 0))))
               scored))
  (memory-selection (map scored-item-item non-expired)
                    #f ;; latency unknown at pure level
                    (length session-items)
                    (length project-items)
                    (length expired)))

;; ============================================================
;; Pure: Include active-task checkpoint
;; ============================================================

;; Decide whether to include a checkpoint based on task state.
;; Returns (or/c message? #f) — the checkpoint message or #f.
(define (include-checkpoint task-checkpoint ; (or/c active-task-checkpoint? #f)
                            #:token-budget [token-budget 800])
  (and task-checkpoint
       (active-task-checkpoint? task-checkpoint)
       ;; Only include if there's actual content
       (let* ([rec (render-task-checkpoint task-checkpoint #:token-budget token-budget)]
              [text (task-checkpoint-record-text rec)])
         (and (> (string-length text) 0)
              (make-message (generate-id)
                            #f
                            'system
                            'system-instruction
                            (list (make-text-part (string-append "# Active Task Checkpoint\n\n"
                                                                 text)))
                            (current-seconds)
                            (hasheq))))))

;; ============================================================
;; Pure: Package everything into a context-package
;; ============================================================

;; Compose a context-package from the raw building blocks.
;; The caller (orchestration layer) provides the tiered-context from context-floor
;; and the separately selected checkpoint/memory messages.
;;
;; Returns context-package ready for the orchestration layer.
(define (package-context tiered-context checkpoint-message memory-messages working-set-messages)
  (context-package tiered-context checkpoint-message memory-messages))

;; ============================================================
;; Top-level: selection-authority
;; ============================================================

;; The single context-selection authority.
;; This is the ONLY entry point for context decisions.
;;
;; Parameters (all pure / data-only):
;;   session-message-count: number of messages in the session
;;   compacted?: whether the session has been compacted
;;   recent-messages: (listof message?) — recent messages for retention profiling
;;   task-checkpoint: (or/c active-task-checkpoint? #f) — from projection
;;   session-items: (listof memory-item?) — from session backend
;;   project-items: (listof memory-item?) — from project backend
;;   memory-query: string — query for hybrid recall
;;   working-set-messages: (listof message?) — resolved working set entries
;;   profile: one of 'auto, 'compact, 'normal, 'full (default: 'auto)
;;   checkpoint-budget: token budget for checkpoint (default: 800)
;;   memory-limit: max memory items (default: 10)
;;
;; Returns authority-selection containing the decision trace and result.
(define (selection-authority session-message-count
                             compacted?
                             recent-messages
                             task-checkpoint
                             session-items
                             project-items
                             memory-query
                             working-set-messages
                             #:profile [profile 'auto]
                             #:checkpoint-budget [checkpoint-budget 800]
                             #:memory-limit [memory-limit 10])
  ;; Step 1: Decide retention profile
  (define retention
    (decide-retention session-message-count compacted? recent-messages #:preferred-profile profile))
  (define reasoning (retention-decision-reasoning retention))

  ;; Step 2: Select memory items
  (define mem-selection
    (select-memory session-items project-items #:query-text memory-query #:limit memory-limit))
  (define memory-items (memory-selection-items mem-selection))
  (define memory-msgs
    (for/list ([item (in-list memory-items)])
      (make-message (generate-id)
                    #f
                    'system
                    'system-instruction
                    (list (make-text-part (format "[Memory] ~a (scope: ~a, type: ~a)"
                                                  (memory-item-content item)
                                                  (memory-item-scope item)
                                                  (memory-item-type item))))
                    (current-seconds)
                    (hasheq 'memory-item-id (memory-item-id item)))))
  (push! reasoning
         (format "Memory: ~a session items, ~a project items, ~a expired filtered"
                 (memory-selection-session-count mem-selection)
                 (memory-selection-project-count mem-selection)
                 (memory-selection-expired-filtered mem-selection)))

  ;; Step 3: Include checkpoint
  (define cp-msg (include-checkpoint task-checkpoint #:token-budget checkpoint-budget))
  (when cp-msg
    (push! reasoning "Checkpoint included"))

  ;; Step 4: Package
  (define t-a (list))
  (define t-b (list))
  (define t-c (list))
  ;; Note: The actual tiered-context is built by context-floor in the
  ;; orchestration layer. At the pure level, we report what should happen.
  (define t-a-count
    (+ (retention-decision-tier-a-count retention)
       (if memory-items
           (length memory-items)
           0)
       (if cp-msg 1 0)))
  (define t-b-count (retention-decision-tier-b-count retention))
  (define t-c-count (retention-decision-tier-c-count retention))

  (define result
    (selection-result t-a-count
                      t-b-count
                      t-c-count
                      (length memory-items)
                      (not (not cp-msg))
                      0 ;; total-tokens unknown at pure level
                      (reverse reasoning)
                      #f))

  ;; in-scope: what's included
  (authority-selection (hasheq 'retention retention 'memory mem-selection 'checkpoint (and cp-msg #t))
                       ;; excluded: reasons for exclusions
                       (hasheq 'expired-memory
                               (memory-selection-expired-filtered mem-selection)
                               'excluded-by-profile
                               (and compacted? "Compacted session — only tier-a"))
                       ;; trace: full decision log
                       (reverse (retention-decision-reasoning retention))
                       ;; result
                       result))

;; ============================================================
;; Utils
;; ============================================================

(define (push! lst val)
  (set! lst (cons val lst)))

(define (current-seconds-epoch->iso-8601 secs)
  (define dt (seconds->date secs))
  (define (pad n)
    (let ([s (number->string n)])
      (if (< (string-length s) 2)
          (string-append "0" s)
          s)))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year dt)
          (pad (date-month dt))
          (pad (date-day dt))
          (pad (date-hour dt))
          (pad (date-minute dt))
          (pad (date-second dt))))

;; ============================================================
;; Compile guard
;; ============================================================

(void)
