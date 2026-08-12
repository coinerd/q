#lang racket/base

;; runtime/session/session-prompt-preparation.rkt — Pure prompt preparation plan
;; v0.99.92 W1 — Pure prompt preparation extraction (MA-10 trace equivalence).
;;
;; This module computes, from pure inputs, every value the prompt lifecycle
;; must apply when preparing a user prompt: the canonical user message, the
;; post-append session index, the path-derived model setting, the context
;; message list, and the system-injected final context. It performs no I/O and
;; mutates no session or index state (the caller owns applying the effects in
;; the historical order). It is R-18 pure: it must never gain racket/port,
;; racket/file, racket/tcp, or racket/os imports.

(require racket/contract
         racket/struct
         (only-in "../../util/message/message.rkt" message? message-id message-kind)
         (only-in "session-lifecycle-transitions.rkt"
                  build-user-message
                  compute-parent-id
                  inject-system-instructions)
         (only-in "../session-index/mutations.rkt" append-to-leaf/pure)
         (only-in "../session-index/schema.rkt"
                  session-index
                  session-index?
                  session-index-active-leaf-id)
         (only-in "../context/context-assembly.rkt"
                  (build-session-context build-session-context/from-index)
                  build-tiered-context-with-hooks
                  tiered-context->message-list)
         (only-in "../working-set.rkt" working-set? working-set-resolve-messages)
         (only-in "session-context.rkt" extract-path-settings)
         (only-in "../../llm/token-budget.rkt" DEFAULT-TOKEN-BUDGET-THRESHOLD))

(provide (contract-out
          [build-prompt-preparation-plan
           (->* ((or/c string? message?))
                (#:history (listof message?)
                           #:index (or/c session-index? #f)
                           #:system-instructions (listof string?)
                           #:provider? boolean?
                           #:working-set (or/c working-set? #f)
                           #:max-tokens exact-nonnegative-integer?)
                prompt-preparation-plan?)]
          [prompt-preparation-plan? (-> any/c boolean?)]
          [prompt-preparation-plan-canonical-user-message (-> prompt-preparation-plan? message?)]
          [prompt-preparation-plan-post-append-index
           (-> prompt-preparation-plan? (or/c session-index? #f))]
          [prompt-preparation-plan-appended-entry (-> prompt-preparation-plan? (or/c message? #f))]
          [prompt-preparation-plan-parent-id (-> prompt-preparation-plan? (or/c string? #f))]
          [prompt-preparation-plan-context-messages (-> prompt-preparation-plan? (listof message?))]
          [prompt-preparation-plan-model-name (-> prompt-preparation-plan? (or/c string? #f))]
          [prompt-preparation-plan-context-with-system
           (-> prompt-preparation-plan? (listof message?))]))

;; The pure preparation plan. All fields are values; applying them to a session
;; is the caller's responsibility in the historical effect order.
(struct prompt-preparation-plan
        (canonical-user-message post-append-index
                                appended-entry
                                parent-id
                                context-messages
                                model-name
                                context-with-system)
  #:transparent)

;; prompt-preparation-plan : (or/c string? message?)
;;   #:history  (listof message?) — pre-loaded session log (caller probed log)
;;   #:index    (or/c session-index? #f) — resolved index (caller ensured/build)
;;   #:system-instructions — session system instructions
;;   #:provider? — provider presence selects the tiered context source
;;   #:working-set — post-reset working set (caller reset before invoking)
;;
;; Pure computation replicating the historical build-session-context-for-prompt
;; decision set without performing any side effect.
(define (build-prompt-preparation-plan user-message
                                       #:history [history '()]
                                       #:index [idx #f]
                                       #:system-instructions [system-instrs '()]
                                       #:provider? [provider? #f]
                                       #:working-set [ws #f]
                                       #:max-tokens [max-tokens DEFAULT-TOKEN-BUDGET-THRESHOLD])
  ;; 1. Resolve the base user message and its parent (string input only).
  (define-values (initial-msg parent-id)
    (if (string? user-message)
        (let ([pid (compute-parent-id history idx)])
          (values (build-user-message user-message pid) pid))
        (values user-message #f)))
  ;; 2. Canonicalize via the pure index append when an index is present.
  (define-values (post-append-index appended-entry)
    (if idx
        (append-to-leaf/pure idx initial-msg)
        (values #f #f)))
  (define canonical-msg (or appended-entry initial-msg))
  ;; 3. Build the context from a post-append index whose active-leaf box points
  ;;    at the newly appended message. Creating a fresh box keeps this module
  ;;    pure (the caller's index box is untouched) while reproducing the
  ;;    historical behavior where the tree walk starts from the new user
  ;;    message, so the canonical user message is the context tail.
  (define context-index
    (if appended-entry
        (struct-copy session-index
                     post-append-index
                     [active-leaf-id (box (message-id appended-entry))])
        post-append-index))
  ;; 4. Build the context messages from the selected source:
  ;;    tiered (index + provider), tree walk (index, no provider), or
  ;;    linear (no index — history + buffered user message).
  (define context-messages
    (if context-index
        (if provider?
            (let* ([raw-msgs (build-session-context/from-index context-index)]
                   [ws-msgs (if ws
                                (working-set-resolve-messages ws raw-msgs message-id)
                                '())]
                   [tiered (car (call-with-values
                                 (lambda ()
                                   (build-tiered-context-with-hooks raw-msgs
                                                                    #:max-tokens max-tokens
                                                                    #:working-set-messages ws-msgs))
                                 list))])
              (tiered-context->message-list tiered))
            (build-session-context/from-index context-index))
        (if (null? history)
            (list canonical-msg)
            (append history (list canonical-msg)))))
  ;; 4. Path-derived settings (model override).
  (define model-name (hash-ref (extract-path-settings context-messages) 'model #f))
  ;; 5. Ephemeral system-instruction prefix.
  (define context-with-system (inject-system-instructions context-messages system-instrs))
  (prompt-preparation-plan canonical-msg
                           post-append-index
                           appended-entry
                           parent-id
                           context-messages
                           model-name
                           context-with-system))
