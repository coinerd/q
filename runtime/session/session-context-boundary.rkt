#lang racket/base

;; runtime/session/session-context-boundary.rkt — Explicit context-build request/result
;; v0.99.92 W2 — Context-Build Request/Result Boundary (MA-10 trace equivalence).
;;
;; Makes the Context Assembly boundary explicit: a caller builds a
;; context-build-request (pure inputs: user message, pre-loaded history, resolved
;; index, system instructions, provider presence, post-reset working set, and
;; max tokens) and receives a context-build-result (pure outputs the caller
;; applies through its own effects E2/E3/E4). Context Assembly stays
;; Runtime-owned and cross-turn state stays session-owned. This module performs
;; no I/O and mutates no session/index/working-set state; it is R-18 pure.

(require racket/contract
         (only-in "session-prompt-preparation.rkt"
                  build-prompt-preparation-plan
                  prompt-preparation-plan-canonical-user-message
                  prompt-preparation-plan-post-append-index
                  prompt-preparation-plan-appended-entry
                  prompt-preparation-plan-parent-id
                  prompt-preparation-plan-context-messages
                  prompt-preparation-plan-model-name
                  prompt-preparation-plan-context-with-system)
         (only-in "../session-index/schema.rkt" session-index?)
         (only-in "../../util/message/message.rkt" message?)
         (only-in "../working-set.rkt" working-set?))

(provide (contract-out
          [context-build-request
           (-> (or/c string? message?)
               (listof message?)
               (or/c session-index? #f)
               (listof string?)
               boolean?
               (or/c working-set? #f)
               exact-nonnegative-integer?
               context-build-request?)]
          [context-build-request? (-> any/c boolean?)]
          [context-build-request-user-message (-> context-build-request? (or/c string? message?))]
          [context-build-request-history (-> context-build-request? (listof message?))]
          [context-build-request-index (-> context-build-request? (or/c session-index? #f))]
          [context-build-request-system-instructions (-> context-build-request? (listof string?))]
          [context-build-request-provider? (-> context-build-request? boolean?)]
          [context-build-request-working-set (-> context-build-request? (or/c working-set? #f))]
          [context-build-request-max-tokens (-> context-build-request? exact-nonnegative-integer?)]
          [context-build-result
           (-> message?
               (or/c session-index? #f)
               (or/c message? #f)
               (or/c string? #f)
               (listof message?)
               (or/c string? #f)
               (listof message?)
               context-build-result?)]
          [context-build-result? (-> any/c boolean?)]
          [context-build-result-canonical-user-message (-> context-build-result? message?)]
          [context-build-result-post-append-index (-> context-build-result? (or/c session-index? #f))]
          [context-build-result-appended-entry (-> context-build-result? (or/c message? #f))]
          [context-build-result-parent-id (-> context-build-result? (or/c string? #f))]
          [context-build-result-context-messages (-> context-build-result? (listof message?))]
          [context-build-result-model-name (-> context-build-result? (or/c string? #f))]
          [context-build-result-context-with-system (-> context-build-result? (listof message?))]
          [context-build (-> context-build-request? context-build-result?)]))

(struct context-build-request
        (user-message history index system-instructions provider? working-set max-tokens)
  #:transparent)

(struct context-build-result
        (canonical-user-message post-append-index
                                appended-entry
                                parent-id
                                context-messages
                                model-name
                                context-with-system)
  #:transparent)

;; context-build : context-build-request? -> context-build-result?
;;
;; Pure composition of the v0.99.92 W1 preparation plan under an explicit
;; request/result boundary. Returns every value the caller needs to apply its
;; effects (E2 index install/save, E3 buffer/append, E4 model-name) in the
;; historical order, without performing any side effect itself.
(define (context-build req)
  (define p
    (build-prompt-preparation-plan (context-build-request-user-message req)
                                   #:history (context-build-request-history req)
                                   #:index (context-build-request-index req)
                                   #:system-instructions
                                   (context-build-request-system-instructions req)
                                   #:provider? (context-build-request-provider? req)
                                   #:working-set (context-build-request-working-set req)
                                   #:max-tokens (context-build-request-max-tokens req)))
  (context-build-result (prompt-preparation-plan-canonical-user-message p)
                        (prompt-preparation-plan-post-append-index p)
                        (prompt-preparation-plan-appended-entry p)
                        (prompt-preparation-plan-parent-id p)
                        (prompt-preparation-plan-context-messages p)
                        (prompt-preparation-plan-model-name p)
                        (prompt-preparation-plan-context-with-system p)))
