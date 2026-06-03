#lang racket/base

;; runtime/session-lifecycle-transitions.rkt -- Pure FSM transition functions
;; v0.74.4: Extracted from session-lifecycle.rkt for testability.
;;
;; These functions are pure state transitions: given data inputs,
;; they produce data outputs without I/O side effects.

(require (only-in "../../util/message.rkt"
                  make-message
                  message?
                  message-id
                  message-kind
                  message-parent-id
                  message-role
                  message-content)
         (only-in "../../util/content-parts.rkt" make-text-part)
         (only-in "../../util/ids.rkt" generate-id now-seconds)
         (only-in "../session-index/query.rkt" active-leaf)
         racket/list
         racket/string
         racket/contract)

(provide (contract-out
          [build-user-message (-> string? (or/c string? #f) message?)]
          [compute-parent-id (->* ((listof message?)) ((or/c any/c #f)) (or/c string? #f))]
          [inject-system-instructions (-> (listof message?) (listof string?) (listof message?))]))

;; build-user-message : string? (or/c message-id? #f) -> message?
;; Pure: creates a user message from a string prompt.
(define (build-user-message text parent-id)
  (make-message (generate-id)
                parent-id
                'user
                'message
                (list (make-text-part text))
                (now-seconds)
                (hasheq)))

;; compute-parent-id : (listof message?) (or/c index? #f) -> (or/c message-id? #f)
;; Pure: determines the parent message ID from entries or index.
(define (compute-parent-id entries [idx #f])
  (if idx
      (let ([leaf (active-leaf idx)])
        (cond
          [(not leaf) #f]
          [(eq? (message-kind leaf) 'session-info) #f]
          [else (message-id leaf)]))
      (let ([existing (filter (lambda (m) (not (eq? (message-kind m) 'session-info))) entries)])
        (if (null? existing)
            #f
            (message-id (last existing))))))

;; inject-system-instructions : (listof message?) (listof string?) -> (listof message?)
;; Pure: prepends a system message. Always ensures the first message is a system
;; message, even when instructions are empty. This prevents provider 500 errors
;; from strict chat templates that require messages[0].role == 'system'.
(define (inject-system-instructions context-messages system-instrs)
  (define system-content
    (if (null? system-instrs)
        ""
        (string-join system-instrs "\n\n")))
  (cons (make-message (generate-id)
                      #f
                      'system
                      'system-instruction
                      (list (make-text-part system-content))
                      (now-seconds)
                      (hasheq))
        context-messages))
