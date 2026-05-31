#lang racket/base

;; gui/gui-types.rkt — Struct types for GUI state
;;
;; Replaces raw hash constructions in gui/state-sync.rkt with
;; typed structs to prevent key-typo bugs (§8, §12).

(require racket/contract
         racket/list)

(provide (struct-out gui-message)
         (struct-out gui-state)
         (contract-out [make-gui-message (->* (string? string?) (any/c) gui-message?)]
                       [make-gui-state
                        (->* ()
                             (#:model (or/c string? #f)
                                      #:messages list?
                                      #:status symbol?
                                      #:active-goal (or/c hash? #f))
                             gui-state?)]
                       [gui-state-add-message (-> gui-state? gui-message? gui-state?)]
                       [gui-state-update-last-message
                        (-> gui-state? (-> gui-message? gui-message?) gui-state?)]
                       [gui-state-set-status (-> gui-state? symbol? gui-state?)]
                       [gui-state-set-active-goal (-> gui-state? (or/c hash? #f) gui-state?)]
                       [gui-state->hash (-> gui-state? hash?)]
                       [hash->gui-state (-> hash? gui-state?)]
                       [gui-message->hash (-> gui-message? hash?)]
                       [hash->gui-message (-> hash? gui-message?)]))

;; A single chat message in the GUI transcript.
(struct gui-message (role text meta) #:transparent)

;; The full GUI state: messages, status, model name.
(struct gui-state (messages status model active-goal) #:transparent)

;; --- Constructors with defaults ---

(define (make-gui-message role text [meta (hasheq)])
  (gui-message role text meta))

(define (make-gui-state #:model [model #f]
                        #:messages [messages '()]
                        #:status [status 'idle]
                        #:active-goal [active-goal #f])
  (gui-state messages status model active-goal))

;; --- Immutable update helpers ---

(define (gui-state-add-message gs msg)
  (struct-copy gui-state gs [messages (append (gui-state-messages gs) (list msg))]))

(define (gui-state-update-last-message gs updater)
  (define msgs (gui-state-messages gs))
  (if (null? msgs)
      gs
      (let* ([all-but-last (drop-right msgs 1)]
             [last-msg (last msgs)]
             [updated (updater last-msg)])
        (struct-copy gui-state gs [messages (append all-but-last (list updated))]))))

(define (gui-state-set-status gs status)
  (struct-copy gui-state gs [status status]))

(define (gui-state-set-active-goal gs goal-info)
  (struct-copy gui-state gs [active-goal goal-info]))

;; --- Hash conversion (backward compatibility) ---

(define (gui-message->hash msg)
  (hash 'role (gui-message-role msg) 'text (gui-message-text msg) 'meta (gui-message-meta msg)))

(define (hash->gui-message h)
  (gui-message (hash-ref h 'role "") (hash-ref h 'text "") (hash-ref h 'meta (hasheq))))

(define (gui-state->hash gs)
  (hash 'messages
        (map gui-message->hash (gui-state-messages gs))
        'status
        (gui-state-status gs)
        'model
        (gui-state-model gs)
        'active-goal
        (gui-state-active-goal gs)))

(define (hash->gui-state h)
  (gui-state (map hash->gui-message (hash-ref h 'messages '()))
             (hash-ref h 'status 'idle)
             (hash-ref h 'model #f)
             (hash-ref h 'active-goal #f)))
