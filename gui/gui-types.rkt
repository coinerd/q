#lang racket/base

;; gui/gui-types.rkt — Struct types for GUI state
;;
;; Replaces raw hash constructions in gui/state-sync.rkt with
;; typed structs to prevent key-typo bugs (§8, §12).

(require racket/contract
         racket/list)

(provide (struct-out gui-message)
         (struct-out gui-state)
         (contract-out [make-gui-message (->* (string? string?) (any/c #:kind symbol?) gui-message?)]
                       [make-gui-state
                        (->* ()
                             (#:model (or/c string? #f)
                                      #:messages list?
                                      #:status symbol?
                                      #:active-goal (or/c hash? #f)
                                      #:context-info (or/c hash? #f)
                                      #:cost any/c)
                             gui-state?)]
                       [gui-state-add-message (-> gui-state? gui-message? gui-state?)]
                       [gui-state-update-last-message
                        (-> gui-state? (-> gui-message? gui-message?) gui-state?)]
                       [gui-state-update-tool-message-by-name
                        (-> gui-state? string? (-> gui-message? gui-message?) gui-state?)]
                       [gui-state-set-status (-> gui-state? symbol? gui-state?)]
                       [gui-state-set-active-goal (-> gui-state? (or/c hash? #f) gui-state?)]
                       [gui-state-set-context-info (-> gui-state? (or/c hash? #f) gui-state?)]
                       [gui-state-set-cost (-> gui-state? any/c gui-state?)]
                       [gui-state-set-model (-> gui-state? string? gui-state?)]
                       [gui-state->hash (-> gui-state? hash?)]
                       [hash->gui-state (-> hash? gui-state?)]
                       [gui-message->hash (-> gui-message? hash?)]
                       [hash->gui-message (-> hash? gui-message?)]))

;; A single chat message in the GUI transcript.
;; kind: symbol — 'message | 'tool-start | 'tool-end | 'tool-fail | 'thinking | 'system | 'error | 'assistant | 'user
(struct gui-message (role text kind meta) #:transparent)

;; The full GUI state: messages, status, model name, context info, cost.
(struct gui-state (messages status model active-goal context-info cost) #:transparent)

;; --- Constructors with defaults ---

(define (make-gui-message role text [meta (hasheq)] #:kind [kind 'message])
  (gui-message role text kind meta))

(define (make-gui-state #:model [model #f]
                        #:messages [messages '()]
                        #:status [status 'idle]
                        #:active-goal [active-goal #f]
                        #:context-info [context-info #f]
                        #:cost [cost #f])
  (gui-state messages status model active-goal context-info cost))

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

(define (gui-state-update-tool-message-by-name gs tool-name updater)
  (define msgs (gui-state-messages gs))
  (define idx
    (for/first ([i (in-range (sub1 (length msgs)) -1 -1)]
                #:when (let ([m (list-ref msgs i)])
                         (and (equal? (gui-message-role m) "tool")
                              (equal? (hash-ref (gui-message-meta m) 'name #f) tool-name)
                              (not (hash-ref (gui-message-meta m) 'completed #f)))))
      i))
  (if (not idx)
      gs
      (let* ([pre (take msgs idx)]
             [post (drop msgs (add1 idx))]
             [target (list-ref msgs idx)]
             [updated (updater target)])
        (struct-copy gui-state gs [messages (append pre (list updated) post)]))))

(define (gui-state-set-status gs status)
  (struct-copy gui-state gs [status status]))

(define (gui-state-set-active-goal gs goal-info)
  (struct-copy gui-state gs [active-goal goal-info]))

(define (gui-state-set-context-info gs info)
  (struct-copy gui-state gs [context-info info]))

(define (gui-state-set-cost gs cost)
  (struct-copy gui-state gs [cost cost]))

(define (gui-state-set-model gs model)
  (struct-copy gui-state gs [model model]))

;; --- Hash conversion (backward compatibility) ---

(define (gui-message->hash msg)
  (hash 'role
        (gui-message-role msg)
        'text
        (gui-message-text msg)
        'kind
        (gui-message-kind msg)
        'meta
        (gui-message-meta msg)))

(define (hash->gui-message h)
  (gui-message (hash-ref h 'role "")
               (hash-ref h 'text "")
               (hash-ref h 'kind 'message)
               (hash-ref h 'meta (hasheq))))

(define (gui-state->hash gs)
  (hash 'messages
        (map gui-message->hash (gui-state-messages gs))
        'status
        (gui-state-status gs)
        'model
        (gui-state-model gs)
        'active-goal
        (gui-state-active-goal gs)
        'context-info
        (gui-state-context-info gs)
        'cost
        (gui-state-cost gs)))

(define (hash->gui-state h)
  (gui-state (map hash->gui-message (hash-ref h 'messages '()))
             (hash-ref h 'status 'idle)
             (hash-ref h 'model #f)
             (hash-ref h 'active-goal #f)
             (hash-ref h 'context-info #f)
             (hash-ref h 'cost #f)))
