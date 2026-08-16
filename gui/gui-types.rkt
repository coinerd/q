#lang racket/base

;; gui/gui-types.rkt — Struct types for GUI state
;;
;; Replaces raw hash constructions in gui/state-sync.rkt with
;; typed structs to prevent key-typo bugs (§8, §12).

(require racket/contract
         racket/list
         racket/string
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/conversation-reducer.rkt"
         "../ui-core/disclosure-state.rkt"
         "../ui-core/ui-intents.rkt")

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
                                      #:cost any/c
                                      #:conversation-reducer reducer-state?
                                      #:disclosure disclosure-state?
                                      #:active-session-id (or/c string? #f)
                                      #:active-turn-id (or/c string? #f))
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
                       [gui-state-set-conversation-reducer (-> gui-state? reducer-state? gui-state?)]
                       [gui-state-upsert-artifact (-> gui-state? conversation-artifact? gui-state?)]
                       [gui-state-latest-thinking-artifact-id (-> gui-state? (or/c string? #f))]
                       [gui-state-apply-intent (-> gui-state? ui-intent? gui-state?)]
                       [gui-state->hash (-> gui-state? hash?)]
                       [hash->gui-state (-> hash? gui-state?)]
                       [gui-message->hash (-> gui-message? hash?)]
                       [hash->gui-message (-> hash? gui-message?)]))

;; A single chat message in the GUI transcript.
;; kind: symbol — 'message | 'tool-start | 'tool-end | 'tool-fail | 'thinking | 'system | 'error | 'assistant | 'user
(struct gui-message (role text kind meta) #:transparent)

;; The full GUI state: messages, status, model name, context info, cost.
(struct gui-state
        (messages status
                  model
                  active-goal
                  context-info
                  cost
                  conversation-reducer
                  disclosure
                  active-session-id
                  active-turn-id)
  #:transparent)

;; --- Constructors with defaults ---

(define (make-gui-message role text [meta (hasheq)] #:kind [kind 'message])
  (gui-message role text kind meta))

(define (make-gui-state #:model [model #f]
                        #:messages [messages '()]
                        #:status [status 'idle]
                        #:active-goal [active-goal #f]
                        #:context-info [context-info #f]
                        #:cost [cost #f]
                        #:conversation-reducer [conversation-reducer (make-reducer-state)]
                        #:disclosure [disclosure (make-empty-disclosure-state)]
                        #:active-session-id [active-session-id #f]
                        #:active-turn-id [active-turn-id #f])
  (gui-state (take-right messages (min 500 (length messages)))
             status
             model
             active-goal
             context-info
             cost
             conversation-reducer
             disclosure
             active-session-id
             active-turn-id))

(define gui-message-retention-limit 500)

(define (bounded-messages messages)
  (take-right messages (min gui-message-retention-limit (length messages))))

;; --- Immutable update helpers ---

(define (gui-state-add-message gs msg)
  (struct-copy gui-state
               gs
               [messages (bounded-messages (append (gui-state-messages gs) (list msg)))]))

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

(define (gui-state-set-conversation-reducer gs reducer)
  (struct-copy gui-state gs [conversation-reducer reducer]))

(define (artifact-message? msg artifact-id)
  (equal? (hash-ref (gui-message-meta msg) 'artifact-id #f) artifact-id))

(define (artifact-expanded? gs artifact)
  (disclosure-expanded? (gui-state-disclosure gs) (conversation-artifact-id artifact)))

(define (artifact-display-text gs artifact)
  (define body (conversation-artifact-body artifact))
  (cond
    [(not (eq? (conversation-artifact-kind artifact) 'thinking)) body]
    [(artifact-expanded? gs artifact) (string-append body "\n\nHide reasoning · Ctrl+O to collapse")]
    [else (make-collapsed-preview body 3 (length (string-split body "\n" #:repeat? #f)))]))

(define (artifact->gui-message gs artifact)
  (make-gui-message "assistant"
                    (artifact-display-text gs artifact)
                    (hasheq 'artifact
                            artifact
                            'artifact-id
                            (conversation-artifact-id artifact)
                            'session-id
                            (conversation-artifact-session-id artifact)
                            'turn-id
                            (conversation-artifact-turn-id artifact))
                    #:kind (conversation-artifact-kind artifact)))

(define (insert-before-turn-assistant messages artifact message)
  (define session-id (conversation-artifact-session-id artifact))
  (define turn-id (conversation-artifact-turn-id artifact))
  (define index
    (for/first ([candidate (in-list messages)]
                [i (in-naturals)]
                #:when (and (eq? (gui-message-kind candidate) 'assistant)
                            (equal? (hash-ref (gui-message-meta candidate) 'session-id #f) session-id)
                            (equal? (hash-ref (gui-message-meta candidate) 'turn-id #f) turn-id)))
      i))
  (if index
      (append (take messages index) (list message) (drop messages index))
      (append messages (list message))))

;; Add or update the one GUI projection for a canonical artifact.  The full
;; body remains in the struct in metadata even while the visible text is folded.
(define (gui-state-upsert-artifact gs artifact)
  (define body (conversation-artifact-body artifact))
  (if (string=? (string-trim body) "")
      gs
      (let* ([id (conversation-artifact-id artifact)]
             [message (artifact->gui-message gs artifact)]
             [messages (gui-state-messages gs)]
             [existing? (ormap (lambda (candidate) (artifact-message? candidate id)) messages)]
             [updated (cond
                        [existing?
                         (for/list ([candidate (in-list messages)])
                           (if (artifact-message? candidate id) message candidate))]
                        [(eq? (conversation-artifact-kind artifact) 'thinking)
                         (insert-before-turn-assistant messages artifact message)]
                        [else (append messages (list message))])])
        (struct-copy gui-state gs [messages (bounded-messages updated)]))))

(define (gui-state-latest-thinking-artifact-id gs)
  (for/first ([message (in-list (reverse (gui-state-messages gs)))]
              #:when (and (eq? (gui-message-kind message) 'thinking)
                          (string? (hash-ref (gui-message-meta message) 'artifact-id #f))))
    (hash-ref (gui-message-meta message) 'artifact-id)))

(define (gui-state-apply-intent gs intent)
  (cond
    [(toggle-detail-intent? intent)
     (define explicit (ui-intent-target intent))
     (define active
       (for/first ([message (in-list (reverse (gui-state-messages gs)))]
                   #:when (let ([artifact (hash-ref (gui-message-meta message) 'artifact #f)])
                            (and (conversation-artifact? artifact)
                                 (eq? (conversation-artifact-kind artifact) 'thinking)
                                 (eq? (conversation-artifact-lifecycle artifact) 'streaming))))
         (hash-ref (gui-message-meta message) 'artifact-id #f)))
     (define candidates
       (for/list ([message (in-list (gui-state-messages gs))]
                  #:when (eq? (gui-message-kind message) 'thinking))
         (hash-ref (gui-message-meta message) 'artifact-id #f)))
     (define target (resolve-toggle-target (gui-state-disclosure gs) explicit active candidates))
     (if target
         (let* ([next-disclosure (disclosure-toggle (gui-state-disclosure gs) target)]
                [with-disclosure (struct-copy gui-state gs [disclosure next-disclosure])])
           (struct-copy gui-state
                        with-disclosure
                        [messages
                         (for/list ([message (in-list (gui-state-messages with-disclosure))])
                           (define artifact (hash-ref (gui-message-meta message) 'artifact #f))
                           (if (and (conversation-artifact? artifact)
                                    (equal? (conversation-artifact-id artifact) target))
                               (artifact->gui-message with-disclosure artifact)
                               message))]))
         gs)]
    [else gs]))

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
             (hash-ref h 'cost #f)
             (make-reducer-state)
             (make-empty-disclosure-state)
             #f
             #f))
