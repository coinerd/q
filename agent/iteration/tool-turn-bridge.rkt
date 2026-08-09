#lang racket/base

;; agent/iteration/tool-turn-bridge.rkt — tool call dispatch coordination
;;
;; Helpers for seen-paths tracking, exploration counting, tool-error
;; accounting, steering/injection draining, and tool-turn bridging.
;;
;; ARCHITECTURAL NOTE (v0.99.9x): working-set mutation and read-spiral
;; detection are Runtime-owned (the Runtime step executor). Agent iteration
;; no longer imports runtime/working-set.rkt.

(require racket/contract
         racket/list
         (only-in racket/string string-join)
         (only-in "../../util/content/content-parts.rkt" text-part? tool-result-part?)
         (only-in "../../util/event/event.rkt" event-ev)
         (only-in "../../util/message/message.rkt" message-role message? message-content)
         (only-in "../../util/tool/tool-types.rkt" tool-call-name tool-call-arguments)
         (only-in "../../util/content/content-parts.rkt" text-part-text tool-result-part-is-error?)
         (only-in "../../util/event/event.rkt" event-payload)
         "../event-bus.rkt"
         (only-in "../queue.rkt" dequeue-steering! dequeue-followup! dequeue-all-followups!)
         (only-in "../../util/event/event-types.rkt" injection-event-topic)
         (only-in "../../util/shared.rkt" take-at-most))

(provide (contract-out [extract-tool-target-path (-> any/c (or/c path-string? #f))]
                       [take-at-most (-> list? exact-nonnegative-integer? list?)]
                       [update-seen-paths (-> list? list? (values list? boolean?))]
                       [count-tool-errors (-> (listof any/c) exact-nonnegative-integer?)]
                       [compute-tool-counters
                        (-> list?
                            exact-nonnegative-integer?
                            exact-nonnegative-integer?
                            (values exact-nonnegative-integer? exact-nonnegative-integer?))]
                       [extract-last-assistant-text (-> list? any/c)]
                       [dequeue-all-steering! (-> any/c list?)]
                       [drain-injected-messages! (-> any/c any/c any/c list?)]
                       [make-injected-collector! (-> any/c any/c)]))

;; Extract the target file path from a tool call's arguments.
(define (extract-tool-target-path tc)
  (define args (tool-call-arguments tc))
  (define raw
    (cond
      [(hash? args) (or (hash-ref args 'path #f) (hash-ref args 'file #f))]
      [else #f]))
  (if (and (string? raw) (string=? raw "")) #f raw))

;; take-at-most: imported from util/shared.rkt (v0.32.1 Wave 1 DRY)

;; Compute new seen-paths list and whether to increment the consecutive counter.
;; Uses a plain list of strings to avoid TR boundary contract issues with racket/set.
(define (update-seen-paths tool-calls seen-paths)
  (define read-tools '("read" "find" "grep" "ls" "planning-read"))
  (define has-non-read?
    (for/or ([tc (in-list tool-calls)])
      (not (member (tool-call-name tc) read-tools))))
  (cond
    [has-non-read? (values '() #f)]
    [else
     (define new-paths
       (for/list ([tc (in-list tool-calls)]
                  #:when (member (tool-call-name tc) read-tools))
         (define p (extract-tool-target-path tc))
         (or p "__no_path__")))
     (define has-new-path?
       (for/or ([p (in-list new-paths)])
         (not (member p seen-paths))))
     (define updated (remove-duplicates (append seen-paths new-paths)))
     (values updated has-new-path?)]))

;; Count errors in tool result messages.
(define (count-tool-errors messages)
  (for/sum ([tr (filter tool-result-part? (apply append (map message-content messages)))])
           (if (tool-result-part-is-error? tr) 1 0)))

;; Compute exploration and implementation counters from tool calls.
(define (compute-tool-counters tool-calls explore-count implement-count)
  (define new-explore
    (+ explore-count
       (for/sum ([tc (in-list tool-calls)])
                (if (member (tool-call-name tc) '("read" "grep" "find" "ls")) 1 0))))
  (define new-implement
    (+ implement-count
       (for/sum ([tc (in-list tool-calls)]) (if (member (tool-call-name tc) '("edit" "write")) 1 0))))
  (values new-explore new-implement))

;; Extract the last assistant text from context.
(define (extract-last-assistant-text ctx)
  (for/first ([msg (in-list (reverse ctx))]
              #:when (eq? (message-role msg) 'assistant))
    (define content (message-content msg))
    (define texts
      (for/list ([part (in-list content)]
                 #:when (text-part? part))
        (text-part-text part)))
    (if (null? texts)
        #f
        (string-join texts " "))))

;; Dequeue all steering messages from queue.
(define (dequeue-all-steering! q)
  (let loop ([acc '()])
    (define msg (dequeue-steering! q))
    (if msg
        (loop (cons msg acc))
        (reverse acc))))

;; Drain all pending injected messages from the event bus.
(define (drain-injected-messages! bus injected-box session-id)
  (define msgs (unbox injected-box))
  (if (null? msgs)
      '()
      (begin
        (set-box! injected-box '())
        (filter message? msgs))))

;; Create an injected-message collector: subscribes to message.injected events.
(define (make-injected-collector! bus #:inject-topic [inject-topic injection-event-topic])
  (define collected (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) inject-topic)
                  (define payload (event-payload evt))
                  (define msg (hash-ref payload 'message #f))
                  (when msg
                    (let loop ()
                      (define old (unbox collected))
                      (unless (box-cas! collected old (cons msg old))
                        (loop))))))
              #:filter (lambda (evt) (equal? (event-ev evt) inject-topic)))
  collected)
