#lang racket

;; q/tests/test-gui-typed-messages.rkt — W1: typed gui-message kind field
;;
;; Tests that gui-message supports a kind field for typed transcript entries.

(require rackunit
         rackunit/text-ui
         "../gui/gui-types.rkt")

(define-test-suite
 test-gui-typed-messages
 ;; ─── Kind field defaults ───
 (test-case "kind defaults to 'message"
   (define msg (make-gui-message "user" "hello"))
   (check-equal? (gui-message-kind msg) 'message))
 (test-case "kind can be set via #:kind keyword"
   (define msg (make-gui-message "tool" "[bash] ls" #:kind 'tool-start))
   (check-equal? (gui-message-kind msg) 'tool-start))
 (test-case "kind works with meta argument"
   (define msg (make-gui-message "tool" "[bash] ls" (hasheq 'name "bash") #:kind 'tool-start))
   (check-equal? (gui-message-kind msg) 'tool-start)
   (check-equal? (hash-ref (gui-message-meta msg) 'name) "bash"))
 ;; ─── Hash round-trip ───
 (test-case "gui-message->hash includes kind"
   (define msg (make-gui-message "tool" "text" #:kind 'tool-end))
   (define h (gui-message->hash msg))
   (check-equal? (hash-ref h 'kind) 'tool-end))
 (test-case "hash->gui-message reads kind"
   (define h (hash 'role "tool" 'text "text" 'kind 'tool-fail 'meta (hasheq)))
   (define msg (hash->gui-message h))
   (check-equal? (gui-message-kind msg) 'tool-fail))
 (test-case "hash->gui-message defaults kind to 'message when missing"
   (define h (hash 'role "user" 'text "hi" 'meta (hasheq)))
   (define msg (hash->gui-message h))
   (check-equal? (gui-message-kind msg) 'message))
 (test-case "full hash round-trip preserves kind"
   (define msg (make-gui-message "system" "thinking..." #:kind 'thinking))
   (define h (gui-message->hash msg))
   (define msg2 (hash->gui-message h))
   (check-equal? (gui-message-kind msg2) 'thinking)
   (check-equal? (gui-message-role msg2) "system")
   (check-equal? (gui-message-text msg2) "thinking..."))
 ;; ─── All kind symbols ───
 (test-case "all kind symbols accepted"
   (for ([kind (in-list
                '(message tool-start tool-end tool-fail thinking system error assistant user))])
     (define msg (make-gui-message "role" "text" #:kind kind))
     (check-equal? (gui-message-kind msg) kind)))
 ;; ─── gui-state with context-info and cost ───
 (test-case "gui-state has context-info field"
   (define gs (make-gui-state #:context-info (hash 'level 'low 'percent 30)))
   (check-equal? (gui-state-context-info gs) (hash 'level 'low 'percent 30)))
 (test-case "gui-state has cost field"
   (define gs (make-gui-state #:cost 0.042))
   (check-equal? (gui-state-cost gs) 0.042))
 (test-case "gui-state-set-context-info"
   (define gs (make-gui-state))
   (define gs2 (gui-state-set-context-info gs (hash 'level 'high)))
   (check-equal? (gui-state-context-info gs2) (hash 'level 'high)))
 (test-case "gui-state-set-cost"
   (define gs (make-gui-state))
   (define gs2 (gui-state-set-cost gs 1.50))
   (check-equal? (gui-state-cost gs2) 1.50))
 (test-case "gui-state-set-model"
   (define gs (make-gui-state))
   (define gs2 (gui-state-set-model gs "gpt-4"))
   (check-equal? (gui-state-model gs2) "gpt-4"))
 ;; ─── gui-state hash round-trip with new fields ───
 (test-case "gui-state->hash includes context-info and cost"
   (define gs (make-gui-state #:context-info (hash 'level 'medium) #:cost 0.05 #:model "claude-3"))
   (define h (gui-state->hash gs))
   (check-equal? (hash-ref h 'context-info) (hash 'level 'medium))
   (check-equal? (hash-ref h 'cost) 0.05)
   (check-equal? (hash-ref h 'model) "claude-3"))
 (test-case "hash->gui-state reads context-info and cost"
   (define h
     (hash 'messages
           '()
           'status
           'idle
           'model
           #f
           'active-goal
           #f
           'context-info
           (hash 'level 'high)
           'cost
           1.0))
   (define gs (hash->gui-state h))
   (check-equal? (gui-state-context-info gs) (hash 'level 'high))
   (check-equal? (gui-state-cost gs) 1.0)))

(run-tests test-gui-typed-messages)
