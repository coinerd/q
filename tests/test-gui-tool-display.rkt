#lang racket

;; q/tests/test-gui-tool-display.rkt — W2: tool argument + result display
;;
;; Tests that tool events produce typed entries with proper kinds,
;; result truncation, and kind-aware coloring.

(require rackunit
         rackunit/text-ui
         racket/list
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../ui-core/theme-protocol.rkt"
         "../util/event/event.rkt")

(define (mk-event tag payload)
  (event 0 tag 0 #f #f payload))

(define (fresh-box)
  (box (make-gui-state)))

(define dark-theme (default-theme))
(define light-theme (default-theme))

(define-test-suite
 test-gui-tool-display

 ;; ─── Tool-start entries ───

 (test-case "tool.call.started produces kind=tool-start"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "tool.call.started" (hash 'name "bash" 'arguments (hash 'command "ls -la"))))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-kind (car msgs)) 'tool-start)
   (check-equal? (gui-message-role (car msgs)) "tool"))

 (test-case "tool-start shows arg summary in text"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "tool.call.started" (hash 'name "bash" 'arguments (hash 'command "ls -la"))))
   (define msg (car (gui-state-messages (unbox sb))))
   (check-not-false (regexp-match? #rx"bash" (gui-message-text msg)))
   (check-not-false (regexp-match? #rx"command" (gui-message-text msg))))

 ;; ─── Tool-end entries ───

 (test-case "tool.execution.completed produces kind=tool-end"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "tool.call.started" (hash 'name "read")))
   (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary "file contents here")))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 2)
   (check-equal? (gui-message-kind (cadr msgs)) 'tool-end)
   (check-not-false (regexp-match? #rx"read" (gui-message-text (cadr msgs)))))

 (test-case "result text is truncated to 80 chars"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "tool.call.started" (hash 'name "read")))
   (define long-result (make-string 200 #\x))
   (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary long-result)))
   (define msg (cadr (gui-state-messages (unbox sb))))
   ;; The result portion should be <= 80 chars + "..." = 83
   (define text (gui-message-text msg))
   (check-true (<= (string-length text) 120)
               (format "text length ~a should be reasonable" (string-length text))))

 ;; ─── Tool-fail entries ───

 (test-case "error resultSummary produces kind=tool-fail"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "tool.call.started" (hash 'name "bash")))
   (sub (mk-event "tool.execution.completed" (hash 'toolName "bash" 'resultSummary 'error)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 2)
   (check-equal? (gui-message-kind (cadr msgs)) 'tool-fail))

 ;; ─── Kind-aware coloring ───

 (test-case "kind->color returns accent for tool-start"
   (check-equal? (kind->color 'tool-start dark-theme) (theme-ref dark-theme 'accent)))

 (test-case "kind->color returns green for tool-end"
   (check-equal? (kind->color 'tool-end dark-theme) "#a6e3a1"))

 (test-case "kind->color returns red for tool-fail"
   (check-equal? (kind->color 'tool-fail dark-theme) "#f38ba8"))

 (test-case "kind->color returns #f for message"
   (check-false (kind->color 'message dark-theme)))

 ;; ─── Render plan with kind ───

 (test-case "render-message-descriptor uses kind color for tool-start"
   (define msg-h (hash 'role "tool" 'text "[bash] ls" 'kind 'tool-start 'meta (hasheq)))
   (define plan (render-message-descriptor msg-h dark-theme))
   (define segments (hash-ref plan 'segments))
   (check-true (>= (length segments) 1))
   ;; Role segment color should be the accent color
   (define role-seg (car segments))
   (define style (hash-ref role-seg 'style))
   (check-equal? (hash-ref style 'color) (theme-ref dark-theme 'accent)))

 (test-case "render-message-descriptor uses green for tool-end"
   (define msg-h (hash 'role "tool" 'text "[read] → OK" 'kind 'tool-end 'meta (hasheq)))
   (define plan (render-message-descriptor msg-h dark-theme))
   (define segments (hash-ref plan 'segments))
   (define role-seg (car segments))
   (define style (hash-ref role-seg 'style))
   (check-equal? (hash-ref style 'color) "#a6e3a1")))

(run-tests test-gui-tool-display)
