#lang racket

;; tests/test-event-coverage.rkt — #1149: fine-grained lifecycle event coverage
;; Verifies that hook-action-schemas and hook-point-names contain 37+ entries.

(require rackunit
         "../util/hook-types.rkt")

(test-case "hook-point-names returns 37+ points"
  (define names (hook-point-names))
  (check-true (>= (length names) 37) (format "expected >= 37 hook points, got ~a" (length names))))

(test-case "tool lifecycle events exist"
  (for ([name '(tool.execution.start tool.execution.update tool.execution.end)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "message lifecycle events exist"
  (for ([name '(message-start message.update message-end message.stream.delta)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "context events exist"
  (for ([name '(context context.window.changed context.tokens.used)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "provider events exist"
  (for ([name
         '(before-provider-request provider.response.after provider.stream.delta provider.error)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "model events exist"
  (for ([name '(model.selected model.changed)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "session lifecycle events exist"
  (for ([name '(session.created session.loaded
                                session.compacted
                                session-before-compact
                                session-before-fork)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "extension lifecycle events exist"
  (for ([name '(extension.loaded extension.unloaded register-tools)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "agent lifecycle events exist"
  (for ([name '(agent.started agent.idle agent.error)])
    (check-not-false (member name (hook-point-names)) (format "~a not in hook points" name))))

(test-case "resources-discover event exists"
  (check-not-false (member 'resources-discover (hook-point-names))))

(test-case "all new hooks have valid action schemas"
  (define advisory-hooks
    '(tool.execution.start tool.execution.end
                           message.stream.delta
                           context.window.changed
                           provider.stream.delta
                           provider.error
                           model.selected
                           model.changed
                           session.created
                           session.loaded
                           session.compacted
                           extension.loaded
                           extension.unloaded
                           agent.started
                           agent.idle
                           agent.error))
  (for ([name advisory-hooks])
    (define schema (hash-ref hook-action-schemas name #f))
    (check-not-false schema (format "~a missing from hook-action-schemas" name))
    (check-not-false (member 'pass schema) (format "~a should allow 'pass" name))))

(test-case "amend hooks have correct action schemas"
  (define amend-hooks
    '(message.update tool.execution.update context.tokens.used provider.response.after))
  (for ([name amend-hooks])
    (define schema (hash-ref hook-action-schemas name #f))
    (check-not-false schema (format "~a missing from hook-action-schemas" name))
    (check-not-false (member 'amend schema) (format "~a should allow 'amend" name))))
