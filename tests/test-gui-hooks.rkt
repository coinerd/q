#lang racket

;; q/tests/test-gui-hooks.rkt — Tests for GUI hook schemas
;;
;; W3.1 (v0.94.3): Verify GUI lifecycle hooks are registered with valid schemas.

(require rackunit
         rackunit/text-ui
         (only-in "../util/hook-types.rkt" hook-action-schemas valid-hook-name?))

(define gui-hook-names
  '(gui.window.opened gui.window.closed
                      gui.theme.changed
                      gui.layout.changed
                      gui.focus.changed
                      gui.input.submit
                      gui.command.execute
                      gui.scroll.request))

(define-test-suite
 test-gui-hooks
 (test-case "all 8 GUI hooks are registered"
   (for ([name gui-hook-names])
     (check-true (valid-hook-name? name) (format "GUI hook ~a should be registered" name))))
 (test-case "GUI hooks have valid action schemas"
   (for ([name gui-hook-names])
     (define actions (hash-ref hook-action-schemas name #f))
     (check-not-false actions (format "GUI hook ~a should have actions" name))
     (check-true (and (list? actions) (andmap symbol? actions))
                 (format "GUI hook ~a actions should be list of symbols" name))))
 (test-case "gui.input.submit supports pass, amend, block"
   (check-equal? (hash-ref hook-action-schemas 'gui.input.submit) '(pass amend block)))
 (test-case "gui.theme.changed supports pass and amend"
   (check-equal? (hash-ref hook-action-schemas 'gui.theme.changed) '(pass amend))))

(run-tests test-gui-hooks)
