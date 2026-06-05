#lang racket

;; q/tests/test-ui-actions.rkt — Tests for ui-core/ui-actions.rkt and ui-core/ui-delta.rkt
;;
;; W1.1 (v0.94.1): Verify UI action definitions, validators, emitter, and reducer.

(require rackunit
         rackunit/text-ui
         "../ui-core/ui-actions.rkt"
         "../ui-core/ui-delta.rkt")

(define-test-suite
 test-ui-actions

 ;; ─── Action name constants ───

 (test-case "action name constants are strings starting with ui."
   (for ([name (in-list all-ui-action-names)])
     (check-true (string? name))
     (check-true (string-prefix? name "ui."))))

 (test-case "all-ui-action-names contains exactly 13 actions"
   (check-equal? (length all-ui-action-names) 13))

 (test-case "action names are unique"
   (check-equal? (length all-ui-action-names)
                 (length (remove-duplicates all-ui-action-names))))

 ;; ─── Validator ───

 (test-case "valid-ui-action-name? accepts known names"
   (check-true (valid-ui-action-name? "ui.header.set"))
   (check-true (valid-ui-action-name? "ui.widget.unregister-all")))

 (test-case "valid-ui-action-name? rejects unknown names"
   (check-false (valid-ui-action-name? "ui.unknown"))
   (check-false (valid-ui-action-name? "not-ui"))
   (check-false (valid-ui-action-name? 42)))

 ;; ─── Schema ───

 (test-case "ui-action-schema has entry for every action"
   (for ([name (in-list all-ui-action-names)])
     (check-not-false (hash-ref ui-action-schema name #f)
                      (format "schema missing for ~a" name))))

 ;; ─── Feature flag ───

 (test-case "feature flag defaults to off"
   (check-false (current-ui-event-actions-enabled?)))

 (test-case "feature flag can be enabled in parameterize"
   (parameterize ([current-ui-event-actions-enabled? #t])
     (check-true (current-ui-event-actions-enabled?)))
   (check-false (current-ui-event-actions-enabled?)))

 ;; ─── Emitter ───

 (test-case "emit-ui-action! is no-op when flag is off"
   (define events '())
   (define mock-runtime
     (hash 'emit-event (lambda (evt) (set! events (cons evt events)))))
   (emit-ui-action! mock-runtime "ui.header.set" 'lines '("test"))
   (check-equal? events '()))

 (test-case "emit-ui-action! emits event when flag is on"
   (define events '())
   (define mock-runtime
     (hash 'emit-event (lambda (evt) (set! events (cons evt events)))))
   (parameterize ([current-ui-event-actions-enabled? #t])
     (emit-ui-action! mock-runtime "ui.header.set" 'lines '("hello")))
   (check-equal? (length events) 1)
   (check-equal? (hash-ref (car events) 'type) "ui.header.set")
   (check-equal? (hash-ref (car events) 'lines) '("hello")))

 (test-case "emit-ui-action! is no-op when runtime has no emit-event"
   (parameterize ([current-ui-event-actions-enabled? #t])
     ;; Should not error
     (emit-ui-action! (hash) "ui.header.set" 'lines '("test"))
     (check-true #t)))
 )

(define-test-suite
 test-ui-delta

 ;; ─── Delta struct ───

 (test-case "ui-delta construction"
   (define d (ui-delta 'set-header '("line1")))
   (check-equal? (ui-delta-type d) 'set-header)
   (check-equal? (ui-delta-payload d) '("line1")))

 (test-case "ui-delta is transparent"
   (check-equal? (ui-delta 'set-status 'idle)
                 (ui-delta 'set-status 'idle)))

 (test-case "ui-delta-type? accepts known types"
   (check-true (ui-delta-type? 'set-header))
   (check-true (ui-delta-type? 'register-widget)))

 (test-case "ui-delta-type? rejects unknown types"
   (check-false (ui-delta-type? 'unknown))
   (check-false (ui-delta-type? 42)))

 (test-case "all-delta-types has 12 entries"
   (check-equal? (length all-delta-types) 12))

 ;; ─── Pure reducer ───

 (test-case "ui-action->deltas: header.set produces set-header delta"
   (define deltas (ui-action->deltas "ui.header.set" (hash 'lines '("a" "b"))))
   (check-equal? (length deltas) 1)
   (check-equal? (ui-delta-type (car deltas)) 'set-header)
   (check-equal? (ui-delta-payload (car deltas)) '("a" "b")))

 (test-case "ui-action->deltas: header.clear produces clear-header delta"
   (define deltas (ui-action->deltas "ui.header.clear" (hash)))
   (check-equal? (length deltas) 1)
   (check-equal? (ui-delta-type (car deltas)) 'clear-header)
   (check-equal? (ui-delta-payload (car deltas)) #f))

 (test-case "ui-action->deltas: footer.set produces set-footer delta"
   (define deltas (ui-action->deltas "ui.footer.set" (hash 'lines '("f1"))))
   (check-equal? (ui-delta-type (car deltas)) 'set-footer))

 (test-case "ui-action->deltas: footer.clear produces clear-footer delta"
   (define deltas (ui-action->deltas "ui.footer.clear" (hash)))
   (check-equal? (ui-delta-type (car deltas)) 'clear-footer))

 (test-case "ui-action->deltas: status.set produces set-status delta"
   (define deltas (ui-action->deltas "ui.status.set" (hash 'status 'processing)))
   (check-equal? (ui-delta-type (car deltas)) 'set-status)
   (check-equal? (ui-delta-payload (car deltas)) 'processing))

 (test-case "ui-action->deltas: widget.register produces register-widget delta"
   (define desc (hash 'type 'text 'content "hello"))
   (define deltas (ui-action->deltas "ui.widget.register"
                                      (hash 'ext-name 'my-ext
                                            'key 'main
                                            'descriptor desc)))
   (check-equal? (ui-delta-type (car deltas)) 'register-widget)
   (define p (ui-delta-payload (car deltas)))
   (check-equal? (car p) 'my-ext)
   (check-equal? (cadr p) 'main)
   (check-equal? (caddr p) desc))

 (test-case "ui-action->deltas: widget.unregister produces unregister-widget delta"
   (define deltas (ui-action->deltas "ui.widget.unregister"
                                      (hash 'ext-name 'my-ext 'key 'main)))
   (check-equal? (ui-delta-type (car deltas)) 'unregister-widget)
   (check-equal? (ui-delta-payload (car deltas)) (cons 'my-ext 'main)))

 (test-case "ui-action->deltas: widget.unregister-all produces unregister-widget delta with #f key"
   (define deltas (ui-action->deltas "ui.widget.unregister-all"
                                      (hash 'ext-name 'my-ext)))
   (check-equal? (ui-delta-type (car deltas)) 'unregister-widget)
   (check-equal? (ui-delta-payload (car deltas)) (cons 'my-ext #f)))

 (test-case "ui-action->deltas: theme.change produces set-theme delta"
   (define deltas (ui-action->deltas "ui.theme.change" (hash 'theme 'dark)))
   (check-equal? (ui-delta-type (car deltas)) 'set-theme)
   (check-equal? (ui-delta-payload (car deltas)) 'dark))

 (test-case "ui-action->deltas: layout.breakpoint produces set-layout delta"
   (define deltas (ui-action->deltas "ui.layout.breakpoint" (hash 'breakpoint 'wide)))
   (check-equal? (ui-delta-type (car deltas)) 'set-layout)
   (check-equal? (ui-delta-payload (car deltas)) 'wide))

 (test-case "ui-action->deltas: focus.request produces set-focus delta"
   (define deltas (ui-action->deltas "ui.focus.request" (hash 'component 'transcript)))
   (check-equal? (ui-delta-type (car deltas)) 'set-focus)
   (check-equal? (ui-delta-payload (car deltas)) 'transcript))

 (test-case "ui-action->deltas: unknown action returns empty list"
   (check-equal? (ui-action->deltas "ui.unknown" (hash)) '()))

 (test-case "ui-action->deltas: overlay.show returns empty (not yet implemented)"
   (check-equal? (ui-action->deltas "ui.overlay.show" (hash 'content "test")) '()))

 (test-case "ui-action->deltas is pure — same input, same output"
   (define payload (hash 'lines '("a")))
   (check-equal? (ui-action->deltas "ui.header.set" payload)
                 (ui-action->deltas "ui.header.set" payload)))
 )

(run-tests test-ui-actions)
(run-tests test-ui-delta)
