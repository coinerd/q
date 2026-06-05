#lang racket

;; q/tests/test-ui-surface-actions.rkt — Tests for event-publishing dual-path
;; in extensions/ui-surface.rkt
;;
;; W1.2 (v0.94.1): Verify that UI surface functions emit events when
;; the feature flag is on, while preserving callback behavior.

(require rackunit
         rackunit/text-ui
         "../extensions/ui-surface.rkt"
         "../ui-core/ui-actions.rkt"
         (only-in "../tui/state.rkt" ui-state? initial-ui-state ui-state-extension-widgets))

;; Helper: collect events emitted by the mock runtime
(define (make-event-collector)
  (define events-box (box '()))
  (define runtime
    (hash 'emit-event (lambda (evt) (set-box! events-box (cons evt (unbox events-box))))))
  (values runtime
          (lambda () (reverse (unbox events-box)))))

;; Helper: create a registry where all required callbacks are set
(define (make-test-registry)
  (ui-callback-registry
   (lambda (box lines) (void))
   (lambda (box lines) (void))
   (lambda (box) (void))
   (lambda (box) (void))
   (lambda (segments) 'styled-line)
   (lambda (text style) 'styled-segment)
   #f #f #f #f))

(define-test-suite
 test-ui-surface-actions

 ;; ─── Dual-path: event emission when flag ON ───

 (test-case
  "ui-set-footer! emits event when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (ui-set-footer! (box #f) '("footer text")))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (define events (get-events))
  (check-equal? (length events) 1)
  (check-equal? (hash-ref (car events) 'type) "ui.footer.set"))

 (test-case
  "ui-set-header! emits event when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (ui-set-header! (box #f) '("header text")))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (length (get-events)) 1)
  (check-equal? (hash-ref (car (get-events)) 'type) "ui.header.set"))

 (test-case
  "ui-clear-footer! emits event when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (ui-clear-footer! (box #f)))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (length (get-events)) 1)
  (check-equal? (hash-ref (car (get-events)) 'type) "ui.footer.clear"))

 (test-case
  "ui-clear-header! emits event when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (ui-clear-header! (box #f)))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (length (get-events)) 1)
  (check-equal? (hash-ref (car (get-events)) 'type) "ui.header.clear"))

 ;; ─── No events when flag OFF (default) ───

 (test-case
  "ui-set-footer! does not emit when flag off"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime])
    ;; flag defaults to #f
    (ui-set-footer! (box #f) '("test")))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (get-events) '()))

 (test-case
  "ui-set-header! does not emit when flag off"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime])
    (ui-set-header! (box #f) '("test")))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (get-events) '()))

 ;; ─── No events when runtime is #f ───

 (test-case
  "no events when runtime is #f even with flag on"
  (define saved-reg (current-ui-registry))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime #f]
                 [current-ui-event-actions-enabled? #t])
    ;; Should not error
    (ui-set-footer! (box #f) '("test"))
    (check-true #t))
  (current-ui-registry saved-reg))

 ;; ─── Widget functions emit events ───

 (test-case
  "ui-set-extension-widget! emits widget.register when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (define state (initial-ui-state))
    (ui-set-extension-widget! state 'my-ext 'main '("line")))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (define events (get-events))
  (check-equal? (length events) 1)
  (check-equal? (hash-ref (car events) 'type) "ui.widget.register")
  (check-equal? (hash-ref (car events) 'ext-name) 'my-ext))

 (test-case
  "ui-remove-extension-widget! emits widget.unregister when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (define state (initial-ui-state))
    (ui-remove-extension-widget! state 'my-ext 'main))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (length (get-events)) 1)
  (check-equal? (hash-ref (car (get-events)) 'type) "ui.widget.unregister"))

 (test-case
  "ui-remove-all-extension-widgets! emits widget.unregister-all when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (define state (initial-ui-state))
    (ui-remove-all-extension-widgets! state 'my-ext))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (length (get-events)) 1)
  (check-equal? (hash-ref (car (get-events)) 'type) "ui.widget.unregister-all"))

 ;; ─── Callback still fires alongside event ───

 (test-case
  "callback still fires when event flag is on"
  (define-values (runtime get-events) (make-event-collector))
  (define cb-called (box #f))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (define reg
    (ui-callback-registry (lambda (box lines) (set-box! cb-called #t))
                          #f #f #f #f #f #f #f #f #f))
  (parameterize ([current-ui-registry reg]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (ui-set-footer! (box #f) '("test")))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-true (unbox cb-called))
  (check-equal? (length (get-events)) 1))

 ;; ─── Status message emits event ───

 (test-case
  "ui-set-status-message! emits status.set when flag on"
  (define-values (runtime get-events) (make-event-collector))
  (define saved-reg (current-ui-registry))
  (define saved-rt (current-ui-event-runtime))
  (parameterize ([current-ui-registry (make-test-registry)]
                 [current-ui-event-runtime runtime]
                 [current-ui-event-actions-enabled? #t])
    (ui-set-status-message! (box #f) "status msg"))
  (current-ui-event-runtime saved-rt)
  (current-ui-registry saved-reg)
  (check-equal? (length (get-events)) 1)
  (check-equal? (hash-ref (car (get-events)) 'type) "ui.status.set"))
 )

(run-tests test-ui-surface-actions)
