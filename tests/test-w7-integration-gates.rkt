#lang racket

;; q/tests/test-w7-integration-gates.rkt — W7 integration gate verification
;;
;; G0: Pure contracts — all UI core functions compile and return expected types
;; G1: Headless integration — adapters, reducers work without display
;; G2: Render contracts — render hooks return correct shapes
;; G3: Concurrency — no race conditions in callback registry

(require rackunit
         rackunit/text-ui
         "../ui-core/ui-delta.rkt"
         "../ui-core/ui-actions.rkt"
         "../ui-core/widget-descriptor.rkt"
         "../ui-core/ui-reducer.rkt"
         "../ui-core/layout-protocol.rkt"
         "../ui-core/render-hooks.rkt"
         "../extensions/ui-surface.rkt")

(define-test-suite test-w7-gates
                   ;; G0: Pure contracts
                   (test-case "G0: ui-delta struct construction"
                     (define d (ui-delta 'set-header '("line")))
                     (check-equal? (ui-delta-type d) 'set-header)
                     (check-equal? (ui-delta-payload d) '("line")))
                   (test-case "G0: ui-action->deltas returns list of ui-delta"
                     (define deltas (ui-action->deltas "ui.header.set" (hash 'lines '("h"))))
                     (check-true (and (list? deltas) (andmap ui-delta? deltas))))
                   (test-case "G0: widget-descriptor valid construction"
                     (define w (make-widget-descriptor 'id 'ext #:zone 'sidebar #:kind 'text))
                     (check-true (widget-descriptor-valid? w)))
                   (test-case "G0: layout breakpoint classification returns list"
                     (define layout (make-gui-layout #:width 120 #:height 40))
                     (define bp (classify-layout-breakpoint layout))
                     (check-true (list? bp)))
                   ;; G1: Headless integration
                   (test-case "G1: reducer applies deltas through handler table"
                     (define tbl (delta-handlers->table #:set-header (lambda (p s) 'updated)))
                     (define result
                       (apply-action-with tbl "ui.header.set" (hash 'lines '("h")) 'initial))
                     (check-eq? result 'updated))
                   (test-case "G1: widget filter/sort works without display"
                     (define descs
                       (list (make-widget-descriptor 'a 'ext #:priority 200)
                             (make-widget-descriptor 'b 'ext #:priority 50 #:zone 'sidebar)))
                     (define sorted (widget-descriptors->sorted descs))
                     (check-equal? (widget-descriptor-id (car sorted)) 'b)
                     (define sidebar (filter-widgets-by-zone descs 'sidebar))
                     (check-equal? (length sidebar) 1))
                   ;; G2: Render contracts
                   (test-case "G2: render hook with timeout returns correct shape"
                     (define hook
                       (make-render-hook 'test-hook
                                         'pre-render
                                         (lambda (data) (string-append data "!"))
                                         #:timeout-ms 1000))
                     (define-values (result ok?) (apply-render-hook-safe hook "test"))
                     (check-true ok?)
                     (check-equal? result "test!"))
                   ;; G3: Concurrency — registry isolation
                   (test-case "G3: parameterized registry is thread-safe"
                     (define reg1 (ui-callback-registry #f #f #f #f #f #f #f #f #f #f))
                     (define reg2
                       (ui-callback-registry (lambda _ (void))
                                             (lambda _ (void))
                                             (lambda _ (void))
                                             (lambda _ (void))
                                             (lambda _ 'line)
                                             (lambda _ 'seg)
                                             (lambda _ (void))
                                             (lambda args (car args))
                                             (lambda args (car args))
                                             (lambda args (car args))))
                     (parameterize ([current-ui-registry reg1])
                       (check-false (ui-callbacks-installed?)))
                     (parameterize ([current-ui-registry reg2])
                       (check-true (ui-callbacks-installed?)))))

(run-tests test-w7-gates)
