#lang racket

;; q/tests/test-w4-widget-schema.rkt — W4 widget + schema tests
;;
;; W4 (v0.94.8): M-2 (schema validation), M-5 (zone/kind enums), m-8 (lifecycle consumer).

(require rackunit
         rackunit/text-ui
         (only-in "../ui-core/widget-descriptor.rkt"
                  make-widget-descriptor
                  widget-descriptor?
                  widget-descriptor-valid?
                  widget-descriptor-zone
                  widget-descriptor-kind
                  widget-descriptor-lifecycle-token
                  widget-descriptor-matches-lifecycle?
                  valid-widget-zones
                  valid-widget-kinds)
         (only-in "../ui-core/ui-actions.rkt"
                  ui-action-schema
                  valid-ui-action-name?
                  UI-ACTION-HEADER-SET
                  UI-ACTION-STATUS-SET))

(define-test-suite
 test-w4-widget-schema
 ;; ── M-5: zone/kind validation ───
 (test-case "valid zone accepted"
   (for ([z (in-list valid-widget-zones)])
     (define w (make-widget-descriptor 'test 'ext #:zone z))
     (check-true (widget-descriptor-valid? w) (format "zone ~a should be valid" z))))
 (test-case "invalid zone rejected"
   (define w (make-widget-descriptor 'test 'ext #:zone 'invalid-zone))
   (check-false (widget-descriptor-valid? w)))
 (test-case "valid kind accepted"
   (for ([k (in-list valid-widget-kinds)])
     (define w (make-widget-descriptor 'test 'ext #:kind k))
     (check-true (widget-descriptor-valid? w) (format "kind ~a should be valid" k))))
 (test-case "invalid kind rejected"
   (define w (make-widget-descriptor 'test 'ext #:kind 'invalid-kind))
   (check-false (widget-descriptor-valid? w)))
 (test-case "#f zone is accepted"
   (define w (make-widget-descriptor 'test 'ext #:zone #f))
   (check-true (widget-descriptor-valid? w)))
 ;; ── m-8: lifecycle token consumer ───
 (test-case "widget-descriptor-matches-lifecycle? returns #t for same token"
   (define w (make-widget-descriptor 'test 'ext))
   (define token (widget-descriptor-lifecycle-token w))
   (check-true (widget-descriptor-matches-lifecycle? w token)))
 (test-case "widget-descriptor-matches-lifecycle? returns #f for different token"
   (define w (make-widget-descriptor 'test 'ext))
   (check-false (widget-descriptor-matches-lifecycle? w 'other-token)))
 ;; ── M-2: schema is non-empty and has keys ───
 (test-case "ui-action-schema has entries for all action types"
   (check-true (hash? ui-action-schema))
   (check-true (>= (hash-count ui-action-schema) 5)))
 (test-case "schema keys are valid action names"
   (for ([(k v) (in-hash ui-action-schema)])
     (check-true (valid-ui-action-name? k) (format "~a should be valid" k)))))

(define-test-suite test-w4-schema-required-keys
                   (test-case "header-set requires lines"
                     (check-equal? (hash-ref ui-action-schema UI-ACTION-HEADER-SET) '(lines)))
                   (test-case "status-set requires status"
                     (check-equal? (hash-ref ui-action-schema UI-ACTION-STATUS-SET) '(status))))

(run-tests test-w4-widget-schema)
(run-tests test-w4-schema-required-keys)
