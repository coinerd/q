#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-ui-delta.rkt — Tests for ui-delta module
;;
;; W2 (v0.94.8): Cover overlay delta types (C-1) and action→delta mapping.

(require rackunit
         rackunit/text-ui
         (only-in "../ui-core/ui-delta.rkt"
                  ui-delta ui-delta? ui-delta-type ui-delta-payload
                  ui-action->deltas ui-delta-type?
                  DELTA-SHOW-OVERLAY DELTA-DISMISS-OVERLAY
                  all-delta-types))

(define-test-suite
 test-ui-delta

 (test-case "overlay delta types exist"
   (check-equal? DELTA-SHOW-OVERLAY 'show-overlay)
   (check-equal? DELTA-DISMISS-OVERLAY 'dismiss-overlay))

 (test-case "overlay delta types are in all-delta-types"
   (check-not-false (member DELTA-SHOW-OVERLAY all-delta-types))
   (check-not-false (member DELTA-DISMISS-OVERLAY all-delta-types)))

 (test-case "ui-delta-type? recognizes overlay types"
   (check-true (ui-delta-type? 'show-overlay))
   (check-true (ui-delta-type? 'dismiss-overlay)))

 (test-case "ui.overlay.show produces show-overlay delta"
   (define deltas (ui-action->deltas "ui.overlay.show" (hash 'content "hello")))
   (check-equal? (length deltas) 1)
   (check-equal? (ui-delta-type (car deltas)) 'show-overlay)
   (check-equal? (ui-delta-payload (car deltas)) "hello"))

 (test-case "ui.overlay.dismiss produces dismiss-overlay delta"
   (define deltas (ui-action->deltas "ui.overlay.dismiss" (hash)))
   (check-equal? (length deltas) 1)
   (check-equal? (ui-delta-type (car deltas)) 'dismiss-overlay)
   (check-false (ui-delta-payload (car deltas))))

 (test-case "overlay show with missing content defaults to empty hash"
   (define deltas (ui-action->deltas "ui.overlay.show" (hash)))
   (check-equal? (ui-delta-payload (car deltas)) (hash))))

(run-tests test-ui-delta)
