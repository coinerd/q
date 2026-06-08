#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-layout-policy.rkt — Layout breakpoint and policy tests
;;
;; W5.1 (v0.94.5): Verify layout breakpoint classification and adaptive policy.

(require rackunit
         rackunit/text-ui
         (only-in "../ui-core/layout-protocol.rkt"
                  make-gui-layout gui-layout-width gui-layout-height
                  gui-layout-sidebar-width gui-layout-status-height
                  gui-layout-input-height
                  layout-breakpoints classify-layout-breakpoint
                  layout-breakpoint? apply-layout-policy))

(define-test-suite
 test-layout-policy

 ;; ── Breakpoint classification ───

 (test-case "narrow: width < 60"
   (define layout (make-gui-layout #:width 40 #:height 24))
   (check-eq? (classify-layout-breakpoint layout) 'narrow))

 (test-case "standard: 60 <= width < 100"
   (define layout (make-gui-layout #:width 80 #:height 24))
   (check-eq? (classify-layout-breakpoint layout) 'standard))

 (test-case "wide: width >= 100, height < 60"
   (define layout (make-gui-layout #:width 120 #:height 40))
   (check-eq? (classify-layout-breakpoint layout) 'wide))

 (test-case "tall: width >= 100, height >= 60"
   (define layout (make-gui-layout #:width 120 #:height 60))
   (check-eq? (classify-layout-breakpoint layout) 'tall))

 (test-case "boundary: width exactly 59 is narrow"
   (define layout (make-gui-layout #:width 59 #:height 40))
   (check-eq? (classify-layout-breakpoint layout) 'narrow))

 (test-case "boundary: width exactly 60 is standard"
   (define layout (make-gui-layout #:width 60 #:height 24))
   (check-eq? (classify-layout-breakpoint layout) 'standard))

 (test-case "boundary: width exactly 99 is standard"
   (define layout (make-gui-layout #:width 99 #:height 40))
   (check-eq? (classify-layout-breakpoint layout) 'standard))

 (test-case "boundary: width exactly 100 is wide (height < 60)"
   (define layout (make-gui-layout #:width 100 #:height 40))
   (check-eq? (classify-layout-breakpoint layout) 'wide))

 (test-case "boundary: height exactly 59 is wide (not tall)"
   (define layout (make-gui-layout #:width 120 #:height 59))
   (check-eq? (classify-layout-breakpoint layout) 'wide))

 ;; ── Breakpoint predicate ───

 (test-case "layout-breakpoint? accepts valid symbols"
   (for ([bp layout-breakpoints])
     (check-true (layout-breakpoint? bp) (format "~a should be valid" bp))))

 (test-case "layout-breakpoint? rejects invalid symbols"
   (check-false (layout-breakpoint? 'invalid))
   (check-false (layout-breakpoint? 'narrow-extra)))

 ;; ── Apply layout policy ───

 (test-case "narrow policy: no sidebar, minimal chrome"
   (define layout (make-gui-layout #:width 40 #:height 24))
   (define result (apply-layout-policy layout 'narrow))
   (check-equal? (gui-layout-sidebar-width result) 0)
   (check-equal? (gui-layout-input-height result) 2))

 (test-case "standard policy: no sidebar, standard chrome"
   (define layout (make-gui-layout #:width 80 #:height 24))
   (define result (apply-layout-policy layout 'standard))
   (check-equal? (gui-layout-sidebar-width result) 0)
   (check-equal? (gui-layout-input-height result) 3))

 (test-case "wide policy: sidebar enabled"
   (define layout (make-gui-layout #:width 120 #:height 40))
   (define result (apply-layout-policy layout 'wide))
   (check-equal? (gui-layout-sidebar-width result) 24))

 (test-case "tall policy: sidebar + extended status"
   (define layout (make-gui-layout #:width 120 #:height 60))
   (define result (apply-layout-policy layout 'tall))
   (check-equal? (gui-layout-sidebar-width result) 24)
   (check-equal? (gui-layout-status-height result) 2))

 (test-case "unknown breakpoint returns layout unchanged"
   (define layout (make-gui-layout #:width 100 #:height 40))
   (define result (apply-layout-policy layout 'unknown))
   (check-equal? result layout))

 ;; ── Purity ───

 (test-case "classify-layout-breakpoint is pure"
   (define layout (make-gui-layout #:width 80 #:height 30))
   (check-eq? (classify-layout-breakpoint layout)
              (classify-layout-breakpoint layout)))

 (test-case "apply-layout-policy is pure"
   (define layout (make-gui-layout #:width 120 #:height 40))
   (define r1 (apply-layout-policy layout 'wide))
   (define r2 (apply-layout-policy layout 'wide))
   (check-equal? r1 r2)))

(run-tests test-layout-policy)
