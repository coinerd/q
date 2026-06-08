#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-ui-reducer.rkt — Tests for ui-core/ui-reducer.rkt
;;
;; W2.1 (v0.94.2): Verify shared reducer protocol, handler tables,
;; and composition helpers.

(require rackunit
         rackunit/text-ui
         "../ui-core/ui-reducer.rkt"
         "../ui-core/ui-delta.rkt")

(define-test-suite
 test-ui-reducer

 ;; ── Handler table construction ───

 (test-case "make-delta-handler-table creates empty table"
   (define t (make-delta-handler-table))
   (check-true (delta-handler-table? t)))

 (test-case "make-delta-handler-table with key-val pairs"
   (define handler (lambda (payload state) (cons payload state)))
   (define t (make-delta-handler-table 'set-header handler))
   (check-true (delta-handler-table? t)))

 (test-case "delta-handlers->table builds table from keyword args"
   (define t (delta-handlers->table
              #:set-header (lambda (p s) (list 'header p s))
              #:set-status (lambda (p s) (list 'status p s))))
   (check-true (delta-handler-table? t)))

 (test-case "delta-handlers->table with no args produces empty handler map"
   (define t (delta-handlers->table))
   ;; All handlers are #f, so filter removes them all
   (define result (apply-delta-with t (ui-delta 'set-header '("h")) 'initial))
   (check-equal? result 'initial))

 ;; ── apply-delta-with ───

 (test-case "apply-delta-with dispatches to registered handler"
   (define t (delta-handlers->table
              #:set-header (lambda (payload state)
                             (hash-set state 'header payload))))
   (define result (apply-delta-with t (ui-delta 'set-header '("hi")) (hash)))
   (check-equal? (hash-ref result 'header) '("hi")))

 (test-case "apply-delta-with returns state unchanged for unregistered type"
   (define t (delta-handlers->table #:set-header (lambda (p s) s)))
   (define result (apply-delta-with t (ui-delta 'set-footer '("f")) (hash 'x 1)))
   (check-equal? result (hash 'x 1)))

 (test-case "apply-delta-with passes payload to handler"
   (define received-payload (box #f))
   (define t (delta-handlers->table
              #:clear-header (lambda (p s) (set-box! received-payload p) s)))
   (apply-delta-with t (ui-delta 'clear-header #f) 'state)
   (check-false (unbox received-payload)))

 ;; ── apply-deltas-with ───

 (test-case "apply-deltas-with applies multiple deltas in order"
   (define t (delta-handlers->table
              #:set-header (lambda (p s) (hash-set s 'header p))
              #:set-footer (lambda (p s) (hash-set s 'footer p))))
   (define deltas
     (list (ui-delta 'set-header '("h"))
           (ui-delta 'set-footer '("f"))))
   (define result (apply-deltas-with t deltas (hash)))
   (check-equal? (hash-ref result 'header) '("h"))
   (check-equal? (hash-ref result 'footer) '("f")))

 (test-case "apply-deltas-with empty list returns state unchanged"
   (define t (make-delta-handler-table))
   (define result (apply-deltas-with t '() 'state))
   (check-equal? result 'state))

 (test-case "apply-deltas-with folds left (order matters)"
   (define log '())
   (define t (delta-handlers->table
              #:set-status (lambda (p s)
                             (set! log (append log (list p)))
                             s)
              #:set-header (lambda (p s)
                             (set! log (append log (list p)))
                             s)))
   (apply-deltas-with t
                      (list (ui-delta 'set-status 'processing)
                            (ui-delta 'set-header '("h")))
                      'state)
   (check-equal? log '(processing ("h"))))

 ;; ── apply-action-with ───

 (test-case "apply-action-with: end-to-end from action string to state"
   (define t (delta-handlers->table
              #:set-header (lambda (p s) (hash-set s 'header p))
              #:clear-header (lambda (p s) (hash-set s 'header #f))))
   (define result (apply-action-with t "ui.header.set" (hash 'lines '("hi")) (hash)))
   (check-equal? (hash-ref result 'header) '("hi")))

 (test-case "apply-action-with: unknown action returns state unchanged"
   (define t (delta-handlers->table))
   (define result (apply-action-with t "ui.unknown" (hash) 'state))
   (check-equal? result 'state))

 ;; ── Widget handler ───

 (test-case "apply-delta-with: register-widget handler receives descriptor list"
   (define received (box #f))
   (define t (delta-handlers->table
              #:register-widget (lambda (p s)
                                  (set-box! received p)
                                  s)))
   (apply-delta-with t
                     (ui-delta 'register-widget (list 'ext1 'key1 (hash 'content "x")))
                     'state)
   (define p (unbox received))
   (check-equal? (car p) 'ext1)
   (check-equal? (cadr p) 'key1))

 (test-case "apply-delta-with: unregister-widget handler receives (ext . key) pair"
   (define received (box #f))
   (define t (delta-handlers->table
              #:unregister-widget (lambda (p s)
                                    (set-box! received p)
                                    s)))
   (apply-delta-with t
                     (ui-delta 'unregister-widget (cons 'ext1 #f))
                     'state)
   (check-equal? (unbox received) (cons 'ext1 #f)))

 ;; ── Theme and layout ───

 (test-case "apply-delta-with: set-theme handler receives theme value"
   (define t (delta-handlers->table
              #:set-theme (lambda (p s) (hash-set s 'theme p))))
   (define result (apply-delta-with t (ui-delta 'set-theme 'dark) (hash)))
   (check-equal? (hash-ref result 'theme) 'dark))

 (test-case "apply-delta-with: set-layout handler receives breakpoint"
   (define t (delta-handlers->table
              #:set-layout (lambda (p s) (hash-set s 'breakpoint p))))
   (define result (apply-delta-with t (ui-delta 'set-layout 'wide) (hash)))
   (check-equal? (hash-ref result 'breakpoint) 'wide))

 ;; ── Purity: same input → same output ───

 (test-case "apply-deltas-with is pure"
   (define t (delta-handlers->table
              #:set-header (lambda (p s) (cons p s))))
   (define input (list (ui-delta 'set-header '("x"))))
   (check-equal? (apply-deltas-with t input '())
                 (apply-deltas-with t input '())))
 )

(run-tests test-ui-reducer)
