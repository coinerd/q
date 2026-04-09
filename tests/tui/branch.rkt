#lang racket

;;; tests/tui/branch.rkt — tests for TUI branch inspection commands
;;;
;;; Covers:
;;;   - Slash command parsing for /branches, /leaves, /switch, /children
;;;   - branch-info struct and helpers
;;;   - render-branch-list, render-leaf-nodes, render-children-list

(require rackunit
         rackunit/text-ui
         "../../tui/input.rkt"
         "../../tui/state.rkt"
         "../../tui/render.rkt")

(define branch-tests
  (test-suite
   "TUI Branch Inspection"

   ;; ── Slash Command Parsing ──

   (test-case "/branches parses correctly"
     (check-equal? (parse-tui-slash-command "/branches") 'branches))

   (test-case "/leaves parses correctly"
     (check-equal? (parse-tui-slash-command "/leaves") 'leaves))

   (test-case "/switch with argument parses correctly"
     (check-equal? (parse-tui-slash-command "/switch abc123")
                   '(switch "abc123")))

   (test-case "/switch without argument returns error"
     (check-equal? (parse-tui-slash-command "/switch")
                   '(switch-error "Usage: /switch <branch-id>")))

   (test-case "/children with argument parses correctly"
     (check-equal? (parse-tui-slash-command "/children msg-1")
                   '(children "msg-1")))

   (test-case "/children without argument returns error"
     (check-equal? (parse-tui-slash-command "/children")
                   '(children-error "Usage: /children <node-id>")))

   ;; ── branch-info struct ──

   (test-case "branch-info construction and accessors"
     (define bi (branch-info "msg-1" "parent-1" 'user #t #f))
     (check-equal? (branch-info-id bi) "msg-1")
     (check-equal? (branch-info-parent-id bi) "parent-1")
     (check-equal? (branch-info-role bi) 'user)
     (check-equal? (branch-info-leaf? bi) #t)
     (check-equal? (branch-info-active? bi) #f))

   (test-case "branch-info is transparent"
     (define bi (branch-info "id" "pid" 'assistant #f #t))
     (check-true (branch-info? bi)))

   ;; ── Rendering functions ──

   (test-case "render-branch-list produces styled lines"
     (define branches
       (list (branch-info "b1" "root" 'assistant #t #t)
             (branch-info "b2" "root" 'user #f #f)))
     (define lines (render-branch-list branches))
     (check-true (list? lines))
     ;; Header + 2 branch entries = 3 lines
     (check = (length lines) 3)
     (check-true (styled-line? (car lines))))

   (test-case "render-leaf-nodes produces styled lines"
     (define leaves
       (list (branch-info "l1" "p1" 'assistant #t #f)
             (branch-info "l2" "p2" 'user #t #f)))
     (define lines (render-leaf-nodes leaves))
     (check-true (list? lines))
     ;; Header + 2 leaf entries = 3 lines
     (check = (length lines) 3))

   (test-case "render-children-list produces styled lines"
     (define children
       (list (branch-info "c1" "parent" 'assistant #f #f)
             (branch-info "c2" "parent" 'user #t #f)))
     (define lines (render-children-list "parent" children 80))
     (check-true (list? lines))
     ;; Header + 2 child entries = 3 lines
     (check = (length lines) 3))

   (test-case "render-branch-list empty returns header only"
     (define lines (render-branch-list '()))
     (check = (length lines) 1)
     (check-true (styled-line? (car lines))))

   (test-case "render-leaf-nodes empty returns header only"
     (define lines (render-leaf-nodes '()))
     (check = (length lines) 1)
     (check-true (styled-line? (car lines))))

   (test-case "render-children-list empty returns info message"
     (define lines (render-children-list "xyz" '() 80))
     (check-true (list? lines))
     ;; Header + info message = 2 lines
     (check = (length lines) 2))

   ))

(run-tests branch-tests)
