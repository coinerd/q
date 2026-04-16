#lang racket

;; tests/test-custom-ui-api.rkt — tests for Custom Footer, Header, Tool Rendering (#717-#720)
;;
;; Covers:
;;   - #717: Replaceable footer and status bar via extension API
;;   - #718: render-call and render-result on tool definitions
;;   - #719: Custom tool rendering in transcript renderer
;;   - #720: Parent feature

(require rackunit
         "../tui/state.rkt"
         "../tui/render.rkt"
         "../tools/tool.rkt"
         "../extensions/custom-ui-api.rkt"
         "../extensions/custom-renderer-registry.rkt")

;; ============================================================
;; #717: Custom header/footer
;; ============================================================

(test-case "set-custom-header: stores header lines"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "Custom Header" '(bold))))))
  (define s1 (set-custom-header state lines))
  (check-equal? (ui-state-custom-header s1) lines))

(test-case "set-custom-footer: stores footer lines"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "Custom Status" '())))))
  (define s1 (set-custom-footer state lines))
  (check-equal? (ui-state-custom-footer s1) lines))

(test-case "clear-custom-header: resets to #f"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "H" '())))))
  (define s1 (set-custom-header state lines))
  (check-not-false (ui-state-custom-header s1))
  (define s2 (clear-custom-header s1))
  (check-false (ui-state-custom-footer s2)))

(test-case "clear-custom-footer: resets to #f"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "F" '())))))
  (define s1 (set-custom-footer state lines))
  (check-not-false (ui-state-custom-footer s1))
  (define s2 (clear-custom-footer s1))
  (check-false (ui-state-custom-footer s2)))

(test-case "initial state: header/footer are #f"
  (define state (initial-ui-state))
  (check-false (ui-state-custom-header state))
  (check-false (ui-state-custom-footer state)))

(test-case "ctx-set-header: updates ui-state box"
  (define state-box (box (initial-ui-state)))
  (define lines (list (styled-line (list (styled-segment "Box Header" '())))))
  (ctx-set-header state-box lines)
  (check-equal? (ui-state-custom-header (unbox state-box)) lines))

(test-case "ctx-set-footer: updates ui-state box"
  (define state-box (box (initial-ui-state)))
  (define lines (list (styled-line (list (styled-segment "Box Footer" '())))))
  (ctx-set-footer state-box lines)
  (check-equal? (ui-state-custom-footer (unbox state-box)) lines))

(test-case "ctx-clear-header: clears via box"
  (define state-box (box (initial-ui-state)))
  (ctx-set-header state-box (list (styled-line (list (styled-segment "H" '())))))
  (ctx-clear-header state-box)
  (check-false (ui-state-custom-header (unbox state-box))))

(test-case "ctx-clear-footer: clears via box"
  (define state-box (box (initial-ui-state)))
  (ctx-set-footer state-box (list (styled-line (list (styled-segment "F" '())))))
  (ctx-clear-footer state-box)
  (check-false (ui-state-custom-footer (unbox state-box))))

;; ============================================================
;; #718: Tool with render-call/render-result
;; ============================================================

(test-case "make-tool: accepts render-call and render-result"
  (define render-call
    (lambda (args) (list (styled-line (list (styled-segment (format "Calling: ~a" args) '(bold)))))))
  (define render-result
    (lambda (res) (list (styled-line (list (styled-segment (format "Done: ~a" res) '()))))))
  (define t
    (make-tool "my-tool"
               "A test tool"
               (hasheq)
               (lambda (args) "ok")
               #:render-call render-call
               #:render-result render-result))
  (check-equal? (tool-name t) "my-tool")
  (check-true (procedure? (tool-render-call t)))
  (check-true (procedure? (tool-render-result t))))

(test-case "make-tool: defaults render-call/render-result to #f"
  (define t (make-tool "basic" "Basic tool" (hasheq) (lambda (args) "ok")))
  (check-false (tool-render-call t))
  (check-false (tool-render-result t)))

;; ============================================================
;; #719: Custom tool rendering
;; ============================================================

(test-case "render-tool-call: uses custom renderer"
  (define custom
    (list (custom-renderer "my-tool"
                           (lambda (args)
                             (list (styled-line (list (styled-segment "CUSTOM CALL" '(bold))))))
                           #f)))
  (define result (render-tool-call "my-tool" "arg1=val" custom))
  (check-equal? (length result) 1)
  (check-equal? (styled-line->text (car result)) "CUSTOM CALL"))

(test-case "render-tool-call: falls back to default"
  (define result (render-tool-call "unknown-tool" "args" '()))
  (check-equal? (length result) 1)
  (check-true (string-contains? (styled-line->text (car result)) "unknown-tool")))

(test-case "render-tool-result: uses custom renderer"
  (define custom
    (list (custom-renderer "my-tool"
                           #f
                           (lambda (res)
                             (list (styled-line (list (styled-segment "CUSTOM RESULT" '()))))))))
  (define result (render-tool-result "my-tool" "output" custom))
  (check-equal? (length result) 1)
  (check-equal? (styled-line->text (car result)) "CUSTOM RESULT"))

(test-case "render-tool-result: falls back to default"
  (define result (render-tool-result "unknown-tool" "output" '()))
  (check-equal? (length result) 1)
  (check-true (string-contains? (styled-line->text (car result)) "OK")))

;; ============================================================
;; #720: Integration
;; ============================================================

(test-case "custom-renderer struct: stores all fields"
  (define r (custom-renderer "bash" (lambda (a) '()) (lambda (r) '())))
  (check-equal? (custom-renderer-tool-name r) "bash")
  (check-true (procedure? (custom-renderer-render-call r)))
  (check-true (procedure? (custom-renderer-render-result r))))
