#lang racket

;; BOUNDARY: integration

;; tests/test-custom-renderer-registry.rkt — FEAT-70: custom renderer registry + wiring

(require rackunit
         "../extensions/custom-renderer-registry.rkt"
         "../tui/render.rkt"
         "../tui/state.rkt"
         "../tui/state-ui.rkt")

;; ============================================================
;; Registry API
;; ============================================================

(test-case "register-custom-renderer! adds to registry"
  (unregister-custom-renderer! "test-tool-xyz")
  (define call-count 0)
  (define r
    (custom-renderer "test-tool-xyz"
                     (lambda (args)
                       (set! call-count (add1 call-count))
                       (list (styled-line (list (styled-segment (format "CUSTOM: ~a" args) '())))))
                     #f))
  (register-custom-renderer! r)
  (define found (lookup-custom-renderer "test-tool-xyz"))
  (check-not-false found)
  (check-equal? (custom-renderer-tool-name found) "test-tool-xyz")
  ;; Cleanup
  (unregister-custom-renderer! "test-tool-xyz"))

(test-case "unregister-custom-renderer! removes from registry"
  (define r (custom-renderer "test-tool-remove" #f #f))
  (register-custom-renderer! r)
  (check-not-false (lookup-custom-renderer "test-tool-remove"))
  (unregister-custom-renderer! "test-tool-remove")
  (check-false (lookup-custom-renderer "test-tool-remove")))

(test-case "lookup-custom-renderer-for-tool returns correct render fn"
  (unregister-custom-renderer! "test-tool-fn")
  (define r (custom-renderer "test-tool-fn" (lambda (args) '()) (lambda (result) '())))
  (register-custom-renderer! r)
  (check-not-false (lookup-custom-renderer-for-tool "test-tool-fn" 'call))
  (check-not-false (lookup-custom-renderer-for-tool "test-tool-fn" 'result))
  (check-false (lookup-custom-renderer-for-tool "test-tool-fn" 'invalid))
  ;; Cleanup
  (unregister-custom-renderer! "test-tool-fn"))

(test-case "lookup returns #f for unknown tool"
  (check-false (lookup-custom-renderer "nonexistent-tool-xyz")))

(test-case "register replaces existing renderer for same tool"
  (unregister-custom-renderer! "test-tool-replace")
  (define r1 (custom-renderer "test-tool-replace" (lambda (_) '()) #f))
  (define r2 (custom-renderer "test-tool-replace" #f (lambda (_) '())))
  (register-custom-renderer! r1)
  (register-custom-renderer! r2)
  (define found (lookup-custom-renderer "test-tool-replace"))
  (check-not-false found)
  ;; r2 replaces r1 — render-call should be #f
  (check-false (custom-renderer-render-call found))
  (check-not-false (custom-renderer-render-result found))
  ;; Cleanup
  (unregister-custom-renderer! "test-tool-replace"))

;; ============================================================
;; Integration: format-entry uses custom renderer
;; ============================================================

(test-case "format-entry uses custom renderer for tool-start"
  (unregister-custom-renderer! "my-tool")
  (define r
    (custom-renderer "my-tool"
                     (lambda (args)
                       (list (styled-line (list (styled-segment (format "CUSTOM-CALL: ~a" args)
                                                                '(bold))))))
                     #f))
  (register-custom-renderer! r)
  (define entry
    (transcript-entry 'tool-start "my-tool running" 0 (hasheq 'name "my-tool" 'args "file.rkt") #f))
  (define lines (format-entry entry 80))
  (check > (length lines) 0)
  (check-not-false (string-contains? (styled-line->text (car lines)) "CUSTOM-CALL"))
  ;; Cleanup
  (unregister-custom-renderer! "my-tool"))

(test-case "format-entry uses custom renderer for tool-end"
  (unregister-custom-renderer! "result-tool")
  (define r
    (custom-renderer "result-tool"
                     #f
                     (lambda (result)
                       (list (styled-line (list (styled-segment (format "CUSTOM-OK: ~a" result)
                                                                '(green))))))))
  (register-custom-renderer! r)
  (define entry (transcript-entry 'tool-end "done" 0 (hasheq 'name "result-tool") #f))
  (define lines (format-entry entry 80))
  (check > (length lines) 0)
  (check-not-false (string-contains? (styled-line->text (car lines)) "CUSTOM-OK"))
  ;; Cleanup
  (unregister-custom-renderer! "result-tool"))

(test-case "format-entry falls back to default when no custom renderer"
  (unregister-custom-renderer! "default-tool")
  (define entry
    (transcript-entry 'tool-start "default-tool running" 0 (hasheq 'name "default-tool") #f))
  (define lines (format-entry entry 80))
  (check > (length lines) 0)
  ;; Should use default rendering (contains the text)
  (check-not-false (string-contains? (styled-line->text (car lines)) "default-tool running")))

;; ============================================================
;; Thread safety — #5250
;; ============================================================

(test-case "concurrent register/unregister does not corrupt registry"
  (define iterations 100)
  (define thds
    (for/list ([i (in-range 4)])
      (thread (lambda ()
                (for ([j (in-range iterations)])
                  (define tool-name (format "ct-~a-~a" i j))
                  (define r (custom-renderer tool-name #f #f))
                  (register-custom-renderer! r)
                  (lookup-custom-renderer tool-name)
                  (unregister-custom-renderer! tool-name))))))
  (for-each thread-wait thds)
  ;; Registry should be consistent — no ct- entries left
  (check-false (lookup-custom-renderer "ct-0-0")))

(test-case "concurrent lookups during registration are consistent"
  (unregister-custom-renderer! "lkup-test")
  (define r0 (custom-renderer "lkup-test" (lambda (_) '()) #f))
  (register-custom-renderer! r0)
  (define results (box '()))
  (define thds
    (for/list ([_ (in-range 10)])
      (thread (lambda ()
                (define found (lookup-custom-renderer "lkup-test"))
                (set-box! results (cons found (unbox results)))))))
  (for-each thread-wait thds)
  ;; All lookups should find the renderer
  (for ([v (in-list (unbox results))])
    (check-not-false v))
  (unregister-custom-renderer! "lkup-test"))

;; ============================================================
;; Below-transcript widget rendering — #5251
;; ============================================================

(test-case "get-widget-lines-below returns extension widget lines"
  (define st0 (initial-ui-state))
  ;; Initially empty
  (check-equal? (get-widget-lines-below st0) '())
  ;; Set a widget
  (define st1 (set-extension-widget st0 'my-ext 'status '("line1" "line2")))
  (define below (get-widget-lines-below st1))
  (check-equal? (length below) 2)
  (check-equal? (car below) "line1"))

(test-case "get-widget-lines-below merges multiple extensions"
  (define st0 (initial-ui-state))
  (define st1 (set-extension-widget st0 'ext-a 'key1 '("a1")))
  (define st2 (set-extension-widget st1 'ext-b 'key2 '("b1" "b2")))
  (define below (get-widget-lines-below st2))
  (check-equal? (length below) 3))

(test-case "widget-bar-height parameter has default 3"
  (check-equal? (widget-bar-height) 3)
  (parameterize ([widget-bar-height 5])
    (check-equal? (widget-bar-height) 5)))
