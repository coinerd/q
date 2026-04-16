#lang racket

;; tests/test-custom-renderer-registry.rkt — FEAT-70: custom renderer registry + wiring

(require rackunit
         "../extensions/custom-renderer-registry.rkt"
         "../tui/render.rkt"
         "../tui/state.rkt")

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
    (transcript-entry 'tool-start
                      "my-tool running"
                      0
                      (hasheq 'tool-name "my-tool" 'args "file.rkt")
                      #f))
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
  (define entry (transcript-entry 'tool-end "done" 0 (hasheq 'tool-name "result-tool") #f))
  (define lines (format-entry entry 80))
  (check > (length lines) 0)
  (check-not-false (string-contains? (styled-line->text (car lines)) "CUSTOM-OK"))
  ;; Cleanup
  (unregister-custom-renderer! "result-tool"))

(test-case "format-entry falls back to default when no custom renderer"
  (unregister-custom-renderer! "default-tool")
  (define entry
    (transcript-entry 'tool-start "default-tool running" 0 (hasheq 'tool-name "default-tool") #f))
  (define lines (format-entry entry 80))
  (check > (length lines) 0)
  ;; Should use default rendering (contains the text)
  (check-not-false (string-contains? (styled-line->text (car lines)) "default-tool running")))
