#lang racket

;; tests/test-tui-editor-ime.rkt — Tests for custom editor component (#1150) and IME cursor markers (#1151)

(require rackunit
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/component.rkt")

;; ============================================================
;; Custom editor component tests (#1150)
;; ============================================================

(test-case "initial ui-state has no editor component"
  (define s (initial-ui-state))
  (check-false (ui-state-editor-component s)))

(test-case "set-editor-component stores component"
  (define comp (make-q-component (lambda (s w) '()) #:id 'vim-editor))
  (define s1 (set-editor-component (initial-ui-state) comp))
  (check-equal? (q-component-id (ui-state-editor-component s1)) 'vim-editor))

(test-case "clear-editor-component resets to #f"
  (define comp (make-q-component (lambda (s w) '()) #:id 'vim-editor))
  (define s1 (set-editor-component (initial-ui-state) comp))
  (define s2 (clear-editor-component s1))
  (check-false (ui-state-editor-component s2)))

;; ============================================================
;; IME cursor marker tests (#1151)
;; ============================================================

(test-case "CURSOR_MARKER is zero-width space"
  (check-equal? CURSOR_MARKER #\u200B))

(test-case "cursor-marker-string returns string"
  (check-equal? (cursor-marker-string) "\u200B"))

(test-case "strip-cursor-markers removes markers"
  (check-equal? (strip-cursor-markers "hello\u200Bworld") "helloworld")
  (check-equal? (strip-cursor-markers "\u200Bstart") "start")
  (check-equal? (strip-cursor-markers "end\u200B") "end")
  (check-equal? (strip-cursor-markers "no-markers") "no-markers"))

(test-case "has-cursor-markers? detects markers"
  (check-true (has-cursor-markers? "hello\u200Bworld"))
  (check-false (has-cursor-markers? "hello world")))

(test-case "insert-cursor-marker at position"
  (check-equal? (insert-cursor-marker "hello" 3) "hel\u200Blo")
  (check-equal? (insert-cursor-marker "hello" 0) "\u200Bhello")
  (check-equal? (insert-cursor-marker "hello" 5) "hello\u200B"))

(test-case "insert-cursor-marker clamps position"
  (check-equal? (insert-cursor-marker "hi" 10) "hi\u200B"))

(test-case "cursor marker round-trip"
  (define original "hello world")
  (define with-marker (insert-cursor-marker original 5))
  (check-true (has-cursor-markers? with-marker))
  (define cleaned (strip-cursor-markers with-marker))
  (check-equal? cleaned original))
