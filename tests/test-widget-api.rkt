#lang racket

;; tests/test-widget-api.rkt — tests for Extension Widget Slots (#713-#716)
;;
;; Covers:
;;   - #713: Widget containers above/below input (layout + state)
;;   - #714: ctx-set-widget for extension content
;;   - #715: Widget disposal on extension unload
;;   - #716: Parent feature

(require rackunit
         "../tui/state.rkt"
         "../tui/layout.rkt"
         "../tui/render.rkt"
         "../extensions/widget-api.rkt")

;; ============================================================
;; #713: Layout with widget containers
;; ============================================================

(test-case "compute-layout: basic layout unchanged"
  (define l (compute-layout 80 24))
  (check-equal? (tui-layout-cols l) 80)
  (check-equal? (tui-layout-rows l) 24)
  (check-equal? (tui-layout-header-row l) 0)
  (check-equal? (tui-layout-transcript-start-row l) 1)
  (check-equal? (tui-layout-status-row l) 22)
  (check-equal? (tui-layout-input-row l) 23))

(test-case "compute-layout-with-widgets: adjusts for widget rows"
  (define l (compute-layout-with-widgets 80 24 3))
  (check-equal? (tui-layout-cols l) 80)
  (check-equal? (tui-layout-rows l) 24)
  ;; non-transcript = 3 (header+status+input) + 3 (widgets) = 6
  ;; transcript-height = 24 - 6 = 18
  (check-equal? (tui-layout-transcript-height l) 18)
  ;; status-row = 1 + 18 + 3 = 22
  (check-equal? (tui-layout-status-row l) 22)
  ;; input-row = 2 + 18 + 3 = 23
  (check-equal? (tui-layout-input-row l) 23))

(test-case "compute-layout-with-widgets: zero widgets = same as basic"
  (define l0 (compute-layout 80 24))
  (define l0w (compute-layout-with-widgets 80 24 0))
  (check-equal? (tui-layout-transcript-height l0)
                (tui-layout-transcript-height l0w))
  (check-equal? (tui-layout-status-row l0)
                (tui-layout-status-row l0w)))

(test-case "compute-layout-with-widgets: clamps transcript to min 1"
  (define l (compute-layout-with-widgets 80 4 3))
  (check-equal? (tui-layout-transcript-height l) 1))

;; ============================================================
;; #714: Widget state management
;; ============================================================

(test-case "set-extension-widget: adds widget to state"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "Hello from ext" '())))))
  (define s1 (set-extension-widget state "my-ext" "status" lines))
  (define widgets (ui-state-extension-widgets s1))
  (check-equal? (hash-ref widgets (cons "my-ext" "status")) lines))

(test-case "set-extension-widget: overwrites existing widget"
  (define state (initial-ui-state))
  (define lines1 (list (styled-line (list (styled-segment "v1" '())))))
  (define lines2 (list (styled-line (list (styled-segment "v2" '())))))
  (define s1 (set-extension-widget state "ext" "key" lines1))
  (define s2 (set-extension-widget s1 "ext" "key" lines2))
  (check-equal? (hash-ref (ui-state-extension-widgets s2) (cons "ext" "key")) lines2))

(test-case "remove-extension-widget: removes specific widget"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "test" '())))))
  (define s1 (set-extension-widget state "ext" "key" lines))
  (define s2 (remove-extension-widget s1 "ext" "key"))
  (check-equal? (hash-count (ui-state-extension-widgets s2)) 0))

(test-case "remove-extension-widget: non-existent key is no-op"
  (define state (initial-ui-state))
  (define s1 (remove-extension-widget state "ext" "nonexistent"))
  (check-equal? (hash-count (ui-state-extension-widgets s1)) 0))

(test-case "get-widget-lines-above: collects all widget lines"
  (define state (initial-ui-state))
  (define lines1 (list (styled-line (list (styled-segment "line1" '())))))
  (define lines2 (list (styled-line (list (styled-segment "line2" '())))))
  (define s1 (set-extension-widget state "ext1" "a" lines1))
  (define s2 (set-extension-widget s1 "ext2" "b" lines2))
  (define above (get-widget-lines-above s2))
  (check-equal? (length above) 2))

(test-case "get-widget-lines-above: empty when no widgets"
  (define state (initial-ui-state))
  (check-equal? (get-widget-lines-above state) '()))

(test-case "get-widget-lines-below: returns empty for now"
  (define state (initial-ui-state))
  (check-equal? (get-widget-lines-below state) '()))

;; ============================================================
;; #714: ctx-set-widget API
;; ============================================================

(test-case "ctx-set-widget: updates ui-state box"
  (define state-box (box (initial-ui-state)))
  (define lines (list (styled-line (list (styled-segment "widget content" '())))))
  (ctx-set-widget state-box "my-ext" "info" lines)
  (define widgets (ui-state-extension-widgets (unbox state-box)))
  (check-equal? (hash-ref widgets (cons "my-ext" "info")) lines))

(test-case "ctx-remove-widget: removes from ui-state box"
  (define state-box (box (initial-ui-state)))
  (define lines (list (styled-line (list (styled-segment "temp" '())))))
  (ctx-set-widget state-box "ext" "key" lines)
  (ctx-remove-widget state-box "ext" "key")
  (check-equal? (hash-count (ui-state-extension-widgets (unbox state-box))) 0))

;; ============================================================
;; #715: Widget disposal
;; ============================================================

(test-case "remove-all-extension-widgets: removes all widgets for one extension"
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "x" '())))))
  (define s1 (set-extension-widget state "ext1" "a" lines))
  (define s2 (set-extension-widget s1 "ext1" "b" lines))
  (define s3 (set-extension-widget s2 "ext2" "c" lines))
  ;; ext2 has 1 widget, ext1 has 2
  (check-equal? (hash-count (ui-state-extension-widgets s3)) 3)
  (define s4 (remove-all-extension-widgets s3 "ext1"))
  ;; Only ext2's widget remains
  (check-equal? (hash-count (ui-state-extension-widgets s4)) 1))

(test-case "dispose-widgets: removes all for extension from box"
  (define state-box (box (initial-ui-state)))
  (define lines (list (styled-line (list (styled-segment "x" '())))))
  (ctx-set-widget state-box "ext1" "a" lines)
  (ctx-set-widget state-box "ext1" "b" lines)
  (ctx-set-widget state-box "ext2" "c" lines)
  (dispose-widgets state-box "ext1")
  (define widgets (ui-state-extension-widgets (unbox state-box)))
  (check-equal? (hash-count widgets) 1)
  (check-true (hash-has-key? widgets (cons "ext2" "c"))))

(test-case "dispose-widgets: no-op for unknown extension"
  (define state-box (box (initial-ui-state)))
  (define lines (list (styled-line (list (styled-segment "x" '())))))
  (ctx-set-widget state-box "ext1" "a" lines)
  (dispose-widgets state-box "nonexistent")
  (check-equal? (hash-count (ui-state-extension-widgets (unbox state-box))) 1))

;; ============================================================
;; #716: Integration — widget affects layout
;; ============================================================

(test-case "integration: widgets shrink transcript and shift input"
  (define state-box (box (initial-ui-state)))
  ;; No widgets: standard layout
  (define l0 (compute-layout 80 24))
  (define th0 (tui-layout-transcript-height l0))
  ;; Add 2 widget lines
  (define lines (list (styled-line (list (styled-segment "info line 1" '())))
                      (styled-line (list (styled-segment "info line 2" '())))))
  (ctx-set-widget state-box "ext" "info" lines)
  (define widget-count (length (get-widget-lines-above (unbox state-box))))
  (check-equal? widget-count 2)
  ;; Layout with widgets
  (define l1 (compute-layout-with-widgets 80 24 widget-count))
  (check-equal? (tui-layout-transcript-height l1) (- th0 widget-count)))
