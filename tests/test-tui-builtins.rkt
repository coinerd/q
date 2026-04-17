#lang racket

;; tests/test-tui-builtins.rkt — Tests for built-in components and overlay positioning
;;
;; Covers SelectList, BorderedLoader, SettingsList components,
;; and overlay-config / overlay-compute-bounds.

(require rackunit
         rackunit/text-ui
         "../tui/builtins.rkt"
         "../tui/component.rkt"
         "../tui/state.rkt")

(define-test-suite
 test-tui-builtins
 ;; ──────────────────────────────
 ;; SelectList tests
 ;; ──────────────────────────────
 (test-case "make-select-list creates interactive component"
   (define sl (make-select-list '("Option A" "Option B" "Option C")))
   (check-equal? (q-component-id sl) 'select-list)
   (check-true (q-component-wants-focus? sl)))
 (test-case "select-list renders options"
   (define sl (make-select-list '("Alpha" "Beta" "Gamma")))
   (define lines (component-render sl (initial-ui-state) 80))
   (check-true (>= (length lines) 3)))
 (test-case "select-list handles up/down input"
   (define sl (make-select-list '("A" "B" "C")))
   (define s0 (initial-ui-state))
   (define-values (s1 r1) (component-handle-input sl 'down s0))
   (check-true (input-consumed? r1))
   (define-values (s2 r2) (component-handle-input sl 'up s0))
   (check-true (input-consumed? r2)))
 (test-case "select-list handles enter to select"
   (define selected (box #f))
   (define sl (make-select-list '("A" "B" "C") #:on-select (lambda (v) (set-box! selected v))))
   (define-values (s1 r1) (component-handle-input sl 'enter (initial-ui-state)))
   (check-true (input-action? r1))
   (check-equal? (unbox selected) "A"))
 (test-case "select-list handles escape to cancel"
   (define cancelled (box #f))
   (define sl (make-select-list '("A" "B") #:on-cancel (lambda () (set-box! cancelled #t))))
   (define-values (s1 r1) (component-handle-input sl 'escape (initial-ui-state)))
   (check-true (input-action? r1))
   (check-true (unbox cancelled)))
 (test-case "select-list handles filter text"
   (define sl (make-select-list '("Apple" "Banana" "Cherry")))
   (define-values (s1 r1) (component-handle-input sl "a" (initial-ui-state)))
   (check-true (input-consumed? r1))
   (define lines (component-render sl (initial-ui-state) 80))
   ;; Filter "a" should show "Apple" and "Banana" (contain 'a')
   (check-true (>= (length lines) 2)))
 ;; ──────────────────────────────
 ;; BorderedLoader tests
 ;; ──────────────────────────────
 (test-case "make-bordered-loader creates component"
   (define bl (make-bordered-loader "Loading..."))
   (check-equal? (q-component-id bl) 'loader))
 (test-case "bordered-loader renders frame with status"
   (define bl (make-bordered-loader "Processing"))
   (define lines (component-render bl (initial-ui-state) 40))
   (check-true (>= (length lines) 3)))
 ;; ──────────────────────────────
 ;; SettingsList tests
 ;; ──────────────────────────────
 (test-case "make-settings-list creates interactive component"
   (define settings
     (make-settings-list (list (settings-entry 'dark-mode "Dark Mode" #t 'boolean)
                               (settings-entry 'username "Username" "admin" 'string))))
   (check-equal? (q-component-id settings) 'settings)
   (check-true (q-component-wants-focus? settings)))
 (test-case "settings-list renders entries"
   (define settings
     (make-settings-list (list (settings-entry 'dark-mode "Dark Mode" #t 'boolean)
                               (settings-entry 'font-size "Font Size" 12 'number))))
   (define lines (component-render settings (initial-ui-state) 80))
   (check-equal? (length lines) 2))
 (test-case "settings-list toggles boolean on enter"
   (define changed (box #f))
   (define settings
     (make-settings-list (list (settings-entry 'dark-mode "Dark Mode" #t 'boolean))
                         #:on-change (lambda (k v) (set-box! changed (cons k v)))))
   (define-values (s1 r1) (component-handle-input settings 'enter (initial-ui-state)))
   (check-true (input-action? r1))
   (check-equal? (unbox changed) '(dark-mode . #f)))
 (test-case "settings-list navigates up/down"
   (define settings
     (make-settings-list (list (settings-entry 'a "A" #t 'boolean)
                               (settings-entry 'b "B" #f 'boolean))))
   (define-values (s1 r1) (component-handle-input settings 'down (initial-ui-state)))
   (check-true (input-consumed? r1))
   (define-values (s2 r2) (component-handle-input settings 'up (initial-ui-state)))
   (check-true (input-consumed? r2)))
 ;; ──────────────────────────────
 ;; Overlay positioning tests
 ;; ──────────────────────────────
 (test-case "overlay-compute-bounds top-left"
   (define ov (overlay-state 'test '() "" 'top-left 40 10 2))
   (define-values (x y w h) (overlay-compute-bounds ov 120 40))
   (check-equal? x 2)
   (check-equal? y 2)
   (check-equal? w 40)
   (check-equal? h 10))
 (test-case "overlay-compute-bounds mid-center"
   (define ov (overlay-state 'test '() "" 'mid-center 40 10 0))
   (define-values (x y w h) (overlay-compute-bounds ov 120 40))
   (check-equal? x 40) ; (120-40)/2
   (check-equal? y 15)) ; (40-10)/2
 (test-case "overlay-compute-bounds bottom-right"
   (define ov (overlay-state 'test '() "" 'bottom-right 30 8 1))
   (define-values (x y w h) (overlay-compute-bounds ov 100 30))
   (check-equal? x 69) ; 100-30-1
   (check-equal? y 21)) ; 30-8-1
 (test-case "overlay-compute-bounds percentage sizing"
   (define ov (overlay-state 'test '() "" 'mid-center (cons 'pct 0.5) (cons 'pct 0.3) 0))
   (define-values (x y w h) (overlay-compute-bounds ov 100 40))
   (check-equal? w 50) ; 50% of 100
   (check-equal? h 12)) ; 30% of 40
 (test-case "overlay-config struct"
   (define cfg (overlay-config 'mid-center (cons 'pct 0.6) (cons 'pct 0.4) 2))
   (check-equal? (overlay-config-anchor cfg) 'mid-center)
   (check-equal? (overlay-config-width-spec cfg) (cons 'pct 0.6)))
 (test-case "show-overlay-with-config sets active-overlay"
   (define s0 (initial-ui-state))
   (define cfg (overlay-config 'top-center 50 10 1))
   (define s1 (show-overlay-with-config s0 cfg 'palette '()))
   (check-true (overlay-active? s1))
   (check-equal? (overlay-state-anchor (ui-state-active-overlay s1)) 'top-center)))

(module+ main
  (run-tests test-tui-builtins))
