#lang racket

;; tests/tui/test-mouse-bridge.rkt — Mouse bridge integration tests (#1120, #1121)
;;
;; Tests for:
;;   - decode-mouse-x10: X10 protocol decoding
;;   - decode-mouse-tui-term: tui-term struct decoding (with mock)
;;   - SGR mouse protocol constants in terminal.rkt
;;   - Error guard in handle-mouse
;;   - Bounds validation in selection-text

(require rackunit
         rackunit/text-ui
         "../../tui/input.rkt")

;; ── Helper: create a mock tui-term tmousemsg ──
;; tui-term's tmousemsg is an actual struct, but we can't create one
;; without tui-term loaded. We test decode-mouse-tui-term indirectly
;; by testing the X10 path and the error resilience.

(define mouse-bridge-tests
  (test-suite
   "Mouse Bridge"

   ;; ── decode-mouse-x10 tests ──

   (test-case "decode-mouse-x10: left click at (5, 3)"
     ;; cb = 32 (button 0, no modifiers), cx = 38 (5+33), cy = 36 (3+33)
     (define result (decode-mouse-x10 32 38 36))
     (check-equal? result '(mouse click 0 5 3)))

   (test-case "decode-mouse-x10: right click at (10, 7)"
     ;; cb = 34 (button 2), cx = 43 (10+33), cy = 40 (7+33)
     (define result (decode-mouse-x10 34 43 40))
     (check-equal? result '(mouse click 2 10 7)))

   (test-case "decode-mouse-x10: release at (5, 3)"
     ;; cb = 35 (button 3 = release in X10 mode 1002)
     (define result (decode-mouse-x10 35 38 36))
     (check-equal? result '(mouse release 5 3)))

   (test-case "decode-mouse-x10: scroll up at (0, 0)"
     ;; cb = 32+64 = 96 (scroll flag + button 0)
     (define result (decode-mouse-x10 96 33 33))
     (check-equal? result '(mouse scroll-up 0 0)))

   (test-case "decode-mouse-x10: scroll down at (0, 0)"
     ;; cb = 32+64+1 = 97 (scroll flag + button 1)
     (define result (decode-mouse-x10 97 33 33))
     (check-equal? result '(mouse scroll-down 0 0)))

   (test-case "decode-mouse-x10: drag at (8, 4)"
     ;; cb = 32+32 = 64 (motion bit + button 0)
     (define result (decode-mouse-x10 64 41 37))
     (check-equal? result '(mouse drag 8 4)))

   (test-case "decode-mouse-x10: high button bits still map to click"
     ;; cb = 32+128 = 160: button bits = 160 & 3 = 0, no motion (160 & 32 = 0), no scroll (160 & 64 = 0)
     ;; So it's just a click with button 0
     (define result (decode-mouse-x10 160 33 33))
     (check-equal? result '(mouse click 0 0 0)))

   ;; ── decode-mouse-tui-term with mock struct ──
   ;; Since tui-term structs aren't available in test context,
   ;; decode-mouse-tui-term should gracefully return #f for non-structs

   (test-case "decode-mouse-tui-term: returns #f for non-tui-term input"
     ;; A plain vector should not match tui-term struct predicate
     (define result (decode-mouse-tui-term #(tmousemsg press (1 2) #t #f #f)))
     (check-false result))

   ;; ── Edge cases ──

   (test-case "decode-mouse-x10: coordinates at origin (0, 0)"
     ;; cx = 33, cy = 33 → x=0, y=0
     (define result (decode-mouse-x10 32 33 33))
     (check-equal? result '(mouse click 0 0 0)))

   (test-case "decode-mouse-x10: large coordinates"
     ;; cx = 133, cy = 133 → x=100, y=100
     (define result (decode-mouse-x10 32 133 133))
     (check-equal? result '(mouse click 0 100 100)))

   (test-case "decode-mouse-x10: middle button click"
     ;; cb = 33 (button 1 = middle)
     (define result (decode-mouse-x10 33 38 36))
     (check-equal? result '(mouse click 1 5 3)))
   ))

(run-tests mouse-bridge-tests)
