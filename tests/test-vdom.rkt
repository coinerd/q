#lang racket

;; tests/test-vdom.rkt — Tests for tui/vdom.rkt

(require rackunit
         "../tui/vdom.rkt")

;; ============================================================
;; Constructors
;; ============================================================

(test-case "vtext constructs with text and style"
  (define v (vtext "Hello" '(bold)))
  (check-equal? (vtext-text v) "Hello")
  (check-equal? (vtext-style v) '(bold)))

(test-case "vtext* convenience constructor"
  (define v (vtext* "World" 'bold 'red))
  (check-equal? (vtext-text v) "World")
  (check-equal? (vtext-style v) '(bold red)))

(test-case "vhbox constructs with children"
  (define v (vhbox (list (vtext "A" '()) (vtext "B" '()))))
  (check-equal? (length (vhbox-children v)) 2))

(test-case "vvbox constructs with children"
  (define v (vvbox (list (vtext "Line1" '()) (vtext "Line2" '()))))
  (check-equal? (length (vvbox-children v)) 2))

(test-case "vfill constructs with width"
  (define v (vfill 10 #\space '()))
  (check-equal? (vfill-width v) 10)
  (check-equal? (vfill-char v) #\space))

(test-case "vfill* convenience constructor"
  (define v (vfill* 5))
  (check-equal? (vfill-width v) 5)
  (check-equal? (vfill-char v) #\space))

(test-case "voverlay constructs with content and anchor"
  (define v (voverlay (vtext "overlay" '()) (vtext "base" '()) 2 3))
  (check-equal? (voverlay-col v) 2)
  (check-equal? (voverlay-row v) 3))

;; ============================================================
;; vnode? predicate
;; ============================================================

(test-case "vnode? recognizes all vnode types"
  (check-true (vnode? (vtext "x" '())))
  (check-true (vnode? (vhbox '())))
  (check-true (vnode? (vvbox '())))
  (check-true (vnode? (vfill 5 #\space '())))
  (check-true (vnode? (voverlay (vtext "a" '()) (vtext "b" '()) 0 0))))

(test-case "vnode? rejects non-vnodes"
  (check-false (vnode? "string"))
  (check-false (vnode? 42))
  (check-false (vnode? '()))
  (check-false (vnode? (hash))))

;; ============================================================
;; vnode-text-length
;; ============================================================

(test-case "vnode-text-length for vtext"
  (check-equal? (vnode-text-length (vtext "Hello" '())) 5))

(test-case "vnode-text-length for vhbox sums children"
  (define v (vhbox (list (vtext "AB" '()) (vtext "CD" '()))))
  (check-equal? (vnode-text-length v) 4))

(test-case "vnode-text-length for vvbox takes max"
  (define v (vvbox (list (vtext "Short" '()) (vtext "Longer" '()))))
  (check-equal? (vnode-text-length v) 6))

(test-case "vnode-text-length for vfill returns width"
  (check-equal? (vnode-text-length (vfill 20 #\space '())) 20))

(test-case "vnode-text-length for empty vhbox"
  (check-equal? (vnode-text-length (vhbox '())) 0))

;; ============================================================
;; vnode-height
;; ============================================================

(test-case "vnode-height for vtext is 1"
  (check-equal? (vnode-height (vtext "x" '())) 1))

(test-case "vnode-height for vvbox sums children"
  (define v (vvbox (list (vtext "A" '()) (vtext "B" '()) (vtext "C" '()))))
  (check-equal? (vnode-height v) 3))

(test-case "vnode-height for vhbox takes max"
  (define v (vhbox (list (vtext "A" '()) (vvbox (list (vtext "B" '()) (vtext "C" '()))))))
  (check-equal? (vnode-height v) 2))

(test-case "vnode-height for vfill is 1"
  (check-equal? (vnode-height (vfill 5 #\space '())) 1))

;; ============================================================
;; vnode-map-text
;; ============================================================

(test-case "vnode-map-text transforms text nodes"
  (define tree (vhbox (list (vtext "hello" '()) (vtext "world" '()))))
  (define mapped
    (vnode-map-text (lambda (v) (vtext (string-upcase (vtext-text v)) (vtext-style v))) tree))
  (check-equal? (vtext-text (car (vhbox-children mapped))) "HELLO")
  (check-equal? (vtext-text (cadr (vhbox-children mapped))) "WORLD"))

(test-case "vnode-map-text preserves structure"
  (define tree (vvbox (list (vhbox (list (vtext "x" '()))))))
  (define mapped (vnode-map-text identity tree))
  (check-equal? tree mapped))

(test-case "vnode-map-text handles voverlay"
  (define tree (voverlay (vtext "over" '()) (vtext "base" '()) 0 0))
  (define mapped
    (vnode-map-text (lambda (v) (vtext (string-append "!" (vtext-text v)) (vtext-style v))) tree))
  (check-equal? (vtext-text (voverlay-content mapped)) "!over")
  (check-equal? (vtext-text (voverlay-anchor mapped)) "!base"))

;; ============================================================
;; Nested structures
;; ============================================================

(test-case "deeply nested vnode tree"
  (define tree
    (vvbox (list (vhbox (list (vtext "Header" '(bold)) (vfill* 10) (vtext "Time" '())))
                 (vtext "Body line 1" '())
                 (vtext "Body line 2" '())
                 (voverlay (vtext "popup" '(inverse)) (vtext "behind" '()) 5 2))))
  (check-true (vnode? tree))
  (check-equal? (vnode-height tree) 4)
  (check-true (> (vnode-text-length tree) 0)))
