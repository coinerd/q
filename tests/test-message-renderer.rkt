#lang racket

;; tests/test-message-renderer.rkt — tests for Overlay Positioning & Message Renderers (#725-#727)
;;
;; Covers:
;;   - #725: Anchor-based overlay positioning
;;   - #726: Custom message type rendering
;;   - #727: Parent feature

(require rackunit
         "../tui/state.rkt"
         "../tui/render.rkt"
         "../extensions/message-renderer.rkt")

;; ============================================================
;; #725: Anchor-based overlay positioning
;; ============================================================

(test-case "anchor?: validates anchor values"
  (check-true (anchor? 'top-left))
  (check-true (anchor? 'center))
  (check-true (anchor? 'bottom-right))
  (check-false (anchor? 'invalid))
  (check-false (anchor? "top-left")))

(test-case "ANCHOR-*: constants are symbols"
  (check-equal? ANCHOR-TOP-LEFT 'top-left)
  (check-equal? ANCHOR-CENTER 'center)
  (check-equal? ANCHOR-BOTTOM-RIGHT 'bottom-right))

(test-case "show-overlay: default anchor is top-left"
  (define state (initial-ui-state))
  (define s1 (show-overlay state 'test '()))
  (check-equal? (overlay-state-anchor (ui-state-active-overlay s1)) 'top-left))

(test-case "show-overlay: custom anchor"
  (define state (initial-ui-state))
  (define s1 (show-overlay state 'test '() "" #:anchor 'center))
  (check-equal? (overlay-state-anchor (ui-state-active-overlay s1)) 'center))

(test-case "show-overlay: custom width and height"
  (define state (initial-ui-state))
  (define s1 (show-overlay state 'test '() "" #:width 40 #:height 10))
  (define ov (ui-state-active-overlay s1))
  (check-equal? (overlay-state-width ov) 40)
  (check-equal? (overlay-state-height ov) 10))

(test-case "show-overlay: custom margin"
  (define state (initial-ui-state))
  (define s1 (show-overlay state 'test '() "" #:margin 2))
  (check-equal? (overlay-state-margin (ui-state-active-overlay s1)) 2))

(test-case "show-overlay: defaults for optional fields"
  (define state (initial-ui-state))
  (define s1 (show-overlay state 'test '()))
  (define ov (ui-state-active-overlay s1))
  (check-equal? (overlay-state-anchor ov) 'top-left)
  (check-false (overlay-state-width ov))
  (check-false (overlay-state-height ov))
  (check-equal? (overlay-state-margin ov) 0))

(test-case "overlay-state: backward compat with positional args"
  ;; Ensure existing code using overlay-state still works
  (define ov (overlay-state 'command-palette '() "" 'top-left #f #f 0))
  (check-equal? (overlay-state-type ov) 'command-palette)
  (check-equal? (overlay-state-input ov) ""))

(test-case "dismiss-overlay: clears active overlay"
  (define state (initial-ui-state))
  (define s1 (show-overlay state 'test '() "" #:anchor 'bottom-right))
  (check-true (overlay-active? s1))
  (define s2 (dismiss-overlay s1))
  (check-false (overlay-active? s2)))

;; ============================================================
;; #726: Custom message type rendering
;; ============================================================

(test-case "make-message-renderer: creates renderer"
  (define r (make-message-renderer 'custom-type
               (lambda (payload)
                 (list (styled-line (list (styled-segment
                                           (format "Custom: ~a" (hash-ref payload 'text "")) '())))))
               "my-ext"))
  (check-equal? (message-renderer-message-type r) 'custom-type)
  (check-true (procedure? (message-renderer-renderer-fn r)))
  (check-equal? (message-renderer-ext-name r) "my-ext"))

(test-case "renderer-registry: register and lookup"
  (define reg (make-renderer-registry))
  (define r (make-message-renderer 'my-type
               (lambda (p) (list (styled-line (list (styled-segment "rendered" '())))))
               "ext"))
  (register-message-renderer! reg r)
  (define found (lookup-message-renderer reg 'my-type))
  (check-true (message-renderer? found))
  (check-equal? (message-renderer-ext-name found) "ext"))

(test-case "renderer-registry: lookup returns #f for unknown"
  (define reg (make-renderer-registry))
  (check-false (lookup-message-renderer reg 'unknown)))

(test-case "renderer-registry: unregister removes renderer"
  (define reg (make-renderer-registry))
  (define r (make-message-renderer 'temp
               (lambda (p) '()) "ext"))
  (register-message-renderer! reg r)
  (check-true (message-renderer? (lookup-message-renderer reg 'temp)))
  (unregister-message-renderer! reg 'temp)
  (check-false (lookup-message-renderer reg 'temp)))

(test-case "renderer-registry: register overwrites"
  (define reg (make-renderer-registry))
  (define r1 (make-message-renderer 'my-type
               (lambda (p) (list (styled-line (list (styled-segment "v1" '()))))) "ext1"))
  (define r2 (make-message-renderer 'my-type
               (lambda (p) (list (styled-line (list (styled-segment "v2" '()))))) "ext2"))
  (register-message-renderer! reg r1)
  (register-message-renderer! reg r2)
  (check-equal? (message-renderer-ext-name (lookup-message-renderer reg 'my-type)) "ext2"))

(test-case "list-message-renderers: returns all"
  (define reg (make-renderer-registry))
  (register-message-renderer! reg (make-message-renderer 'a (lambda (p) '()) "ext"))
  (register-message-renderer! reg (make-message-renderer 'b (lambda (p) '()) "ext"))
  (check-equal? (length (list-message-renderers reg)) 2))

(test-case "render-custom-message: uses registered renderer"
  (define reg (make-renderer-registry))
  (register-message-renderer! reg
    (make-message-renderer 'greeting
      (lambda (payload)
        (list (styled-line (list (styled-segment
                                  (format "Hello, ~a!" (hash-ref payload 'name "world")) '())))))
      "ext"))
  (define result (render-custom-message reg 'greeting (hasheq 'name "user")))
  (check-equal? (length result) 1)
  (check-equal? (styled-line->text (car result)) "Hello, user!"))

(test-case "render-custom-message: returns #f for unregistered type"
  (define reg (make-renderer-registry))
  (check-false (render-custom-message reg 'unknown (hasheq))))

;; ============================================================
;; #727: Integration
;; ============================================================

(test-case "integration: overlay with anchor + custom message renderer"
  ;; Show overlay with custom positioning
  (define state (initial-ui-state))
  (define lines (list (styled-line (list (styled-segment "Overlay content" '(bold))))))
  (define s1 (show-overlay state 'custom lines ""
                            #:anchor 'center
                            #:width 40
                            #:height 5
                            #:margin 1))
  (define ov (ui-state-active-overlay s1))
  (check-equal? (overlay-state-anchor ov) 'center)
  (check-equal? (overlay-state-width ov) 40)

  ;; Register a message renderer for the overlay content type
  (define reg (make-renderer-registry))
  (register-message-renderer! reg
    (make-message-renderer 'custom
      (lambda (p) (list (styled-line (list (styled-segment "Rendered!" '())))))
      "ext"))
  (define rendered (render-custom-message reg 'custom (hasheq)))
  (check-equal? (styled-line->text (car rendered)) "Rendered!"))
