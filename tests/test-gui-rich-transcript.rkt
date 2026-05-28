#lang racket

;; q/tests/test-gui-rich-transcript.rkt — Tests for rich-transcript-view pure helpers

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/components/rich-transcript-view.rkt")

(define-test-suite test-gui-rich-transcript
                   (test-case "role->label maps known roles"
                     (check-equal? (role->label "user") "You")
                     (check-equal? (role->label "assistant") "Assistant")
                     (check-equal? (role->label "system") "System")
                     (check-equal? (role->label "tool") "Tool"))
                   (test-case "role->label handles unknown role"
                     (check-equal? (role->label "custom") "Custom"))
                   (test-case "role->label handles #f"
                     (check-equal? (role->label #f) "Unknown"))
                   (test-case "role->color returns theme colors"
                     (define t (default-theme))
                     (check-equal? (role->color "user" t) (theme-ref t 'accent))
                     (check-equal? (role->color "assistant" t) (theme-ref t 'foreground))
                     (check-equal? (role->color "system" t) (theme-ref t 'muted))
                     (check-equal? (role->color "tool" t) (theme-ref t 'warning)))
                   (test-case "hex->color-object parses hex"
                     (define c (hex->color-object "#1e1e2e"))
                     (check-equal? (hash-ref c 'r) 30)
                     (check-equal? (hash-ref c 'g) 30)
                     (check-equal? (hash-ref c 'b) 46))
                   (test-case "hex->color-object handles no hash prefix"
                     (define c (hex->color-object "ff0000"))
                     (check-equal? (hash-ref c 'r) 255)
                     (check-equal? (hash-ref c 'g) 0)
                     (check-equal? (hash-ref c 'b) 0))
                   (test-case "make-role-label-delta produces descriptor"
                     (define d (make-role-label-delta "You" "#89b4fa"))
                     (check-equal? (hash-ref d 'type) 'role-label)
                     (check-true (hash-ref d 'bold))
                     (check-equal? (hash-ref d 'label) "You"))
                   (test-case "make-content-delta produces descriptor"
                     (define d (make-content-delta "#cdd6f4"))
                     (check-equal? (hash-ref d 'type) 'content)
                     (check-false (hash-ref d 'bold))))

(run-tests test-gui-rich-transcript)
