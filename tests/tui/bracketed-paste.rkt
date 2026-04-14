#lang racket

;; tests/tui/bracketed-paste.rkt — Tests for bracketed paste support (#435)
;;
;; Tests bracketed paste begin/end detection, buffer accumulation,
;; and paste-event construction.

(require rackunit
         rackunit/text-ui
         "../../tui/terminal-input.rkt")

(define paste-tests
  (test-suite
   "bracketed-paste"

   ;; Reset state before each test
   (test-case "bracketed-paste-begin-pattern?"
     (check-true (bracketed-paste-begin-pattern? "200" #\~))
     (check-false (bracketed-paste-begin-pattern? "201" #\~))
     (check-false (bracketed-paste-begin-pattern? "200" #\A)))

   (test-case "bracketed-paste-end-pattern?"
     (check-true (bracketed-paste-end-pattern? "201" #\~))
     (check-false (bracketed-paste-end-pattern? "200" #\~))
     (check-false (bracketed-paste-end-pattern? "201" #\A)))

   (test-case "paste-buffer lifecycle"
     (paste-buffer-reset!)
     (check-equal? (paste-buffer-get) "")
     (check-false (in-paste?))

     ;; Start paste
     (set-in-paste! #t)
     (check-true (in-paste?))

     ;; Accumulate
     (paste-buffer-add! "hello ")
     (check-equal? (paste-buffer-get) "hello ")

     (paste-buffer-add! "world")
     (check-equal? (paste-buffer-get) "hello world")

     ;; End paste
     (set-in-paste! #f)
     (check-false (in-paste?))

     ;; Reset
     (paste-buffer-reset!)
     (check-equal? (paste-buffer-get) ""))

   (test-case "paste-buffer-max-size cap"
     (paste-buffer-reset!)
     ;; Add exactly max size (1MB)
     (define big-str (make-string 1048576 #\x))
     (paste-buffer-add! big-str)
     (check-equal? (string-length (paste-buffer-get)) 1048576)
     ;; Adding one more char should be silently dropped (exceeds 1MB)
     (paste-buffer-add! "y")
     (check-equal? (string-length (paste-buffer-get)) 1048576)
     (paste-buffer-reset!))

   (test-case "make-paste-event and paste-event?"
     (define evt (make-paste-event "hello world"))
     (check-true (paste-event? evt))
     (check-false (paste-event? "not an event"))
     (check-false (paste-event? (vector 'other)))
     (check-equal? (vector-ref evt 1) "hello world"))

   (test-case "paste-buffer-add! empty string is noop"
     (paste-buffer-reset!)
     (paste-buffer-add! "")
     (check-equal? (paste-buffer-get) "")
     (paste-buffer-reset!))
   ))

(run-tests paste-tests)
