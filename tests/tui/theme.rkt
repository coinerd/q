#lang racket

;; tests/tui/theme.rkt — Tests for TUI theme system

(require rackunit
         rackunit/text-ui
         "../../../q/tui/theme.rkt")

(define theme-tests
  (test-suite
   "TUI Theme"

   (test-case "default-dark-theme has correct fields"
     (check-equal? (tui-theme-text default-dark-theme) 'white)
     (check-equal? (tui-theme-accent default-dark-theme) 'cyan)
     (check-equal? (tui-theme-error default-dark-theme) 'red)
     (check-equal? (tui-theme-success default-dark-theme) 'green)
     (check-equal? (tui-theme-warning default-dark-theme) 'yellow)
     (check-equal? (tui-theme-md-heading default-dark-theme) 'cyan)
     (check-equal? (tui-theme-input-prompt default-dark-theme) 'cyan))

   (test-case "default-light-theme has dark text"
     (check-equal? (tui-theme-text default-light-theme) 'black)
     (check-equal? (tui-theme-accent default-light-theme) 'blue))

   (test-case "theme-ref resolves field names"
     (parameterize ([current-tui-theme default-dark-theme])
       (check-equal? (theme-ref 'text) 'white)
       (check-equal? (theme-ref 'accent) 'cyan)
       (check-equal? (theme-ref 'md-heading) 'cyan)
       (check-equal? (theme-ref 'nonexistent) #f)))

   (test-case "theme-color->sgr: named colors"
     (check-equal? (theme-color->sgr 'red) "31")
     (check-equal? (theme-color->sgr 'cyan) "36")
     (check-equal? (theme-color->sgr 'bright-white) "97")
     (check-equal? (theme-color->sgr 'bright-black) "90")
     (check-equal? (theme-color->sgr #f) #f))

   (test-case "theme-color->sgr-bg: named colors"
     (check-equal? (theme-color->sgr-bg 'blue) "44")
     (check-equal? (theme-color->sgr-bg 'cyan) "46")
     (check-equal? (theme-color->sgr-bg 'bright-black) "100"))

   (test-case "theme-color->sgr: 256-color string passthrough"
     (check-equal? (theme-color->sgr "38;5;202") "38;5;202"))

   (test-case "theme-color->sgr-bg: string converts fg to bg"
     (check-equal? (theme-color->sgr-bg "38;5;202") "48;5;202"))

   (test-case "current-tui-theme parameter"
     (parameterize ([current-tui-theme default-light-theme])
       (check-equal? (tui-theme-text (current-tui-theme)) 'black))
     (check-equal? (tui-theme-text (current-tui-theme)) 'white))

   (test-case "tui-theme? predicate"
     (check-true (tui-theme? default-dark-theme))
     (check-false (tui-theme? 'cyan)))
   ))

(run-tests theme-tests)
