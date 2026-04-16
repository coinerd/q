#lang racket

;; tests/tui/theme.rkt — Tests for TUI theme system (FEAT-68 expanded)

(require rackunit
         rackunit/text-ui
         "../../tui/theme.rkt")

(define theme-tests
  (test-suite "TUI Theme"

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

    ;; FEAT-68: theme-color convenience function
    (test-case "theme-color resolves semantic names to SGR"
      (parameterize ([current-tui-theme default-dark-theme])
        (check-equal? (theme-color 'error) "31")
        (check-equal? (theme-color 'success) "32")
        (check-equal? (theme-color 'warning) "33")
        (check-equal? (theme-color 'accent) "36")
        (check-equal? (theme-color 'muted) "90")
        (check-equal? (theme-color 'nonexistent) #f)))

    (test-case "theme-color respects theme parameter"
      (parameterize ([current-tui-theme default-light-theme])
        (check-equal? (theme-color 'accent) "34")
        (check-equal? (theme-color 'text) "30")))

    (test-case "theme-style->sgr combines attributes"
      (define bold-sgr (theme-style->sgr '(bold)))
      (check-not-false (string-contains? bold-sgr "1"))
      (define ul-sgr (theme-style->sgr '(underline)))
      (check-not-false (string-contains? ul-sgr "4"))
      (check-equal? (theme-style->sgr '()) ""))

    (test-case "theme-style->sgr resolves theme semantic colors"
      (parameterize ([current-tui-theme default-dark-theme])
        (define sgr (theme-style->sgr '(cyan)))
        (check-not-false (string-contains? sgr "36"))))

    (test-case "register-theme! stores a named theme"
      (define custom
        (tui-theme 'white
                   'magenta
                   'bright-black
                   'red
                   'green
                   'yellow
                   'magenta
                   'cyan
                   'bright-green
                   'bright-black
                   'bright-white
                   'bright-black
                   'cyan
                   'bright-black
                   'cyan
                   'bright-black
                   'blue
                   'cyan
                   'cyan
                   'bright-black
                   'bright-black
                   'cyan
                   'green
                   'red
                   'bright-black
                   'bright-black
                   'magenta
                   'bright-green
                   'yellow
                   'cyan
                   'cyan
                   'bright-black))
      (register-theme! 'custom custom)
      (parameterize ([current-tui-theme custom])
        (check-equal? (theme-color 'accent) "35")))))

(run-tests theme-tests)
