#lang racket

;; tests/tui/theme.rkt — Tests for TUI theme system (FEAT-68 expanded)

(require rackunit
         rackunit/text-ui
         "../../tui/theme.rkt"
         json
         racket/file)

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
        (check-equal? (theme-color 'accent) "35")))

    ;; -------------------------------------------------------
    ;; JSON theme loading (GC-25)
    ;; -------------------------------------------------------
    (test-case "tui-theme-field-names has 32 entries"
      (check-equal? (length tui-theme-field-names) 32)
      (check-equal? (car tui-theme-field-names) 'text)
      (check-equal? (last tui-theme-field-names) 'border-muted))

    (test-case "validate-color-value handles all types"
      (check-false (validate-color-value #f))
      (check-false (validate-color-value 'null))
      (check-equal? (validate-color-value "white") "white")
      (check-equal? (validate-color-value 'cyan) 'cyan)
      (check-false (validate-color-value 42)))

    (test-case "load-theme-from-json returns #f for missing file"
      (check-false (load-theme-from-json "/nonexistent/path/theme.json")))

    (test-case "load-theme-from-json returns #f for invalid JSON"
      (define tmp (make-temporary-file "theme-test-~a"))
      (call-with-output-file tmp (lambda (out) (displayln "{broken json!!!" out)) #:exists 'replace)
      (check-false (load-theme-from-json tmp))
      (delete-file tmp))

    (test-case "load-theme-from-json loads valid theme"
      (define tmp (make-temporary-file "theme-test-~a"))
      (call-with-output-file tmp
                             (lambda (out) (write-json (hasheq 'text "white" 'accent "cyan") out))
                             #:exists 'replace)
      (define t (load-theme-from-json tmp))
      (check-not-false t)
      (when t
        (check-equal? (tui-theme-text t) "white")
        (check-equal? (tui-theme-accent t) "cyan")
        ;; Unspecified fields inherit from default-dark-theme
        (check-equal? (tui-theme-border t) (tui-theme-border default-dark-theme)))
      (delete-file tmp))

    (test-case "load-theme-from-json: null values inherit from base"
      (define tmp (make-temporary-file "theme-test-~a"))
      (call-with-output-file tmp
                             (lambda (out) (write-json (hasheq 'text 'null) out))
                             #:exists 'replace)
      (define t (load-theme-from-json tmp))
      (check-not-false t)
      (when t
        (check-equal? (tui-theme-text t) (tui-theme-text default-dark-theme)))
      (delete-file tmp))

    (test-case "json-hash->theme merges partial hash"
      (define h (make-hasheq '((text . "bright-white") (error . "red"))))
      (define t (json-hash->theme h))
      (check-equal? (tui-theme-text t) "bright-white")
      (check-equal? (tui-theme-error t) "red")
      ;; Unspecified fields inherit from default-dark-theme
      (check-equal? (tui-theme-accent t) (tui-theme-accent default-dark-theme)))

    (test-case "theme-field-accessor returns correct accessor"
      (check-equal? ((theme-field-accessor 'text) default-dark-theme)
                    (tui-theme-text default-dark-theme))
      (check-equal? ((theme-field-accessor 'accent) default-dark-theme)
                    (tui-theme-accent default-dark-theme))
      (check-equal? ((theme-field-accessor 'nonexistent) default-dark-theme) #f))))

(run-tests theme-tests)
