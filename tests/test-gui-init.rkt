#lang racket

;; q/tests/test-gui-init.rkt — Tests for GUI scaffold
;;
;; All tests are headless-safe: they test module structure,
;; not actual window rendering.

(require rackunit
         rackunit/text-ui
         racket/port
         "../cli/args.rkt"
         "../gui/main.rkt")

(define-test-suite test-gui-init
                   (test-case "gui/main.rkt provides run-gui-with-runtime"
                     (check-not-false (procedure? run-gui-with-runtime)))
                   (test-case "gui/main.rkt provides run-gui"
                     (check-not-false (procedure? run-gui)))
                   (test-case "run-gui-with-runtime accepts two arguments"
                     (check-equal? (procedure-arity run-gui-with-runtime) 2))
                   (test-case "--gui flag recognized by parse-cli-args"
                     (define cfg
                       (parameterize ([current-command-line-arguments (vector "--gui")])
                         (parse-cli-args)))
                     (check-equal? (cli-config-mode cfg) 'gui))
                   (test-case "info.rkt declares gui-easy-lib dependency"
                     (define info-text (file->string "info.rkt"))
                     (check-not-false (regexp-match #rx"gui-easy-lib" info-text))))

(run-tests test-gui-init)
