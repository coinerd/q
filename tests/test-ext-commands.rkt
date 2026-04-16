#lang racket

;; tests/test-ext-commands.rkt — FEAT-60: extension slash command registration

(require rackunit
         "../extensions/context.rkt"
         "../extensions/ext-commands.rkt"
         "../extensions/api.rkt"
         "../tui/palette.rkt"
         "../agent/event-bus.rkt")

;; Helper: create a context with a boxed command registry
(define (make-ctx-with-cmd-reg)
  (define cmd-reg (box (make-command-registry)))
  (make-extension-ctx #:session-id "s-cmd"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:command-registry cmd-reg))

;; ============================================================
;; ext-register-command!
;; ============================================================

(test-case "ext-register-command! adds a command"
  (define ctx (make-ctx-with-cmd-reg))
  (ext-register-command! ctx "/deploy" "Deploy project" 'general '("<target>") '("dp"))
  (define cmd (ext-lookup-command ctx "/deploy"))
  (check-not-false cmd)
  (check-equal? (cmd-entry-name cmd) "/deploy")
  (check-equal? (cmd-entry-summary cmd) "Deploy project")
  (check-equal? (cmd-entry-category cmd) 'general)
  (check-equal? (cmd-entry-aliases cmd) '("dp")))

(test-case "ext-register-command! with empty args and aliases"
  (define ctx (make-ctx-with-cmd-reg))
  (ext-register-command! ctx "/status" "Show status" 'general '() '())
  (define cmd (ext-lookup-command ctx "/status"))
  (check-not-false cmd)
  (check-equal? (cmd-entry-args-spec cmd) '()))

(test-case "ext-register-command! errors without registry"
  (define ctx
    (make-extension-ctx #:session-id "s-noreg"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-exn exn:fail? (lambda () (ext-register-command! ctx "/bad" "desc" 'general '() '()))))

(test-case "ext-register-command! preserves built-in commands"
  (define ctx (make-ctx-with-cmd-reg))
  (ext-register-command! ctx "/mycmd" "My command" 'general '() '())
  ;; Built-in /help should still be there
  (define help (ext-lookup-command ctx "/help"))
  (check-not-false help)
  (check-equal? (cmd-entry-name help) "/help"))

;; ============================================================
;; ext-unregister-command!
;; ============================================================

(test-case "ext-unregister-command! removes a command"
  (define ctx (make-ctx-with-cmd-reg))
  (ext-register-command! ctx "/temp" "Temp" 'general '() '())
  (check-not-false (ext-lookup-command ctx "/temp"))
  (ext-unregister-command! ctx "/temp")
  (check-false (ext-lookup-command ctx "/temp")))

(test-case "ext-unregister-command! errors without registry"
  (define ctx
    (make-extension-ctx #:session-id "s-noreg2"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-exn exn:fail? (lambda () (ext-unregister-command! ctx "/anything"))))

;; ============================================================
;; ext-lookup-command
;; ============================================================

(test-case "ext-lookup-command returns #f for unknown command"
  (define ctx (make-ctx-with-cmd-reg))
  (check-false (ext-lookup-command ctx "/nonexistent")))

(test-case "ext-lookup-command returns #f without registry"
  (define ctx
    (make-extension-ctx #:session-id "s-noreg3"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-false (ext-lookup-command ctx "/help")))

;; ============================================================
;; ext-list-commands
;; ============================================================

(test-case "ext-list-commands returns sorted list"
  (define ctx (make-ctx-with-cmd-reg))
  (ext-register-command! ctx "/zebra" "Z" 'general '() '())
  (ext-register-command! ctx "/alpha" "A" 'general '() '())
  (define cmds (ext-list-commands ctx))
  (check-true (positive? (length cmds)))
  ;; Should be sorted by name
  (define names (map cmd-entry-name cmds))
  (check-equal? names (sort names string<?)))

(test-case "ext-list-commands returns empty without registry"
  (define ctx
    (make-extension-ctx #:session-id "s-noreg4"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-equal? (ext-list-commands ctx) '()))

;; ============================================================
;; Integration: overrides built-in command
;; ============================================================

(test-case "ext-register-command! can override built-in command"
  (define ctx (make-ctx-with-cmd-reg))
  (ext-register-command! ctx "/help" "Custom help" 'general '() '())
  (define cmd (ext-lookup-command ctx "/help"))
  (check-equal? (cmd-entry-summary cmd) "Custom help"))
