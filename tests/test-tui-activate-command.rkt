#lang racket/base

;; tests/test-tui-activate-command.rkt — Tests for /activate slash command
;;
;; Wave 3 of v0.17.6: /activate TUI command tests.

(require rackunit
         racket/string
         racket/file
         "../tui/commands.rkt"
         "../tui/state.rkt"
         "../extensions/api.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Create a cmd-ctx for testing with the given input text
(define (make-test-cctx #:input-text [input-text "/activate"] #:session-dir [session-dir #f])
  (define state (initial-ui-state))
  (cmd-ctx (box state)
           (box #t)
           #f ; event-bus
           session-dir
           (box #t)
           (box #f) ; model-registry
           (box #f) ; last-prompt
           (lambda (prompt) (void))
           (box input-text)))

;; ============================================================
;; Tests
;; ============================================================

(test-case "/activate with no args shows status (no session-dir needed)"
  (define tmp-dir (make-temporary-file "q-activate-nosess-~a" 'directory))
  (parameterize ([current-directory tmp-dir])
    (define cctx (make-test-cctx))
    (define result (process-slash-command cctx 'activate))
    (check-equal? result 'continue)
    (define state (unbox (cmd-ctx-state-box cctx)))
    (define texts (map transcript-entry-text (ui-state-transcript state)))
    ;; Should show Extension Status heading (not an error)
    (check-not-false (for/or ([t (in-list texts)])
                       (string-contains? t "Extension Status"))))
  (delete-directory/files tmp-dir))

(test-case "/activate with no args shows status"
  (define tmp-dir (make-temporary-file "q-activate-cmd-~a" 'directory))
  (parameterize ([current-directory tmp-dir])
    (define cctx
      (make-test-cctx #:input-text "/activate" #:session-dir (build-path tmp-dir "session")))
    (make-directory* (build-path tmp-dir "session"))
    (define result (process-slash-command cctx 'activate))
    (check-equal? result 'continue)
    (define state (unbox (cmd-ctx-state-box cctx)))
    (define texts (map transcript-entry-text (ui-state-transcript state)))
    ;; Should show Extension Status heading
    (check-not-false (for/or ([t (in-list texts)])
                       (string-contains? t "Extension Status")))
    ;; Should show available extensions
    (check-not-false (for/or ([t (in-list texts)])
                       (string-contains? t "Available extensions"))))
  (delete-directory/files tmp-dir))

(test-case "/activate --available lists extensions"
  (define tmp-dir (make-temporary-file "q-activate-avail-~a" 'directory))
  (parameterize ([current-directory tmp-dir])
    (define cctx
      (make-test-cctx #:input-text "/activate --available"
                      #:session-dir (build-path tmp-dir "session")))
    (make-directory* (build-path tmp-dir "session"))
    (define result (process-slash-command cctx 'activate))
    (check-equal? result 'continue)
    (define state (unbox (cmd-ctx-state-box cctx)))
    (define texts (map transcript-entry-text (ui-state-transcript state)))
    ;; Should list known extensions
    (check-not-false (for/or ([t (in-list texts)])
                       (string-contains? t "gsd-planning"))))
  (delete-directory/files tmp-dir))

(test-case "/activate <name> creates symlink"
  (define tmp-dir (make-temporary-file "q-activate-name-~a" 'directory))
  (parameterize ([current-directory tmp-dir])
    (define cctx
      (make-test-cctx #:input-text "/activate gsd-planning"
                      #:session-dir (build-path tmp-dir "session")))
    (make-directory* (build-path tmp-dir "session"))
    (define result (process-slash-command cctx 'activate))
    (check-equal? result 'continue)
    (define state (unbox (cmd-ctx-state-box cctx)))
    (define texts (map transcript-entry-text (ui-state-transcript state)))
    ;; Should confirm activation
    (check-not-false (for/or ([t (in-list texts)])
                       (and (string-contains? t "gsd-planning") (string-contains? t "activated"))))
    ;; Should have created .q/extensions/ in project dir (= current-directory)
    (define ext-dir (build-path tmp-dir ".q" "extensions"))
    (check-true (directory-exists? ext-dir)))
  (delete-directory/files tmp-dir))

(test-case "/activate <unknown> shows error"
  (define tmp-dir (make-temporary-file "q-activate-unk-~a" 'directory))
  (parameterize ([current-directory tmp-dir])
    (define cctx
      (make-test-cctx #:input-text "/activate nonexistent-extension"
                      #:session-dir (build-path tmp-dir "session")))
    (make-directory* (build-path tmp-dir "session"))
    (define result (process-slash-command cctx 'activate))
    (check-equal? result 'continue)
    (define state (unbox (cmd-ctx-state-box cctx)))
    (define texts (map transcript-entry-text (ui-state-transcript state)))
    ;; Should show error
    (check-not-false (for/or ([t (in-list texts)])
                       (string-contains? t "not found"))))
  (delete-directory/files tmp-dir))
