#lang racket/base

;; tui/stderr-guard.rkt — BUG-0072: the TUI owns its error-level log stream.
;;
;; Problem: Racket's default logger installs a stderr pump at process start
;; that holds the original fd-2 port (it does not read `current-error-port`
;; dynamically, so parameterizing cannot suppress it). Any error-level record
;; — e.g. BUG-0068's escalated hook-failure log-error — is therefore printed
;; with a full backtrace into the TUI frame mid-redraw: stale-line fragments,
;; character-interleaved rows, and exception text in the prompt area.
;;
;; Guard mechanism (defense in depth; Layer-1 payload fixes prevent the
;; dominant per-tool-call source, this module hardens the frame itself):
;;   1. Swap `current-logger` to an UNPARENTED 'q-tui logger. Bare
;;      `log-error`/`log-debug` calls resolve the logger dynamically, so
;;      error-level records from runtime code stop reaching the original
;;      pump. (Module-bound `define-logger` topics keep their own parents —
;;      a documented limitation; they are rare at error level.)
;;   2. A log receiver on the new logger surfaces each error-level record:
;;      written to a spill file (nothing lost) and offered to an optional
;;      `#:on-error-record` callback for TUI status-line display — visible
;;      through the UI, honoring BUG-0068's intent.
;;   3. `restore-stderr-guard!` reinstates the original logger, stops the
;;      pump thread, and closes the spill file. Called on TUI teardown.

(require racket/date
         racket/file)

(provide install-stderr-guard!
         restore-stderr-guard!
         stderr-guard-active?
         stderr-guard-spill-path)

(struct stderr-guard-state (logger receiver thread spill-port spill-path original-logger)
  #:transparent)

(define current-guard (box #f))

(define (stderr-guard-active?)
  (and (unbox current-guard) #t))

(define (stderr-guard-spill-path)
  (define g (unbox current-guard))
  (and g (stderr-guard-state-spill-path g)))

;; install-stderr-guard! : [#:on-error-record (-> string? any/c)] -> path-string?
;; Returns the spill path. Re-installation while active is a no-op returning
;; the existing spill path.
(define (install-stderr-guard! #:on-error-record [on-error-record #f])
  (or (stderr-guard-spill-path)
      (let* ([logger (make-logger 'q-tui #f)] ; unparented: pump never sees these
             [spill-path (make-temporary-file "q-tui-stderr-~a.log")]
             [spill-port (open-output-file spill-path #:exists 'truncate/replace)]
             [receiver (make-log-receiver logger 'error)]
             [original-logger (current-logger)]
             [thread-thunk
              (lambda ()
                (let loop ()
                  (define v (sync receiver))
                  ;; v = (vector level message data topic)
                  (define topic (let ([t (vector-ref v 3)]) (if (symbol? t) t 'unknown)))
                  (define message (vector-ref v 1))
                  (define line
                    (format "[~a] [~a] ~a~n" (date->string (current-date) #t) topic message))
                  (with-handlers ([exn:fail? void])
                    (display line spill-port)
                    (flush-output spill-port))
                  (when on-error-record
                    (with-handlers ([exn:fail? void])
                      (on-error-record message)))
                  (loop)))])
        (current-logger logger)
        (define state
          (stderr-guard-state logger
                              receiver
                              (thread thread-thunk)
                              spill-port
                              spill-path
                              original-logger))
        (set-box! current-guard state)
        spill-path)))

;; restore-stderr-guard! : -> void?
;; Idempotent. Restores the pre-guard logger and releases resources.
(define (restore-stderr-guard!)
  (define g (unbox current-guard))
  (when g
    (set-box! current-guard #f)
    (with-handlers ([exn:fail? void])
      (kill-thread (stderr-guard-state-thread g)))
    (with-handlers ([exn:fail? void])
      (close-output-port (stderr-guard-state-spill-port g)))
    (current-logger (stderr-guard-state-original-logger g))))
