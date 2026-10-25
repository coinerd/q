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
;;   3. `restore-stderr-guard!` reinstates the original logger, drains any
;;      already-queued records (BUG-0098/R98: bounded best-effort), stops the
;;      pump thread, and closes the spill file. Called on TUI teardown.
;;
;; BUG-0098 (R98) capture contract: the spill file remains the single owner
;; of the full record (owner-only permissions). The callback receives TWO
;; arguments — (message captured-spill-path) — where captured-spill-path is
;; the guard's real spill path when THIS record's spill write succeeded, and
;; #f when it failed. Callers must never present "full log: <path>" for a
;; record the guard could not capture. A callback that raises gets a bounded
;; disclosure note appended to the spill file instead of silent loss.

(require racket/date
         racket/file)

(provide install-stderr-guard!
         restore-stderr-guard!
         stderr-guard-active?
         stderr-guard-spill-path
         ;; BUG-0098/R98: read-only observation of the private spill stream.
         ;; Also the seam used to inject spill-write failures in tests (the
         ;; guard keeps no other handle to the port).
         stderr-guard-spill-port)

(struct stderr-guard-state
  (logger receiver thread spill-port spill-path original-logger on-error-record)
  #:transparent)

(define current-guard (box #f))

;; Bound for disclosed failure notes; a note must never become an unbounded
;; copy of an exception message. (The record itself is owned by the spill.)
(define disclosure-note-limit 300)

(define (string-clamp s n)
  (if (<= (string-length s) n) s (substring s 0 n)))

(define (stderr-guard-active?)
  (and (unbox current-guard) #t))

(define (stderr-guard-spill-path)
  (define g (unbox current-guard))
  (and g (stderr-guard-state-spill-path g)))

(define (stderr-guard-spill-port)
  (define g (unbox current-guard))
  (and g (stderr-guard-state-spill-port g)))

;; process-record! : output-port? path-string? (or/c procedure? #f) any/c -> void?
;; One record: append the FULL record line to the private spill file (the
;; durable owner of the record), then surface it via on-error-record with the
;; per-record capture status. Memory stays bounded: the only copy of the full
;; text lives transiently in the spill line; nothing is retained here.
(define (process-record! spill-port spill-path on-error-record v)
  ;; v = (vector level message data topic)
  (define topic (let ([t (vector-ref v 3)]) (if (symbol? t) t 'unknown)))
  (define message
    (let ([m (vector-ref v 1)]) (if (string? m) m (format "~a" m))))
  (define captured?
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (fprintf spill-port "[~a] [~a] ~a~n" (date->string (current-date) #t) topic message)
      (flush-output spill-port)
      #t))
  (when on-error-record
    (with-handlers
        ([exn:fail?
          (lambda (e)
            ;; BUG-0098/R98: a failed surface attempt is disclosed where it is
            ;; inspectable. Never pretend the record was surfaced.
            (with-handlers ([exn:fail? void])
              (fprintf spill-port
                       "[~a] [q-tui-stderr-guard] diagnostic surface FAILED (~a); record ~a~n"
                       (date->string (current-date) #t)
                       (string-clamp (exn-message e) disclosure-note-limit)
                       (if captured?
                           "captured above but NOT surfaced"
                           "NOT captured (spill write also failed)"))
              (flush-output spill-port)))])
      (on-error-record message (and captured? spill-path)))))

;; install-stderr-guard! : [#:on-error-record (-> string? (or/c #f path-string?) any/c)]
;;                         -> path-string?
;; Returns the spill path. Re-installation while active is a no-op returning
;; the existing spill path.
(define (install-stderr-guard! #:on-error-record [on-error-record #f])
  (or (stderr-guard-spill-path)
      (let* ([logger (make-logger 'q-tui #f)] ; unparented: pump never sees these
             [spill-path (make-temporary-file "q-tui-stderr-~a.log")]
             ;; BUG-0098/R98: owner-only spill regardless of umask
             ;; (make-temporary-file already uses #o600; pinned explicitly).
             [spill-port (open-output-file spill-path
                                           #:exists 'truncate/replace
                                           #:permissions #o600)]
             [receiver (make-log-receiver logger 'error)]
             [original-logger (current-logger)])
        (current-logger logger)
        (define state
          (stderr-guard-state
           logger
           receiver
           (thread (lambda ()
                     (let loop ()
                       (process-record! spill-port spill-path on-error-record (sync receiver))
                       (loop))))
           spill-port
           spill-path
           original-logger
           on-error-record))
        (set-box! current-guard state)
        spill-path)))

;; restore-stderr-guard! : -> void?
;; Idempotent. Best-effort bounded drain of already-queued records, then
;; restores the pre-guard logger and releases resources.
(define (restore-stderr-guard!)
  (define g (unbox current-guard))
  (when g
    (set-box! current-guard #f)
    ;; BUG-0098/R98: drain whatever is already queued on the receiver so a
    ;; teardown racing log-error does not silently lose records. Bounded: only
    ;; events ready right now are drained; no waiting.
    (let drain ()
      (define v (sync/timeout 0 (stderr-guard-state-receiver g)))
      (when v
        (process-record! (stderr-guard-state-spill-port g)
                         (stderr-guard-state-spill-path g)
                         (stderr-guard-state-on-error-record g)
                         v)
        (drain)))
    (with-handlers ([exn:fail? void])
      (kill-thread (stderr-guard-state-thread g)))
    (with-handlers ([exn:fail? void])
      (close-output-port (stderr-guard-state-spill-port g)))
    (current-logger (stderr-guard-state-original-logger g))))
