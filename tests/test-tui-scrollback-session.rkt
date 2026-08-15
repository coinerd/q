#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-tui-scrollback-session.rkt — BUG-0001 regression test
;;
;; The TUI must compute the scrollback path from the SESSION, not from a
;; global base dir. Before the fix (v1.00.00) create-tui-session
;; returned <base>/scrollback.jsonl — one file shared by every session —
;; so each new session loaded the previous session's buffer at startup
;; and, worse, any exit flushed the mixed buffer back into that shared
;; file. The per-session path must now be
;; <base>/<session-id>/scrollback.jsonl, and:
;;   * a NEW session starts from an empty buffer even though the previous
;;     session's scrollback file exists on disk;
;;   * RESUMING a session restores exactly that session's buffer.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string         "../util/event/event-bus.rkt"
         "../tools/tool.rkt"
         "../tui/context.rkt"
         "../tui/state.rkt"
         "../tui/tui-init.rkt"
         "../tui/scrollback.rkt"
         "../runtime/agent-session.rkt"
         (only-in "../util/config-paths.rkt" global-config-dir)
         (only-in "helpers/mock-provider.rkt" make-simple-mock-provider))

;; Mirror of the first-run detection in load-tui-scrollback (tui-init.rkt):
;; welcome entries are appended ONLY for first-run users, so the expected
;; initial buffer differs between dev machines and fresh CI containers.
(define (tui-first-run?)
  (define q-config-dir (global-config-dir))
  (not (or (directory-exists? q-config-dir)
           (file-exists? (build-path q-config-dir "config.json")))))

(define (make-rt-config tmp-dir bus prov)
  (hasheq 'provider prov
          'tool-registry (make-tool-registry)
          'event-bus bus
          'session-dir (path->string tmp-dir)
          'model-name "test"))

;; Headless equivalent of TUI startup for one session: create the session,
;; load its scrollback into the ctx state box, return the resulting state.
(define (start-session cfg)
  (define-values (ctx sess path) (create-tui-session cfg #f))
  (load-tui-scrollback ctx sess cfg path)
  (values ctx sess path (unbox (tui-ctx-ui-state-box ctx))))

(define (transcript-texts state)
  (map transcript-entry-text (ui-state-transcript state)))

(define-test-suite tui-scrollback-session-suite
  (test-case
   "scrollback path is session-scoped, not global"
   (define tmp-dir (make-temporary-file "q-scrollback-test-~a" 'directory))
   (dynamic-wind
    void
    (lambda ()
      (define-values (ctx sess path) (create-tui-session (make-rt-config tmp-dir (make-event-bus) (make-simple-mock-provider)) #f))
      (define sid (session-id sess))
      (check-equal? path (build-path tmp-dir sid "scrollback.jsonl"))
      ;; The old bug: a single <base>/scrollback.jsonl shared by all sessions.
      (check-not-equal? path (build-path tmp-dir "scrollback.jsonl"))
      ;; The per-session directory must exist (create-tui-session
      ;; guarantees it via make-agent-session's eager ensure-persisted!).
      (check-true (directory-exists? (agent-session-session-dir sess)))
      ;; First-ever session: no scrollback file yet.
      (check-false (file-exists? path))
      (void))
    (lambda () (delete-directory/files tmp-dir #:must-exist? #f))))

  (test-case
   "new session B starts empty even though session A wrote scrollback"
   (define tmp-dir (make-temporary-file "q-scrollback-test-~a" 'directory))
   (dynamic-wind
    void
    (lambda ()
      ;; Session A runs, writes its scrollback, exits.
      (define-values (_a sess-a path-a state-a) (start-session (make-rt-config tmp-dir (make-event-bus) (make-simple-mock-provider))))
      (define a-marker "A-ONLY-MARKER-a1b2c3")
      (save-scrollback
       (append (ui-state-transcript state-a)
               (list (make-entry 'user a-marker 1000.0 (hasheq 'test #t))))
       path-a)
      (check-true (file-exists? path-a))

      ;; Session B starts with a NEW session id in the same base dir.
      (define-values (ctx-b sess-b path-b state-b) (start-session (make-rt-config tmp-dir (make-event-bus) (make-simple-mock-provider))))
      (check-not-equal? (session-id sess-b) (session-id sess-a))
      (check-not-equal? path-b path-a)
      ;; B's file does not exist → B loads an empty buffer.
      (check-false (file-exists? path-b))
      (check-equal? (load-scrollback path-b) '())
      ;; B's in-memory buffer contains NO content from A. Startup banners
      ;; (provider-info line, and the two first-run welcome lines) are the
      ;; only permitted entries.
      (define texts-b (transcript-texts state-b))
      (check-false (ormap (lambda (t) (string-contains? t a-marker)) texts-b))
      (define startup-banner?
        (lambda (t)
          (or (string-prefix? t "Provider:")
              (string-contains? t "Welcome to q!")
              (string-contains? t "Commands:"))))
      (check-equal? (filter (lambda (t) (not (startup-banner? t))) texts-b) '())
      (check-true (<= (length texts-b) 3))
      (void))
    (lambda () (delete-directory/files tmp-dir #:must-exist? #f))))

  (test-case
   "resuming session A restores A's buffer"
   (define tmp-dir (make-temporary-file "q-scrollback-test-~a" 'directory))
   (dynamic-wind
    void
    (lambda ()
      (define-values (_a sess-a path-a state-a) (start-session (make-rt-config tmp-dir (make-event-bus) (make-simple-mock-provider))))
      (define sid-a (session-id sess-a))
      (define a-marker "A-ONLY-MARKER-d4e5f6")
      (define entries-a
        (append (ui-state-transcript state-a)
                (list (make-entry 'user a-marker 1000.0 (hasheq 'test #t)))))
      (save-scrollback entries-a path-a)

      ;; Resume A: same session id → same path → A's buffer is restored.
      (define cfg-resume (hash-set (make-rt-config tmp-dir (make-event-bus) (make-simple-mock-provider))
                                   'session-id sid-a))
      (define-values (_r sess-r path-r state-r) (start-session cfg-resume))
      (check-equal? (session-id sess-r) sid-a)
      (check-equal? path-r path-a)
      (check-true (file-exists? path-r))
      (define texts-r (transcript-texts state-r))
      (check-true (and (pair? texts-r)
                       (ormap (lambda (t) (string-contains? t a-marker)) texts-r))
                  "resumed session must restore its own scrollback content")
      ;; next-entry-id advanced past the highest restored id.
      (check-true (>= (ui-state-next-entry-id state-r) (length (ui-state-transcript state-r))))
      (void))
    (lambda () (delete-directory/files tmp-dir #:must-exist? #f)))))

(module+ test
  (void (run-tests tui-scrollback-session-suite)))
