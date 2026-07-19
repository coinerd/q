#lang racket/base

;; @speed fast  ;; @suite default

;; BOUNDARY: integration

;; W4 A-09 (F-11): GUI must resume the EXACT requested session instead of
;; unconditionally creating a new sibling. gui-resolve-session is the GUI's
;; session-construction seam (no display required to test it).

(require rackunit
         racket/file
         racket/list
         (only-in "helpers/mock-provider.rkt" make-test-config make-simple-mock-provider)
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  session-id
                  agent-session-session-dir
                  session-active?)
         (only-in "../runtime/session/session-store.rkt" append-entry!)
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         "../gui/main.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-gui-resume-~a" 'directory))

(define (make-test-runtime-config dir)
  (define bus (make-event-bus))
  (define prov (make-simple-mock-provider))
  (make-test-config dir bus prov))

(test-case "gui-resolve-session with requested id resumes exact session and loads history"
  (define dir (make-temp-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create a real session S1 and persist one message.
     (define cfg (make-test-runtime-config dir))
     (define sess1 (make-agent-session cfg))
     (define sid (session-id sess1))
     (append-entry! (build-path (agent-session-session-dir sess1) "session.jsonl")
                    (make-message "m1" #f 'user 'user (list (make-text-part "hello")) 1000 (hasheq)))
     ;; Snapshot the siblings present before GUI resolution.
     (define before (sort (directory-list dir) string<? #:key path->string))
     ;; GUI bootstrap with the requested id.
     (define cfg-with-id (hash-set cfg 'session-id sid))
     (define sess2 (gui-resolve-session cfg-with-id))
     ;; Exact id, same directory, no new sibling created.
     (check-equal? (session-id sess2) sid)
     (check-equal? (path->string (simplify-path (agent-session-session-dir sess2)))
                   (path->string (simplify-path (agent-session-session-dir sess1))))
     (define after (sort (directory-list dir) string<? #:key path->string))
     (check-equal? after before "GUI resume must not create a sibling session"))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "gui-resolve-session without an id creates a new session"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess (gui-resolve-session cfg))
                  (check-true (session-active? sess))
                  (check-pred string? (session-id sess) "new session has a fresh id"))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "gui-resolve-session with a missing/invalid id fails before any window launch"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define cfg-bad (hash-set cfg 'session-id "does-not-exist-12345"))
                  (check-exn exn:fail?
                             (lambda () (gui-resolve-session cfg-bad))
                             "missing session must fail rather than silently creating a new one"))
                (lambda () (delete-directory/files dir #:must-exist? #f))))
