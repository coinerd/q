#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; Red-first tests for F-11 #8749: resume creates sibling session.
;; All mode runners must resume the exact session-id from rt-config
;; instead of creating a fresh sibling session.

(require rackunit
         racket/file
         racket/dict
         (only-in "helpers/mock-provider.rkt" make-test-config make-simple-mock-provider)
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  session-id
                  session-history)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         (only-in "../wiring/run-modes.rkt" open-or-resume-session)
         "../interfaces/sessions.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-resume-test-~a" 'directory))

(define (make-test-runtime-config dir)
  (define bus (make-event-bus))
  (define prov (make-simple-mock-provider))
  (make-test-config dir bus prov))

;; ============================================================
;; F-11: open-or-resume-session must branch on rt-config session-id
;; ============================================================

(test-case "open-or-resume-session creates new session without session-id"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess (open-or-resume-session cfg))
                  (check-pred string? (session-id sess))
                  (check-true (> (string-length (session-id sess)) 0)))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "open-or-resume-session resumes exact id with session-id present"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess1 (make-agent-session cfg))
                  (define sid1 (session-id sess1))
                  (define cfg-with-sid (dict-set cfg 'session-id sid1))
                  (define sess2 (open-or-resume-session cfg-with-sid))
                  (check-equal? (session-id sess2) sid1))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "open-or-resume-session resumes without creating sibling"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess1 (make-agent-session cfg))
                  (define sid1 (session-id sess1))
                  (define before (sessions-list dir))
                  (define cfg-with-sid (dict-set cfg 'session-id sid1))
                  (define sess2 (open-or-resume-session cfg-with-sid))
                  (check-equal? (session-id sess2) sid1)
                  (define after (sessions-list dir))
                  (check-equal? (length before) (length after) "resume must not create a sibling"))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "open-or-resume-session with missing id fails closed"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define cfg-with-bad-sid (dict-set cfg 'session-id "nonexistent-session-id-12345"))
                  (check-exn exn:fail? (lambda () (open-or-resume-session cfg-with-bad-sid))))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "open-or-resume-session preserves prior history"
  (define dir (make-temp-dir))
  (dynamic-wind
   void
   (lambda ()
     (define cfg (make-test-runtime-config dir))
     (define sess1 (make-agent-session cfg))
     (define sid1 (session-id sess1))
     (define hist1 (session-history sess1))
     (define cfg-with-sid (dict-set cfg 'session-id sid1))
     (define sess2 (open-or-resume-session cfg-with-sid))
     (define hist2 (session-history sess2))
     (check-equal? (length hist1) (length hist2) "resumed session must have same history"))
   (lambda () (delete-directory/files dir #:must-exist? #f))))
