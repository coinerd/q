#lang racket/base

;; @speed fast
;; @suite runtime
;; @boundary integration

;; tests/test-session-scratch-lifecycle.rkt — W2 scratch ownership is PER AGENT
;; SESSION, not per process.
;;
;; These are the runtime-lifecycle counterparts of the pure allocation tests in
;; tests/test-tool-scratch-allocation.rkt:
;;   - every created/resumed/forked agent session allocates exactly one trusted
;;     root, keyed by its own session id;
;;   - two sessions in the SAME process never share that root;
;;   - close-session! forgets only that session's binding;
;;   - a root a trusted producer allocated and advertised BEFORE the session
;;     existed is adopted by exactly one session creation.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         "../runtime/agent-session.rkt"
         "../util/event/event-bus.rkt"
         "../tools/scratch.rkt"
         (only-in "helpers/mock-provider.rkt" make-simple-mock-provider make-test-config))

(define (fresh-dir name)
  (make-temporary-file name 'directory))

(define (cleanup dir)
  (delete-directory/files dir #:must-exist? #f))

(define (new-session! prov bus dir)
  (make-agent-session (make-test-config dir bus prov)))

(define session-scratch-suite
  (test-suite "tools/scratch.rkt session lifecycle"

    (test-case "each agent session owns exactly one distinct scratch root"
      (define base (fresh-dir "q-scratch-lifecycle-~a"))
      (define dir (fresh-dir "q-scratch-lifecycle-sessions-~a"))
      (dynamic-wind void
                    (lambda ()
                      (parameterize ([current-scratch-base-dir base])
                        (reset-session-scratch-registry!)
                        (define prov (make-simple-mock-provider "ok"))
                        (define bus (make-event-bus))
                        (define session-a (new-session! prov bus dir))
                        (define session-b (new-session! prov bus dir))
                        (define id-a (session-id session-a))
                        (define id-b (session-id session-b))
                        (define root-a (allocated-session-scratch-root id-a))
                        (define root-b (allocated-session-scratch-root id-b))
                        (check-true (and root-a (path? root-a))
                                    "a created session owns a trusted scratch root")
                        (check-true (and root-b (path? root-b)))
                        (check-not-equal? (path->string root-a)
                                          (path->string root-b)
                                          "two sessions in one process never share a scratch root")
                        (for ([root (in-list (list root-a root-b))])
                          (check-true (trusted-session-scratch-path? root))
                          (check-true (directory-exists? root))
                          (check-equal? (file-or-directory-permissions root 'bits)
                                        #o700
                                        "the session root stays owner-only"))
                        (check-false (allocated-session-scratch-root "no-such-session")
                                     "no allocation, no fallback")
                        (close-session! session-a)
                        (check-false (allocated-session-scratch-root id-a)
                                     "closing a session forgets exactly its own binding")
                        (check-true (same-path? root-b (allocated-session-scratch-root id-b))
                                    "the other session keeps its root")
                        (close-session! session-b)
                        (reset-session-scratch-registry!)))
                    (lambda ()
                      (cleanup dir)
                      (cleanup base))))

    (test-case "a session adopts exactly one explicitly advertised root"
      ;; The /go executor prompt must name its scratch path before the executor
      ;; session exists. The campaign interpreter therefore allocates the root,
      ;; advertises it, and adopts it single-shot for the session it is about to
      ;; create; a second session created in the same extent gets its own root.
      (define base (fresh-dir "q-scratch-adopt-~a"))
      (define dir (fresh-dir "q-scratch-adopt-sessions-~a"))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-scratch-base-dir base])
           (reset-session-scratch-registry!)
           (define prov (make-simple-mock-provider "ok"))
           (define bus (make-event-bus))
           (define advertised (allocate-session-scratch-root! "campaign-scratch-token"))
           (define adopted
             (call-with-adopted-session-scratch-root advertised
                                                     (lambda () (new-session! prov bus dir))))
           (define sibling (new-session! prov bus dir))
           (check-true (same-path? advertised (allocated-session-scratch-root (session-id adopted)))
                       "the advertised root is the executor session's root")
           (check-false (same-path? (allocated-session-scratch-root (session-id adopted))
                                    (allocated-session-scratch-root (session-id sibling)))
                        "adoption is single-shot; a sibling session allocates its own")
           (close-session! adopted)
           (close-session! sibling)
           (reset-session-scratch-registry!)))
       (lambda ()
         (cleanup dir)
         (cleanup base))))))

(define (same-path? a b)
  (and a b (equal? (path->string a) (path->string b))))

(module+ test
  (run-tests session-scratch-suite))

(module+ main
  (run-tests session-scratch-suite))
