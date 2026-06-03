#lang racket/base

;; examples/sdk/06-sessions.rkt — In-memory sessions, resume, list

(require "../../runtime/session/session-manager.rkt"
         (only-in "../../interfaces/sdk-core.rkt" create-agent-session session-info))

(define mgr (make-in-memory-session-manager))

;; Create session with in-memory manager
(define rt (create-agent-session #:provider (hasheq 'type 'test) #:session-manager mgr))

(printf "Sessions: ~a~n" (sm-list mgr))
(printf "Session info: ~a~n" (session-info rt))
