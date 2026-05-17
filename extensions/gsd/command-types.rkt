#lang racket/base

;; extensions/gsd/command-types.rkt — Structured command results
;; STABILITY: evolving
;;
;; Replaces ad-hoc hasheq responses from GSD command handlers with
;; a proper struct. All command handlers return gsd-command-result.

(provide (struct-out gsd-command-result)
         gsd-ok
         gsd-err
         gsd-command-result-success
         gsd-command-result-mode
         gsd-command-result-message
         gsd-command-result-data
         gsd-result?
         gsd-success?
         gsd-failed?)

;; Structured command result replacing ad-hoc hasheq responses.
;; NOTE: For new code, prefer shared command-result from util/command-types.rkt (F14).
(struct gsd-command-result
        (success    ; boolean
         mode       ; symbol (current GSD state after command)
         message    ; string (human-readable)
         data)      ; any — optional extra (wave index, archive path, etc.)
  #:transparent)

;; Constructor for successful results
(define (gsd-ok #:mode mode #:message msg #:data [data #f])
  (gsd-command-result #t mode msg data))

;; Constructor for error results
(define (gsd-err #:mode mode #:message msg)
  (gsd-command-result #f mode msg #f))

;; Predicate — is this a gsd-command-result?
(define (gsd-result? v)
  (gsd-command-result? v))

;; Was the command successful?
(define (gsd-success? r)
  (and (gsd-command-result? r)
       (gsd-command-result-success r)))

;; Did the command fail?
(define (gsd-failed? r)
  (and (gsd-command-result? r)
       (not (gsd-command-result-success r))))
