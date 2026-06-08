#lang racket

;; @speed slow
;; @suite default

;; BOUNDARY: unit
;; Tests for strict argument validation in run-tests.rkt (v0.83.1 W0)

(require rackunit
         racket/port
         racket/system
         racket/runtime-path)

(define-runtime-path runner-path "../scripts/run-tests.rkt")

;; Load runner module
(define runner-loaded? (box #f))
(define runner-cache (make-hash))

(define (runner-ref sym)
  (unless (unbox runner-loaded?)
    (dynamic-require runner-path #f)
    (set-box! runner-loaded? #t))
  (hash-ref! runner-cache sym (lambda () (dynamic-require runner-path sym))))

;; ---------------------------------------------------------------------------
;; Unit tests: validate-args!
;; ---------------------------------------------------------------------------

(test-case "validate-args!: accepts valid all suite"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn (lambda () (validate 4 #f #f #t 'all '() 1 #f #f))))

(test-case "validate-args!: accepts valid fast suite"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn (lambda () (validate 4 #f 60 #t 'fast '() 1 #f #f))))

(test-case "validate-args!: rejects unknown suite"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"unknown suite"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 4 #f #f #t 'nope '() 1 #f #f)))))

(test-case "validate-args!: rejects --jobs 0"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"jobs must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 0 #f #f #t 'all '() 1 #f #f)))))

(test-case "validate-args!: rejects negative --jobs"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"jobs must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate -1 #f #f #t 'all '() 1 #f #f)))))

(test-case "validate-args!: rejects non-integer --jobs"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail? (lambda () (validate #f #f #f #t 'all '() 1 #f #f))))

(test-case "validate-args!: rejects --repeat 0"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"repeat must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 4 #f #f #t 'all '() 0 #f #f)))))

(test-case "validate-args!: rejects negative --repeat"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"repeat must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 4 #f #f #t 'all '() -2 #f #f)))))

(test-case "validate-args!: rejects --timeout 0"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"timeout must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 4 #f 0 #t 'all '() 1 #f #f)))))

(test-case "validate-args!: rejects negative --timeout"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"timeout must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 4 #f -5 #t 'all '() 1 #f #f)))))

(test-case "validate-args!: #f timeout is allowed (no timeout)"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn (lambda () (validate 4 #f #f #t 'all '() 1 #f #f))))

(test-case "validate-args!: rejects non-number --timeout"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"timeout must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (validate 4 #f 'not-a-number #t 'all '() 1 #f #f)))))

;; ---------------------------------------------------------------------------
;; known-suites list
;; ---------------------------------------------------------------------------

(test-case "known-suites: contains all expected suites"
  (define suites (runner-ref 'known-suites))
  (for ([s '(all fast slow smoke tui security arch runtime extensions workflows)])
    (check-not-false (memq s suites) (format "expected ~a in known-suites" s))))
