#lang racket

;; @speed slow
;; @suite default
;; @boundary unit

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

;; Arguments after inventory?, in validate-args! order. Keep scheduler and
;; ordering defaults aligned with parse-args so these tests exercise validation
;; rather than stale procedure arity.
(define default-new-args (list #f 'auto 'batch #f #f 'local #f #f #f #f #f #f #f #f #f #f 'fifo))

;; ---------------------------------------------------------------------------
;; Unit tests: validate-args!
;; ---------------------------------------------------------------------------

(test-case "validate-args!: accepts valid all suite"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn (lambda () (apply validate 4 #f #f #t 'all '() 1 #f #f default-new-args))))

(test-case "validate-args!: accepts valid fast suite"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn (lambda () (apply validate 4 #f 60 #t 'fast '() 1 #f #f default-new-args))))

(test-case "validate-args!: rejects unknown suite"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"unknown suite"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 4 #f #f #t 'nope '() 1 #f #f default-new-args)))))

(test-case "validate-args!: rejects --jobs 0"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"jobs must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 0 #f #f #t 'all '() 1 #f #f default-new-args)))))

(test-case "validate-args!: rejects negative --jobs"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"jobs must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate -1 #f #f #t 'all '() 1 #f #f default-new-args)))))

(test-case "validate-args!: rejects non-integer --jobs"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail? (lambda () (apply validate #f #f #f #t 'all '() 1 #f #f default-new-args))))

(test-case "validate-args!: rejects --repeat 0"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"repeat must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 4 #f #f #t 'all '() 0 #f #f default-new-args)))))

(test-case "validate-args!: rejects negative --repeat"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"repeat must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 4 #f #f #t 'all '() -2 #f #f default-new-args)))))

(test-case "validate-args!: rejects --timeout 0"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"timeout must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 4 #f 0 #t 'all '() 1 #f #f default-new-args)))))

(test-case "validate-args!: rejects negative --timeout"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"timeout must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 4 #f -5 #t 'all '() 1 #f #f default-new-args)))))

(test-case "validate-args!: #f timeout is allowed (no timeout)"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn (lambda () (apply validate 4 #f #f #t 'all '() 1 #f #f default-new-args))))

(test-case "validate-args!: rejects non-number --timeout"
  (define validate (runner-ref 'validate-args!))
  (check-exn exn:fail?
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (check-not-false (regexp-match? #rx"timeout must be"
                                                                            (exn-message e)))
                                            (raise e))])
                 (apply validate 4 #f 'not-a-number #t 'all '() 1 #f #f default-new-args)))))

;; ---------------------------------------------------------------------------
;; W10: --shard-plan measure validation
;; ---------------------------------------------------------------------------

;; default-new-args positions (0-indexed, after inventory?): 14 = --shard-plan,
;; 3 = --json-out.
(define (new-args-with-shard-plan v json-out)
  (list-set (list-set default-new-args 14 v) 3 json-out))

(test-case "validate-args!: --shard-plan measure requires --json-out"
  (define validate (runner-ref 'validate-args!))
  (check-exn
   exn:fail?
   (lambda ()
     (with-handlers ([exn:fail? (lambda (e)
                                  (check-not-false (regexp-match? #rx"measure requires --json-out"
                                                                  (exn-message e)))
                                  (raise e))])
       (apply validate 4 #f #f #t 'all '() 1 #f #f (new-args-with-shard-plan "measure" #f)))))
  (check-not-exn
   (lambda ()
     (apply validate 4 #f #f #t 'all '() 1 #f #f (new-args-with-shard-plan "measure" "snap.json")))))

(test-case "validate-args!: --shard-plan measure is a valid mode; nonsense is rejected"
  (define validate (runner-ref 'validate-args!))
  (check-not-exn
   (lambda ()
     (apply validate 4 #f #f #t 'all '() 1 #f #f (new-args-with-shard-plan "measure" "snap.json"))))
  (check-exn
   exn:fail?
   (lambda ()
     (apply validate 4 #f #f #t 'all '() 1 #f #f (new-args-with-shard-plan "sometimes" #f)))))

;; ---------------------------------------------------------------------------
;; known-suites list
;; ---------------------------------------------------------------------------

(test-case "known-suites: contains all expected suites"
  (define suites (runner-ref 'known-suites))
  (for ([s '(all fast slow smoke tui security arch runtime extensions workflows)])
    (check-not-false (memq s suites) (format "expected ~a in known-suites" s))))
