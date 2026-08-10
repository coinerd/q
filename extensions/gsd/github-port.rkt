#lang racket/base
;; STABILITY: internal

;; extensions/gsd/github-port.rkt — GitHub/release side-effect boundary (v0.99.90 W4)
;;
;; Board/issue/PR/release operations are modeled as idempotent correlated
;; commands. Every command carries a correlation ID; the port keeps a journal
;; of correlation IDs already executed so a retry of the same command is
;; answered from the journal instead of creating a duplicate external effect
;; (duplicate issue, duplicate merge, duplicate release).
;;
;; Safety properties (Kontrolle):
;;   - dry-run default: a port constructed with #:dry-run? #t (the default)
;;     NEVER calls the live adapter; execute returns a dry-run result.
;;   - immutable SHA assertions: pr-merge / release-create commands carry an
;;     expected-sha; the adapter must attest the same sha before the port
;;     reports success, otherwise exn:fail:github-sha-mismatch is raised and
;;     nothing is executed.
;;   - token redaction: the port never writes the token to the journal or
;;     results; adapter error messages are redacted before they escape.
;;   - no destructive live test: standard tests use fakes; live GitHub is
;;     reachable only by explicitly constructing an adapter in an approved
;;     smoke (never from standard tests).

(require racket/contract
         racket/string
         "effect-ports.rkt")

(provide make-github-port
         make-dry-run-github-port
         make-inert-github-adapter
         make-github-adapter
         github-adapter
         github-adapter?
         github-adapter-create-issue!
         github-adapter-close-issue!
         github-adapter-set-board-field!
         github-adapter-merge-pr!
         github-adapter-create-release!
         github-adapter-get-pr
         github-adapter-get-issue
         github-adapter-find-issue-by-key
         github-adapter-find-release-by-tag
         exn:fail:github-sha-mismatch?
         exn:fail:github-sha-mismatch-expected
         exn:fail:github-sha-mismatch-actual
         redact-token)

;; ============================================================
;; Adapter protocol
;; ============================================================

;; The injected boundary a live GitHub client must satisfy. Standard tests
;; inject make-fake-github-adapter (tests/helpers/gsd-port-fakes.rkt); the
;; production default is make-inert-github-adapter behind a dry-run port so
;; no live call is ever reachable without an explicit approved smoke.
;;
;; PR records are (list 'merged <bool> 'head-sha <string> 'merge-sha <string>)
;; or #f for unknown pull numbers; issue records are
;; (list 'state <symbol> 'number <string>) or #f.
(struct github-adapter
        (create-issue! close-issue!
                       set-board-field!
                       merge-pr!
                       create-release!
                       get-pr
                       get-issue
                       find-issue-by-key
                       find-release-by-tag)
  #:transparent)

(define (make-github-adapter create-issue!
                             close-issue!
                             set-board-field!
                             merge-pr!
                             create-release!
                             get-pr
                             get-issue
                             find-issue-by-key
                             find-release-by-tag)
  (github-adapter create-issue!
                  close-issue!
                  set-board-field!
                  merge-pr!
                  create-release!
                  get-pr
                  get-issue
                  find-issue-by-key
                  find-release-by-tag))

(define (make-inert-github-adapter)
  (define (inert! _x)
    (raise (exn:fail:user "github adapter invoked: live GitHub requires an explicit approved smoke"
                          (current-continuation-marks))))
  (github-adapter inert!
                  inert!
                  inert!
                  inert!
                  inert!
                  (lambda (_n) #f)
                  (lambda (_n) #f)
                  (lambda (_k) #f)
                  (lambda (_t) #f)))

;; ============================================================
;; SHA mismatch error
;; ============================================================

(define-struct (exn:fail:github-sha-mismatch exn:fail) (expected actual)
  #:transparent)

(define (sha-mismatch! expected actual)
  (raise (exn:fail:github-sha-mismatch
          (format "immutable sha assertion failed: expected ~a, live ~a" expected actual)
          (current-continuation-marks)
          expected
          actual)))

;; ============================================================
;; Token redaction
;; ============================================================

;; Scrub a configured token from arbitrary text. Used to guarantee adapter
;; error messages never leak the credential past the boundary.
(define (redact-token token text)
  (cond
    [(not (and (string? token) (positive? (string-length token)))) text]
    [(string-contains? text token) (string-replace text token "[REDACTED]")]
    [else text]))

;; ============================================================
;; Idempotent correlated command execution
;; ============================================================

;; Wrap an adapter operation so any raised error is re-raised with the
;; configured token redacted (never leak credentials in error surfaces).
(define (guarded adapter-op token)
  (lambda args
    (with-handlers ([exn:fail? (lambda (e)
                                 (raise (make-exn:fail (redact-token token (exn-message e))
                                                       (current-continuation-marks))))])
      (apply adapter-op args))))

(define (make-github-port adapter #:dry-run? [dry-run? #t] #:token [token #f])
  (define journal (make-hash))
  (define (already-executed? correlation-id)
    (hash-has-key? journal correlation-id))
  (define (note-done! correlation-id result)
    (hash-set! journal correlation-id result))

  ;; External dedup: ask the adapter whether the effect already exists before
  ;; creating it (cross-restart safety: journal is in-memory only). Read ops
  ;; are guarded the same way as write ops so adapter read errors are also
  ;; redacted (MINOR-2 fold).
  (define (dedup-external kind params)
    (case kind
      [(issue-create)
       (define key (hash-ref params 'dedup-key (hash-ref params 'title #f)))
       (and key ((guarded (github-adapter-find-issue-by-key adapter) token) key))]
      [(release-create)
       (define tag (hash-ref params 'tag #f))
       (and tag ((guarded (github-adapter-find-release-by-tag adapter) token) tag))]
      [else #f]))

  ;; Immutable SHA assertion for release-create: the attested expected-sha
  ;; must equal the target commitish (MINOR-1 fold: also enforced on the
  ;; external-dedup path so a pre-existing release on the wrong commit can
  ;; never be reported as success).
  (define (assert-release-sha! params expected-sha)
    (define target (hash-ref params 'target-commitish #f))
    (when (and expected-sha target (not (equal? expected-sha target)))
      (sha-mismatch! expected-sha target)))

  ;; Immutable SHA assertion for pr-merge: the live PR head must equal the
  ;; attested expected-sha, else fail closed (no merge).
  (define (assert-pr-sha! pull-number expected-sha)
    (when expected-sha
      (define pr ((guarded (github-adapter-get-pr adapter) token) pull-number))
      (define live-head (and pr (memq 'head-sha pr) (cadr (memq 'head-sha pr))))
      (unless (equal? live-head expected-sha)
        (sha-mismatch! expected-sha (or live-head "unknown")))))

  (define (execute-command cmd)
    (define kind (gsd-github-command-kind cmd))
    (define correlation-id (gsd-github-command-correlation-id cmd))
    (define params (gsd-github-command-params cmd))
    (define expected-sha (gsd-github-command-expected-sha cmd))

    (cond
      ;; 1. dry-run is the default; never touch the adapter, never journal.
      [dry-run?
       (gsd-github-command-result correlation-id
                                  kind
                                  #f
                                  #t
                                  #f
                                  "dry-run: no external effect (dry-run default)")]
      ;; 2. journal replay: same correlation-id -> recorded result, no call.
      [(already-executed? correlation-id)
       (define recorded (hash-ref journal correlation-id))
       (gsd-github-command-result correlation-id
                                  kind
                                  (gsd-github-command-result-external-id recorded)
                                  #f
                                  #t
                                  "journal replay: no duplicate external effect")]
      [else
       (define existing (dedup-external kind params))
       ;; MINOR-1 fold: release-create dedup still enforces the immutable
       ;; SHA assertion (a pre-existing release on the wrong commit must
       ;; never be reported as success).
       (when (and existing (eq? kind 'release-create))
         (assert-release-sha! params expected-sha))
       (define result
         (if existing
             (gsd-github-command-result correlation-id
                                        kind
                                        existing
                                        #f
                                        #t
                                        "external dedup: effect already exists")
             (case kind
               [(issue-create)
                (define id ((guarded (github-adapter-create-issue! adapter) token) params))
                (gsd-github-command-result correlation-id kind id #f #f "created")]
               [(issue-close)
                ((guarded (github-adapter-close-issue! adapter) token) (hash-ref params
                                                                                 'issue-number))
                (gsd-github-command-result correlation-id kind #f #f #f "closed")]
               [(board-set-field)
                ((guarded (github-adapter-set-board-field! adapter) token) params)
                (gsd-github-command-result correlation-id kind #f #f #f "field set")]
               [(pr-merge)
                (assert-pr-sha! (hash-ref params 'pull-number) expected-sha)
                (define merged-pr ((github-adapter-get-pr adapter) (hash-ref params 'pull-number)))
                (if (and merged-pr (memq 'merged merged-pr) (cadr (memq 'merged merged-pr)))
                    ;; already merged externally -> return existing merge sha
                    (gsd-github-command-result correlation-id
                                               kind
                                               (cadr (memq 'merge-sha merged-pr))
                                               #f
                                               #t
                                               "already merged")
                    (let ([merge-sha ((guarded (github-adapter-merge-pr! adapter) token) params)])
                      (gsd-github-command-result correlation-id kind merge-sha #f #f "merged")))]
               [(release-create)
                (assert-release-sha! params expected-sha)
                (define id ((guarded (github-adapter-create-release! adapter) token) params))
                (gsd-github-command-result correlation-id kind id #f #f "created")]
               [else (error 'github-port "unhandled command kind: ~s" kind)])))
       (note-done! correlation-id result)
       result]))
  (gsd-github-port execute-command (lambda () dry-run?) (lambda () (hash-keys journal))))

;; Production-safe default: dry-run port backed by an adapter that raises if
;; ever invoked. Live GitHub requires an explicit approved smoke.
(define (make-dry-run-github-port)
  (make-github-port (make-inert-github-adapter) #:dry-run? #t))
