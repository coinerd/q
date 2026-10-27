#lang racket/base
;; @covers extensions/gsd/planning-artifact-ownership.rkt

;; @speed fast
;; @suite extensions
;; @boundary integration

;; tests/test-gsd-planning-artifact-ownership.rkt — W4 (GSD-SIMPLIFICATION-PLAN):
;; the ONE authentic ownership-marker predicate shared by the snapshot sweep,
;; /plan supersede rotation, the read-only doctor and archive cleanup.
;;
;; These are the RED-first contracts for the W4 blocker: only a real, nofollow
;; marker file whose datum is a hash with schema-version 1 and the exact
;; creator KIND proves ownership. A wrong kind, a wrong schema, empty/arbitrary
;; bytes, a symlink, a directory or a lookalike name is unknown operator state
;; and every consumer must preserve it.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "../extensions/gsd/planning-artifact-ownership.rkt")

(define (with-temp-dir thunk)
  (define dir (make-temporary-file "gsd-artifact-ownership-~a" 'directory))
  (dynamic-wind void
                (lambda () (thunk dir))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(define (write-datum! path datum)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (write datum out)) #:exists 'truncate))

(define (write-bytes! path content)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display content out)) #:exists 'truncate))

(define (write-authentic-marker! dir kind #:schema [schema 1] #:recorded-kind [recorded-kind kind])
  (write-datum! (build-path dir (planning-artifact-marker-filename kind))
                (hasheq 'schema-version schema 'kind recorded-kind 'pid 1 'created-at 0)))

;; ------------------------------------------------------------
;; Case bodies (each receives one fresh temp directory)
;; ------------------------------------------------------------

(define (check-known-kinds _dir)
  (check-equal? (planning-artifact-ownership-kinds) '(plan-snapshot-staging waves-pre-plan-backup))
  (check-equal? (planning-artifact-marker-filename 'plan-snapshot-staging)
                ".owned-plan-snapshot-staging")
  (check-equal? (planning-artifact-marker-filename 'waves-pre-plan-backup)
                ".q-gsd-plan-supersede-owner")
  (check-exn exn:fail?
             (lambda () (planning-artifact-marker-filename 'made-up-owner))
             "an unknown kind is never silently mapped to a marker name"))

(define (check-every-authentic-kind dir)
  (for ([kind (in-list (planning-artifact-ownership-kinds))])
    (define owned (build-path dir (format "dir-~a" kind)))
    (make-directory owned)
    (write-authentic-marker! owned kind)
    (check-true (authentic-planning-artifact-marker? owned kind)
                (format "~a authentic marker accepted" kind))
    (check-eq? (owned-planning-artifact-dir? owned) kind)))

(define (check-crossed-kind dir)
  (define d (build-path dir "crossed"))
  (make-directory d)
  ;; The staging marker FILENAME carries the backup KIND.
  (write-authentic-marker! d 'plan-snapshot-staging #:recorded-kind 'waves-pre-plan-backup)
  (check-false (authentic-planning-artifact-marker? d 'plan-snapshot-staging))
  (check-false (owned-planning-artifact-dir? d)))

(define (check-bad-schema dir)
  (for ([bad (in-list (list (hasheq 'schema-version 2 'kind 'plan-snapshot-staging)
                            (hasheq 'schema-version "1" 'kind 'plan-snapshot-staging)
                            (hasheq 'kind 'plan-snapshot-staging)
                            (hasheq 'schema-version 1 'kind "plan-snapshot-staging")))]
        [n (in-naturals)])
    (define d (build-path dir (format "bad-schema-~a" n)))
    (make-directory d)
    (write-datum! (build-path d (planning-artifact-marker-filename 'plan-snapshot-staging)) bad)
    (check-false (authentic-planning-artifact-marker? d 'plan-snapshot-staging)
                 (format "rejected marker datum ~s" bad))
    (check-false (owned-planning-artifact-dir? d))))

(define (check-bad-bytes dir)
  (for ([content (in-list (list "" "\n" "not a hash\n" "#f\n"))]
        [n (in-naturals)])
    (define d (build-path dir (format "bad-bytes-~a" n)))
    (make-directory d)
    (write-bytes! (build-path d (planning-artifact-marker-filename 'waves-pre-plan-backup)) content)
    (check-false (authentic-planning-artifact-marker? d 'waves-pre-plan-backup)
                 (format "rejected marker bytes ~s" content))
    (check-false (owned-planning-artifact-dir? d))))

(define (check-trailing-junk dir)
  ;; A valid record followed by ANY further bytes is not a record a creator
  ;; writes: "the first hash wins" would accept an edited/annotated marker.
  (define d (build-path dir "trailing-junk"))
  (make-directory d)
  (write-bytes! (build-path d (planning-artifact-marker-filename 'plan-snapshot-staging))
                (string-append
                 "#hash((schema-version . 1) (kind . plan-snapshot-staging))"
                 "\nnot ours\n"))
  (check-false (authentic-planning-artifact-marker? d 'plan-snapshot-staging))
  (check-false (owned-planning-artifact-dir? d)))

(define (check-symlinked-marker dir)
  (define d (build-path dir "linked-marker"))
  (make-directory d)
  (define outside (build-path dir "outside-marker"))
  (write-datum! outside (hasheq 'schema-version 1 'kind 'plan-snapshot-staging 'pid 1 'created-at 0))
  (make-file-or-directory-link
   outside
   (build-path d (planning-artifact-marker-filename 'plan-snapshot-staging)))
  (check-false (authentic-planning-artifact-marker? d 'plan-snapshot-staging))
  (check-false (owned-planning-artifact-dir? d)))

(define (check-marker-directory dir)
  (define d (build-path dir "marker-is-dir"))
  (make-directory* (build-path d (planning-artifact-marker-filename 'plan-snapshot-staging)))
  (check-false (authentic-planning-artifact-marker? d 'plan-snapshot-staging))
  (check-false (owned-planning-artifact-dir? d)))

(define (check-symlinked-directory dir)
  (define real (build-path dir "real-owned"))
  (make-directory real)
  (write-authentic-marker! real 'waves-pre-plan-backup)
  (define alias (build-path dir "alias"))
  (make-file-or-directory-link real alias)
  (check-true (authentic-planning-artifact-marker? real 'waves-pre-plan-backup))
  (check-false (owned-planning-artifact-dir? alias)
               "deleting through a symlink could reach outside the planning root"))

(define (check-plain-directory dir)
  (define d (build-path dir "notes"))
  (make-directory d)
  (check-false (owned-planning-artifact-dir? d))
  (check-false (authentic-planning-artifact-marker? d 'plan-snapshot-staging)))

(define (check-pure-read dir)
  (define d (build-path dir "owned"))
  (make-directory d)
  (write-authentic-marker! d 'plan-snapshot-staging)
  (define marker (build-path d (planning-artifact-marker-filename 'plan-snapshot-staging)))
  (define before (file->bytes marker))
  (check-eq? (owned-planning-artifact-dir? d) 'plan-snapshot-staging)
  (check-true (authentic-planning-artifact-marker? d 'plan-snapshot-staging))
  (check-equal? (file->bytes marker) before)
  (check-equal? (map path->string (directory-list d #:build? #t)) (list (path->string marker))))

;; ------------------------------------------------------------
;; Suite
;; ------------------------------------------------------------

(define ownership-suite
  (test-suite "gsd-planning-artifact-ownership"

    (test-case "the known ownership kinds are closed and each has one marker name"
      (check-known-kinds #f))

    (test-case "an authentic marker for every known kind proves ownership"
      (with-temp-dir check-every-authentic-kind))

    (test-case "a marker for ANOTHER kind does not transfer ownership"
      (with-temp-dir check-crossed-kind))

    (test-case "a non-schema-1 or missing schema is not ownership"
      (with-temp-dir check-bad-schema))

    (test-case "empty or arbitrary marker bytes are not ownership"
      (with-temp-dir check-bad-bytes))

    (test-case "a valid record followed by junk is not ownership"
      (with-temp-dir check-trailing-junk))

    (test-case "a symlinked marker is not ownership (nofollow)"
      (with-temp-dir check-symlinked-marker))

    (test-case "a marker directory is not ownership"
      (with-temp-dir check-marker-directory))

    (test-case "a symlinked directory is never owned"
      (with-temp-dir check-symlinked-directory))

    (test-case "a plain directory without a marker is not ownership"
      (with-temp-dir check-plain-directory))

    (test-case "the predicate is a pure read shell (no marker or directory changes)"
      (with-temp-dir check-pure-read))))

(void (run-tests ownership-suite))
