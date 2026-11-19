#lang racket

;; tests/test-testing-audit.rkt
;; Pure-unit admission audit (final plan §3, rev 3 capability trigger):
;; the generated admission record, the fail-closed admission states, and the
;; §9 correctness cases — a stale record and a capability-denylist hit must be
;; review-required, never admitted; a refresh without a reviewed diff never
;; widens execution; structure changes require renewed review.
;;
;; Runs against a FIXTURE root (its own tests/.quick-admission.rktd) with the
;; owner table parameterized to the fixture owner — never the production
;; record.

;; @speed fast
;; @suite testing
;; @boundary unit

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         "../scripts/testing/quick-audit.rkt")

(define-runtime-path production-record "../tests/.quick-admission.rktd")

;; The production tree root, anchored to THIS file (raco test's working
;; directory is not guaranteed to be the repository root).
(define production-root (path->directory-path (simplify-path (build-path production-record 'up 'up))))

(define (make-fixture-root)
  (define dir (make-temporary-file "quick-audit-~a" 'directory))
  (make-directory (build-path dir "tests"))
  (make-directory (build-path dir "lib"))
  (write-file
   (build-path dir "tests" "pure-owner.rkt")
   "#lang racket/base\n(require \"../lib/pure.rkt\")\n(module+ test (require rackunit) (check-true (pure-add 1 2)))\n")
  (write-file (build-path dir "lib" "pure.rkt")
              "#lang racket/base\n(provide pure-add)\n(define (pure-add a b) (+ a b))\n")
  dir)

(define (write-file path text)
  (make-parent-directory* path)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; All fixture admission mechanics run with the owner table parameterized to
;; the fixture owner.
(define (fixture-admit! dir)
  (parameterize ([current-admitted-owners (list "tests/pure-owner.rkt")])
    (refresh-record! dir)))
(define (fixture-state dir)
  (parameterize ([current-admitted-owners (list "tests/pure-owner.rkt")])
    (owner-admission-state dir "tests/pure-owner.rkt")))
(define (fixture-ok? dir)
  (parameterize ([current-admitted-owners (list "tests/pure-owner.rkt")])
    (audit-ok? dir "tests/pure-owner.rkt")))

(module+ test
  (test-case "the production admission record exists and covers the owner table"
    (check-true (file-exists? production-record)
                "tests/.quick-admission.rktd is generated and committed")
    (define record (call-with-input-file production-record read))
    (check-equal? (cdr (assoc 'record-version record)) 1)
    (check-equal? (cdr (assoc 'denylist-version record)) denylist-version)
    (define owners (cdr (assoc 'owners record)))
    (check-equal? (length owners) (length (admitted-owners)) "every table owner has a record entry")
    ;; A record entry with a capability hit stores review-required; the
    ;; human disposition table (same PR) is what admits it. The executable
    ;; contract is audit-ok?, checked below and per owner.
    (for ([entry (in-list owners)])
      (check-pred (lambda (s) (memq s '(admitted review-required)))
                  (cdr (assoc 'status (cdr entry)))
                  (format "~a record status" (car entry)))))

  (test-case "every admitted owner passes the execution gate on the real tree"
    (for ([owner (in-list (admitted-owners))])
      (check-true (audit-ok? production-root owner) (format "~a must be executable by quick" owner))))

  (test-case "a non-owner is rejected, never admitted"
    (define dir (make-fixture-root))
    (dynamic-wind void
                  (lambda ()
                    (define state (owner-admission-state dir "tests/other.rkt"))
                    (check-equal? (admission-state-status state) 'rejected)
                    (check-false (audit-ok? dir "tests/other.rkt")))
                  (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "a table member without a record is review-required"
    (define dir (make-fixture-root))
    (dynamic-wind void
                  (lambda ()
                    (define state (fixture-state dir))
                    (check-equal? (admission-state-status state) 'review-required)
                    (check-regexp-match #rx"record missing|missing from the record"
                                        (admission-state-reason state)))
                  (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "a stale record (source changed since generation) is review-required"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (check-true (fixture-ok? dir))
       (write-file (build-path dir "lib" "pure.rkt")
                   "#lang racket/base\n(provide pure-add)\n(define (pure-add a b) (+ a b 0))\n")
       (define state (fixture-state dir))
       (check-equal? (admission-state-status state) 'review-required)
       (check-regexp-match #rx"source changed" (admission-state-reason state))
       (check-false (fixture-ok? dir)))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "refresh re-admits a clean content change (scan, not a human diff)"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (check-true (fixture-ok? dir))
       (write-file (build-path dir "lib" "pure.rkt")
                   "#lang racket/base\n(provide pure-add)\n(define (pure-add a b) (+ a b 0))\n")
       (check-false (fixture-ok? dir))
       (fixture-admit! dir)
       (check-true (fixture-ok? dir)))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "a content-edit refresh changes only hashes: closure and verdicts stay identical"
    ;; W2 (review 7002474250): the refresh path for an ordinary edit must not
    ;; be able to widen anything - the refreshed entry keeps the same closure,
    ;; the same status, and the reviewed identity differs only in the hash of
    ;; the edited file.
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (define before (call-with-input-file (record-path dir) read))
       (define before-fields (cdr (car (cdr (assoc 'owners before)))))
       (write-file (build-path dir "lib" "pure.rkt")
                   "#lang racket/base\n(provide pure-add)\n(define (pure-add a b) (+ a b 0))\n")
       (fixture-admit! dir)
       (define after (call-with-input-file (record-path dir) read))
       (define after-owners (cdr (assoc 'owners after)))
       (define after-fields (cdr (car after-owners)))
       ;; Same closure, same admission verdict, same record shape.
       (check-equal? (cdr (assoc 'closure after-fields))
                     (cdr (assoc 'closure before-fields))
                     "a content edit does not change the dependency closure")
       (check-equal? (cdr (assoc 'status after-fields))
                     (cdr (assoc 'status before-fields))
                     "a clean content edit keeps the admission verdict")
       (check-equal? (length after-owners)
                     (length (cdr (assoc 'owners before)))
                     "no owner is added or dropped")
       ;; Exactly one hash changed: the edited file's.
       (define changed
         (for/list ([pair-before (in-list (cdr (assoc 'hashes before-fields)))]
                    #:when (not (equal? pair-before
                                        (assoc (car pair-before)
                                               (cdr (assoc 'hashes after-fields))))))
           (car pair-before)))
       (check-equal? changed '("lib/pure.rkt") "only the edited file's hash moves"))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "refresh is deterministic: two refreshes produce a byte-identical committed diff"
    ;; The refresh is a committed diff, so it must be a pure function of the
    ;; tree: re-running it on an unchanged tree produces the identical bytes.
    (define dir (make-fixture-root))
    (dynamic-wind void
                  (lambda ()
                    (fixture-admit! dir)
                    (define bytes-1 (file->bytes (record-path dir)))
                    (fixture-admit! dir)
                    (check-equal? (file->bytes (record-path dir))
                                  bytes-1
                                  "refresh is a pure function of the tree"))
                  (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "a capability-denylist hit is review-required, never admitted (§9)"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (check-true (fixture-ok? dir))
       (write-file (build-path dir "lib" "pure.rkt")
                   (string-append
                    "#lang racket/base\n(provide pure-add)\n(require racket/file)\n"
                    "(define (pure-add a b) (delete-directory/files \"/tmp/nope\") (+ a b))\n"))
       (fixture-admit! dir)
       (define state (fixture-state dir))
       (check-equal? (admission-state-status state) 'review-required)
       (check-regexp-match #rx"denylist hit: delete-directory/files" (admission-state-reason state))
       (check-false (fixture-ok? dir)))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "a dependency-structure change requires renewed review (§9)"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (check-true (fixture-ok? dir))
       ;; Capture the PRE-change record entry: the crafted stale record below
       ;; uses it after the structure grows.
       (define pre-change-entry
         (for/first ([e (in-list (cdr (assoc 'owners (call-with-input-file (record-path dir) read))))]
                     #:when (equal? (car e) "tests/pure-owner.rkt"))
           e))
       (write-file (build-path dir "lib" "extra.rkt")
                   "#lang racket/base\n(provide three)\n(define three 3)\n")
       (write-file
        (build-path dir "lib" "pure.rkt")
        "#lang racket/base\n(require \"extra.rkt\")\n(provide pure-add)\n(define (pure-add a b) (+ a b three))\n")
       (fixture-admit! dir)
       ;; The refreshed record admits the new structure (the scan is clean and
       ;; the structure change is itself the reviewed diff of this commit).
       (check-true (fixture-ok? dir))
       ;; But a record generated BEFORE the structure change refuses.
       (define stale
         (list (cons 'record-version 1)
               (cons 'denylist-version denylist-version)
               (cons 'owners (list pre-change-entry))))
       (call-with-output-file (record-path dir) #:exists 'truncate (lambda (out) (write stale out)))
       (define state (fixture-state dir))
       (check-equal? (admission-state-status state) 'review-required)
       (check-regexp-match #rx"dependency structure changed" (admission-state-reason state)))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "a denylist-version change invalidates every record (tightening is review-required)"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (check-true (fixture-ok? dir))
       (define rec (call-with-input-file (record-path dir) read))
       (define stale
         (list (cons 'record-version 1)
               (cons 'denylist-version 0)
               (cons 'owners (cdr (assoc 'owners rec)))))
       (call-with-output-file (record-path dir) #:exists 'truncate (lambda (out) (write stale out)))
       (define state (fixture-state dir))
       (check-equal? (admission-state-status state) 'review-required)
       (check-regexp-match #rx"denylist version changed" (admission-state-reason state)))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "the scanner flags capability symbols and skips strings and quoted data"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (write-file (build-path dir "tests" "quoted.rkt")
                   (string-append "#lang racket/base\n"
                                  "(define kinds '(system subprocess network))\n"
                                  "(define msg \"call-with-output-file is a string here\")\n"
                                  "(module+ test (require rackunit) (check-true #t))\n"))
       (define-values (hit? cap _l) (scan-file (build-path dir "tests" "quoted.rkt")))
       (check-false hit? "quoted data and strings are not capabilities")
       (write-file (build-path dir "tests" "hits.rkt")
                   (string-append
                    "#lang racket/base\n(require racket/file)\n(module+ test (require rackunit))\n"
                    "(define (bad) (delete-directory/files \"/tmp/x\"))\n"))
       (define-values (hit2? cap2 _l2) (scan-file (build-path dir "tests" "hits.rkt")))
       (check-true hit2?)
       (check-equal? cap2 'delete-directory/files))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "the audit report classifies admitted, review-required and rejected"
    (define dir (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (fixture-admit! dir)
       (write-file
        (build-path dir "tests" "test-writer.rkt")
        "#lang racket/base\n(module+ test (require rackunit) (check-true #t))\n(define (w) (make-directory* \"/tmp/x\"))\n")
       (define states
         (parameterize ([current-admitted-owners (list "tests/pure-owner.rkt")])
           (audit-report dir)))
       (define rejected-writer
         (for/first ([s (in-list states)]
                     #:when (equal? (admission-state-owner s) "tests/test-writer.rkt"))
           s))
       (check-true (and rejected-writer #t) "the candidate appears in the report")
       (check-equal? (admission-state-status rejected-writer) 'rejected)
       (check-regexp-match #rx"capability scan hit: make-directory"
                           (admission-state-reason rejected-writer)))
     (lambda () (delete-directory/files dir #:must-exist? #f)))))
